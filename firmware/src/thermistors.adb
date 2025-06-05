with STM32.GPIO;   use STM32.GPIO;
with STM32.Device; use STM32.Device;
with STM32.ADC;    use STM32.ADC;
with STM32.DMA;    use STM32.DMA;
with HAL;          use HAL;
with Heaters;
with Last_Chance_Handler;

package body Thermistors is

   procedure Init is
   begin
      ADC_Handler.Init;
   end Init;

   --  Using an access type here is inconvenient, but GCC will try to put the entire large array on the stack since
   --  we are passing it to a protected procedure.
   procedure Setup (Thermistor_Curves : access Thermistor_Curves_Array; Heater_Map : Heater_Thermistor_Map) is
   begin
      ADC_Handler.Setup (Thermistor_Curves, Heater_Map);
   end Setup;

   procedure Start_ISR_Loop is
   begin
      ADC_Handler.Start_ISR_Loop;
   end Start_ISR_Loop;

   function Last_Reported_Temperature (Thermistor : Thermistor_Name) return Temperature is
   begin
      return ADC_Handler.Last_Reported_Temperature (Thermistor);
   end Last_Reported_Temperature;

   protected body ADC_Handler is
      procedure Init is
      begin
         if Init_Done then
            raise Constraint_Error with "Tried to call thermistor Init more than once.";
         end if;

         Enable_Clock (Thermistor_ADC);
         Enable_Clock (Thermistor_DMA_Controller);

         for C of Curves loop
            for P of C loop
               P := (Temp => Bad_Reading_Indicator, Value => 0.0 * ohm);
            end loop;
         end loop;

         Disable (Thermistor_ADC);

         --  Shared with heater current sense.
         Configure_Common_Properties
           (This           => Thermistor_ADC,
            Mode           => Independent,
            Prescaler      => Div_1,
            Clock_Mode     => PCLK2_Div_4,
            DMA_Mode       => Disabled,
            Sampling_Delay => Sampling_Delay_5_Cycles);

         Calibrate (Thermistor_ADC, Single_Ended);

         Configure_Unit (Thermistor_ADC, ADC_Resolution_12_Bits, Right_Aligned);
         Thermistor_ADC_Internal.CFGR2.ROVSE := True;   --  Regular oversampling
         Thermistor_ADC_Internal.CFGR2.OVSS := 4;      --  4 bit shift
         Thermistor_ADC_Internal.CFGR2.OVSR := 2#111#; --  256 samples

         Configure_Regular_Conversions
           (This        => Thermistor_ADC,
            Continuous  => False,
            Trigger     => Software_Triggered,
            Conversions =>
              [for I in 1 .. Thermistor_ADC_Input_Name'Pos (Thermistor_ADC_Input_Name'Last) + 1
               => (Channel     => Thermistor_ADC_Channels (Thermistor_ADC_Input_Name'Val (I - 1)),
                Sample_Time => Sample_92P5_Cycles)]);

         Enable (Thermistor_ADC);

         for Thermistor in Thermistor_ADC_Input_Name loop
            Configure_IO (Thermistor_GPIO_Points (Thermistor), (Mode => Mode_Analog, Resistors => Floating));
         end loop;

         Configure
           (Thermistor_DMA_Controller,
            Thermistor_DMA_Stream,
            (Channel                      => Thermistor_DMA_Channel,
             Direction                    => Peripheral_To_Memory,
             Increment_Peripheral_Address => False,
             Increment_Memory_Address     => True,
             Peripheral_Data_Format       => HalfWords,
             Memory_Data_Format           => HalfWords,
             Operation_Mode               => Normal_Mode,
             Priority                     => Thermistor_DMA_Priority,
             Memory_Burst_Size            => Memory_Burst_Single,
             Peripheral_Burst_Size        => Peripheral_Burst_Single));

         Enable_DMA (Thermistor_ADC);

         Init_Done := True;
      end Init;

      procedure Setup (Thermistor_Curves : access Thermistor_Curves_Array; Heater_Map : Heater_Thermistor_Map) is
      begin
         if not Init_Done then
            raise Constraint_Error with "Tried to call thermistor Setup before Init.";
         end if;

         if Setup_Done then
            raise Constraint_Error with "Tried to call thermistor Setup more than once.";
         end if;

         for Thermistor in Thermistor_Name loop
            for I in Thermistor_Curve_Index loop
               Curves (Thermistor) (I) :=
                 (Temp  => Dimensionless (Thermistor_Curves (Thermistor) (I).Temp) * celsius,
                  Value => Dimensionless (Thermistor_Curves (Thermistor) (I).Value) * ohm);
            end loop;
         end loop;
         Heater_Thermistors := Heater_Map;

         Setup_Done := True;
      end Setup;

      function Last_Reported_Temperature (Thermistor : Thermistor_Name) return Temperature is
      begin
         return Last_Temperatures (Thermistor);
      end Last_Reported_Temperature;

      procedure Start_ISR_Loop is
      begin
         if not Setup_Done then
            raise Constraint_Error with "Tried to start thermistor ISR loop before calling Setup.";
         end if;

         if ISR_Loop_Started then
            raise Constraint_Error with "Tried to start thermistor ISR loop more than once.";
         end if;

         ISR_Loop_Started := True;

         Start_Conversion;
      end Start_ISR_Loop;

      procedure Start_Conversion is
      begin
         Start_Transfer_with_Interrupts
           (This               => Thermistor_DMA_Controller,
            Stream             => Thermistor_DMA_Stream,
            Source             => Data_Register_Address (Thermistor_ADC),
            Destination        => ADC_Results'Address,
            Data_Count         => ADC_Results'Length,
            Enabled_Interrupts => [Transfer_Complete_Interrupt => True, others => False]);
         Start_Conversion (Thermistor_ADC);
      end Start_Conversion;

      function Interpolate (Res : Resistance; Thermistor : Thermistor_Name) return Temperature is
         Curve : Float_Thermistor_Curve renames Curves (Thermistor);
         Left  : Thermistor_Curve_Index := Thermistor_Curve'First;
         Right : Thermistor_Curve_Index := Thermistor_Curve'Last - 1;
         Mid   : Thermistor_Curve_Index;
      begin
         while Left <= Right loop
            Mid := Left + (Right - Left) / 2;
            if Res >= Curve (Mid).Value and then Res <= Curve (Mid + 1).Value then
               exit;
            elsif Res < Curve (Mid).Value then
               if Mid = Left then
                  return Bad_Reading_Indicator;
               end if;
               Right := Mid - 1;
            else
               if Mid = Left then
                  return Bad_Reading_Indicator;
               end if;
               Left := Mid + 1;
            end if;
         end loop;

         declare
            Lower_Point : constant Float_Thermistor_Point := Curve (Mid);
            Upper_Point : constant Float_Thermistor_Point := Curve (Mid + 1);
         begin
            if Lower_Point.Temp = Upper_Point.Temp then
               return Lower_Point.Temp;
            else
               return
                 Lower_Point.Temp
                 + (Upper_Point.Temp - Lower_Point.Temp)
                   / (Upper_Point.Value - Lower_Point.Value)
                   * (Res - Lower_Point.Value);
            end if;
         end;
      end Interpolate;

      procedure End_Of_Sequence_Handler is
      begin
         Clear_All_Status (Thermistor_DMA_Controller, Thermistor_DMA_Stream);

         for Thermistor in Thermistor_ADC_Input_Name loop
            Accumulators (Thermistor) := @ + Accumulator_Type (ADC_Results (Thermistor));
         end loop;

         Start_Conversion;

         if Step = Accumulator_Step'Last then
            if Accumulators (Thermistor_Ground) > Accumulator_Type'Last / 4 then
               --  The divisor of 4 allows for a maximum resistance of 100 ohms when all thermistors are 0 ohms.
               raise Bad_Reading_Error
                 with "A thermistor wire appears to be shorted to something else (PTC > 100ohm).";
            end if;

            if Accumulators (Thermistor_VDD) < Accumulator_Type'Last / 30 * 14 then
               raise Bad_Reading_Error with "Detected short or damage in thermistor section of board (VDD < 3.1V).";
            end if;

            if Accumulators (Thermistor_VDD) > Accumulator_Type'Last / 30 * 16 then
               declare
                  Most_Likely_Short : Thermistor_ADC_Input_Name := Thermistor_1;
               begin
                  for T in Thermistor_ADC_Input_Name range Thermistor_1 .. Thermistor_4 loop
                     if Accumulators (T) > Accumulators (Most_Likely_Short) then
                        Most_Likely_Short := T;
                     end if;
                  end loop;
                  raise Bad_Reading_Error
                    with
                      "A thermistor wire appears to be shorted to something else (likely "
                      & Most_Likely_Short'Image
                      & ") (VDD > 3.5V).";
               end;
            end if;

            for Thermistor in Thermistor_Name loop
               declare
                  Internal_Thermistor_Name : Thermistor_ADC_Input_Name;
               begin
                  case Thermistor is
                     when Thermistor_1 =>
                        Internal_Thermistor_Name := Thermistor_1;

                     when Thermistor_2 =>
                        Internal_Thermistor_Name := Thermistor_2;

                     when Thermistor_3 =>
                        Internal_Thermistor_Name := Thermistor_3;

                     when Thermistor_4 =>
                        Internal_Thermistor_Name := Thermistor_4;
                  end case;

                  if Accumulators (Internal_Thermistor_Name) > Accumulator_Type'Last / 470 * 431 then
                     declare
                        Most_Likely_Short : Thermistor_ADC_Input_Name := Thermistor_1;
                     begin
                        for T in Thermistor_ADC_Input_Name range Thermistor_1 .. Thermistor_4 loop
                           if Accumulators (T) > Accumulators (Most_Likely_Short) then
                              Most_Likely_Short := T;
                           end if;
                        end loop;
                        raise Bad_Reading_Error
                          with
                            "A thermistor wire appears to be shorted to something else (likely "
                            & Most_Likely_Short'Image
                            & ") (Reading too high).";
                     end;
                  end if;

                  if Accumulators (Internal_Thermistor_Name) <= Accumulators (Thermistor_Ground) then
                     raise Bad_Reading_Error
                       with
                         "A thermistor wire appears to be shorted to something else (likely "
                         & Thermistor'Image
                         & ") (Reading below ground).";
                  end if;

                  declare
                     Thermistor_ADC_Reading : constant Voltage := Voltage (Accumulators (Internal_Thermistor_Name));
                     Ground_ADC_Reading     : constant Voltage := Voltage (Accumulators (Thermistor_Ground));
                     --  The above are voltages but are not scaled correctly. This does not matter as they both share
                     --  the same linear scale and we will cancel out the voltage unit below.

                     Thermistor_Disconnected : constant Boolean :=
                       Accumulators (Internal_Thermistor_Name) > Accumulator_Type'Last / 470 * 429;

                     --  TODO: Make sure that the user can not input a configuration that results in expected
                     --  thermistor readings above 750 kohm.
                     Thermistor_Resistance : constant Resistance :=
                       (if Thermistor_Disconnected
                        then 1.0e9 * ohm
                        else
                          (86000.0 * ohm)
                          * (Thermistor_ADC_Reading - Ground_ADC_Reading)
                          / (43.0 * Voltage (Accumulator_Type'Last) - 47.0 * Thermistor_ADC_Reading));
                  begin
                     if Thermistor_Disconnected then
                        Last_Temperatures (Thermistor) := Bad_Reading_Indicator;

                        for Heater in Heater_Name loop
                           if Heater_Thermistors (Heater) = Thermistor then
                              raise Bad_Reading_Error
                                with
                                  Thermistor'Image
                                  & " appears disconnected. This thermistor is required for "
                                  & Heater'Image;
                           end if;
                        end loop;
                     else
                        Last_Temperatures (Thermistor) := Interpolate (Thermistor_Resistance, Thermistor);

                        if Last_Temperatures (Thermistor) = Bad_Reading_Indicator then
                           raise Bad_Reading_Error
                             with
                               "Thermistor reading out of range for "
                               & Thermistor'Image
                               & " (ADC = "
                               & Accumulators (Internal_Thermistor_Name)'Image
                               & ", Resistance = "
                               & Thermistor_Resistance'Image
                               & ")";
                        end if;
                     end if;
                  end;
               end;
            end loop;

            for Heater in Heater_Name loop
               Heaters.Update_Reading (Heater, Last_Temperatures (Heater_Thermistors (Heater)));
            end loop;

            Accumulators := (others => 0);

            Step := Accumulator_Step'First;
         else
            Step := @ + 1;
         end if;
      exception
         when E : others =>
            Last_Chance_Handler.Last_Chance_Handler (E);
      end End_Of_Sequence_Handler;
   end ADC_Handler;

end Thermistors;
