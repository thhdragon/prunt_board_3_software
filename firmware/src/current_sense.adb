with STM32.GPIO;   use STM32.GPIO;
with STM32.Device; use STM32.Device;
with STM32.ADC;    use STM32.ADC;
with STM32.DMA;    use STM32.DMA;
with HAL;          use HAL;
with Thermistors;
with Last_Chance_Handler;

package body Current_Sense is

   procedure Init is
   begin
      ADC_Handler.Init;
   end Init;

   function Last_Reported_Current (Heater : Internal_Heater_Name) return Current is
   begin
      return ADC_Handler.Last_Reported_Current (Heater);
   end Last_Reported_Current;

   protected body ADC_Handler is
      procedure Init is
      begin
         if Init_Done then
            raise Constraint_Error with "Tried to call current sense Init more than once.";
         end if;

         if not Thermistors.Init_Is_Done then
            raise Constraint_Error with "Thermistor init must be called before current sense init as ADC is shared.";
         end if;

         Enable_Clock (Internal_Heater_CS_DMA_Controller);

         --  ADC common properties are configured by thermistors package.

         Calibrate (Internal_Heater_CS_ADC, Single_Ended);

         Configure_Unit (Internal_Heater_CS_ADC, ADC_Resolution_12_Bits, Right_Aligned);
         Internal_Heater_CS_ADC_Internal.CFGR2.ROVSE := True;  --  Regular oversampling
         Internal_Heater_CS_ADC_Internal.CFGR2.OVSS := 4;      --  4 bit shift
         Internal_Heater_CS_ADC_Internal.CFGR2.OVSR := 2#111#; --  256 samples

         Internal_Heater_CS_ADC_Internal.CFGR.OVRMOD := True;

         Configure_Regular_Conversions
           (This        => Internal_Heater_CS_ADC,
            Continuous  => True,
            Trigger     => Software_Triggered,
            Conversions =>
              [1 =>
                 (Channel => Internal_Heater_CS_ADC_Channels (Current_Heater), Sample_Time => Sample_640P5_Cycles)]);

         Enable (Internal_Heater_CS_ADC);

         for Heater in Internal_Heater_Name loop
            Configure_IO (Internal_Heater_CS_GPIO_Points (Heater), (Mode => Mode_Analog, Resistors => Floating));
         end loop;

         Configure
           (Internal_Heater_CS_DMA_Controller,
            Internal_Heater_CS_DMA_Stream,
            (Channel                      => Internal_Heater_CS_DMA_Channel,
             Direction                    => Peripheral_To_Memory,
             Increment_Peripheral_Address => False,
             Increment_Memory_Address     => True,
             Peripheral_Data_Format       => HalfWords,
             Memory_Data_Format           => HalfWords,
             Operation_Mode               => Normal_Mode,
             Priority                     => Internal_Heater_CS_DMA_Priority,
             Memory_Burst_Size            => Memory_Burst_Single,
             Peripheral_Burst_Size        => Peripheral_Burst_Single));

         Enable_DMA (Internal_Heater_CS_ADC);
         Enable_DMA_After_Last_Transfer (Internal_Heater_CS_ADC);
         Enable_Interrupts (Internal_Heater_CS_ADC, Overrun);

         Init_Done := True;

         Start_Transfer;
         Start_Conversion (Internal_Heater_CS_ADC);
      end Init;

      function Last_Reported_Current (Heater : Internal_Heater_Name) return Current is
      begin
         return Last_Currents (Heater);
      end Last_Reported_Current;

      procedure Start_Transfer is
      begin
         Start_Transfer_with_Interrupts
           (This               => Internal_Heater_CS_DMA_Controller,
            Stream             => Internal_Heater_CS_DMA_Stream,
            Source             => Data_Register_Address (Internal_Heater_CS_ADC),
            Destination        => ADC_Results'Address,
            Data_Count         => ADC_Results'Length,
            Enabled_Interrupts => [Transfer_Complete_Interrupt => True, others => False]);
      end Start_Transfer;

      procedure End_Of_Sequence_Handler is
      begin
         Clear_All_Status (Internal_Heater_CS_DMA_Controller, Internal_Heater_CS_DMA_Stream);

         for Result of ADC_Results loop
            Accumulator := @ + Accumulator_Type (Result);
         end loop;

         if Step = Accumulator_Step'Last then
            Stop_Conversion (Internal_Heater_CS_ADC);

            declare
               Next_Heater : constant Internal_Heater_Name :=
                 (if Current_Heater = Internal_Heater_Name'Last
                  then Internal_Heater_Name'First
                  else Internal_Heater_Name'Succ (Current_Heater));
            begin
               Configure_Regular_Conversions
                 (This        => Internal_Heater_CS_ADC,
                  Continuous  => True,
                  Trigger     => Software_Triggered,
                  Conversions =>
                    [1 =>
                       (Channel     => Internal_Heater_CS_ADC_Channels (Next_Heater),
                        Sample_Time => Sample_640P5_Cycles)]);

               Start_Transfer;
               Start_Conversion (Internal_Heater_CS_ADC);

               Last_Currents (Current_Heater) :=
                 (Dimensionless (Accumulator) / Dimensionless (Accumulator_Type'Last) - 0.5) * (3300.0 / 90.0 * amp);

               Current_Heater := Next_Heater;
               Accumulator := 0;
               Step := Accumulator_Step'First;
            end;
         else
            Step := @ + 1;
            Start_Transfer;
         end if;

      exception
         when E : others =>
            Last_Chance_Handler.Last_Chance_Handler (E);
      end End_Of_Sequence_Handler;

      procedure ADC_Overrun_Handler is
      begin
         Clear_Interrupt_Pending (Internal_Heater_CS_ADC, Overrun);
      end ADC_Overrun_Handler;
   end ADC_Handler;

end Current_Sense;
