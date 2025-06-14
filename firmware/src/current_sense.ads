with Messages;               use Messages;
with Hardware_Configuration; use Hardware_Configuration;
with Physical_Types;         use Physical_Types;

package Current_Sense is

   procedure Init;
   function Last_Reported_Current (Heater : Internal_Heater_Name) return Current;

private

   type ADC_16 is mod 2**16 with Size => 16;

   type DMA_Index is range 1 .. 16;

   type ADC_Results_Type is array (DMA_Index) of ADC_16 with Alignment => 2, Pack, Volatile, Volatile_Components;
   ADC_Results : aliased ADC_Results_Type;

   type Float_Reported_Currents is array (Internal_Heater_Name) of Current;

   type Accumulator_Step is range 1 .. 16;
   type Accumulator_Type is range 0 .. Integer (DMA_Index'Last) * Integer (Accumulator_Step'Last) * 2**16 - 1;

   protected ADC_Handler
   with Interrupt_Priority => Internal_Heater_CS_DMA_Interrupt_Priority
   is
      procedure Init;
      function Last_Reported_Current (Heater : Internal_Heater_Name) return Current;
   private
      Last_Currents  : Float_Reported_Currents := (others => 0.0 * amp);
      Init_Done      : Boolean := False;
      Accumulator    : Accumulator_Type := 0;
      Step           : Accumulator_Step := Accumulator_Step'First;
      Current_Heater : Internal_Heater_Name := Internal_Heater_Name'First;

      procedure Start_Transfer;
      procedure End_Of_Sequence_Handler
      with Attach_Handler => Internal_Heater_CS_DMA_Interrupt_ID;
      procedure ADC_Overrun_Handler
      with Attach_Handler => Internal_Heater_CS_ADC_Overrun_Interrupt_ID;
   end ADC_Handler;

end Current_Sense;
