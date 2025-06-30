with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;
with Server_Communication;
with Step_Generator;
with Steppers;
with Input_Switches;
with Thermistors;
with Heaters;
with Fans;
with Ada.Real_Time; use Ada.Real_Time;
with System;
with Self_Check;
with MCU_Temperature;
with Current_Sense;

with Last_Chance_Handler;
pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when an exception is propagated. We need it in
--  the executable, therefore it must be somewhere in the closure of the context clauses.

procedure Prunt_Board_3_Firmware is
   pragma Priority (System.Priority'First);
   --  DMA uses polling, so this task has to have a low priority. Even without polling it still makes sense to use a
   --  low priority here so communication will time out in the case that the MCU is overloaded.
   --
   --  TODO: Use interrupts instead of polling.
begin
   Enable_Clock (GPIO_A);
   Enable_Clock (GPIO_B);
   Enable_Clock (GPIO_C);
   Enable_Clock (GPIO_D);
   Enable_Clock (GPIO_F);

   Heaters.Make_Safe;

   --  Unused floating pins and BOOT0.
   Configure_IO (Points => (PB8, PG10), Config => (Mode => Mode_In, Resistors => Pull_Down));

   --  Always start server communication first so exceptions can be reported.
   Server_Communication.Init;

   if not Self_Check.Current_Bank_Is_Valid then
      raise Constraint_Error with "Integrity check failed. Manual flashing is required.";
   end if;

   Heaters.Make_Safe;
   Fans.Init;
   Input_Switches.Init;
   Steppers.Init;
   Step_Generator.Init;

   delay until Clock + Seconds (1); --  Ensure voltages have time to come up before ADC calibration.

   Thermistors.Init;
   MCU_Temperature.Init;

   Server_Communication.Run;

end Prunt_Board_3_Firmware;
