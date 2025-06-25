with Hardware_Configuration; use Hardware_Configuration;
with Messages;               use Messages;
with Physical_Types;         use Physical_Types;
with STM32.COMP;             use STM32.COMP;
with Init_Checkers;

package Fans is

   procedure Init;
   procedure Reconfigure (Fan : Fan_Name; PWM_Frequency : Fixed_Point_Fan_PWM_Frequency; Use_High_Side : Boolean);
   procedure Set_PWM (Fan : Fan_Name; Scale : Fixed_Point_PWM_Scale);
   function Get_PWM (Fan : Fan_Name) return PWM_Scale;
   function Get_Tach_Counter (Fan : Fan_Name) return Tach_Counter;
   procedure Update_Software_Fan_Counters;

private

   type Fan_PWMs is array (Fan_Name) of Fixed_Point_PWM_Scale;

   protected Fan_Handlers is
      procedure Init;
      procedure Reconfigure (Fan : Fan_Name; PWM_Frequency : Fixed_Point_Fan_PWM_Frequency; Use_High_Side : Boolean);
      procedure Set_PWM (Fan : Fan_Name; Scale : Fixed_Point_PWM_Scale);
      function Get_PWM (Fan : Fan_Name) return PWM_Scale;
      function Get_Tach_Counter (Fan : Fan_Name) return Tach_Counter;
   private
      Last_PWMs : Fan_PWMs := (others => 1.0);
   end Fan_Handlers;

   type Software_Fan_Counter is mod 2**32 with Atomic, Volatile;

   Software_Fan_Counters             : array (Fan_Name) of Software_Fan_Counter := (others => 0);
   Software_Fan_Counters_Last_States : array (Fan_Name) of Boolean := (others => False)
   with Atomic_Components, Volatile_Components;

   Init_Checker : Init_Checkers.Init_Checker;

end Fans;
