with STM32.DMA;            use STM32.DMA;
with System;               use System;
with STM32.USARTs;         use STM32.USARTs;
with Messages;             use Messages;
with STM32.GPIO;           use STM32.GPIO;
with STM32.Device;         use STM32.Device;
with STM32;                use STM32;
with STM32.CRC;            use STM32.CRC;
with STM32.Timers;         use STM32.Timers;
with STM32.LPTimers;       use STM32.LPTimers;
with STM32.ADC;            use STM32.ADC;
with STM32.COMP;           use STM32.COMP;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Ada.Interrupts;       use Ada.Interrupts;
with STM32_SVD;
with STM32_SVD.USART;
with STM32_SVD.ADC;

package Hardware_Configuration is

   --  Self_Check

   Self_Check_CRC_Unit : CRC_32 renames CRC_Unit;
   --  Shared with Server_Communications.

   --  Server_Communications

   Comms_UART_DMA_RX_Controller : DMA_Controller renames DMA_1;
   Comms_UART_DMA_RX_Stream     : constant DMA_Stream_Selector := Stream_1;
   Comms_UART_DMA_RX_Priority   : constant DMA_Priority_Level := Priority_Very_High;
   Comms_UART_DMA_RX_Channel    : constant DMA_Channel_Selector := USART3_RX;

   Comms_CRC_Unit : CRC_32 renames CRC_Unit;
   --  Shared with Self_Check.

   Comms_UART           : USART renames USART_3;
   Comms_UART_TX_Pin    : constant GPIO_Point := PC10;
   Comms_UART_TX_Pin_AF : constant GPIO_Alternate_Function := GPIO_AF_USART3_7;
   Comms_UART_RX_Pin    : constant GPIO_Point := PC11;
   Comms_UART_RX_Pin_AF : constant GPIO_Alternate_Function := GPIO_AF_USART3_7;

   --  Steppers

   TMC_UART_DMA_RX_Controller : DMA_Controller renames DMA_1;
   TMC_UART_DMA_RX_Stream     : constant DMA_Stream_Selector := Stream_2;
   TMC_UART_DMA_RX_Priority   : constant DMA_Priority_Level := Priority_Low;
   TMC_UART_DMA_RX_Channel    : constant DMA_Channel_Selector := USART2_RX;

   TMC_UART_Internal : aliased STM32_SVD.USART.USART_Peripheral
   with Import, Volatile, Address => STM32_SVD.USART2_Base;
   TMC_UART          : USART renames USART_2;
   TMC_UART_Pin      : constant GPIO_Point := PB3;
   TMC_UART_Pin_AF   : constant GPIO_Alternate_Function := GPIO_AF_USART2_7;

   --  Input_Switches

   Switch_Points : constant array (Input_Switch_Name) of GPIO_Point :=
     (Endstop_1 => PB6, Endstop_2 => PC13, Endstop_3 => PC14, Endstop_4 => PC15, Stepper_Diag_0 => PA12);

   --  Step_Generator

   Step_Generator_Interrupt_Priority : constant Interrupt_Priority := Interrupt_Priority'Last_Valid;
   --  Step_Generator also uses HRTimer and all HRTimer output pins (PA8-PA11, PB12-PB15, PC6-PC9).

   --  Heaters

   Heater_Timers              : constant array (Heater_Name) of access Timer :=
     (Heater_1 => Timer_20'Access, Heater_2 => Timer_17'Access, Heater_3 => Timer_1'Access);
   Heater_Timer_Channels      : constant array (Heater_Name) of Timer_Channel :=
     (Heater_1 => Channel_1, Heater_2 => Channel_1, Heater_3 => Channel_4);
   Heater_Timer_Polarities    : constant array (Heater_Name) of Timer_Output_Compare_Polarity :=
     (Heater_1 => Low, Heater_2 => Low, Heater_3 => Low);
   Heater_Timer_Complementary : constant array (Heater_Name) of Boolean :=
     (Heater_1 => False, Heater_2 => False, Heater_3 => True);
   Heater_GPIO_Points         : constant array (Heater_Name) of GPIO_Point :=
     (Heater_1 => PB2, Heater_2 => PB5, Heater_3 => PC5);
   Heater_GPIO_AFs            : constant array (Heater_Name) of GPIO_Alternate_Function :=
     (Heater_1 => GPIO_AF_TIM20_3, Heater_2 => GPIO_AF_TIM17_10, Heater_3 => GPIO_AF_TIM1_6);
   --  Heaters package also uses IWDG.

   Internal_Heater_CS_DMA_Interrupt_Priority : constant Interrupt_Priority := Interrupt_Priority'Last_Valid - 2;
   Internal_Heater_CS_DMA_Interrupt_ID       : constant Interrupt_ID := DMA2_CH3_Interrupt;
   Internal_Heater_CS_DMA_Controller         : DMA_Controller renames DMA_2;
   Internal_Heater_CS_DMA_Stream             : constant DMA_Stream_Selector := Stream_3;
   Internal_Heater_CS_DMA_Priority           : constant DMA_Priority_Level := Priority_Very_High;
   Internal_Heater_CS_DMA_Channel            : constant DMA_Channel_Selector := ADC2;

   Internal_Heater_CS_ADC                      : Analog_To_Digital_Converter renames ADC_2;
   Internal_Heater_CS_ADC_Internal             : aliased STM32_SVD.ADC.ADC1_Peripheral
   with Volatile, Import, Address => STM32_SVD.ADC2_Base;
   Internal_Heater_CS_ADC_Overrun_Interrupt_ID : constant Interrupt_ID := ADC1_2_Interrupt;
   Internal_Heater_CS_ADC_Channels             : constant array (Internal_Heater_Name) of Analog_Input_Channel :=
     (Heater_1 => 5, Heater_2 => 10);
   Internal_Heater_CS_GPIO_Points              : constant array (Internal_Heater_Name) of GPIO_Point :=
     (Heater_1 => PC4, Heater_2 => PF1);

   Internal_Heater_OC_GPIO_Points : constant array (Internal_Heater_Name) of GPIO_Point :=
     (Heater_1 => PA13, Heater_2 => PB10);

   --  Thermistors

   Thermistor_DMA_Interrupt_Priority : constant Interrupt_Priority := Interrupt_Priority'Last_Valid - 1;
   Thermistor_DMA_Interrupt_ID       : constant Interrupt_ID := DMA2_CH2_Interrupt;
   Thermistor_DMA_Controller         : DMA_Controller renames DMA_2;
   Thermistor_DMA_Stream             : constant DMA_Stream_Selector := Stream_2;
   Thermistor_DMA_Priority           : constant DMA_Priority_Level := Priority_Very_High;
   Thermistor_DMA_Channel            : constant DMA_Channel_Selector := ADC1;

   type Thermistor_ADC_Input_Name is
     (Thermistor_1, Thermistor_2, Thermistor_3, Thermistor_4, Thermistor_VDD, Thermistor_Ground);

   Thermistor_ADC          : Analog_To_Digital_Converter renames ADC_1;
   Thermistor_ADC_Internal : aliased STM32_SVD.ADC.ADC1_Peripheral
   with Volatile, Import, Address => STM32_SVD.ADC1_Base;
   Thermistor_ADC_Channels : constant array (Thermistor_ADC_Input_Name) of Analog_Input_Channel :=
     (Thermistor_1      => 8,
      Thermistor_2      => 9,
      Thermistor_3      => 3,
      Thermistor_4      => 2,
      Thermistor_VDD    => 7,
      Thermistor_Ground => 1);
   Thermistor_GPIO_Points  : constant array (Thermistor_ADC_Input_Name) of GPIO_Point :=
     (Thermistor_1      => PC2,
      Thermistor_2      => PC3,
      Thermistor_3      => PA2,
      Thermistor_4      => PA1,
      Thermistor_VDD    => PC1,
      Thermistor_Ground => PA0);

   --  Fans

   Fan_Timers            : constant array (Fan_Name) of access Timer :=
     (Fan_1 => Timer_2'Access, Fan_2 => Timer_3'Access, Fan_3 => Timer_4'Access, Fan_4 => Timer_8'Access);
   Fan_Timer_LS_Channels : constant array (Fan_Name) of Timer_Channel :=
     (Fan_1 => Channel_4, Fan_2 => Channel_2, Fan_3 => Channel_2, Fan_4 => Channel_2);
   Fan_Timer_HS_Channels : constant array (Fan_Name) of Timer_Channel :=
     (Fan_1 => Channel_1, Fan_2 => Channel_1, Fan_3 => Channel_4, Fan_4 => Channel_1);
   Fan_LS_GPIO_Points    : constant array (Fan_Name) of GPIO_Point :=
     (Fan_1 => PA3, Fan_2 => PA4, Fan_3 => PB7, Fan_4 => PA14);
   Fan_HS_GPIO_Points    : constant array (Fan_Name) of GPIO_Point :=
     (Fan_1 => PA5, Fan_2 => PA6, Fan_3 => PB9, Fan_4 => PA15);
   Fan_LS_GPIO_AFs       : constant array (Fan_Name) of GPIO_Alternate_Function :=
     (Fan_1 => GPIO_AF_TIM2_1, Fan_2 => GPIO_AF_TIM3_2, Fan_3 => GPIO_AF_TIM4_2, Fan_4 => GPIO_AF_TIM8_5);
   Fan_HS_GPIO_AFs       : constant array (Fan_Name) of GPIO_Alternate_Function :=
     (Fan_1 => GPIO_AF_TIM2_1, Fan_2 => GPIO_AF_TIM3_2, Fan_3 => GPIO_AF_TIM4_2, Fan_4 => GPIO_AF_TIM8_2);

   FAN_ST_GPIO_Points : constant array (Fan_Name) of GPIO_Point :=
     (Fan_1 => PC0, Fan_2 => PB4, Fan_3 => PD2, Fan_4 => PC12);

   type Tach_Mux_Index is range 1 .. 4;

   type Tach_Config_Kind is (Timer_Kind, LPTimer_Kind, In_Step_Generator_Loop_Kind);

   type Tach_Config (Kind : Tach_Config_Kind := Timer_Kind) is record
      Point : GPIO_Point;
      Comp  : access Comparator;
      case Kind is
         when Timer_Kind =>
            Tim     : access Timer;
            Trigger : Timer_External_Trigger_Source;

         when LPTimer_Kind =>
            LPTim : access LPTimer;
            Clock : LPTimer_Input_Clock_Enum;

         when In_Step_Generator_Loop_Kind =>
            null;
      end case;
   end record;

   Tach_Configs : constant array (Fan_Name) of Tach_Config :=
     (Fan_1 => (Kind => In_Step_Generator_Loop_Kind, Point => PA7, Comp => Comp_2'Access),
      --  Fan_2 =>
      --    (Kind => Timer_Kind, Point => PB0, Comp => Comp_4'Access, Tim => Timer_5'Access, Trigger => Comp_4_Output),
      --  Fan_3 =>
      --    (Kind => LPTimer_Kind, Point => PB1, Comp => Comp_1'Access, LPTim => LPTimer_1'Access, Clock => Option_1),
      Fan_2 => (Kind => In_Step_Generator_Loop_Kind, Point => PB0, Comp => Comp_4'Access),
      Fan_3 => (Kind => In_Step_Generator_Loop_Kind, Point => PB1, Comp => Comp_1'Access),
      Fan_4 => (Kind => In_Step_Generator_Loop_Kind, Point => PB11, Comp => Comp_6'Access));

   --  MCU_Temperature uses ADC_5.

end Hardware_Configuration;
