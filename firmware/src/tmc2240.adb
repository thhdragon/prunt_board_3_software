package body TMC2240 is

   function Compute_CRC (Bytes : UART_Bytes_For_CRC) return UART_CRC is
      CRC : UART_CRC := 0;
   begin
      for I in reverse Bytes'Range loop
         declare
            Current_Byte : UART_Byte := Bytes (I);
         begin
            for J in 0 .. 7 loop
               if UART_Byte (CRC / 2**7) = (Current_Byte and 1) then
                  CRC := CRC * 2;
               else
                  CRC := (CRC * 2) xor 7;
               end if;
               Current_Byte := Current_Byte / 2;
            end loop;
         end;
      end loop;

      return CRC;
   end Compute_CRC;

   function Compute_CRC (Message : UART_Data_Message) return UART_CRC is
   begin
      return Compute_CRC (UART_Bytes_For_CRC (Message.Bytes (2 .. 8)));
   end Compute_CRC;

   function Compute_CRC (Message : UART_Query_Message) return UART_CRC is
   begin
      return Compute_CRC (UART_Bytes_For_CRC (Message.Bytes (2 .. 4)));
   end Compute_CRC;

end TMC2240;
