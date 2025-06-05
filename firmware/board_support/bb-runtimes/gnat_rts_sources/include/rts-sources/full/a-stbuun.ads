------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    ADA.STRINGS.TEXT_BUFFERS.UNBOUNDED                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Text_Buffers.Bounded;

package Ada.Strings.Text_Buffers.Unbounded with
   Pure
is

   subtype Buffer_Type is Text_Buffers.Bounded.Buffer_Type (200);

   function Get (Buffer : in out Buffer_Type) return String;

   function Wide_Get (Buffer : in out Buffer_Type) return Wide_String;

   function Wide_Wide_Get
     (Buffer : in out Buffer_Type) return Wide_Wide_String;

   function Get_UTF_8
     (Buffer : in out Buffer_Type) return UTF_Encoding.UTF_8_String;

   function Wide_Get_UTF_16
     (Buffer : in out Buffer_Type) return UTF_Encoding.UTF_16_Wide_String;

end Ada.Strings.Text_Buffers.Unbounded;
