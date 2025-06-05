------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    ADA.STRINGS.TEXT_BUFFERS.UNBOUNDED                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2023, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body Ada.Strings.Text_Buffers.Unbounded is

   function Get (Buffer : in out Buffer_Type) return String is
   begin
      return Bounded.Get_UTF_8 (Buffer);
   end Get;

   function Wide_Get (Buffer : in out Buffer_Type) return Wide_String is
   begin
      return Bounded.Wide_Get (Buffer);
   end Wide_Get;

   function Wide_Wide_Get (Buffer : in out Buffer_Type) return Wide_Wide_String
   is
   begin
      return Bounded.Wide_Wide_Get (Buffer);
   end Wide_Wide_Get;

   function Get_UTF_8
     (Buffer : in out Buffer_Type) return UTF_Encoding.UTF_8_String
   is
   begin
      return Bounded.Get_UTF_8 (Buffer);
   end Get_UTF_8;

   function Wide_Get_UTF_16
     (Buffer : in out Buffer_Type) return UTF_Encoding.UTF_16_Wide_String
   is
   begin
      return Bounded.Wide_Get_UTF_16 (Buffer);
   end Wide_Get_UTF_16;

end Ada.Strings.Text_Buffers.Unbounded;
