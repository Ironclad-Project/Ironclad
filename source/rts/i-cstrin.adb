--  i-cstrin.adb: Utilities to interface with C strings.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

with Ada.Characters.Latin_1;
with System.Storage_Elements; use System.Storage_Elements;

package body Interfaces.C.Strings is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   pragma Warnings
      (GNATprove, Off, "through a potential alias",
       Reason => "No alias are taken");

   function Strlen (Addr : System.Address) return Natural is
      use System;
      Length : Natural := 0;
   begin
      if Addr = System.Null_Address then
         return 0;
      end if;

      loop
         pragma Loop_Variant (Increases => Length);
         declare
            C : constant Character
               with Address => Addr + Storage_Offset (Length), Import;
         begin
            exit when C = Ada.Characters.Latin_1.NUL or Length = Natural'Last;
            Length := Length + 1;
         end;
      end loop;
      return Length;
   end Strlen;
end Interfaces.C.Strings;
