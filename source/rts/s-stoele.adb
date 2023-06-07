--  s-stoele.ads: Storage elements library.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

with Ada.Unchecked_Conversion;

package body System.Storage_Elements is
   pragma Suppress (All_Checks);

   function To_Addr is new Ada.Unchecked_Conversion (Storage_Offset, Address);
   function To_Off  is new Ada.Unchecked_Conversion (Address, Storage_Offset);

   function To_Address (Value : Integer_Address) return Address is
   begin
      return Address (Value);
   end To_Address;

   function To_Integer (Value : Address) return Integer_Address is
   begin
      return Integer_Address (Value);
   end To_Integer;

   function "+" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return To_Address (To_Integer (Left) + To_Integer (To_Addr (Right)));
   end "+";

   function "+" (Left : Storage_Offset; Right : Address) return Address is
   begin
      return To_Address (To_Integer (To_Addr (Left)) + To_Integer (Right));
   end "+";

   function "-" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return To_Address (To_Integer (Left) - To_Integer (To_Addr (Right)));
   end "-";

   function "-" (Left, Right : Address) return Storage_Offset is
   begin
      return To_Off (To_Address (To_Integer (Left) - To_Integer (Right)));
   end "-";

   function "mod" (L : Address; R : Storage_Offset) return Storage_Offset is
   begin
      return Storage_Offset (To_Integer (L) mod Integer_Address (R));
   end "mod";
end System.Storage_Elements;
