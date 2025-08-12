--  s-stoele.ads: Storage elements library.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

with Ada.Unchecked_Conversion;

package body System.Storage_Elements is
   pragma Suppress (All_Checks); --  Unit passes AoRTE.

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
      pragma Annotate
         (GNATprove, False_Positive, "divide by zero might fail",
          "Cannot happen, R not being 0 is part of the preconditions");
      pragma Annotate
         (GNATprove, False_Positive, "cannot prove upper bound",
          "Cannot happen mathematically");
   begin
      --  Need to do this awkward stuff to make sure the abs doesnt overflow.
      if R = Storage_Offset'First then
         return Storage_Offset (To_Integer (L) mod
                (Integer_Address (Storage_Offset'Last) + 1));
      else
         return Storage_Offset (To_Integer (L) mod Integer_Address (abs R));
      end if;
   end "mod";
end System.Storage_Elements;
