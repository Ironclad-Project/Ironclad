--  s-stoele.ads: Storage elements library.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

package System.Storage_Elements with Pure is
   type Storage_Offset is range
     -(2 ** (Integer'(Standard'Address_Size) - 1)) ..
     +(2 ** (Integer'(Standard'Address_Size) - 1)) - 1;

   subtype Storage_Count is Storage_Offset range 0 .. Storage_Offset'Last;
   type Storage_Element  is mod 2 ** Storage_Unit with Size => Storage_Unit;

   --  "This type is used by the expander to implement aggregate copy"
   --  Sure, whatever...
   pragma Universal_Aliasing (Storage_Element);

   function "+" (Left : Address; Right : Storage_Offset) return Address;
   pragma Convention (Intrinsic, "+");
   pragma Inline_Always ("+");
   pragma Pure_Function ("+");

   function "+" (Left : Storage_Offset; Right : Address) return Address;
   pragma Convention (Intrinsic, "+");
   pragma Inline_Always ("+");
   pragma Pure_Function ("+");

   function "-" (Left : Address; Right : Storage_Offset) return Address;
   pragma Convention (Intrinsic, "-");
   pragma Inline_Always ("-");
   pragma Pure_Function ("-");

   function "-" (Left, Right : Address) return Storage_Offset;
   pragma Convention (Intrinsic, "-");
   pragma Inline_Always ("-");
   pragma Pure_Function ("-");

   function "mod" (L : Address; R : Storage_Offset) return Storage_Offset
      with Pre => R /= 0;
   pragma Convention (Intrinsic, "mod");
   pragma Inline_Always ("mod");
   pragma Pure_Function ("mod");
   ----------------------------------------------------------------------------
   type Integer_Address is mod Memory_Size;

   function To_Address (Value : Integer_Address) return Address;
   pragma Convention (Intrinsic, To_Address);
   pragma Inline_Always (To_Address);
   pragma Pure_Function (To_Address);

   function To_Integer (Value : Address) return Integer_Address;
   pragma Convention (Intrinsic, To_Integer);
   pragma Inline_Always (To_Integer);
   pragma Pure_Function (To_Integer);
end System.Storage_Elements;
