--  s-atacco.ads: Address to access conversions.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

generic
   type Object (<>) is limited private;
package System.Address_To_Access_Conversions is
   pragma Preelaborate;
   pragma Compile_Time_Warning
     (Object'Unconstrained_Array,
      "Object is unconstrained array type, To_Pointer may not have bounds");

   type Object_Pointer is access all Object with Size => Standard'Address_Size;

   function To_Pointer (Value : Address) return Object_Pointer;
   function To_Address (Value : Object_Pointer) return Address
      with SPARK_Mode => Off;

   pragma Import (Intrinsic, To_Pointer);
   pragma Import (Intrinsic, To_Address);
end System.Address_To_Access_Conversions;
