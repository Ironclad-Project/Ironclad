--  i-c.ads: C interface library.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

package Interfaces.C with Pure is
   --  Broad limits from <limits.h> that might be useful.
   CHAR_BIT  : constant := 8;
   SCHAR_MIN : constant := -128;
   SCHAR_MAX : constant := 127;
   UCHAR_MAX : constant := 255;

   --  The bulk of the types.
   type int   is new Integer;
   type short is new Short_Integer;
   type long  is range -(2 ** (Long_Integer'Size - Integer'(1))) ..
                       +(2 ** (Long_Integer'Size - Integer'(1))) - 1;
   type signed_char    is range SCHAR_MIN .. SCHAR_MAX with Size => CHAR_BIT;
   type unsigned       is mod 2 ** int'Size;
   type unsigned_short is mod 2 ** short'Size;
   type unsigned_long  is mod 2 ** long'Size;
   type unsigned_char  is mod (UCHAR_MAX + 1) with Size => CHAR_BIT;
   subtype plain_char  is unsigned_char;
   type ptrdiff_t is range -(2 ** (Standard'Address_Size - Integer'(1))) ..
                           +(2 ** (Standard'Address_Size - Integer'(1)) - 1);
   type size_t is mod 2 ** Standard'Address_Size;
   type char is new Character;
   type char_array is array (size_t range <>) of aliased char with
      Component_Size => CHAR_BIT;
   type wchar_t is new Wide_Character with Size => Standard'Wchar_T_Size;
   type wchar_array is array (size_t range <>) of aliased wchar_t;
   type char16_t is new Wide_Character;
   type char16_array is array (size_t range <>) of aliased char16_t;
   type char32_t is new Wide_Wide_Character;
   type char32_array is array (size_t range <>) of aliased char32_t;
end Interfaces.C;
