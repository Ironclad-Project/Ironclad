--  i-cstrin.ads: Utilities to interface with C strings.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

with System;

package Interfaces.C.Strings is
   --  Get the length of a C-Style string.
   --  @param Addr Address of the C-Style, NUL-terminated string to check.
   --  @return Length of the passed string.
   function Strlen (Addr : System.Address) return Natural;
end Interfaces.C.Strings;
