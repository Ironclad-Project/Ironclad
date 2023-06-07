--  a-unccon.ads: Unchecked conversion library.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

generic
   type Source (<>) is limited private;
   type Target (<>) is limited private;
function Ada.Unchecked_Conversion (S : Source) return Target with Pure;

--  If this go in the with instead, it detonates, idk why.
pragma Import (Intrinsic, Ada.Unchecked_Conversion);
