--  s-image.ads: Generic integer imaging.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

generic
   type Int  is range <>;
   type UInt is mod <>;
   Do_Hex : Boolean;
package System.Image with Pure is
   procedure Image_Integer
     (V : Int;
      S : in out String;
      P : out Natural);
   procedure Image_Unsigned
     (V : UInt;
      S : in out String;
      P : out Natural);
end System.Image;
