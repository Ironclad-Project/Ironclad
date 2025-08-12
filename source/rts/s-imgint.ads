--  s-imgint.ads: Integer printing version.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

with System.Image;
with System.Unsigned_Types;

package System.Img_Int with Pure is
   subtype Unsigned is Unsigned_Types.Unsigned;
   package Impl is new Image (Integer, Unsigned, False);
   procedure Image_Integer
      (V : Integer;
       S : out String;
       P : out Natural)
      renames Impl.Image_Integer;
end System.Img_Int;
