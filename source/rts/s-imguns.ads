--  s-imguns.ads: Unsigned printing version.
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

package System.Img_UNS with SPARK_Mode is
   subtype Unsigned is Unsigned_Types.Unsigned;
   package Impl is new Image (Integer, Unsigned, False);
   procedure Image_Unsigned
      (V : Unsigned;
       S : in out String;
       P : out Natural)
      renames Impl.Image_Unsigned;
end System.Img_UNS;
