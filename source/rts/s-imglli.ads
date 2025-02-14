--  s-imglli.ads: Integer printing version.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

with System.Unsigned_Types;

package System.Img_LLI with SPARK_Mode is
   subtype Long_Long_Unsigned is Unsigned_Types.Long_Long_Unsigned;

   procedure Image_Long_Long_Integer
     (V : Long_Long_Integer;
      S : in out String;
      P : out Natural);
end System.Img_LLI;
