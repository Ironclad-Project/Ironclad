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
      (Value    : Int;
       Str      : out String;
       Consumed : out Natural)
      with Pre  => Str'First = 1 and Str'Length = 21,
           Post => Consumed <= Str'Length;

   procedure Image_Unsigned
      (Value    : UInt;
       Str      : out String;
       Consumed : out Natural)
      with Pre  => Str'First = 1 and Str'Length = 20,
           Post => Consumed <= Str'Length;
end System.Image;
