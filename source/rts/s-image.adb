--  s-image.adb: Generic integer imaging.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

package body System.Image is
   Conversion : constant String := "0123456789ABCDEF";

   procedure Image_Integer
     (V : Int;
      S : in out String;
      P : out Natural)
   is
   begin
      Image_Unsigned (UInt (abs V), S, P);
      if V < 0 then
         S (S'First + 1 .. P + 1) := S (S'First .. P);
         S (S'First) := '-';
         P := P + 1;
      end if;
   end Image_Integer;

   procedure Image_Unsigned
     (V : UInt;
      S : in out String;
      P : out Natural)
   is
      Base       : constant UInt := (if Do_Hex then 16 else 10);
      To_Convert :          UInt := V;
      Current    :       Natural := S'Last;
   begin
      S := [others => '0'];
      if To_Convert = 0 then
         P := 1;
         return;
      end if;

      while To_Convert /= 0 loop
         S (Current) := Conversion (Integer (To_Convert rem Base) + 1);
         To_Convert  := To_Convert / Base;
         Current     := Current - 1;
      end loop;

      P := S'Length - Current;
      S (S'First .. P) := S (S'Last - P + 1 .. S'Last);
   end Image_Unsigned;
end System.Image;