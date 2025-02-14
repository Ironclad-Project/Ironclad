--  s-imglli.adb: Integer printing version.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

package body System.Img_LLI is
   Conversion : constant            String := "0123456789ABCDEF";
   Base       : constant Long_Long_Integer := 16;

   procedure Image_Long_Long_Integer
     (V : Long_Long_Integer;
      S : in out String;
      P : out Natural)
   is
      To_Convert : Long_Long_Integer := abs V;
      Current    : Natural := S'Last;
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

      if V > 0 then
         S (S'First .. P) := S (S'Last - P + 1 .. S'Last);
      else
         S (S'First) := '-';
         S (S'First + 1 .. P + 1) := S (S'Last - P + 1 .. S'Last);
         P := P + 1;
      end if;
   end Image_Long_Long_Integer;
end System.Img_LLI;
