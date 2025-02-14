--  s-imgllu.adb: Integer printing version.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception

package body System.Img_LLU is
   Conversion : constant             String := "0123456789ABCDEF";
   Base       : constant Long_Long_Unsigned := 16;

   procedure Image_Long_Long_Unsigned
     (V : Long_Long_Unsigned;
      S : in out String;
      P : out Natural)
   is
      use System.Unsigned_Types;

      To_Convert : Long_Long_Unsigned := V;
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
      S (S'First .. P) := S (S'Last - P + 1 .. S'Last);
   end Image_Long_Long_Unsigned;
end System.Img_LLU;
