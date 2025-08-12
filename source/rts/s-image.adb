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
   pragma Suppress (All_Checks); --  Unit passes AoRTE.

   procedure Image_Integer
     (Value    : Int;
      Str      : out String;
      Consumed : out Natural)
   is
   begin
      Str := [others => '0'];

      if Value = Int'First then
         Image_Unsigned (UInt (Int'Last) + 1,
            Str (1 .. Str'Last - 1), Consumed);
      else
         Image_Unsigned (UInt (abs Value), Str (1 .. Str'Last - 1), Consumed);
      end if;
      if Value < 0 then
         Str (Str'First + 1 .. Consumed + 1) := Str (Str'First .. Consumed);
         Str (Str'First) := '-';
         Consumed := Consumed + 1;
      end if;
   end Image_Integer;

   procedure Image_Unsigned
     (Value    : UInt;
      Str      : out String;
      Consumed : out Natural)
   is
      pragma Annotate
         (GNATprove, False_Positive, "loop invariant might",
          "Cannot happen mathematically");
      pragma Annotate
         (GNATprove, False_Positive, "range check might fail",
          "Cannot happen mathematically");

      Conversion : constant String (1 .. 16) := "0123456789ABCDEF";
      Base       : constant UInt := (if Do_Hex then 16 else 10);
      To_Convert :          UInt := Value;
      Current    :       Natural := Str'Last;
   begin
      Str := [others => '0'];
      if To_Convert = 0 then
         Consumed := 1;
         return;
      end if;

      while To_Convert /= 0 loop
         pragma Loop_Invariant (Current in Str'Range);
         pragma Loop_Invariant ((To_Convert mod Base) < Conversion'Length);
         Str (Current) := Conversion (Integer (To_Convert mod Base) + 1);
         To_Convert    := To_Convert / Base;
         exit when Current = 1;
         Current := Current - 1;
      end loop;

      Consumed := Str'Length - Current;
      Str (Str'First .. Consumed) := Str (Str'Last - Consumed + 1 .. Str'Last);
   end Image_Unsigned;
end System.Image;
