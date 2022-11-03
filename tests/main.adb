with Ada.Text_IO; use Ada.Text_IO;
with Cryptography.MD5; use Cryptography.MD5;
with Ada.Unchecked_Conversion;
with Ada.Characters.Latin_1;

procedure Main is
   type MD5_Exact_Str is new String (1 .. 64) with Size => 512;
   function Conv is new Ada.Unchecked_Conversion (
      Source => MD5_Exact_Str,
      Target => Cryptography.MD5.MD5_Block
   );

   --  Colors for printing.
   Text_Red   : constant String := Ada.Characters.Latin_1.ESC & "[31m";
   Text_Reset : constant String := Ada.Characters.Latin_1.ESC & "[0m";

   --  MD5 test array.
   type MD5_Test is record
      Data     : access MD5_Blocks;
      Expected : MD5_String;
   end record;
   type MD5_Test_Arr is array (Positive range <>) of MD5_Test;

   MD5_Test1 : constant MD5_Exact_Str :=
      "VIHP4l285gJ4IenB0EeLk9QLChfBx35QCJR11LY90XIsiyfW4qgSyESgtw2idle4";
   MD5_Test_1 : constant access MD5_Blocks :=
      new MD5_Blocks'((0 => Conv (MD5_Test1),
                       1 => (0 => 16#80#, 1 .. 13 => 0, 14 => 512, 15 => 0)));

   --  Empty string.
   MD5_Test_2 : constant access MD5_Blocks :=
      new MD5_Blocks'(0 => (0 => 16#80#, 1 .. 15 => 0));
   MD5_Tests : constant MD5_Test_Arr (1 .. 2) := (
      1 => (Data     => MD5_Test_1,
            Expected => "90adb0735901070d47c9d32cc10b975c"),
      2 => (Data     => MD5_Test_2,
            Expected => "d41d8cd98f00b204e9800998ecf8427e")
   );
begin
   Put_Line ("Cryptography");
   Put_Line ("-----------------------------------");
   Put_Line ("Testing MD5 using known vectors... ");
   for I in MD5_Tests'Range loop
      if MD5_Tests (I).Expected /= Digest (MD5_Tests (I).Data.all) then
         Put_Line (Text_Red & Integer'Image (I) & " failed" & Text_Reset);
      end if;
   end loop;
end Main;
