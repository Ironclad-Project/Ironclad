--  main.adb: Main test runner.
--  Copyright (C) 2023 streaksu
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Exceptions; use Ada.Exceptions;
with Cryptography_Tests; use Cryptography_Tests;
with Lib_Tests; use Lib_Tests;

procedure Main is
   Begin_Col    : constant Ada.Text_IO.Count := 1;
   Result_Col   : constant Ada.Text_IO.Count := 31;
   Text_Success : constant String := Ada.Characters.Latin_1.ESC & "[32m";
   Text_Fail    : constant String := Ada.Characters.Latin_1.ESC & "[31m";
   Text_Reset   : constant String := Ada.Characters.Latin_1.ESC & "[0m";

   type Test_Name is access String;
   type Test_Data is record
      Name : Test_Name;
      Test : access procedure;
   end record;

   Failed_Num : Natural := 0;
   Tests : constant array (1 .. 6) of Test_Data := (
      (new String'("Cryptography - Chacha20"), Run_Chacha20_Tests'Access),
      (new String'("Cryptography - MD5"),      Run_MD5_Tests'Access),
      (new String'("Lib - Cmdline"),           Run_Cmdline_Tests'Access),
      (new String'("Lib - Alignment"),         Run_Alignment_Tests'Access),
      (new String'("Lib - Math"),              Run_Math_Tests'Access),
      (new String'("Lib - Hashing"),           Run_Hashing_Tests'Access)
   );
begin
   for Test of Tests loop
      begin
         Ada.Text_IO.Set_Col (Begin_Col);
         Ada.Text_IO.Put (Test.Name.all & " ");
         Ada.Text_IO.Set_Col (Result_Col);
         Test.Test.all; --  We catch this.
         Ada.Text_IO.Put (Text_Success & "Passed");
      exception
         when Error : others =>
            Ada.Text_IO.Put
               (Text_Fail & "Failed (" & Exception_Message (Error) & ")");
            Failed_Num := Failed_Num + 1;
      end;
      Ada.Text_IO.Put_Line (Text_Reset);
   end loop;

   Ada.Text_IO.Put ("Of " & Integer'Image (Tests'Length) & " tests, ");
   if Failed_Num = 0 then
      Ada.Text_IO.Put (Text_Success);
   else
      Ada.Text_IO.Put (Text_Fail);
   end if;
   Ada.Text_IO.Put_Line (Integer'Image (Failed_Num) & " failed" & Text_Reset);
end Main;
