--  lib_tests.adb: Lib-related unit tests.
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

with Interfaces; use Interfaces;
with Lib.Alignment;
with Lib.Cmdline; use Lib.Cmdline;
with Lib;

package body Lib_Tests is
   procedure Run_Cmdline_Tests is
      Example : constant String := "example1 example2=3 hi supreader aa z=321";
      Ret     : String (1 .. 60);
      Ret_Len : Natural;
      Found   : Boolean;
   begin
      if not Is_Key_Present (Example, "example1")  or
         not Is_Key_Present (Example, "example2")  or
         not Is_Key_Present (Example, "hi")        or
         not Is_Key_Present (Example, "supreader") or
         not Is_Key_Present (Example, "a")         or
         not Is_Key_Present (Example, "z")         or
         Is_Key_Present (Example, "boooo")         or
         Is_Key_Present (Example, "sugar")
      then
         raise Lib_Exception with "Individual keys";
      end if;

      Get_Parameter
         (Cmdline  => Example,
          Key      => "example2",
          Returned => Ret,
          Found    => Found,
          Length   => Ret_Len);
      if not Found or else Ret (1 .. Ret_Len) /= "3" then
         raise Lib_Exception with "First key do not match";
      end if;

      Get_Parameter
         (Cmdline  => Example,
          Key      => "z",
          Returned => Ret,
          Found    => Found,
          Length   => Ret_Len);
      if not Found or else Ret (1 .. Ret_Len) /= "321" then
         raise Lib_Exception with "Second key does not match";
      end if;
   end Run_Cmdline_Tests;

   procedure Run_Alignment_Tests is
      package Align is new Lib.Alignment (Unsigned_32);
   begin
      if Align.Align_Up   (16#823#,   16#1000#) /= 16#1000# or
         Align.Align_Down (16#2401#, 16#20000#) /= 16#0#
      then
         raise Lib_Exception with "Does not align properly";
      end if;

      if Align.Divide_Round_Up (3, 2) /= 2 or
         Align.Divide_Round_Up (0, 2) /= 0
      then
         raise Lib_Exception with "Does not round up properly";
      end if;
   end Run_Alignment_Tests;

   procedure Run_Math_Tests is
   begin
      if Lib.Least_Common_Multiple (12, 18)   /= 36 or
         Lib.Least_Common_Multiple (-6, 14)   /= 42 or
         Lib.Least_Common_Multiple (35, 0)    /= 00 or
         Lib.Least_Common_Multiple (-10, -14) /= -70
      then
         raise Lib_Exception with "LCM test failed";
      end if;
   end Run_Math_Tests;
end Lib_Tests;
