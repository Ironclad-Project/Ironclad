--  lib-cmdline.ads: Parsing command line options.
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

package Lib.Cmdline is
   --  The format of the command line is as such:
   --  arg1=val1 arg2=val2 arg3="val3 val4" ... argN=(")valN(")
   --  Double quotes used for values with spaces.

   --  Some notable keys specified in the documentation.
   Root_Key         : constant String := "root";       --  Device to use as /.
   Root_UUID_Key    : constant String := "rootuuid";   --  UUID to use as /.
   Init_Key         : constant String := "init";       --  Init to load.
   Init_Arg_Key     : constant String := "initargs";   --  init's arguments.
   No_Program_ASLR  : constant String := "noprogaslr"; --  Program ASLR.
   No_Location_ASLR : constant String := "nolocaslr";  --  Location ASLR.

   --  Get the value of a key.
   --  @param Cmdline Command line to search in.
   --  @param Key Key to search for.
   --  @return An allocated string pointer, or null in failure.
   procedure Get_Key_Value
      (Cmdline, Key : String;
       Returned     : out String;
       Found        : out Boolean;
       Length       : out Natural)
      with Pre => Key'Length /= 0 and Returned'First = 1;

   --  Check whether an option is present.
   --  @param Cmdline Command line to search in.
   --  @param Key Key to search for.
   --  @return True if found, or False if not found.
   function Is_Key_Present (Cmdline, Key : String) return Boolean
      with Pre => Cmdline'Length /= 0 and Key'Length /= 0;

private

   procedure Find_Key
      (Cmdline, Key : String;
       Found        : out Boolean;
       End_Index    : out Natural);
end Lib.Cmdline;
