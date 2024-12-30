--  lib-runtime.adb: Functions needed by the compiler.
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

with Lib.Messages;
with Lib.Panic;
with Interfaces; use Interfaces;

package body Lib.Runtime with SPARK_Mode => Off is
   --  Failing a check here would just make it infinitely recursive.
   pragma Suppress (All_Checks);

   procedure Last_Chance_Handler (File : System.Address; Line : Integer) is
      Line_Len    : Natural;
      Line_Buffer : Messages.Translated_String;
      File_String : String (1 .. Lib.C_String_Length (File))
         with Address => File, Import;
   begin
      Messages.Image (Unsigned_32 (Line), Line_Buffer, Line_Len);
      Lib.Panic.Hard_Panic
         ("Ada exception triggered at " & File_String & ":" &
          Line_Buffer (Line_Buffer'Last - Line_Len + 1 .. Line_Buffer'Last));
   end Last_Chance_Handler;
end Lib.Runtime;
