--  lib-glue.adb: Calls generated for the compiler that we must fulfill.
--  Copyright (C) 2021 streaksu
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

with Ada.Characters.Latin_1;
with System.Storage_Elements; use System.Storage_Elements;
with Lib.Messages;
with Lib.Panic;

package body Lib.Glue is
   procedure Access_Check (File : System.Address; Line : Integer) is
   begin
      Print_Exception ("Access check failure", File, Line);
   end Access_Check;

   procedure Index_Check (File : System.Address; Line : Integer) is
   begin
      Print_Exception ("Index check failure", File, Line);
   end Index_Check;

   procedure Range_Check (File : System.Address; Line : Integer) is
   begin
      Print_Exception ("Range check failure", File, Line);
   end Range_Check;

   procedure Accessib_Check (File : System.Address; Line : Integer) is
   begin
      Print_Exception ("Accessibility check failure", File, Line);
   end Accessib_Check;

   procedure Overflow_Check (File : System.Address; Line : Integer) is
   begin
      Print_Exception ("Overflow check failure", File, Line);
   end Overflow_Check;

   procedure Large_Object_Check (File : System.Address; Line : Integer) is
   begin
      Print_Exception ("Large object check failure", File, Line);
   end Large_Object_Check;

   procedure Invalid_Data_Check (File : System.Address; Line : Integer) is
   begin
      Print_Exception ("Invalid data check failure", File, Line);
   end Invalid_Data_Check;

   procedure Divide_By_Zero_Check (File : System.Address; Line : Integer) is
   begin
      Print_Exception ("Divide by zero check failure", File, Line);
   end Divide_By_Zero_Check;

   procedure Print_Exception
      (Message      : String;
       File_Address : System.Address;
       Line_Number  : Integer) is
      File_Length : Integer := 0;
   begin
      --  Get length of the C string and turn it into Ada.
      Find_Length :
      loop
         declare
            C : Character;
            for C'Address use File_Address + Storage_Offset (File_Length);
         begin
            exit Find_Length when C = Ada.Characters.Latin_1.NUL;
            File_Length := File_Length + 1;
         end;
      end loop Find_Length;

      --  Declare the Ada string, print and panic.
      declare
         File_String : String (1 .. File_Length) with Address => File_Address;
      begin
         Lib.Messages.Put_Line ("");
         Lib.Messages.Put      ("Exception triggered at ");
         Lib.Messages.Put      (File_String);
         Lib.Messages.Put      (":");
         Lib.Messages.Put      (Line_Number);
         Lib.Messages.Put_Line ("");
         Lib.Panic.Hard_Panic  (Message);
      end;
   end Print_Exception;
end Lib.Glue;
