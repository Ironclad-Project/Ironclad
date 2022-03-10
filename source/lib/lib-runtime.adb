--  lib-runtime.adb: Functions needed by the compiler.
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

with Interfaces; use Interfaces;
with Lib.Messages;
with Lib.Panic;
with System.Storage_Elements; use System.Storage_Elements;
with Lib;

package body Lib.Runtime is
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

   procedure Length_Check (File : System.Address; Line : Integer) is
   begin
      Print_Exception ("Length check failure", File, Line);
   end Length_Check;

   procedure Divide_By_Zero_Check (File : System.Address; Line : Integer) is
   begin
      Print_Exception ("Divide by zero check failure", File, Line);
   end Divide_By_Zero_Check;

   function MemCmp (S1, S2 : System.Address; Size : size_t) return int is
      Str1 : array (1 .. Size) of Unsigned_8 with Address => S1;
      Str2 : array (1 .. Size) of Unsigned_8 with Address => S2;
   begin
      for I in 1 .. Size loop
         if Str1 (I) /= Str2 (I) then
            if Str1 (I) < Str2 (I) then
               return -1;
            else
               return 1;
            end if;
         end if;
      end loop;
      return 0;
   end MemCmp;

   function MemCpy
      (Desto, Source : System.Address;
       Size          : size_t) return System.Address
   is
      Dst : array (1 .. Size) of Unsigned_8 with Address => Desto;
      Src : array (1 .. Size) of Unsigned_8 with Address => Source;
   begin
      for I in 1 .. Size loop
         Dst (I) := Src (I);
      end loop;
      return Desto;
   end MemCpy;

   function MemMove
      (Desto, Source : System.Address;
       Size          : size_t) return System.Address
   is
      Dst : array (1 .. Size) of Unsigned_8 with Address => Desto;
      Src : array (1 .. Size) of Unsigned_8 with Address => Source;
      Desto_Int  : constant Integer_Address := To_Integer (Desto);
      Source_Int : constant Integer_Address := To_Integer (Source);
   begin
      if Source_Int > Desto_Int then
         for I in Dst'Range loop
            Dst (I) := Src (I);
         end loop;
      elsif Source_Int < Desto_Int then
         for I in reverse Dst'Range loop
            Dst (I) := Src (I);
         end loop;
      end if;
      return Desto;
   end MemMove;

   function MemSet
      (Desto : System.Address;
       Value : Integer;
       Size  : size_t) return System.Address
   is
      Dst : array (1 .. Size) of Unsigned_8 with Address => Desto;
   begin
      for I in Dst'Range loop
         Dst (I) := Unsigned_8 (Value);
      end loop;
      return Desto;
   end MemSet;

   procedure Print_Exception
      (Message      : String;
       File_Address : System.Address;
       Line_Number  : Integer)
   is
      File_Length : constant Natural := Lib.C_String_Length (File_Address);
      File_String : String (1 .. File_Length) with Address => File_Address;
   begin
      Lib.Messages.Put_Line ("");
      Lib.Messages.Put      ("Exception triggered at ");
      Lib.Messages.Put      (File_String);
      Lib.Messages.Put      (":");
      Lib.Messages.Put      (Line_Number);
      Lib.Messages.Put_Line ("");
      Lib.Panic.Hard_Panic  (Message);
   end Print_Exception;
end Lib.Runtime;
