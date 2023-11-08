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
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces;              use Interfaces;

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
   ----------------------------------------------------------------------------
   function MemCmp (S1, S2 : System.Address; Size : size_t) return int is
      Str1 : array (1 .. Size) of Unsigned_8 with Address => S1, Import;
      Str2 : array (1 .. Size) of Unsigned_8 with Address => S2, Import;
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
      Dst : array (1 .. Size) of Unsigned_8 with Address => Desto,  Import;
      Src : array (1 .. Size) of Unsigned_8 with Address => Source, Import;
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
      Dst : array (1 .. Size) of Unsigned_8 with Address => Desto,  Import;
      Src : array (1 .. Size) of Unsigned_8 with Address => Source, Import;
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
      Val : constant Unsigned_8 := Unsigned_8 (Value);
      Dst : array (1 .. Size) of Unsigned_8 with Address => Desto, Import;
   begin
      for C of Dst loop
         C := Val;
      end loop;
      return Desto;
   end MemSet;
end Lib.Runtime;
