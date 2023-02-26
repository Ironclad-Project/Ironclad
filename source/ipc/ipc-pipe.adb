--  vfs-pipe.adb: Pipe creation and management.
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

with Ada.Unchecked_Deallocation;
with Arch.Snippets;

package body IPC.Pipe with SPARK_Mode => Off is
   procedure Free is new Ada.Unchecked_Deallocation
         (Pipe_Writer, Pipe_Writer_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
         (Pipe_Reader, Pipe_Reader_Acc);

   procedure Create_Pair
      (Write_End   : out Pipe_Writer_Acc;
       Read_End    : out Pipe_Reader_Acc;
       Is_Blocking : Boolean)
   is
   begin
      Write_End := new Pipe_Writer;
      Read_End  := new Pipe_Reader;
      Write_End.all := (
         Refcount    => 1,
         Mutex       => Lib.Synchronization.Unlocked_Semaphore,
         Is_Blocking => Is_Blocking,
         Data_Count  => 0,
         Data        => (others => 0),
         Reader      => Read_End
      );
      Read_End.all := (
         Refcount        => 1,
         Writer_Is_Ghost => False,
         Is_Blocking     => Is_Blocking,
         Other_End       => Write_End
      );
   end Create_Pair;

   function Is_Blocking (P : Pipe_Writer_Acc) return Boolean is
   begin
      return P.Is_Blocking;
   end Is_Blocking;

   function Is_Blocking (P : Pipe_Reader_Acc) return Boolean is
   begin
      return P.Is_Blocking;
   end Is_Blocking;

   procedure Set_Blocking (P : Pipe_Writer_Acc; B : Boolean) is
   begin
      Lib.Synchronization.Seize (P.Mutex);
      P.Is_Blocking := B;
      Lib.Synchronization.Release (P.Mutex);
   end Set_Blocking;

   procedure Set_Blocking (P : Pipe_Reader_Acc; B : Boolean) is
   begin
      P.Is_Blocking := B;
   end Set_Blocking;

   procedure Increase_Refcount (P : Pipe_Writer_Acc) is
   begin
      P.Refcount := P.Refcount + 1;
   end Increase_Refcount;

   procedure Increase_Refcount (P : Pipe_Reader_Acc) is
   begin
      P.Refcount := P.Refcount + 1;
   end Increase_Refcount;

   procedure Close (To_Close : in out Pipe_Writer_Acc) is
   begin
      To_Close.Refcount := To_Close.Refcount - 1;
      if To_Close.Refcount = 0 then
         if To_Close.Reader /= null then
            To_Close.Reader.Writer_Is_Ghost := True;
         else
            Free (To_Close);
            return;
         end if;
      end if;
   end Close;

   procedure Close (To_Close : in out Pipe_Reader_Acc) is
   begin
      To_Close.Refcount := To_Close.Refcount - 1;
      if To_Close.Refcount = 0 then
         if To_Close.Writer_Is_Ghost then
            Free (To_Close.Other_End);
         else
            To_Close.Other_End.Reader := null;
         end if;
         Free (To_Close);
         return;
      end if;
   end Close;

   function Read
      (To_Read     : Pipe_Reader_Acc;
       Count       : Unsigned_64;
       Destination : System.Address) return Unsigned_64
   is
      Len       : constant Natural := Natural (Count);
      Final_Len : Natural          := Len;
      Data      : Pipe_Data (1 .. Len) with Import, Address => Destination;
   begin
      if To_Read.Writer_Is_Ghost and To_Read.Other_End.Data_Count = 0 then
         return 0;
      end if;

      if To_Read.Is_Blocking then
         loop
            exit when To_Read.Other_End.Data_Count /= 0;
            Arch.Snippets.Pause;
         end loop;
      end if;

      Lib.Synchronization.Seize (To_Read.Other_End.Mutex);
      if Final_Len > To_Read.Other_End.Data_Count then
         Final_Len := To_Read.Other_End.Data_Count;
      end if;

      Data (1 .. Final_Len) := To_Read.Other_End.Data (1 .. Final_Len);
      for I in 1 .. Final_Len loop
         for J in
            To_Read.Other_End.Data'First .. To_Read.Other_End.Data'Last - 1
         loop
            To_Read.Other_End.Data (J) := To_Read.Other_End.Data (J + 1);
         end loop;
         To_Read.Other_End.Data_Count := To_Read.Other_End.Data_Count - 1;
      end loop;

      Lib.Synchronization.Release (To_Read.Other_End.Mutex);
      return Unsigned_64 (Final_Len);
   end Read;

   function Write
      (To_Write : Pipe_Writer_Acc;
       Count    : Unsigned_64;
       Source   : System.Address) return Unsigned_64
   is
      Len   : Natural := Natural (Count);
      Final : Natural;
      Data  : Pipe_Data (1 .. Len) with Import, Address => Source;
   begin
      if To_Write.Reader = null then
         return 0;
      end if;

      if To_Write.Data_Count = Pipe_Data_Len then
         if To_Write.Is_Blocking then
            loop
               exit when To_Write.Data_Count /= Pipe_Data_Len;
               Arch.Snippets.Pause;
            end loop;
         else
            return 0;
         end if;
      end if;

      Lib.Synchronization.Seize (To_Write.Mutex);
      if Len + To_Write.Data_Count > Pipe_Data_Len then
         Final := Pipe_Data_Len;
         Len   := Pipe_Data_Len - To_Write.Data_Count;
      else
         Final := To_Write.Data_Count + Len;
      end if;

      To_Write.Data (To_Write.Data_Count + 1 .. Final) := Data (1 .. Len);
      To_Write.Data_Count := Final;
      Lib.Synchronization.Release (To_Write.Mutex);
      return Unsigned_64 (Len);
   end Write;
end IPC.Pipe;
