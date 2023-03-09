--  ipc-pty.adb: Pipe creation and management.
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

with Ada.Unchecked_Deallocation;

package body IPC.PTY with SPARK_Mode => Off is
   procedure Free is new Ada.Unchecked_Deallocation (Primary,   Primary_Acc);
   procedure Free is new Ada.Unchecked_Deallocation (Secondary, Secondary_Acc);

   procedure Create_Pair
      (Primary_End   : out Primary_Acc;
       Secondary_End : out Secondary_Acc;
       Termios       : Devices.TermIOs.Main_Data;
       Window_Size   : Devices.TermIOs.Win_Size)
   is
      Reader1, Reader2 : Pipe.Pipe_Reader_Acc;
      Writer1, Writer2 : Pipe.Pipe_Writer_Acc;
   begin
      Pipe.Create_Pair (Writer1, Reader1, True);
      Pipe.Create_Pair (Writer2, Reader2, True);
      Primary_End := new Primary'
         (Mutex               => Lib.Synchronization.Unlocked_Semaphore,
          Reader_To_Secondary => Reader2,
          Reader_To_Primary   => Reader1,
          Writer_To_Secondary => Writer2,
          Writer_To_Primary   => Writer1,
          Secondary           => null,
          Term_Info           => Termios,
          Term_Size           => Window_Size,
          Refcount            => 1);
      Secondary_End := new Secondary'
         (Mutex            => Lib.Synchronization.Unlocked_Semaphore,
          Primary_End      => Primary_End,
          Primary_Is_Ghost => False,
          Refcount         => 1);
      Primary_End.Secondary := Secondary_End;
   end Create_Pair;

   procedure Increase_Refcount (P : Primary_Acc) is
   begin
      Lib.Synchronization.Seize (P.Mutex);
      P.Refcount := P.Refcount + 1;
      Lib.Synchronization.Release (P.Mutex);
   end Increase_Refcount;

   procedure Increase_Refcount (P : Secondary_Acc) is
   begin
      Lib.Synchronization.Seize (P.Mutex);
      P.Refcount := P.Refcount + 1;
      Lib.Synchronization.Release (P.Mutex);
   end Increase_Refcount;

   procedure Close (Closed : in out Primary_Acc) is
   begin
      Lib.Synchronization.Seize (Closed.Mutex);
      if Closed.Secondary /= null then
         Lib.Synchronization.Seize (Closed.Secondary.Mutex);
      end if;

      Closed.Refcount := Closed.Refcount - 1;
      if Closed.Refcount = 0 then
         if Closed.Secondary /= null then
            Closed.Secondary.Primary_Is_Ghost := True;
         else
            Free_Primary (Closed);
            return;
         end if;
      end if;

      if Closed.Secondary /= null then
         Lib.Synchronization.Release (Closed.Secondary.Mutex);
      end if;
      Lib.Synchronization.Release (Closed.Mutex);
   end Close;

   procedure Close (Closed : in out Secondary_Acc) is
   begin
      Lib.Synchronization.Seize (Closed.Mutex);
      Lib.Synchronization.Seize (Closed.Primary_End.Mutex);

      Closed.Refcount := Closed.Refcount - 1;
      if Closed.Refcount = 0 then
         if Closed.Primary_Is_Ghost then
            Free_Primary (Closed.Primary_End);
         else
            Closed.Primary_End.Secondary := null;
            Lib.Synchronization.Release (Closed.Primary_End.Mutex);
         end if;
         Free (Closed);
         return;
      end if;

      Lib.Synchronization.Release (Closed.Primary_End.Mutex);
      Lib.Synchronization.Release (Closed.Mutex);
   end Close;

   procedure Free_Primary (Prim : in out Primary_Acc) is
   begin
      Pipe.Close (Prim.Reader_To_Secondary);
      Pipe.Close (Prim.Reader_To_Primary);
      Pipe.Close (Prim.Writer_To_Secondary);
      Pipe.Close (Prim.Writer_To_Primary);
      Free (Prim);
   end Free_Primary;

   function Read
      (To_Read     : Primary_Acc;
       Count       : Unsigned_64;
       Destination : System.Address) return Unsigned_64
   is
   begin
      return Pipe.Read (To_Read.Reader_To_Primary, Count, Destination);
   end Read;

   function Write
      (To_Write : Primary_Acc;
       Count    : Unsigned_64;
       Source   : System.Address) return Unsigned_64
   is
   begin
      return Pipe.Write (To_Write.Writer_To_Secondary, Count, Source);
   end Write;

   function IO_Control
      (File     : Primary_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      use Devices.TermIOs;
      Result_Info : Main_Data with Import, Address => Argument;
      Result_Size : Win_Size  with Import, Address => Argument;
   begin
      case Request is
         when TCGETS                     => Result_Info    := File.Term_Info;
         when TCSETS | TCSETSW | TCSETSF => File.Term_Info := Result_Info;
         when TIOCGWINSZ                 => Result_Size    := File.Term_Size;
         when TIOCSWINSZ                 => File.Term_Size := Result_Size;
         when others                     => return False;
      end case;
      return True;
   end IO_Control;

   function Read
      (To_Read     : Secondary_Acc;
       Count       : Unsigned_64;
       Destination : System.Address) return Unsigned_64
   is
      Primary_End : Primary_Acc renames To_Read.Primary_End;
   begin
      return Pipe.Read (Primary_End.Reader_To_Secondary, Count, Destination);
   end Read;

   function Write
      (To_Write : Secondary_Acc;
       Count    : Unsigned_64;
       Source   : System.Address) return Unsigned_64
   is
      Primary_End : Primary_Acc renames To_Write.Primary_End;
   begin
      return Pipe.Write (Primary_End.Writer_To_Primary, Count, Source);
   end Write;

   function IO_Control
      (File     : Secondary_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
   begin
      return IO_Control (File.Primary_End, Request, Argument);
   end IO_Control;
end IPC.PTY;
