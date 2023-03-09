--  ipc-pty.ads: Pipe creation and management.
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

with System;
with Interfaces; use Interfaces;
with Devices.TermIOs;
with IPC.Pipe;
with Lib.Synchronization;

package IPC.PTY with SPARK_Mode => Off is
   type Primary       is private;
   type Secondary     is private;
   type Primary_Acc   is access Primary;
   type Secondary_Acc is access Secondary;

   --  Create a fresh PTY pair.
   procedure Create_Pair
      (Primary_End   : out Primary_Acc;
       Secondary_End : out Secondary_Acc;
       Termios       : Devices.TermIOs.Main_Data;
       Window_Size   : Devices.TermIOs.Win_Size);

   --  Close the passed end, and do preparations for the other end.
   --  Both ends must be closed individually.
   --  If the writing end is closed but the reader end isnt, the reader will
   --  be allowed to read until the end of the held data.
   procedure Increase_Refcount (P : Primary_Acc)   with Pre => P /= null;
   procedure Increase_Refcount (P : Secondary_Acc) with Pre => P /= null;
   procedure Close (Closed : in out Primary_Acc)   with Pre => Closed /= null;
   procedure Close (Closed : in out Secondary_Acc) with Pre => Closed /= null;

   --  Read, write, and ioctl for the primary pty.
   function Read
      (To_Read     : Primary_Acc;
       Count       : Unsigned_64;
       Destination : System.Address) return Unsigned_64
      with Pre => To_Read /= null;

   function Write
      (To_Write : Primary_Acc;
       Count    : Unsigned_64;
       Source   : System.Address) return Unsigned_64
      with Pre => To_Write /= null;

   function IO_Control
      (File     : Primary_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean with Pre => File /= null;

   --  Read, write, and ioctl for the secondary pty.
   function Read
      (To_Read     : Secondary_Acc;
       Count       : Unsigned_64;
       Destination : System.Address) return Unsigned_64
      with Pre => To_Read /= null;

   function Write
      (To_Write : Secondary_Acc;
       Count    : Unsigned_64;
       Source   : System.Address) return Unsigned_64
      with Pre => To_Write /= null;

   function IO_Control
      (File     : Secondary_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean with Pre => File /= null;

private

   type Primary is record
      Mutex               : aliased Lib.Synchronization.Binary_Semaphore;
      Reader_To_Secondary : Pipe.Pipe_Reader_Acc;
      Reader_To_Primary   : Pipe.Pipe_Reader_Acc;
      Writer_To_Secondary : Pipe.Pipe_Writer_Acc;
      Writer_To_Primary   : Pipe.Pipe_Writer_Acc;
      Secondary           : Secondary_Acc;
      Term_Info           : Devices.TermIOs.Main_Data;
      Term_Size           : Devices.TermIOs.Win_Size;
      Refcount            : Natural;
   end record;

   type Secondary is record
      Mutex            : aliased Lib.Synchronization.Binary_Semaphore;
      Primary_End      : Primary_Acc;
      Primary_Is_Ghost : Boolean;
      Refcount         : Natural;
   end record;

   procedure Free_Primary (Prim : in out Primary_Acc);
end IPC.PTY;
