--  ipc-pipe.ads: Pipe creation and management.
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

with System;
with Lib.Synchronization;
with Interfaces; use Interfaces;

package IPC.Pipe with SPARK_Mode => Off is
   --  Pipes are one of the simplest IPC methods of Ironclad.
   --  Its basically a memory buffer held by the kernel which can be read
   --  and written to, blocking or non blocking.
   type Pipe_Writer     is private;
   type Pipe_Reader     is private;
   type Pipe_Writer_Acc is access Pipe_Writer;
   type Pipe_Reader_Acc is access Pipe_Reader;

   --  Create a fresh pair of pipes.
   procedure Create_Pair
      (Write_End   : out Pipe_Writer_Acc;
       Read_End    : out Pipe_Reader_Acc;
       Is_Blocking : Boolean);

   --  Check or change whether the passed pipe is blocking or not.
   function Is_Blocking (P : Pipe_Writer_Acc) return Boolean
      with Inline, Pre => P /= null;
   function Is_Blocking (P : Pipe_Reader_Acc) return Boolean
      with Inline, Pre => P /= null;
   procedure Set_Blocking (P : Pipe_Writer_Acc; B : Boolean)
      with Inline, Pre => P /= null;
   procedure Set_Blocking (P : Pipe_Reader_Acc; B : Boolean)
      with Inline, Pre => P /= null;

   --  Check whether the pipe is broken.
   function Is_Broken (P : Pipe_Reader_Acc) return Boolean
      with Pre => P /= null;

   --  Check whether the pipe is empty.
   function Is_Empty (P : Pipe_Reader_Acc) return Boolean
      with Pre => P /= null;
   function Is_Empty (P : Pipe_Writer_Acc) return Boolean
      with Pre => P /= null;

   --  Close the passed end, and do preparations for the other end.
   --  Both ends must be closed individually,.
   --  If the writing end is closed but the reader end isnt, the reader will
   --  be allowed to read until the end of the held data.
   procedure Increase_Refcount (P : Pipe_Writer_Acc)   with Pre => P /= null;
   procedure Increase_Refcount (P : Pipe_Reader_Acc)   with Pre => P /= null;
   procedure Close (To_Close : in out Pipe_Writer_Acc)
      with Pre => To_Close /= null;
   procedure Close (To_Close : in out Pipe_Reader_Acc)
      with Pre => To_Close /= null;

   --  Read from the reader end of a pipe.
   function Read
      (To_Read     : Pipe_Reader_Acc;
       Count       : Unsigned_64;
       Destination : System.Address) return Unsigned_64
      with Pre => To_Read /= null;

   --  Write to the writer end of a pipe.
   function Write
      (To_Write : Pipe_Writer_Acc;
       Count    : Unsigned_64;
       Source   : System.Address) return Unsigned_64
      with Pre => To_Write /= null;

private

   Pipe_Data_Len : constant := 512;
   type Pipe_Data is array (Natural range <>) of Unsigned_8;
   type Pipe_Writer is record
      Mutex       : aliased Lib.Synchronization.Binary_Semaphore;
      Refcount    : Natural;
      Is_Blocking : Boolean;
      Data_Count  : Natural range 0 .. Pipe_Data_Len with Volatile;
      Data        : Pipe_Data (1 .. Pipe_Data_Len);
      Reader      : Pipe_Reader_Acc;
   end record;

   type Pipe_Reader is record
      Mutex           : aliased Lib.Synchronization.Binary_Semaphore;
      Refcount        : Natural;
      Writer_Is_Ghost : Boolean;
      Is_Blocking     : Boolean;
      Other_End       : Pipe_Writer_Acc;
   end record;
end IPC.Pipe;
