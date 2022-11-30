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
   type Pipe_Writer;
   type Pipe_Reader;
   type Pipe_Writer_Acc is access Pipe_Writer;
   type Pipe_Reader_Acc is access Pipe_Reader;

   --  Writer and reader need to be aware of the location of each other.
   --  The writer holds the data, and has the reader as validity check, while
   --  the reader just accesses it.
   Pipe_Data_Len : constant := 512;
   type Pipe_Data is array (Natural range <>) of Unsigned_8;
   type Pipe_Writer is record
      Mutex       : aliased Lib.Synchronization.Binary_Semaphore;
      Is_Blocking : Boolean;
      Data_Count  : Natural range 0 .. Pipe_Data_Len with Volatile;
      Data        : Pipe_Data (1 .. Pipe_Data_Len);
      Reader      : Pipe_Reader_Acc;
   end record;

   --  Reader end holds a pointer to the writer and whether to block.
   type Pipe_Reader is record
      Is_Blocking : Boolean;
      Other_End   : Pipe_Writer_Acc;
   end record;

   --  Create a fresh pair of pipes.
   procedure Create_Pair
      (Write_End   : out Pipe_Writer_Acc;
       Read_End    : out Pipe_Reader_Acc;
       Is_Blocking : Boolean);

   --  Change whether the passed pipe is blocking or not.
   procedure Set_Blocking (P : Pipe_Writer_Acc; B : Boolean);
   procedure Set_Blocking (P : Pipe_Reader_Acc; B : Boolean);

   --  Close the passed end, and do preparations for the other end.
   --  Both ends must be closed individually.
   procedure Close (To_Close : in out Pipe_Writer_Acc);
   procedure Close (To_Close : in out Pipe_Reader_Acc);

   --  Read from the reader end of a pipe.
   function Read
      (To_Read     : Pipe_Reader_Acc;
       Count       : Unsigned_64;
       Destination : System.Address) return Unsigned_64;

   --  Write to the writer end of a pipe.
   function Write
      (To_Write : Pipe_Writer_Acc;
       Count    : Unsigned_64;
       Source   : System.Address) return Unsigned_64;
end IPC.Pipe;
