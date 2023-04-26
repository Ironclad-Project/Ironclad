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

with Lib.Synchronization;
with Devices;

package IPC.Pipe is
   --  Pipes are one of the simplest IPC methods of Ironclad.
   --  Its basically a memory buffer held by the kernel which can be read
   --  and written to, blocking or non blocking.
   type Writer     is private;
   type Reader     is private;
   type Writer_Acc is access Writer;
   type Reader_Acc is access Reader;

   --  Create a fresh pair of pipes.
   procedure Create_Pair
      (Write_End   : out Writer_Acc;
       Read_End    : out Reader_Acc;
       Is_Blocking : Boolean);

   --  Check or change whether the passed pipe is blocking or not.
   function Is_Blocking (P : Writer_Acc) return Boolean
      with Inline, Pre => P /= null;
   function Is_Blocking (P : Reader_Acc) return Boolean
      with Inline, Pre => P /= null;
   procedure Set_Blocking (P : Writer_Acc; B : Boolean)
      with Inline, Pre => P /= null;
   procedure Set_Blocking (P : Reader_Acc; B : Boolean)
      with Inline, Pre => P /= null;

   --  Check whether the pipe is broken.
   function Is_Broken (P : Reader_Acc) return Boolean with Pre => P /= null;

   --  Check whether the pipe is empty.
   function Is_Empty (P : Reader_Acc) return Boolean with Pre => P /= null;
   function Is_Empty (P : Writer_Acc) return Boolean with Pre => P /= null;

   --  Close the passed end, and do preparations for the other end.
   --  Both ends must be closed individually,.
   --  If the writing end is closed but the reader end isnt, the reader will
   --  be allowed to read until the end of the held data.
   procedure Increase_Refcount (P : Writer_Acc)   with Pre => P /= null;
   procedure Increase_Refcount (P : Reader_Acc)   with Pre => P /= null;
   procedure Close (To_Close : in out Writer_Acc) with Pre => To_Close /= null;
   procedure Close (To_Close : in out Reader_Acc) with Pre => To_Close /= null;

   --  Returned status of a pipe operation.
   type Pipe_Status is (Pipe_Success, Broken_Failure, Would_Block_Failure);

   --  Read from the reader end of a pipe.
   --  @param To_Read   Pipe end to read from.
   --  @param Data      Buffer to write read data.
   --  @param Ret_Count Count of data read.
   --  @apram Success   Return status of the operation.
   procedure Read
      (To_Read   : Reader_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Pipe_Status)
      with Pre => To_Read /= null;

   --  Write to the writer end of a pipe.
   --  @param To_Write  Pipe end to write to.
   --  @param Data      Data to write.
   --  @param Ret_Count Count of data written.
   --  @apram Success   Return status of the operation.
   procedure Write
      (To_Write  : Writer_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Pipe_Status)
      with Pre => To_Write /= null;

private

   Data_Len : constant := 512;
   type Writer is record
      Mutex       : aliased Lib.Synchronization.Binary_Semaphore;
      Refcount    : Natural;
      Is_Blocking : Boolean;
      Data_Count  : Natural range 0 .. Data_Len;
      Data        : Devices.Operation_Data (1 .. Data_Len);
      Reader      : Reader_Acc;
   end record;

   type Reader is record
      Mutex           : aliased Lib.Synchronization.Binary_Semaphore;
      Refcount        : Natural;
      Writer_Is_Ghost : Boolean;
      Is_Blocking     : Boolean;
      Other_End       : Writer_Acc;
   end record;
end IPC.Pipe;
