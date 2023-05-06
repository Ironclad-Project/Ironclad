--  ipc-fifo.ads: Pipe creation and management.
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

with Lib.Synchronization;
with Devices; use Devices;
with Memory.Virtual;

package IPC.FIFO is
   --  FIFOs are the simplest and most versatile IPC methods of Ironclad.
   --  It's basically a buffer held in memory by the kernel, open for read and
   --  write, with independent settings for each operation.
   --
   --  A FIFO has 2 sides, a write and read side, each one has separate
   --  refcount, reduced when closed. The whole object is deallocated when both
   --  reach 0.
   --  If the read side's one goes down to 0, all subsequent writes will fail
   --  until increased. Reads will continue working. This is done instead of a
   --  global refcount in order to implement UNIXy behaviour easily. Do not
   --  hope for the existance of SIGPIPE.
   --
   --  FIFOs can be used standalone or to implement other UNIXy interfaces
   --  like pipes or UNIX-domain sockets.

   --  Objects used to represent the FIFO.
   type Inner     is private;
   type Inner_Acc is access Inner;

   --  Data inside the FIFO has a default set maximum length. The length is
   --  configurable using some functions here.
   Default_Data_Length : constant Natural;

   --  Create a fresh FIFO. It will be created with one refcount for the read
   --  end and one refcount for the write end.
   function Create (Is_Blocking : Boolean := True) return Inner_Acc
      with Post => Is_Valid (Create'Result);

   --  Check or change whether the FIFO is blocking or not, for each end.
   function Is_Read_Blocking (P : Inner_Acc) return Boolean
      with Inline, Pre => Is_Valid (P);
   function Is_Write_Blocking (P : Inner_Acc) return Boolean
      with Inline, Pre => Is_Valid (P);
   procedure Set_Read_Blocking (P : Inner_Acc; B : Boolean)
      with Inline, Pre => Is_Valid (P);
   procedure Set_Write_Blocking (P : Inner_Acc; B : Boolean)
      with Inline, Pre => Is_Valid (P);

   --  Check whether the FIFO is empty.
   function Is_Empty (P : Inner_Acc) return Boolean with Pre => Is_Valid (P);

   --  Check whether the FIFO is full.
   function Is_Full (P : Inner_Acc) return Boolean with Pre => Is_Valid (P);

   --  Check whether the FIFO is broken, that is, whether it has no readers.
   function Is_Broken (P : Inner_Acc) return Boolean with Pre => Is_Valid (P);

   --  Increase the refcount or close the passed end.
   procedure Increase_Reader_Refcount (P : Inner_Acc) with Pre => Is_Valid (P);
   procedure Increase_Writer_Refcount (P : Inner_Acc) with Pre => Is_Valid (P);
   procedure Close_Reader (To_Close : in out Inner_Acc)
      with Pre => Is_Valid (To_Close);
   procedure Close_Writer (To_Close : in out Inner_Acc)
      with Pre => Is_Valid (To_Close);

   --  Non-specific one just closes both reader and writer.
   procedure Close (To_Close : in out Inner_Acc)
      with Pre => Is_Valid (To_Close);

   --  Get the current size of a FIFO.
   --  @param P    FIFO to configure.
   --  @param Size Size of the FIFO.
   procedure Get_Size (P : Inner_Acc; Size : out Natural)
      with Pre => Is_Valid (P);

   --  Set the current size of a FIFO.
   --  @param P FIFO to configure.
   --  @param Size Size to set for the FIFO.
   --  @param Success True in success, False if there was a failure or the
   --  passed size would cause data loss.
   procedure Set_Size (P : Inner_Acc; Size : Natural; Success : out Boolean)
      with Pre => Is_Valid (P);

   --  Returned status of a pipe operation.
   type Pipe_Status is (Pipe_Success, Broken_Failure, Would_Block_Failure);

   --  Read from the reader end of a pipe.
   --  @param To_Read   Pipe end to read from.
   --  @param Data      Buffer to write read data.
   --  @param Ret_Count Count of data read.
   --  @apram Success   Return status of the operation.
   procedure Read
      (To_Read   : Inner_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Pipe_Status)
      with Pre => Is_Valid (To_Read);

   --  Write to the writer end of a pipe.
   --  @param To_Write  Pipe end to write to.
   --  @param Data      Data to write.
   --  @param Ret_Count Count of data written.
   --  @apram Success   Return status of the operation.
   procedure Write
      (To_Write  : Inner_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Pipe_Status)
      with Pre => Is_Valid (To_Write);

   --  Ghost function for checking whether a FIFO is properly initialized.
   function Is_Valid (P : Inner_Acc) return Boolean with Ghost;

private

   Default_Data_Length : constant Natural := Memory.Virtual.Page_Size * 4;
   type Inner is record
      Mutex             : aliased Lib.Synchronization.Binary_Semaphore;
      Reader_Refcount   : Natural;
      Writer_Refcount   : Natural;
      Is_Read_Blocking  : Boolean;
      Is_Write_Blocking : Boolean;
      Data_Count        : Natural;
      Data              : Devices.Operation_Data_Acc;
   end record;

   function Is_Valid (P : Inner_Acc) return Boolean is
      (P /= null and then P.Data /= null);

   procedure Common_Close (To_Close : in out Inner_Acc)
      with Pre => To_Close /= null;
end IPC.FIFO;
