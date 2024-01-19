--  ipc-signalpost.ads: Signal post creation and management.
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

package IPC.SignalPost is
   --  Data structures to define a signal post
   type SignalPost     is private;
   type SignalPost_Acc is access SignalPost;

   --  Create a signal post.
   --  @param Is_Blocking True if blocking, False if not blocking.
   --  @return The returned post, or null in failure.
   function Create (Is_Blocking : Boolean := True) return SignalPost_Acc;

   --  Close the signal post, if possible.
   --  @param To_Close Post to free and close, will be always set to null.
   procedure Close (To_Close : in out SignalPost_Acc)
      with Pre => To_Close /= null, Post => To_Close = null;

   --  Get whether the post is blocking.
   --  @param Post     Post to check.
   --  @param Blocking True if blocking, False if not.
   procedure Is_Blocking (Post : SignalPost_Acc; Blocking : out Boolean)
      with Pre => Post /= null;

   --  Set whether the post is blocking.
   --  @param Post        Post to modify.
   --  @param Is_Blocking True if blocking, False if not.
   procedure Set_Blocking (Post : SignalPost_Acc; Is_Blocking : Boolean)
      with Pre => Post /= null;

   --  Poll the availability of operations that can be done to a post.
   --  @param Post      Post to operate on.
   --  @param Can_Read  True if a read operation would not block.
   --  @param Is_Error  True if an error condition was triggered.
   procedure Poll
      (Post     : SignalPost_Acc;
       Can_Read : out Boolean;
       Is_Error : out Boolean)
      with Pre => Post /= null;

   --  Read from a post.
   --  @param Post      Post to read from, or its connection.
   --  @param Data      Data to read to.
   --  @param Ret_Count Count of data read.
   --  @param Success   Resulting status of the operation.
   procedure Read
      (Post      : SignalPost_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
      with Pre => Post /= null;

private

   type SignalPost is record
      Mutex       : aliased Lib.Synchronization.Binary_Semaphore;
      Is_Blocking : Boolean;
   end record;
end IPC.SignalPost;
