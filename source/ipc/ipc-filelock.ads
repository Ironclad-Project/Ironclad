--  ipc-filelock.ads: File locking.
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

with Interfaces;       use Interfaces;
with VFS;              use VFS;
with Userland.Process; use Userland.Process;

package IPC.FileLock is
   --  This module implements POSIX-compliant advisory file locking.

   --  Checks whether a lock could be acquired.
   --  @param Acquired_FS  FS containing the inode to acquire.
   --  @param Acquired_Ino Inode to acquire.
   --  @param Start        Start to lock, or conflicting start.
   --  @param Length       Lenght to lock, or conflicting length.
   --  @param Acquirer     Acquirer of the lock, or current holder.
   --  @param Is_Write     Whether the lock is read (inclusive) or write.
   --  @param Success      True if the lock could be placed, else, information
   --                      of one of the holders is returned in other params.
   procedure Could_Acquire_Lock
      (Acquired_FS  : VFS.FS_Handle;
       Acquired_Ino : VFS.File_Inode_Number;
       Start        : in out Unsigned_64;
       Length       : in out Unsigned_64;
       Acquirer     : in out Userland.Process.PID;
       Is_Write     : in out Boolean;
       Success      : out Boolean);

   --  Acquire a lock.
   --  @param Acquired_FS  FS containing the inode to acquire.
   --  @param Acquired_Ino Inode to acquire.
   --  @param Start        Start to lock.
   --  @param Length       Lenght to lock.
   --  @param Acquirer     Acquirer of the lock.
   --  @param Is_Write     Whether the lock is read (inclusive) or write.
   --  @param Is_Blocking  Block until blocked, or give up early.
   --  @param Success      True if the lock could be placed.
   procedure Acquire_Lock
      (Acquired_FS  : VFS.FS_Handle;
       Acquired_Ino : VFS.File_Inode_Number;
       Start        : Unsigned_64;
       Length       : Unsigned_64;
       Acquirer     : Userland.Process.PID;
       Is_Write     : Boolean;
       Is_Blocking  : Boolean;
       Success      : out Boolean);

   --  Release a lock.
   --  @param Acquired_FS  FS containing the inode to release.
   --  @param Acquired_Ino Inode to release.
   --  @param Start        Start to unlock.
   --  @param Length       Lenght to unlock.
   --  @param Acquirer     Acquirer of the lock.
   --  @param Success      True if the lock could be unlocked.
   procedure Release_Lock
      (Acquired_FS  : VFS.FS_Handle;
       Acquired_Ino : VFS.File_Inode_Number;
       Start        : Unsigned_64;
       Length       : Unsigned_64;
       Acquirer     : Userland.Process.PID;
       Success      : out Boolean);

   --  Release all locks regardless of range and owner.
   --  @param Acquired_FS  FS containing the inode to release.
   --  @param Acquired_Ino Inode to release.
   --  @param Success      True if the lock could be unlocked.
   procedure Release_Lock
      (Acquired_FS  : VFS.FS_Handle;
       Acquired_Ino : VFS.File_Inode_Number;
       Success      : out Boolean);

   --  Information of a file lock for debugging.
   type Lock_Info is record
      Acquirer   : PID;
      Is_Writing : Boolean;
      Start      : Unsigned_64;
      Length     : Unsigned_64;
      FS         : VFS.FS_Handle;
      Ino        : VFS.File_Inode_Number;
   end record;
   type Lock_Arr is array (Natural range <>) of Lock_Info;

   --  List all file locks on the system.
   --  @param Buffer Where to write all the lock information.
   --  @param Length Total count of locks, even if it is > List'Length.
   procedure List_All (Buffer : out Lock_Arr; Length : out Natural);

private

   procedure Inner_Could_Acquire
      (Acquired_FS  : VFS.FS_Handle;
       Acquired_Ino : VFS.File_Inode_Number;
       Start        : Unsigned_64;
       Length       : Unsigned_64;
       Is_Write     : Boolean;
       Conflicting  : out Natural;
       Success      : out Boolean);
end IPC.FileLock;
