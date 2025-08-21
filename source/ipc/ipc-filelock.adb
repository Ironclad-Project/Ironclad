--  ipc-filelock.adb: File locking.
--  Copyright (C) 2024 streaksu
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

with Scheduler;

package body IPC.FileLock is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   procedure Could_Acquire_Lock
      (Acquired_FS  : VFS.FS_Handle;
       Acquired_Ino : VFS.File_Inode_Number;
       Start        : in out Unsigned_64;
       Length       : in out Unsigned_64;
       Acquirer     : in out Userland.Process.PID;
       Is_Write     : in out Boolean;
       Success      : out Boolean)
   is
      Conflicting : Natural;
   begin
      Synchronization.Seize (Registry_Mutex);
      Inner_Could_Acquire
         (Acquired_FS, Acquired_Ino, Start, Length, Is_Write,
          Conflicting, Success);
      if not Success then
         Start    := Registry (Conflicting).Start;
         Length   := Registry (Conflicting).Length;
         Acquirer := Registry (Conflicting).Acquirer;
         Is_Write := Registry (Conflicting).Is_Write;
      end if;
      Synchronization.Release (Registry_Mutex);
   end Could_Acquire_Lock;

   procedure Acquire_Lock
      (Acquired_FS  : VFS.FS_Handle;
       Acquired_Ino : VFS.File_Inode_Number;
       Start        : Unsigned_64;
       Length       : Unsigned_64;
       Acquirer     : Userland.Process.PID;
       Is_Write     : Boolean;
       Is_Blocking  : Boolean;
       Success      : out Boolean)
   is
      Discard : Natural;
   begin
      loop
         Synchronization.Seize (Registry_Mutex);
         Inner_Could_Acquire
            (Acquired_FS, Acquired_Ino, Start, Length, Is_Write,
             Discard, Success);
         if Success then
            Success := False;
            for L of Registry loop
               if L.Acquirer = Error_PID then
                  L.FS       := Acquired_FS;
                  L.Ino      := Acquired_Ino;
                  L.Start    := Start;
                  L.Length   := Length;
                  L.Acquirer := Acquirer;
                  L.Is_Write := Is_Write;
                  Success := True;
                  exit;
               end if;
            end loop;
         else
            Success := False;
            for L of Registry loop
               if L.Acquirer = Acquirer and then
                  L.Start = Start and then
                  L.Length = Length
               then
                  L.Is_Write := Is_Write;
                  Success    := True;
                  exit;
               end if;
            end loop;
         end if;
         Synchronization.Release (Registry_Mutex);
         exit when not Is_Blocking or Success;
         Scheduler.Yield_If_Able;
      end loop;
   end Acquire_Lock;

   procedure Release_Lock
      (Acquired_FS  : VFS.FS_Handle;
       Acquired_Ino : VFS.File_Inode_Number;
       Start        : Unsigned_64;
       Length       : Unsigned_64;
       Acquirer     : Userland.Process.PID;
       Success      : out Boolean)
   is
      pragma Unreferenced (Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Acquirer);
   begin
      Release_Lock (Acquired_FS, Acquired_Ino, Success);
   end Release_Lock;

   procedure Release_Lock
      (Acquired_FS  : VFS.FS_Handle;
       Acquired_Ino : VFS.File_Inode_Number;
       Success      : out Boolean)
   is
   begin
      Synchronization.Seize (Registry_Mutex);
      Success := False;
      for L of Registry loop
         if L.Acquirer /= Error_PID and then
            L.FS = Acquired_FS      and then
            L.Ino = Acquired_Ino
         then
            L.Acquirer := Error_PID;
            Success := True;
            exit;
         end if;
      end loop;
      Synchronization.Release (Registry_Mutex);
   end Release_Lock;

   procedure List_All (Buffer : out Lock_Arr; Length : out Natural) is
      Curr_Index : Natural := 0;
   begin
      Buffer := [others => (Error_PID, False, 0, 0, VFS.Error_Handle, 0)];
      Length := 0;

      if Buffer'Length /= 0 then
         Curr_Index := Buffer'First;
      end if;

      Synchronization.Seize (Registry_Mutex);
      for I in Registry'Range loop
         if Registry (I).Acquirer /= Error_PID then
            Length := Length + 1;

            if Buffer'Length /= 0 and then Curr_Index <= Buffer'Last then
               Buffer (Curr_Index) :=
                  (Acquirer   => Registry (I).Acquirer,
                   Is_Writing => Registry (I).Is_Write,
                   Start      => Registry (I).Start,
                   Length     => Registry (I).Length,
                   FS         => Registry (I).FS,
                   Ino        => Registry (I).Ino);

               if Curr_Index /= Natural'Last then
                  Curr_Index := Curr_Index + 1;
               end if;
            end if;
         end if;
      end loop;
      Synchronization.Release (Registry_Mutex);
   end List_All;
   ----------------------------------------------------------------------------
   procedure Inner_Could_Acquire
      (Acquired_FS  : VFS.FS_Handle;
       Acquired_Ino : VFS.File_Inode_Number;
       Start        : Unsigned_64;
       Length       : Unsigned_64;
       Is_Write     : Boolean;
       Conflicting  : out Natural;
       Success      : out Boolean)
   is
   begin
      Success := True;
      Conflicting := 0;
      for I in Registry'Range loop
         if Registry (I).Acquirer /= Error_PID  and then
            Registry (I).FS  = Acquired_FS      and then
            Registry (I).Ino = Acquired_Ino
         then
            if Registry (I).Is_Write or Is_Write or
               (Registry (I).Start <= Start and Registry (I).Length >= Length)
            then
               Conflicting := I;
               Success := False;
               exit;
            end if;
         end if;
      end loop;
   end Inner_Could_Acquire;
end IPC.FileLock;
