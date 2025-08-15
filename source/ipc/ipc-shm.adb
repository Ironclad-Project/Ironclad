--  ipc-shm.adb: SysV shared memory segments.
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

with Synchronization; use Synchronization;
with Memory;
with Memory.Physical; use Memory.Physical;
with System.Storage_Elements; use System.Storage_Elements;

package body IPC.SHM is
   pragma Suppress (All_Checks); --  Unit passes AoRTE.

   type Segment_Inner is record
      Is_Present       : Boolean;
      Key              : Unsigned_32;
      Owner_UID        : Unsigned_32;
      Owner_GID        : Unsigned_32;
      Creator_UID      : Unsigned_32;
      Creator_GID      : Unsigned_32;
      Mode             : Unsigned_64;
      Physical_Address : Memory.Physical_Address;
      Size             : Memory.Size;
      Refcount         : Natural;
      Is_Refcounted    : Boolean;
   end record;
   type Segment_Arr is array (Segment_ID range 1 .. 20) of Segment_Inner;

   Registry_Mutex : aliased Mutex := Unlocked_Mutex;
   Registry : Segment_Arr :=
      [others => (False, 0, 0, 0, 0, 0, 0, 0, 0, 0, False)];

   procedure Create_Segment
      (Wanted_Key  : Unsigned_32;
       Wanted_Size : Unsigned_64;
       Creator_UID : Unsigned_32;
       Creator_GID : Unsigned_32;
       Mode        : Unsigned_64;
       Segment     : out Segment_ID)
   is
      Addr    : Integer_Address;
      Success : Boolean;
   begin
      Segment := Error_ID;

      Synchronization.Seize (Registry_Mutex);
      for I in Registry'Range loop
         if Registry (I).Is_Present and Registry (I).Key = Wanted_Key then
            goto Cleanup;
         end if;
         if not Registry (I).Is_Present and Segment = Error_ID then
            Segment := I;
         end if;
      end loop;

      if Segment = Error_ID then
         goto Cleanup;
      end if;

      Memory.Physical.User_Alloc (Addr, Wanted_Size, Success);
      if not Success then
         Segment := Error_ID;
         goto Cleanup;
      end if;

      Registry (Segment) :=
         (Is_Present       => True,
          Key              => Wanted_Key,
          Owner_UID        => Creator_UID,
          Owner_GID        => Creator_GID,
          Creator_UID      => Creator_UID,
          Creator_GID      => Creator_GID,
          Mode             => Mode,
          Physical_Address => Addr - Memory.Memory_Offset,
          Size             => Memory.Size (Wanted_Size),
          Refcount         => 0,
          Is_Refcounted    => False);

   <<Cleanup>>
      Synchronization.Release (Registry_Mutex);
   end Create_Segment;

   procedure Create_Unkeyed_Segment
      (Wanted_Size : Unsigned_64;
       Creator_UID : Unsigned_32;
       Creator_GID : Unsigned_32;
       Mode        : Unsigned_64;
       Segment     : out Segment_ID)
   is
      Addr    : Integer_Address;
      Success : Boolean;
   begin
      Segment := Error_ID;

      Synchronization.Seize (Registry_Mutex);
      for I in Registry'Range loop
         if not Registry (I).Is_Present then
            Memory.Physical.User_Alloc (Addr, Wanted_Size, Success);
            if not Success then
               goto Cleanup;
            end if;

            Segment := I;
            Registry (Segment) :=
               (Is_Present       => True,
                Key              => 0,
                Owner_UID        => Creator_UID,
                Owner_GID        => Creator_GID,
                Creator_UID      => Creator_UID,
                Creator_GID      => Creator_GID,
                Mode             => Mode,
                Physical_Address => Addr - Memory.Memory_Offset,
                Size             => Memory.Size (Wanted_Size),
                Refcount         => 0,
                Is_Refcounted    => False);
            exit;
         end if;
      end loop;

   <<Cleanup>>
      Synchronization.Release (Registry_Mutex);
   end Create_Unkeyed_Segment;

   procedure Get_Segment (Key : Unsigned_32; Segment : out Segment_ID) is
   begin
      Segment := Error_ID;
      Synchronization.Seize (Registry_Mutex);
      for I in Registry'Range loop
         if Registry (I).Is_Present and Registry (I).Key = Key then
            Segment := I;
            exit;
         end if;
      end loop;
      Synchronization.Release (Registry_Mutex);
   end Get_Segment;

   procedure Get_Segment_And_Size
      (Address : Unsigned_64;
       Size    : out Unsigned_64;
       ID      : out Segment_ID)
   is
   begin
      Size := 0;
      ID   := Error_ID;

      Synchronization.Seize (Registry_Mutex);
      for I in Registry'Range loop
         if Registry (I).Is_Present and
            Registry (I).Physical_Address = Memory.Physical_Address (Address)
         then
            Size := Unsigned_64 (Registry (I).Size);
            ID   := I;
            exit;
         end if;
      end loop;
      Synchronization.Release (Registry_Mutex);
   end Get_Segment_And_Size;

   procedure Get_Address
      (ID      : Segment_ID;
       Address : out Unsigned_64;
       Size    : out Unsigned_64)
   is
   begin
      Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         Address := Unsigned_64 (Registry (ID).Physical_Address);
         Size := Unsigned_64 (Registry (ID).Size);
      else
         Address := 0;
         Size := 0;
      end if;
      Synchronization.Release (Registry_Mutex);
   end Get_Address;

   procedure Check_Permissions
      (ID      : Segment_ID;
       UID     : Unsigned_32;
       GID     : Unsigned_32;
       Success : out Boolean)
   is
   begin
      Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         Success := UID = Registry (ID).Owner_UID or
            (GID = Registry (ID).Owner_GID and
            (Registry (ID).Mode and 8#040#) /= 0);
      else
         Success := False;
      end if;
      Synchronization.Release (Registry_Mutex);
   end Check_Permissions;

   procedure Mark_Refcounted (ID : Segment_ID) is
   begin
      Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         Registry (ID).Is_Refcounted := True;
         Check_And_Maybe_Free (ID);
      end if;
      Synchronization.Release (Registry_Mutex);
   end Mark_Refcounted;

   procedure Modify_Attachment (ID : Segment_ID; Increment : Boolean) is
   begin
      Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         if Increment and Registry (ID).Refcount /= Natural'Last then
            Registry (ID).Refcount := Registry (ID).Refcount + 1;
         elsif Registry (ID).Refcount /= 0 then
            Registry (ID).Refcount := Registry (ID).Refcount - 1;
         end if;
         Check_And_Maybe_Free (ID);
      end if;
      Synchronization.Release (Registry_Mutex);
   end Modify_Attachment;

   procedure Modify_Permissions
      (ID   : Segment_ID;
       UID  : Unsigned_32;
       GID  : Unsigned_32;
       Mode : Unsigned_64)
   is
   begin
      Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         Registry (ID).Owner_UID := UID;
         Registry (ID).Owner_GID := GID;
         Registry (ID).Mode := Mode;
      end if;
      Synchronization.Release (Registry_Mutex);
   end Modify_Permissions;

   procedure Fetch_Information
      (ID    : Segment_ID;
       Info  : out Segment_Information;
       Found : out Boolean)
   is
   begin
      Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         Info :=
            (Key         => Registry (ID).Key,
             Size        => Unsigned_64 (Registry (ID).Size),
             Owner_UID   => Registry (ID).Owner_UID,
             Owner_GID   => Registry (ID).Owner_GID,
             Creator_UID => Registry (ID).Creator_UID,
             Creator_GID => Registry (ID).Creator_GID,
             Mode        => Registry (ID).Mode,
             Refcount    => Registry (ID).Refcount);
         Found := True;
      else
         Info :=
            (Key         => 0,
             Size        => 0,
             Owner_UID   => 0,
             Owner_GID   => 0,
             Creator_UID => 0,
             Creator_GID => 0,
             Mode        => 0,
             Refcount    => 0);
         Found := False;
      end if;
      Synchronization.Release (Registry_Mutex);
   end Fetch_Information;

   procedure Get_Total_Size (Size : out Unsigned_64) is
   begin
      Size := 0;
      Synchronization.Seize (Registry_Mutex);
      for Reg of Registry loop
         if Reg.Is_Present then
            Size := Size + Unsigned_64 (Reg.Size);
         end if;
      end loop;
      Synchronization.Release (Registry_Mutex);
   end Get_Total_Size;
   ----------------------------------------------------------------------------
   procedure Check_And_Maybe_Free (ID : Segment_ID) is
   begin
      if Registry (ID).Refcount = 0 and Registry (ID).Is_Refcounted then
         Registry (ID).Is_Present := False;
         User_Free (Registry (ID).Physical_Address);
      end if;
   end Check_And_Maybe_Free;
end IPC.SHM;
