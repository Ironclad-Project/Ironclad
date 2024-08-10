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

with Lib.Synchronization; use Lib.Synchronization;
with Memory;
with Memory.Physical; use Memory.Physical;
with Interfaces.C; use Interfaces.C;
with System.Storage_Elements; use System.Storage_Elements;

package body IPC.SHM is
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

   Registry_Mutex : aliased Binary_Semaphore := Unlocked_Semaphore;
   Registry : Segment_Arr :=
      (others => (False, 0, 0, 0, 0, 0, 0, 0, 0, 0, False));

   function Create_Segment
      (Wanted_Key  : Unsigned_32;
       Wanted_Size : Unsigned_64;
       Creator_UID : Unsigned_32;
       Creator_GID : Unsigned_32;
       Mode        : Unsigned_64) return Segment_ID
   is
      Returned : Segment_ID := Error_ID;
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      for I in Registry'Range loop
         if Registry (I).Is_Present and Registry (I).Key = Wanted_Key then
            goto Cleanup;
         end if;
         if not Registry (I).Is_Present and Returned = Error_ID then
            Returned := I;
         end if;
      end loop;

      Registry (Returned).Is_Present := True;
      Registry (Returned).Key := Wanted_Key;
      Registry (Returned).Owner_UID := Creator_UID;
      Registry (Returned).Owner_GID := Creator_GID;
      Registry (Returned).Creator_UID := Creator_UID;
      Registry (Returned).Creator_GID := Creator_GID;
      Registry (Returned).Mode := Mode;
      Registry (Returned).Physical_Address :=
         Alloc (size_t (Wanted_Size)) - Memory.Memory_Offset;
      Registry (Returned).Size := Memory.Size (Wanted_Size);
      Registry (Returned).Refcount := 0;
      Registry (Returned).Is_Refcounted := False;

   <<Cleanup>>
      Lib.Synchronization.Release (Registry_Mutex);
      return Returned;
   end Create_Segment;

   function Create_Unkeyed_Segment
      (Wanted_Size : Unsigned_64;
       Creator_UID : Unsigned_32;
       Creator_GID : Unsigned_32;
       Mode        : Unsigned_64) return Segment_ID
   is
      Returned : Segment_ID := Error_ID;
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      for I in Registry'Range loop
         if not Registry (I).Is_Present then
            Registry (I).Is_Present := True;
            Registry (I).Key := 0;
            Registry (I).Owner_UID := Creator_UID;
            Registry (I).Owner_GID := Creator_GID;
            Registry (I).Creator_UID := Creator_UID;
            Registry (I).Creator_GID := Creator_GID;
            Registry (I).Mode := Mode;
            Registry (I).Physical_Address :=
               Alloc (size_t (Wanted_Size)) - Memory.Memory_Offset;
            Registry (I).Size := Memory.Size (Wanted_Size);
            Registry (I).Refcount := 0;
            Registry (I).Is_Refcounted := False;
            Returned := I;
            exit;
         end if;
      end loop;

      Lib.Synchronization.Release (Registry_Mutex);
      return Returned;
   end Create_Unkeyed_Segment;

   function Get_Segment (Key : Unsigned_32) return Segment_ID is
      Returned : Segment_ID := Error_ID;
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      for I in Registry'Range loop
         if Registry (I).Is_Present and Registry (I).Key = Key then
            Returned := I;
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry_Mutex);
      return Returned;
   end Get_Segment;

   procedure Get_Segment_And_Size
      (Address : Unsigned_64;
       Size    : out Unsigned_64;
       ID      : out Segment_ID)
   is
   begin
      Size := 0;
      ID   := Error_ID;

      Lib.Synchronization.Seize (Registry_Mutex);
      for I in Registry'Range loop
         if Registry (I).Is_Present and
            Registry (I).Physical_Address = Memory.Physical_Address (Address)
         then
            Size := Unsigned_64 (Registry (I).Size);
            ID   := I;
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (Registry_Mutex);
   end Get_Segment_And_Size;

   procedure Get_Address
      (ID      : Segment_ID;
       Address : out Unsigned_64;
       Size    : out Unsigned_64)
   is
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         Address := Unsigned_64 (Registry (ID).Physical_Address);
         Size := Unsigned_64 (Registry (ID).Size);
      else
         Address := 0;
         Size := 0;
      end if;
      Lib.Synchronization.Release (Registry_Mutex);
   end Get_Address;

   function Check_Permissions
      (ID  : Segment_ID;
       UID : Unsigned_32;
       GID : Unsigned_32) return Boolean
   is
      Result : Boolean;
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         Result := UID = Registry (ID).Owner_UID or
            (GID = Registry (ID).Owner_GID and
            (Registry (ID).Mode and 8#040#) /= 0);
      else
         Result := False;
      end if;
      Lib.Synchronization.Release (Registry_Mutex);
      return Result;
   end Check_Permissions;

   procedure Mark_Refcounted (ID : Segment_ID) is
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         Registry (ID).Is_Refcounted := True;
         Check_And_Maybe_Free (ID);
      end if;
      Lib.Synchronization.Release (Registry_Mutex);
   end Mark_Refcounted;

   procedure Modify_Attachment (ID : Segment_ID; Increment : Boolean) is
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         Registry (ID).Refcount :=
            Registry (ID).Refcount + (if Increment then 1 else -1);
         Check_And_Maybe_Free (ID);
      end if;
      Lib.Synchronization.Release (Registry_Mutex);
   end Modify_Attachment;

   procedure Modify_Permissions
      (ID   : Segment_ID;
       UID  : Unsigned_32;
       GID  : Unsigned_32;
       Mode : Unsigned_64)
   is
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
      if Registry (ID).Is_Present then
         Registry (ID).Owner_UID := UID;
         Registry (ID).Owner_GID := GID;
         Registry (ID).Mode := Mode;
      end if;
      Lib.Synchronization.Release (Registry_Mutex);
   end Modify_Permissions;

   procedure Fetch_Information
      (ID    : Segment_ID;
       Info  : out Segment_Information;
       Found : out Boolean)
   is
   begin
      Lib.Synchronization.Seize (Registry_Mutex);
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
         Found := False;
      end if;
      Lib.Synchronization.Release (Registry_Mutex);
   end Fetch_Information;

   procedure Get_Total_Size (Size : out Unsigned_64) is
   begin
      Size := 0;
      Lib.Synchronization.Seize (Registry_Mutex);
      for Reg of Registry loop
         if Reg.Is_Present then
            Size := Size + Unsigned_64 (Reg.Size);
         end if;
      end loop;
      Lib.Synchronization.Release (Registry_Mutex);
   end Get_Total_Size;
   ----------------------------------------------------------------------------
   procedure Check_And_Maybe_Free (ID : Segment_ID) is
   begin
      if Registry (ID).Refcount = 0 and Registry (ID).Is_Refcounted then
         Registry (ID).Is_Present := False;
         Free (size_t (Registry (ID).Physical_Address));
      end if;
   end Check_And_Maybe_Free;
end IPC.SHM;
