--  ipc-shm.ads: SysV shared memory segments.
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

with Interfaces; use Interfaces;

package IPC.SHM is
   --  This package implements SystemV shared memory segments.
   --  Segments must be registered individually and are not freed ever, unless
   --  marked as refcounted. That is POSIX behaviour, so make sure to do so!

   --  ID to identify a segment, and error value for no segment.
   subtype Segment_ID is Unsigned_32 range 0 .. 20;
   Error_ID : constant Segment_ID := 0;

   --  Create a segment with a unique key.
   procedure Create_Segment
      (Wanted_Key  : Unsigned_32;
       Wanted_Size : Unsigned_64;
       Creator_UID : Unsigned_32;
       Creator_GID : Unsigned_32;
       Mode        : Unsigned_64;
       Segment     : out Segment_ID)
      with Pre => Wanted_Key /= 0;

   --  Create a segment without a unique key.
   procedure Create_Unkeyed_Segment
      (Wanted_Size : Unsigned_64;
       Creator_UID : Unsigned_32;
       Creator_GID : Unsigned_32;
       Mode        : Unsigned_64;
       Segment     : out Segment_ID);

   --  Fetch a segment from its key.
   procedure Get_Segment (Key : Unsigned_32; Segment : out Segment_ID);

   --  Fetch a segment and its size from its physical address.
   procedure Get_Segment_And_Size
      (Address : Unsigned_64;
       Size    : out Unsigned_64;
       ID      : out Segment_ID);

   --  Fetch a segment's physical address and size.
   procedure Get_Address
      (ID      : Segment_ID;
       Address : out Unsigned_64;
       Size    : out Unsigned_64)
      with Pre => ID /= Error_ID;

   procedure Check_Permissions
      (ID      : Segment_ID;
       UID     : Unsigned_32;
       GID     : Unsigned_32;
       Success : out Boolean)
      with Pre => ID /= Error_ID;

   procedure Mark_Refcounted (ID : Segment_ID) with Pre => ID /= Error_ID;

   --  Mark the segment has either been attached (incremented) or detached
   --  (decremented).
   procedure Modify_Attachment (ID : Segment_ID; Increment : Boolean)
      with Pre => ID /= Error_ID;

   procedure Modify_Permissions
      (ID   : Segment_ID;
       UID  : Unsigned_32;
       GID  : Unsigned_32;
       Mode : Unsigned_64)
      with Pre => ID /= Error_ID;

   type Segment_Information is record
      Key         : Unsigned_32;
      Size        : Unsigned_64;
      Owner_UID   : Unsigned_32;
      Owner_GID   : Unsigned_32;
      Creator_UID : Unsigned_32;
      Creator_GID : Unsigned_32;
      Mode        : Unsigned_64;
      Refcount    : Natural;
   end record;

   procedure Fetch_Information
      (ID    : Segment_ID;
       Info  : out Segment_Information;
       Found : out Boolean)
      with Pre => ID /= Error_ID;

   --  Get size of all segments of the system.
   procedure Get_Total_Size (Size : out Unsigned_64);

private

   procedure Check_And_Maybe_Free (ID : Segment_ID)
      with Pre => ID /= Error_ID;
end IPC.SHM;
