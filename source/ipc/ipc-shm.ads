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

package IPC.SHM with SPARK_Mode => Off is

   subtype Segment_ID is Unsigned_32;
   Error_ID : constant Segment_ID := 0;

   function Create_Segment
      (Wanted_Key  : Unsigned_32;
       Wanted_Size : Unsigned_64;
       Mode        : Unsigned_64) return Segment_ID;

   function Get_Segment (Key : Unsigned_32) return Segment_ID;

   procedure Get_Address
      (ID      : Segment_ID;
       Address : out Unsigned_64;
       Size    : out Unsigned_64);
end IPC.SHM;
