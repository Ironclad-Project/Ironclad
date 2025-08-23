--  devices-partitions.ads: Split a block device into partitions.
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

package Devices.Partitions is
   --  Register the partitions of the passed device as separate devices.
   --  The resulting support read/write, and that's it.
   --  @param Name    Name to prepend p<index> with.
   --  @param Dev     Device to scan partitions for.
   --  @param Success True if everything is nominal, regardless of whether
   --  any partitions were found. False if one of the systems
   --  failed unexpectedly or a non-block device was passed.
   procedure Parse_Partitions
      (Name    : String;
       Dev     : Device_Handle;
       Success : out Boolean);

private

   --  Scan partitions for GPT and MBR.
   procedure Parse_GPT_Partitions
      (Name             : String;
       Dev              : Device_Handle;
       Found_Partitions : out Boolean;
       Success          : out Boolean);
   procedure Parse_MBR_Partitions
      (Name             : String;
       Dev              : Device_Handle;
       Found_Partitions : out Boolean;
       Success          : out Boolean);

   --  Register data for a partition.
   type Partition_Data is record
      Inner_Device : Device_Handle;
      Block_Size   : Natural;
      LBA_Offset   : Unsigned_64;
      LBA_Length   : Unsigned_64;
   end record;
   type Partition_Data_Acc is access all Partition_Data;
   procedure Set_Part
      (Name       : String;
       Index      : Positive;
       Block_Size : Natural;
       Part       : Partition_Data_Acc;
       ID         : UUID;
       Success    : out Boolean);
   ----------------------------------------------------------------------------
   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Sync (Key : System.Address; Success : out Boolean);

   procedure Sync_Range
      (Key     : System.Address;
       Offset  : Unsigned_64;
       Count   : Unsigned_64;
       Success : out Boolean);
end Devices.Partitions;
