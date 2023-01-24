--  devices-partitions.ads: Split a block device into partitions.
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

package Devices.Partitions with SPARK_Mode => Off is
   --  Register the partitions of the passed device as separate devices.
   --  The resulting support read/write, and that's it.
   --  @param Name Name to prepend all partitions with, p<index> will be added.
   --  @param Dev Device to scan partitions for.
   --  @return True if everything is nominal, False if one of the systems
   --  failed unexpectedly or the requirements are not met.
   function Parse_Partitions
      (Name : String;
       Dev  : Device_Handle) return Boolean;

private

   --  Scan partitions for GPT and MBR.
   function Parse_GPT_Partitions
      (Name                 : String;
       Dev                  : Device_Handle;
       Found_Any_Partitions : out Boolean) return Boolean;
   function Parse_MBR_Partitions
      (Name                 : String;
       Dev                  : Device_Handle;
       Found_Any_Partitions : out Boolean) return Boolean;

   --  Register data for a partition.
   type Partition_Data is record
      Inner_Device : Device_Handle;
      Block_Size   : Unsigned_64;
      LBA_Offset   : Unsigned_64;
      LBA_Length   : Unsigned_64;
   end record;
   type Partition_Data_Acc is access all Partition_Data;
   function Set_Part
      (Name       : String;
       Index      : Positive;
       Block_Size : Unsigned_64;
       Part       : Partition_Data_Acc) return Boolean;
   ----------------------------------------------------------------------------
   function Read
      (Data   : Resource_Acc;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64;

   function Write
      (Data     : Resource_Acc;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64;
end Devices.Partitions;
