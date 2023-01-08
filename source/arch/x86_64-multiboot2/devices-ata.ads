--  devices-ata.ads: ATA driver.
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

package Devices.ATA with SPARK_Mode => Off is
   --  Probe for ATA drives and add em.
   function Init return Boolean;

private

   --  ATA drives come in pairs, master and slave, and usually found in one of
   --  these 2 ports.
   Pair_Ports : constant array (0 .. 1) of Unsigned_16 := (16#1F0#, 16#170#);

   --  Data stored for each drive.
   Sector_Size : constant := 512;
   type ATA_Identify is array (1 .. 256) of Unsigned_16;
   type ATA_Data is record
      Is_Master         : Boolean;
      Identify          : ATA_Identify;
      Data_Port         : Unsigned_16;
      Error_Port        : Unsigned_16;
      Sector_Count_Port : Unsigned_16;
      LBA_Low_Port      : Unsigned_16;
      LBA_Mid_Port      : Unsigned_16;
      LBA_High_Port     : Unsigned_16;
      Device_Port       : Unsigned_16;
      Command_Port      : Unsigned_16;
      Control_Port      : Unsigned_16;
      Sector_Count      : Unsigned_64;
   end record;
   type ATA_Data_Acc is access all ATA_Data;

   --  Probe a port and return an initialized ATA drive, or null if not found.
   function Init_Port (Port_Index : Natural) return ATA_Data_Acc;

   --  Read-write ATA function.
   function Read_Write
      (Drive         : ATA_Data_Acc;
       Offset_Sector : Unsigned_64;
       Count_Sector  : Unsigned_64;
       Is_Write      : Boolean;
       Data_Addr     : System.Address) return Boolean;
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
end Devices.ATA;
