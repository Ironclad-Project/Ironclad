--  devices-sata.ads: SATA driver.
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

with Lib.Synchronization;

package Devices.SATA with SPARK_Mode => Off is
   --  Probe for ATA drives and add em.
   function Init return Boolean;

private

   Sector_Size          : constant := 512;
   Ports_Per_Controller : constant := 32;

   ATA_Device_Busy : constant := 16#80#;
   ATA_Device_DRQ  : constant := 16#08#;

   HBA_PxCMD_ST  : constant := 16#0001#;
   HBA_PxCMD_FRE : constant := 16#0010#;
   HBA_PxCMD_FR  : constant := 16#4000#;
   HBA_PxCMD_CR  : constant := 16#8000#;

   type HBA_Padding is array (Natural range <>) of Unsigned_8;
   type HBA_Port is record
      Command_List_Base  : Unsigned_32;
      Command_List_Upper : Unsigned_32;
      FIS_Base           : Unsigned_32;
      FIS_Upper          : Unsigned_32;
      Interrupt_Status   : Unsigned_32;
      Interrupt_Enable   : Unsigned_32;
      Command_And_Status : Unsigned_32;
      Reserved_1         : Unsigned_32;
      Task_File_Data     : Unsigned_32;
      Signature          : Unsigned_32;
      SATA_Status        : Unsigned_32;
      SATA_Control       : Unsigned_32;
      SATA_Error         : Unsigned_32;
      SATA_Active        : Unsigned_32;
      Command_Issue      : Unsigned_32;
      SATA_Notification  : Unsigned_32;
      FIS_Switch_Control : Unsigned_32;
      Reserved_2         : HBA_Padding (1 .. 44);
      Vendor_Specific    : HBA_Padding (1 .. 16);
   end record with Volatile;

   type HBA_Port_Acc is access all HBA_Port with Volatile;
   type HBA_Port_Arr is array (Natural range <>) of aliased HBA_Port with Pack;

   type HBA_Memory is record
      Host_Capabilities_1 : Unsigned_32;
      Global_Host_Control : Unsigned_32;
      Interrupt_Status    : Unsigned_32;
      Port_Usage_Bitmap   : Unsigned_32;
      Version             : Unsigned_32;
      Coalescing_Control  : Unsigned_32;
      Coalescing_Ports    : Unsigned_32;
      Enclosure_Location  : Unsigned_32;
      Enclosure_Control   : Unsigned_32;
      Host_Capabilities_2 : Unsigned_32;
      Handoff_Control     : Unsigned_32;
      Reserved_1          : HBA_Padding (1 .. 116);
      Vendor_Specific     : HBA_Padding (1 .. 96);
      Ports               : HBA_Port_Arr (1 .. Ports_Per_Controller);
   end record with Volatile;
   type HBA_Memory_Acc is access all HBA_Memory;

   type Unsigned_4 is mod 2 ** 4;
   type Unsigned_5 is mod 2 ** 5;
   type HBA_Command_Header is record
      Control_Flags : Unsigned_5;
      A             : Boolean;
      W             : Boolean;
      P             : Boolean;
      R             : Boolean;
      B             : Boolean;
      C             : Boolean;
      Reserved_1    : Boolean;
      PMPort        : Unsigned_4;
      PRDTL         : Unsigned_16;
      PRDBC         : Unsigned_32 with Volatile;
      CTBA          : Unsigned_32;
      CTBAU         : Unsigned_32;
      Reserved_2    : HBA_Padding (1 .. 16);
   end record with Size => 256, Volatile;
   for HBA_Command_Header use record
      Control_Flags at 0 range   0 ..   4;
      A             at 0 range   5 ..   5;
      W             at 0 range   6 ..   6;
      P             at 0 range   7 ..   7;
      R             at 0 range   8 ..   8;
      B             at 0 range   9 ..   9;
      C             at 0 range  10 ..  10;
      Reserved_1    at 0 range  11 ..  11;
      PMPort        at 0 range  12 ..  15;
      PRDTL         at 0 range  16 ..  31;
      PRDBC         at 0 range  32 ..  63;
      CTBA          at 0 range  64 ..  95;
      CTBAU         at 0 range  96 .. 127;
      Reserved_2    at 0 range 128 .. 255;
   end record;
   type HBA_Command_Headers is array (Natural range <>) of HBA_Command_Header;

   type Unsigned_9  is mod 2 **  9;
   type Unsigned_22 is mod 2 ** 22;
   type HBA_PRDTL is record
      DBA        : Unsigned_32;
      DBAU       : Unsigned_32;
      Reserved_1 : Unsigned_32;
      DBC        : Unsigned_22;
      Reserved_2 : Unsigned_9;
      I          : Boolean;
   end record with Size => 128;
   for HBA_PRDTL use record
      DBA        at 0 range   0 ..  31;
      DBAU       at 0 range  32 ..  63;
      Reserved_1 at 0 range  64 ..  95;
      DBC        at 0 range  96 .. 117;
      Reserved_2 at 0 range 118 .. 126;
      I          at 0 range 127 .. 127;
   end record;

   type HBA_Command_TBL is record
      FIS_Buffer : HBA_Padding (1 .. 64);
      Command    : HBA_Padding (1 .. 16);
      Reserved   : HBA_Padding (1 .. 48);
      PRDT       : HBA_PRDTL;
      Padding    : HBA_Padding (1 .. 3952); --  Padding to 4k.
   end record with Size => 32768, Volatile;
   for HBA_Command_TBL use record
      FIS_Buffer at 0 range    0 ..   511;
      Command    at 0 range  512 ..   639;
      Reserved   at 0 range  640 ..  1023;
      PRDT       at 0 range 1024 ..  1151;
      Padding    at 0 range 1152 .. 32767;
   end record;
   type HBA_Command_TBL_Acc is access all HBA_Command_TBL;
   type HBA_Command_TBL_Arr is array (Natural range <>) of HBA_Command_TBL_Acc;

   type HBA_Command_Area is record
      Headers : HBA_Command_Headers (1 .. Ports_Per_Controller);
      Padding : HBA_Padding (1 .. 3072); --  Padding so it is 4k aligned.
   end record with Size => 32768, Volatile;
   for HBA_Command_Area use record
      Headers at 0 range    0 ..  8191;
      Padding at 0 range 8192 .. 32767;
   end record;
   type HBA_Command_Area_Acc is access HBA_Command_Area;

   FIS_Type_DMA_Setup   : constant := 16#41#;
   FIS_Type_PIO_Setup   : constant := 16#5F#;
   FIS_Type_H2D         : constant := 16#27#;
   FIS_Type_D2H         : constant := 16#34#;
   FIS_Type_Device_Bits : constant := 16#A1#;

   type FIS_DMA_Setup is record
      FIS_Type           : Unsigned_8;
      PMPort             : Unsigned_4;
      Reserved_1         : Boolean;
      Transfer_Direction : Boolean;
      Interrupt          : Boolean;
      Reserved_2         : Boolean;
      Reserved_3         : HBA_Padding (1 .. 2);
      Buffer_ID          : Unsigned_64;
      Reserved_4         : Unsigned_32;
      Buffer_Offset      : Unsigned_32;
      Transfer_Count     : Unsigned_32;
      Reserved_5         : Unsigned_32;
   end record with Size => 224, Volatile;
   for FIS_DMA_Setup use record
      FIS_Type           at 0 range   0 ..   7;
      PMPort             at 0 range   8 ..  11;
      Reserved_1         at 0 range  12 ..  12;
      Transfer_Direction at 0 range  13 ..  13;
      Interrupt          at 0 range  14 ..  14;
      Reserved_2         at 0 range  15 ..  15;
      Reserved_3         at 0 range  16 ..  31;
      Buffer_ID          at 0 range  32 ..  95;
      Reserved_4         at 0 range  96 .. 127;
      Buffer_Offset      at 0 range 128 .. 159;
      Transfer_Count     at 0 range 160 .. 191;
      Reserved_5         at 0 range 192 .. 223;
   end record;

   type FIS_PIO_Setup is record
      FIS_Type           : Unsigned_8;
      PMPort             : Unsigned_4;
      Reserved_1         : Boolean;
      Transfer_Direction : Boolean;
      Interrupt          : Boolean;
      Reserved_2         : Boolean;
      Status             : Unsigned_8;
      Error              : Unsigned_8;
      LBA0               : Unsigned_8;
      LBA1               : Unsigned_8;
      LBA2               : Unsigned_8;
      Device             : Unsigned_8;
      LBA3               : Unsigned_8;
      LBA4               : Unsigned_8;
      LBA5               : Unsigned_8;
      Reserved_3         : Unsigned_8;
      Count_Low          : Unsigned_8;
      Count_High         : Unsigned_8;
      Reserved_4         : Unsigned_8;
      New_Status         : Unsigned_8;
      Transfer_Count     : Unsigned_8;
      Reserved_5         : HBA_Padding (1 .. 2);
   end record with Size => 152, Volatile;
   for FIS_PIO_Setup use record
      FIS_Type           at 0 range   0 ..   7;
      PMPort             at 0 range   8 ..  11;
      Reserved_1         at 0 range  12 ..  12;
      Transfer_Direction at 0 range  13 ..  13;
      Interrupt          at 0 range  14 ..  14;
      Reserved_2         at 0 range  15 ..  15;
      Status             at 0 range  16 ..  23;
      Error              at 0 range  24 ..  31;
      LBA0               at 0 range  32 ..  39;
      LBA1               at 0 range  40 ..  47;
      LBA2               at 0 range  48 ..  55;
      Device             at 0 range  56 ..  63;
      LBA3               at 0 range  64 ..  71;
      LBA4               at 0 range  72 ..  79;
      LBA5               at 0 range  80 ..  87;
      Reserved_3         at 0 range  88 ..  95;
      Count_Low          at 0 range  96 .. 103;
      Count_High         at 0 range 104 .. 111;
      Reserved_4         at 0 range 112 .. 119;
      New_Status         at 0 range 120 .. 127;
      Transfer_Count     at 0 range 128 .. 135;
      Reserved_5         at 0 range 136 .. 151;
   end record;

   type FIS_Host_To_Device is record
      FIS_Type           : Unsigned_8;
      PMPort             : Unsigned_4;
      Reserved_1         : Boolean;
      Reserved_2         : Boolean;
      Reserved_3         : Boolean;
      Command_Or_Control : Boolean;
      Command            : Unsigned_8;
      Feature_Low        : Unsigned_8;
      LBA0               : Unsigned_8;
      LBA1               : Unsigned_8;
      LBA2               : Unsigned_8;
      Device             : Unsigned_8;
      LBA3               : Unsigned_8;
      LBA4               : Unsigned_8;
      LBA5               : Unsigned_8;
      Feature_High       : Unsigned_8;
      Count_Low          : Unsigned_8;
      Count_High         : Unsigned_8;
      Command_Completion : Unsigned_8;
      Control            : Unsigned_8;
      Reserved_4         : HBA_Padding (1 .. 4);
   end record with Size => 160, Volatile;
   for FIS_Host_To_Device use record
      FIS_Type           at 0 range   0 ..   7;
      PMPort             at 0 range   8 ..  11;
      Reserved_1         at 0 range  12 ..  12;
      Reserved_2         at 0 range  13 ..  13;
      Reserved_3         at 0 range  14 ..  14;
      Command_Or_Control at 0 range  15 ..  15;
      Command            at 0 range  16 ..  23;
      Feature_Low        at 0 range  24 ..  31;
      LBA0               at 0 range  32 ..  39;
      LBA1               at 0 range  40 ..  47;
      LBA2               at 0 range  48 ..  55;
      Device             at 0 range  56 ..  63;
      LBA3               at 0 range  64 ..  71;
      LBA4               at 0 range  72 ..  79;
      LBA5               at 0 range  80 ..  87;
      Feature_High       at 0 range  88 ..  95;
      Count_Low          at 0 range  96 .. 103;
      Count_High         at 0 range 104 .. 111;
      Command_Completion at 0 range 112 .. 119;
      Control            at 0 range 120 .. 127;
      Reserved_4         at 0 range 128 .. 159;
   end record;

   type FIS_Device_To_Host is record
      FIS_Type           : Unsigned_8;
      PMPort             : Unsigned_4;
      Reserved_1         : Boolean;
      Transfer_Direction : Boolean;
      Interrupt          : Boolean;
      Reserved_2         : Boolean;
      Status             : Unsigned_8;
      Error              : Unsigned_8;
      LBA0               : Unsigned_8;
      LBA1               : Unsigned_8;
      LBA2               : Unsigned_8;
      Device             : Unsigned_8;
      LBA3               : Unsigned_8;
      LBA4               : Unsigned_8;
      LBA5               : Unsigned_8;
      Reserved_3         : Unsigned_8;
      Count_Low          : Unsigned_8;
      Count_High         : Unsigned_8;
      Reserved_4         : HBA_Padding (1 .. 6);
   end record with Size => 160, Volatile;
   for FIS_Device_To_Host use record
      FIS_Type           at 0 range   0 ..   7;
      PMPort             at 0 range   8 ..  11;
      Reserved_1         at 0 range  12 ..  12;
      Transfer_Direction at 0 range  13 ..  13;
      Interrupt          at 0 range  14 ..  14;
      Reserved_2         at 0 range  15 ..  15;
      Status             at 0 range  16 ..  23;
      Error              at 0 range  24 ..  31;
      LBA0               at 0 range  32 ..  39;
      LBA1               at 0 range  40 ..  47;
      LBA2               at 0 range  48 ..  55;
      Device             at 0 range  56 ..  63;
      LBA3               at 0 range  64 ..  71;
      LBA4               at 0 range  72 ..  79;
      LBA5               at 0 range  80 ..  87;
      Reserved_3         at 0 range  88 ..  95;
      Count_Low          at 0 range  96 .. 103;
      Count_High         at 0 range 104 .. 111;
      Reserved_4         at 0 range 112 .. 159;
   end record;

   type HBA_FIS is record
      DS_FIS     : FIS_DMA_Setup;
      Reserved_1 : HBA_Padding (1 .. 4);
      PS_FIS     : FIS_PIO_Setup;
      Reserved_2 : HBA_Padding (1 .. 12);
      Reg_FIS    : FIS_Device_To_Host;
      Reserved_3 : HBA_Padding (1 .. 4);
      SDB_FIS    : HBA_Padding (1 .. 8);
      U_FIS      : HBA_Padding (1 .. 64);
      Reserved_4 : HBA_Padding (1 .. 96);
   end record with Size => 2048, Volatile;
   for HBA_FIS use record
      DS_FIS     at 0 range    0 ..  223;
      Reserved_1 at 0 range  224 ..  255;
      PS_FIS     at 0 range  256 ..  407;
      Reserved_2 at 0 range  408 ..  503;
      Reg_FIS    at 0 range  504 ..  663;
      Reserved_3 at 0 range  664 ..  695;
      SDB_FIS    at 0 range  696 ..  759;
      U_FIS      at 0 range  760 .. 1271;
      Reserved_4 at 0 range 1272 .. 2039;
   end record;
   type HBA_FIS_Acc is access HBA_FIS;

   --  Data stored for each drive.
   subtype Sector_Data is Operation_Data (1 .. Sector_Size);
   type Sector_Cache is record
      Is_Used    : Boolean;
      LBA_Offset : Unsigned_64;
      Is_Dirty   : Boolean;
      Data       : Sector_Data;
   end record;
   type Sector_Caches is array (Natural range <>) of Sector_Cache;

   type SATA_Identify is array (1 .. 256) of Unsigned_16;
   type SATA_Identify_Acc is access all SATA_Identify;
   type SATA_Data is record
      Mutex        : aliased Lib.Synchronization.Binary_Semaphore;
      FIS          : HBA_FIS_Acc;
      Command_Area : HBA_Command_Area_Acc;
      Command_TBLs : HBA_Command_TBL_Arr (1 .. Ports_Per_Controller);
      Port_Data    : HBA_Port_Acc;
      Sector_Count : Unsigned_64;
      Caches       : Sector_Caches (1 .. 8000);
      Next_Evict   : Natural range 1 .. 8000;
   end record;
   type SATA_Data_Acc is access all SATA_Data;

   --  Probe a port and return an initialized ATA drive, or null if not found.
   function Init_Port (M : HBA_Memory_Acc; I : Natural) return SATA_Data_Acc;

   --  Issue a command, and return success or failure.
   function Issue_Command
      (Drive       : SATA_Data_Acc;
       LBA         : Unsigned_64;
       Command     : Unsigned_8;
       Is_Identify : Boolean;
       Is_Write    : Boolean;
       Data_Addr   : Unsigned_64) return Boolean;

   --  Read a single sector.
   function Read_Sector
      (Drive       : SATA_Data_Acc;
       LBA         : Unsigned_64;
       Data_Buffer : out Sector_Data) return Boolean;

   --  Write a single sector.
   function Write_Sector
      (Drive       : SATA_Data_Acc;
       LBA         : Unsigned_64;
       Data_Buffer : Sector_Data) return Boolean;

   --  Find a cache index for the drive given the desired LBA.
   procedure Get_Cache_Index
      (Drive   : SATA_Data_Acc;
       LBA     : Unsigned_64;
       Idx     : out Natural;
       Success : out Boolean);

   --  Find a free slot in the command header table of a port.
   function Find_Command_Slot (Port : HBA_Port_Acc) return Natural;

   --  Start and stop the command engine of a port.
   procedure Start_Command_Engine (Port : HBA_Port_Acc);
   procedure Stop_Command_Engine  (Port : HBA_Port_Acc);
   ----------------------------------------------------------------------------
   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean);

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean);

   function Sync (Key : System.Address) return Boolean;

   function Sync_Range
      (Key    : System.Address;
       Offset : Unsigned_64;
       Count  : Unsigned_64) return Boolean;
end Devices.SATA;
