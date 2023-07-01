--  devices-sata.adb: SATA driver.
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

with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;
with Lib.Messages;
with Devices.Partitions;
with Arch.PCI;
with Memory;

package body Devices.SATA with SPARK_Mode => Off is
   package C1 is new System.Address_To_Access_Conversions (SATA_Data);
   package C2 is new System.Address_To_Access_Conversions (HBA_Memory);
   package C3 is new System.Address_To_Access_Conversions (FIS_Host_To_Device);

   function Init return Boolean is
      PCI_Dev    : Arch.PCI.PCI_Device;
      PCI_BAR    : Arch.PCI.Base_Address_Register;
      Drive_Data : SATA_Data_Acc;
      Mem_Addr   : Integer_Address;
      Dev_Mem    : HBA_Memory_Acc;
      Base_Name  : String := "sata00";
      Success    : Boolean;
   begin
      if not Arch.PCI.Search_Device (1, 6, 1, PCI_Dev) or else
         not Arch.PCI.Get_BAR (PCI_Dev, 5, PCI_BAR)    or else
         not PCI_BAR.Is_MMIO
      then
         return True;
      end if;

      Arch.PCI.Enable_Bus_Mastering (PCI_Dev);
      Mem_Addr := PCI_BAR.Base + Memory.Memory_Offset;
      Dev_Mem  := HBA_Memory_Acc (C2.To_Pointer (To_Address (Mem_Addr)));

      for I in 1 .. Ports_Per_Controller loop
         Drive_Data := Init_Port (Dev_Mem, I);
         if Drive_Data /= null then
            Base_Name (5) := Character'Val ((I / 10) + Character'Pos ('0'));
            Base_Name (6) := Character'Val (I        + Character'Pos ('0'));
            Register (
               (Data        => C1.To_Address (C1.Object_Pointer (Drive_Data)),
                Is_Block    => True,
                Block_Size  => Sector_Size,
                Block_Count => Drive_Data.Sector_Count,
                Read        => Read'Access,
                Write       => Write'Access,
                Sync        => Sync'Access,
                Sync_Range  => Sync_Range'Access,
                IO_Control  => null,
                Mmap        => null), Base_Name, Success);
            if not Success or else
               not Partitions.Parse_Partitions (Base_Name, Fetch (Base_Name))
            then
               return False;
            end if;
         end if;
      end loop;

      return True;
   end Init;

   function Init_Port (M : HBA_Memory_Acc; I : Natural) return SATA_Data_Acc is
      Identify  : SATA_Identify_Acc;
      IPM, Det  : Unsigned_8;
      Port_Data : HBA_Port_Acc;
      Port_FIS  : HBA_FIS_Acc;
      Cmd_Area  : HBA_Command_Area_Acc;
      Cmd_TBLs  : HBA_Command_TBL_Arr (1 .. Ports_Per_Controller);
      Tmp       : Integer_Address;
      Tmp2      : Unsigned_64;
      Dev_Data  : SATA_Data_Acc;
      Success   : Boolean;
   begin
      --  Ensure the port is populated.
      if (M.Port_Usage_Bitmap and Shift_Left (1, I - 1)) = 0 then
         return null;
      end if;

      --  Check the drive is SATA and not a bridge, SATAPI, or anything weird.
      IPM := Unsigned_8 (Shift_Right (M.Ports (I).SATA_Status, 8) and 16#F#);
      Det := Unsigned_8 (Shift_Right (M.Ports (I).SATA_Status, 0) and 16#F#);
      if IPM /= 16#1# or Det /= 16#3# or M.Ports (I).Signature /= 16#101# then
         return null;
      end if;

      --  Allocate and initialize all the structures needed in advance.
      Port_FIS := new HBA_FIS'
         (DS_FIS     => (FIS_Type => FIS_Type_DMA_Setup, others => <>),
          Reserved_1 => (others => 0),
          PS_FIS     => (FIS_Type => FIS_Type_PIO_Setup, others => <>),
          Reserved_2 => (others => 0),
          Reg_FIS    => (FIS_Type => FIS_Type_D2H, others => <>),
          Reserved_3 => (others => 0),
          SDB_FIS    => (1 => FIS_Type_Device_Bits, others => 0),
          U_FIS      => (others => 0),
          Reserved_4 => (others => 0));

      Port_Data := M.Ports (I)'Access;
      Tmp       := To_Integer (Port_FIS.all'Address);
      Tmp2      := Unsigned_64 (Tmp - Memory.Memory_Offset);
      Port_Data.FIS_Base  := Unsigned_32 (Tmp2 and 16#FFFFFFFF#);
      Port_Data.FIS_Upper := Unsigned_32 (Shift_Right (Tmp2, 32));

      Cmd_Area := new HBA_Command_Area;
      Tmp      := To_Integer (Cmd_Area.all'Address);
      Tmp2     := Unsigned_64 (Tmp - Memory.Memory_Offset);
      Port_Data.Command_List_Base  := Unsigned_32 (Tmp2 and 16#FFFFFFFF#);
      Port_Data.Command_List_Upper := Unsigned_32 (Shift_Right (Tmp2, 32));

      for I in Cmd_Area.Headers'Range loop
         Cmd_TBLs (I) := new HBA_Command_TBL;
         Tmp          := To_Integer (Cmd_TBLs (I).all'Address);
         Tmp2         := Unsigned_64 (Tmp - Memory.Memory_Offset);
         Cmd_Area.Headers (I).PRDTL := 8;
         Cmd_Area.Headers (I).CTBA  := Unsigned_32 (Tmp2 and 16#FFFFFFFF#);
         Cmd_Area.Headers (I).CTBAU := Unsigned_32 (Shift_Right (Tmp2, 32));
      end loop;

      --  Find a slot for the identify command and setup the header.
      Identify := new SATA_Identify;
      Tmp      := To_Integer  (Identify.all'Address);
      Tmp2     := Unsigned_64 (Tmp - Memory.Memory_Offset);
      Dev_Data := new SATA_Data'
         (Mutex         => Lib.Synchronization.Unlocked_Semaphore,
          FIS           => Port_FIS,
          Command_Area  => Cmd_Area,
          Command_TBLs  => Cmd_TBLs,
          Port_Data     => Port_Data,
          Sector_Count  => 0,
          Caches        => (others => (Is_Used => False, others => <>)),
          Next_Evict    => 1);
      Success := Issue_Command
         (Drive       => Dev_Data,
          LBA         => 0,
          Command     => 16#EC#,
          Is_Identify => True,
          Is_Write    => False,
          Data_Addr   => Tmp2);
      if not Success then
         Lib.Messages.Warn ("SATA error while issuing identify");
         return null;
      end if;

      declare
         Sectors : Unsigned_64 with Address => Identify (101)'Address;
      begin
         Dev_Data.Sector_Count := Sectors;
      end;
      return Dev_Data;
   end Init_Port;

   function Issue_Command
      (Drive       : SATA_Data_Acc;
       LBA         : Unsigned_64;
       Command     : Unsigned_8;
       Is_Identify : Boolean;
       Is_Write    : Boolean;
       Data_Addr   : Unsigned_64) return Boolean
   is
      L0 : constant Unsigned_64 := Shift_Right (LBA and 16#0000000000FF#,  0);
      L1 : constant Unsigned_64 := Shift_Right (LBA and 16#00000000FF00#,  8);
      L2 : constant Unsigned_64 := Shift_Right (LBA and 16#000000FF0000#, 16);
      L3 : constant Unsigned_64 := Shift_Right (LBA and 16#0000FF000000#, 24);
      L4 : constant Unsigned_64 := Shift_Right (LBA and 16#00FF00000000#, 32);
      L5 : constant Unsigned_64 := Shift_Right (LBA and 16#FF0000000000#, 40);

      Tmp     : Integer_Address;
      Slot    : Natural;
      FIS_Ptr : C3.Object_Pointer;
      Spin    : Natural;
      Tmp3    : Unsigned_32;
   begin
      --  Find a slot for the identify command and setup the header.
      Slot := Find_Command_Slot (Drive.Port_Data);
      if Slot = 0 then
         return False;
      end if;

      Tmp := (FIS_Host_To_Device'Size / 8) / 4;
      Drive.Command_Area.Headers (Slot) :=
         (Control_Flags => Unsigned_5 (Tmp),
          A             => False,
          W             => Is_Write,
          P             => False,
          R             => False,
          B             => False,
          C             => False,
          PMPort        => 0,
          PRDTL         => 1,
          Reserved_1    => False,
          PRDBC         => 0,
          Reserved_2    => (others => 0),
          CTBA          => Drive.Command_Area.Headers (Slot).CTBA,
          CTBAU         => Drive.Command_Area.Headers (Slot).CTBAU);

      --  Fill the command table of the header with the command.
      Drive.Command_TBLs (Slot).all :=
         (FIS_Buffer => (others => 0),
          Command    => (others => 0),
          Reserved   => (others => 0),
          PRDT       => (DBA    => Unsigned_32 (Data_Addr and 16#FFFFFFFF#),
                         DBAU   => Unsigned_32 (Shift_Right (Data_Addr, 32)),
                         DBC    => 511,
                         I      => True,
                         others => <>),
          Padding    => (others => <>));

      --  Fill the identify FIS.
      FIS_Ptr := C3.To_Pointer (Drive.Command_TBLs (Slot).FIS_Buffer'Address);
      FIS_Ptr.all :=
         (FIS_Type           => FIS_Type_H2D,
          PMPort             => 0,
          Reserved_1         => False,
          Reserved_2         => False,
          Reserved_3         => False,
          Command_Or_Control => True,
          Command            => Command,
          Feature_Low        => 0,
          LBA0               => Unsigned_8 (L0),
          LBA1               => Unsigned_8 (L1),
          LBA2               => Unsigned_8 (L2),
          Device             => (if Is_Identify then 0 else Shift_Left (1, 6)),
          LBA3               => Unsigned_8 (L3),
          LBA4               => Unsigned_8 (L4),
          LBA5               => Unsigned_8 (L5),
          Feature_High       => 0,
          Count_Low          => (if Is_Identify then 0 else 1),
          Count_High         => 0,
          Command_Completion => 0,
          Control            => 0,
          Reserved_4         => (others => 0));

      --  Wait for completion.
      Spin := 0;
      Tmp3 := ATA_Device_Busy or ATA_Device_DRQ;
      loop
         if (Drive.Port_Data.Task_File_Data and Tmp3) = 0 then
            exit;
         end if;

         if Spin >= 1000000 then
            return False;
         end if;

         Spin := Spin + 1;
      end loop;

      --  Start the command engine, poll for success, and stop.
      Start_Command_Engine (Drive.Port_Data);
      Drive.Port_Data.Command_Issue := Shift_Left (1, Slot - 1);
      loop
         if (Drive.Port_Data.Command_Issue and Shift_Left (1, Slot - 1)) = 0
         then
            exit;
         end if;

         if (Drive.Port_Data.Interrupt_Status and Shift_Left (1, 30)) /= 0 then
            return False;
         end if;
      end loop;
      Stop_Command_Engine (Drive.Port_Data);
      return True;
   end Issue_Command;

   function Read_Sector
      (Drive       : SATA_Data_Acc;
       LBA         : Unsigned_64;
       Data_Buffer : out Sector_Data) return Boolean
   is
      SAddr   : constant  System.Address := Data_Buffer'Address;
      IAddr   : constant Integer_Address := To_Integer (SAddr);
      Success : Boolean;
   begin
      Success := Issue_Command
         (Drive       => Drive,
          LBA         => LBA,
          Command     => 16#25#,
          Is_Identify => False,
          Is_Write    => False,
          Data_Addr   => Unsigned_64 (IAddr - Memory.Memory_Offset));
      if not Success then
         Lib.Messages.Warn ("SATA error while reading");
      end if;
      return Success;
   end Read_Sector;

   function Write_Sector
      (Drive       : SATA_Data_Acc;
       LBA         : Unsigned_64;
       Data_Buffer : Sector_Data) return Boolean
   is
      SAddr   : constant  System.Address := Data_Buffer'Address;
      IAddr   : constant Integer_Address := To_Integer (SAddr);
      Success : Boolean;
   begin
      Success := Issue_Command
         (Drive       => Drive,
          LBA         => LBA,
          Command     => 16#35#,
          Is_Identify => False,
          Is_Write    => True,
          Data_Addr   => Unsigned_64 (IAddr - Memory.Memory_Offset));
      if not Success then
         Lib.Messages.Warn ("SATA error while reading");
      end if;
      return Success;
   end Write_Sector;

   function Get_Cache_Index
      (Drive : SATA_Data_Acc;
       LBA   : Unsigned_64) return Natural
   is
      Success  : Boolean;
      Returned : Natural := 0;
   begin
      for I in Drive.Caches'Range loop
         if Drive.Caches (I).Is_Used and Drive.Caches (I).LBA_Offset = LBA then
            return I;
         elsif not Drive.Caches (I).Is_Used and Returned = 0 then
            Returned := I;
         end if;
      end loop;

      if Returned = 0 then
         Returned := Drive.Next_Evict;

         if Drive.Caches (Drive.Next_Evict).Is_Dirty then
            Success := Write_Sector
               (Drive       => Drive,
                LBA         => Drive.Caches (Returned).LBA_Offset,
                Data_Buffer => Drive.Caches (Returned).Data);
            if not Success then
               Lib.Messages.Warn ("SATA could not write on cache fetching!");
            end if;
         end if;

         if Drive.Next_Evict = Drive.Caches'Last then
            Drive.Next_Evict := Drive.Caches'First;
         else
            Drive.Next_Evict := Drive.Next_Evict + 1;
         end if;
      end if;

      Drive.Caches (Returned) :=
         (Is_Used    => True,
          LBA_Offset => LBA,
          Is_Dirty   => False,
          Data       => <>);

      Success := Read_Sector
         (Drive       => Drive,
          LBA         => Drive.Caches (Returned).LBA_Offset,
          Data_Buffer => Drive.Caches (Returned).Data);
      if not Success then
         Lib.Messages.Warn ("SATA could not read on cache fetching!");
      end if;

      return Returned;
   end Get_Cache_Index;

   function Find_Command_Slot (Port : HBA_Port_Acc) return Natural is
      Slots : constant Unsigned_32 := Port.SATA_Active or Port.Command_Issue;
   begin
      for I in 1 .. Ports_Per_Controller loop
         if (Slots and Shift_Left (1, I - 1)) = 0 then
            return I;
         end if;
      end loop;

      return 0;
   end Find_Command_Slot;

   procedure Start_Command_Engine (Port : HBA_Port_Acc) is
   begin
      Port.Command_And_Status := Port.Command_And_Status and not HBA_PxCMD_ST;
      loop
         if (Port.Command_And_Status and HBA_PxCMD_CR) = 0 then
            exit;
         end if;
      end loop;
      Port.Command_And_Status := Port.Command_And_Status or HBA_PxCMD_FRE;
      Port.Command_And_Status := Port.Command_And_Status or HBA_PxCMD_ST;
   end Start_Command_Engine;

   procedure Stop_Command_Engine (Port : HBA_Port_Acc) is
   begin
      Port.Command_And_Status := Port.Command_And_Status and not HBA_PxCMD_ST;
      loop
         if (Port.Command_And_Status and HBA_PxCMD_CR) = 0 then
            exit;
         end if;
      end loop;
      Port.Command_And_Status := Port.Command_And_Status and not HBA_PxCMD_FRE;
   end Stop_Command_Engine;
   ----------------------------------------------------------------------------
   procedure Read
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      D : constant SATA_Data_Acc := SATA_Data_Acc (C1.To_Pointer (Key));
      Cache_Idx, Progress, Copy_Count, Cache_Offset : Natural := 0;
      Current_LBA : Unsigned_64;
   begin
      if Data'Length = 0 then
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      Lib.Synchronization.Seize (D.Mutex);
      while Progress < Data'Length loop
         Current_LBA  := (Offset + Unsigned_64 (Progress)) / Sector_Size;
         Cache_Idx    := Get_Cache_Index (D, Current_LBA);
         Copy_Count   := Data'Length - Progress;
         Cache_Offset := Natural ((Offset + Unsigned_64 (Progress)) mod
                                  Sector_Size);
         if Copy_Count > Sector_Size - Cache_Offset then
            Copy_Count := Sector_Size - Cache_Offset;
         end if;
         Data (Data'First + Progress .. Data'First + Progress + Copy_Count - 1)
            := D.Caches (Cache_Idx).Data (Cache_Offset + 1 ..
                                          Cache_Offset + Copy_Count);
         Progress := Progress + Copy_Count;
      end loop;
      Lib.Synchronization.Release (D.Mutex);

      Ret_Count := Progress;
      Success   := True;
   end Read;

   procedure Write
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      D : constant SATA_Data_Acc := SATA_Data_Acc (C1.To_Pointer (Key));
      Cache_Idx, Progress, Copy_Count, Cache_Offset : Natural := 0;
      Current_LBA : Unsigned_64;
   begin
      if Data'Length = 0 then
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      Lib.Synchronization.Seize (D.Mutex);
      while Progress < Data'Length loop
         Current_LBA  := (Offset + Unsigned_64 (Progress)) / Sector_Size;
         Cache_Idx    := Get_Cache_Index (D, Current_LBA);
         Copy_Count   := Data'Length - Progress;
         Cache_Offset := Natural ((Offset + Unsigned_64 (Progress)) mod
                                  Sector_Size);
         if Copy_Count > Sector_Size - Cache_Offset then
            Copy_Count := Sector_Size - Cache_Offset;
         end if;
         D.Caches (Cache_Idx).Data (Cache_Offset + 1 ..
                                    Cache_Offset + Copy_Count) :=
            Data (Data'First + Progress ..
                  Data'First + Progress + Copy_Count - 1);
         D.Caches (Cache_Idx).Is_Dirty := True;
         Progress := Progress + Copy_Count;
      end loop;
      Lib.Synchronization.Release (D.Mutex);

      Ret_Count := Progress;
      Success   := True;
   end Write;

   function Sync (Key : System.Address) return Boolean is
      Success : Boolean;
      Drive   : constant SATA_Data_Acc := SATA_Data_Acc (C1.To_Pointer (Key));
   begin
      Lib.Synchronization.Seize (Drive.Mutex);
      for Cache of Drive.Caches loop
         if Cache.Is_Used and Cache.Is_Dirty then
            Success := Write_Sector
               (Drive       => Drive,
                LBA         => Cache.LBA_Offset,
                Data_Buffer => Cache.Data);
            if not Success then
               return False;
            end if;

            Cache.Is_Dirty := False;
         end if;
      end loop;
      Lib.Synchronization.Release (Drive.Mutex);
      return True;
   end Sync;

   function Sync_Range
      (Key    : System.Address;
       Offset : Unsigned_64;
       Count  : Unsigned_64) return Boolean
   is
      Drive : constant SATA_Data_Acc := SATA_Data_Acc (C1.To_Pointer (Key));
      Curr_Idx  : Unsigned_64;
      Cache_Idx : Natural;
      Success   : Boolean;
   begin
      Curr_Idx := Offset;
      Lib.Synchronization.Seize (Drive.Mutex);
      while Curr_Idx < Offset + Count loop
         Cache_Idx := Get_Cache_Index (Drive, Curr_Idx / Sector_Size);
         if Drive.Caches (Cache_Idx).Is_Used and
            Drive.Caches (Cache_Idx).Is_Dirty
         then
            Success := Write_Sector
               (Drive       => Drive,
                LBA         => Drive.Caches (Cache_Idx).LBA_Offset,
                Data_Buffer => Drive.Caches (Cache_Idx).Data);
            if not Success then
               return False;
            end if;
            Drive.Caches (Cache_Idx).Is_Dirty := False;
         end if;
         Curr_Idx := Curr_Idx + Sector_Size;
      end loop;
      Lib.Synchronization.Release (Drive.Mutex);
      return True;
   end Sync_Range;
end Devices.SATA;
