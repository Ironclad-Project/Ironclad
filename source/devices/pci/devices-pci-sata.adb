--  devices-sata.adb: SATA driver.
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

with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;
with Messages;
with Devices.Partitions;
with Memory.MMU;
with Memory;
with Alignment;
with Cryptography.Random;
with Arch.MMU;

package body Devices.PCI.SATA with SPARK_Mode => Off is
   package C1 is new System.Address_To_Access_Conversions (SATA_Data);
   package C2 is new System.Address_To_Access_Conversions (HBA_Memory);
   package C3 is new System.Address_To_Access_Conversions (FIS_Host_To_Device);
   package A  is new Alignment (Unsigned_64);

   procedure Init (Success : out Boolean) is
      PCI_Dev    : Devices.PCI.PCI_Device;
      PCI_BAR    : Devices.PCI.Base_Address_Register;
      Drive_Data : SATA_Data_Acc;
      Mem_Addr   : Integer_Address;
      Dev_Mem    : HBA_Memory_Acc;
      Drive_Idx  : Natural := 0;
      Base_Name  : constant String := "sata";
   begin
      Success := True;
      for Idx in 1 .. Devices.PCI.Enumerate_Devices (1, 6, 1) loop
         Devices.PCI.Search_Device (1, 6, 1, Idx, PCI_Dev, Success);
         if not Success then
            Success := True;
            return;
         end if;

         Devices.PCI.Get_BAR (PCI_Dev, 5, PCI_BAR, Success);
         if not Success or else not PCI_BAR.Is_MMIO then
            Success := True;
            return;
         end if;

         Devices.PCI.Enable_Bus_Mastering (PCI_Dev);
         Mem_Addr := PCI_BAR.Base + Memory.Memory_Offset;
         Dev_Mem  := HBA_Memory_Acc (C2.To_Pointer (To_Address (Mem_Addr)));

         Memory.MMU.Map_Range
            (Map            => Memory.MMU.Kernel_Table,
             Physical_Start => To_Address (PCI_BAR.Base),
             Virtual_Start  => To_Address (Mem_Addr),
             Length         => Storage_Count (A.Align_Up
              (HBA_Memory'Size / 8, Memory.MMU.Page_Size)),
             Permissions    =>
              (Is_User_Accessible => False,
               Can_Read          => True,
               Can_Write         => True,
               Can_Execute       => False,
               Is_Global         => True),
             Success        => Success,
             Caching        => Arch.MMU.Uncacheable);
         if not Success then
            return;
         end if;

         for I in 1 .. Ports_Per_Controller loop
            Drive_Data := Init_Port (Dev_Mem, I);
            if Drive_Data /= null then
               Drive_Idx := Drive_Idx + 1;

               declare
                  Final_Name : constant String := Base_Name & Drive_Idx'Image;
               begin
                  Register (
                     (Data => C1.To_Address (C1.Object_Pointer (Drive_Data)),
                      ID          => Zero_UUID,
                      Is_Block    => True,
                      Block_Size  => Sector_Size,
                      Block_Count => Drive_Data.Sector_Count,
                      Read        => Read'Access,
                      Write       => Write'Access,
                      Sync        => Sync'Access,
                      Sync_Range  => Sync_Range'Access,
                      IO_Control  => null,
                      Mmap        => null,
                      Poll        => null,
                      Remove      => null), Final_Name, Success);
                  if not Success or else
                     not Partitions.Parse_Partitions
                        (Final_Name, Fetch (Final_Name))
                  then
                     Success := False;
                     return;
                  end if;
               end;
            end if;
         end loop;
      end loop;
   exception
      when Constraint_Error =>
         Success := False;
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
      Port_Data := M.Ports (I)'Access;
      IPM := Unsigned_8 (Shift_Right (Port_Data.SATA_Status, 8) and 16#F#);
      Det := Unsigned_8 (Shift_Right (Port_Data.SATA_Status, 0) and 16#F#);
      if IPM /= 16#1# or Det /= 16#3# or Port_Data.Signature /= 16#101# then
         return null;
      end if;

      --  Stop the command engine.
      Stop_Command_Engine (Port_Data);

      --  Allocate and initialize all the structures needed in advance.
      Port_FIS := new HBA_FIS'
         (DS_FIS     => (FIS_Type => FIS_Type_DMA_Setup, others => <>),
          Reserved_1 => [others => 0],
          PS_FIS     => (FIS_Type => FIS_Type_PIO_Setup, others => <>),
          Reserved_2 => [others => 0],
          Reg_FIS    => (FIS_Type => FIS_Type_D2H, others => <>),
          Reserved_3 => [others => 0],
          SDB_FIS    => [1 => FIS_Type_Device_Bits, others => 0],
          U_FIS      => [others => 0],
          Reserved_4 => [others => 0]);

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

      --  Start the command engine.
      Start_Command_Engine (Port_Data);

      --  Find a slot for the identify command and setup the header.
      Identify := new SATA_Identify;
      Tmp      := To_Integer  (Identify.all'Address);
      Tmp2     := Unsigned_64 (Tmp - Memory.Memory_Offset);
      Dev_Data := new SATA_Data'
         (Mutex         => Synchronization.Unlocked_Mutex,
          FIS           => Port_FIS,
          Command_Area  => Cmd_Area,
          Command_TBLs  => Cmd_TBLs,
          Port_Data     => Port_Data,
          Sector_Count  => 0,
          Cache_Reg     => <>);
      Caching.Init
         (Dev_Data.all'Address,
          Read_Sector'Address,
          Write_Sector'Address,
          Dev_Data.Cache_Reg);
      Success := Issue_Command
         (Drive       => Dev_Data,
          LBA         => 0,
          Command     => 16#EC#,
          Is_Identify => True,
          Is_Write    => False,
          Data_Addr   => Tmp2);
      if not Success then
         Messages.Put_Line ("SATA error while issuing identify");
         return null;
      end if;

      declare
         Sectors : Unsigned_64 with Import, Address => Identify (101)'Address;
         Serial  : Cryptography.Random.Crypto_Data (1 .. 20)
            with Import, Address => Identify (10)'Address;
      begin
         --  Serial numbers make great randomness to add.
         Cryptography.Random.Feed_Entropy (Serial);
         Dev_Data.Sector_Count := Sectors;
      end;
      return Dev_Data;
   exception
      when Constraint_Error =>
         return null;
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
      --  Make sure we are not going to do all of this for an LBA that does not
      --  even exist.
      if not Is_Identify and LBA >= Drive.Sector_Count then
         return False;
      end if;

      Synchronization.Seize (Drive.Mutex);

      --  Find a slot for the identify command and setup the header.
      loop
         Slot := Find_Command_Slot (Drive.Port_Data);
         exit when Slot /= 0;
      end loop;

      Tmp := (FIS_Host_To_Device'Size / 8) / 4;
      Drive.Command_Area.Headers (Slot).Control_Flags := Unsigned_5 (Tmp);
      Drive.Command_Area.Headers (Slot).A := False;
      Drive.Command_Area.Headers (Slot).W := Is_Write;
      Drive.Command_Area.Headers (Slot).P := False;
      Drive.Command_Area.Headers (Slot).R := False;
      Drive.Command_Area.Headers (Slot).B := False;
      Drive.Command_Area.Headers (Slot).C := False;
      Drive.Command_Area.Headers (Slot).PMPort := 0;
      Drive.Command_Area.Headers (Slot).PRDTL  := 1;
      Drive.Command_Area.Headers (Slot).PRDBC  := 0;

      --  Fill the command table of the header with the command.
      Drive.Command_TBLs (Slot).FIS_Buffer := [others => 0];
      Drive.Command_TBLs (Slot).Command := [others => 0];
      Drive.Command_TBLs (Slot).PRDT.DBA :=
         Unsigned_32 (Data_Addr and 16#FFFFFFFF#);
      Drive.Command_TBLs (Slot).PRDT.DBAU :=
         Unsigned_32 (Shift_Right (Data_Addr, 32));
      Drive.Command_TBLs (Slot).PRDT.DBC := 511;
      Drive.Command_TBLs (Slot).PRDT.I := True;

      --  Fill the identify FIS.
      FIS_Ptr := C3.To_Pointer (Drive.Command_TBLs (Slot).FIS_Buffer'Address);
      FIS_Ptr.FIS_Type := FIS_Type_H2D;
      FIS_Ptr.PMPort := 0;
      FIS_Ptr.Command_Or_Control := True;
      FIS_Ptr.Command := Command;
      FIS_Ptr.Feature_Low := 0;
      FIS_Ptr.LBA0 := Unsigned_8 (L0);
      FIS_Ptr.LBA1 := Unsigned_8 (L1);
      FIS_Ptr.LBA2 :=  Unsigned_8 (L2);
      FIS_Ptr.Device := (if Is_Identify then 0 else Shift_Left (1, 6));
      FIS_Ptr.LBA3 := Unsigned_8 (L3);
      FIS_Ptr.LBA4 := Unsigned_8 (L4);
      FIS_Ptr.LBA5 := Unsigned_8 (L5);
      FIS_Ptr.Feature_High := 0;
      FIS_Ptr.Count_Low := (if Is_Identify then 0 else 1);
      FIS_Ptr.Count_High := 0;
      FIS_Ptr.Command_Completion := 0;
      FIS_Ptr.Control := 0;

      --  Wait for completion of the port, we dont want to issue commands
      --  while busy.
      Spin := 0;
      Tmp3 := ATA_Device_Busy or ATA_Device_DRQ;
      loop
         if (Drive.Port_Data.Task_File_Data and Tmp3) = 0 then
            exit;
         end if;

         if Spin >= 1000000 then
            goto Failure_Cleanup;
         end if;

         Spin := Spin + 1;
      end loop;

      --  Issue the command.
      Drive.Port_Data.Command_Issue := Shift_Left (1, Slot - 1);

      --  Unlock for other commands.
      Synchronization.Release (Drive.Mutex);

      --  Poll for success.
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

      return True;

   <<Failure_Cleanup>>
      Synchronization.Release (Drive.Mutex);
      return False;
   exception
      when Constraint_Error =>
         return False;
   end Issue_Command;

   procedure Read_Sector
      (D           : System.Address;
       LBA         : Unsigned_64;
       Data_Buffer : out Caching.Sector_Data;
       Success     : out Boolean)
   is
      Drive : constant SATA_Data_Acc := SATA_Data_Acc (C1.To_Pointer (D));
      SAddr : constant  System.Address := Data_Buffer'Address;
      IAddr : constant Integer_Address := To_Integer (SAddr);
   begin
      Success := Issue_Command
         (Drive       => Drive,
          LBA         => LBA,
          Command     => 16#25#,
          Is_Identify => False,
          Is_Write    => False,
          Data_Addr   => Unsigned_64 (IAddr - Memory.Memory_Offset));
   end Read_Sector;

   procedure Write_Sector
      (D           : System.Address;
       LBA         : Unsigned_64;
       Data_Buffer : Caching.Sector_Data;
       Success     : out Boolean)
   is
      Drive : constant SATA_Data_Acc := SATA_Data_Acc (C1.To_Pointer (D));
      SAddr : constant  System.Address := Data_Buffer'Address;
      IAddr : constant Integer_Address := To_Integer (SAddr);
   begin
      Success := Issue_Command
         (Drive       => Drive,
          LBA         => LBA,
          Command     => 16#35#,
          Is_Identify => False,
          Is_Write    => True,
          Data_Addr   => Unsigned_64 (IAddr - Memory.Memory_Offset));
   end Write_Sector;

   function Find_Command_Slot (Port : HBA_Port_Acc) return Natural is
      Slots : Unsigned_32;
   begin
      Slots := Port.SATA_Active or Port.Command_Issue;
      for I in 1 .. Ports_Per_Controller loop
         if (Slots and Shift_Left (1, I - 1)) = 0 then
            return I;
         end if;
      end loop;

      return 0;
   exception
      when Constraint_Error =>
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
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Failed to start SATA command line");
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
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Failed to stop SATA command line");
   end Stop_Command_Engine;
   ----------------------------------------------------------------------------
   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Is_Blocking);
      D : constant SATA_Data_Acc := SATA_Data_Acc (C1.To_Pointer (Key));
   begin
      Caching.Read
         (Registry  => D.Cache_Reg,
          Offset    => Offset,
          Data      => Data,
          Ret_Count => Ret_Count,
          Success   => Success);
   exception
      when Constraint_Error =>
         Data      := [others => 0];
         Success   := Dev_IO_Failure;
         Ret_Count := 0;
   end Read;

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Is_Blocking);
      D : constant SATA_Data_Acc := SATA_Data_Acc (C1.To_Pointer (Key));
   begin
      Caching.Write
         (Registry  => D.Cache_Reg,
          Offset    => Offset,
          Data      => Data,
          Ret_Count => Ret_Count,
          Success   => Success);
   exception
      when Constraint_Error =>
         Success   := Dev_IO_Failure;
         Ret_Count := 0;
   end Write;

   procedure Sync (Key : System.Address; Success : out Boolean) is
      Drive : constant SATA_Data_Acc := SATA_Data_Acc (C1.To_Pointer (Key));
   begin
      Caching.Sync (Drive.Cache_Reg, Success);
   exception
      when Constraint_Error =>
         Success := False;
   end Sync;

   procedure Sync_Range
      (Key     : System.Address;
       Offset  : Unsigned_64;
       Count   : Unsigned_64;
       Success : out Boolean)
   is
      Drive : constant SATA_Data_Acc := SATA_Data_Acc (C1.To_Pointer (Key));
   begin
      Caching.Sync_Range (Drive.Cache_Reg, Offset, Count, Success);
   exception
      when Constraint_Error =>
         Success := False;
   end Sync_Range;
end Devices.PCI.SATA;
