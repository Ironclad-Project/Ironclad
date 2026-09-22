--  devices-pci-virtiorng.adb: VirtIO RNG devices.
--  Copyright (C) 2025 no95
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

with Ada.Unchecked_Deallocation;
with Alignment;
with Devices.PCI.Virtio; use Devices.PCI.Virtio;
with Messages;
with System.Address_To_Access_Conversions;

package body Devices.PCI.VirtioRNG with SPARK_Mode => Off is
   package A is new Alignment (Unsigned_32);

   package C1 is new System.Address_To_Access_Conversions (Pci_Common_Config);
   package C2 is new System.Address_To_Access_Conversions (Rng_Data);
   package C3 is new System.Address_To_Access_Conversions (Unsigned_16);

   procedure Init (Success : out Boolean) is
      PCI_Dev : Devices.PCI.PCI_Device;
      Cap_Offset : Unsigned_8;
      Cap_Type : Unsigned_8;
      Cap_BAR : Unsigned_8;
      Cap_Area_Offset : Unsigned_32;
      Cap_Area_Length : Unsigned_32;
      Cap_Found : Boolean := False;

      Mem_Addr : Integer_Address;
      Common_Config : Pci_Common_Config_Acc := null;
      Notification : access Unsigned_16 := null;

      End_Features : Boolean := False;
      Reset_Done : Boolean;

      Device_Idx : Natural := 1;
   begin
      Success := True;

      for Idx in 1 .. Devices.PCI.Enumerate_Devices (16#1AF4#, 16#1044#) loop
         Reset_Done := False;
         Devices.PCI.Search_Device (16#1AF4#, 16#1044#, Idx, PCI_Dev, Success);
         if not Success then
            Success := True;
            return;
         end if;

         Devices.PCI.Enable_Bus_Mastering (PCI_Dev);

         for CapIdx in 1 ..
            Devices.PCI.Enumerate_Capability (PCI_Dev, Cap_Vendor_Specific)
         loop
            Devices.PCI.Search_Capability (PCI_Dev, Cap_Vendor_Specific,
               CapIdx, Cap_Offset, Cap_Found);
            exit when Cap_Found = False;

            Devices.PCI.Read8
               (PCI_Dev, Unsigned_16 (Cap_Offset + 3), Cap_Type);
            Devices.PCI.Read8 (PCI_Dev, Unsigned_16 (Cap_Offset + 4), Cap_BAR);
            Devices.PCI.Read32
               (PCI_Dev, Unsigned_16 (Cap_Offset + 8), Cap_Area_Offset);
            Devices.PCI.Read32
               (PCI_Dev, Unsigned_16 (Cap_Offset + 12), Cap_Area_Length);

            if Unsigned_8'Pos (Cap_BAR) in BAR_Index'First .. BAR_Index'Last
            then
               if Cap_Type = 1 then
                  Devices.PCI.Virtio.Map_Configuration_Area
                     (PCI_Dev, BAR_Index (Cap_BAR), Cap_Area_Offset,
                      Cap_Area_Length, Mem_Addr, Success);

                  if Success then
                     Common_Config := Pci_Common_Config_Acc (C1.To_Pointer
                        (To_Address (Mem_Addr)));
                     Devices.PCI.Virtio.Reset_Device
                        (Common_Config, Reset_Done);
                  end if;
               elsif Cap_Type = 2 then
                  Devices.PCI.Virtio.Map_Configuration_Area
                     (PCI_Dev, BAR_Index (Cap_BAR),
                      A.Align_Down (Cap_Area_Offset, Memory.MMU.Page_Size),
                      16#1000#, Mem_Addr, Success);

                  if Success then
                     Notification := C3.To_Pointer (To_Address (Mem_Addr +
                        Memory.Virtual_Address
                           (Cap_Area_Offset mod Memory.MMU.Page_Size)));
                  end if;
               end if;
            end if;
         end loop;

         if not Reset_Done then
            Success := False;
            Messages.Put_Line ("virtio-rng did not finish its reset");
            return;
         end if;

         Common_Config.Device_Status := 1;
         Common_Config.Device_Status := 3;

         Devices.PCI.Virtio.Check_Device_Feature
            (Common_Config, 32, End_Features);
         if End_Features = False then
            Success := False;
            Messages.Put_Line ("Unexpected failure to set features");
            return;
         end if;

         Devices.PCI.Virtio.Ack_Device_Feature (Common_Config, 32);
         Common_Config.Device_Status := 11;

         declare
            Base_Name : constant String := "virtio-rng";
            Final_Name : constant String := Base_Name & Device_Idx'Image;

            Queue : constant Devices.PCI.Virtio.Virtio_Queue_Acc :=
               Devices.PCI.Virtio.Setup_Queue
                  (Common_Config, 0, Notification);

            Data_Addr : System.Address;
         begin
            Data_Addr := C2.To_Address (new Rng_Data'(
               Queue => Queue,
               Mutex => Synchronization.Unlocked_Mutex));

            Common_Config.Device_Status := 15;

            Register (
               (Data        => Data_Addr,
                Is_Block    => False,
                Block_Size  => 4096,
                Block_Count => 0,
                Read        => Read'Access,
                Write       => Write'Access,
                Sync        => null,
                Sync_Range  => null,
                IO_Control  => null,
                IO_Argument => null,
                Mmap        => null,
                Poll        => null,
                Remove      => null), Final_Name, Success);
         end;

         Device_Idx := Device_Idx + 1;
      end loop;
   exception
      when Constraint_Error =>
         Success := False;
   end Init;

   function Issue_Command
      (Device : Rng_Data_Acc;
       Data_Addr : Unsigned_64;
       Data_Length : Unsigned_32) return Natural
   is
      Queue_Highest_Index : constant Natural :=
         Natural (Device.Queue.Queue_Size - 1);

      Desc_Array : Virtqueue_Descriptor_Entries
         (0 .. Queue_Highest_Index)
            with Import, Address => To_Address (Device.Queue.Descriptor_Addr);
      Avail : Virtqueue_Available
         with Import, Address => To_Address (Device.Queue.Available_Addr);
      Avail_Entries_Array : Virtqueue_Available_Entries
         (0 .. Queue_Highest_Index)
            with Import, Address => To_Address
               (Device.Queue.Available_Addr + 4);

      Used : Virtqueue_Used
         with Import, Address => To_Address (Device.Queue.Used_Addr);
      Used_Entries_Array : Virtqueue_Used_Entries
         (0 .. Queue_Highest_Index)
         with Import, Address => To_Address
            (Device.Queue.Used_Addr + 4);

      Slot : Unsigned_16;
      Slot_Idx : Natural;
      Used_Index : Natural;

      Written : Unsigned_32 := 0;
   begin
      Synchronization.Seize (Device.Mutex);

      Slot := Avail.Index;
      Slot_Idx := Natural (Slot mod Device.Queue.Queue_Size);
      Used_Index := Natural
         (Device.Queue.Used_Head mod Device.Queue.Queue_Size);

      Desc_Array (0) :=
         (Has_Next => False,
          Address => Data_Addr,
          Length => Data_Length,
          Flag_Write => True,
          others => <>);

      Avail_Entries_Array (Slot_Idx) := Unsigned_16 (0);
      Avail := (Flags => 1, Index => (Slot + 1));

      Device.Queue.Notification.all := Device.Queue.Notify_Index;

      while Used.HeadIndex = Device.Queue.Used_Head loop
         null;
      end loop;

      declare
         Ring_Idx : constant Unsigned_32 := Used_Entries_Array (Used_Index).Id;
      begin
         if Ring_Idx /= 0
         then
            goto Failure_Cleanup;
         end if;

         Written := Used_Entries_Array (Used_Index).Length;
      end;

      Device.Queue.Used_Head := Device.Queue.Used_Head + 1;

      --  Unlock for other commands.
      Synchronization.Release (Device.Mutex);

      return Natural (Written);

   <<Failure_Cleanup>>
      Synchronization.Release (Device.Mutex);
      return 0;
   exception
      when Constraint_Error =>
         return 0;
   end Issue_Command;

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset, Is_Blocking);

      Buffer : Operation_Data_Acc := null;
      procedure Free is
         new Ada.Unchecked_Deallocation (Operation_Data, Operation_Data_Acc);
   begin
      Buffer := new Operation_Data (1 .. Data'Length);

      Ret_Count := Issue_Command
         (Device => Rng_Data_Acc (C2.To_Pointer (Key)),
          Data_Addr => Unsigned_64
            (To_Integer (Buffer.all'Address) - Memory.Memory_Offset),
          Data_Length => Unsigned_32 (Data'Length));

      if Ret_Count = 0 then
         Free (Buffer);
         Success := Dev_IO_Failure;
         return;
      end if;

      Data (1 .. Ret_Count) := Buffer (1 .. Ret_Count);
      Free (Buffer);
      Success := Dev_Success;
   exception
      when Constraint_Error =>
         Success := Dev_IO_Failure;
   end Read;

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset, Data, Ret_Count, Is_Blocking);
   begin
      Success := Dev_Not_Supported;
   end Write;
end Devices.PCI.VirtioRNG;
