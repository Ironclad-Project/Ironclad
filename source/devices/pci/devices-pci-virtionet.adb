--  devices-pci-virtionet.adb: VirtIO network devices.
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
with Networking.Interfaces;
with Scheduler;
with System.Address_To_Access_Conversions;

package body Devices.PCI.VirtioNet with SPARK_Mode => Off is
   package A is new Alignment (Unsigned_32);

   package C1 is new System.Address_To_Access_Conversions (Pci_Common_Config);
   package C2 is new System.Address_To_Access_Conversions (Net_Data);
   package C3 is new System.Address_To_Access_Conversions (Unsigned_16);
   package C4 is new System.Address_To_Access_Conversions (Net_Config);

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
      Notification_Addr : Integer_Address;
      Net_Config : Net_Config_Acc := null;

      Notify_Off_Multiplier : Unsigned_32 := 0;
      Feature : Boolean := False;
      Reset_Done : Boolean;

      Device_Idx : Natural := 0;
   begin
      Success := True;

      for Idx in 1 .. Devices.PCI.Enumerate_Devices (16#1AF4#, 16#1041#) loop
         Reset_Done := False;
         Devices.PCI.Search_Device (16#1AF4#, 16#1041#, Idx, PCI_Dev, Success);
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
                  Devices.PCI.Read32 (PCI_Dev, Unsigned_16 (Cap_Offset + 16),
                     Notify_Off_Multiplier);

                  Devices.PCI.Virtio.Map_Configuration_Area
                     (PCI_Dev, BAR_Index (Cap_BAR),
                      A.Align_Down (Cap_Area_Offset, Memory.MMU.Page_Size),
                      16#1000#, Mem_Addr, Success);

                  if Success then
                     Notification_Addr := Mem_Addr + Memory.Virtual_Address
                        (Cap_Area_Offset mod Memory.MMU.Page_Size);
                  end if;
               elsif Cap_Type = 4 then
                  Devices.PCI.Virtio.Map_Configuration_Area
                     (PCI_Dev, BAR_Index (Cap_BAR), Cap_Area_Offset,
                      Cap_Area_Length, Mem_Addr, Success);

                  if Success then
                     Net_Config := Net_Config_Acc (C4.To_Pointer
                        (To_Address (Mem_Addr)));
                  end if;
               end if;
            end if;
         end loop;

         if not Reset_Done then
            Success := False;
            Messages.Put_Line ("virtio-net did not finish its reset");
            return;
         end if;

         Common_Config.Device_Status := 1;
         Common_Config.Device_Status := 3;

         Devices.PCI.Virtio.Check_Device_Feature
            (Common_Config, 5, Feature);
         if Feature = False then
            Success := False;
            Messages.Put_Line ("virtio-net does not support MAC reporting");
            return;
         end if;

         Devices.PCI.Virtio.Check_Device_Feature
            (Common_Config, 32, Feature);
         if Feature = False then
            Success := False;
            Messages.Put_Line ("Unexpected failure to set features");
            return;
         end if;

         Devices.PCI.Virtio.Ack_Device_Feature (Common_Config, 32);
         Common_Config.Device_Status := 11;

         declare
            Base_Name : constant String := "virtio-net";
            Final_Name : constant String := Base_Name & Device_Idx'Image;

            Recv_Queue : constant Devices.PCI.Virtio.Virtio_Queue_Acc :=
               Devices.PCI.Virtio.Setup_Queue
                  (Common_Config, 0, C3.To_Pointer (To_Address (
                     Notification_Addr)));
            Send_Queue : constant Devices.PCI.Virtio.Virtio_Queue_Acc :=
               Devices.PCI.Virtio.Setup_Queue
                  (Common_Config, 1, C3.To_Pointer (To_Address (
                     Notification_Addr +
                     Integer_Address (Notify_Off_Multiplier))));

            Data_Addr : System.Address;

            Dev    : Device_Handle;
         begin
            Data_Addr := C2.To_Address (new Net_Data'(
               Recv_Queue => Recv_Queue,
               Send_Queue => Send_Queue,
               Mutex => Synchronization.Unlocked_Mutex,
               Common => Common_Config,
               Is_Retired => False));

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
                Mmap        => null,
                Poll        => null,
                Remove      => null), Final_Name, Success);

            if Success then
               Dev := Fetch (Final_Name);
               Networking.Interfaces.Register_Interface
                  (Interfaced  => Dev,
                   MAC         => Net_Config.Mac,
                   IPv4        => [10, 0, 2, 15],
                   IPv4_Subnet => [255, 0, 0, 0],
                   Success     => Success);
               Networking.Interfaces.Block (Dev, False, Success);
            end if;
         end;

         Device_Idx := Device_Idx + 1;
      end loop;
   exception
      when Constraint_Error =>
         Success := False;
   end Init;

   --  A command's header is written through by the device like its buffer,
   --  so it lives where the buffer does: on the heap, freed once the device
   --  is done with it, which for a command given up on is once the device has
   --  been reset.
   type Header_Acc is access Packet_Header;
   procedure Free_Header is new Ada.Unchecked_Deallocation
      (Packet_Header, Header_Acc);

   procedure Issue_Command
      (Device : Net_Data_Acc;
       Queue : Devices.PCI.Virtio.Virtio_Queue_Acc;
       Data_Addr : Unsigned_64;
       Data_Length : Unsigned_32;
       Send : Boolean;
       Ret_Count : out Natural;
       Success : out Boolean;
       Kept : out Boolean)
   is
      Queue_Highest_Index : constant Natural :=
         Natural (Queue.Queue_Size - 1);

      Desc_Array : Virtqueue_Descriptor_Entries
         (0 .. Queue_Highest_Index)
            with Import, Address => To_Address (Queue.Descriptor_Addr);
      Avail : Virtqueue_Available
         with Import, Address => To_Address (Queue.Available_Addr);
      Avail_Entries_Array : Virtqueue_Available_Entries
         (0 .. Queue_Highest_Index)
            with Import, Address => To_Address
               (Queue.Available_Addr + 4);

      Used : Virtqueue_Used
         with Import, Address => To_Address (Queue.Used_Addr);
      Used_Entries_Array : Virtqueue_Used_Entries
         (0 .. Queue_Highest_Index)
         with Import, Address => To_Address
            (Queue.Used_Addr + 4);

      Req_Header : Header_Acc := new Packet_Header'
         (Flags => 0,
          Gso_Type => 0,
          Hdr_Len => 0,
          Gso_Size => 0,
          Csum_Start => 0,
          Csum_Offset => 0,
          Num_Buffers => 0);

      Slot : Unsigned_16;
      Slot_Idx : Natural;
      Used_Index : Natural;

      Written : Unsigned_32 := 0;
      Reset_Done : Boolean;
   begin
      Kept := False;
      Synchronization.Seize (Device.Mutex);

      if Device.Is_Retired then
         Synchronization.Release (Device.Mutex);
         Free_Header (Req_Header);
         Success := False;
         Ret_Count := 0;
         return;
      end if;

      Slot := Avail.Index;
      Slot_Idx := Natural (Slot mod Queue.Queue_Size);
      Used_Index := Natural
         (Queue.Used_Head mod Queue.Queue_Size);

      Desc_Array (0) :=
         (Has_Next => True,
          Address => Unsigned_64
            (To_Integer (Req_Header.all'Address) - Memory.Memory_Offset),
          Length => 10,
          Flag_Write => (if Send then False else True),
          Next => 1,
          others => <>);

      Desc_Array (1) :=
         (Has_Next => False,
          Address => Data_Addr,
          Length => Data_Length,
          Flag_Write => (if Send then False else True),
          others => <>);

      Avail_Entries_Array (Slot_Idx) := Unsigned_16 (0);
      Avail := (Flags => 1, Index => (Slot + 1));

      Queue.Notification.all := Queue.Notify_Index;

      --  The wait yields and gives up when the thread is killed, since a
      --  device that never completes a command would otherwise hold its
      --  thread for good. What is given up is still named by the descriptors,
      --  and the device may write through them at any time until it is reset:
      --  a reset takes its queues out of its hands (virtio 1.3, 2.4.1), after
      --  which the driver may take back what it exposed (3.3.1). So the device
      --  is reset and retired, and the buffer and header are freed, unless it
      --  never finishes resetting, and then both are left to it.
      while Used.HeadIndex = Queue.Used_Head loop
         if Scheduler.Is_Doomed then
            Device.Is_Retired := True;
            Devices.PCI.Virtio.Reset_Device (Device.Common, Reset_Done);
            Kept := not Reset_Done;
            Synchronization.Release (Device.Mutex);
            if not Kept then
               Free_Header (Req_Header);
            end if;
            Messages.Put_Line
               ("virtio-net: a command was given up on, retiring it");
            Success := False;
            Ret_Count := 0;
            return;
         end if;
         Scheduler.Yield_If_Able;
      end loop;

      declare
         Ring_Idx : constant Unsigned_32 := Used_Entries_Array (Used_Index).Id;
      begin
         if Ring_Idx /= 0
         then
            goto Failure_Cleanup;
         end if;
         Written := Used_Entries_Array (Used_Index).Length;
         if Send = False and Written > 10 then
            Written := Written - 10;
         end if;
      end;

      Queue.Used_Head := Queue.Used_Head + 1;

      --  Unlock for other commands.
      Synchronization.Release (Device.Mutex);
      Free_Header (Req_Header);

      Success := True;
      if Send then
         Ret_Count := Natural (Data_Length);
      else
         Ret_Count := Natural (Written);
      end if;
      return;

      --  A completion that names another command leaves this call's own in
      --  the device's hands, and what it names with it.
   <<Failure_Cleanup>>
      Synchronization.Release (Device.Mutex);
      Kept := True;
      Success := False;
      Ret_Count := 0;
      return;
   exception
      when Constraint_Error =>
         --  Whether the command was handed over is not known here, so what
         --  it named is left to the device.
         Kept := True;
         Success := False;
         Ret_Count := 0;
         return;
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

      Command_Success : Boolean := False;
      Was_Kept : Boolean := False;
      Buffer : Operation_Data_Acc := null;
      procedure Free is
         new Ada.Unchecked_Deallocation (Operation_Data, Operation_Data_Acc);
   begin
      Buffer := new Operation_Data (1 .. Data'Length);

      Issue_Command
         (Device      => Net_Data_Acc (C2.To_Pointer (Key)),
          Queue       => Net_Data_Acc (C2.To_Pointer (Key)).Recv_Queue,
          Data_Addr   => Unsigned_64
            (To_Integer (Buffer.all'Address) - Memory.Memory_Offset),
          Data_Length => Unsigned_32 (Data'Length),
          Send        => False,
          Ret_Count   => Ret_Count,
          Success     => Command_Success,
          Kept        => Was_Kept);

      if Command_Success then
         Success := Dev_Success;
         Data (1 .. Ret_Count) := Buffer (1 .. Ret_Count);
      else
         Success := Dev_IO_Failure;
      end if;

      --  Unless the device may still write into it.
      if not Was_Kept then
         Free (Buffer);
      end if;
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
      pragma Unreferenced (Offset, Is_Blocking);

      Command_Success : Boolean := False;
      Was_Kept : Boolean := False;
      Buffer : Operation_Data_Acc := null;
      procedure Free is
         new Ada.Unchecked_Deallocation (Operation_Data, Operation_Data_Acc);
   begin
      Buffer := new Operation_Data (1 .. Data'Length);
      Buffer.all := Data;

      Issue_Command
         (Device      => Net_Data_Acc (C2.To_Pointer (Key)),
          Queue       => Net_Data_Acc (C2.To_Pointer (Key)).Send_Queue,
          Data_Addr   => Unsigned_64
            (To_Integer (Buffer.all'Address) - Memory.Memory_Offset),
          Data_Length => Unsigned_32 (Data'Length),
          Send        => True,
          Ret_Count   => Ret_Count,
          Success     => Command_Success,
          Kept        => Was_Kept);

      if not Was_Kept then
         Free (Buffer);
      end if;
      if Command_Success then
         Success := Dev_Success;
      else
         Success := Dev_IO_Failure;
      end if;
   exception
      when Constraint_Error =>
         Success := Dev_IO_Failure;
   end Write;
end Devices.PCI.VirtioNet;
