with Ada.Unchecked_Deallocation;
with Alignment;
with Devices.PCI.Virtio; use Devices.PCI.Virtio;
with Messages;
with Networking.Interfaces;
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

      Device_Idx : Natural := 0;
   begin
      Success := True;

      for Idx in 1 .. Devices.PCI.Enumerate_Devices (16#1AF4#, 16#1041#) loop
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
                     Common_Config.Device_Status := 0;
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
                   IPv6        => [1 .. 8 => 0, 9 .. 12 => 16#FF#,
                                   13 => 10, 14 => 0,  15 => 2, 16 => 15],
                   IPv6_Subnet => [16 => 0, 1 .. 8 => 0, others => 16#FF#],
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

   procedure Issue_Command
      (Device : Net_Data_Acc;
       Queue : Devices.PCI.Virtio.Virtio_Queue_Acc;
       Data_Addr : Unsigned_64;
       Data_Length : Unsigned_32;
       Send : Boolean;
       Ret_Count : out Natural;
       Success : out Boolean)
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

      Req_Header : aliased Packet_Header :=
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
   begin
      Synchronization.Seize (Device.Mutex);

      Slot := Avail.Index;
      Slot_Idx := Natural (Slot mod Queue.Queue_Size);
      Used_Index := Natural
         (Queue.Used_Head mod Queue.Queue_Size);

      Desc_Array (0) :=
         (Has_Next => True,
          Address => Unsigned_64
            (To_Integer (Req_Header'Address) - Memory.Memory_Offset),
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

      while Used.HeadIndex = Queue.Used_Head loop
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
         if Send = False and Written > 10 then
            Written := Written - 10;
         end if;
      end;

      Queue.Used_Head := Queue.Used_Head + 1;

      --  Unlock for other commands.
      Synchronization.Release (Device.Mutex);

      Success := True;
      if Send then
         Ret_Count := Natural (Data_Length);
      else
         Ret_Count := Natural (Written);
      end if;
      return;

   <<Failure_Cleanup>>
      Synchronization.Release (Device.Mutex);
      Success := False;
      Ret_Count := 0;
      return;
   exception
      when Constraint_Error =>
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
          Success     => Command_Success);

      if Command_Success then
         Success := Dev_Success;
         Data (1 .. Ret_Count) := Buffer (1 .. Ret_Count);
      else
         Success := Dev_IO_Failure;
      end if;
      Free (Buffer);
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
          Success     => Command_Success);

      Free (Buffer);
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
