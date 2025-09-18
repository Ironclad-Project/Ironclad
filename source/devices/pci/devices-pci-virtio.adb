with Alignment;
with Interfaces.C; use Interfaces.C;
with Memory.Physical;
with Panic;

package body Devices.PCI.Virtio with SPARK_Mode => Off is
   package A is new Alignment (Unsigned_64);

   procedure Map_Configuration_Area
      (PCI_Dev : Devices.PCI.PCI_Device;
       BAR : BAR_Index;
       Offset : Unsigned_32;
       Length : Unsigned_32;
       Addr : out Integer_Address;
       Success : out Boolean)
   is
      PCI_BAR : Devices.PCI.Base_Address_Register;
      Mem_Addr : Integer_Address;
   begin
      Devices.PCI.Get_BAR (PCI_Dev, BAR, PCI_BAR,
         Success);

      if not Success or else not PCI_BAR.Is_MMIO then
         Success := False;
         return;
      end if;

      Mem_Addr :=
         PCI_BAR.Base + Memory.Memory_Offset + Integer_Address (Offset);

      Memory.MMU.Map_Range
         (Map => Memory.MMU.Kernel_Table,
          Physical_Start => To_Address
            (PCI_BAR.Base + Integer_Address (Offset)),
          Virtual_Start => To_Address (Mem_Addr),
          Length => Storage_Count (A.Align_Up
            (Unsigned_64 (Length), Memory.MMU.Page_Size)),
          Permissions =>
            (Is_User_Accessible => False,
             Can_Read          => True,
             Can_Write         => True,
             Can_Execute       => False,
             Is_Global         => True),
          Success => Success,
          Caching => Arch.MMU.Uncacheable);
      if not Success then
         return;
      end if;

      Addr := Mem_Addr;
   exception
      when Constraint_Error =>
         Success := False;
   end Map_Configuration_Area;

   procedure Check_Device_Feature
      (Dev : Pci_Common_Config_Acc;
       Feature : Unsigned_32;
       Supported : out Boolean)
   is
   begin
      Dev.Device_Feature_Select := Shift_Right (Feature, 5);
      Supported := (Dev.Device_Feature and
         Shift_Left (1, Integer (Feature and 16#1F#))) /= 0;
   exception
      when Constraint_Error =>
         Supported := False;
   end Check_Device_Feature;

   procedure Ack_Device_Feature
      (Dev : Pci_Common_Config_Acc;
       Feature : Unsigned_32)
   is
      Old_Value : Unsigned_32;
   begin
      Dev.Device_Feature_Select := Shift_Right (Feature, 5);
      Old_Value := Dev.Device_Feature;
      Dev.Device_Feature :=
         Old_Value or Shift_Left (1, Integer (Feature and 16#1F#));
   exception
      when Constraint_Error =>
         null;
   end Ack_Device_Feature;

   function Setup_Queue
      (Dev : Pci_Common_Config_Acc;
       Queue_Index : Virtqueue_Index;
       Notification : access Unsigned_16) return Virtio_Queue_Acc
   is
      Queue_Size : Unsigned_16;
      Notify_Index : Unsigned_16;

      Available_Offset : Unsigned_64;
      Used_Offset : Unsigned_64;
      Queue_Area_Length : Unsigned_64;
      Queue_Addr : Memory.Virtual_Address;
   begin
      Dev.Queue_Select := Unsigned_16 (Queue_Index);
      Queue_Size := Dev.Queue_Size;
      Notify_Index := Dev.Queue_Notify_Off;

      Available_Offset := 16 * Unsigned_64 (Queue_Size);
      Used_Offset := A.Align_Up (Available_Offset + 6 +
            (2 * Unsigned_64 (Queue_Size)), Virtqueue_Used'Alignment);

      Queue_Area_Length := A.Align_Up (Used_Offset + 6 +
            (8 * Unsigned_64 (Queue_Size)), Memory.MMU.Page_Size);

      Memory.Physical.Alloc (size_t (Queue_Area_Length), Queue_Addr);

      Dev.Queue_Desc := Unsigned_64 (Queue_Addr) - Memory.Memory_Offset;
      Dev.Queue_Driver := Unsigned_64 (Queue_Addr) +
         Available_Offset - Memory.Memory_Offset;
      Dev.Queue_Device := Unsigned_64 (Queue_Addr) +
         Used_Offset - Memory.Memory_Offset;

      declare
         Avail : Virtqueue_Available
            with Import, Address => To_Address
               (Queue_Addr + Memory.Virtual_Address (Available_Offset));
         Avail_Entries_Array : Virtqueue_Available_Entries
            (0 .. Natural (Queue_Size - 1))
               with Import, Address => To_Address
                  (Queue_Addr + Memory.Virtual_Address (Available_Offset) + 4);

         Used : Virtqueue_Used
            with Import, Address => To_Address
               (Queue_Addr + Memory.Virtual_Address (Used_Offset));
         Used_Entries_Array : Virtqueue_Used_Entries
            (0 .. Natural (Queue_Size - 1))
               with Import, Address => To_Address
                  (Queue_Addr + Memory.Virtual_Address (Used_Offset) + 4);
      begin
         Avail := (Flags => 1, Index => 0);
         for I in Avail_Entries_Array'Range loop
            Avail_Entries_Array (I) := 16#FFFF#;
         end loop;

         Used := (Flags => 1, HeadIndex => 0);
         for I in Used_Entries_Array'Range loop
            Used_Entries_Array (I).Id := 16#FFFF#;
         end loop;
      end;

      Dev.Queue_Enable := 1;

      return new Virtio_Queue'(
         Queue_Index => Queue_Index,
         Queue_Size => Queue_Size,
         Notify_Index => Notify_Index,
         Descriptor_Addr => Queue_Addr,
         Available_Addr =>
            Queue_Addr + Memory.Virtual_Address (Available_Offset),
         Used_Addr => Queue_Addr + Memory.Virtual_Address (Used_Offset),
         Notification => Notification,
         others => <>
      );
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Failed to set up virtio queue");
   end Setup_Queue;
end Devices.PCI.Virtio;
