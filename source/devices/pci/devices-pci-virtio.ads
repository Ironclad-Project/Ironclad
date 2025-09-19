package Devices.PCI.Virtio with SPARK_Mode => Off is
   type Unsigned_14 is mod 2 ** 14;

   type Virtqueue_Index is mod 2**16;
   type Virtqueue_Index_Acc is access all Virtqueue_Index;

   type Pci_Common_Config is record
      Device_Feature_Select : Unsigned_32;
      Device_Feature : Unsigned_32;
      Driver_Feature_Select : Unsigned_32;
      Driver_Feature : Unsigned_32;
      Config_MsiX_Vector : Unsigned_16;
      Num_Queues : Unsigned_16;
      Device_Status : Unsigned_8;
      Config_Generation : Unsigned_8;
      Queue_Select : Unsigned_16;
      Queue_Size : Unsigned_16;
      Queue_MSIX_Vector : Unsigned_16;
      Queue_Enable : Unsigned_16;
      Queue_Notify_Off : Unsigned_16;
      Queue_Desc : Unsigned_64;
      Queue_Driver : Unsigned_64;
      Queue_Device : Unsigned_64;
      Queue_Notif_Config_Data : Unsigned_16;
      Queue_Reset : Unsigned_16;
   end record with Size => 60 * 8, Volatile;
   pragma Pack (Pci_Common_Config);

   type Pci_Common_Config_Acc is access all Pci_Common_Config;

   type Virtqueue_Descriptor (Has_Next : Boolean := False) is record
      Address : Unsigned_64;
      Length : Unsigned_32;
      Flag_Write : Boolean := False;
      Flags : Unsigned_14 := 0;
      case Has_Next is
         when True =>
            Next : Unsigned_16 := 0;
         when False =>
            null;
      end case;
   end record with Size => 16 * 8, Volatile, Alignment => 16;
   for Virtqueue_Descriptor use record
      Address at 0 range 0 .. 63;
      Length at 8 range 0 .. 31;
      Has_Next at 12 range 0 .. 0;
      Flag_Write at 12 range 1 .. 1;
      Flags at 12 range 2 .. 15;
      Next at 14 range 0 .. 15;
   end record;
   type Virtqueue_Descriptor_Entries is array (Natural range <>)
      of aliased Virtqueue_Descriptor with Pack;

   type Virtqueue_Available is record
      Flags : Unsigned_16;
      Index : Unsigned_16;
   end record with Size => 4 * 8, Volatile, Alignment => 2;

   type Virtqueue_Available_Entries is array (Natural range <>)
      of aliased Unsigned_16 with Volatile, Pack;

   type Virtqueue_Used is record
      Flags : Unsigned_16;
      HeadIndex : Unsigned_16;
   end record with Size => 4 * 8, Volatile, Alignment => 4;

   type Virtqueue_Used_Element is record
      Id : Unsigned_32;
      Length : Unsigned_32;
   end record with Size => 8 * 8, Volatile;

   type Virtqueue_Used_Entries is array (Natural range <>)
      of aliased Virtqueue_Used_Element with Volatile, Pack;

   type Virtio_Queue is record
      Queue_Index : Virtqueue_Index;
      Queue_Size : Unsigned_16;
      Notify_Index : Unsigned_16;
      Descriptor_Addr : Memory.Virtual_Address;
      Available_Addr : Memory.Virtual_Address;
      Used_Addr : Memory.Virtual_Address;
      Notification : access Unsigned_16;
      Used_Head : Unsigned_16 := 0;
   end record;
   type Virtio_Queue_Acc is not null access all Virtio_Queue;

   procedure Map_Configuration_Area
      (PCI_Dev : Devices.PCI.PCI_Device;
       BAR : BAR_Index;
       Offset : Unsigned_32;
       Length : Unsigned_32;
       Addr : out Integer_Address;
       Success : out Boolean);

   procedure Check_Device_Feature
      (Dev : Pci_Common_Config_Acc;
       Feature : Unsigned_32;
       Supported : out Boolean);

   procedure Ack_Device_Feature
      (Dev : Pci_Common_Config_Acc;
       Feature : Unsigned_32);

   function Setup_Queue
      (Dev : Pci_Common_Config_Acc;
       Queue_Index : Virtqueue_Index;
       Notification : access Unsigned_16) return Virtio_Queue_Acc;
end Devices.PCI.Virtio;
