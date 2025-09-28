with Devices.PCI.Virtio;
with Synchronization;
with Networking;

package Devices.PCI.VirtioNet with SPARK_Mode => Off is
   type Net_Config is record
      Mac : Networking.MAC_Address;
      Status : Unsigned_16;
      Max_Virtqueue_Pairs : Unsigned_16;
      Mtu : Unsigned_16;
      Speed : Unsigned_32;
      Duplex : Unsigned_8;
      Rss_Max_Key_Size : Unsigned_8;
      Rss_Max_Indirection_Table_Length : Unsigned_16;
      Supported_Hash_Types : Unsigned_32;
   end record with Pack;
   type Net_Config_Acc is access all Net_Config;

   type Packet_Header is record
      Flags : Unsigned_8;
      Gso_Type : Unsigned_8;
      Hdr_Len : Unsigned_16;
      Gso_Size : Unsigned_16;
      Csum_Start : Unsigned_16;
      Csum_Offset : Unsigned_16;
      Num_Buffers : Unsigned_16;
   end record;
   for Packet_Header use record
      Flags at 0 range 0 .. 7;
      Gso_Type at 1 range 0 .. 7;
      Hdr_Len at 2 range 0 .. 15;
      Gso_Size at 4 range 0 .. 15;
      Csum_Start at 6 range 0 .. 15;
      Csum_Offset at 8 range 0 .. 15;
      Num_Buffers at 10 range 0 .. 15;
   end record;

   pragma Warnings (Off, "may call Last_Chance_Handler");
   type Net_Data is record
      Recv_Queue : Devices.PCI.Virtio.Virtio_Queue_Acc;
      Send_Queue : Devices.PCI.Virtio.Virtio_Queue_Acc;
      Mutex : aliased Synchronization.Mutex;
   end record;
   type Net_Data_Acc is not null access all Net_Data;
   pragma Warnings (On, "may call Last_Chance_Handler");

   procedure Init (Success : out Boolean);

   procedure Read
      (Key : System.Address;
       Offset : Unsigned_64;
       Data : out Operation_Data;
       Ret_Count : out Natural;
       Success : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Write
      (Key : System.Address;
       Offset : Unsigned_64;
       Data : Operation_Data;
       Ret_Count : out Natural;
       Success : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Issue_Command
      (Device : Net_Data_Acc;
       Queue : Devices.PCI.Virtio.Virtio_Queue_Acc;
       Data_Addr : Unsigned_64;
       Data_Length : Unsigned_32;
       Send : Boolean;
       Ret_Count : out Natural;
       Success : out Boolean);
end Devices.PCI.VirtioNet;
