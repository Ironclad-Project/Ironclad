with Devices.PCI.Virtio;
with Synchronization;

package Devices.PCI.VirtioRNG with SPARK_Mode => Off is
   pragma Warnings (Off, "may call Last_Chance_Handler");
   type Rng_Data is record
      Queue : Devices.PCI.Virtio.Virtio_Queue_Acc;
      Mutex : aliased Synchronization.Mutex;
   end record;
   type Rng_Data_Acc is not null access all Rng_Data;
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

   function Issue_Command
      (Device : Rng_Data_Acc;
       Data_Addr : Unsigned_64;
       Data_Length : Unsigned_32) return Natural;
end Devices.PCI.VirtioRNG;
