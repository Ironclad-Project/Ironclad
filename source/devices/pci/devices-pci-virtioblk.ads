with Synchronization;
with Devices.Drive_Cache;
with Devices.PCI.Virtio;

package Devices.PCI.VirtioBlk with SPARK_Mode => Off is
   type Blk_Req_Type is (Blk_In, Blk_Out);
   for Blk_Req_Type use (
      Blk_In => 0,
      Blk_Out => 1
   );
   for Blk_Req_Type'Size use Unsigned_32'Size;

   type Blk_Req_Header is record
      Req_Type : Blk_Req_Type;
      Reserved : Unsigned_32 := 0;
      Sector : Unsigned_64;
   end record with Size => 16 * 8, Volatile, Alignment => 4;
   for Blk_Req_Header use record
      Req_Type at 0 range 0 .. 31;
      Reserved at 4 range 0 .. 31;
      Sector at 8 range 0 .. 63;
   end record;

   type Status_Byte is (Ok, IO_Err, Unsupported);
   for Status_Byte use (
      Ok => 0,
      IO_Err => 1,
      Unsupported => 2
   );
   for Status_Byte'Size use Unsigned_8'Size;

   type Blk_Config is record
      Capacity : Unsigned_64;
   end record with Size => 8 * 8, Volatile;
   type Blk_Config_Acc is access all Blk_Config;

   package Caching is new Devices.Drive_Cache (Sector_Size => 512);

   type Blk_Data is record
      Queue : Devices.PCI.Virtio.Virtio_Queue_Acc;
      LBA_Count : Unsigned_64;
      Cache_Reg : aliased Caching.Cache_Registry;
      Mutex : aliased Synchronization.Mutex;
   end record;
   type Blk_Data_Acc is not null access all Blk_Data;

   procedure Init (Success : out Boolean);

   function Issue_Command
      (Drive       : Blk_Data_Acc;
       LBA         : Unsigned_64;
       Is_Write    : Boolean;
       Data_Addr   : Unsigned_64) return Boolean;

   procedure Read_Sector
      (D           : System.Address;
       LBA         : Unsigned_64;
       Data_Buffer : out Caching.Sector_Data;
       Success     : out Boolean);
   procedure Write_Sector
      (D           : System.Address;
       LBA         : Unsigned_64;
       Data_Buffer : Caching.Sector_Data;
       Success     : out Boolean);

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

   procedure Sync (Key : System.Address; Success : out Boolean);

   procedure Sync_Range
      (Key     : System.Address;
       Offset  : Unsigned_64;
       Count   : Unsigned_64;
       Success : out Boolean);
end Devices.PCI.VirtioBlk;
