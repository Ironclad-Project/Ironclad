with Alignment;
with Devices.PCI.Virtio; use Devices.PCI.Virtio;
with Devices.Partitions;
with Messages;
with System.Address_To_Access_Conversions;

package body Devices.PCI.VirtioBlk with SPARK_Mode => Off is
   package A is new Alignment (Unsigned_32);

   package C1 is new System.Address_To_Access_Conversions (Pci_Common_Config);
   package C2 is new System.Address_To_Access_Conversions (Blk_Config);
   package C3 is new System.Address_To_Access_Conversions (Blk_Data);
   package C4 is new System.Address_To_Access_Conversions (Unsigned_16);

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

      Blk_Config : Blk_Config_Acc := null;
      Notify_Off_Multiplier : Unsigned_32 := 0;

      End_Features : Boolean := False;

      Drive_Idx : Natural := 1;
   begin
      Success := True;

      for Idx in 1 .. Devices.PCI.Enumerate_Devices (16#1AF4#, 16#1042#) loop
         Devices.PCI.Search_Device (16#1AF4#, 16#1042#, Idx, PCI_Dev, Success);
         if not Success then
            Success := True;
            return;
         end if;

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
                     Notification := C4.To_Pointer (To_Address (Mem_Addr +
                        Memory.Virtual_Address
                           (Cap_Area_Offset mod Memory.MMU.Page_Size)));
                  end if;
               elsif Cap_Type = 4 then
                  Devices.PCI.Virtio.Map_Configuration_Area
                     (PCI_Dev, BAR_Index (Cap_BAR), Cap_Area_Offset,
                      Cap_Area_Length, Mem_Addr, Success);

                  if Success then
                     Blk_Config := Blk_Config_Acc (C2.To_Pointer
                        (To_Address (Mem_Addr)));
                  end if;
               end if;
            end if;
         end loop;

         Devices.PCI.Enable_Bus_Mastering (PCI_Dev);

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
            Base_Name : constant String := "virtio-blk";
            Final_Name : constant String := Base_Name & Drive_Idx'Image;

            Queue : constant Devices.PCI.Virtio.Virtio_Queue_Acc :=
               Devices.PCI.Virtio.Setup_Queue
                  (Common_Config, 0, Notification);

            NS_Addr : System.Address;
         begin
            NS_Addr := C3.To_Address (new Blk_Data'(
               LBA_Count => Blk_Config.Capacity,
               Queue => Queue,
               Mutex => Synchronization.Unlocked_Mutex,
               Cache_Reg => <>));

            Common_Config.Device_Status := 15;

            Caching.Init
               (NS_Addr,
                Read_Sector'Access,
                Write_Sector'Access,
                Blk_Data_Acc (C3.To_Pointer (NS_Addr)).Cache_Reg);

            Register (
               (Data        => NS_Addr,
                Is_Block    => True,
                Block_Size  => 512,
                Block_Count => Blk_Config.Capacity,
                Read        => Read'Access,
                Write       => Write'Access,
                Sync        => Sync'Access,
                Sync_Range  => Sync_Range'Access,
                IO_Control  => null,
                Mmap        => null,
                Poll        => null,
                Remove      => null), Final_Name, Success);

            if Success then
               Partitions.Parse_Partitions
                  (Final_Name, Fetch (Final_Name), Success);
            end if;
         end;

         Drive_Idx := Drive_Idx + 1;
      end loop;
   exception
      when Constraint_Error =>
         Success := False;
   end Init;

   function Issue_Command
      (Drive       : Blk_Data_Acc;
       LBA         : Unsigned_64;
       Is_Write    : Boolean;
       Data_Addr   : Unsigned_64) return Boolean
   is
      Queue_Highest_Index : constant Natural :=
         Natural (Drive.Queue.Queue_Size - 1);

      Desc_Array : Virtqueue_Descriptor_Entries
         (0 .. Queue_Highest_Index)
            with Import, Address => To_Address (Drive.Queue.Descriptor_Addr);
      Avail : Virtqueue_Available
         with Import, Address => To_Address (Drive.Queue.Available_Addr);
      Avail_Entries_Array : Virtqueue_Available_Entries
         (0 .. Queue_Highest_Index)
            with Import, Address => To_Address
               (Drive.Queue.Available_Addr + 4);

      Used : Virtqueue_Used
         with Import, Address => To_Address (Drive.Queue.Used_Addr);
      Used_Entries_Array : Virtqueue_Used_Entries
         (0 .. Queue_Highest_Index)
         with Import, Address => To_Address
            (Drive.Queue.Used_Addr + 4);

      Slot : Unsigned_16;
      Slot_Idx : Natural;
      Used_Index : Natural;

      Req_Header : aliased Blk_Req_Header :=
         (Req_Type => (if Is_Write then Blk_Out else Blk_In),
          Sector => LBA,
          others => <>);
      Status : aliased Status_Byte := Unsupported;

      --  Written data (512 bytes, if any) + status byte.
      Expected_Written : constant Unsigned_32 :=
         (if Is_Write then 1 else 513);
   begin
      if LBA >= Drive.LBA_Count then
         return False;
      end if;

      Synchronization.Seize (Drive.Mutex);

      Slot := Avail.Index;
      Slot_Idx := Natural (Slot mod Drive.Queue.Queue_Size);
      Used_Index := Natural (Drive.Queue.Used_Head mod Drive.Queue.Queue_Size);

      Desc_Array (0) :=
         (Has_Next => True,
          Address => Unsigned_64
            (To_Integer (Req_Header'Address) - Memory.Memory_Offset),
          Length => Req_Header'Size / 8,
          Next => 1,
          others => <>);

      Desc_Array (1) :=
         (Has_Next => True,
          Address => Data_Addr,
          Length => 512,
         --  this flag controls whether the buffer is device write-only
          Flag_Write => not Is_Write,
          Next => 2,
          others => <>);

      Desc_Array (2) :=
         (Has_Next => False,
          Address => Unsigned_64
            (To_Integer (Status'Address) - Memory.Memory_Offset),
          Length => 1,
          Flag_Write => True,
          others => <>);

      Avail_Entries_Array (Slot_Idx) := Unsigned_16 (0);
      Avail := (Flags => 1, Index => (Slot + 1));

      Drive.Queue.Notification.all := Drive.Queue.Notify_Index;

      while Used.HeadIndex = Drive.Queue.Used_Head loop
         null;
      end loop;

      declare
         Ring_Idx : constant Unsigned_32 := Used_Entries_Array (Used_Index).Id;
         Written : constant Unsigned_32 :=
            Used_Entries_Array (Used_Index).Length;
      begin
         if Ring_Idx /= 0 or Written /= Expected_Written or Status /= Ok
         then
            goto Failure_Cleanup;
         end if;
      end;

      Drive.Queue.Used_Head := Drive.Queue.Used_Head + 1;

      --  Unlock for other commands.
      Synchronization.Release (Drive.Mutex);

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
      SAddr : constant  System.Address := Data_Buffer'Address;
      IAddr : constant Integer_Address := To_Integer (SAddr);
   begin
      Success := Issue_Command
         (Drive       => Blk_Data_Acc (C3.To_Pointer (D)),
          LBA         => LBA,
          Is_Write    => False,
          Data_Addr   => Unsigned_64 (IAddr - Memory.Memory_Offset));
   exception
      when Constraint_Error =>
         Success := False;
   end Read_Sector;

   procedure Write_Sector
      (D           : System.Address;
       LBA         : Unsigned_64;
       Data_Buffer : Caching.Sector_Data;
       Success     : out Boolean)
   is
      SAddr : constant  System.Address := Data_Buffer'Address;
      IAddr : constant Integer_Address := To_Integer (SAddr);
   begin
      Success := Issue_Command
         (Drive       => Blk_Data_Acc (C3.To_Pointer (D)),
          LBA         => LBA,
          Is_Write    => True,
          Data_Addr   => Unsigned_64 (IAddr - Memory.Memory_Offset));
   exception
      when Constraint_Error =>
         Success := False;
   end Write_Sector;

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Is_Blocking);
   begin
      Caching.Read
         (Registry  => Blk_Data_Acc (C3.To_Pointer (Key)).Cache_Reg,
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
   begin
      Caching.Write
         (Registry  => Blk_Data_Acc (C3.To_Pointer (Key)).Cache_Reg,
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
   begin
      Caching.Sync (Blk_Data_Acc (C3.To_Pointer (Key)).Cache_Reg, Success);
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
   begin
      Caching.Sync_Range
         (Blk_Data_Acc (C3.To_Pointer (Key)).Cache_Reg, Offset, Count,
            Success);
   exception
      when Constraint_Error =>
         Success := False;
   end Sync_Range;
end Devices.PCI.VirtioBlk;
