--  devices-pci-e1000.adb: Intel e1000 Gigabit Ethernet driver.
--  Copyright (C) 2025 Alexander Richards
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
with Ada.Unchecked_Conversion;
--  TODO: not x86
with Arch.APIC;
with Arch.CPU;
with Arch.IDT; use Arch.IDT;
with Arch.MMU;
with Messages;
with Memory.Physical;
with Alignment;
with Networking.Interfaces;
with Scheduler;

with Interfaces.C; use Interfaces.C;

package body Devices.PCI.E1000 with SPARK_Mode => Off is
   package A is new Alignment (size_t);
   package C1 is new System.Address_To_Access_Conversions
      (Controller_Data);
   package C_MMIO is new System.Address_To_Access_Conversions
      (MMIO_Registers);
   package C_RX_Desc is new System.Address_To_Access_Conversions
      (RX_Descriptor_Array);
   package C_TX_Desc is new System.Address_To_Access_Conversions
      (TX_Descriptor_Array);

   --  Conversion function from Integer_Address to System.Address
   function To_Addr is new Ada.Unchecked_Conversion
      (Integer_Address, System.Address);

   --  Array to track interrupt handlers for multiple e1000 devices
   type E1000_Interrupt is record
      Idx : Arch.IDT.IDT_Index;
      CD : Controller_Data_Acc;
   end record;
   type E1000_Interrupt_Arr is array (1 .. 10) of E1000_Interrupt;
   Interrupts : E1000_Interrupt_Arr :=
      [others => (1, null)];


   function Initialize_Descriptors
      (CD : Controller_Data_Acc) return Boolean
   is
   begin
      --  At this point the descriptor rings should already be allocated
      --  We just initialize the MMIO registers to point to them

      --  Set up the Receive Descriptor Ring
      --  RDBAL/RDBAH point to the descriptor ring base address
      --  RDLEN is the size in bytes
      --  RDH is head, RDT is tail
      if CD.MMIO_Regs /= null then
         CD.MMIO_Regs.RDLEN := RX_RING_SIZE * 16;
         CD.MMIO_Regs.RDH := 0;
         CD.MMIO_Regs.RDT := RX_RING_SIZE - 1;

         --  Set up the Transmit Descriptor Ring
         CD.MMIO_Regs.TDLEN := TX_RING_SIZE * 16;
         CD.MMIO_Regs.TDH := 0;
         CD.MMIO_Regs.TDT := 0;
      end if;

      return True;
   exception
      when Constraint_Error =>
         Messages.Put_Line
            ("Constraint_Error in E1000 Initialize_Descriptors");
         return False;
   end Initialize_Descriptors;


   function Enable_Receiver
      (CD : Controller_Data_Acc) return Boolean
   is
   begin
      if CD.MMIO_Regs /= null then
         --  Configure Receive Control Register
         --  RCTL bits:
         --    Bit 1 (0x2):       EN  - Receiver Enable
         --    Bit 2 (0x4):       SBP - Store Bad Packets
         --    Bit 3 (0x8):       UPE - Unicast Promiscuous Enable
         --    Bit 15 (0x8000):   BAM - Broadcast Accept Mode
         --    Bit 25 (0x2000000): BSIZE extension for 8K buffers
         --    Bit 26 (0x4000000): SECRC - Strip Ethernet CRC
         --  Buffer size: bits 16-17 = 0b10 (for 512 bytes) + bit 25 =
         --    8192 bytes
         --  RCTL = EN | SBP | UPE | BAM | SECRC | BSIZE_8192
         --       = 0x2 | 0x4 | 0x8 | 0x8000 | 0x4000000 | 0x20000
         --       = 0x402800E
         CD.MMIO_Regs.RCTL := 16#402800E#;
      end if;
      return True;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in E1000 Enable_Receiver");
         return False;
   end Enable_Receiver;


   function Enable_Transmitter
      (CD : Controller_Data_Acc) return Boolean
   is
   begin
      if CD.MMIO_Regs /= null then
         --  Configure Transmit Control Register
         --  TCTL: Enable (bit 1), Pad Short Packets (bit 3),
         --        Collision Threshold (bits 4-11),
         --        Collision Distance (bits 12-21)
         --  CT = 15, COLD = 64 (full duplex)
         CD.MMIO_Regs.TCTL := 16#010400FA#;
         --  Bits: EN (1) | PSP (8) | CT=15 (0xF << 4) | COLD=64 (0x40 << 12)

         --  TODO: Set TIPG (Transmit Inter-Packet Gap) properly
         --  This depends on the network speed, but we can use default values
         --  that should result in the minimum legal speed.
         --  These do depend on the exact model, but thats a problem for later.

         --  TIPG: Transmit Inter-Packet Gap
         --        Bits 0-9: IPGT (set to 10)
         --        Bits 10-19: IPGR1 (set to 10)
         --        Bits 20-29: IPGR2 (set to 10)
         CD.MMIO_Regs.TIPG := 16#A0280A#;

         --  (Section 13.4.34 and 14.5 in e1000 manual)
      end if;
      return True;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in E1000 Enable_Transmitter");
         return False;
   end Enable_Transmitter;


   procedure Generic_E1000_Interrupt_Handler
      (Num : Integer)
   is
   begin
      for Int of Interrupts loop
         if Int.CD /= null and Natural (Int.Idx - 1) = Num then
            Handle_Interrupt (Int.CD);
            return;
         end if;
      end loop;

      Messages.Put_Line ("Unexpected E1000 Interrupt!");
      Arch.APIC.LAPIC_EOI;
   end Generic_E1000_Interrupt_Handler;

   function Install_Interrupt_Handler (CD : Controller_Data_Acc) return Boolean
   is
      Index   : Arch.IDT.IRQ_Index;
      Success : Boolean;
      I       : Arch.IDT.IRQ_Index;
   begin
      Messages.Put_Line ("E1000 Interrupt at " & Unsigned_8'Image
       (CD.Interrupt));

      --  Allocate an interrupt in the IDT and unmask it
      I := Arch.IDT.IRQ_Index (CD.Interrupt + 33);
      Arch.IDT.Load_ISR
         (Address => Generic_E1000_Interrupt_Handler'Address,
          Index   => Index,
          Success => Success);
      if not Success then
         return False;
      end if;

      Arch.APIC.IOAPIC_Set_Redirect
         (LAPIC_ID => Arch.CPU.Core_Locals (1).LAPIC_ID,
          IRQ => I,
          IDT_Entry => Index,
          Enable => True,
          Success => Success);
      if not Success then
         return False;
      end if;

      CD.Interrupt_IDT_Index := Unsigned_16 (Index);

      for Int of Interrupts loop
         if Int.CD = null then
            Int := (Index, CD);
            exit;
         end if;
      end loop;

      return True;
   exception
      when Constraint_Error =>
         return False;
   end Install_Interrupt_Handler;

   procedure Handle_Interrupt (CD : Controller_Data_Acc)
   is
      ICR : Unsigned_32;
   begin
      if CD.MMIO_Regs /= null then
         --  Read interrupt cause (reading ICR acknowledges the interrupt)
         ICR := CD.MMIO_Regs.ICR;

         --  Check interrupt causes
         --  Bit 0 (0x01): Transmit Descriptor Written Back
         --  Bit 1 (0x02): Transmit Queue Empty
         --  Bit 2 (0x04): Link Status Change
         --  Bit 4 (0x10): Receive Descriptor Minimum Threshold
         --  Bit 6 (0x40): Receiver FIFO Overrun
         --  Bit 7 (0x80): Receive Timer Interrupt

         if (ICR and 16#04#) /= 0 then
            Messages.Put_Line ("E1000: Link status changed");
         end if;

         if (ICR and 16#80#) /= 0 or (ICR and 16#10#) /= 0 then
            Messages.Put_Line ("E1000: Packet(s) received (ICR=" &
               Unsigned_32'Image (ICR) & ")");
            --  The actual packet processing will happen when Read is called
         end if;

         if (ICR and 16#02#) /= 0 or (ICR and 16#01#) /= 0 then
            Messages.Put_Line ("E1000: Transmit completed");
         end if;
      end if;

      --  Acknowledge the interrupt at LAPIC
      Arch.APIC.LAPIC_EOI;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in E1000 Handle_Interrupt!");
         Arch.APIC.LAPIC_EOI;
   end Handle_Interrupt;


   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      CD : constant Controller_Data_Acc :=
         Controller_Data_Acc (C1.To_Pointer (Key));
      pragma Unreferenced (Offset);

      Packet_Len : Unsigned_16;
   begin
      --  Check if there's a packet available
      if (CD.RX_Descriptors (CD.RX_Next).Status and
          RX_STATUS_DD) = 0
      then
         --  No packet available
         if not Is_Blocking then
            Ret_Count := 0;
            Success := Dev_Success;
            return;
         end if;

         --  Wait for packet if blocking
         loop
            exit when (CD.RX_Descriptors (CD.RX_Next).Status and
               RX_STATUS_DD) /= 0;
            Scheduler.Yield_If_Able;
         end loop;
      end if;

      Packet_Len := CD.RX_Descriptors (CD.RX_Next).Length;

      --  Check if packet fits in buffer
      if Natural (Packet_Len) > Data'Length then
         Ret_Count := 0;
         Success := Dev_IO_Failure;
         return;
      end if;

      --  Copy packet data from RX buffer
      declare
         RX_Buffer : Operation_Data (1 .. MAX_PACKET_SIZE)
            with Import, Address => To_Addr
               (CD.RX_Buffers (CD.RX_Next));
      begin
         Data (Data'First .. Data'First + Natural (Packet_Len) - 1) :=
            RX_Buffer (1 .. Natural (Packet_Len));
      end;

      --  Reset descriptor for reuse
      CD.RX_Descriptors (CD.RX_Next).Status := 0;

      --  Advance RX tail pointer to make descriptor available again
      if CD.MMIO_Regs /= null then
         CD.MMIO_Regs.RDT := Unsigned_32 (CD.RX_Next);
      end if;

      --  Advance to next descriptor
      CD.RX_Next := CD.RX_Next + 1;
      Ret_Count := Natural (Packet_Len);
      Success := Dev_Success;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in E1000 Read!");
         Ret_Count := 0;
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
      CD : constant Controller_Data_Acc :=
         Controller_Data_Acc (C1.To_Pointer (Key));
      pragma Unreferenced (Offset);
   begin
      --  Check if CD is valid
      if CD = null then
         Ret_Count := 0;
         Success := Dev_IO_Failure;
         return;
      end if;

      --  Check if packet is too large
      if Data'Length > MAX_PACKET_SIZE then
         Messages.Put_Line ("E1000 Write: Packet too large (" &
            Data'Length'Image & " bytes, max=" & MAX_PACKET_SIZE'Image & ")");
         Ret_Count := 0;
         Success := Dev_IO_Failure;
         return;
      end if;

      --  Check if the descriptor is free
      if (CD.TX_Descriptors (CD.TX_Next).Status and
          DESC_STATUS_DD) = 0
      then
         if not Is_Blocking then
            Ret_Count := 0;
            Success := Dev_IO_Failure;
            return;
         end if;

         --  Wait for descriptor to become free
         loop
            exit when (CD.TX_Descriptors (CD.TX_Next).Status and
               DESC_STATUS_DD) /= 0;
            Scheduler.Yield_If_Able;
         end loop;
      end if;

      --  Copy packet data to TX buffer
      declare
         TX_Buffer : Operation_Data (1 .. MAX_PACKET_SIZE)
            with Import, Address => To_Addr
               (CD.TX_Buffers (CD.TX_Next));
      begin
         TX_Buffer (1 .. Data'Length) := Data;
      end;

      --  Setup TX descriptor
      CD.TX_Descriptors (CD.TX_Next).Address :=
         Unsigned_64 (CD.TX_Buffers (CD.TX_Next) -
            Memory.Memory_Offset);
      CD.TX_Descriptors (CD.TX_Next).Length :=
         Unsigned_16 (Data'Length);

      CD.TX_Descriptors (CD.TX_Next).CMD :=
         TX_CMD_EOP or TX_CMD_IFCS or TX_CMD_RS;

      CD.TX_Descriptors (CD.TX_Next).Status := 0;


      --  Advance TX tail pointer
      CD.TX_Next := CD.TX_Next + 1;

      if CD.MMIO_Regs /= null then
         CD.MMIO_Regs.TDT := Unsigned_32 (CD.TX_Next);
      else
         Messages.Put_Line ("E1000 Write: ERROR - MMIO_Regs is null!");
      end if;

      --  Wait for transmission to complete if blocking
      if Is_Blocking then
         loop
            exit when (CD.TX_Descriptors (CD.TX_Next).Status and
               DESC_STATUS_DD) /= 0;
         end loop;
      end if;

      Ret_Count := Data'Length;
      Success := Dev_Success;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("E1000 Write: *** Constraint_Error caught! ***");
         Ret_Count := 0;
         Success := Dev_IO_Failure;
   end Write;


   procedure Init (Success : out Boolean) is
      PCI_Dev : Devices.PCI.PCI_Device;
   begin
      Success := True;

      --  Enumerate and initialize all e1000 devices
      for Idx in 1 .. Devices.PCI.Enumerate_Devices (
         Vendor_ID => E1000_VENDOR_ID,
         Device_ID => E1000_DEVICE_ID
      ) loop
         Devices.PCI.Search_Device
            (Vendor_ID => E1000_VENDOR_ID,
             Device_ID => E1000_DEVICE_ID,
             Idx => Idx,
             Result => PCI_Dev,
             Success => Success);
         if not Success then
            return;
         end if;

         Init_Device (PCI_Dev, E1000, Idx, Success);
      end loop;

      --  Enumerate and initialize all e1000e devices
      for Idx in 1 .. Devices.PCI.Enumerate_Devices (
         Vendor_ID => E1000E_VENDOR_ID,
         Device_ID => E1000E_DEVICE_ID
      ) loop
         Devices.PCI.Search_Device
            (Vendor_ID => E1000E_VENDOR_ID,
             Device_ID => E1000E_DEVICE_ID,
             Idx => Idx,
             Result => PCI_Dev,
             Success => Success);
         if not Success then
            return;
         end if;

         Init_Device (PCI_Dev, E1000E, Idx, Success);
      end loop;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in E1000 Init!");
         Success := False;
   end Init;

   procedure Init_Device
      (PCI_Dev : Devices.PCI.PCI_Device;
       Model   : Device_Model;
       Idx     : Natural;
       Success : out Boolean)
   is
      PCI_Bar : Devices.PCI.Base_Address_Register;

      RX_Ring_Mem : Memory.Virtual_Address;
      TX_Ring_Mem : Memory.Virtual_Address;
      RX_Bufs_Start : Memory.Virtual_Address;
      TX_Bufs_Start : Memory.Virtual_Address;

      CD : Controller_Data_Acc;
      Dev : Device_Handle;
      Device : Resource;
      MAC_Low : Unsigned_32;
      MAC_High : Unsigned_16;
      U : Unsigned_32;
      pragma Unreferenced (U);
   begin
      
      --  Get the MMIO BAR (usually BAR0 for e1000)
      Devices.PCI.Get_BAR (PCI_Dev, 0, PCI_Bar, Success);
      if not Success then
         Messages.Put_Line ("Failed to get BAR0");
         return;
      end if;

      --  Map the MMIO BAR to virtual memory
      Memory.MMU.Map_Range
         (Map            => Memory.MMU.Kernel_Table,
            Physical_Start => To_Address (PCI_Bar.Base),
            Virtual_Start  => To_Address
            (PCI_Bar.Base + Memory.Memory_Offset),
            Length         => Storage_Count (A.Align_Up
            ((MMIO_Registers'Size / 8), Memory.MMU.Page_Size)),
            Permissions    =>
            (Is_User_Accessible => False,
            Can_Read          => True,
            Can_Write         => True,
            Can_Execute       => False,
            Is_Global         => True),
            Success        => Success);
      if not Success then
         Messages.Put_Line ("Failed to map MMIO BAR to virtual memory");
         return;
      end if;

      --  Enable bus mastering
      Devices.PCI.Enable_Bus_Mastering (PCI_Dev);

      --  Allocate receive descriptor ring
      Memory.Physical.Alloc
         (Result => RX_Ring_Mem,
            Sz => A.Align_Up (RX_RING_SIZE * (RX_Descriptor'Size / 8),
               Arch.MMU.Page_Size));

      --  Allocate transmit descriptor ring
      Memory.Physical.Alloc
         (Result => TX_Ring_Mem,
            Sz => A.Align_Up (TX_RING_SIZE * (TX_Descriptor'Size / 8),
               Arch.MMU.Page_Size));

      --  Allocate receive packet buffers
      Memory.Physical.Alloc
         (Result => RX_Bufs_Start,
            Sz => A.Align_Up (RX_RING_SIZE * MAX_PACKET_SIZE,
                              Arch.MMU.Page_Size));
      if not Success then
         Messages.Put_Line ("Failed to allocate RX packet buffers");
         return;
      end if;

      --  Allocate transmit packet buffers
      Memory.Physical.Alloc
         (Result => TX_Bufs_Start,
            Sz => A.Align_Up (TX_RING_SIZE * MAX_PACKET_SIZE,
                              Arch.MMU.Page_Size));
      if not Success then
         Messages.Put_Line ("Failed to allocate TX packet buffers");
         return;
      end if;

      Messages.Put_Line ("E1000: MMIO BAR at " &
         Integer_Address'Image (PCI_Bar.Base) &
         ", mapped at " &
         System.Address'Image
            (To_Addr (PCI_Bar.Base + Memory.Memory_Offset)));

      --  Create controller data structure
      CD := new Controller_Data'
         (MMIO_Regs => MMIO_Registers_Acc (C_MMIO.To_Pointer
            (To_Addr (PCI_Bar.Base + Memory.Memory_Offset))),
          RX_Descriptors => RX_Descriptor_Array_Acc (C_RX_Desc.To_Pointer
               (To_Addr (RX_Ring_Mem))),
          TX_Descriptors => TX_Descriptor_Array_Acc (C_TX_Desc.To_Pointer
               (To_Addr (TX_Ring_Mem))),
          RX_Buffers => [others => RX_Bufs_Start],
          TX_Buffers => [others => TX_Bufs_Start],
          RX_Next => 0,
          TX_Next => 0,
          Interrupt => 0,
          Interrupt_IDT_Index => 0,
          Model => Model);

      --  Get interrupt number from PCI config space
      Devices.PCI.Read8 (PCI_Dev, 16#3C#, CD.Interrupt);

      --  Update buffer addresses in the controller data;
      for J in RX_Desc_Idx_Type loop
         CD.RX_Buffers (J) := RX_Bufs_Start +
            Memory.Virtual_Address'Mod (Unsigned_64 (J) * MAX_PACKET_SIZE);
      end loop;

      for J in TX_Desc_Idx_Type loop
         CD.TX_Buffers (J) := TX_Bufs_Start +
            Memory.Virtual_Address'Mod (Unsigned_64 (J) * MAX_PACKET_SIZE);
      end loop;

      --  Initialize TX descriptors
      for J in TX_Desc_Idx_Type loop
         CD.TX_Descriptors (J).Address := 0;
         CD.TX_Descriptors (J).Length := 0;
         CD.TX_Descriptors (J).CSO := 0;
         CD.TX_Descriptors (J).CMD := 0;
         CD.TX_Descriptors (J).Status := DESC_STATUS_DD;  -- Mark as done
         CD.TX_Descriptors (J).CSS := 0;
         CD.TX_Descriptors (J).VLAN := 0;
      end loop;

      --  Initialize RX descriptors with buffer addresses
      for J in RX_Desc_Idx_Type loop
         CD.RX_Descriptors (J).Address := Unsigned_64 (
            CD.RX_Buffers (J) - Memory.Memory_Offset);
         CD.RX_Descriptors (J).Length := 0;
         CD.RX_Descriptors (J).Checksum := 0;
         CD.RX_Descriptors (J).Status := 0;
         CD.RX_Descriptors (J).Errors := 0;
         CD.RX_Descriptors (J).Reserved := 0;
      end loop;

      --  Set RDBAL and RDBAH with descriptor ring addresses
      if CD.MMIO_Regs /= null then
         declare
            RX_Phys : constant Unsigned_64 := Unsigned_64 (
               RX_Ring_Mem - Memory.Memory_Offset);
            TX_Phys : constant Unsigned_64 := Unsigned_64 (
               TX_Ring_Mem - Memory.Memory_Offset);
         begin
            CD.MMIO_Regs.RDBAL := Unsigned_32
               (RX_Phys and Unsigned_64 (Unsigned_32'Last));
            CD.MMIO_Regs.RDBAH := Unsigned_32 (Shift_Right (RX_Phys, 32));
            CD.MMIO_Regs.TDBAL := Unsigned_32
               (TX_Phys and Unsigned_64 (Unsigned_32'Last));
            CD.MMIO_Regs.TDBAH := Unsigned_32 (Shift_Right (TX_Phys, 32));
         end;
      end if;

      --  Initialize the descriptors
      if not Initialize_Descriptors (CD) then
         Messages.Put_Line ("Failed to initialize descriptors");
         return;
      end if;

      --  Enable receiver
      if not Enable_Receiver (CD) then
         Messages.Put_Line ("Failed to enable receiver");
         return;
      end if;

      --  Enable transmitter
      if not Enable_Transmitter (CD) then
         Messages.Put_Line ("Failed to enable transmitter");
         return;
      end if;

      --  Install interrupt handler
      Success := Install_Interrupt_Handler (CD);
      if not Success then
         return;
      end if;

      --  Enable interrupts
      if CD.MMIO_Regs /= null then
         --  Set interrupt mask to enable:
         --  Bit 0 (0x01): Transmit Descriptor Written Back
         --  Bit 1 (0x02): Transmit Queue Empty
         --  Bit 2 (0x04): Link Status Change
         --  Bit 4 (0x10): Receive Descriptor Minimum Threshold
         --  Bit 6 (0x40): Receiver FIFO Overrun
         --  Bit 7 (0x80): Receive Timer Interrupt
         --  Common mask: 0x1F6DC or simplified 0x9D
         --   (LSU + RXT0 + RXDMT0 + TXDW + TXQE)
         CD.MMIO_Regs.IMask := 16#1F6DC#;

         --  Clear any pending interrupts
         U := Unsigned_32 (CD.MMIO_Regs.ICR);
      end if;

      --  Read MAC address from MMIO registers at offset 0x5400
      if CD.MMIO_Regs /= null then
         MAC_Low := CD.MMIO_Regs.RA_Low;
         MAC_High := Unsigned_16
            (CD.MMIO_Regs.RA_High and Unsigned_32 (Unsigned_16'Last));
      end if;

      --  Register the device with the kernel
      Device :=
         (Data        => C1.To_Address (C1.Object_Pointer (CD)),
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
            Remove      => null);
      Register (Device, "e1000" & Integer'Image (Idx), Success);

      if Success then
         Dev := Fetch ("e1000" & Integer'Image (Idx));
         Networking.Interfaces.Register_Interface
            (Interfaced  => Dev,
               MAC         => [Unsigned_8 (MAC_Low and
                                          Unsigned_32 (Unsigned_8'Last)),
                              Unsigned_8 (Shift_Right (MAC_Low, 8)
                                          and Unsigned_32 (Unsigned_8'Last)),
                              Unsigned_8 (Shift_Right (MAC_Low, 16)
                                          and Unsigned_32 (Unsigned_8'Last)),
                              Unsigned_8 (Shift_Right (MAC_Low, 24)
                                          and Unsigned_32 (Unsigned_8'Last)),
                              Unsigned_8 (MAC_High and
                                          Unsigned_16 (Unsigned_8'Last)),
                              Unsigned_8 (Shift_Right (MAC_High, 8)
                                          and Unsigned_16 (Unsigned_8'Last))
                              ],
               IPv4        => [10, 0, 2, 15],
               IPv4_Subnet => [255, 0, 0, 0],
               IPv6        => [1 .. 8 => 0, 9 .. 12 => Unsigned_8'Last,
               13 => 10, 14 => 0, 15 => 2, 16 => 15],
               IPv6_Subnet => [16 => 0, 1 .. 8 => 0,
                              others => Unsigned_8'Last],
               Success     => Success);
         Networking.Interfaces.Block (Dev, False, Success);
      end if;

      Messages.Put_Line ("Enumerated e1000-compatible device, " &
         "Model " & Device_Model'Image (Model) & ", ID " &
         "MMIO Base at " & Integer_Address'Image (PCI_Bar.Base));
      Messages.Put_Line ("  MAC Address: " &
         Unsigned_8'Image
            (Unsigned_8 (MAC_Low and Unsigned_32 (Unsigned_8'Last))) &
         ":" &
         Unsigned_8'Image
            (Unsigned_8 (Shift_Right (MAC_Low, 8) and
                           Unsigned_32 (Unsigned_8'Last)))
            & ":" &
         Unsigned_8'Image
            (Unsigned_8 (Shift_Right (MAC_Low, 16) and
                           Unsigned_32 (Unsigned_8'Last)))
            & ":" &
         Unsigned_8'Image
            (Unsigned_8 (Shift_Right (MAC_Low, 24) and
                           Unsigned_32 (Unsigned_8'Last)))
            & ":" &
         Unsigned_8'Image
            (Unsigned_8 (MAC_High and Unsigned_16 (Unsigned_8'Last))) &
            ":" &
         Unsigned_8'Image
            (Unsigned_8 (Shift_Right (MAC_High, 8) and
                           Unsigned_16 (Unsigned_8'Last))));
                           
      Success := True;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in E1000 Init_Device!");
         Success := False;
   end Init_Device;

end Devices.PCI.E1000;
