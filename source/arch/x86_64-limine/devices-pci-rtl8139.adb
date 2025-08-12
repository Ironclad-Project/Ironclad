--  devices-rtl8139.ads: RTL8139 driver.
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

with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;
--  TODO: not x86
with Arch.APIC;
with Arch.CPU;
with Arch.IDT; use Arch.IDT;
with Arch.MMU;
with Messages;
with Memory.Physical;
with Alignment;
with Arch.Snippets;
with Scheduler;
with Networking.Interfaces;

package body Devices.PCI.RTL8139 with SPARK_Mode => Off is
   package A is new Alignment (Unsigned_64);
   package C1 is new System.Address_To_Access_Conversions (Controller_Data);

   function Init return Boolean is
      PCI_Dev : Devices.PCI.PCI_Device;
      PCI_Bar : Devices.PCI.Base_Address_Register;

      Receive_Buffer_Start   : Memory.Virtual_Address;
      Transmit_Buffers_Start : Memory.Virtual_Address;

      IO_Base : Unsigned_16;
      CD : Controller_Data_Acc;

      Temp_Bool : Boolean;

      Dev    : Device_Handle;
      Device : Resource;

      Success : Boolean;
   begin
      Success := True;

      for Idx in 1 .. Devices.PCI.Enumerate_Devices (
         Vendor_ID => 16#10EC#,
         Device_ID => 16#8139#
      ) loop
         Devices.PCI.Search_Device
            (Vendor_ID => 16#10EC#,
             Device_ID => 16#8139#,
             Idx => Idx,
             Result => PCI_Dev,
             Success => Success);
         if not Success then
            return False;
         end if;

         Devices.PCI.Get_BAR (PCI_Dev, 0, PCI_Bar, Success);
         Devices.PCI.Enable_Bus_Mastering (PCI_Dev);

         Memory.Physical.Lower_Half_Alloc
          (Addr => Receive_Buffer_Start,
           --  We configure a 16K buffer in the NIC, and also enable
           --  receive buffer wrapping.
           --  Datasheet says to allocate 16K + 16 bytes for 16K receive buffer
           --  size, plus 1500 bytes if WRAP is enabled.
           Size => A.Align_Up (RECEIVE_BUFFER_SIZE, Arch.MMU.Page_Size),
           Success => Success);

         if not Success then
            Messages.Put_Line ("Failed to allocate receive buffer; "
            & "got addr=" & Memory.Virtual_Address'Image
             (Receive_Buffer_Start));
            return False;
         end if;

         if PCI_Bar.Base > Memory.Virtual_Address (Unsigned_16'Last) then
            Messages.Put_Line ("IO Base out of range");
            return False;
         end if;

         --  Really ugly code to convert the PCI Bar to a port IO offset
         IO_Base := Unsigned_16 (To_Integer (To_Address (PCI_Bar.Base)));

         --  Allocate the Controller Data
         CD := new Controller_Data'
          (Receive_Buffer_Start   => Receive_Buffer_Start,
           IO_Base                => IO_Base,
           Interrupt              => <>,
           Transmit_Desc_Idx      => 0,
           Transmit_Buffers       => [others => Memory.Null_Address]);
         Devices.PCI.Read8 (PCI_Dev, 16#3C#, CD.Interrupt);
         --  Allocate the transmit buffers
         Memory.Physical.Lower_Half_Alloc
          (Addr    => Transmit_Buffers_Start,
           Size    => A.Align_Up (16#700# * 4, Arch.MMU.Page_Size),
           Success => Success);
         if not Success then
            return False;
         end if;

         for Idx in Transmit_Desc_Idx_Type loop
            CD.Transmit_Buffers (Idx) := Transmit_Buffers_Start +
             Memory.Virtual_Address'Mod (Natural (Idx) * 16#700#);
         end loop;

         --  Hook the interrupt
         Success := Install_Interrupt_Handler (CD);
         if not Success then
            return False;
         end if;

         --  Power on the NIC
         Put_IO_8 (CD, REG_CONFIG_1, 0);

         --  Reset the NIC
         Put_IO_8 (CD, REG_CMD, 16#10#);

         while (Get_IO_8 (CD, REG_CMD) and 16#10#) /= 0 loop
            Scheduler.Yield_If_Able;
         end loop;

         --  Enable device interrupts
         --  Specifically, we set the TOK (Transmit OK) and ROK (Receive OK)
         --  bits here
         Put_IO_16 (CD, REG_IMR, 16#0005#);

         --  Program Receive Configuration Register
         --  Specifically, we enable:
         --   physical MAC match, multicast, and broadcast packets (bits 1-3)
         --   receive buffer wrap (bit 7)
         --   16K buffer (bit 11)
         --  TODO: do we want to set other things here?
         Put_IO_32 (CD, REG_RCR, 16#0000088F#);

         --  Set the receive buffer
         Success := Set_Receive_Buffer (CD);
         if not Success then
            Messages.Put_Line ("Failed to set receive buffer");
            return False;
         end if;

         --  Enable the receive and transmit engines now
         Put_IO_8 (CD, REG_CMD, 16#0C#);

         --  Register the device
         Device :=
           (Data        => C1.To_Address (C1.Object_Pointer (CD)),
            ID          => Zero_UUID,
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
         Register (Device, "rtl8139" & Integer'Image (Idx), Success);
         if Success then
            Dev := Fetch ("rtl8139" & Integer'Image (Idx));
            Networking.Interfaces.Register_Interface
               (Interfaced  => Dev,
                MAC         => [others => 1],
                IPv4        => [10, 0, 2, 15],
                IPv4_Subnet => [255, 0, 0, 0],
                IPv6        => [1 .. 8 => 0, 9 .. 12 => 16#FF#,
                 13 => 10, 14 => 0,  15 => 2, 16 => 15],
                IPv6_Subnet => [16 => 0, 1 .. 8 => 0, others => 16#FF#],
                Success     => Success);
            Networking.Interfaces.Block (Dev, False, Success);
         end if;

         Messages.Put_Line ("Enumerated RTL8139, IO Base at "
          & Unsigned_16'Image (CD.IO_Base));
         Messages.Put_Line ("MAC Address: " &
            Unsigned_64'Image (Unsigned_64 (Get_IO_8 (CD, REG_ID + 0))) & ":" &
            Unsigned_64'Image (Unsigned_64 (Get_IO_8 (CD, REG_ID + 1))) & ":" &
            Unsigned_64'Image (Unsigned_64 (Get_IO_8 (CD, REG_ID + 2))) & ":" &
            Unsigned_64'Image (Unsigned_64 (Get_IO_8 (CD, REG_ID + 3))) & ":" &
            Unsigned_64'Image (Unsigned_64 (Get_IO_8 (CD, REG_ID + 4))) & ":" &
            Unsigned_64'Image (Unsigned_64 (Get_IO_8 (CD, REG_ID + 5))));
      end loop;
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Init;

   function Set_Receive_Buffer (CD : Controller_Data_Acc) return Boolean
   is
   begin
      --  Program the RBSTART register; which points to the beginning of the
      --  receiver buffer
      Put_IO_32 (CD, REG_RBSTART, Unsigned_32
       (CD.Receive_Buffer_Start - Memory.Memory_Offset));
      return True;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error when setting receive "
          & "buffers");
         return False;
   end Set_Receive_Buffer;

   type RTL8139_Interrupt is record
      Idx : Arch.IDT.IDT_Index;
      CD : Controller_Data_Acc;
   end record;
   type RTL8139_Interrupt_Arr is array (1 .. 10) of RTL8139_Interrupt;
   Interrupts : RTL8139_Interrupt_Arr :=
      [others => (1, null)];

   procedure Generic_RTL8139_Interrupt_Handler (Num : Integer)
   is
   begin
      for Int of Interrupts loop
         if Natural (Int.Idx - 1) = Num then
            Handle_Interrupt (Int.CD);
            return;
         end if;
      end loop;

      Messages.Put_Line ("Unexpected RTL8139 Interrupt!");
      Arch.APIC.LAPIC_EOI;
   end Generic_RTL8139_Interrupt_Handler;

   function Install_Interrupt_Handler (CD : Controller_Data_Acc) return Boolean
   is
      Index   : Arch.IDT.IRQ_Index;
      Success : Boolean;
      I       : Arch.IDT.IRQ_Index;
   begin
      Messages.Put_Line ("RTL8139 Interrupt at " & Unsigned_8'Image
       (CD.Interrupt));

      --  Allocate an interrupt in the IDT and unmask it.
      I := Arch.IDT.IRQ_Index (CD.Interrupt + 33);
      Arch.IDT.Load_ISR
         (Address => Generic_RTL8139_Interrupt_Handler'Address,
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

      for Int of Interrupts loop
         if Int.Idx = 1 then
            Int := (Index, CD);
         end if;
      end loop;

      return True;
   exception
      when Constraint_Error =>
         return False;
   end Install_Interrupt_Handler;

   procedure Handle_Interrupt (CD : Controller_Data_Acc)
   is
      Status : Unsigned_16;
   begin
      Status := Get_IO_16 (CD, REG_ISR);
      Put_IO_16 (CD, REG_ISR, 16#05#);

      --  We acknowledge the interrupt here to prevent it from firing twice.
      Arch.APIC.LAPIC_EOI;

      --  TODO: Read out packets here
      --  In theory, a 100mbit line can do ~10k packets a second (assuming they
      --  are all) minimum size packets, which is not always unusual for some
      --  applications
      --  Polling this isn't super realistic, as the NIC will drop packets when
      --  it runs out of space in the ring buffer.
      --  We should (to a limit) read out packets here, and store them, as the
      --  ring buffer the NIC has is quite small.
      Messages.Put_Line ("RTL8139 Handle Interrupt, status=" &
         Unsigned_16'Image (Status));
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in Handle_Interrupt!");
         return;
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
      pragma Unreferenced (Offset, Is_Blocking);
   begin
      declare
         CMD : Unsigned_8;
      begin
         loop
            CMD := Get_IO_8 (CD, REG_CMD);
            --  Exit the loop when the BUFE (RX buffer empty) bit is 0
            exit when (CMD and 16#01#) = 0;
            Scheduler.Yield_If_Able;
         end loop;
      end;

      declare
         CAPR : Unsigned_16 := Get_IO_16 (CD, REG_CAPR);

         Packet_Length       : Unsigned_16;
         Packet_Header       : Unsigned_16;
         This_Receive_Buffer : Receive_Buffer
            with Import, Address => To_Address (CD.Receive_Buffer_Start);
         --  TODO: read the packet header; it contains bits for if the
         --  packet was properly received, and if CRC passed.
         pragma Unreferenced (Packet_Header);
      begin
         --  For some reason, CAPR is offset by -0x10. Offset this.
         CAPR := CAPR + 16#10#;

         --  Read the packet header.
         --  We have to offset everything by one as this array starts at 1.
         Packet_Header :=
            Unsigned_16 (This_Receive_Buffer (Integer (CAPR + 2)) * 2**8)
         or
            Unsigned_16 (This_Receive_Buffer (Integer (CAPR + 1)));

         Packet_Length :=
            Unsigned_16 (This_Receive_Buffer (Integer (CAPR + 4)) * 2**8)
         or
            Unsigned_16 (This_Receive_Buffer (Integer (CAPR + 3)));

         --  Copy the packet data out.
         if Integer (Packet_Length) <= Data'Length then
            declare
               Packet_Begin : constant Integer := Integer (CAPR) + 5;
               Packet_End   : constant Integer :=
                  Packet_Begin + Integer (Packet_Length) - 2;
            begin
               Data (Data'First .. Data'First + Integer (Packet_Length) - 2) :=
                  This_Receive_Buffer (Packet_Begin .. Packet_End);
               Ret_Count := Integer (Packet_Length - 2);
               Success := Dev_Success;
            end;
         else
            Messages.Put_Line ("Error in RTL8139 Receive: buffer too small!" &
               " We need " & Packet_Length'Image & " bytes, buffer is " &
               Data'Length'Image);
            Ret_Count := 0;
            Success := Dev_IO_Failure;
         end if;

         --  Update CAPR.
         CAPR := CAPR + Packet_Length + 4 - 16#10#;
         if CAPR >= RECEIVE_BUFFER_LOGICAL_SIZE then
            --  The NIC seems to be weird - when wrapping the ring buffer
            --  around, it does not seem to offset the CAPR by 0x10.
            CAPR := (CAPR + 16#10#) mod RECEIVE_BUFFER_LOGICAL_SIZE;
         end if;
         Put_IO_16 (CD => CD, Offset => REG_CAPR, Value => CAPR);
      end;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in Read!");
         Ret_Count := 0;
         Success := Dev_IO_Failure;
         return;
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
      Transmit_Descriptor : Unsigned_32;
      pragma Unreferenced (Offset, Is_Blocking);
   begin
      declare
         This_Transmit_Buffer_Address : constant Memory.Virtual_Address :=
            CD.Transmit_Buffers (CD.Transmit_Desc_Idx);
         This_Transmit_Buffer : Transmit_Buffer
            with Import, Address => To_Address (This_Transmit_Buffer_Address);
      begin
         --  We can not send packets larger then 0x700
         if Data'Length > 16#700# then
            Ret_Count := 0;
            Success := Dev_Full;
            return;
         end if;

         --  Copy the packet into the preallocated 100% low memory buffer
         This_Transmit_Buffer (1 .. Data'Length) := Data;

         --  Check if we own the current descriptor index
         --  AKA, if we are sending packets too quickly, wait for a descriptor
         --  to open up
         loop
            Transmit_Descriptor := Get_IO_32 (CD,
               Transmit_Descriptor_Status_Registers (CD.Transmit_Desc_Idx));
            exit when (Transmit_Descriptor and 16#2000#) /= 0;
            Scheduler.Yield_If_Able;
         end loop;

         --  Create the descriptor index, write the address and then the status
         --  descriptor
         --  TODO: do we want to set the TX FIFO setting thingy?
         Transmit_Descriptor := Data'Length;

         --  The really ugly hack to get the integer of the address again
         Put_IO_32 (CD,
            Transmit_Descriptor_Address_Registers (CD.Transmit_Desc_Idx),
            Unsigned_32 (CD.Transmit_Buffers (CD.Transmit_Desc_Idx)
               - Memory.Memory_Offset));

         --  Actually start the transmission by writing the status register
         --  (or, more specifically, setting the OWN bit to 0)
         Put_IO_32 (CD,
            Transmit_Descriptor_Status_Registers (CD.Transmit_Desc_Idx),
            Transmit_Descriptor);

         --  Increment our transmit descriptor index
         CD.Transmit_Desc_Idx := CD.Transmit_Desc_Idx + 1;
         Ret_Count := Data'Length;
         Success := Dev_Success;
      end;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in Write!");
         Ret_Count := 0;
         Success := Dev_IO_Failure;
         return;
   end Write;

   procedure Put_IO_8
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8;
       Value  : Unsigned_8)
   is
   begin
      Arch.Snippets.Port_Out
       (Port  => CD.IO_Base + Unsigned_16 (Offset),
        Value => Value);
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in Put_IO_8");
   end Put_IO_8;

   procedure Put_IO_16
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8;
       Value  : Unsigned_16)
   is
   begin
      Arch.Snippets.Port_Out16
       (Port  => CD.IO_Base + Unsigned_16 (Offset),
        Value => Value);
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in Put_IO_16");
   end Put_IO_16;

   procedure Put_IO_32
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8;
       Value  : Unsigned_32)
   is
   begin
      Arch.Snippets.Port_Out32
       (Port  => CD.IO_Base + Unsigned_16 (Offset),
        Value => Value);
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in Put_IO_32");
   end Put_IO_32;

   function Get_IO_8
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8) return Unsigned_8
   is
   begin
      return Arch.Snippets.Port_In (CD.IO_Base + Unsigned_16 (Offset));
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in Get_IO_8");
         return 0;
   end Get_IO_8;

   function Get_IO_16
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8) return Unsigned_16
   is
   begin
      return Arch.Snippets.Port_In16 (CD.IO_Base + Unsigned_16 (Offset));
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in Get_IO_16");
         return 0;
   end Get_IO_16;

   function Get_IO_32
      (CD     : Controller_Data_Acc;
       Offset : Unsigned_8) return Unsigned_32
   is
   begin
      return Arch.Snippets.Port_In32 (CD.IO_Base + Unsigned_16 (Offset));
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Constraint_Error in Get_IO_32");
         return 0;
   end Get_IO_32;

end Devices.PCI.RTL8139;
