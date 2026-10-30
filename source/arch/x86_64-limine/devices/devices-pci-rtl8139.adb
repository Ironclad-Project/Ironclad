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

   procedure Init (Success : out Boolean) is
      PCI_Dev : Devices.PCI.PCI_Device;
      PCI_Bar : Devices.PCI.Base_Address_Register;

      Receive_Buffer_Start   : Memory.Virtual_Address;
      Transmit_Buffers_Start : Memory.Virtual_Address;

      IO_Base : Unsigned_16;
      CD : Controller_Data_Acc;

      Temp_Bool : Boolean;

      Dev    : Device_Handle;
      Device : Resource;
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
            return;
         end if;

         Devices.PCI.Get_BAR (PCI_Dev, 0, PCI_Bar, Success);
         Devices.PCI.Enable_Bus_Mastering (PCI_Dev);

         Memory.Physical.Lower_Half_Alloc
          (Addr => Receive_Buffer_Start,
           --  We configure a 16K buffer in the NIC, and also enable
           --  receive buffer wrapping, so the buffer also holds the longest
           --  frame taken running past the ring's end.
           Size => A.Align_Up (RECEIVE_BUFFER_SIZE, Arch.MMU.Page_Size),
           Success => Success);

         if not Success then
            Messages.Put_Line ("Failed to allocate receive buffer; "
            & "got addr=" & Memory.Virtual_Address'Image
             (Receive_Buffer_Start));
            return;
         end if;

         if PCI_Bar.Base > Memory.Virtual_Address (Unsigned_16'Last) then
            Messages.Put_Line ("IO Base out of range");
            return;
         end if;

         --  Really ugly code to convert the PCI Bar to a port IO offset
         IO_Base := Unsigned_16 (To_Integer (To_Address (PCI_Bar.Base)));

         --  Allocate the Controller Data
         CD := new Controller_Data'
          (Receive_Buffer_Start   => Receive_Buffer_Start,
           IO_Base                => IO_Base,
           Interrupt              => <>,
           Transmit_Desc_Idx      => 0,
           Transmit_Buffers       => [others => Memory.Null_Address],
           Lock                   => Synchronization.Unlocked_Mutex,
           Receive_Offset         => 0);
         Devices.PCI.Read8 (PCI_Dev, 16#3C#, CD.Interrupt);
         --  Allocate the transmit buffers
         Memory.Physical.Lower_Half_Alloc
          (Addr    => Transmit_Buffers_Start,
           Size    => A.Align_Up (MAX_FRAME * 4, Arch.MMU.Page_Size),
           Success => Success);
         if not Success then
            return;
         end if;

         for Idx in Transmit_Desc_Idx_Type loop
            CD.Transmit_Buffers (Idx) := Transmit_Buffers_Start +
             Memory.Virtual_Address'Mod (Natural (Idx) * MAX_FRAME);
         end loop;

         --  Hook the interrupt
         Success := Install_Interrupt_Handler (CD);
         if not Success then
            return;
         end if;

         --  Power on the NIC
         Put_IO_8 (CD, REG_CONFIG_1, 0);

         --  Reset the NIC
         Put_IO_8 (CD, REG_CMD, 16#10#);

         while (Get_IO_8 (CD, REG_CMD) and 16#10#) /= 0 loop
            Scheduler.Yield_If_Able;
         end loop;

         --  No interrupt source is enabled: the handler only reads and
         --  acknowledges the status and takes no packet out of the ring, so
         --  an interrupt per packet costs time and does nothing. Read polls
         --  the BUFE bit instead. Enable ROK (bit 0) and TOK (bit 2) when
         --  the handler does something with them.
         Put_IO_16 (CD, REG_IMR, 16#0000#);

         --  Program Receive Configuration Register
         --  Specifically, we enable:
         --   physical MAC match, multicast, and broadcast packets (bits 1-3)
         --   receive buffer wrap (bit 7)
         --   16K buffer (bit 11)
         --  TODO: do we want to set other things here?
         Put_IO_32 (CD, REG_RCR, RCR_SETTINGS);

         --  Set the receive buffer
         Success := Set_Receive_Buffer (CD);
         if not Success then
            Messages.Put_Line ("Failed to set receive buffer");
            return;
         end if;

         --  Enable the receive and transmit engines now
         Put_IO_8 (CD, REG_CMD, 16#0C#);

         --  Register the device
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
            IO_Argument => null,
            Mmap        => null,
            Poll        => null,
            Remove      => null);
         Register (Device, "rtl8139" & Integer'Image (Idx), Success);
         if Success then
            Dev := Fetch ("rtl8139" & Integer'Image (Idx));

            --  The card's own address, out of IDR0 .. IDR5, and no IPv4
            --  address until one is given, as the e1000 registers.
            Networking.Interfaces.Register_Interface
               (Interfaced  => Dev,
                MAC         =>
                  [Get_IO_8 (CD, REG_ID),     Get_IO_8 (CD, REG_ID + 1),
                   Get_IO_8 (CD, REG_ID + 2), Get_IO_8 (CD, REG_ID + 3),
                   Get_IO_8 (CD, REG_ID + 4), Get_IO_8 (CD, REG_ID + 5)],
                IPv4        => [0, 0, 0, 0],
                IPv4_Subnet => [0, 0, 0, 0],
                Success     => Success);
         end if;
      end loop;
   exception
      when Constraint_Error =>
         Success := False;
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

   procedure Reset_Receiver (CD : Controller_Ref) is
   begin
      Put_IO_8 (CD, REG_CMD, CMD_TE);
      Put_IO_8 (CD, REG_CMD, CMD_RE or CMD_TE);
      Put_IO_32 (CD, REG_RCR, RCR_SETTINGS);
      Put_IO_16 (CD, REG_CAPR, 16#FFF0#);
      CD.Receive_Offset := 0;
   end Reset_Receiver;

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
      --  Acknowledge what was pending of the two sources IMR enables, ROK
      --  (bit 0) and TOK (bit 2): "Writing a 1 to any bit will reset that
      --  bit, but writing a 0 has no effect" (RTL8139C(L)+ datasheet rev
      --  1.6, 6.7).
      Status := Get_IO_16 (CD, REG_ISR);
      Put_IO_16 (CD, REG_ISR, Status and 16#05#);

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
      --
      --  Nothing is printed here, as the console is far too slow to be
      --  written to once per packet.
   end Handle_Interrupt;

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset);
   begin
      Ret_Count := 0;
      declare
         CD       : constant Controller_Ref :=
            Controller_Ref (C1.To_Pointer (Key));
         Ring     : Receive_Buffer
            with Import, Address => To_Address (CD.Receive_Buffer_Start);
         Held     : Boolean := False;
         At_Frame : Positive;
         Status   : Unsigned_16;
         Length   : Natural;
         Count    : Natural;
      begin
         loop
            Synchronization.Seize (CD.Lock);
            Held := True;
            if (Get_IO_8 (CD, REG_CMD) and CMD_BUFE) = 0 then
               --  The header is read after the card says a frame is there.
               Arch.Snippets.Full_Memory_Load_Store_Barrier;
               At_Frame := CD.Receive_Offset + 1;
               Status :=
                  Unsigned_16 (Ring (At_Frame)) or
                  Unsigned_16 (Ring (At_Frame + 1)) * 16#100#;
               Length :=
                  Natural (Ring (At_Frame + 2)) +
                  Natural (Ring (At_Frame + 3)) * 16#100#;
               if (Status and RX_STATUS_ROK) /= 0 and
                  Length in RX_LENGTH_MIN .. RX_LENGTH_MAX
               then
                  Count := Natural'Min (Length - 4, Data'Length);
                  Data (Data'First .. Data'First + Count - 1) :=
                     Ring (At_Frame + 4 .. At_Frame + 3 + Count);

                  --  The frame's room goes back to the card once it is read.
                  --  Each frame starts at the next double word after the last
                  --  (RTL8139C(L)+ datasheet rev 1.6, 8.3.2), and CAPR is 16
                  --  bytes short of the offset it stands for, 0FFF0h with
                  --  nothing read (6).
                  CD.Receive_Offset :=
                     ((CD.Receive_Offset + Length + 4 + 3) / 4 * 4) mod
                     RECEIVE_RING_SIZE;
                  Arch.Snippets.Full_Memory_Load_Store_Barrier;
                  Put_IO_16 (CD, REG_CAPR, Unsigned_16
                     ((CD.Receive_Offset + 16#FFF0#) mod 16#1_0000#));
                  Held := False;
                  Synchronization.Release (CD.Lock);
                  Ret_Count := Count;
                  Success   := Dev_Success;
                  return;
               end if;
               Reset_Receiver (CD);
            end if;
            Held := False;
            Synchronization.Release (CD.Lock);
            if not Is_Blocking then
               Success := Dev_Would_Block;
               return;
            elsif Scheduler.Is_Interrupted then
               Success := Dev_Interrupted;
               return;
            end if;
            Scheduler.Yield_If_Able;
         end loop;
      exception
         when Constraint_Error =>
            if Held then
               Synchronization.Release (CD.Lock);
            end if;
            Messages.Put_Line ("Constraint_Error in Read!");
            Success := Dev_IO_Failure;
      end;
   exception
      when Constraint_Error =>
         Success := Dev_IO_Failure;
   end Read;

   --  A write puts the frame into the buffer of the next transmit descriptor
   --  once the card has read what that buffer held, padded to the shortest
   --  frame that goes out, and hands the descriptor to the card. With the
   --  buffer still the card's, a write told not to wait fails with
   --  Dev_Would_Block, and one told to wait waits for it until the thread
   --  is killed or has a signal to take, or the card has kept it for too
   --  many tries. The descriptors are only touched with the lock held, and
   --  the lock is never held across a wait.
   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset);
      Max_Tries : constant := 100_000;
   begin
      Ret_Count := 0;
      if Data'Length = 0 then
         Success := Dev_Success;
         return;
      elsif Data'Length > MAX_FRAME then
         Success := Dev_Out_Of_Range;
         return;
      end if;

      declare
         CD    : constant Controller_Ref :=
            Controller_Ref (C1.To_Pointer (Key));
         Held  : Boolean := False;
         Tries : Natural := 0;
         Idx   : Transmit_Desc_Idx_Type;
      begin
         loop
            Synchronization.Seize (CD.Lock);
            Held := True;
            Idx := CD.Transmit_Desc_Idx;
            exit when (Get_IO_32 (CD, Transmit_Descriptor_Status_Registers
               (Idx)) and TSD_OWN) /= 0;
            Held := False;
            Synchronization.Release (CD.Lock);
            Tries := Tries + 1;
            if not Is_Blocking then
               Success := Dev_Would_Block;
               return;
            elsif Tries = Max_Tries then
               Success := Dev_IO_Failure;
               return;
            elsif Scheduler.Is_Interrupted then
               Success := Dev_Interrupted;
               return;
            end if;
            Scheduler.Yield_If_Able;
         end loop;

         declare
            Buffer : Transmit_Buffer
               with Import, Address => To_Address (CD.Transmit_Buffers (Idx));
         begin
            Buffer (1 .. Data'Length) := Data;
            if Data'Length < MIN_FRAME then
               Buffer (Data'Length + 1 .. MIN_FRAME) := [others => 0];
            end if;
         end;

         --  The frame is in memory before the card is told to read it.
         Arch.Snippets.Full_Memory_Load_Store_Barrier;
         Put_IO_32 (CD, Transmit_Descriptor_Address_Registers (Idx),
            Unsigned_32 (CD.Transmit_Buffers (Idx) - Memory.Memory_Offset));

         --  The size written with OWN clear starts the transmission.
         Put_IO_32 (CD, Transmit_Descriptor_Status_Registers (Idx),
            Unsigned_32 (Natural'Max (Data'Length, MIN_FRAME)));
         CD.Transmit_Desc_Idx := Idx + 1;
         Held := False;
         Synchronization.Release (CD.Lock);
         Ret_Count := Data'Length;
         Success   := Dev_Success;
      exception
         when Constraint_Error =>
            if Held then
               Synchronization.Release (CD.Lock);
            end if;
            Messages.Put_Line ("Constraint_Error in Write!");
            Success := Dev_IO_Failure;
      end;
   exception
      when Constraint_Error =>
         Success := Dev_IO_Failure;
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
