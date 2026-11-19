--  devices-nvme.adb: NVMe driver.
--  Copyright (C) 2025 no92
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

with Alignment;
with Messages;
with Panic;
with Memory.Physical;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;
with Arch.MMU;

with Interfaces.C; use Interfaces.C;

package body Devices.PCI.NVMe with SPARK_Mode => Off is
   package C1 is new System.Address_To_Access_Conversions (NVMe_Registers);
   package C2 is new System.Address_To_Access_Conversions (NVMe_Doorbell);
   package C3 is
      new System.Address_To_Access_Conversions (Identify_Controller_Data);
   package C4 is
      new System.Address_To_Access_Conversions (Identify_Namespace_Data);
   package C5 is new System.Address_To_Access_Conversions (Namespace_Data);
   package C6 is
      new System.Address_To_Access_Conversions (Active_Namespace_Id_List);

   package A  is new Alignment (Unsigned_64);

   procedure Init (Success : out Boolean) is
      PCI_Dev : Devices.PCI.PCI_Device;
      PCI_BAR : Devices.PCI.Base_Address_Register;

      Mem_Addr : Integer_Address;
      Dev_Mem : NVMe_Registers_Acc;

      Controller_Caps : NVMe_Register_Controller_Capabilities;
      Controller_Version : NVMe_Register_Version;

      AQA : constant NVMe_Register_Admin_Queue_Attributes := (
         Admin_Submission_Queue_Size => 32,
         Admin_Completion_Queue_Size => 32,
         others => <>);

      Admin_Queue : Admin_Queue_Acc;
      IO_Queue : IO_Queue_Acc;

      Ctrl_Identify : Identify_Controller_Data_Acc;
      Namespace_List : Active_Namespace_Id_List_Acc;

      Val : Unsigned_16;
      Drive_Idx : Natural := 0;
   begin
      Success := True;
      for Idx in 1 .. Devices.PCI.Enumerate_Devices (1, 8, 2) loop
         Devices.PCI.Search_Device (1, 8, 2, Idx, PCI_Dev, Success);
         if not Success then
            Success := True;
            return;
         end if;

         Devices.PCI.Get_BAR (PCI_Dev, 0, PCI_BAR, Success);
         if not Success or else not PCI_BAR.Is_MMIO then
            Success := True;
            return;
         end if;

         Devices.PCI.Read16 (PCI_Dev, 4, Val);
         Devices.PCI.Write16 (PCI_Dev, 4, (Val and 16#77F#) or 16#406#);

         Mem_Addr := PCI_BAR.Base + Memory.Memory_Offset;
         Dev_Mem  := NVMe_Registers_Acc (C1.To_Pointer
            (To_Address (Mem_Addr)));

         Memory.MMU.Map_Range
            (Map            => Memory.MMU.Kernel_Table,
             Physical_Start => To_Address (PCI_BAR.Base),
             Virtual_Start  => To_Address (Mem_Addr),
             Length         => Storage_Count (A.Align_Up
              (16#2000#, Memory.MMU.Page_Size)),
             Permissions    =>
              (Is_User_Accessible => False,
               Can_Read          => True,
               Can_Write         => True,
               Can_Execute       => False,
               Is_Global         => True),
             Success        => Success,
             Caching        => Arch.MMU.Uncacheable);
         if not Success then
            return;
         end if;

         Controller_Caps := Dev_Mem.Controller_Capabilities;
         Controller_Version := Dev_Mem.Version;

         if (Shift_Left (1, 12 +
             Natural (Controller_Caps.Memory_Page_Size_Minimum))
             > Unsigned_64 (Memory.MMU.Page_Size)) or
            (Shift_Left (1, 12 +
             Natural (Controller_Caps.Memory_Page_Size_Maximum))
             < Unsigned_64 (Memory.MMU.Page_Size))
         then
            Success := True;
            return;
         end if;

         if (Controller_Caps.Command_Sets_Supported and Unsigned_8 (1)) = 0
         then
            Messages.Put_Line
               ("NVMe controller does not support the NVM command set");
         end if;

         Disable_Controller (Dev_Mem);

         Admin_Queue := Setup_Admin_Queue (Dev_Mem, 0, 32);

         Dev_Mem.Admin_Queue_Attributes := AQA;

         --  We rely on the fact that Memory.Physical.Alloc returns a
         --  page-aligned address for sizes >= Page_Size for making sure the
         --  lowest CC.MPS bits are zero.
         Dev_Mem.Admin_Submission_Queue_Base_Address
            := Unsigned_64 (Admin_Queue.SQ_Addr - Memory.Memory_Offset);
         Dev_Mem.Admin_Completion_Queue_Base_Address
            := Unsigned_64 (Admin_Queue.CQ_Addr - Memory.Memory_Offset);

         Enable_Controller (Dev_Mem);

         Ctrl_Identify := Controller_Identify (Admin_Queue);

         if Ctrl_Identify.Submission_Queue_Entry_Size /= 16#66# then
            Messages.Put_Line
               ("NVMe controller uses non-standard SQ Entry size, skipping.");
            Memory.Physical.Free
               (size_t (To_Integer (Ctrl_Identify.all'Address)));
            Success := True;
            return;
         end if;

         if Ctrl_Identify.Completion_Queue_Entry_Size /= 16#44# then
            Messages.Put_Line
               ("NVMe controller uses non-standard CQ Entry size, skipping.");
            Memory.Physical.Free
               (size_t (To_Integer (Ctrl_Identify.all'Address)));
            Success := True;
            return;
         end if;

         Memory.Physical.Free
            (size_t (To_Integer (Ctrl_Identify.all'Address)));
         Drive_Idx := Drive_Idx + 1;

         --  TODO: Set Feature "Number of Queues" (0x07)
         Request_IO_Queues (Admin_Queue, 1, 1);

         IO_Queue := Setup_IO_Queue (Dev_Mem, 1, Admin_Queue);

         --  NVMe 1.0 only supports CNS values of 0 and 1
         if Controller_Version.Major > 1 or
            (Controller_Version.Major = 1 and Controller_Version.Minor >= 1)
         then
            Namespace_List := Active_Namespaces (Admin_Queue);

            for NS_Idx in Namespace_List'Range loop
               exit when Namespace_List (NS_Idx) = 0;

               Init_Namespace
                  (Namespace_List (NS_Idx), Drive_Idx,
                   Admin_Queue, IO_Queue);
            end loop;
         else
            for NS_Idx in 1 .. Ctrl_Identify.Number_Of_Namespaces loop
               Init_Namespace
                  (NS_Idx, Drive_Idx, Admin_Queue, IO_Queue);
            end loop;
         end if;
      end loop;
   exception
      when Constraint_Error =>
         Success := False;
   end Init;

   procedure Request_IO_Queues
      (Admin_Queue : Admin_Queue_Acc;
       SQ_Count : Unsigned_16;
       CQ_Count : Unsigned_16)
   is
      Cmd : Admin_Submission_Queue_Entry;
      Status : Completion_Queue_Entry;
   begin
      Cmd := (Opcode => Set_Features,
         Set_Feature => (
            Id => Number_Of_Queues,
            IO_Submission_Queues => SQ_Count - 1,
            IO_Completion_Queues => CQ_Count - 1,
            others => <>
         ),
         others => <>);
      Status := Submit_Admin_Command (Admin_Queue, Cmd);

      if Status.Dword3.Status /= 0 then
         Panic.Hard_Panic ("Controller rejected Request for " &
            SQ_Count'Image & " SQs and " & CQ_Count'Image & " CQs.");
      end if;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Failed to request I/O queues");
   end Request_IO_Queues;

   procedure Init_Namespace
      (NS_Id : NVMe_NS_Id;
       Drive_Idx : Natural;
       Admin_Queue : Admin_Queue_Acc;
       IO_Queue : IO_Queue_Acc)
   is
      NS_Identify : Identify_Namespace_Data_Acc;

      FLBAS : Natural;
      LBADS : Natural;
      MS : Natural;
      NS_LBA_Size : Natural;
      NS_LBAs : Unsigned_64;
      NS_LBA_Buffer : Memory.Virtual_Address;

      Base_Name : constant String := "nvme";
      Success : Boolean;
   begin
      NS_Identify := Namespace_Identify (Admin_Queue, NS_Id);

      if NS_Identify.Namespace_Capacity = 0 then
         return;
      end if;

      --  The NVMe 1.0e spec falsely claims that this is a 0's based value?
      FLBAS := Natural (NS_Identify.Formatted_LBA_Size and 16#F#);

      if FLBAS > Natural (NS_Identify.Number_of_LBA_Formats) then
         Messages.Put_Line
            ("NVMe " & Drive_Idx'Image & " Namespace " & NS_Id'Image &
               " has no valid LBA formats");
         return;
      end if;

      --  LBA Size is given in terms of a power of two (2^n)
      LBADS := Natural (NS_Identify.LBA_Format_Support (FLBAS).LBA_Data_Size);
      MS := Natural (NS_Identify.LBA_Format_Support (FLBAS).Metadata_Size);
      if LBADS not in 9 .. 63 or else
         Shift_Left (Unsigned_64 (1), LBADS) > Memory.MMU.Page_Size or else
         Shift_Left (Unsigned_64 (1), LBADS) + Unsigned_64 (MS) >
            2 * Memory.MMU.Page_Size
      then
         Messages.Put_Line
            ("unsupported LBA format in NVMe " & Drive_Idx'Image &
             " namespace " & NS_Id'Image);
         Memory.Physical.Free
            (size_t (To_Integer (NS_Identify.all'Address)));
         return;
      end if;
      NS_LBA_Size := 2 ** LBADS;
      --  Namespace Size is reported in units of LBAs
      NS_LBAs := NS_Identify.Namespace_Size;

      --  Allocations are page aligned, so an LBA never crosses a page, and
      --  with its metadata it spans two at most.
      Memory.Physical.Alloc (size_t (NS_LBA_Size + MS), NS_LBA_Buffer);

      declare
         NS : constant Namespace_Data_Acc := new Namespace_Data'(
            LBA_Size => NS_LBA_Size,
            LBA_Count => NS_LBAs,
            LBA_Buffer => NS_LBA_Buffer,
            Metadata_Size => MS,
            Is_Extended => (NS_Identify.Formatted_LBA_Size and 16#10#) /= 0,
            Has_PI =>
               (NS_Identify.End_To_End_Data_Protection_Type_Settings and 7)
               /= 0,
            Namespace_Id => NS_Id,
            Queue => IO_Queue,
            Name  => new String'(Base_Name & Drive_Idx'Image &
               "n" & NS_Id'Image),
            others => <>);
      begin
         Memory.Physical.Free
            (size_t (To_Integer (NS_Identify.all'Address)));

         Caching.Init
            (NS.all'Address,
             NS_LBAs * Unsigned_64 (NS_LBA_Size / Caching.Sector_Data'Length),
             NS_Read'Access,
             NS_Write'Access,
             NS.Cache_Reg);

         Register (
            (Data => C5.To_Address (C5.Object_Pointer (NS)),
             Is_Block    => True,
             Block_Size  => NS_LBA_Size,
             Block_Count => NS_LBAs,
             Read        => Read'Access,
             Write       => Write'Access,
             Sync        => Sync'Access,
             Sync_Range  => Sync_Range'Access,
             IO_Control  => IO_Control'Access,
             IO_Argument => IO_Argument'Access,
             Mmap        => null,
             Poll        => null,
             Remove      => null), NS.Name.all, Success);
         if Success then
            Partitions.Parse_Partitions
               (NS.Name.all, Fetch (NS.Name.all), NS.Parts, Success);
         end if;
         if not Success then
            return;
         end if;
      end;
   exception
      when Constraint_Error =>
         Success := False;
   end Init_Namespace;

   function Setup_Admin_Queue
      (M : NVMe_Registers_Acc;
       Id : Natural;
       Depth : Admin_Queue_Depth) return Admin_Queue_Acc
   is
      SQ_Size : Storage_Count;
      SQ_Addr : Memory.Virtual_Address;

      CQ_Size : Storage_Count;
      CQ_Addr : Memory.Virtual_Address;

      Controller_Caps : NVMe_Register_Controller_Capabilities;
      Doorbell_Stride : Natural;
      SQ_Doorbell_Addr : Memory.Virtual_Address;
      CQ_Doorbell_Addr : Memory.Virtual_Address;
      SQ_Doorbell : NVMe_Doorbell_Acc;
      CQ_Doorbell : NVMe_Doorbell_Acc;
   begin
      SQ_Size := Storage_Count (A.Align_Up (
         (Admin_Submission_Queue_Entry'Size / 8) * Unsigned_64 (Depth),
          Memory.MMU.Page_Size));
      Memory.Physical.Alloc (size_t (SQ_Size), SQ_Addr);

      CQ_Size := Storage_Count (A.Align_Up (
         (Completion_Queue_Entry'Size / 8) * Unsigned_64 (Depth),
          Memory.MMU.Page_Size));
      Memory.Physical.Alloc (size_t (CQ_Size), CQ_Addr);

      Controller_Caps := M.Controller_Capabilities;
      Doorbell_Stride := 2 ** (Natural (Controller_Caps.Doorbell_Stride) + 2);

      SQ_Doorbell_Addr :=
         To_Integer (M.all'Address) + (NVMe_Registers'Size / 8) +
            Integer_Address ((2 * Id) * Doorbell_Stride);
      CQ_Doorbell_Addr :=
         To_Integer (M.all'Address) + (NVMe_Registers'Size / 8) +
            Integer_Address (((2 * Id) + 1) * Doorbell_Stride);

      SQ_Doorbell :=
         NVMe_Doorbell_Acc (C2.To_Pointer (To_Address (SQ_Doorbell_Addr)));
      CQ_Doorbell :=
         NVMe_Doorbell_Acc (C2.To_Pointer (To_Address (CQ_Doorbell_Addr)));

      declare
         SQ_Array : Admin_Submission_Queue_Entries (1 .. Depth)
            with Import, Address => To_Address (SQ_Addr);
         CQ_Array : Completion_Queue_Entries (1 .. Depth)
            with Import, Address => To_Address (CQ_Addr);
      begin
         SQ_Array := [others => (
            Opcode => Reserved,
            Namespace_Id => 0,
            others => <>
         )];
         CQ_Array := [others => (others => <>)];

         return new Admin_Queue'
            (Queue_Id => Id,
            Depth => Depth,
            Controller_Regs => M,
            SQ_Tail_Doorbell => SQ_Doorbell,
            CQ_Head_Doorbell => CQ_Doorbell,
            SQ_Addr => SQ_Addr,
            CQ_Addr => CQ_Addr,
            others => <>);
      end;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Failed to set up admin queue");
         return null;
   end Setup_Admin_Queue;

   function Setup_IO_Queue
      (M : NVMe_Registers_Acc;
       Id : Natural;
       Admin_Queue : Admin_Queue_Acc) return IO_Queue_Acc
   is
      Controller_Cap : NVMe_Register_Controller_Capabilities;
      Depth : Natural;

      Queue : IO_Queue_Acc;
      SQ_Size : Storage_Count;
      SQ_Addr : Memory.Virtual_Address;

      CQ_Size : Storage_Count;
      CQ_Addr : Memory.Virtual_Address;

      Doorbell_Stride : Natural;
      SQ_Doorbell_Addr : Memory.Virtual_Address;
      CQ_Doorbell_Addr : Memory.Virtual_Address;

      SQ_Cmd : Admin_Submission_Queue_Entry;
      CQ_Status : Completion_Queue_Entry;

      CQ_Cmd : Admin_Submission_Queue_Entry;
      SQ_Status : Completion_Queue_Entry;
   begin
      Controller_Cap := M.Controller_Capabilities;
      Depth := (
         if Controller_Cap.Maximum_Queue_Entries_Supported > 32 then
            32
         else
            Natural (Controller_Cap.Maximum_Queue_Entries_Supported)
      );

      SQ_Size := Storage_Count (A.Align_Up (
         (Admin_Submission_Queue_Entry'Size / 8) * Unsigned_64 (Depth),
            Memory.MMU.Page_Size));
      Memory.Physical.Alloc (size_t (SQ_Size), SQ_Addr);

      CQ_Size := Storage_Count (A.Align_Up (
         (Completion_Queue_Entry'Size / 8) * Unsigned_64 (Depth),
            Memory.MMU.Page_Size));
      Memory.Physical.Alloc (size_t (CQ_Size), CQ_Addr);

      Doorbell_Stride := 2 ** (Natural (Controller_Cap.Doorbell_Stride) + 2);
      SQ_Doorbell_Addr :=
         To_Integer (M.all'Address) + (NVMe_Registers'Size / 8) +
            Integer_Address ((2 * Id) * Doorbell_Stride);
      CQ_Doorbell_Addr :=
         To_Integer (M.all'Address) + (NVMe_Registers'Size / 8) +
            Integer_Address (((2 * Id) + 1) * Doorbell_Stride);

      if SQ_Size > Memory.MMU.Page_Size or CQ_Size > Memory.MMU.Page_Size then
         Panic.Hard_Panic ("Queue sizes over 1 page are unimplemented");
      end if;

      SQ_Cmd := (Opcode => Create_IO_SQ,
         Data_Pointer => [
            1 => Unsigned_64 (SQ_Addr - Memory.Memory_Offset),
            2 => 0
         ],
         SQ_Queue_Id => Unsigned_16 (Id),
         SQ_Queue_Size => IO_Queue_Size (Depth),
         SQ_Physically_Contiguous => True,
         SQ_Completion_Queue_Id => Unsigned_16 (Id),
         others => <>);

      CQ_Cmd := (Opcode => Create_IO_CQ,
         Data_Pointer => [
            1 => Unsigned_64 (CQ_Addr - Memory.Memory_Offset),
            2 => 0
         ],
         CQ_Queue_Id => Unsigned_16 (Id),
         CQ_Queue_Size => IO_Queue_Size (Depth),
         CQ_Physically_Contiguous => True,
         CQ_Interrupts_Enabled => True,
         CQ_Interrupt_Vector => 0,
         others => <>);

      declare
         SQ_Array : Admin_Submission_Queue_Entries (1 .. Depth)
            with Import, Address => To_Address (SQ_Addr);
         CQ_Array : Completion_Queue_Entries (1 .. Depth)
            with Import, Address => To_Address (CQ_Addr);

         SQ_Doorbell : constant NVMe_Doorbell_Acc :=
            NVMe_Doorbell_Acc (C2.To_Pointer (To_Address (SQ_Doorbell_Addr)));
         CQ_Doorbell : constant NVMe_Doorbell_Acc :=
            NVMe_Doorbell_Acc (C2.To_Pointer (To_Address (CQ_Doorbell_Addr)));
      begin
         SQ_Array := [others => (
            Opcode => Reserved,
            Namespace_Id => 0,
            others => <>
         )];
         CQ_Array := [others => (
            others => <>
         )];

         Queue := new IO_Queue'
            (Queue_Id => Id,
            Depth => Depth,
            Controller_Regs => M,
            SQ_Tail_Doorbell => SQ_Doorbell,
            CQ_Head_Doorbell => CQ_Doorbell,
            SQ_Addr => SQ_Addr,
            CQ_Addr => CQ_Addr,
            others => <>);
      end;

      CQ_Status := Submit_Admin_Command (Admin_Queue, CQ_Cmd);
      if CQ_Status.Dword3.Status /= 0 then
         Panic.Hard_Panic ("I/O CQ setup status != 0");
      end if;
      SQ_Status := Submit_Admin_Command (Admin_Queue, SQ_Cmd);
      if SQ_Status.Dword3.Status /= 0 then
         Panic.Hard_Panic ("I/O SQ setup status != 0");
      end if;

      return Queue;
   exception
      when Constraint_Error =>
         Messages.Put_Line ("Failed to setup I/O queue");
         return null;
   end Setup_IO_Queue;

   function Submit_Admin_Command
      (Queue : Admin_Queue_Acc;
       Command : in out Admin_Submission_Queue_Entry)
      return Completion_Queue_Entry
   is
      CQ_Result_Id : Natural;

      Command_Id : Unsigned_16;
   begin
      Command_Id := Unsigned_16 (Queue.Slot);
      Command.Command_Id := Command_Id;
      Queue.Slot := Queue.Slot + 1;

      declare
         SQ_Array : Admin_Submission_Queue_Entries (0 .. Queue.Depth - 1)
            with Import, Address => To_Address (Queue.SQ_Addr);
      begin
         SQ_Array (Natural (Queue.SQ_Tail)) := Command;
      end;

      if Natural (Queue.SQ_Tail) = Queue.Depth - 1 then
         Queue.SQ_Tail := 0;
      else
         Queue.SQ_Tail := Queue.SQ_Tail + 1;
      end if;

      Queue.SQ_Tail_Doorbell.all := (Doorbell => Queue.SQ_Tail, others => 0);

      declare
         CQ_Array : Completion_Queue_Entries (0 .. Queue.Depth - 1)
            with Import, Address => To_Address (Queue.CQ_Addr);
      begin
         loop
            loop
               exit when CQ_Array (Natural (Queue.CQ_Head)).Dword3.Phase_Tag =
                  Queue.CQ_Phase;
            end loop;

            Queue.Controller_Regs.Interrupt_Mask_Set := 1;

            if
               CQ_Array (Natural (Queue.CQ_Head)).Dword3.Command_Id
               = Command_Id
            then
               CQ_Result_Id := Natural (Queue.CQ_Head);

               if Natural (Queue.CQ_Head) = Queue.Depth - 1 then
                  Queue.CQ_Head := 0;
                  Queue.CQ_Phase := not Queue.CQ_Phase;
               else
                  Queue.CQ_Head := Queue.CQ_Head + 1;
               end if;

               Queue.CQ_Head_Doorbell.all :=
                  (Doorbell => Queue.CQ_Head, others => 0);

               Queue.Controller_Regs.Interrupt_Mask_Clear := 1;

               return CQ_Array (CQ_Result_Id);
            end if;
         end loop;
      end;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("failed to submit admin command");
   end Submit_Admin_Command;

   function Submit_IO_Command
      (Queue : IO_Queue_Acc;
       Command : in out IO_Submission_Queue_Entry)
      return Completion_Queue_Entry
   is
      CQ_Result_Id : Natural;
      Result : Completion_Queue_Entry;

      Command_Id : Unsigned_16;
   begin
      Synchronization.Seize (Queue.Mutex);
      Command_Id := Unsigned_16 (Queue.Slot);
      Command.Command_Id := Command_Id;
      Queue.Slot := Queue.Slot + 1;

      declare
         SQ_Array : IO_Submission_Queue_Entries (0 .. Queue.Depth - 1)
            with Import, Address => To_Address (Queue.SQ_Addr);
      begin
         SQ_Array (Natural (Queue.SQ_Tail)) := Command;
      end;

      if Natural (Queue.SQ_Tail) = Queue.Depth - 1 then
         Queue.SQ_Tail := 0;
      else
         Queue.SQ_Tail := Queue.SQ_Tail + 1;
      end if;

      Queue.SQ_Tail_Doorbell.all := (Doorbell => Queue.SQ_Tail, others => 0);

      declare
         CQ_Array : Completion_Queue_Entries (0 .. Queue.Depth - 1)
            with Import, Address => To_Address (Queue.CQ_Addr);
      begin
         loop
            loop
               exit when CQ_Array (Natural (Queue.CQ_Head)).Dword3.Phase_Tag =
                  Queue.CQ_Phase;
            end loop;

            Queue.Controller_Regs.Interrupt_Mask_Set := 1;

            if
               CQ_Array (Natural (Queue.CQ_Head)).Dword3.Command_Id
               = Command_Id
            then
               CQ_Result_Id := Natural (Queue.CQ_Head);

               if Natural (Queue.CQ_Head) = Queue.Depth - 1 then
                  Queue.CQ_Head := 0;
                  Queue.CQ_Phase := not Queue.CQ_Phase;
               else
                  Queue.CQ_Head := Queue.CQ_Head + 1;
               end if;

               Queue.CQ_Head_Doorbell.all :=
                  (Doorbell => Queue.CQ_Head, others => 0);

               Queue.Controller_Regs.Interrupt_Mask_Clear := 1;

               Result := CQ_Array (CQ_Result_Id);
               Synchronization.Release (Queue.Mutex);
               return Result;
            end if;
         end loop;
      end;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("failed to submit I/O command");
   end Submit_IO_Command;

   function Controller_Identify
      (Q : Admin_Queue_Acc) return Identify_Controller_Data_Acc
   is
      Data_Addr : Memory.Virtual_Address;
      Data : Identify_Controller_Data_Acc;

      Cmd : Admin_Submission_Queue_Entry;
      Reply : Completion_Queue_Entry;
   begin
      Memory.Physical.Alloc (Memory.MMU.Page_Size, Data_Addr);
      Data := Identify_Controller_Data_Acc (C3.To_Pointer
            (To_Address (Data_Addr)));

      Cmd := (Opcode => Identify,
         Data_Pointer => [
            1 => Unsigned_64 (Data_Addr - Memory.Memory_Offset),
            2 => 0],
         Identify_CNS => Identify_Controller,
         others => <>);

      Reply := Submit_Admin_Command (Q, Cmd);

      if Reply.Dword3.Status /= 0 then
         Memory.Physical.Free (size_t (Data_Addr));
         return null;
      end if;

      return Data;
   exception
      when Constraint_Error =>
         return null;
   end Controller_Identify;

   function Namespace_Identify
      (Q : Admin_Queue_Acc;
       Namespace : NVMe_NS_Id) return Identify_Namespace_Data_Acc
   is
      Data_Addr : Memory.Virtual_Address;
      Data : Identify_Namespace_Data_Acc;

      Cmd : Admin_Submission_Queue_Entry;
      Reply : Completion_Queue_Entry;
   begin
      Memory.Physical.Alloc (Memory.MMU.Page_Size, Data_Addr);
      Data := Identify_Namespace_Data_Acc
            (C4.To_Pointer (To_Address (Data_Addr)));

      Cmd := (Opcode => Identify,
         Namespace_Id => Namespace,
         Data_Pointer => [
            1 => Unsigned_64 (Data_Addr - Memory.Memory_Offset),
            2 => 0],
         Identify_CNS => Identify_Namespace,
         others => <>);

      Reply := Submit_Admin_Command (Q, Cmd);

      if Reply.Dword3.Status /= 0 then
         Memory.Physical.Free (size_t (Data_Addr));
         return null;
      end if;

      return Data;
   exception
      when Constraint_Error =>
         return null;
   end Namespace_Identify;

   function Active_Namespaces
      (Q : Admin_Queue_Acc) return Active_Namespace_Id_List_Acc
   is
      Data_Addr : Memory.Virtual_Address;
      Data : Active_Namespace_Id_List_Acc;

      Cmd : Admin_Submission_Queue_Entry;
      Reply : Completion_Queue_Entry;
   begin
      Memory.Physical.Alloc (Memory.MMU.Page_Size, Data_Addr);
      Data := Active_Namespace_Id_List_Acc (C6.To_Pointer
            (To_Address (Data_Addr)));

      Cmd := (Opcode => Identify,
         Data_Pointer => [
            1 => Unsigned_64 (Data_Addr - Memory.Memory_Offset),
            2 => 0],
         Identify_CNS => Active_Namespaces,
         others => <>);

      Reply := Submit_Admin_Command (Q, Cmd);

      if Reply.Dword3.Status /= 0 then
         Memory.Physical.Free (size_t (Data_Addr));
         return null;
      end if;

      return Data;
   exception
      when Constraint_Error =>
         return null;
   end Active_Namespaces;

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Is_Blocking);
      D : constant Namespace_Data_Acc :=
         Namespace_Data_Acc (C5.To_Pointer (Key));
   begin
      Caching.Read
         (Registry  => D.Cache_Reg,
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
      D : constant Namespace_Data_Acc :=
         Namespace_Data_Acc (C5.To_Pointer (Key));
   begin
      Caching.Write
         (Registry  => D.Cache_Reg,
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
      Drive   : constant Namespace_Data_Acc
         := Namespace_Data_Acc (C5.To_Pointer (Key));
   begin
      Caching.Sync (Drive.Cache_Reg, Success);
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
      Drive   : constant Namespace_Data_Acc
         := Namespace_Data_Acc (C5.To_Pointer (Key));
   begin
      Caching.Sync_Range (Drive.Cache_Reg, Offset, Count, Success);
   exception
      when Constraint_Error =>
         Success := False;
   end Sync_Range;

   BLKRRPART : constant := 16#9825#;

   procedure IO_Control
      (Key      : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address;
       Extra    : out Unsigned_64;
       Success  : out Boolean)
   is
      pragma Unreferenced (Argument);
      Drive : constant Namespace_Data_Acc :=
         Namespace_Data_Acc (C5.To_Pointer (Key));
   begin
      Extra := 0;
      if Request = BLKRRPART then
         for Part of Drive.Parts loop
            exit when Part = Devices.Error_Handle;
            Devices.Remove (Part, Success);
            if not Success then
               return;
            end if;
         end loop;
         Partitions.Parse_Partitions
            (Drive.Name.all, Fetch (Drive.Name.all), Drive.Parts, Success);
      else
         Success := False;
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end IO_Control;

   procedure IO_Argument
      (Key     : System.Address;
       Request : Unsigned_64;
       Usage   : out IO_Usage;
       Size    : out Natural)
   is
      pragma Unreferenced (Key);
   begin
      Usage := (if Request = BLKRRPART then IO_No_Memory else IO_Unknown);
      Size  := 0;
   end IO_Argument;

   procedure NS_Read
      (Drive : System.Address;
       LBA : Unsigned_64;
       Data_Buffer : out Caching.Sector_Data;
       Success : out Boolean)
   is
      D : constant Namespace_Data_Acc
         := Namespace_Data_Acc (C5.To_Pointer (Drive));
      Sectors : Unsigned_64;
      Offset : Natural;
   begin
      Synchronization.Seize (D.Mutex);
      Sectors := Unsigned_64 (D.LBA_Size / Data_Buffer'Length);
      Offset := Natural (LBA mod Sectors) * Data_Buffer'Length;
      declare
         LBA_Data : Operation_Data (1 .. D.LBA_Size)
            with Import, Address => To_Address (D.LBA_Buffer);
      begin
         Success := NS_Transfer (D, LBA / Sectors, False);
         if Success then
            Data_Buffer :=
               LBA_Data (Offset + 1 .. Offset + Data_Buffer'Length);
         end if;
      end;
      Synchronization.Release (D.Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (D.Mutex);
         Success := False;
   end NS_Read;

   procedure NS_Write
      (Drive : System.Address;
       LBA : Unsigned_64;
       Data_Buffer : Caching.Sector_Data;
       Success : out Boolean)
   is
      D : constant Namespace_Data_Acc
         := Namespace_Data_Acc (C5.To_Pointer (Drive));
      Sectors : Unsigned_64;
      Offset : Natural;
   begin
      Synchronization.Seize (D.Mutex);
      Sectors := Unsigned_64 (D.LBA_Size / Data_Buffer'Length);
      Offset := Natural (LBA mod Sectors) * Data_Buffer'Length;
      declare
         LBA_Data : Operation_Data (1 .. D.LBA_Size)
            with Import, Address => To_Address (D.LBA_Buffer);
      begin
         --  Writes carry the whole LBA and its metadata, so what is not being
         --  changed has to be read first.
         Success := (Sectors = 1 and D.Metadata_Size = 0) or else
                    NS_Transfer (D, LBA / Sectors, False);
         if Success then
            LBA_Data (Offset + 1 .. Offset + Data_Buffer'Length) :=
               Data_Buffer;
            Success := NS_Transfer (D, LBA / Sectors, True);
         end if;
      end;
      Synchronization.Release (D.Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (D.Mutex);
         Success := False;
   end NS_Write;

   function NS_Transfer
      (D        : Namespace_Data_Acc;
       LBA      : Unsigned_64;
       Is_Write : Boolean) return Boolean
   is
      Data_Addr : Unsigned_64;
      Data_Pointer : Submission_Queue_Entry_Data_Pointer;
      Metadata_Addr : Unsigned_64 := 0;
      Cmd : IO_Submission_Queue_Entry;
      Reply : Completion_Queue_Entry;
   begin
      Data_Addr := Unsigned_64 (D.LBA_Buffer) - Memory.Memory_Offset;
      Data_Pointer := [1 => Data_Addr, 2 => 0];
      if D.Is_Extended then
         if D.LBA_Size + D.Metadata_Size > Memory.MMU.Page_Size then
            Data_Pointer (2) := Data_Addr + Memory.MMU.Page_Size;
         end if;
      elsif D.Metadata_Size /= 0 then
         Metadata_Addr := Data_Addr + Unsigned_64 (D.LBA_Size);
      end if;

      if Is_Write then
         --  The controller generates the protection information of the new
         --  data, and Type 1 checks its reference tag against the LBA.
         Cmd := (Opcode => Write,
            Namespace_Id => D.Namespace_Id,
            Metadata_Pointer => Metadata_Addr,
            Data_Pointer => Data_Pointer,
            Write_Starting_LBA => LBA,
            Write_Number_Of_LBAs => 0,
            Write_Protection_Information => (if D.Has_PI then 2#1000# else 0),
            Write_Expected_Initial_Logical_Block_Reference_Tag =>
               (if D.Has_PI then Unsigned_32 (LBA and 16#FFFF_FFFF#) else 0),
            others => <>);
      else
         Cmd := (Opcode => Read,
            Namespace_Id => D.Namespace_Id,
            Metadata_Pointer => Metadata_Addr,
            Data_Pointer => Data_Pointer,
            Read_Starting_LBA => LBA,
            Read_Number_Of_LBAs => 0,
            others => <>);
      end if;

      Reply := Submit_IO_Command (D.Queue, Cmd);
      return Reply.Dword3.Status = 0;
   exception
      when Constraint_Error =>
         return False;
   end NS_Transfer;

   procedure Controller_Await_Ready
      (M : NVMe_Registers_Acc;
       Enabled : Boolean)
   is
   begin
      loop
         exit when M.Controller_Status.Ready = Enabled;
      end loop;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Failed to await controller ready");
   end Controller_Await_Ready;

   procedure Disable_Controller (M : NVMe_Registers_Acc) is
      Config : NVMe_Register_Controller_Configuration;
   begin
      Config := M.Controller_Configuration;
      Config.Enable := False;
      M.Controller_Configuration := Config;

      Controller_Await_Ready (M, False);
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Failed to disable controller");
   end Disable_Controller;

   procedure Enable_Controller (M : NVMe_Registers_Acc) is
      Config : NVMe_Register_Controller_Configuration;
   begin
      Config := M.Controller_Configuration;
      Config.Enable := True;
      Config.IO_Completion_Queue_Entry_Size := 4;
      Config.IO_Submission_Queue_Entry_Size := 6;
      Config.Arbitration_Mechanism_Selected := 0;
      Config.IO_Command_Set_Selected := 0;

      declare
         MPS_Temp : Unsigned_32 := Memory.MMU.Page_Size / 16#1000#;
         MPS : Unsigned_4 := 0;
      begin
         loop
            exit when MPS_Temp = 1;
            MPS_Temp := Shift_Right (MPS_Temp, 1);
            MPS := MPS + 1;
         end loop;

         Config.Memory_Page_Size := MPS;
      end;

      M.Controller_Configuration := Config;

      Controller_Await_Ready (M, True);
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Failed to enable controller");
   end Enable_Controller;
end Devices.PCI.NVMe;
