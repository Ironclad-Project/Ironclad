--  arch-acpi.adb: ACPI driver.
--  Copyright (C) 2025 streaksu
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

with Arch.Limine;
with Arch.Clocks;
with Arch.Snippets;
with Arch.MMU;
with Arch.PCI;
with Arch.Local;
with Lib.Alignment;
with Lib.Time;
with Lib.Synchronization;
with Lib.Messages;
with Lib;
with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Memory.Physical;
with Scheduler;

package body Arch.ACPI with SPARK_Mode => Off is
   --  Request to get the RSDP.
   --  Response is a pointer to an RSDP_Response.
   RSDP_Request : Limine.Request :=
      (ID => (Limine.Limine_Common_Magic_1, Limine.Limine_Common_Magic_2,
              16#c5e77b6b397e7b43#, 16#27637845accdcf3c#),
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   type Buffer     is array (1 .. 4096) of Unsigned_8;
   type Buffer_Acc is access Buffer;

   procedure Free is new Ada.Unchecked_Deallocation (Buffer, Buffer_Acc);

   Early_Buffer : Buffer_Acc := null;
   Is_Init      : Boolean    := False;

   function Is_Supported return Boolean is
   begin
      return True;
   end Is_Supported;

   procedure Initialize (Success : out Boolean) is
   begin
      if not Is_Init then
         if Initialize (0) /= Status_OK then
            Success := False;
            return;
         end if;

         if Early_Buffer /= null then
            Free (Early_Buffer);
         end if;

         Is_Init := True;
      end if;

      if Namespace_Load /= Status_OK then
         Success := False;
         return;
      end if;
      if Namespace_Init /= Status_OK then
         Success := False;
         return;
      end if;

      Success := True;
   end Initialize;
   ----------------------------------------------------------------------------
   function FindTable (Signature : SDT_Signature) return Virtual_Address is
      Sig : String (1 .. Signature'Length + 1) :=
         Signature & (1 => Ada.Characters.Latin_1.NUL);
      Rec : Table_Record;
   begin
      if not Is_Init and Early_Buffer = null then
         Early_Buffer := new Buffer'(others => 0);

         if Setup_Early_Table_Access
            (Early_Buffer.all'Address, Early_Buffer'Length) /= Status_OK
         then
            return 0;
         end if;
      end if;

      if Find_Table_By_Signature (Sig'Address, Rec'Address) = Status_OK then
         return To_Integer (Rec.Virt_Addr);
      else
         return 0;
      end if;
   end FindTable;
   ----------------------------------------------------------------------------
   procedure Enter_Sleep (Level : Sleep_Level; Success : out Boolean) is
      Translated : Sleep_State;
   begin
      case Level is
         when S0 => Translated := Sleep_S0;
         when S1 => Translated := Sleep_S1;
         when S2 => Translated := Sleep_S2;
         when S3 => Translated := Sleep_S3;
         when S4 => Translated := Sleep_S4;
         when S5 => Translated := Sleep_S5;
      end case;

      if Prepare_For_Sleep (Translated) = Status_OK then
         if Enter_Sleep (Translated) = Status_OK then
            Success := True;
            return;
         end if;
      end if;

      Success := False;
   end Enter_Sleep;

   procedure Do_Reboot is
      Discard : Status;
   begin
      Discard := Reboot;
   end Do_Reboot;
   ----------------------------------------------------------------------------
   function Get_RSDP (Addr : access Unsigned_64) return Status is
      RSDPonse : Limine.RSDP_Response
         with Import, Address => RSDP_Request.Response;
   begin
      Addr.all := Unsigned_64 (To_Integer (RSDPonse.Addr));
      return Status_OK;
   end Get_RSDP;

   procedure Stall (USec : Unsigned_8) is
      Curr_Sec, Curr_Nsec, Tgt_Sec, Tgt_Nsec : Unsigned_64;
   begin
      Arch.Clocks.Get_Monotonic_Time (Tgt_Sec, Tgt_Nsec);
      Lib.Time.Increment (Tgt_Sec, Tgt_Nsec, 0, Unsigned_64 (USec) * 1000);
      loop
         Arch.Clocks.Get_Monotonic_Time (Curr_Sec, Curr_Nsec);
         exit when Lib.Time.Is_Greater_Equal
            (Tgt_Sec, Tgt_Nsec, Curr_Sec, Curr_Nsec);
      end loop;
   end Stall;

   procedure Sleep (MSec : Unsigned_64) is
      Curr_Sec, Curr_Nsec, Tgt_Sec, Tgt_Nsec : Unsigned_64;
   begin
      Arch.Clocks.Get_Monotonic_Time (Tgt_Sec, Tgt_Nsec);
      Lib.Time.Increment
         (Tgt_Sec, Tgt_Nsec, MSec / 1000, (MSec mod 1000) * 1000000);
      loop
         Arch.Clocks.Get_Monotonic_Time (Curr_Sec, Curr_Nsec);
         exit when Lib.Time.Is_Greater_Equal
            (Tgt_Sec, Tgt_Nsec, Curr_Sec, Curr_Nsec);
         Scheduler.Yield_If_Able;
      end loop;
   end Sleep;

   function Create_Event return System.Address is
   begin
      return System'To_Address (3);
   end Create_Event;

   procedure Free_Event (Handle : System.Address) is
      pragma Unreferenced (Handle);
   begin
      null;
   end Free_Event;

   type uACPI_Spinlock is record
      Lock : aliased Lib.Synchronization.Binary_Semaphore;
   end record;
   type uACPI_Spinlock_Acc is access all uACPI_Spinlock;
   package C1 is new System.Address_To_Access_Conversions (uACPI_Spinlock);

   function Create_Spinlock return System.Address is
      Lock : constant uACPI_Spinlock_Acc :=
         new uACPI_Spinlock'(Lock => Lib.Synchronization.Unlocked_Semaphore);
   begin
      return C1.To_Address (C1.Object_Pointer (Lock));
   end Create_Spinlock;

   procedure Free_Spinlock (Handle : System.Address) is
      procedure Free is new Ada.Unchecked_Deallocation
         (uACPI_Spinlock, uACPI_Spinlock_Acc);
      Lock : uACPI_Spinlock_Acc := uACPI_Spinlock_Acc (C1.To_Pointer (Handle));
   begin
      Free (Lock);
   end Free_Spinlock;

   function Lock_Spinlock (Handle : System.Address) return Unsigned_64 is
      Lock : constant uACPI_Spinlock_Acc :=
         uACPI_Spinlock_Acc (C1.To_Pointer (Handle));
   begin
      Lib.Synchronization.Seize (Lock.Lock);
      return 0;
   end Lock_Spinlock;

   procedure Unlock_Spinlock (Handle : System.Address; Flags : Unsigned_64) is
      pragma Unreferenced (Flags);
      Lock : constant uACPI_Spinlock_Acc :=
         uACPI_Spinlock_Acc (C1.To_Pointer (Handle));
   begin
      Lib.Synchronization.Release (Lock.Lock);
   end Unlock_Spinlock;

   function Alloc (Size : size_t) return System.Address is
   begin
      return To_Address (Memory.Physical.Alloc (Size));
   end Alloc;

   procedure Free (Ptr : System.Address) is
   begin
      Memory.Physical.Free (size_t (To_Integer (Ptr)));
   end Free;

   type uACPI_PCI is record
      Dev : Arch.PCI.PCI_Device;
   end record;
   type uACPI_PCI_Acc is access all uACPI_PCI;
   package C3 is new System.Address_To_Access_Conversions (uACPI_PCI);

   function PCI_Device_Open
      (Address : PCI_Address;
       Handle  : out System.Address) return Status
   is
      Ret     : constant uACPI_PCI_Acc := new uACPI_PCI;
      Success : Boolean;
   begin
      Arch.PCI.Search_Device
         (Bus     => Address.Bus,
          Slot    => Address.Device,
          Func    => Address.Func,
          Result  => Ret.Dev,
          Success => Success);
      if Success then
         Handle := C3.To_Address (C3.Object_Pointer (Ret));
         return Status_OK;
      else
         Handle := System.Null_Address;
         return Status_Not_Found;
      end if;
   end PCI_Device_Open;

   procedure PCI_Device_Close (Handle : System.Address) is
      procedure Free is new Ada.Unchecked_Deallocation
         (uACPI_PCI, uACPI_PCI_Acc);
      Dev : uACPI_PCI_Acc := uACPI_PCI_Acc (C3.To_Pointer (Handle));
   begin
      Free (Dev);
   end PCI_Device_Close;

   function PCI_Read8
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : out Unsigned_8) return Status
   is
      Dev : constant uACPI_PCI_Acc := uACPI_PCI_Acc (C3.To_Pointer (Handle));
   begin
      Value := Arch.PCI.Read8 (Dev.Dev, Unsigned_16 (Offset));
      return Status_OK;
   end PCI_Read8;

   function PCI_Write8
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : Unsigned_8) return Status
   is
      Dev : constant uACPI_PCI_Acc := uACPI_PCI_Acc (C3.To_Pointer (Handle));
   begin
      Arch.PCI.Write8 (Dev.Dev, Unsigned_16 (Offset), Value);
      return Status_OK;
   end PCI_Write8;

   function PCI_Read16
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : out Unsigned_16) return Status
   is
      Dev : constant uACPI_PCI_Acc := uACPI_PCI_Acc (C3.To_Pointer (Handle));
   begin
      Value := Arch.PCI.Read16 (Dev.Dev, Unsigned_16 (Offset));
      return Status_OK;
   end PCI_Read16;

   function PCI_Write16
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : Unsigned_16) return Status
   is
      Dev : constant uACPI_PCI_Acc := uACPI_PCI_Acc (C3.To_Pointer (Handle));
   begin
      Arch.PCI.Write16 (Dev.Dev, Unsigned_16 (Offset), Value);
      return Status_OK;
   end PCI_Write16;

   function PCI_Read32
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : out Unsigned_32) return Status
   is
      Dev : constant uACPI_PCI_Acc := uACPI_PCI_Acc (C3.To_Pointer (Handle));
   begin
      Value := Arch.PCI.Read32 (Dev.Dev, Unsigned_16 (Offset));
      return Status_OK;
   end PCI_Read32;

   function PCI_Write32
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : Unsigned_32) return Status
   is
      Dev : constant uACPI_PCI_Acc := uACPI_PCI_Acc (C3.To_Pointer (Handle));
   begin
      Arch.PCI.Write32 (Dev.Dev, Unsigned_16 (Offset), Value);
      return Status_OK;
   end PCI_Write32;

   function IO_Map
      (Base   : Unsigned_64;
       Len    : size_t;
       Handle : out System.Address) return Status
   is
      pragma Unreferenced (Len);
   begin
      Handle := To_Address (Integer_Address (Base));
      return Status_OK;
   end IO_Map;

   procedure IO_Unmap (Handle : System.Address) is
      pragma Unreferenced (Handle);
   begin
      null;
   end IO_Unmap;

   function IO_Read8
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : out Unsigned_8) return Status
   is
      Base : constant Unsigned_16 := Unsigned_16 (To_Integer (Handle));
      Port : constant Unsigned_16 := Base + Unsigned_16 (Offset);
   begin
      Value := Arch.Snippets.Port_In (Port);
      return Status_OK;
   end IO_Read8;

   function IO_Write8
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : Unsigned_8) return Status
   is
      Base : constant Unsigned_16 := Unsigned_16 (To_Integer (Handle));
      Port : constant Unsigned_16 := Base + Unsigned_16 (Offset);
   begin
      Arch.Snippets.Port_Out (Port, Value);
      return Status_OK;
   end IO_Write8;

   function IO_Read16
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : out Unsigned_16) return Status
   is
      Base : constant Unsigned_16 := Unsigned_16 (To_Integer (Handle));
      Port : constant Unsigned_16 := Base + Unsigned_16 (Offset);
   begin
      Value := Arch.Snippets.Port_In16 (Port);
      return Status_OK;
   end IO_Read16;

   function IO_Write16
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : Unsigned_16) return Status
   is
      Base : constant Unsigned_16 := Unsigned_16 (To_Integer (Handle));
      Port : constant Unsigned_16 := Base + Unsigned_16 (Offset);
   begin
      Arch.Snippets.Port_Out16 (Port, Value);
      return Status_OK;
   end IO_Write16;

   function IO_Read32
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : out Unsigned_32) return Status
   is
      Base : constant Unsigned_16 := Unsigned_16 (To_Integer (Handle));
      Port : constant Unsigned_16 := Base + Unsigned_16 (Offset);
   begin
      Value := Arch.Snippets.Port_In32 (Port);
      return Status_OK;
   end IO_Read32;

   function IO_Write32
      (Handle     : System.Address;
       Offset     : size_t;
       Value      : Unsigned_32) return Status
   is
      Base : constant Unsigned_16 := Unsigned_16 (To_Integer (Handle));
      Port : constant Unsigned_16 := Base + Unsigned_16 (Offset);
   begin
      Arch.Snippets.Port_Out32 (Port, Value);
      return Status_OK;
   end IO_Write32;

   package A2 is new Lib.Alignment (Integer_Address);

   function Map
      (Phys_Addr : Unsigned_64;
       Length    : size_t) return System.Address
   is
      Success : Boolean;
      Start   : constant Integer_Address :=
         A2.Align_Down (Integer_Address (Phys_Addr), Arch.MMU.Page_Size);
      Top     : constant Integer_Address :=
         A2.Align_Up (Integer_Address (Phys_Addr) + Integer_Address (Length),
                      Arch.MMU.Page_Size);
      Len     : constant Integer_Address := Top - Start;
   begin
      Arch.MMU.Map_Range
         (Map            => Arch.MMU.Kernel_Table,
          Physical_Start => To_Address (Start),
          Virtual_Start  => To_Address (Memory.Memory_Offset + Start),
          Length         => Storage_Count (Len),
          Permissions    =>
            (Is_User_Accesible => False,
             Can_Read          => True,
             Can_Write         => True,
             Can_Execute       => False,
             Is_Global         => True),
          Success        => Success);
      return To_Address (Memory.Memory_Offset + Integer_Address (Phys_Addr));
   end Map;

   procedure Unmap (Address : System.Address; Length : size_t) is
      pragma Unreferenced (Address, Length);
   begin
      --  We implement Map as a map to the Higher Half Direct Map, where
      --  we perform all our other accesses to miscellaneous physical memory.
      --
      --  We might want to keep using these addresses in the future, like
      --  reading ACPI tables ourselves, and having uACPI unmap these will make
      --  this confusing, therefore implement Unmap as a no-op.
      null;
   end Unmap;

   function Wait_For_Work_Completion return Status is
   begin
      return Status_Unimplemented;
   end Wait_For_Work_Completion;

   function Install_Interrupt_Handler
      (IRQ     : Unsigned_32;
       Handler : System.Address;
       Context : System.Address;
       Handle  : out System.Address) return Status
   is
      pragma Unreferenced (IRQ, Handler, Context);
   begin
      Handle := System.Null_Address;
      return Status_OK;
   end Install_Interrupt_Handler;

   function Uninstall_Interrupt_Handler
      (Handler : System.Address;
       Handle  : System.Address) return Status
   is
      pragma Unreferenced (Handler, Handle);
   begin
      return Status_OK;
   end Uninstall_Interrupt_Handler;

   function Schedule_Work
      (Work_Type    : int;
       Work_Handler : System.Address;
       Context      : System.Address) return Status
   is
      pragma Unreferenced (Work_Type, Work_Handler, Context);
   begin
      return Status_Unimplemented;
   end Schedule_Work;

   function Handle_Firmware_Request (Request : System.Address) return Status is
      pragma Unreferenced (Request);
   begin
      return Status_Unimplemented;
   end Handle_Firmware_Request;

   procedure Signal_Event (Event : System.Address) is
   begin
      null;
   end Signal_Event;

   procedure Reset_Event (Event : System.Address) is
   begin
      null;
   end Reset_Event;

   function Wait_For_Event
      (Handle  : System.Address;
       Timeout : Unsigned_16) return int
   is
      pragma Unreferenced (Handle, Timeout);
   begin
      return 0;
   end Wait_For_Event;

   function Get_Nanoseconds_Since_Boot return Unsigned_64 is
      Curr_Sec, Curr_Nsec : Unsigned_64;
   begin
      Arch.Clocks.Get_Monotonic_Time (Curr_Sec, Curr_Nsec);
      return (Curr_Sec * 1_000_000_000) + Curr_Nsec;
   end Get_Nanoseconds_Since_Boot;

   type uACPI_Mutex is record
      Lock : aliased Lib.Synchronization.Mutex;
   end record;
   type uACPI_Mutex_Acc is access all uACPI_Mutex;
   package C2 is new System.Address_To_Access_Conversions (uACPI_Mutex);

   function Create_Mutex return System.Address is
      Lock : constant uACPI_Mutex_Acc :=
         new uACPI_Mutex'(Lock => Lib.Synchronization.Unlocked_Mutex);
   begin
      return C2.To_Address (C2.Object_Pointer (Lock));
   end Create_Mutex;

   procedure Free_Mutex (Handle : System.Address) is
      procedure Free is new Ada.Unchecked_Deallocation
         (uACPI_Mutex, uACPI_Mutex_Acc);
      Lock : uACPI_Mutex_Acc := uACPI_Mutex_Acc (C2.To_Pointer (Handle));
   begin
      Free (Lock);
   end Free_Mutex;

   function Acquire_Mutex
      (Handle  : System.Address;
       Timeout : Unsigned_16) return Status
   is
      pragma Unreferenced (Timeout);
      Lock : constant uACPI_Mutex_Acc :=
         uACPI_Mutex_Acc (C2.To_Pointer (Handle));
   begin
      Lib.Synchronization.Seize (Lock.Lock);
      return Status_OK;
   end Acquire_Mutex;

   procedure Release_Mutex (Handle : System.Address) is
      Lock : constant uACPI_Mutex_Acc :=
         uACPI_Mutex_Acc (C2.To_Pointer (Handle));
   begin
      Lib.Synchronization.Release (Lock.Lock);
   end Release_Mutex;

   function Get_Thread_ID return Unsigned_64 is
   begin
      return Unsigned_64 (Scheduler.Convert (Arch.Local.Get_Current_Thread));
   end Get_Thread_ID;

   procedure Kernel_Log (Level : int; Str_Addr : System.Address) is
      pragma Unreferenced (Level);
      Len : constant Natural := Lib.C_String_Length (Str_Addr);
      Str : String (1 .. Len - 1) with Import, Address => Str_Addr;
   begin
      Lib.Messages.Put_Line (Str);
   end Kernel_Log;
end Arch.ACPI;
