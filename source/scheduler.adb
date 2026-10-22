--  scheduler.adb: Thread scheduler.
--  Copyright (C) 2024 streaksu
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

with System; use System;
with Ada.Unchecked_Deallocation;
with Synchronization;
with Panic;
with System.Storage_Elements; use System.Storage_Elements;
with Userland.Process;
with Arch;
with Arch.Local;
with Arch.Hooks;
with Arch.Clocks;
with Arch.Snippets;
with Arch.MMU;
with Cryptography.Random;
with Memory.Userland_Transfer;

package body Scheduler with SPARK_Mode => Off is
   Fast_Reschedule_Micros : constant := 10_000;
   Idle_Reschedule_Micros : constant := 100_000;  --  See Next_Wake.
   Removed_Wait_Seconds   : constant := 2;  --  See Wait_For_Removed.
   Removed_Poll_Micros    : constant := 1_000;

   --  How soon a core retries leaving or letting go of a thread that has gone
   --  when the lock is busy.
   Retire_Retry_Micros : constant := 100;

   type Thread_Stack is array (Natural range <>) of Unsigned_8;
   type Thread_Stack_64 is array (Natural range <>) of Unsigned_64;
   --  A stack's top is the address one past its last byte, and the psABIs
   --  (x86-64 1.0, RISC-V 1.0) want it 16-byte aligned.
   type Kernel_Stack is array (1 ..  Memory.Kernel_Stack_Size) of Unsigned_8
      with Alignment => 16;
   type Kernel_Stack_Acc is access Kernel_Stack;
   procedure Free_Kernel_Stack is new Ada.Unchecked_Deallocation
      (Kernel_Stack, Kernel_Stack_Acc);

   type Thread_Info is record
      Is_Present      : Boolean with Atomic;
      Is_Running      : Boolean with Atomic;
      Is_Doomed       : Boolean with Atomic;
      Is_Held         : Boolean with Atomic;  --  See Create_User_Thread.
      Path            : String (1 .. 20);
      Path_Len        : Natural range 0 .. 20;
      Pol             : Policy;
      Nice            : Niceness;
      Prio            : Priority;
      RR_Micro_Inter  : Natural;
      TCB_Pointer     : System.Address;
      PageMap         : System.Address;
      Kernel_Stack    : Kernel_Stack_Acc;
      GP_State        : Arch.Context.GP_Context;
      FP_State        : Arch.Context.FP_Context;
      Syscall_State   : Arch.Context.GP_Context;
      Process         : Userland.Process.PID;
      System_Runtime  : Time.Timestamp;
      User_Runtime    : Time.Timestamp;
      System_Tmp      : Time.Timestamp;
      User_Tmp        : Time.Timestamp;
      User_Stack      : System.Address;
      User_Stack_Size : Unsigned_64;
      User_Stack_Used : Boolean;
      Is_Disabled     : Boolean;
      Start_Clock     : Time.Clock_Type;
      Start_Time      : Time.Timestamp;
      Wake_Pending    : Boolean;  --  See Clear_Wake.
      Event_Waiting   : Boolean;  --  Sleeping in Wait_Event.
      Core            : Natural;  --  Core it runs on, see Kick.
      Masked_Signals  : Unsigned_64;
      Saved_Mask      : Unsigned_64;
      Has_Saved_Mask  : Boolean;
   end record;
   type Thread_Info_Arr     is array (TID range 1 .. TID'Last) of Thread_Info;
   type Thread_Info_Arr_Acc is access Thread_Info_Arr;

   Thread_Pool     : Thread_Info_Arr_Acc;
   Scheduler_Mutex : aliased Synchronization.Binary_Semaphore :=
      Synchronization.Unlocked_Semaphore;

   --  Keys each thread waits on. Wait_Mutex must be taken before
   --  Scheduler_Mutex, never while holding it.
   Max_Wait_Keys : constant := 64;
   type Wait_Key_Arr is array (1 .. Max_Wait_Keys) of System.Address;
   type Wait_Info is record
      Count : Natural range 0 .. Max_Wait_Keys;
      Keys  : Wait_Key_Arr;
   end record;
   type Wait_Info_Arr     is array (TID range 1 .. TID'Last) of Wait_Info;
   type Wait_Info_Arr_Acc is access Wait_Info_Arr;

   Waits      : Wait_Info_Arr_Acc;
   Wait_Mutex : aliased Synchronization.Binary_Semaphore :=
      Synchronization.Unlocked_Semaphore;

   --  Cores waiting for an interrupt with nothing to run, see Kick.
   Max_Cores  : constant := 256;
   Is_Waiting : array (1 .. Max_Cores) of Boolean := [others => False]
      with Atomic_Components;

   --  In order to keep statistics of usage, we keep a list of buckets of
   --  1 minute resolution, and calculate resolution on demand.
   --  We take advantage of the monotic clock starting at 0 in boot.
   type Stats_Bucket_Arr is array (1 .. 15) of Unsigned_32;
   Last_Bucket : Unsigned_64      := 0;
   Buckets     : Stats_Bucket_Arr := [others => 0];

   --  Common stack permissions.
   Tmp_Stack_Permissions : constant Arch.MMU.Page_Permissions :=
      (Is_User_Accessible => False,
       Can_Read           => True,
       Can_Write          => True,
       Can_Execute        => False,
       Is_Global          => False);
   Stack_Permissions : constant Arch.MMU.Page_Permissions :=
      (Is_User_Accessible => True,
       Can_Read           => True,
       Can_Write          => True,
       Can_Execute        => False,
       Is_Global          => False);

   --  The frame a signal handler is entered with.
   type Siginfo_Tail is array (1 .. Siginfo_Size - 28) of Unsigned_8;
   type Siginfo is record
      Number : Unsigned_32;
      Errno  : Unsigned_32;
      Code   : Unsigned_32;
      Pad    : Unsigned_32;
      PID    : Unsigned_32;
      UID    : Unsigned_32;
      Status : Unsigned_32;
      Tail   : Siginfo_Tail;
   end record with Size => Siginfo_Size * 8;
   for Siginfo use record
      Number at  0 range 0 .. 31;
      Errno  at  4 range 0 .. 31;
      Code   at  8 range 0 .. 31;
      Pad    at 12 range 0 .. 31;
      PID    at 16 range 0 .. 31;
      UID    at 20 range 0 .. 31;
      Status at 24 range 0 .. 31;
      Tail   at 28 range 0 .. (Siginfo_Size - 28) * 8 - 1;
   end record;

   #if ArchName = """riscv64-limine"""
      UC_Stack    : constant := 2;
      UC_Sigmask  : constant := 5;
      UC_Gregs    : constant := 22;
      UC_FPRegs   : constant := 54;
      UC_Words    : constant := 120;
      Return_Slot : constant := 0;
      Red_Zone    : constant := 0;
   #elsif ArchName = """x86_64-limine"""
      UC_Stack    : constant := 2;
      UC_Gregs    : constant := 5;
      UC_FPRegs   : constant := 28;
      UC_Sigmask  : constant := 37;
      UC_FP_Mem   : constant := 53;
      UC_Words    : constant := 121;
      Return_Slot : constant := 8;
      Red_Zone    : constant := 128;

      User_RFLAGS : constant := 16#24_4DD5#;
      Entry_Clear_RFLAGS : constant := 16#1_0500#;
   #end if;
   type UContext is array (0 .. UC_Words - 1) of Unsigned_64;

   --  The frame sits at the top of the stack.
   Frame_Size : constant :=
      15 + 16 + Siginfo_Size + UC_Words * 8 + 15 + Return_Slot;

   SS_ONSTACK : constant := 1;
   SS_DISABLE : constant := 2;

   procedure Init (Success : out Boolean) is
   begin
      --  Initialize registries. GNAT before 15 can build an array aggregate on
      --  the stack first, and we still run on the bootloader's small one, so
      --  the thread table is filled one entry at a time.
      Thread_Pool := new Thread_Info_Arr;
      for Thread of Thread_Pool.all loop
         Thread :=
            (Is_Present      => False,
             Is_Running      => False,
             Is_Doomed       => False,
             Is_Held         => False,
             Path            => [others => ' '],
             Path_Len        => 0,
             RR_Micro_Inter  => Default_RR_NS_Interval / 1000,
             Pol             => Policy_Other,
             Prio            => Default_Priority,
             Nice            => Default_Niceness,
             TCB_Pointer     => System.Null_Address,
             PageMap         => System.Null_Address,
             Kernel_Stack    => null,
             GP_State        => <>,
             FP_State        => <>,
             Syscall_State   => <>,
             Process         => Userland.Process.Error_PID,
             System_Runtime  => (0, 0),
             User_Runtime    => (0, 0),
             System_Tmp      => (0, 0),
             User_Tmp        => (0, 0),
             User_Stack      => System.Null_Address,
             User_Stack_Size => 0,
             User_Stack_Used => False,
             Masked_Signals  => 0,
             Is_Disabled     => True,
             Start_Clock     => Time.Monotonic_Clock,
             Start_Time      => (0, 0),
             Wake_Pending    => False,
             Event_Waiting   => False,
             Core            => 0,
             Saved_Mask      => 0,
             Has_Saved_Mask  => False);
      end loop;
      Waits := new Wait_Info_Arr'
         [others => (Count => 0, Keys => [others => System.Null_Address])];

      Is_Initialized := True;
      Synchronization.Release (Scheduler_Mutex);
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end Init;

   procedure Idle_Core is
      To_Test : Boolean;
   begin
      loop
         To_Test := Is_Initialized;
         exit when To_Test;
         Arch.Snippets.Pause;
      end loop;
      Arch.Local.Reschedule_ASAP;
      Waiting_Spot;
   end Idle_Core;

   procedure Create_User_Thread
      (Address    : Virtual_Address;
       Args       : Userland.Argument_Arr;
       Env        : Userland.Environment_Arr;
       Map        : Memory.MMU.Page_Table_Acc;
       Vector     : Userland.ELF.Auxval;
       Pol        : Policy;
       Stack_Size : Unsigned_64;
       PID        : Natural;
       New_TID    : out TID)
   is
      Proc : constant Userland.Process.PID := Userland.Process.Convert (PID);
      GP_State  : Arch.Context.GP_Context;
      FP_State  : Arch.Context.FP_Context;
      Stack_Top : Unsigned_64;
      Success   : Boolean;
      Curr_Map  : System.Address;
   begin
      New_TID := Error_TID;

      --  Set the stack map so we can access the allocated range.
      Curr_Map := Memory.MMU.Get_Curr_Table_Addr;
      Success  := Memory.MMU.Make_Active (Map);
      if not Success then
         return;
      end if;

      --  Initialize thread state. Start by mapping the user stack.
      Userland.Process.Bump_Alloc_Base (Proc, Stack_Size, Stack_Top);
      Memory.MMU.Map_Allocated_Range
         (Map           => Map,
          Virtual_Start => To_Address (Virtual_Address (Stack_Top)),
          Length        => Storage_Offset (Stack_Size),
          Permissions   => Tmp_Stack_Permissions,
          Success       => Success);
      if not Success then
         goto Cleanup;
      end if;

      declare
         UID    : Unsigned_32;
         EUID   : Unsigned_32;
         GID    : Unsigned_32;
         EGID   : Unsigned_32;
         Is_Secure_Exec : Boolean;
         Sz     : constant Natural := Natural (Stack_Size);
         Stk_8  : Thread_Stack (1 .. Sz)
            with Import, Address => To_Address (Virtual_Address (Stack_Top));
         Stk_64 : Thread_Stack_64 (1 .. Sz / 8)
            with Import, Address => To_Address (Virtual_Address (Stack_Top));
         Index_8  : Natural := Stk_8'Last;
         Index_64 : Natural := Stk_64'Last;
      begin
         --  Get proc info.
         Userland.Process.Get_UID (Proc, UID);
         Userland.Process.Get_Effective_UID (Proc, EUID);
         Userland.Process.Get_GID (Proc, GID);
         Userland.Process.Get_Effective_GID (Proc, EGID);
         Is_Secure_Exec := UID /= EUID or GID /= EGID;

         --  Load env into the stack.
         for En of reverse Env loop
            Stk_8 (Index_8) := 0;
            Index_8 := Index_8 - 1;
            for C of reverse En.all loop
               Stk_8 (Index_8) := Character'Pos (C);
               Index_8 := Index_8 - 1;
            end loop;
         end loop;

         --  Load argv into the stack.
         for Arg of reverse Args loop
            Stk_8 (Index_8) := 0;
            Index_8 := Index_8 - 1;
            for C of reverse Arg.all loop
               Stk_8 (Index_8) := Character'Pos (C);
               Index_8 := Index_8 - 1;
            end loop;
         end loop;

         --  Get the equivalent 64-bit stack index and align it to 16 bytes.
         Index_64 := (Index_8 / 8) - ((Index_8 / 8) mod 16);
         Index_64 := Index_64 - ((Args'Length + Env'Length + 3) mod 2);

         --  16 random bytes of data for AT_RANDOM.
         Cryptography.Random.Get_Integer (Stk_64 (Index_64 - 0));
         Cryptography.Random.Get_Integer (Stk_64 (Index_64 - 1));
         Index_64 := Index_64 - 2;

         --  Load auxval.
         Stk_64 (Index_64 - 0)  := 0;
         Stk_64 (Index_64 - 1)  := Userland.ELF.Auxval_Null;
         Stk_64 (Index_64 - 2)  := Vector.Entrypoint;
         Stk_64 (Index_64 - 3)  := Userland.ELF.Auxval_Entrypoint;
         Stk_64 (Index_64 - 4)  := Vector.Program_Headers;
         Stk_64 (Index_64 - 5)  := Userland.ELF.Auxval_Program_Headers;
         Stk_64 (Index_64 - 6)  := Vector.Program_Header_Count;
         Stk_64 (Index_64 - 7)  := Userland.ELF.Auxval_Header_Count;
         Stk_64 (Index_64 - 8)  := Vector.Program_Header_Size;
         Stk_64 (Index_64 - 9)  := Userland.ELF.Auxval_Header_Size;
         Stk_64 (Index_64 - 10) := Memory.MMU.Page_Size;
         Stk_64 (Index_64 - 11) := Userland.ELF.Auxval_Page_Size;
         Stk_64 (Index_64 - 12) := (if Is_Secure_Exec then 1 else 0);
         Stk_64 (Index_64 - 13) := Userland.ELF.Auxval_Secure_Treatment;
         Stk_64 (Index_64 - 14) := Unsigned_64 (UID);
         Stk_64 (Index_64 - 15) := Userland.ELF.Auxval_UID;
         Stk_64 (Index_64 - 16) := Unsigned_64 (EUID);
         Stk_64 (Index_64 - 17) := Userland.ELF.Auxval_EUID;
         Stk_64 (Index_64 - 18) := Unsigned_64 (GID);
         Stk_64 (Index_64 - 19) := Userland.ELF.Auxval_GID;
         Stk_64 (Index_64 - 20) := Unsigned_64 (EGID);
         Stk_64 (Index_64 - 21) := Userland.ELF.Auxval_EGID;
         Stk_64 (Index_64 - 22) := 0;
         Stk_64 (Index_64 - 23) := Userland.ELF.Auxval_Flags;
         Arch.Hooks.Get_User_Hardware_Caps (Stk_64 (Index_64 - 24));
         Stk_64 (Index_64 - 25) := Userland.ELF.Auxval_Hardware_Cap;
         Stk_64 (Index_64 - 26) := Stack_Top + Unsigned_64 (Index_64 * 8);
         Stk_64 (Index_64 - 27) := Userland.ELF.Auxval_Random;
         Index_64 := Index_64 - 28;

         --  Load envp taking into account the pointers at the beginning.
         Index_8 := Stk_8'Last;
         Stk_64 (Index_64) := 0; --  Null at the end of envp.
         Index_64 := Index_64 - 1;
         for En of reverse Env loop
            Index_8 := (Index_8 - En.all'Length) - 1;
            Stk_64 (Index_64) := Stack_Top + Unsigned_64 (Index_8);
            Index_64 := Index_64 - 1;
         end loop;

         --  Load argv into the stack.
         Stk_64 (Index_64) := 0; --  Null at the end of argv.
         Index_64 := Index_64 - 1;
         for Arg of reverse Args loop
            Index_8 := (Index_8 - Arg.all'Length) - 1;
            Stk_64 (Index_64) := Stack_Top + Unsigned_64 (Index_8);
            Index_64 := Index_64 - 1;
         end loop;

         --  Write argc and we are done!
         Stk_64 (Index_64) := Args'Length;
         Index_64 := Index_64 - 1;

         --  Remap the stack for user permissions.
         Memory.MMU.Remap_Range
            (Map           => Map,
             Virtual_Start => To_Address (Virtual_Address (Stack_Top)),
             Length        => Storage_Offset (Stack_Size),
             Permissions   => Stack_Permissions,
             Success       => Success);
         if not Success then
            goto Cleanup;
         end if;

         --  Initialize context information.
         Index_64 := Index_64 * 8;
         Arch.Context.Init_GP_Context
            (GP_State,
             To_Address (Integer_Address (Stack_Top + Unsigned_64 (Index_64))),
             To_Address (Address));
         Arch.Context.Init_FP_Context (FP_State);

         Create_User_Thread
            (GP_State => GP_State,
             FP_State => FP_State,
             Map      => Map,
             PID      => PID,
             Pol      => Pol,
             TCB      => System.Null_Address,
             New_TID  => New_TID);
      end;

   <<Cleanup>>
      Memory.MMU.Set_Table_Addr (Curr_Map);
   exception
      when Constraint_Error =>
         New_TID := Error_TID;
   end Create_User_Thread;

   procedure Create_User_Thread
      (Address    : Virtual_Address;
       Map        : Memory.MMU.Page_Table_Acc;
       Stack_Addr : Unsigned_64;
       TLS_Addr   : Unsigned_64;
       Pol        : Policy;
       Argument   : Unsigned_64;
       PID        : Natural;
       New_TID    : out TID)
   is
      GP_State : Arch.Context.GP_Context;
      FP_State : Arch.Context.FP_Context;
   begin
      Arch.Context.Init_GP_Context
         (GP_State,
          To_Address (Integer_Address (Stack_Addr)),
          To_Address (Address),
          Argument, 0, 0);
      Arch.Context.Init_FP_Context (FP_State);
      Create_User_Thread
         (GP_State => GP_State,
          FP_State => FP_State,
          Map      => Map,
          PID      => PID,
          Pol      => Pol,
          TCB      => To_Address (Integer_Address (TLS_Addr)),
          New_TID  => New_TID);
   end Create_User_Thread;

   procedure Create_User_Thread
      (GP_State : Arch.Context.GP_Context;
       FP_State : Arch.Context.FP_Context;
       Map      : Memory.MMU.Page_Table_Acc;
       Pol      : Policy;
       PID      : Natural;
       TCB      : System.Address;
       New_TID  : out TID)
   is
      New_Stack : Kernel_Stack_Acc;
      Maker          : constant TID := Arch.Local.Get_Current_Thread;
      Inherited_Mask : Unsigned_64;
   begin
      New_TID := Error_TID;
      Get_Masked_Signals (Maker, Inherited_Mask);
      Synchronization.Seize (Scheduler_Mutex);

      --  Find a new TID.
      for I in Thread_Pool'Range loop
         if not Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Running
         then
            New_TID := I;
            goto Found_TID;
         end if;
      end loop;
      goto End_Return;

   <<Found_TID>>
      if Thread_Pool (New_TID).Kernel_Stack = null then
         New_Stack := new Kernel_Stack'[others => 0];
      else
         New_Stack := Thread_Pool (New_TID).Kernel_Stack;
         Thread_Pool (New_TID).Kernel_Stack.all := [others => 0];
      end if;

      Thread_Pool (New_TID) :=
         (Is_Present     => True,
          Is_Running     => False,
          Is_Doomed      => False,
          Is_Held        => True,
          Path           => [others => ' '],
          Path_Len       => 0,
          RR_Micro_Inter => Default_RR_NS_Interval / 1000,
          Pol            => Pol,
          Prio           => Default_Priority,
          Nice           => Default_Niceness,
          TCB_Pointer    => TCB,
          PageMap        => Memory.MMU.Get_Map_Table_Addr (Map),
          Kernel_Stack   => New_Stack,
          GP_State       => GP_State,
          FP_State       => FP_State,
          Syscall_State  => GP_State,
          Process        => Userland.Process.Convert (PID),
          User_Stack      => System.Null_Address,
          User_Stack_Size => 0,
          User_Stack_Used => False,
          Masked_Signals  => Inherited_Mask,
          Is_Disabled     => True,
          Start_Clock     => Time.Monotonic_Clock,
          Start_Time      => (0, 0),
          Wake_Pending    => False,
          Event_Waiting   => False,
          Core            => 0,
          System_Runtime  => (0, 0),
          User_Runtime    => (0, 0),
          System_Tmp      => (0, 0),
          User_Tmp        => (0, 0),
          Saved_Mask      => 0,
          Has_Saved_Mask  => False);

      Userland.Process.Clear_Thread_Signals (New_TID);

      Arch.Clocks.Get_Monotonic_Time (Thread_Pool (New_TID).User_Tmp);
      Arch.Context.Success_Fork_Result (Thread_Pool (New_TID).GP_State);

   <<End_Return>>
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
         New_TID := Error_TID;
   end Create_User_Thread;

   procedure Release_Thread (Thread : TID) is
   begin
      --  No lock: the flag is atomic, and Add_Thread calls this with the
      --  process's own lock held, which the scheduler's must never nest in.
      Thread_Pool (Thread).Is_Held := False;
      Kick (Thread);
   exception
      when Constraint_Error =>
         null;
   end Release_Thread;

   procedure Delete_Thread (Thread : TID) is
      Self : constant TID := Arch.Local.Get_Current_Thread;
   begin
      Synchronization.Seize (Scheduler_Mutex);
      if Thread_Pool (Thread).Is_Present then
         if Thread = Self then
            Drop_Thread (Thread);
         elsif not Thread_Pool (Thread).Is_Running and then
               Arch.Context.Is_User_Context (Thread_Pool (Thread).GP_State)
         then
            Drop_Thread (Thread);
         else
            Thread_Pool (Thread).Is_Doomed   := True;
            Thread_Pool (Thread).Start_Clock := Time.Monotonic_Clock;
            Thread_Pool (Thread).Start_Time  := (0, 0);
            Kick (Thread);
         end if;
      end if;
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
   end Delete_Thread;

   procedure Wait_For_Removed (PID : Natural) is
      Self   : constant TID := Arch.Local.Get_Current_Thread;
      Kernel : constant System.Address :=
         Memory.MMU.Get_Map_Table_Addr (Memory.MMU.Kernel_Table);
      Final  : Time.Timestamp;
      Curr   : Time.Timestamp;
      Found  : Boolean;
   begin
      --  Threads that went to wait on the kernel's map count as gone, see Bail
      --  and Scheduler_ISR. One that never leaves its core, as one stuck in
      --  the kernel would, is only waited for a while.
      Arch.Clocks.Get_Monotonic_Time (Final);
      Final := Final + (Removed_Wait_Seconds, 0);
      loop
         Found := False;
         Synchronization.Seize (Scheduler_Mutex);
         for T in Thread_Pool'Range loop
            if T /= Self and then
               not Thread_Pool (T).Is_Present and then
               Thread_Pool (T).Is_Running and then
               Thread_Pool (T).PageMap /= Kernel and then
               Userland.Process.Convert (Thread_Pool (T).Process) = PID
            then
               Found := True;
               exit;
            end if;
         end loop;
         Synchronization.Release (Scheduler_Mutex);

         Arch.Clocks.Get_Monotonic_Time (Curr);
         exit when not Found or Curr >= Final;
         Begin_Wait;
         Wait_Event (No_Deadline, Removed_Poll_Micros);
         End_Wait;
      end loop;
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
   end Wait_For_Removed;

   procedure Yield_If_Able is
      Curr_TID : constant     TID := Arch.Local.Get_Current_Thread;
      Is_Init  : constant Boolean := Is_Initialized;
   begin
      if Is_Init and Curr_TID /= Error_TID then
         Arch.Local.Reschedule_ASAP;
         if Arch.Snippets.Interrupts_Enabled then
            Wait_If_Suspended (Curr_TID);
         end if;
      end if;
   end Yield_If_Able;

   procedure Bail is
      Thread  : constant TID := Arch.Local.Get_Current_Thread;
      Discard : Boolean;
   begin
      --  The core waits on the kernel's own table, as the thread's may be
      --  freed before anything else runs here.
      Discard := Memory.MMU.Make_Active (Memory.MMU.Kernel_Table);
      Synchronization.Seize (Scheduler_Mutex);
      if Thread_Pool (Thread).Is_Present then
         Thread_Pool (Thread).Is_Present := False;
         Arch.Context.Destroy_FP_Context (Thread_Pool (Thread).FP_State);
      end if;
      Thread_Pool (Thread).PageMap := Memory.MMU.Get_Curr_Table_Addr;
      Synchronization.Release (Scheduler_Mutex);
      Arch.Local.Reschedule_ASAP;
      Waiting_Spot;
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
         Arch.Local.Reschedule_ASAP;
         Waiting_Spot;
   end Bail;

   function Is_Doomed return Boolean is
      Thread : constant TID := Arch.Local.Get_Current_Thread;
   begin
      return Thread /= Error_TID and then Thread_Pool (Thread).Is_Doomed;
   exception
      when Constraint_Error =>
         return False;
   end Is_Doomed;

   function Is_Interrupted return Boolean is
      Thread : constant TID := Arch.Local.Get_Current_Thread;
      Proc   : constant Userland.Process.PID :=
         Arch.Local.Get_Current_Process;
   begin
      --  A signal the thread can take ends the wait as surely as being
      --  doomed does: it is delivered on the way out of the call, so the
      --  call has to get there, and what it answers is EINTR.
      return Is_Doomed or else
             (Thread /= Error_TID and then
              Userland.Process.Has_Deliverable_Signal
                 (Proc, Thread, Thread_Pool (Thread).Masked_Signals));
   exception
      when Constraint_Error =>
         return False;
   end Is_Interrupted;

   procedure Get_Runtimes (Thread : TID; System, User : out Time.Timestamp) is
   begin
      Synchronization.Seize (Scheduler_Mutex);
      System := Thread_Pool (Thread).System_Runtime;
      User := Thread_Pool (Thread).User_Runtime;
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
         System := (0, 0);
         User := (0, 0);
   end Get_Runtimes;

   procedure Signal_Kernel_Entry (Thread : TID) is
      Tmp : Time.Timestamp;
   begin
      Arch.Clocks.Get_Monotonic_Time (Tmp);
      Thread_Pool (Thread).System_Tmp := Tmp;
      Thread_Pool (Thread).User_Runtime := Thread_Pool (Thread).User_Runtime +
         (Tmp - Thread_Pool (Thread).User_Tmp);
   exception
      when Constraint_Error =>
         null;
   end Signal_Kernel_Entry;

   procedure Set_Syscall_State
      (Thread : TID;
       State  : Arch.Context.GP_Context)
   is
   begin
      Thread_Pool (Thread).Syscall_State := State;
   exception
      when Constraint_Error =>
         null;
   end Set_Syscall_State;

   procedure Restart_Syscall
      (Thread : TID;
       State  : in out Arch.Context.GP_Context)
   is
   begin
      Arch.Context.Repeat_Syscall (State, Thread_Pool (Thread).Syscall_State);
   exception
      when Constraint_Error =>
         null;
   end Restart_Syscall;

   procedure Set_Saved_Mask (Thread : TID; Mask : Unsigned_64) is
   begin
      Thread_Pool (Thread).Saved_Mask     := Mask;
      Thread_Pool (Thread).Has_Saved_Mask := True;
   exception
      when Constraint_Error =>
         null;
   end Set_Saved_Mask;

   procedure Take_Saved_Mask
      (Thread : TID;
       Mask   : out Unsigned_64;
       Valid  : out Boolean)
   is
   begin
      Mask  := Thread_Pool (Thread).Saved_Mask;
      Valid := Thread_Pool (Thread).Has_Saved_Mask;
      Thread_Pool (Thread).Has_Saved_Mask := False;
   exception
      when Constraint_Error =>
         Mask  := 0;
         Valid := False;
   end Take_Saved_Mask;

   procedure Signal_Kernel_Exit (Thread : TID) is
      Tmp : Time.Timestamp;
   begin
      Arch.Clocks.Get_Monotonic_Time (Tmp);
      Thread_Pool (Thread).User_Tmp := Tmp;

      if Thread_Pool (Thread).System_Tmp /= (0, 0) then
         Tmp := Tmp - Thread_Pool (Thread).System_Tmp;
         Thread_Pool (Thread).System_Runtime :=
            Thread_Pool (Thread).System_Runtime + Tmp;
      end if;
   exception
      when Constraint_Error =>
         null;
   end Signal_Kernel_Exit;

   procedure Suspend_Until
      (Clock      : Time.Clock_Type;
       Start_Time : Time.Timestamp)
   is
      Thread : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
      Stop : Boolean;
   begin
      Synchronization.Seize (Scheduler_Mutex);
      if not Thread_Pool (Thread).Is_Doomed then
         Thread_Pool (Thread).Start_Clock := Clock;
         Thread_Pool (Thread).Start_Time := Start_Time;
      end if;
      Synchronization.Release (Scheduler_Mutex);

      loop
         Synchronization.Seize (Scheduler_Mutex);
         Evaluate_Suspended (Thread, Stop);
         Synchronization.Release (Scheduler_Mutex);
         exit when not Stop or else Is_Interrupted;
         Scheduler.Yield_If_Able;
      end loop;

      --  A sleep given up for a signal or a kill leaves no deadline behind:
      --  the thread would go on counting as asleep, and once off its core it
      --  would not be run again before that time.
      Synchronization.Seize (Scheduler_Mutex);
      Thread_Pool (Thread).Start_Clock := Time.Monotonic_Clock;
      Thread_Pool (Thread).Start_Time  := (0, 0);
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         --  The index check fires with the lock held.
         Synchronization.Release (Scheduler_Mutex);
   end Suspend_Until;

   procedure Mark_Suspend is
      Thread : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
   begin
      --  Wait until the end of time is pretty close to waiting forever.
      Synchronization.Seize (Scheduler_Mutex);
      Thread_Pool (Thread).Start_Clock := Time.Monotonic_Clock;
      Thread_Pool (Thread).Start_Time := (others => Unsigned_64'Last);
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         --  The index check fires with the lock held.
         Synchronization.Release (Scheduler_Mutex);
   end Mark_Suspend;

   procedure Lift_Suspension (Thread : TID) is
   begin
      --  Wait until the beginning of time is pretty close to not waiting.
      Synchronization.Seize (Scheduler_Mutex);
      Thread_Pool (Thread).Start_Clock := Time.Monotonic_Clock;
      Thread_Pool (Thread).Start_Time := (0, 0);
      Synchronization.Release (Scheduler_Mutex);
      Kick (Thread);
   exception
      when Constraint_Error =>
         --  The index check fires with the lock held.
         Synchronization.Release (Scheduler_Mutex);
   end Lift_Suspension;

   procedure Is_Suspended (Thread : TID; Suspended : out Boolean) is
   begin
      Synchronization.Seize (Scheduler_Mutex);
      Evaluate_Suspended (Thread, Suspended);
      Synchronization.Release (Scheduler_Mutex);
   end Is_Suspended;
   ----------------------------------------------------------------------------
   procedure Begin_Wait is
      Thread : constant TID := Arch.Local.Get_Current_Thread;
   begin
      if Thread = Error_TID or Waits = null then
         return;
      end if;
      Synchronization.Seize (Wait_Mutex);
      Waits (Thread).Count := 0;
      Synchronization.Release (Wait_Mutex);
      Clear_Wake;
   exception
      when Constraint_Error =>
         Synchronization.Release (Wait_Mutex);
   end Begin_Wait;

   procedure Add_Wait_Key (Key : System.Address; Success : out Boolean) is
      Thread : constant TID := Arch.Local.Get_Current_Thread;
   begin
      Success := False;
      if Thread = Error_TID or Waits = null then
         return;
      end if;

      Synchronization.Seize (Wait_Mutex);
      for I in 1 .. Waits (Thread).Count loop
         if Waits (Thread).Keys (I) = Key then
            Success := True;
            goto Done;
         end if;
      end loop;
      if Waits (Thread).Count < Max_Wait_Keys then
         Waits (Thread).Count := Waits (Thread).Count + 1;
         Waits (Thread).Keys (Waits (Thread).Count) := Key;
         Success := True;
      end if;
   <<Done>>
      Synchronization.Release (Wait_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Wait_Mutex);
         Success := False;
   end Add_Wait_Key;

   procedure Clear_Wake is
      Thread : constant TID := Arch.Local.Get_Current_Thread;
   begin
      if Thread = Error_TID then
         return;
      end if;
      Synchronization.Seize (Scheduler_Mutex);
      Thread_Pool (Thread).Wake_Pending := False;
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
   end Clear_Wake;

   procedure Wait_Event (Deadline : Time.Timestamp; Max_Micros : Natural) is
      Thread : constant TID := Arch.Local.Get_Current_Thread;
      Until_Time : Time.Timestamp;
      Stop   : Boolean;
   begin
      if Thread = Error_TID then
         return;
      end if;

      Arch.Clocks.Get_Monotonic_Time (Until_Time);
      Until_Time := Until_Time + (0, Unsigned_64 (Max_Micros) * 1_000);
      if Until_Time > Deadline then
         Until_Time := Deadline;
      end if;

      --  Wake_Event sets the flag with the lock held, so wakes are either
      --  seen here or lift the sleep below.
      Synchronization.Seize (Scheduler_Mutex);
      if not Thread_Pool (Thread).Wake_Pending and
         not Thread_Pool (Thread).Is_Doomed
      then
         Thread_Pool (Thread).Event_Waiting := True;
         Thread_Pool (Thread).Start_Clock   := Time.Monotonic_Clock;
         Thread_Pool (Thread).Start_Time    := Until_Time;
      end if;
      Synchronization.Release (Scheduler_Mutex);

      loop
         Synchronization.Seize (Scheduler_Mutex);
         Evaluate_Suspended (Thread, Stop);
         Synchronization.Release (Scheduler_Mutex);
         exit when not Stop;
         Yield_If_Able;
         exit when Is_Doomed;
      end loop;

      --  As in Suspend_Until, a sleep given up early leaves no deadline.
      Synchronization.Seize (Scheduler_Mutex);
      Thread_Pool (Thread).Event_Waiting := False;
      Thread_Pool (Thread).Start_Clock   := Time.Monotonic_Clock;
      Thread_Pool (Thread).Start_Time    := (0, 0);
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         --  The index check fires with the lock held.
         Synchronization.Release (Scheduler_Mutex);
   end Wait_Event;

   procedure End_Wait is
      Thread : constant TID := Arch.Local.Get_Current_Thread;
   begin
      if Thread = Error_TID or Waits = null then
         return;
      end if;
      Synchronization.Seize (Wait_Mutex);
      Waits (Thread).Count := 0;
      Synchronization.Release (Wait_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Wait_Mutex);
   end End_Wait;

   procedure Wake_Event (Key : System.Address) is
      Was_Waiting : Boolean;
   begin
      if Key = System.Null_Address or Waits = null then
         return;
      end if;

      Synchronization.Seize (Wait_Mutex);
      for T in Waits'Range loop
         for I in 1 .. Waits (T).Count loop
            if Waits (T).Keys (I) = Key then
               --  Only lift sleeps in Wait_Event, not those of other kinds.
               Synchronization.Seize (Scheduler_Mutex);
               Thread_Pool (T).Wake_Pending := True;
               Was_Waiting := Thread_Pool (T).Event_Waiting;
               if Was_Waiting then
                  Thread_Pool (T).Start_Clock := Time.Monotonic_Clock;
                  Thread_Pool (T).Start_Time  := (0, 0);
               end if;
               Synchronization.Release (Scheduler_Mutex);
               if Was_Waiting then
                  Kick (T);
               end if;
               exit;
            end if;
         end loop;
      end loop;
      Synchronization.Release (Wait_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Wait_Mutex);
   end Wake_Event;

   procedure Wake_For_Signal (PID : Natural; Thread : TID) is
      Self : constant Positive := Arch.Local.Get_Core_Number;
      Core : Natural;
   begin
      --  Under the lock a sleeping thread is checked and its core marked
      --  waiting with, so a kick either finds that core waiting or comes
      --  before the check, which then sees the signal: see Kick and
      --  Wait_If_Suspended. A thread running on another core that is not
      --  waiting is interrupted there, and takes the signal on its way back
      --  to its own code rather than at the end of its time.
      Synchronization.Seize (Scheduler_Mutex);
      for T in Thread_Pool'Range loop
         if Thread_Pool (T).Is_Present and then
            (if Thread /= Error_TID then T = Thread
             else Userland.Process.Convert (Thread_Pool (T).Process) = PID)
         then
            Core := Thread_Pool (T).Core;
            if Thread_Pool (T).Is_Running and then Core /= Self and then
               Core in Is_Waiting'Range and then not Is_Waiting (Core)
            then
               Arch.Local.Reschedule_Core (Core);
            else
               Kick (T);
            end if;
         end if;
      end loop;
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
   end Wake_For_Signal;

   function Get_Niceness (Thread : TID) return Niceness is
   begin
      return Thread_Pool (Thread).Nice;
   exception
      when Constraint_Error =>
         return Default_Niceness;
   end Get_Niceness;

   procedure Set_Niceness (Thread : TID; Nice : Niceness) is
   begin
      Thread_Pool (Thread).Nice := Nice;
   exception
      when Constraint_Error =>
         null;
   end Set_Niceness;

   function Get_Priority (Thread : TID) return Priority is
   begin
      return Thread_Pool (Thread).Prio;
   exception
      when Constraint_Error =>
         return Default_Priority;
   end Get_Priority;

   procedure Set_Priority (Thread : TID; Prio : Priority) is
   begin
      Thread_Pool (Thread).Prio := Prio;
   exception
      when Constraint_Error =>
         null;
   end Set_Priority;

   procedure Set_Policy (Thread : TID; Pol : Policy) is
   begin
      Thread_Pool (Thread).Pol := Pol;
   exception
      when Constraint_Error =>
         null;
   end Set_Policy;

   procedure Set_RR_Interval (Thread : TID; RR_Sec, RR_NS : Unsigned_64)
   is
   begin
      Thread_Pool (Thread).RR_Micro_Inter :=
         Natural (RR_Sec * 1000000) + Natural (RR_NS / 1000);
   exception
      when Constraint_Error =>
         null;
   end Set_RR_Interval;

   procedure Get_Name (Thread : TID; Name : out String; Len : out Natural) is
   begin
      if Name'Length >= Thread_Pool (Thread).Path_Len and
         Thread_Pool (Thread).Path_Len /= 0
      then
         Name (Name'First .. Name'First + Thread_Pool (Thread).Path_Len - 1) :=
            Thread_Pool (Thread).Path (1 .. Thread_Pool (Thread).Path_Len);
         Len := Thread_Pool (Thread).Path_Len;
      else
         Name := [others => ' '];
         Len  := 0;
      end if;
   exception
      when Constraint_Error =>
         Name := [others => ' '];
         Len  := 0;
   end Get_Name;

   procedure Set_Name (Thread : TID; Name : String; Success : out Boolean) is
   begin
      if Name'Length <= Thread_Pool (Thread).Path'Length then
         Thread_Pool (Thread).Path (1 .. Name'Length) := Name;
         Thread_Pool (Thread).Path_Len                := Name'Length;
         Success := True;
      else
         Success := False;
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end Set_Name;
   ----------------------------------------------------------------------------
   procedure Get_Masked_Signals (Thread : TID; Mask : out Unsigned_64) is
   begin
      Mask := Thread_Pool (Thread).Masked_Signals;
   exception
      when Constraint_Error =>
         Mask := 0;
   end Get_Masked_Signals;

   procedure Set_Masked_Signals (Thread : TID; Mask : Unsigned_64) is
   begin
      Thread_Pool (Thread).Masked_Signals := Mask;
   exception
      when Constraint_Error =>
         null;
   end Set_Masked_Signals;

   procedure Get_Signal_Stack
      (Thread      : TID;
       Addr        : out System.Address;
       Size        : out Unsigned_64;
       Is_Disabled : out Boolean;
       Is_Using    : out Boolean)
   is
   begin
      Addr := Thread_Pool (Thread).User_Stack;
      Size := Thread_Pool (Thread).User_Stack_Size;
      Is_Disabled := Thread_Pool (Thread).Is_Disabled;
      Is_Using := Thread_Pool (Thread).User_Stack_Used;
   exception
      when Constraint_Error =>
         Addr := System.Null_Address;
         Size := 0;
         Is_Disabled := True;
         Is_Using := False;
   end Get_Signal_Stack;

   procedure Set_Signal_Stack
      (Thread      : TID;
       Addr        : System.Address;
       Size        : Unsigned_64;
       Is_Disabled : Boolean;
       Success     : out Boolean)
   is
   begin
      if not Thread_Pool (Thread).User_Stack_Used then
         Thread_Pool (Thread).User_Stack := Addr;
         Thread_Pool (Thread).User_Stack_Size := Size;
         Thread_Pool (Thread).Is_Disabled := Is_Disabled;
         Success := True;
      else
         Success := False;
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end Set_Signal_Stack;

   procedure Enter_Signal_Handler
      (State            : in out Arch.Context.GP_Context;
       Signal_Number    : Unsigned_64;
       Handle, Restorer : System.Address;
       Is_Altstack      : Boolean;
       Info_Code        : Unsigned_32;
       Info_PID         : Unsigned_32;
       Info_UID         : Unsigned_32;
       Info_Status      : Unsigned_32;
       Info_Value       : Unsigned_64;
       Old_Mask         : Unsigned_64;
       Fault_Addr       : Unsigned_64 := 0;
       Is_Fault         : Boolean     := False;
       Success          : out Boolean)
   is
      Th   : constant TID := Arch.Local.Get_Current_Thread;
      Proc : constant Userland.Process.PID :=
         Arch.Local.Get_Current_Process;
      Own_FP    : Arch.Context.FP_Context;
      Map       : Memory.MMU.Page_Table_Acc;
      Use_Altsk : Boolean;
      Was_On    : Boolean;
      Top       : Unsigned_64;
   begin
      Success := False;

      --  A handler is entered through the call's own return path, which on
      --  x86 takes the address in a register the processor requires to be
      --  canonical, and which runs in kernel mode: an address that is not
      --  userland's would fault the kernel rather than the process.
      if Unsigned_64 (To_Integer (Handle)) >=
         Unsigned_64 (Arch.MMU.Canonical_Hole_Offset)
      then
         return;
      end if;

      Userland.Process.Get_Common_Map (Proc, Map);
      Was_On := Thread_Pool (Th).User_Stack_Used;

      --  Initialize signal stack. We either start by mapping a new user stack
      --  or we use to passed one.
      Use_Altsk :=
         Is_Altstack and
         Thread_Pool (Th).User_Stack /= System.Null_Address and
         not Thread_Pool (Th).Is_Disabled;

      if Use_Altsk then
         if Thread_Pool (Th).User_Stack_Size < Frame_Size then
            return;
         end if;
         Top :=
            (Unsigned_64 (To_Integer (Thread_Pool (Th).User_Stack)) +
             Thread_Pool (Th).User_Stack_Size) and not 15;
      else
         #if ArchName = """x86_64-limine""" then
            --  Below what the thread was interrupted on, past the bytes a
            --  leaf function may be using without moving the pointer.
            Top := (State.RSP - Red_Zone) and not 15;
         #elsif ArchName = """riscv64-limine""" then
            Top := (State.X2 - Red_Zone) and not 15;
         #end if;
      end if;

      --  The frame is built in kernel memory and copied out at once, which
      --  checks that the stack really is memory the process may write: an
      --  alternate stack is wherever userland said it was, and another
      --  thread may unmap any stack meanwhile.
      declare
         Info_Addr : constant Unsigned_64 := Top - 16 - Siginfo_Size;
         Cont_Addr : constant Unsigned_64 :=
            (Info_Addr - UContext'Size / 8) and not 15;
         SP        : constant Unsigned_64 := Cont_Addr - Return_Slot;
         Frame_Len : constant Natural     := Natural (Top - SP);
         subtype Frame_Data is Thread_Stack (1 .. Frame_Len);
         package Trans is new Memory.Userland_Transfer (Frame_Data);
         Frame : Frame_Data := [others => 0];
         Info : Siginfo
            with Import,
                 Address => Frame'Address + Storage_Offset (Info_Addr - SP);
         Cont : UContext
            with Import,
                 Address => Frame'Address + Storage_Offset (Cont_Addr - SP);
      begin
         Info :=
            (Number => Unsigned_32 (Signal_Number),
             Errno  => 0,
             Code   => Info_Code,
             Pad    => 0,
             PID    => Info_PID,
             UID    => Info_UID,
             Status => Info_Status,
             Tail   => [others => 0]);

         --  For a hardware fault the union at byte 16 is si_addr.
         if Is_Fault then
            declare
               Fault_At : Unsigned_64
                  with Import,
                       Address => Frame'Address +
                          Storage_Offset (Info_Addr - SP) + 16;
            begin
               Fault_At := Fault_Addr;
            end;
         end if;

         --  si_value shares the union's slot with si_status, and at most one
         --  of the two is ever set: a signal queued with a value reports no
         --  status and a child's report carries no value. So the slot is
         --  written only when there is a value, a si_value of 0 reading as
         --  the 0 the frame already holds.
         if Info_Value /= 0 then
            declare
               Val_At : Unsigned_64
                  with Import,
                       Address => Frame'Address +
                          Storage_Offset (Info_Addr - SP) + 24;
            begin
               Val_At := Info_Value;
            end;
         end if;

         Cont (UC_Stack) :=
            Unsigned_64 (To_Integer (Thread_Pool (Th).User_Stack));
         Cont (UC_Stack + 1) := Thread_Pool (Th).User_Stack_Size;
         if Thread_Pool (Th).Is_Disabled or
            Thread_Pool (Th).User_Stack = System.Null_Address
         then
            Cont (UC_Stack + 2) := SS_DISABLE;
         elsif Was_On then
            --  What the return puts back, so that a nested handler gives
            --  the alternate stack back to the one it interrupted.
            Cont (UC_Stack + 2) := SS_ONSTACK;
         end if;
         Cont (UC_Sigmask) := Old_Mask;

         --  The interrupted state: the registers the thread is leaving the
         --  kernel with, and its floating point state as it stands.
         Arch.Context.Clone_FP_Context (Own_FP);
         #if ArchName = """x86_64-limine""" then
            declare
               FXSAVE : Thread_Stack (1 .. 512)
                  with Import, Address => Own_FP;
               FP_Mem : Thread_Stack (1 .. 512)
                  with Import, Address => Frame'Address +
                     Storage_Offset (Cont_Addr + UC_FP_Mem * 8 - SP);
            begin
               Cont (UC_Gregs .. UC_Gregs + 22) :=
                  [State.R8,  State.R9,  State.R10, State.R11, State.R12,
                   State.R13, State.R14, State.R15, State.RDI, State.RSI,
                   State.RBP, State.RSP, State.RBX, State.RDX, State.RCX,
                   State.RAX, State.RIP, State.CS,  State.RFLAGS,
                   State.Error_Code, 0, Old_Mask, 0];
               Cont (UC_FPRegs) := Cont_Addr + UC_FP_Mem * 8;
               FP_Mem := FXSAVE;
            end;

            --  x86 requires the return address in the stack.
            declare
               Return_Addr : Unsigned_64
                  with Import, Address => Frame'Address;
            begin
               Return_Addr := Unsigned_64 (To_Integer (Restorer));
            end;
         #elsif ArchName = """riscv64-limine""" then
            Cont (UC_Gregs .. UC_Gregs + 31) :=
               [State.SEPC, State.X1, State.X2, State.X3, State.X4,
                State.X5, State.X6, State.X7, State.X8, State.X9,
                State.X10, State.X11, State.X12, State.X13, State.X14,
                State.X15, State.X16, State.X17, State.X18, State.X19,
                State.X20, State.X21, State.X22, State.X23, State.X24,
                State.X25, State.X26, State.X27, State.X28, State.X29,
                State.X30, State.X31];
            Cont (UC_FPRegs .. UC_FPRegs + 32) :=
               [Own_FP.F0, Own_FP.F1, Own_FP.F2, Own_FP.F3, Own_FP.F4,
                Own_FP.F5, Own_FP.F6, Own_FP.F7, Own_FP.F8, Own_FP.F9,
                Own_FP.F10, Own_FP.F11, Own_FP.F12, Own_FP.F13, Own_FP.F14,
                Own_FP.F15, Own_FP.F16, Own_FP.F17, Own_FP.F18, Own_FP.F19,
                Own_FP.F20, Own_FP.F21, Own_FP.F22, Own_FP.F23, Own_FP.F24,
                Own_FP.F25, Own_FP.F26, Own_FP.F27, Own_FP.F28, Own_FP.F29,
                Own_FP.F30, Own_FP.F31, Own_FP.FCSR];
         #end if;
         Arch.Context.Destroy_FP_Context (Own_FP);

         Trans.Paste_Into_Userland
            (Map, Frame, To_Address (Integer_Address (SP)), Success);
         if not Success then
            return;
         end if;

         --  The thread goes back to userland at the handler itself, on the
         --  frame just written under it; the handler's own return goes to
         --  the restorer, which asks for the ucontext to be put back.
         #if ArchName = """x86_64-limine""" then
            State.RIP    := Unsigned_64 (To_Integer (Handle));
            State.RSP    := SP;
            State.RDI    := Signal_Number;
            State.RSI    := Info_Addr;
            State.RDX    := Cont_Addr;
            State.RAX    := 0;
            State.RFLAGS := State.RFLAGS and not Entry_Clear_RFLAGS;
         #elsif ArchName = """riscv64-limine""" then
            State.SEPC := Unsigned_64 (To_Integer (Handle));
            State.X2   := SP;
            State.X10  := Signal_Number;
            State.X11  := Info_Addr;
            State.X12  := Cont_Addr;
            State.X1   := Unsigned_64 (To_Integer (Restorer));
         #end if;
      end;

      if Use_Altsk then
         Thread_Pool (Th).User_Stack_Used := True;
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end Enter_Signal_Handler;

   procedure Build_Siginfo
      (Number : Unsigned_64;
       Code   : Unsigned_32;
       PID    : Unsigned_32;
       UID    : Unsigned_32;
       Status : Unsigned_32;
       Value  : Unsigned_64;
       Into   : out Siginfo_Bytes)
   is
      Info : Siginfo with Import, Address => Into'Address;
   begin
      Into := [others => 0];
      Info :=
         (Number => Unsigned_32 (Number),
          Errno  => 0,
          Code   => Code,
          Pad    => 0,
          PID    => PID,
          UID    => UID,
          Status => Status,
          Tail   => [others => 0]);

      --  si_value shares si_status's slot, and only one of the two is ever
      --  set, as when a handler is entered.
      if Value /= 0 then
         declare
            Val_At : Unsigned_64
               with Import, Address => Into'Address + Storage_Offset (24);
         begin
            Val_At := Value;
         end;
      end if;
   exception
      when Constraint_Error =>
         Into := [others => 0];
   end Build_Siginfo;

   procedure Leave_Signal_Handler
      (State    : in out Arch.Context.GP_Context;
       New_Mask : out Unsigned_64;
       Success  : out Boolean)
   is
      package Trans is new Memory.Userland_Transfer (UContext);
      Th   : constant TID := Arch.Local.Get_Current_Thread;
      Proc : constant Userland.Process.PID :=
         Arch.Local.Get_Current_Process;
      Own_FP : Arch.Context.FP_Context;
      Map    : Memory.MMU.Page_Table_Acc;
      Cont   : UContext;
      SP     : Unsigned_64;
   begin
      New_Mask := 0;
      Userland.Process.Get_Common_Map (Proc, Map);

      --  Both restorers are the bare system call with no stack adjustment
      --  of their own, so what the handler returned onto is the ucontext.
      #if ArchName = """x86_64-limine""" then
         SP := State.RSP;
      #elsif ArchName = """riscv64-limine""" then
         SP := State.X2;
      #end if;

      Trans.Take_From_Userland
         (Map, Cont, To_Address (Integer_Address (SP)), Success);
      if not Success then
         return;
      end if;

      #if ArchName = """x86_64-limine""" then
         --  The same requirement as entering one: a return address that is
         --  not userland's is a fault of the process, never of the kernel.
         if Cont (UC_Gregs + 16) >=
            Unsigned_64 (Arch.MMU.Canonical_Hole_Offset)
         then
            Success := False;
            return;
         end if;

         State.R8  := Cont (UC_Gregs);
         State.R9  := Cont (UC_Gregs + 1);
         State.R10 := Cont (UC_Gregs + 2);
         State.R11 := Cont (UC_Gregs + 3);
         State.R12 := Cont (UC_Gregs + 4);
         State.R13 := Cont (UC_Gregs + 5);
         State.R14 := Cont (UC_Gregs + 6);
         State.R15 := Cont (UC_Gregs + 7);
         State.RDI := Cont (UC_Gregs + 8);
         State.RSI := Cont (UC_Gregs + 9);
         State.RBP := Cont (UC_Gregs + 10);
         State.RSP := Cont (UC_Gregs + 11);
         State.RBX := Cont (UC_Gregs + 12);
         State.RDX := Cont (UC_Gregs + 13);
         State.RCX := Cont (UC_Gregs + 14);
         State.RAX := Cont (UC_Gregs + 15);
         State.RIP := Cont (UC_Gregs + 16);

         --  The segments are never taken from userland, and neither are the
         --  flags the processor does not let a program set: a handler that
         --  rewrote its own ucontext must not be able to give itself
         --  interrupts off or a higher I/O privilege level.
         State.RFLAGS := (State.RFLAGS and not User_RFLAGS) or
                         (Cont (UC_Gregs + 18) and User_RFLAGS);

         --  The floating point state in the ucontext, laid over a save of
         --  the current one, so that whatever the 512-byte image does not
         --  describe stays consistent.
         Arch.Context.Clone_FP_Context (Own_FP);
         declare
            FXSAVE : Thread_Stack (1 .. 512)
               with Import, Address => Own_FP;
            FP_Mem : Thread_Stack (1 .. 512)
               with Import, Address => Cont (UC_FP_Mem)'Address;
         begin
            FXSAVE := FP_Mem;
         end;
         Arch.Context.Load_FP_Context (Own_FP);
         Arch.Context.Destroy_FP_Context (Own_FP);
      #elsif ArchName = """riscv64-limine""" then
         if Cont (UC_Gregs) >=
            Unsigned_64 (Arch.MMU.Canonical_Hole_Offset)
         then
            Success := False;
            return;
         end if;

         State.SEPC := Cont (UC_Gregs);
         State.X1   := Cont (UC_Gregs + 1);
         State.X2   := Cont (UC_Gregs + 2);
         State.X3   := Cont (UC_Gregs + 3);
         State.X4   := Cont (UC_Gregs + 4);
         State.X5   := Cont (UC_Gregs + 5);
         State.X6   := Cont (UC_Gregs + 6);
         State.X7   := Cont (UC_Gregs + 7);
         State.X8   := Cont (UC_Gregs + 8);
         State.X9   := Cont (UC_Gregs + 9);
         State.X10  := Cont (UC_Gregs + 10);
         State.X11  := Cont (UC_Gregs + 11);
         State.X12  := Cont (UC_Gregs + 12);
         State.X13  := Cont (UC_Gregs + 13);
         State.X14  := Cont (UC_Gregs + 14);
         State.X15  := Cont (UC_Gregs + 15);
         State.X16  := Cont (UC_Gregs + 16);
         State.X17  := Cont (UC_Gregs + 17);
         State.X18  := Cont (UC_Gregs + 18);
         State.X19  := Cont (UC_Gregs + 19);
         State.X20  := Cont (UC_Gregs + 20);
         State.X21  := Cont (UC_Gregs + 21);
         State.X22  := Cont (UC_Gregs + 22);
         State.X23  := Cont (UC_Gregs + 23);
         State.X24  := Cont (UC_Gregs + 24);
         State.X25  := Cont (UC_Gregs + 25);
         State.X26  := Cont (UC_Gregs + 26);
         State.X27  := Cont (UC_Gregs + 27);
         State.X28  := Cont (UC_Gregs + 28);
         State.X29  := Cont (UC_Gregs + 29);
         State.X30  := Cont (UC_Gregs + 30);
         State.X31  := Cont (UC_Gregs + 31);

         --  SSTATUS is the kernel's and is never taken from userland.
         Own_FP :=
            (F0   => Cont (UC_FPRegs),      F1  => Cont (UC_FPRegs + 1),
             F2   => Cont (UC_FPRegs + 2),  F3  => Cont (UC_FPRegs + 3),
             F4   => Cont (UC_FPRegs + 4),  F5  => Cont (UC_FPRegs + 5),
             F6   => Cont (UC_FPRegs + 6),  F7  => Cont (UC_FPRegs + 7),
             F8   => Cont (UC_FPRegs + 8),  F9  => Cont (UC_FPRegs + 9),
             F10  => Cont (UC_FPRegs + 10), F11 => Cont (UC_FPRegs + 11),
             F12  => Cont (UC_FPRegs + 12), F13 => Cont (UC_FPRegs + 13),
             F14  => Cont (UC_FPRegs + 14), F15 => Cont (UC_FPRegs + 15),
             F16  => Cont (UC_FPRegs + 16), F17 => Cont (UC_FPRegs + 17),
             F18  => Cont (UC_FPRegs + 18), F19 => Cont (UC_FPRegs + 19),
             F20  => Cont (UC_FPRegs + 20), F21 => Cont (UC_FPRegs + 21),
             F22  => Cont (UC_FPRegs + 22), F23 => Cont (UC_FPRegs + 23),
             F24  => Cont (UC_FPRegs + 24), F25 => Cont (UC_FPRegs + 25),
             F26  => Cont (UC_FPRegs + 26), F27 => Cont (UC_FPRegs + 27),
             F28  => Cont (UC_FPRegs + 28), F29 => Cont (UC_FPRegs + 29),
             F30  => Cont (UC_FPRegs + 30), F31 => Cont (UC_FPRegs + 31),
             FCSR => Cont (UC_FPRegs + 32));
         Arch.Context.Load_FP_Context (Own_FP);
         Arch.Context.Destroy_FP_Context (Own_FP);
      #end if;

      --  The mask the handler interrupted goes back with it, and so does
      --  whether that was on the alternate stack, which is what gives a
      --  nested handler's stack back to the one it interrupted.
      New_Mask := Cont (UC_Sigmask);
      Thread_Pool (Th).User_Stack_Used :=
         (Cont (UC_Stack + 2) and SS_ONSTACK) /= 0;
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end Leave_Signal_Handler;
   ----------------------------------------------------------------------------
   procedure Get_Load_Averages (Avg_1, Avg_5, Avg_15 : out Unsigned_32) is
      pragma Warnings (Off, "handler can never be entered", Reason => "Bug");
   begin
      Synchronization.Seize (Scheduler_Mutex);
      Avg_1 := Buckets (Buckets'First) * 100;

      Avg_5 := 0;
      for Val of Buckets (Buckets'First .. Buckets'First + 4) loop
         Avg_5 := Avg_5 + (Val * 100);
      end loop;
      Avg_5 := Avg_5 / 5;

      Avg_15 := 0;
      for Val of Buckets loop
         Avg_15 := Avg_15 + (Val * 100);
      end loop;
      Avg_15 := Avg_15 / 15;

      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
         Avg_1  := 0;
         Avg_5  := 0;
         Avg_15 := 0;
   end Get_Load_Averages;
   ----------------------------------------------------------------------------
   procedure Scheduler_ISR (State : in out Arch.Context.GP_Context) is
      Core        : constant Positive := Arch.Local.Get_Core_Number;
      Current_TID : constant TID := Arch.Local.Get_Current_Thread;
      Retiring    : constant TID := Arch.Local.Get_Retiring_Thread;
      Next_TID    :          TID := Error_TID;
      Timeout     : Natural;
      Curr        : Time.Timestamp;
      Count       : Unsigned_32;
      Did_Seize : Boolean;
      Discard   : Boolean;
      Can_Run   : Boolean;
      Is_Asleep : Boolean;
   begin
      --  A switch leaves through a frame on the old thread's kernel stack, so
      --  the old thread stays running until the entry the switch asks for,
      --  which comes in on the new thread's stack: it lets the old thread go
      --  and arms the new one's time. A new thread that has gone meanwhile
      --  is switched away from at once.
      --
      --  This entry is also the first moment no core is on the kernel stack
      --  of a thread that has gone, so that stack is given back here. It goes
      --  under the lock and before the thread is let go, as a slot let go is
      --  taken by the next thread made, and a stack still in it would be
      --  handed over. When the lock is busy, the thread stays retiring and is
      --  let go at the next entry, which is asked for soon.
      if Retiring /= Error_TID then
         if not Thread_Pool (Retiring).Is_Present and then
            Thread_Pool (Retiring).Kernel_Stack /= null
         then
            Synchronization.Try_Seize (Scheduler_Mutex, Did_Seize);
            if not Did_Seize then
               Arch.Local.Reschedule_In (Retire_Retry_Micros);
               return;
            end if;
            Free_Kernel_Stack (Thread_Pool (Retiring).Kernel_Stack);
            Thread_Pool (Retiring).Is_Running := False;
            Synchronization.Release (Scheduler_Mutex);
         else
            Thread_Pool (Retiring).Is_Running := False;
         end if;
         Arch.Local.Set_Retiring_Thread (Error_TID);

         --  Waiting cores do not look for threads until their own time, so
         --  one that could still run is given to them.
         Evaluate_Runnable (Retiring, Can_Run);
         if Can_Run then
            Kick (Retiring);
         end if;
         if Current_TID /= Error_TID and then
            not Thread_Pool (Current_TID).Is_Present
         then
            Arch.Local.Reschedule_ASAP;
         else
            Arch.Local.Reschedule_In
               (if Current_TID /= Error_TID
                then Thread_Pool (Current_TID).RR_Micro_Inter
                else Fast_Reschedule_Micros);
         end if;
         return;
      end if;

      Arch.Clocks.Get_Monotonic_Time (Curr);

      Synchronization.Try_Seize (Scheduler_Mutex, Did_Seize);
      if not Did_Seize then
         Arch.Local.Reschedule_In
            (if Current_TID /= Error_TID and then
                not Thread_Pool (Current_TID).Is_Present
             then Retire_Retry_Micros
             else Fast_Reschedule_Micros);
         return;
      end if;

      --  Adjust the moving stats if at least a minute has passed since
      --  last poll.
      if Curr.Seconds >= Last_Bucket + 60 then
         Last_Bucket := Curr.Seconds;
         Count := 0;
         for I in Thread_Pool'First .. Thread_Pool'Last loop
            if Thread_Pool (I).Is_Present then
               Count := Count + 1;
            end if;
         end loop;

         for I in reverse Buckets'First .. Buckets'Last - 1 loop
            Buckets (I + 1) := Buckets (I);
         end loop;
         Buckets (Buckets'First) := Count;
      end if;

      --  Find the next thread. One that is gone or asleep is not kept on the
      --  core by its policy while any other can run.
      if Current_TID = Error_TID then
         Next_From_Nothing (Timeout, Next_TID);
      else
         case Thread_Pool (Current_TID).Pol is
            when Policy_FIFO  => Next_FIFO (Current_TID, Timeout, Next_TID);
            when Policy_Other => Next_Other (Current_TID, Timeout, Next_TID);
            when Policy_RR    => Next_RR (Current_TID, Timeout, Next_TID);
         end case;
         if Next_TID = Error_TID and
            Thread_Pool (Current_TID).Pol /= Policy_Other
         then
            Evaluate_Suspended (Current_TID, Is_Asleep);
            if Is_Asleep or not Thread_Pool (Current_TID).Is_Present then
               Next_Other (Current_TID, Timeout, Next_TID);
            end if;
         end if;
      end if;

      if Next_TID = Error_TID then
         if Current_TID /= Error_TID and then
            Thread_Pool (Current_TID).Is_Present and then
            Thread_Pool (Current_TID).Is_Doomed and then
            Arch.Context.Is_User_Context (State)
         then
            Drop_Thread (Current_TID);
         end if;

         if Current_TID /= Error_TID and then
            not Thread_Pool (Current_TID).Is_Present
         then
            Discard := Memory.MMU.Make_Active (Memory.MMU.Kernel_Table);
            Thread_Pool (Current_TID).PageMap :=
               Memory.MMU.Get_Curr_Table_Addr;
            Arch.Local.Set_Retiring_Thread (Current_TID);
            Arch.Local.Set_Current_Thread (Error_TID);
            Arch.Local.Set_Current_Process (Userland.Process.Error_PID);
            Arch.Context.Init_Kernel_GP_Context
               (State, Arch.Local.Get_Idle_Stack, Waiting_Spot'Address);
            Arch.Local.Reschedule_ASAP;
            Synchronization.Release (Scheduler_Mutex);
            return;
         end if;
         Synchronization.Release (Scheduler_Mutex);
         Arch.Local.Reschedule_In (Timeout);
         return;
      end if;

      if Current_TID /= Error_TID then
         if Current_TID /= Next_TID then
            Arch.Local.Set_Retiring_Thread (Current_TID);
         end if;
         if Thread_Pool (Current_TID).Is_Present and then
            Thread_Pool (Current_TID).Is_Doomed and then
            Arch.Context.Is_User_Context (State)
         then
            Drop_Thread (Current_TID);
         elsif Thread_Pool (Current_TID).Is_Present then
            Thread_Pool (Current_TID).PageMap := MMU.Get_Curr_Table_Addr;
            Thread_Pool (Current_TID).TCB_Pointer := Arch.Local.Fetch_TCB;
            Thread_Pool (Current_TID).GP_State    := State;
            Arch.Context.Save_FP_Context (Thread_Pool (Current_TID).FP_State);
         end if;
      end if;

      --  A thread left behind is let go by the entry asked for here, which
      --  arms the new thread's time; with none left, it is armed now.
      if Arch.Local.Get_Retiring_Thread /= Error_TID then
         Arch.Local.Reschedule_ASAP;
      else
         Arch.Local.Reschedule_In (Timeout);
      end if;

      --  Reset state.
      Memory.MMU.Set_Table_Addr (Thread_Pool (Next_TID).PageMap);
      Arch.Local.Set_Current_Process (Thread_Pool (Next_TID).Process);
      Arch.Local.Set_Current_Thread (Next_TID);
      Thread_Pool (Next_TID).Is_Running := True;
      Thread_Pool (Next_TID).Core       := Core;
      Set_Waiting (False);
      Arch.Local.Set_Stacks
         (Thread_Pool (Next_TID).Kernel_Stack.all'Address +
          Kernel_Stack'Length);
      Arch.Context.Load_FP_Context (Thread_Pool (Next_TID).FP_State);
      State := Thread_Pool (Next_TID).GP_State;
      Arch.Local.Load_TCB (State, Thread_Pool (Next_TID).TCB_Pointer);
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Exception while reescheduling");
   end Scheduler_ISR;
   ----------------------------------------------------------------------------
   function Convert (Thread : TID) return Natural is
   begin
      return Natural (Thread);
   end Convert;

   function Convert (Value : Natural) return TID is
   begin
      return TID (Value);
   exception
      when Constraint_Error =>
         return Error_TID;
   end Convert;

   function Is_Alive (Thread : TID; PID : Natural) return Boolean is
   begin
      return Thread /= Error_TID                 and then
             Thread_Pool (Thread).Is_Present     and then
             Userland.Process.Convert (Thread_Pool (Thread).Process) = PID;
   exception
      when Constraint_Error =>
         return False;
   end Is_Alive;

   function Get_Thread_Process (Thread : TID) return Natural is
   begin
      if Thread = Error_TID or else not Thread_Pool (Thread).Is_Present then
         return 0;
      end if;
      return Userland.Process.Convert (Thread_Pool (Thread).Process);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Thread_Process;

   procedure List_All (List : out Thread_Listing_Arr; Total : out Natural) is
      Curr_Index : Natural := 0;
   begin
      List  := [others => (Error_TID, 0)];
      Total := 0;

      Synchronization.Seize (Scheduler_Mutex);
      for I in Thread_Pool.all'Range loop
         if Thread_Pool (I).Is_Present then
            Total := Total + 1;
            if Curr_Index < List'Length then
               List (List'First + Curr_Index) :=
                  (I, Userland.Process.Convert (Thread_Pool (I).Process));
               Curr_Index := Curr_Index + 1;
            end if;
         end if;
      end loop;
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
         Total := 0;
   end List_All;
   ----------------------------------------------------------------------------
   procedure Next_From_Nothing (Timeout : out Natural; Next : out TID) is
      Can_Run : Boolean;
   begin
      --  Just loop around all threads searching for something to schedule.
      for I in Thread_Pool'Range loop
         Evaluate_Runnable (I, Can_Run);
         if Can_Run then
            Next := I;
            Timeout := Thread_Pool (Next).RR_Micro_Inter;
            return;
         end if;
      end loop;

      Next_Wake (Idle_Reschedule_Micros, Timeout);
      Next := Error_TID;
   exception
      when Constraint_Error =>
         Timeout := Fast_Reschedule_Micros;
         Next := Error_TID;
   end Next_From_Nothing;

   procedure Next_FIFO (Curr : TID; Timeout : out Natural; Next : out TID) is
      Curr_Prio : Priority;
      FIFO_Count : Natural := 0;
      Can_Run : Boolean;
   begin
      --  We want to check for other FIFO threads with greater priority, if
      --  we find any, we move to the highest priority. Otherwise, we stay.
      Curr_Prio := Thread_Pool (Curr).Prio;
      Next := Error_TID;
      Timeout := Thread_Pool (Curr).RR_Micro_Inter;
      for I in Thread_Pool'Range loop
         Evaluate_Runnable (I, Can_Run);
         if Can_Run and (Thread_Pool (I).Pol = Policy_FIFO) then
            FIFO_Count := FIFO_Count + 1;
            if Thread_Pool (I).Prio > Curr_Prio then
               Curr_Prio := Thread_Pool (I).Prio;
               Next := I;
               Timeout := Thread_Pool (I).RR_Micro_Inter;
            end if;
         end if;
      end loop;

      --  If we find no present FIFO threads, including us, that means we are
      --  the last exited FIFO thread, thus, we schedule from nothing.
      if FIFO_Count = 0 then
         Next_From_Nothing (Timeout, Next);
      end if;
   exception
      when Constraint_Error =>
         Timeout := Fast_Reschedule_Micros;
         Next := Error_TID;
   end Next_FIFO;

   procedure Next_RR (Curr : TID; Timeout : out Natural; Next : out TID) is
      Curr_Prio : Priority;
      RR_Count : Natural := 0;
      RR_Equal_Prio_Count : Natural := 0;
      Can_Run : Boolean;
   begin
      --  We want to check for other RR threads with greater priority, if
      --  we find any, we move to the highest priority. Otherwise, we stay.
      --  We also find total RR threads and threads with same priority.
      Curr_Prio := Thread_Pool (Curr).Prio;
      Next := Error_TID;
      Timeout := Thread_Pool (Curr).RR_Micro_Inter;
      for I in Thread_Pool'Range loop
         Evaluate_Runnable (I, Can_Run);
         if Can_Run and Thread_Pool (I).Pol = Policy_RR then
            RR_Count := RR_Count + 1;
            if Thread_Pool (I).Prio > Curr_Prio then
               Curr_Prio := Thread_Pool (I).Prio;
               Next := I;
               Timeout := Thread_Pool (I).RR_Micro_Inter;
            elsif Thread_Pool (I).Prio = Curr_Prio then
               RR_Equal_Prio_Count := RR_Equal_Prio_Count + 1;
            end if;
         end if;
      end loop;
      if Next /= Error_TID then
         return;
      end if;

      --  If we find no present RR threads, including us, that means we are
      --  the last exited RR thread, thus, we schedule from nothing.
      if RR_Count = 0 then
         Next_From_Nothing (Timeout, Next);
         return;
      end if;

      --  We did not find higher priority and there are more RR threads apart
      --  from us with same prio, so lets just go for the next one numerically
      --  to avoid deadlocks.
      if Next = Error_TID and RR_Equal_Prio_Count /= 0 then
         for I in Curr + 1 .. Thread_Pool'Last loop
            Evaluate_Runnable (I, Can_Run);
            if Can_Run and Thread_Pool (I).Pol = Policy_RR and
               Thread_Pool (I).Prio = Curr_Prio
            then
               Next := I;
               Timeout := Thread_Pool (I).RR_Micro_Inter;
               return;
            end if;
         end loop;
         for I in Thread_Pool'First .. Curr - 1 loop
            Evaluate_Runnable (I, Can_Run);
            if Can_Run and Thread_Pool (I).Pol = Policy_RR and
               Thread_Pool (I).Prio = Curr_Prio
            then
               Next := I;
               Timeout := Thread_Pool (I).RR_Micro_Inter;
               return;
            end if;
         end loop;
      end if;

      --  If we did not find equal prio alternatives yet we have alternatives,
      --  we just pick an arbitrary lower prio.
      if Next = Error_TID and RR_Count > 1 then
         for I in Curr + 1 .. Thread_Pool'Last loop
            Evaluate_Runnable (I, Can_Run);
            if Can_Run and (Thread_Pool (I).Pol = Policy_RR) then
               Next := I;
               Timeout := Thread_Pool (I).RR_Micro_Inter;
               return;
            end if;
         end loop;
         for I in Thread_Pool'First .. Curr - 1 loop
            Evaluate_Runnable (I, Can_Run);
            if Can_Run and (Thread_Pool (I).Pol = Policy_RR) then
               Next := I;
               Timeout := Thread_Pool (I).RR_Micro_Inter;
               return;
            end if;
         end loop;
      end if;
   exception
      when Constraint_Error =>
         Timeout := Fast_Reschedule_Micros;
         Next := Error_TID;
   end Next_RR;

   procedure Next_Other (Curr : TID; Timeout : out Natural; Next : out TID) is
      Can_Run : Boolean;
   begin
      --  We want to check if there is a real time policy thread that we can
      --  pick into.
      for I in Thread_Pool'Range loop
         Evaluate_Runnable (I, Can_Run);
         if Can_Run and then
            (Thread_Pool (I).Pol = Policy_FIFO or
             Thread_Pool (I).Pol = Policy_RR)
         then
            Next := I;
            Timeout := Thread_Pool (I).RR_Micro_Inter;
            return;
         end if;
      end loop;

      --  Just RR into the next Policy_Other thread.
      for I in Curr + 1 .. Thread_Pool'Last loop
         Evaluate_Runnable (I, Can_Run);
         if Can_Run then
            Next := I;
            Timeout := Thread_Pool (I).RR_Micro_Inter;
            return;
         end if;
      end loop;
      for I in Thread_Pool'First .. Curr - 1 loop
         Evaluate_Runnable (I, Can_Run);
         if Can_Run then
            Next := I;
            Timeout := Thread_Pool (I).RR_Micro_Inter;
            return;
         end if;
      end loop;

      --  We did not find anything so we just set our own values.
      Next := Error_TID;
      Timeout := Thread_Pool (Curr).RR_Micro_Inter;
   exception
      when Constraint_Error =>
         Timeout := Fast_Reschedule_Micros;
         Next := Error_TID;
   end Next_Other;

   procedure Waiting_Spot is
      Micros : Natural;
   begin
      loop
         Arch.Snippets.Disable_Interrupts;
         Synchronization.Seize (Scheduler_Mutex);
         Set_Waiting (True);
         Next_Wake
            ((if Is_Retire_Pending then Retire_Retry_Micros
              else Idle_Reschedule_Micros), Micros);
         Synchronization.Release (Scheduler_Mutex);
         Arch.Local.Reschedule_In (Micros);
         Arch.Snippets.Enable_Interrupts_And_Wait;
      end loop;
   end Waiting_Spot;

   procedure Wait_If_Suspended (Thread : TID) is
      Stop   : Boolean;
      Curr   : Time.Timestamp;
      Left   : Time.Timestamp;
      Micros : Natural;
   begin
      --  The thread is checked and the core marked waiting under the lock a
      --  wake takes, so a wake either ends the suspension before the check
      --  or finds the core waiting and interrupts it, see Kick. It is asked
      --  what the pickers ask (Evaluate_Suspended), so a signal it can take
      --  ends the sleep too.
      Arch.Snippets.Disable_Interrupts;
      Synchronization.Seize (Scheduler_Mutex);
      Time.Get_Time (Thread_Pool (Thread).Start_Clock, Curr);
      Stop := Thread_Pool (Thread).Start_Time > Curr and then
              not Userland.Process.Has_Deliverable_Signal
                 (Thread_Pool (Thread).Process, Thread,
                  Thread_Pool (Thread).Masked_Signals);
      if Stop then
         --  The core also wakes for the other threads, as an idle one does.
         Next_Wake
            ((if Is_Retire_Pending then Retire_Retry_Micros
              else Idle_Reschedule_Micros), Micros);
         Left   := Thread_Pool (Thread).Start_Time - Curr;
         if Left.Seconds = 0 and
            Left.Nanoseconds < Unsigned_64 (Micros) * 1_000
         then
            Micros := Natural (Left.Nanoseconds / 1_000) + 1;
         end if;
         Set_Waiting (True);
      end if;
      Synchronization.Release (Scheduler_Mutex);

      if Stop then
         Arch.Local.Reschedule_In (Micros);
         Arch.Snippets.Enable_Interrupts_And_Wait;
         Arch.Snippets.Disable_Interrupts;
         Set_Waiting (False);
         Arch.Snippets.Enable_Interrupts;
      else
         Arch.Snippets.Enable_Interrupts;
      end if;
   exception
      when Constraint_Error =>
         --  The index check fires with the lock held.
         Synchronization.Release (Scheduler_Mutex);
         Arch.Snippets.Enable_Interrupts;
   end Wait_If_Suspended;

   procedure Next_Wake (Max_Micros : Natural; Micros : out Natural) is
      Mono : Time.Timestamp;
      Curr : Time.Timestamp;
      Left : Time.Timestamp;
   begin
      Micros := Max_Micros;
      Time.Get_Time (Time.Monotonic_Clock, Mono);
      for I in Thread_Pool'Range loop
         declare
            T : Thread_Info renames Thread_Pool (I);
         begin
            if T.Is_Present and not T.Is_Running and not T.Is_Held then
               if T.Start_Clock = Time.Monotonic_Clock then
                  Curr := Mono;
               else
                  Time.Get_Time (T.Start_Clock, Curr);
               end if;

               --  One with a signal it can take is not asleep, as the
               --  pickers see it (Evaluate_Suspended).
               if T.Start_Time > Curr and then
                  not Userland.Process.Has_Deliverable_Signal
                     (T.Process, I, T.Masked_Signals)
               then
                  Left := T.Start_Time - Curr;
                  if Left.Seconds = 0 and
                     Left.Nanoseconds < Unsigned_64 (Micros) * 1_000
                  then
                     Micros := Natural (Left.Nanoseconds / 1_000) + 1;
                  end if;
               else
                  --  One made runnable since the last search for a thread,
                  --  which a kick may have missed, is due at once.
                  Micros := 1;
               end if;
            end if;
         end;
      end loop;
   exception
      when Constraint_Error =>
         Micros := Max_Micros;
   end Next_Wake;

   procedure Set_Waiting (Value : Boolean) is
      Core : constant Positive := Arch.Local.Get_Core_Number;
   begin
      if Core in Is_Waiting'Range then
         Is_Waiting (Core) := Value;
      end if;
   exception
      when Constraint_Error =>
         null;
   end Set_Waiting;

   function Is_Retire_Pending return Boolean is
      Current : constant TID := Arch.Local.Get_Current_Thread;
   begin
      return Arch.Local.Get_Retiring_Thread /= Error_TID or else
             (Current /= Error_TID and then
              not Thread_Pool (Current).Is_Present);
   exception
      when Constraint_Error =>
         return False;
   end Is_Retire_Pending;

   procedure Kick (Thread : TID) is
      Self : constant Positive := Arch.Local.Get_Core_Number;
      Core : Natural := 0;
   begin
      --  A thread running on a waiting core is woken there, and one that is
      --  not running is given to any waiting core. A kicked core counts as
      --  waiting no more, so that the wakes of a burst go to several.
      if Thread_Pool (Thread).Is_Running then
         Core := Thread_Pool (Thread).Core;
      else
         for C in Is_Waiting'Range loop
            if C /= Self and then Is_Waiting (C) then
               Core := C;
               exit;
            end if;
         end loop;
      end if;

      if Core in Is_Waiting'Range and then Core /= Self and then
         Is_Waiting (Core)
      then
         Is_Waiting (Core) := False;
         Arch.Local.Reschedule_Core (Core);
      end if;
   exception
      when Constraint_Error =>
         null;
   end Kick;

   procedure Evaluate_Runnable (T : TID; Can_Run : out Boolean) is
   begin
      Can_Run := Thread_Pool (T).Is_Present and
                 not Thread_Pool (T).Is_Running and
                 not Thread_Pool (T).Is_Held;

      if Can_Run and then Thread_Pool (T).Is_Doomed and then
         Arch.Context.Is_User_Context (Thread_Pool (T).GP_State)
      then
         Drop_Thread (T);
         Can_Run := False;
      elsif Can_Run then
         Evaluate_Suspended (T, Can_Run);
         Can_Run := not Can_Run;
      end if;
   exception
      when Constraint_Error =>
         Can_Run := False;
   end Evaluate_Runnable;

   procedure Evaluate_Suspended (T : TID; Suspended : out Boolean) is
      Curr : Time.Timestamp;
   begin
      Time.Get_Time (Thread_Pool (T).Start_Clock, Curr);
      Suspended := Thread_Pool (T).Start_Time > Curr;

      --  A sleeping thread is not scheduled at all, so one with a signal it
      --  can take has to stop being suspended, or it would never run to give
      --  its wait up and let the signal be delivered on the way out. The
      --  signal is looked for without a lock, see Has_Deliverable_Signal.
      if Suspended and then
         Userland.Process.Has_Deliverable_Signal
            (Thread_Pool (T).Process, T, Thread_Pool (T).Masked_Signals)
      then
         Suspended := False;
      end if;
   exception
      when Constraint_Error =>
         Suspended := False;
   end Evaluate_Suspended;

   procedure Drop_Thread (Thread : TID) is
   begin
      Thread_Pool (Thread).Is_Present := False;
      Thread_Pool (Thread).Is_Doomed  := False;
      Arch.Context.Destroy_FP_Context (Thread_Pool (Thread).FP_State);
      if not Thread_Pool (Thread).Is_Running then
         Free_Kernel_Stack (Thread_Pool (Thread).Kernel_Stack);
      end if;
   exception
      when Constraint_Error =>
         null;
   end Drop_Thread;
end Scheduler;
