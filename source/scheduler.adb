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
   type Thread_Stack is array (Natural range <>) of Unsigned_8;
   type Thread_Stack_64 is array (Natural range <>) of Unsigned_64;
   --  A stack's top is the address one past its last byte, and the psABIs
   --  (x86-64 1.0, RISC-V 1.0) want it 16-byte aligned.
   type Kernel_Stack is array (1 ..  Memory.Kernel_Stack_Size) of Unsigned_8
      with Alignment => 16;
   type Kernel_Stack_Acc is access Kernel_Stack;

   type Thread_Info is record
      Is_Present      : Boolean with Atomic;
      Is_Running      : Boolean with Atomic;
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

   procedure Init (Success : out Boolean) is
   begin
      --  Initialize registries.
      Thread_Pool := new Thread_Info_Arr'
         [others =>
            (Is_Present      => False,
             Is_Running      => False,
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
             Process         => Userland.Process.Error_PID,
             System_Runtime  => (0, 0),
             User_Runtime    => (0, 0),
             System_Tmp      => (0, 0),
             User_Tmp        => (0, 0),
             User_Stack      => System.Null_Address,
             User_Stack_Size => 0,
             User_Stack_Used => False,
             Is_Disabled     => True,
             Start_Clock     => Time.Monotonic_Clock,
             Start_Time      => (0, 0),
             Wake_Pending    => False,
             Event_Waiting   => False)];
      Waits := new Wait_Info_Arr'
         [others => (Count => 0, Keys => [others => System.Null_Address])];

      Is_Initialized := True;
      Synchronization.Release (Scheduler_Mutex);
      Success := True;
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
   begin
      New_TID := Error_TID;
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
          Process        => Userland.Process.Convert (PID),
          User_Stack      => System.Null_Address,
          User_Stack_Size => 0,
          User_Stack_Used => False,
          Is_Disabled     => True,
          Start_Clock     => Time.Monotonic_Clock,
          Start_Time      => (0, 0),
          Wake_Pending    => False,
          Event_Waiting   => False,
          System_Runtime  => (0, 0),
          User_Runtime    => (0, 0),
          System_Tmp      => (0, 0),
          User_Tmp        => (0, 0));

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
   exception
      when Constraint_Error =>
         null;
   end Release_Thread;

   procedure Delete_Thread (Thread : TID) is
   begin
      Synchronization.Seize (Scheduler_Mutex);
      if Thread_Pool (Thread).Is_Present then
         Thread_Pool (Thread).Is_Present := False;
         Arch.Context.Destroy_FP_Context (Thread_Pool (Thread).FP_State);
      end if;
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
   end Delete_Thread;

   procedure Yield_If_Able is
      Curr_TID : constant     TID := Arch.Local.Get_Current_Thread;
      Is_Init  : constant Boolean := Is_Initialized;
   begin
      if Is_Init and Curr_TID /= Error_TID then
         Arch.Local.Reschedule_ASAP;
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
      return Thread /= Error_TID and then not Thread_Pool (Thread).Is_Present;
   exception
      when Constraint_Error =>
         return False;
   end Is_Doomed;

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
      Thread_Pool (Thread).Start_Clock := Clock;
      Thread_Pool (Thread).Start_Time := Start_Time;
      Synchronization.Release (Scheduler_Mutex);

      loop
         Synchronization.Seize (Scheduler_Mutex);
         Evaluate_Suspended (Thread, Stop);
         Synchronization.Release (Scheduler_Mutex);
         exit when not Stop;
         Scheduler.Yield_If_Able;
      end loop;
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
      if not Thread_Pool (Thread).Wake_Pending then
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

      Synchronization.Seize (Scheduler_Mutex);
      Thread_Pool (Thread).Event_Waiting := False;
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
               if Thread_Pool (T).Event_Waiting then
                  Thread_Pool (T).Start_Clock := Time.Monotonic_Clock;
                  Thread_Pool (T).Start_Time  := (0, 0);
               end if;
               Synchronization.Release (Scheduler_Mutex);
               exit;
            end if;
         end loop;
      end loop;
      Synchronization.Release (Wait_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Wait_Mutex);
   end Wake_Event;

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

   procedure Launch_Signal_Thread
      (Signal_Number    : Unsigned_64;
       Handle, Restorer : System.Address;
       Is_Altstack      : Boolean;
       Success          : out Boolean)
   is
      Stack_Size : Unsigned_64 := Kernel_Stack_Size;
      GP_State  : Arch.Context.GP_Context;
      FP_State  : Arch.Context.FP_Context;
      New_TID   : TID;
      Stack_Top : Unsigned_64;
      Map       : Memory.MMU.Page_Table_Acc;
      Use_Altsk : Boolean;
      Killed    : Boolean := False;
      Discard   : Boolean;
      Th        : constant TID := Arch.Local.Get_Current_Thread;
      Proc : constant Userland.Process.PID := Arch.Local.Get_Current_Process;
   begin
      Userland.Process.Get_Common_Map (Proc, Map);

      --  Initialize signal stack. We either start by mapping a new user stack
      --  or we use to passed one.
      Use_Altsk :=
         Is_Altstack and
         Thread_Pool (Th).User_Stack /= System.Null_Address and
         not Thread_Pool (Th).Is_Disabled;

      if Use_Altsk then
         Thread_Pool (Th).User_Stack_Used := True;
         Stack_Top  := Unsigned_64 (To_Integer (Thread_Pool (Th).User_Stack));
         Stack_Size := Thread_Pool (Th).User_Stack_Size;
      else
         Userland.Process.Bump_Alloc_Base (Proc, Stack_Size, Stack_Top);
         Memory.MMU.Map_Allocated_Range
            (Map           => Map,
             Virtual_Start => To_Address (Virtual_Address (Stack_Top)),
             Length        => Storage_Offset (Stack_Size),
             Permissions   => Tmp_Stack_Permissions,
             Success       => Success);
         if not Success then
            return;
         end if;
      end if;

      declare
         type Siginfo is record
            Signal_Number : Unsigned_32;
            Signal_Code   : Unsigned_32;
            Signal_Errno  : Unsigned_32;
            Signal_PID    : Unsigned_32;
            Signal_UID    : Unsigned_32;
            Signal_Addr   : Unsigned_64;
            Signal_Status : Unsigned_32;
            Sival_Pointer : Unsigned_64;
         end record;

         type U64_Arr is array (Natural range <>) of Unsigned_64;
         type MContext is record
            Old_Mask        : Unsigned_64;
            Regs            : U64_Arr (1 .. 16);
            PC, PR, SR      : Unsigned_64;
            GBR, Mach, Macl : Unsigned_64;
            FPRegs          : U64_Arr (1 .. 16);
            XFPregs         : U64_Arr (1 .. 16);
            FPSCR, FPul, FP : Unsigned_32;
         end record;

         type UContext is record
            Link    : Unsigned_64;
            Stack   : Unsigned_64;
            Context : MContext;
            Sigmask : Unsigned_64;
         end record;

         Sz     : constant Natural := Natural (Stack_Size);
         Stk_64 : Thread_Stack_64 (1 .. Sz / 8)
            with Import, Address => To_Address (Virtual_Address (Stack_Top));
         Info_Idx : constant Natural := Stk_64'Last - (Siginfo'Size / 64) - 1;
         Cont_Idx : constant Natural := Info_Idx - (UContext'Size / 64) - 1;
         Index_64 : Natural := Cont_Idx;
         Info : Siginfo with Import, Address => Stk_64 (Info_Idx)'Address;
         Cont : UContext with Import, Address => Stk_64 (Cont_Idx)'Address;
         Info_Val : Siginfo;
         Cont_Val : UContext;
      begin
         --  Load siginfo and context info.
         Info_Val :=
            (Signal_Number => Unsigned_32 (Signal_Number),
             Signal_Code   => 0,
             Signal_Errno  => 0,
             Signal_PID    => Unsigned_32 (Userland.Process.Convert (Proc)),
             Signal_UID    => 0,
             Signal_Addr   => 0,
             Signal_Status => 0,
             Sival_Pointer => 0);
         Cont_Val :=
            (Link    => 0,
             Stack   => 0,
             Context =>
               (Old_Mask => 0,
                Regs     => [others => 0],
                PC       => 0,
                PR       => 0,
                SR       => 0,
                GBR      => 0,
                Mach     => 0,
                Macl     => 0,
                FPRegs   => [others => 0],
                XFPregs  => [others => 0],
                FPSCR    => 0,
                FPul     => 0,
                FP       => 0),
             Sigmask => 0);

         if Use_Altsk then
            --  The alternate stack is the process's own memory, taken as it
            --  is: the frame goes on it as any copy to userland does, and a
            --  stack that such a copy cannot write to takes no signal.
            declare
               package Info_Trans is new Memory.Userland_Transfer (Siginfo);
               package Cont_Trans is new Memory.Userland_Transfer (UContext);
               #if ArchName = """x86_64-limine""" then
                  package Word_Trans is new Memory.Userland_Transfer
                     (Unsigned_64);
               #end if;
            begin
               Info_Trans.Paste_Into_Userland
                  (Map, Info_Val, Stk_64 (Info_Idx)'Address, Success);
               if Success then
                  Cont_Trans.Paste_Into_Userland
                     (Map, Cont_Val, Stk_64 (Cont_Idx)'Address, Success);
               end if;

               --  x86 requires the return address in the stack.
               #if ArchName = """x86_64-limine""" then
                  if Success then
                     Word_Trans.Paste_Into_Userland
                        (Map, Unsigned_64 (To_Integer (Restorer)),
                         Stk_64 (Index_64)'Address, Success);
                  end if;
                  Index_64 := Index_64 - 1;
               #end if;
            end;
            if not Success then
               Thread_Pool (Th).User_Stack_Used := False;
               return;
            end if;
         else
            Info := Info_Val;
            Cont := Cont_Val;

            --  x86 requires the return address in the stack.
            #if ArchName = """x86_64-limine""" then
               Stk_64 (Index_64) := Unsigned_64 (To_Integer (Restorer));
               Index_64 := Index_64 - 1;
            #end if;

            Memory.MMU.Remap_Range
               (Map           => Map,
                Virtual_Start => To_Address (Virtual_Address (Stack_Top)),
                Length        => Storage_Offset (Stack_Size),
                Permissions   => Stack_Permissions,
                Success       => Success);
            if not Success then
               return;
            end if;
         end if;

         Index_64 := Index_64 * 8;

         --  Initialize context information.
         --  TODO: Provide siginfo_t* and ucontext_t* on the 2nd and 3rd arg.
         Arch.Context.Init_GP_Context
            (GP_State,
             To_Address (Integer_Address (Stack_Top + Unsigned_64 (Index_64))),
             Handle,
             Signal_Number,
             Stack_Top + Unsigned_64 (Info_Idx),
             Stack_Top + Unsigned_64 (Cont_Idx));
         Arch.Context.Init_FP_Context (FP_State);

         #if ArchName = """riscv64-limine""" then
            GP_State.X1 := Unsigned_64 (To_Integer (Restorer));
         #end if;
      end;

      Create_User_Thread
         (GP_State => GP_State,
          FP_State => FP_State,
          Map      => Map,
          Pol      => Policy_Other,
          PID      => Userland.Process.Convert (Proc),
          TCB      => Arch.Local.Fetch_TCB,
          New_TID  => New_TID);
      if New_TID = Error_TID then
         goto Give_Back_Stack;
      end if;

      --  A signal thread is listed in no process, so it is let go here.
      Release_Thread (New_TID);

      --  It is taken down with the thread it interrupted, as nothing else
      --  would take it down.
      while Thread_Pool (New_TID).Is_Present loop
         if Is_Doomed then
            Delete_Thread (New_TID);
            Killed := True;
         else
            Yield_If_Able;
         end if;
      end loop;

   <<Give_Back_Stack>>
      --  The stack goes once the handler is done with it. One taken down may
      --  still be leaving on another core, and its process goes too.
      if Use_Altsk then
         Thread_Pool (Th).User_Stack_Used := False;
      elsif not Killed then
         Memory.MMU.Unmap_Range
            (Map           => Map,
             Virtual_Start => To_Address (Virtual_Address (Stack_Top)),
             Length        => Storage_Offset (Stack_Size),
             Success       => Discard);
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end Launch_Signal_Thread;

   procedure Exit_Signal_And_Reschedule is
   begin
      Bail;
   end Exit_Signal_And_Reschedule;
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
      Current_TID : constant TID := Arch.Local.Get_Current_Thread;
      Retiring    : constant TID := Arch.Local.Get_Retiring_Thread;
      Next_TID    :          TID := Error_TID;
      Timeout     : Natural;
      Curr        : Time.Timestamp;
      Count       : Unsigned_32;
      Did_Seize : Boolean;
      Discard   : Boolean;
   begin
      --  A switch leaves through a frame on the old thread's kernel stack, so
      --  the old thread stays running until the entry the switch asks for,
      --  which comes in on the new thread's stack: it lets the old thread go
      --  and arms the new one's time. A new thread that has gone meanwhile
      --  is switched away from at once.
      if Retiring /= Error_TID then
         Thread_Pool (Retiring).Is_Running := False;
         Arch.Local.Set_Retiring_Thread (Error_TID);
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
         Arch.Local.Reschedule_In (Fast_Reschedule_Micros);
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

      --  Find the next thread.
      if Current_TID = Error_TID then
         Next_From_Nothing (Timeout, Next_TID);
      else
         case Thread_Pool (Current_TID).Pol is
            when Policy_FIFO  => Next_FIFO (Current_TID, Timeout, Next_TID);
            when Policy_Other => Next_Other (Current_TID, Timeout, Next_TID);
            when Policy_RR    => Next_RR (Current_TID, Timeout, Next_TID);
         end case;
      end if;

      --  We only get here if the thread search did not find anything, and we
      --  are just going back to whoever called. A thread deleted while in
      --  userland is not gone back to: the core waits on its stack instead,
      --  as after a Bail.
      if Next_TID = Error_TID then
         if Current_TID /= Error_TID and then
            not Thread_Pool (Current_TID).Is_Present and then
            Arch.Context.Is_User_Context (State)
         then
            Discard := Memory.MMU.Make_Active (Memory.MMU.Kernel_Table);
            Arch.Context.Init_Kernel_GP_Context
               (State,
                Thread_Pool (Current_TID).Kernel_Stack.all'Address +
                Kernel_Stack'Length,
                Waiting_Spot'Address);
         end if;
         Synchronization.Release (Scheduler_Mutex);
         Arch.Local.Reschedule_In (Timeout);
         return;
      end if;

      --  Save state.
      if Current_TID /= Error_TID then
         if Current_TID /= Next_TID then
            Arch.Local.Set_Retiring_Thread (Current_TID);
         end if;
         if Thread_Pool (Current_TID).Is_Present then
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

      Timeout := Fast_Reschedule_Micros;
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
   begin
      Arch.Snippets.Enable_Interrupts;
      loop Arch.Snippets.Wait_For_Interrupt; end loop;
   end Waiting_Spot;

   procedure Evaluate_Runnable (T : TID; Can_Run : out Boolean) is
   begin
      Can_Run := Thread_Pool (T).Is_Present and
                 not Thread_Pool (T).Is_Running and
                 not Thread_Pool (T).Is_Held;
      if Can_Run then
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
   exception
      when Constraint_Error =>
         Suspended := False;
   end Evaluate_Suspended;
end Scheduler;
