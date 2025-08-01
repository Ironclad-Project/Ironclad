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

with System;
with Synchronization;
with Panic;
with System.Storage_Elements; use System.Storage_Elements;
with Userland.Process;
with Arch;
with Arch.Local;
with Arch.Clocks;
with Arch.Snippets;
with Time;

package body Scheduler with SPARK_Mode => Off is
   Kernel_Stack_Size : constant := 16#4000#;
   type Thread_Stack     is array (Natural range <>) of Unsigned_8;
   type Thread_Stack_64  is array (Natural range <>) of Unsigned_64;
   type Kernel_Stack     is array (1 ..  Kernel_Stack_Size) of Unsigned_8;
   type Kernel_Stack_Acc is access Kernel_Stack;

   type Thread_Info is record
      Is_Present      : Boolean with Atomic;
      Is_Running      : Boolean with Atomic;
      Path            : String (1 .. 20);
      Path_Len        : Natural range 0 .. 20;
      Pol             : Policy;
      Nice            : Niceness;
      Prio            : Priority;
      TCB_Pointer     : System.Address;
      PageMap         : System.Address;
      Kernel_Stack    : Kernel_Stack_Acc;
      GP_State        : Arch.Context.GP_Context;
      FP_State        : Arch.Context.FP_Context;
      C_State         : Arch.Context.Core_Context;
      Process         : Userland.Process.PID;
      Last_Sched_Sec  : Unsigned_64;
      Last_Sched_NSec : Unsigned_64;
      System_Sec      : Unsigned_64;
      System_NSec     : Unsigned_64;
      User_Sec        : Unsigned_64;
      User_NSec       : Unsigned_64;
      System_Tmp_Sec  : Unsigned_64;
      System_Tmp_NSec : Unsigned_64;
      User_Tmp_Sec    : Unsigned_64;
      User_Tmp_NSec   : Unsigned_64;
   end record;
   type Thread_Info_Arr     is array (TID range 1 .. TID'Last) of Thread_Info;
   type Thread_Info_Arr_Acc is access Thread_Info_Arr;

   Thread_Pool     : Thread_Info_Arr_Acc;
   Scheduler_Mutex : aliased Synchronization.Binary_Semaphore :=
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
             Path            => [others => ' '],
             Path_Len        => 0,
             Pol             => Policy_Other,
             Prio            => Default_Priority,
             Nice            => Default_Niceness,
             TCB_Pointer     => System.Null_Address,
             PageMap         => System.Null_Address,
             Kernel_Stack    => null,
             GP_State        => <>,
             FP_State        => <>,
             C_State         => <>,
             Process         => Userland.Process.Error_PID,
             Last_Sched_Sec  => 0,
             Last_Sched_NSec => 0,
             System_Sec      => 0,
             System_NSec     => 0,
             User_Sec        => 0,
             User_NSec       => 0,
             System_Tmp_Sec  => 0,
             System_Tmp_NSec => 0,
             User_Tmp_Sec    => 0,
             User_Tmp_NSec   => 0)];

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
       Map        : Arch.MMU.Page_Table_Acc;
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
      Curr_Map := Arch.MMU.Get_Curr_Table_Addr;
      Success  := Arch.MMU.Make_Active (Map);
      if not Success then
         return;
      end if;

      --  Initialize thread state. Start by mapping the user stack.
      Userland.Process.Bump_Stack_Base (Proc, Stack_Size, Stack_Top);
      Arch.MMU.Map_Allocated_Range
         (Map           => Map,
          Virtual_Start => To_Address (Virtual_Address (Stack_Top)),
          Length        => Storage_Offset (Stack_Size),
          Permissions   => Tmp_Stack_Permissions,
          Success       => Success);
      if not Success then
         goto Cleanup;
      end if;

      declare
         EUID   : Unsigned_32;
         Sz     : constant Natural := Natural (Stack_Size);
         Stk_8  : Thread_Stack (1 .. Sz)
            with Import, Address => To_Address (Virtual_Address (Stack_Top));
         Stk_64 : Thread_Stack_64 (1 .. Sz / 8)
            with Import, Address => To_Address (Virtual_Address (Stack_Top));
         Index_8  : Natural := Stk_8'Last;
         Index_64 : Natural := Stk_64'Last;
      begin
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

         Userland.Process.Get_Effective_UID (Proc, EUID);

         if Userland.Process.Get_Capabilities (Proc).Can_Manage_MAC and then
            EUID = 0
         then
            Stk_64 (Index_64 - 10) := 1;
         else
            Stk_64 (Index_64 - 10) := 0;
         end if;
         Stk_64 (Index_64 - 11) := Userland.ELF.Auxval_Secure_Treatment;
         Index_64 := Index_64 - 12;

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
         Arch.MMU.Remap_Range
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
      Arch.MMU.Set_Table_Addr (Curr_Map);
   exception
      when Constraint_Error =>
         New_TID := Error_TID;
   end Create_User_Thread;

   procedure Create_User_Thread
      (Address    : Virtual_Address;
       Map        : Arch.MMU.Page_Table_Acc;
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
       Map      : Arch.MMU.Page_Table_Acc;
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
      end if;

      Thread_Pool (New_TID) :=
         (Is_Present   => True,
          Is_Running   => False,
          Path         => [others => ' '],
          Path_Len     => 0,
          Pol          => Pol,
          Prio         => Default_Priority,
          Nice         => Default_Niceness,
          TCB_Pointer  => TCB,
          PageMap      => Arch.MMU.Get_Map_Table_Addr (Map),
          Kernel_Stack => New_Stack,
          GP_State     => GP_State,
          FP_State     => FP_State,
          C_State      => <>,
          Process      => Userland.Process.Convert (PID),
          others       => 0);

      Arch.Context.Success_Fork_Result (Thread_Pool (New_TID).GP_State);

   <<End_Return>>
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
         New_TID := Error_TID;
   end Create_User_Thread;

   procedure Delete_Thread (Thread : TID) is
   begin
      Synchronization.Seize (Scheduler_Mutex);
      if Thread_Pool (Thread).Is_Present then
         Thread_Pool (Thread).Is_Present := False;
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
      Thread : constant TID := Arch.Local.Get_Current_Thread;
   begin
      Synchronization.Seize (Scheduler_Mutex);
      if Thread_Pool (Thread).Is_Present then
         Thread_Pool (Thread).Is_Present := False;
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

   procedure Get_Runtime_Times
      (Thread : TID;
       System_Seconds, System_Nanoseconds : out Unsigned_64;
       User_Seconds, User_Nanoseconds     : out Unsigned_64)
   is
   begin
      Synchronization.Seize (Scheduler_Mutex);
      System_Seconds := Thread_Pool (Thread).System_Sec;
      System_Nanoseconds := Thread_Pool (Thread).System_NSec;
      User_Seconds := Thread_Pool (Thread).User_Sec;
      User_Nanoseconds := Thread_Pool (Thread).User_NSec;
      Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Scheduler_Mutex);
         System_Seconds     := 0;
         User_Seconds       := 0;
         System_Nanoseconds := 0;
         User_Nanoseconds   := 0;
   end Get_Runtime_Times;

   procedure Signal_Kernel_Entry (Thread : TID) is
      T1, T2  : Unsigned_64;
      Discard : Boolean;
   begin
      Arch.Clocks.Get_Monotonic_Time (T1, T2);

      Thread_Pool (Thread).System_Tmp_Sec := T1;
      Thread_Pool (Thread).System_Tmp_NSec := T2;

      Time.Subtract
         (T1, T2,
          Thread_Pool (Thread).User_Tmp_Sec,
          Thread_Pool (Thread).User_Tmp_NSec);
      Time.Increment
         (Thread_Pool (Thread).User_Sec, Thread_Pool (Thread).User_NSec,
          T1, T2);
   exception
      when Constraint_Error =>
         null;
   end Signal_Kernel_Entry;

   procedure Signal_Kernel_Exit (Thread : TID) is
      Temp_Sec, Temp_NSec : Unsigned_64;
   begin
      Arch.Clocks.Get_Monotonic_Time (Temp_Sec, Temp_NSec);

      Thread_Pool (Thread).User_Tmp_Sec := Temp_Sec;
      Thread_Pool (Thread).User_Tmp_NSec := Temp_NSec;

      if Thread_Pool (Thread).System_Tmp_Sec /= 0 or
         Thread_Pool (Thread).System_Tmp_NSec /= 0
      then
         Time.Subtract
            (Seconds1     => Temp_Sec,
             Nanoseconds1 => Temp_NSec,
             Seconds2     => Thread_Pool (Thread).System_Tmp_Sec,
             Nanoseconds2 => Thread_Pool (Thread).System_Tmp_NSec);
         Time.Increment
            (Thread_Pool (Thread).System_Sec, Thread_Pool (Thread).System_NSec,
             Temp_Sec, Temp_NSec);
      end if;
   exception
      when Constraint_Error =>
         null;
   end Signal_Kernel_Exit;

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
   procedure Launch_Signal_Thread
      (Signal_Number    : Unsigned_64;
       Handle, Restorer : System.Address;
       Success          : out Boolean)
   is
      Stack_Size : constant := Kernel_Stack_Size;

      GP_State  : Arch.Context.GP_Context;
      FP_State  : Arch.Context.FP_Context;
      New_TID   : TID;
      Stack_Top : Unsigned_64;
      Map       : Arch.MMU.Page_Table_Acc;
      Curr_Proc : constant Userland.Process.PID :=
         Arch.Local.Get_Current_Process;
   begin
      --  Initialize signal stack. Start by mapping the user stack.
      Userland.Process.Bump_Stack_Base (Curr_Proc, Stack_Size, Stack_Top);
      Userland.Process.Get_Common_Map (Curr_Proc, Map);
      Arch.MMU.Map_Allocated_Range
         (Map           => Map,
          Virtual_Start => To_Address (Virtual_Address (Stack_Top)),
          Length        => Storage_Offset (Stack_Size),
          Permissions   => Tmp_Stack_Permissions,
          Success       => Success);
      if not Success then
         return;
      end if;

      declare
         Sz     : constant Natural := Natural (Stack_Size);
         Stk_64 : Thread_Stack_64 (1 .. Sz / 8)
            with Import, Address => To_Address (Virtual_Address (Stack_Top));
         Index_64 : Natural := Stk_64'Last;
      begin
         Stk_64 (Index_64) := Unsigned_64 (To_Integer (Restorer));
         Index_64 := Index_64 - 1;
         Index_64 := Index_64 * 8;

         Arch.MMU.Remap_Range
            (Map           => Map,
             Virtual_Start => To_Address (Virtual_Address (Stack_Top)),
             Length        => Storage_Offset (Stack_Size),
             Permissions   => Stack_Permissions,
             Success       => Success);
         if not Success then
            return;
         end if;

         --  Initialize context information.
         --  TODO: Provide siginfo_t* and ucontext_t* on the 2nd and 3rd arg.
         Arch.Context.Init_GP_Context
            (GP_State,
             To_Address (Integer_Address (Stack_Top + Unsigned_64 (Index_64))),
             Handle,
             Signal_Number,
             0,
             0);
         Arch.Context.Init_FP_Context (FP_State);
      end;

      Create_User_Thread
         (GP_State => GP_State,
          FP_State => FP_State,
          Map      => Map,
          Pol      => Policy_Other,
          PID      => Userland.Process.Convert (Curr_Proc),
          TCB      => Arch.Local.Fetch_TCB,
          New_TID  => New_TID);
      if not Success then
         return;
      end if;

      while Thread_Pool (New_TID).Is_Present loop
         Yield_If_Able;
      end loop;
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
   procedure Scheduler_ISR (State : Arch.Context.GP_Context) is
      Current_TID : constant TID := Arch.Local.Get_Current_Thread;
      Next_TID    :          TID := Error_TID;
      Next_State  : Arch.Context.GP_Context;
      Timeout     : Natural;
      Curr_Sec    : Unsigned_64;
      Curr_NSec   : Unsigned_64;
      Count       : Unsigned_32;
   begin
      Arch.Clocks.Get_Monotonic_Time (Curr_Sec, Curr_NSec);

      Synchronization.Seize (Scheduler_Mutex);

      --  Adjust the moving stats if at least a minute has passed since
      --  last poll.
      if Curr_Sec >= Last_Bucket + 60 then
         Last_Bucket := Curr_Sec;
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

      Timeout := 100_000;

      --  Find the next thread in a very rough RR.
      for I in Current_TID + 1 .. Thread_Pool'Last loop
         if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Running then
            Next_TID := I;
            goto Found_TID;
         end if;
      end loop;
      if Current_TID /= Error_TID then
         for I in Thread_Pool'First .. Current_TID - 1 loop
            if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Running
            then
               Next_TID := I;
               goto Found_TID;
            end if;
         end loop;
      end if;

      --  We only get here if the thread search did not find anything, and we
      --  are just going back to whoever called.
      Synchronization.Release (Scheduler_Mutex);
      Arch.Local.Reschedule_In (Timeout);
      return;

   <<Found_TID>>
      --  Save state.
      if Current_TID /= Error_TID then
         Thread_Pool (Current_TID).Is_Running := False;

         Time.Subtract
            (Curr_Sec, Curr_NSec,
             Thread_Pool (Current_TID).Last_Sched_Sec,
             Thread_Pool (Current_TID).Last_Sched_NSec);

         if Thread_Pool (Current_TID).Is_Present then
            Thread_Pool (Current_TID).PageMap := Arch.MMU.Get_Curr_Table_Addr;
            Thread_Pool (Current_TID).TCB_Pointer := Arch.Local.Fetch_TCB;
            Thread_Pool (Current_TID).GP_State    := State;
            Arch.Context.Save_Core_Context (Thread_Pool (Current_TID).C_State);
            Arch.Context.Save_FP_Context (Thread_Pool (Current_TID).FP_State);
         else
            Arch.Context.Destroy_FP_Context
               (Thread_Pool (Current_TID).FP_State);
         end if;
      end if;

      --  Set the last time of entry to userland if none.
      Thread_Pool (Next_TID).Last_Sched_Sec  := Curr_Sec;
      Thread_Pool (Next_TID).Last_Sched_NSec := Curr_NSec;
      if Thread_Pool (Next_TID).User_Tmp_Sec  = 0 and
         Thread_Pool (Next_TID).User_Tmp_NSec = 0
      then
         Thread_Pool (Next_TID).User_Tmp_Sec  := Curr_Sec;
         Thread_Pool (Next_TID).User_Tmp_NSec := Curr_NSec;
      end if;

      --  Rearm the timer for next tick and unlock.
      Arch.Local.Reschedule_In (Timeout);

      --  Reset state.
      Arch.MMU.Set_Table_Addr (Thread_Pool (Next_TID).PageMap);
      Arch.Local.Set_Current_Process (Thread_Pool (Next_TID).Process);
      Arch.Local.Set_Current_Thread (Next_TID);
      Thread_Pool (Next_TID).Is_Running := True;
      Arch.Local.Set_Stacks
         (Thread_Pool (Next_TID).C_State,
          Thread_Pool (Next_TID).Kernel_Stack (Kernel_Stack'Last)'Address);
      Arch.Local.Load_TCB (Thread_Pool (Next_TID).TCB_Pointer);
      Arch.Context.Load_FP_Context (Thread_Pool (Next_TID).FP_State);
      Next_State := Thread_Pool (Next_TID).GP_State;
      Synchronization.Release (Scheduler_Mutex);
      Arch.Context.Load_GP_Context (Next_State);
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

   procedure List_All (List : out Thread_Listing_Arr; Total : out Natural) is
      Curr_Index : Natural := 0;
   begin
      List  := [others => (Error_TID, 0)];
      Total := 0;

      Synchronization.Seize (Scheduler_Mutex);
      for I in Thread_Pool.all'Range loop
         if Thread_Pool (I).Is_Present then
            Total := Total + 1;
            if Curr_Index < Thread_Pool'Length then
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
   procedure Waiting_Spot is
   begin
      Arch.Snippets.Enable_Interrupts;
      loop Arch.Snippets.Wait_For_Interrupt; end loop;
   end Waiting_Spot;
end Scheduler;
