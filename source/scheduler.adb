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
with Lib.Synchronization;
with Lib.Panic;
with Lib.Messages;
with System.Storage_Elements; use System.Storage_Elements;
with Userland.Process;
with Arch;
with Arch.Local;
with Arch.Clocks;
with Arch.Snippets;
with Arch.Power;
with Lib;
with Lib.Time;

package body Scheduler with SPARK_Mode => Off is
   Kernel_Stack_Size : constant := 16#4000#;
   type Thread_Stack     is array (Natural range <>) of Unsigned_8;
   type Thread_Stack_64  is array (Natural range <>) of Unsigned_64;
   type Kernel_Stack     is array (1 ..  Kernel_Stack_Size) of Unsigned_8;
   type Kernel_Stack_Acc is access Kernel_Stack;

   type Thread_Cluster is record
      Is_Present       : Boolean;
      Algorithm        : Cluster_Algorithm;
      RR_Quantum       : Natural;
      Is_Interruptible : Boolean;
      Percentage       : Natural range 0 .. 100;
      Progress_Seconds : Unsigned_64;
      Progress_Nanos   : Unsigned_64;
   end record;
   type Cluster_Arr     is array (TCID range 1 .. TCID'Last) of Thread_Cluster;
   type Cluster_Arr_Acc is access Cluster_Arr;

   type Thread_Info is record
      Is_Present      : Boolean with Atomic;
      Is_Running      : Boolean with Atomic;
      Path            : String (1 .. 20);
      Path_Len        : Natural range 0 .. 20;
      Nice            : Niceness;
      Cluster         : TCID;
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

   --  Scheduling timeslices are calculated using a major frame
   --  (or cluster frame) and a minor frame (or cluster quantum).
   --  Both are calculated from the total slice.
   Total_Slice : constant := 1_000_000; --  A second in microseconds.

   Scheduler_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
   Cluster_Pool    : Cluster_Arr_Acc;
   Thread_Pool     : Thread_Info_Arr_Acc;

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
      RR_Quantum : Natural;
      Profile    : Arch.Power.Power_Profile;
   begin
      --  Initialize registries.
      Thread_Pool := new Thread_Info_Arr'
         [others =>
            (Is_Present      => False,
             Is_Running      => False,
             Path            => [others => ' '],
             Path_Len        => 0,
             Nice            => 0,
             Cluster         => Error_TCID,
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

      Cluster_Pool := new Cluster_Arr'
         [others =>
            (Is_Present       => False,
             Algorithm        => Cluster_RR,
             RR_Quantum       => 0,
             Is_Interruptible => False,
             Percentage       => 0,
             Progress_Seconds => 0,
             Progress_Nanos   => 0)];

      --  Create the default cluster.
      Cluster_Pool (Cluster_Pool'First).Is_Present       := True;
      Cluster_Pool (Cluster_Pool'First).Algorithm        := Cluster_RR;
      Cluster_Pool (Cluster_Pool'First).Is_Interruptible := True;
      Cluster_Pool (Cluster_Pool'First).Percentage       := 100;
      Cluster_Pool (Cluster_Pool'First).Progress_Seconds := 0;
      Cluster_Pool (Cluster_Pool'First).Progress_Nanos   := 0;

      --  Set a higher quantum, thus less power consumption, for small devices.
      Arch.Power.Get_Preferred_Profile (Profile);
      case Profile is
         when Arch.Power.Mobile | Arch.Power.Appliance =>
            Lib.Messages.Put_Line
               ("Using higher latency RR_Quantum for low profile devices");
            RR_Quantum := 60000;
         when others =>
            RR_Quantum := 20000;
      end case;
      Cluster_Pool (Cluster_Pool'First).RR_Quantum := RR_Quantum;

      Is_Initialized := True;
      Lib.Synchronization.Release (Scheduler_Mutex);
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
       Map        : Arch.MMU.Page_Table_Acc;
       Vector     : Userland.ELF.Auxval;
       Cluster    : TCID;
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
      Userland.Process.Get_Stack_Base (Proc, Stack_Top);
      Userland.Process.Set_Stack_Base (Proc, Stack_Top + Stack_Size);
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
         if Userland.Process.Get_Capabilities (Proc).Can_Change_Scheduling then
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
             Map       => Map,
             PID       => PID,
             Cluster   => Cluster,
             TCB       => System.Null_Address,
             New_TID   => New_TID);
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
       Cluster    : TCID;
       PID        : Natural;
       New_TID    : out TID)
   is
      GP_State : Arch.Context.GP_Context;
      FP_State : Arch.Context.FP_Context;
   begin
      Arch.Context.Init_GP_Context
         (GP_State,
          To_Address (Integer_Address (Stack_Addr)),
          To_Address (Address));
      Arch.Context.Init_FP_Context (FP_State);
      Create_User_Thread
         (GP_State => GP_State,
          FP_State => FP_State,
          Map      => Map,
          PID      => PID,
          Cluster  => Cluster,
          TCB      => To_Address (Integer_Address (TLS_Addr)),
          New_TID  => New_TID);
   end Create_User_Thread;

   procedure Create_User_Thread
      (GP_State : Arch.Context.GP_Context;
       FP_State : Arch.Context.FP_Context;
       Map      : Arch.MMU.Page_Table_Acc;
       Cluster  : TCID;
       PID      : Natural;
       TCB      : System.Address;
       New_TID  : out TID)
   is
      New_Stack : Kernel_Stack_Acc;
   begin
      New_TID := Error_TID;
      Lib.Synchronization.Seize (Scheduler_Mutex);

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
          Nice         => 0,
          Cluster      => Cluster,
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
      Lib.Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
         New_TID := Error_TID;
   end Create_User_Thread;

   procedure Delete_Thread (Thread : TID) is
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);
      if Thread_Pool (Thread).Is_Present then
         Thread_Pool (Thread).Is_Present := False;
      end if;
      Lib.Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
   end Delete_Thread;

   procedure Yield_If_Able is
      Curr_TID : constant     TID := Arch.Local.Get_Current_Thread;
      Is_Init  : constant Boolean := Is_Initialized;
   begin
      if Is_Init               and then
         Curr_TID /= Error_TID and then
         Cluster_Pool (Thread_Pool (Curr_TID).Cluster).Is_Interruptible
      then
         Arch.Local.Reschedule_ASAP;
      end if;
   exception
      when Constraint_Error =>
         null;
   end Yield_If_Able;

   procedure Bail is
      Thread : constant TID := Arch.Local.Get_Current_Thread;
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);
      if Thread_Pool (Thread).Is_Present then
         Thread_Pool (Thread).Is_Present := False;
      end if;
      Lib.Synchronization.Release (Scheduler_Mutex);
      Arch.Local.Reschedule_ASAP;
      Waiting_Spot;
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
         Arch.Local.Reschedule_ASAP;
         Waiting_Spot;
   end Bail;

   procedure Get_Runtime_Times
      (Thread : TID;
       System_Seconds, System_Nanoseconds : out Unsigned_64;
       User_Seconds, User_Nanoseconds     : out Unsigned_64)
   is
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);
      System_Seconds := Thread_Pool (Thread).System_Sec;
      System_Nanoseconds := Thread_Pool (Thread).System_NSec;
      User_Seconds := Thread_Pool (Thread).User_Sec;
      User_Nanoseconds := Thread_Pool (Thread).User_NSec;
      Lib.Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
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

      Lib.Time.Subtract
         (T1, T2,
          Thread_Pool (Thread).User_Tmp_Sec,
          Thread_Pool (Thread).User_Tmp_NSec);
      Lib.Time.Increment
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
         Lib.Time.Subtract
            (Seconds1     => Temp_Sec,
             Nanoseconds1 => Temp_NSec,
             Seconds2     => Thread_Pool (Thread).System_Tmp_Sec,
             Nanoseconds2 => Thread_Pool (Thread).System_Tmp_NSec);
         Lib.Time.Increment
            (Thread_Pool (Thread).System_Sec, Thread_Pool (Thread).System_NSec,
             Temp_Sec, Temp_NSec);
      end if;
   exception
      when Constraint_Error =>
         null;
   end Signal_Kernel_Exit;
   ----------------------------------------------------------------------------
   procedure Set_Scheduling_Algorithm
      (Cluster          : TCID;
       Algo             : Cluster_Algorithm;
       Quantum          : Natural;
       Is_Interruptible : Boolean;
       Success          : out Boolean)
   is
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);
      Cluster_Pool (Cluster).Algorithm        := Algo;
      Cluster_Pool (Cluster).RR_Quantum       := Quantum;
      Cluster_Pool (Cluster).Is_Interruptible := Is_Interruptible;
      Lib.Synchronization.Release (Scheduler_Mutex);
      Success := True;
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
         Success := False;
   end Set_Scheduling_Algorithm;

   procedure Set_Time_Slice
      (Cluster : TCID;
       Per     : Natural;
       Success : out Boolean)
   is
      Consumed : Natural := 0;
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);
      for I in Cluster_Pool'Range loop
         if Cluster_Pool (I).Is_Present and I /= Cluster then
            Consumed := Consumed + Cluster_Pool (I).Percentage;
         end if;
      end loop;

      if Cluster_Pool (Cluster).Is_Present and Per <= (100 - Consumed) then
         Cluster_Pool (Cluster).Percentage := Per;
         Success := True;
      else
         Success := False;
      end if;
      Lib.Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
         Success := False;
   end Set_Time_Slice;

   procedure Create_Cluster (New_TCID : out TCID) is
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);
      for I in Cluster_Pool'Range loop
         if not Cluster_Pool (I).Is_Present then
            Cluster_Pool (I) :=
               (Is_Present       => True,
                Algorithm        => Cluster_RR,
                Is_Interruptible => False,
                RR_Quantum       => 4000,
                Percentage       => 0,
                others           => 0);
            New_TCID := I;
            goto Cleanup;
         end if;
      end loop;
      New_TCID := Error_TCID;
   <<Cleanup>>
      Lib.Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
         New_TCID := Error_TCID;
   end Create_Cluster;

   procedure Delete_Cluster (Cluster : TCID; Success : out Boolean) is
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);
      if Cluster_Pool (Cluster).Is_Present then
         Cluster_Pool (Cluster).Is_Present := False;
         Success := True;
      else
         Success := False;
      end if;
      Lib.Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
         Success := False;
   end Delete_Cluster;

   procedure Switch_Cluster
      (Cluster : TCID;
       Thread  : TID;
       Success : out Boolean)
   is
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);
      if Cluster_Pool (Cluster).Is_Present and
         Thread_Pool (Thread).Is_Present
      then
         Thread_Pool (Thread).Cluster := Cluster;
         Success := True;
      else
         Success := False;
      end if;
      Lib.Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
         Success := False;
   end Switch_Cluster;
   ----------------------------------------------------------------------------
   function Get_Niceness (Thread : TID) return Niceness is
   begin
      return Thread_Pool (Thread).Nice;
   exception
      when Constraint_Error =>
         return 0;
   end Get_Niceness;

   procedure Set_Niceness (Thread : TID; Nice : Niceness) is
   begin
      Thread_Pool (Thread).Nice := Nice;
   exception
      when Constraint_Error =>
         null;
   end Set_Niceness;

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
      Curr_TID  : constant TID :=
         Arch.Local.Get_Current_Thread;
      Curr_Proc : constant Userland.Process.PID :=
         Arch.Local.Get_Current_Process;
   begin
      --  Initialize signal stack. Start by mapping the user stack.
      Userland.Process.Get_Stack_Base (Curr_Proc, Stack_Top);
      Userland.Process.Set_Stack_Base (Curr_Proc, Stack_Top + Stack_Size);
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
         (GP_State  => GP_State,
          FP_State  => FP_State,
          Map       => Map,
          PID       => Userland.Process.Convert (Curr_Proc),
          Cluster   => Thread_Pool (Curr_TID).Cluster,
          TCB       => Arch.Local.Fetch_TCB,
          New_TID   => New_TID);
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
   begin
      Lib.Synchronization.Seize (Scheduler_Mutex);
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

      Lib.Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
         Avg_1  := 0;
         Avg_5  := 0;
         Avg_15 := 0;
   end Get_Load_Averages;
   ----------------------------------------------------------------------------
   procedure Scheduler_ISR (State : Arch.Context.GP_Context) is
      Current_TID : constant TID := Arch.Local.Get_Current_Thread;
      Next_TID    :          TID := Error_TID;
      Next_State  : Arch.Context.GP_Context;
      Timeout, Max_Next_Timeout : Natural;
      Curr_Cluster, Next_Cluster : TCID := Error_TCID;
      Curr_Sec, Curr_NSec : Unsigned_64;
      Count : Unsigned_32;
   begin
      Arch.Clocks.Get_Monotonic_Time (Curr_Sec, Curr_NSec);

      Lib.Synchronization.Seize (Scheduler_Mutex);

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
         Add_Bucket_And_Shift (Count);
      end if;

      --  If we come from a thread, we can do cluster management and logic.
      --  Else, we just need to go to a thread, any, and pick up from there.
      if Current_TID /= Error_TID then
         Curr_Cluster := Thread_Pool (Current_TID).Cluster;
         Lib.Time.Subtract
            (Curr_Sec, Curr_NSec,
             Thread_Pool (Current_TID).Last_Sched_Sec,
             Thread_Pool (Current_TID).Last_Sched_NSec);
         Lib.Time.Increment
            (Cluster_Pool (Curr_Cluster).Progress_Seconds,
             Cluster_Pool (Curr_Cluster).Progress_Nanos, Curr_Sec, Curr_NSec);

         if Has_Available_Time (Curr_Cluster) then
            Next_Cluster := Curr_Cluster;
         else
            --  Find a new cluster with available time.
            for I in Curr_Cluster + 1 .. Cluster_Pool'Last loop
               if Has_Available_Time (I) then
                  Next_Cluster := I;
                  exit;
               end if;
            end loop;
            if Next_Cluster = Error_TCID then
               for I in Cluster_Pool'First .. Curr_Cluster - 1 loop
                  if Has_Available_Time (I) then
                     Next_Cluster := I;
                     exit;
                  end if;
               end loop;
            end if;

            --  If we find no new clusters, we just reset em.
            if Next_Cluster = Error_TCID then
               for C of Cluster_Pool.all loop
                  C.Progress_Seconds := 0;
                  C.Progress_Nanos := 0;
               end loop;
               Next_Cluster := 1;
            end if;
         end if;

         Max_Next_Timeout :=
            (Total_Slice * Cluster_Pool (Next_Cluster).Percentage / 100) -
            Natural (Cluster_Pool (Next_Cluster).Progress_Nanos / 1000);

         case Cluster_Pool (Next_Cluster).Algorithm is
            when Cluster_RR =>
               Timeout :=
                  Cluster_Pool (Next_Cluster).RR_Quantum -
                  ((Cluster_Pool (Next_Cluster).RR_Quantum / 40) *
                   Thread_Pool (Current_TID).Nice);

               if Timeout > Max_Next_Timeout then
                  Timeout := Max_Next_Timeout;
               end if;
            when Cluster_Cooperative =>
               Timeout := Max_Next_Timeout;
         end case;

         --  Find the next thread from said cluster using the algorithm.
         --  Cluster_Cooperative finds new threads the same way as RR, so we
         --  do not need to handle it especially.
         for I in Current_TID + 1 .. Thread_Pool'Last loop
            if Is_Switchable (I, Next_Cluster) then
               Next_TID := I;
               goto Found_TID_TCID_Combo;
            end if;
         end loop;
         if Current_TID /= Error_TID then
            for I in Thread_Pool'First .. Current_TID - 1 loop
               if Is_Switchable (I, Next_Cluster) then
                  Next_TID := I;
                  goto Found_TID_TCID_Combo;
               end if;
            end loop;
         end if;
      else
         Timeout := 4000;
         for I in Thread_Pool'Range loop
            if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Running
            then
               Next_TID := I;
               Next_Cluster := Thread_Pool (I).Cluster;
               goto Found_TID_TCID_Combo;
            end if;
         end loop;
      end if;

      --  We only get here if the thread search did not find anything, and we
      --  are just going back to whoever called.
      Lib.Synchronization.Release (Scheduler_Mutex);
      Arch.Local.Reschedule_In (Timeout);
      return;

   <<Found_TID_TCID_Combo>>
      --  Save state.
      if Current_TID /= Error_TID then
         Thread_Pool (Current_TID).Is_Running := False;

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
      Arch.Clocks.Get_Monotonic_Time
         (Thread_Pool (Next_TID).Last_Sched_Sec,
          Thread_Pool (Next_TID).Last_Sched_NSec);
      if Thread_Pool (Next_TID).User_Tmp_Sec = 0 and
         Thread_Pool (Next_TID).User_Tmp_NSec = 0
      then
         Thread_Pool (Next_TID).User_Tmp_Sec  :=
            Thread_Pool (Next_TID).Last_Sched_Sec;
         Thread_Pool (Next_TID).User_Tmp_NSec :=
            Thread_Pool (Next_TID).Last_Sched_NSec;
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
      Lib.Synchronization.Release (Scheduler_Mutex);
      Arch.Context.Load_GP_Context (Next_State);
   exception
      when Constraint_Error =>
         Lib.Panic.Hard_Panic ("Exception while reescheduling");
   end Scheduler_ISR;
   ----------------------------------------------------------------------------

   function Convert (Thread : TID) return Natural is
   begin
      return Natural (Thread);
   end Convert;

   function Convert (Group : TCID) return Natural is
   begin
      return Natural (Group);
   end Convert;

   function Convert (Value : Natural) return TID is
   begin
      return TID (Value);
   exception
      when Constraint_Error =>
         return Error_TID;
   end Convert;

   function Convert (Value : Natural) return TCID is
   begin
      return TCID (Value);
   exception
      when Constraint_Error =>
         return Error_TCID;
   end Convert;

   procedure List_All (List : out Thread_Listing_Arr; Total : out Natural) is
      Curr_Index : Natural := 0;
   begin
      List  := [others => (Error_TID, Error_TCID, 0)];
      Total := 0;

      Lib.Synchronization.Seize (Scheduler_Mutex);
      for I in Thread_Pool.all'Range loop
         if Thread_Pool (I).Is_Present then
            Total := Total + 1;
            if Curr_Index < Thread_Pool'Length then
               List (List'First + Curr_Index) :=
                  (I, Thread_Pool (I).Cluster,
                   Userland.Process.Convert (Thread_Pool (I).Process));
               Curr_Index := Curr_Index + 1;
            end if;
         end if;
      end loop;
      Lib.Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
         Total := 0;
   end List_All;

   procedure List_All (List : out Cluster_Listing_Arr; Total : out Natural) is
      Curr_Index : Natural := 0;
   begin
      List  := [others => (Error_TCID, Cluster_RR, False, 0)];
      Total := 0;

      Lib.Synchronization.Seize (Scheduler_Mutex);
      for I in Cluster_Pool.all'Range loop
         if Cluster_Pool (I).Is_Present then
            Total := Total + 1;
            if Curr_Index < Thread_Pool'Length then
               List (List'First + Curr_Index) :=
                  (I,
                   Cluster_Pool (I).Algorithm,
                   Cluster_Pool (I).Is_Interruptible,
                   Cluster_Pool (I).RR_Quantum);
               Curr_Index := Curr_Index + 1;
            end if;
         end if;
      end loop;
      Lib.Synchronization.Release (Scheduler_Mutex);
   exception
      when Constraint_Error =>
         Lib.Synchronization.Release (Scheduler_Mutex);
         Total := 0;
   end List_All;
   ----------------------------------------------------------------------------
   procedure Waiting_Spot is
   begin
      Arch.Snippets.Enable_Interrupts;
      loop Arch.Snippets.Wait_For_Interrupt; end loop;
   end Waiting_Spot;

   function Has_Available_Time (C : TCID) return Boolean is
      Secs_To_Micros, Nanos_To_Micros : Unsigned_64;
   begin
      if C /= Error_TCID then
         Secs_To_Micros  := Cluster_Pool (C).Progress_Seconds * 1_000_000;
         Nanos_To_Micros := Cluster_Pool (C).Progress_Nanos   / 1_000;

         return Secs_To_Micros + Nanos_To_Micros <
                Total_Slice * Unsigned_64 (Cluster_Pool (C).Percentage) / 100;
      else
         return False;
      end if;
   exception
      when Constraint_Error =>
         return False;
   end Has_Available_Time;

   function Is_Switchable (T : TID; C : TCID) return Boolean is
   begin
      return Thread_Pool (T).Is_Present     and
             not Thread_Pool (T).Is_Running and
             Thread_Pool (T).Cluster = C;
   exception
      when Constraint_Error =>
         return False;
   end Is_Switchable;

   procedure Add_Bucket_And_Shift (Last_Bucket : Unsigned_32) is
   begin
      for I in reverse Buckets'First .. Buckets'Last - 1 loop
         Buckets (I + 1) := Buckets (I);
      end loop;
      Buckets (Buckets'First) := Last_Bucket;
   exception
      when Constraint_Error =>
         null;
   end Add_Bucket_And_Shift;
end Scheduler;
