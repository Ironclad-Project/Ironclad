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

package body Scheduler with SPARK_Mode => Off is
   Fast_Reschedule_Micros : constant := 10_000;
   type Thread_Stack is array (Natural range <>) of Unsigned_8;
   type Thread_Stack_64 is array (Natural range <>) of Unsigned_64;
   type Kernel_Stack is array (1 ..  Memory.Kernel_Stack_Size) of Unsigned_8;
   type Kernel_Stack_Acc is access Kernel_Stack;

   type Thread_Info is record
      Is_Present      : Boolean with Atomic;
      Is_Running      : Boolean with Atomic;
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
      Last_Sched      : Time.Timestamp;
      System_Runtime  : Time.Timestamp;
      User_Runtime    : Time.Timestamp;
      System_Tmp      : Time.Timestamp;
      User_Tmp        : Time.Timestamp;
      User_Stack      : System.Address;
      User_Stack_Size : Unsigned_64;
      User_Stack_Used : Boolean;
      Is_Disabled     : Boolean;
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
             Last_Sched      => (0, 0),
             System_Runtime  => (0, 0),
             User_Runtime    => (0, 0),
             System_Tmp      => (0, 0),
             User_Tmp        => (0, 0),
             User_Stack      => System.Null_Address,
             User_Stack_Size => 0,
             User_Stack_Used => False,
             Is_Disabled     => True)];

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
         Is_Priv : Boolean;
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
         Is_Priv :=
            Userland.Process.Get_Capabilities (Proc).Can_Manage_MAC and then
            EUID = 0;

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
         Stk_64 (Index_64 - 12) := (if Is_Priv then 1 else 0);
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
          Last_Sched      => (0, 0),
          System_Runtime  => (0, 0),
          User_Runtime    => (0, 0),
          System_Tmp      => (0, 0),
          User_Tmp        => (0, 0));

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
      Thread : constant TID := Arch.Local.Get_Current_Thread;
   begin
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
      Tmp     : Time.Timestamp;
      Discard : Boolean;
   begin
      Arch.Clocks.Get_Monotonic_Time (Tmp);
      Thread_Pool (Thread).System_Tmp := Tmp;
      Tmp := Tmp - Thread_Pool (Thread).User_Tmp;
      Thread_Pool (Thread).User_Runtime := Thread_Pool (Thread).User_Runtime +
         Tmp;
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

         Memory.MMU.Remap_Range
            (Map           => Map,
             Virtual_Start => To_Address (Virtual_Address (Stack_Top)),
             Length        => Storage_Offset (Stack_Size),
             Permissions   => Tmp_Stack_Permissions,
             Success       => Success);
         if not Success then
            return;
         end if;
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
         Info_Idx : constant Natural := Stk_64'Last - (Siginfo'Size / 64);
         Cont_Idx : constant Natural := Info_Idx - (UContext'Size / 64);
         Index_64 : Natural := Cont_Idx;
         Info : Siginfo with Import, Address => Stk_64 (Info_Idx)'Address;
         Cont : UContext with Import, Address => Stk_64 (Cont_Idx)'Address;
      begin
         --  Load siginfo and context info.
         Info :=
            (Signal_Number => Unsigned_32 (Signal_Number),
             Signal_Code   => 0,
             Signal_Errno  => 0,
             Signal_PID    => Unsigned_32 (Userland.Process.Convert (Proc)),
             Signal_UID    => 0,
             Signal_Addr   => 0,
             Signal_Status => 0,
             Sival_Pointer => 0);
         Cont :=
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

         --  x86 requires the return address in the stack.
         #if ArchName = """x86_64-limine""" then
            Stk_64 (Index_64) := Unsigned_64 (To_Integer (Restorer));
            Index_64 := Index_64 - 1;
         #end if;

         Index_64 := Index_64 * 8;

         Memory.MMU.Remap_Range
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
      if not Success then
         return;
      end if;

      while Thread_Pool (New_TID).Is_Present loop
         Yield_If_Able;
      end loop;

      if Use_Altsk then
         Thread_Pool (Th).User_Stack_Used := False;
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
      Next_TID    :          TID := Error_TID;
      Timeout     : Natural;
      Curr        : Time.Timestamp;
      Count       : Unsigned_32;
      Did_Seize : Boolean;
   begin
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
      --  are just going back to whoever called.
      if Next_TID = Error_TID then
         Synchronization.Release (Scheduler_Mutex);
         Arch.Local.Reschedule_In (Timeout);
         return;
      end if;

      --  Save state.
      if Current_TID /= Error_TID then
         Thread_Pool (Current_TID).Is_Running := False;
         Curr := Curr - Thread_Pool (Current_TID).Last_Sched;
         if Thread_Pool (Current_TID).Is_Present then
            Thread_Pool (Current_TID).PageMap := MMU.Get_Curr_Table_Addr;
            Thread_Pool (Current_TID).TCB_Pointer := Arch.Local.Fetch_TCB;
            Thread_Pool (Current_TID).GP_State    := State;
            Arch.Context.Save_FP_Context (Thread_Pool (Current_TID).FP_State);
         end if;
      end if;

      --  Set the last time of entry to userland if none.
      Thread_Pool (Next_TID).Last_Sched := Curr;
      if Thread_Pool (Next_TID).User_Tmp = (0, 0) then
         Thread_Pool (Next_TID).User_Tmp := Curr;
      end if;

      --  FIXME: This originally was between the lock release and the
      --  return of the procedure, putting it there though makes the kernel
      --  panic under x86 SMP with memory corruption.
      Arch.Local.Reschedule_In (Timeout);

      --  Reset state.
      Memory.MMU.Set_Table_Addr (Thread_Pool (Next_TID).PageMap);
      Arch.Local.Set_Current_Process (Thread_Pool (Next_TID).Process);
      Arch.Local.Set_Current_Thread (Next_TID);
      Thread_Pool (Next_TID).Is_Running := True;
      Arch.Local.Set_Stacks
         (Thread_Pool (Next_TID).Kernel_Stack (Kernel_Stack'Last)'Address);
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
   procedure Next_From_Nothing (Timeout : out Natural; Next : out TID) is
   begin
      --  Just loop around all threads searching for something to schedule.
      for I in Thread_Pool'Range loop
         if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Running then
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
   begin
      --  We want to check for other FIFO threads with greater priority, if
      --  we find any, we move to the highest priority. Otherwise, we stay.
      Curr_Prio := Thread_Pool (Curr).Prio;
      Next := Error_TID;
      Timeout := Thread_Pool (Curr).RR_Micro_Inter;
      for I in Thread_Pool'Range loop
         if Thread_Pool (I).Is_Present and Thread_Pool (I).Pol = Policy_FIFO
         then
            FIFO_Count := FIFO_Count + 1;
            if not Thread_Pool (I).Is_Running and
               Thread_Pool (I).Prio > Curr_Prio
            then
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
   begin
      --  We want to check for other RR threads with greater priority, if
      --  we find any, we move to the highest priority. Otherwise, we stay.
      --  We also find total RR threads and threads with same priority.
      Curr_Prio := Thread_Pool (Curr).Prio;
      Next := Error_TID;
      Timeout := Thread_Pool (Curr).RR_Micro_Inter;
      for I in Thread_Pool'Range loop
         if Thread_Pool (I).Is_Present and Thread_Pool (I).Pol = Policy_RR then
            RR_Count := RR_Count + 1;
            if not Thread_Pool (I).Is_Running then
               if Thread_Pool (I).Prio > Curr_Prio then
                  Curr_Prio := Thread_Pool (I).Prio;
                  Next := I;
                  Timeout := Thread_Pool (I).RR_Micro_Inter;
               elsif Thread_Pool (I).Prio = Curr_Prio then
                  RR_Equal_Prio_Count := RR_Equal_Prio_Count + 1;
               end if;
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
            if Thread_Pool (I).Is_Present and
               not Thread_Pool (I).Is_Running and
               Thread_Pool (I).Pol = Policy_RR and
               Thread_Pool (I).Prio = Curr_Prio
            then
               Next := I;
               Timeout := Thread_Pool (I).RR_Micro_Inter;
               return;
            end if;
         end loop;
         for I in Thread_Pool'First .. Curr - 1 loop
            if Thread_Pool (I).Is_Present and
               not Thread_Pool (I).Is_Running and
               Thread_Pool (I).Pol = Policy_RR and
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
            if Thread_Pool (I).Is_Present and
               not Thread_Pool (I).Is_Running and
               Thread_Pool (I).Pol = Policy_RR
            then
               Next := I;
               Timeout := Thread_Pool (I).RR_Micro_Inter;
               return;
            end if;
         end loop;
         for I in Thread_Pool'First .. Curr - 1 loop
            if Thread_Pool (I).Is_Present and
               not Thread_Pool (I).Is_Running and
               Thread_Pool (I).Pol = Policy_RR
            then
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
   begin
      --  We want to check if there is a real time policy thread that we can
      --  pick into.
      for I in Thread_Pool'Range loop
         if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Running then
            if Thread_Pool (I).Pol = Policy_FIFO or
               Thread_Pool (I).Pol = Policy_RR
            then
               Next := I;
               Timeout := Thread_Pool (I).RR_Micro_Inter;
               return;
            end if;
         end if;
      end loop;

      --  Just RR into the next Policy_Other thread.
      for I in Curr + 1 .. Thread_Pool'Last loop
         if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Running then
            Next := I;
            Timeout := Thread_Pool (I).RR_Micro_Inter;
            return;
         end if;
      end loop;
      for I in Thread_Pool'First .. Curr - 1 loop
         if Thread_Pool (I).Is_Present and not Thread_Pool (I).Is_Running then
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
end Scheduler;
