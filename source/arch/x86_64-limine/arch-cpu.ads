--  arch-cpu.ads: CPU management routines.
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

with Interfaces; use Interfaces;
with Userland.Process;
with Scheduler;
with Arch.GDT;
with Arch.Limine;

package Arch.CPU is
   --  Core-local data, that each core holds an own version of.
   type Core_Local;
   type Core_Local_Acc is access all Core_Local;
   type Core_Local is record
      --  Do not move the following members used in assembly code.
      Self            : Core_Local_Acc; --  Here for performance reasons.
      Kernel_Stack    : Unsigned_64;
      User_Stack      : Unsigned_64;
      --  End of members not to move.
      Number          : Positive;       --  Core number, 1 based.
      LAPIC_ID        : Unsigned_32;    --  LAPIC ID of the core.
      LAPIC_Timer_Hz  : Unsigned_64;
      Core_TSS        : Arch.GDT.TSS;
      Current_Thread  : Scheduler.TID;
      Current_Process : Userland.Process.PID;
      Retiring_Thread : Scheduler.TID;  --  See Arch.Local.
      Online          : Boolean := False; --  Its IDT and LAPIC are set up.
   end record;
   for Core_Local use record
      Self         at 0 range   0 ..  63;
      Kernel_Stack at 0 range  64 .. 127;
      User_Stack   at 0 range 128 .. 191;
   end record;

   --  Core locals and the number of cores, used as an index for the former.
   type Core_Local_Arr is array (Positive range <>) of aliased Core_Local;
   type Core_Local_Arr_Acc is access Core_Local_Arr;
   Core_Count  : Positive;
   Core_Locals : Core_Local_Arr_Acc;

   --  CPU features detected during initialization.
   Global_Use_XSAVE : Boolean := False;
   Global_Use_SMAP  : Boolean := False;
   Global_FPU_Size  : Unsigned_32;

   --  The TLB shootdown in flight, of which there is one at a time, made by
   --  Arch.MMU.Flush_TLBs. Every other online core drops the range of the
   --  table from its TLB if it has that table loaded, all of it if the whole
   --  flag is set, and then counts the pending cores down.
   Shootdown_Map     : Unsigned_64    := 0     with Volatile;
   Shootdown_Start   : System.Address          with Volatile;
   Shootdown_End     : System.Address          with Volatile;
   Shootdown_Whole   : Boolean        := False with Volatile;
   Shootdown_Pending : Unsigned_64    := 0     with Volatile;

   --  Init the cores and BSP.
   procedure Init_Cores;

   --  Get the core local structure of the passed core.
   function Get_Local return Core_Local_Acc;

private

   type MTRR_Store is array (Natural range <>) of Unsigned_64;
   type MTRR_Store_Acc is access MTRR_Store;

   --  XXX: All x86-64 systems have MTRR support, so we needn't check whether
   --  it's there or not, we just use it.
   Saved_MTRRs : MTRR_Store_Acc;
   procedure Save_MTRRs;
   procedure Restore_MTRRs;

   procedure Core_Bootstrap (Info : access Limine.SMP_CPU_Info)
      with Convention => C, Export;

   procedure Init_Core (Core_Number : Positive; LAPIC_ID : Unsigned_8);
   procedure Init_Common (Core_Number : Positive; LAPIC : Unsigned_32);
   procedure Get_BSP_LAPIC_ID (ID : out Unsigned_32);
end Arch.CPU;
