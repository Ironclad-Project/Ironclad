--  arch-cpu.adb: CPU management routines.
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
with Lib.Messages;
with Lib.Panic;

package body Arch.CPU with SPARK_Mode => Off is
   SMP_Request : Limine.SMP_Request :=
      (Base  =>
         (ID       => Limine.SMP_ID,
          Revision => 0,
          Response => System.Null_Address),
       Flags => 0)
      with Export, Async_Writers;

   procedure Init_Cores is
      BSP_Hart_ID : Unsigned_64;
      Idx         : Natural := 2;

      SMPPonse : Limine.RISCV64_SMP_Response
         with Import, Address => SMP_Request.Base.Response;
   begin
      --  Check we got a limine answer at all.
      if SMP_Request.Base.Response = System.Null_Address then
         Lib.Panic.Hard_Panic ("Limine SMP request needed");
      end if;

      --  Fetch info.
      Core_Count  := Natural (SMPPonse.CPU_Count);
      BSP_Hart_ID := SMPPonse.BSP_Hart_ID;

      --  Initialize the locals list, and initialize the BSP.
      Core_Locals := new Core_Local_Arr (1 .. Core_Count);
      Init_Common (1, BSP_Hart_ID);

      --  Initialize the other cores.
      if Core_Count > 1 then
         declare
            SMP_CPUs : Limine.RISCV64_CPU_Info_Arr (1 .. SMPPonse.CPU_Count)
               with Import, Address => SMPPonse.CPUs;
         begin
            for CPU of SMP_CPUs loop
               if CPU.Hart_ID /= BSP_Hart_ID then
                  CPU.Extra_Arg := Unsigned_64 (Idx);
                  CPU.Addr      := Core_Bootstrap'Address;
                  Idx           := Idx + 1;
               end if;
            end loop;
         end;
      end if;
   end Init_Cores;
   ----------------------------------------------------------------------------
   procedure Core_Bootstrap (Info : access Limine.RISCV64_SMP_CPU_Info) is
      Stp     : Lib.Messages.Translated_String;
      Stp_Len : Natural;
   begin
      Lib.Messages.Image (Info.Extra_Arg, Stp, Stp_Len);
      Lib.Messages.Put_Line ("Hello from core " & Stp);
      Init_Common (Natural (Info.Extra_Arg), Info.Hart_ID);
   end Core_Bootstrap;

   procedure Init_Common (Core_Number : Positive; Hart_ID : Unsigned_64) is
   begin
      Core_Locals (Core_Number) :=
         (Self            => Core_Locals (Core_Number)'Access,
          Kernel_Stack    => 0,
          User_Stack      => 0,
          Number          => Core_Number,
          Hart_ID         => Hart_ID,
          Current_Thread  => Scheduler.Error_TID,
          Current_Process => Userland.Process.Error_PID);
   end Init_Common;
end Arch.CPU;
