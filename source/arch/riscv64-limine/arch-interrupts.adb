--  arch-exceptions.adb: Specification of interrupt utilities.
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

with System.Machine_Code;
with Panic;
with Arch.Snippets;
with Scheduler;

package body Arch.Interrupts with SPARK_Mode => Off is
   Interrupt_Table : array (Interrupt_Index) of Interrupt_Handler
      := [others => null];

   procedure Initialize is
   begin
      Load_Trap_Vector;
   end Initialize;

   procedure Load_Trap_Vector is
   begin
      System.Machine_Code.Asm
         ("csrw stvec, %0",
          Inputs   => System.Address'Asm_Input ("r", trap_entry'Address),
          Volatile => True);
   end Load_Trap_Vector;

   procedure Setup_Interrupt
      (Handler : Interrupt_Handler;
       Index   : out Interrupt_Index;
       Success : out Boolean)
   is
   begin
      Index   := 1;
      Success := False;

      for I in Interrupt_Table'Range loop
         if Interrupt_Table (I) = null then
            Interrupt_Table (I) := Handler;
            Index := I;
            Success := True;
            return;
         end if;
      end loop;
   end Setup_Interrupt;

   procedure Unload_Interrupt (Index : Interrupt_Index) is
   begin
      Interrupt_Table (Index) := null;
   exception
      when Constraint_Error =>
         null;
   end Unload_Interrupt;
   ----------------------------------------------------------------------------
   procedure Handle_Trap (Ctx : not null Frame_Acc) is
      SCause : Unsigned_64;
      Is_Int : Boolean;
      Cause  : Unsigned_64;
   begin
      --  Read exception data to determine cause.
      System.Machine_Code.Asm
         ("csrr %0, scause",
          Outputs  => Unsigned_64'Asm_Output ("=r", SCause),
          Clobber  => "memory",
          Volatile => True);
      Is_Int := (SCause and Shift_Left (1, 63)) /= 0;
      Cause  := SCause and not Shift_Left (1, 63);

      if Is_Int then
         if Cause = 5 then
            System.Machine_Code.Asm
               ("csrc sie, %0",
                Inputs   => Unsigned_64'Asm_Input ("r", 32),
                Clobber  => "memory",
                Volatile => True);

            Scheduler.Scheduler_ISR (Ctx.all);
         else
            Panic.Hard_Panic ("Missing interrupt with cause " & Cause'Image);
         end if;
      else
         --  FIXME: Adding or removing any case makes this not work.
         --  Like ????
         Panic.Hard_Panic
            ((case Cause is
               when 0 => "Instruction misalign",
               when 1 => "Instruction access",
               when 2 => "Illegal instruction",
               when 3 => "Breakpoint",
               when 4 => "Reserved",
               when 5 => "Load access fault",
               when 6 => "AMO access misaligned",
               when 7 => "Store/AMO access fault",
               when 8 => "Environment call",
               when others => "Reserved trap"), Ctx.all);
      end if;

      --  Reenable interrupts.
      Snippets.Enable_Interrupts;
   exception
      when Constraint_Error =>
         Panic.Hard_Panic ("Trap handler faced an exception");
   end Handle_Trap;
end Arch.Interrupts;
