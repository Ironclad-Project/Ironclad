--  arch-exceptions.ads: Interrupt utilities.
--  Copyright (C) 2023 streaksu
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

package Arch.Interrupts is
   --  Passed to every interrupt called ever as an access.
   type ISR_GPRs is record
      --  Pushed by us.
      RAX : Unsigned_64;
      RBX : Unsigned_64;
      RCX : Unsigned_64;
      RDX : Unsigned_64;
      RSI : Unsigned_64;
      RDI : Unsigned_64;
      RBP : Unsigned_64;
      R8  : Unsigned_64;
      R9  : Unsigned_64;
      R10 : Unsigned_64;
      R11 : Unsigned_64;
      R12 : Unsigned_64;
      R13 : Unsigned_64;
      R14 : Unsigned_64;
      R15 : Unsigned_64;
      --  Passed naturaly by the interrupt handler.
      Error_Code : Unsigned_64;
      RIP        : Unsigned_64;
      CS         : Unsigned_64;
      RFLAGS     : Unsigned_64;
      RSP        : Unsigned_64;
      SS         : Unsigned_64;
   end record with Size => 1344;
   for ISR_GPRs use record
      RAX        at 0 range    0 ..   63;
      RBX        at 0 range   64 ..  127;
      RCX        at 0 range  128 ..  191;
      RDX        at 0 range  192 ..  255;
      RSI        at 0 range  256 ..  319;
      RDI        at 0 range  320 ..  383;
      RBP        at 0 range  384 ..  447;
      R8         at 0 range  448 ..  511;
      R9         at 0 range  512 ..  575;
      R10        at 0 range  576 ..  639;
      R11        at 0 range  640 ..  703;
      R12        at 0 range  704 ..  767;
      R13        at 0 range  768 ..  831;
      R14        at 0 range  832 ..  895;
      R15        at 0 range  896 ..  959;
      Error_Code at 0 range  960 .. 1023;
      RIP        at 0 range 1024 .. 1087;
      CS         at 0 range 1088 .. 1151;
      RFLAGS     at 0 range 1152 .. 1215;
      RSP        at 0 range 1216 .. 1279;
      SS         at 0 range 1280 .. 1343;
   end record;
   type ISR_GPRs_Acc is access ISR_GPRs;

   --  Generic handler for exceptions.
   procedure Exception_Handler (Num : Integer; State : not null ISR_GPRs_Acc)
      with Convention => C;

   --  Entrypoint of the syscall dispatcher.
   procedure Syscall_Handler (State : not null ISR_GPRs_Acc)
      with Export, Convention => C, External_Name => "syscall_handler";

   --  Entrypoint for the scheduler handler.
   Scheduler_Interrupt : constant := 16#82#;
   procedure Scheduler_Handler (Num : Integer; State : not null ISR_GPRs_Acc);

   --  Entrypoint for the scheduler handler.
   Panic_Interrupt : constant := 16#83#;
   procedure Panic_Handler;

   --  Entrypoint for the scheduler handler.
   Invalidate_Interrupt : constant := 16#84#;
   procedure Invalidate_Handler;

   --  Default ISR handler.
   procedure Default_ISR_Handler;

   --  LAPIC spurious interrupt handling.
   procedure Spurious_Handler;

private

   procedure Print_Triple (N1, N2, N3 : String; V1, V2, V3 : Unsigned_64);

   --  Returns true if memory error.

   type Machine_Check_Type is (Memory_MCE, Unrecognized_MCE);

   function Process_Machine_Check_Banks return Machine_Check_Type;
end Arch.Interrupts;
