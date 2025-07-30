--  arch-sbi.adb: Supervisor Binary Interface (SBI) wrapper.
--  Copyright (C) 2025 Sean Weeks, streaksu
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
with Arch.Clocks;

package body Arch.SBI is
   Base_Extension_EID : constant := 16#10#;
   Time_Extension_EID : constant := 16#54494D45#;

   procedure Is_Present (Success : out Boolean) is
   begin
      Probe_Extension (Base_Extension_EID, Success);
   end Is_Present;

   procedure Probe_Extension (EID : Unsigned_64; Success : out Boolean) is
      Result, Error : Unsigned_64;
   begin
      SBI_ECall
         (Extension_ID => Base_Extension_EID,
          Function_ID  => 3,
          Result       => Result,
          Error        => Error,
          Arg0         => EID,
          Arg1         => 0,
          Arg2         => 0);

      Success := (Error = SBI_SUCCESS) and (Result = 1);
   end Probe_Extension;

   procedure Set_Timer (Microseconds : Unsigned_64; Success : out Boolean) is
      Result, Error, Start : Unsigned_64;
   begin
      Probe_Extension (Time_Extension_EID, Success);
      if not Success then
         return;
      end if;

      System.Machine_Code.Asm
         ("rdtime %0",
          Outputs  => Unsigned_64'Asm_Output ("=r", Start),
          Volatile => True);

      Start := Start + (Microseconds * Clocks.Ticks_Per_Microsecond);

      SBI_ECall
         (Extension_ID => Time_Extension_EID,
          Function_ID  => 0,
          Result       => Result,
          Error        => Error,
          Arg0         => Start,
          Arg1         => 0,
          Arg2         => 0);

      Success := Error = SBI_SUCCESS;
   end Set_Timer;

   procedure Get_Arch_ID (Result : out Unsigned_64; Success : out Boolean) is
      Error : Unsigned_64;
   begin
      SBI_ECall
         (Extension_ID => Base_Extension_EID,
          Function_ID  => 5,
          Result       => Result,
          Error        => Error,
          Arg0         => 0,
          Arg1         => 0,
          Arg2         => 0);
      Success := Error = SBI_SUCCESS;
   end Get_Arch_ID;

   procedure Get_Vendor_ID (Result : out Unsigned_64; Success : out Boolean) is
      Error : Unsigned_64;
   begin
      SBI_ECall
         (Extension_ID => Base_Extension_EID,
          Function_ID  => 4,
          Result       => Result,
          Error        => Error,
          Arg0         => 0,
          Arg1         => 0,
          Arg2         => 0);
      Success := Error = SBI_SUCCESS;
   end Get_Vendor_ID;
   ----------------------------------------------------------------------------
   procedure SBI_ECall
     (Extension_ID : Unsigned_64;
      Function_ID  : Unsigned_64;
      Result       : out Unsigned_64;
      Error        : out Unsigned_64;
      Arg0         : Unsigned_64;
      Arg1         : Unsigned_64;
      Arg2         : Unsigned_64)
   is
   begin
      System.Machine_Code.Asm
         ("mv a7, %2;" &
          "mv a6, %3;" &
          "mv a0, %4;" &
          "mv a1, %5;" &
          "mv a2, %6;" &
          "ecall;"     &
          "mv %0, a0;" &
          "mv %1, a1",
          Outputs => [Unsigned_64'Asm_Output ("=r", Error),
                      Unsigned_64'Asm_Output ("=r", Result)],
          Inputs  => [Unsigned_64'Asm_Input ("r", Extension_ID),
                      Unsigned_64'Asm_Input ("r", Function_ID),
                      Unsigned_64'Asm_Input ("r", Arg0),
                      Unsigned_64'Asm_Input ("r", Arg1),
                      Unsigned_64'Asm_Input ("r", Arg2)],
          Clobber => "a0,a1,a2,a6,a7,memory");
   end SBI_ECall;
end Arch.SBI;
