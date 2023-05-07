--  arch-exceptions.ads: Specification of interrupt utilities.
--  Copyright (C) 2021 streaksu
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

package Arch.Interrupts with SPARK_Mode => Off is
   --  Passed to every interrupt called ever as an access.
   type Frame is record
      SPSR : Unsigned_64;
      PC   : Unsigned_64;
      X30  : Unsigned_64;
      X29  : Unsigned_64;
      X28  : Unsigned_64;
      X27  : Unsigned_64;
      X26  : Unsigned_64;
      X25  : Unsigned_64;
      X24  : Unsigned_64;
      X23  : Unsigned_64;
      X22  : Unsigned_64;
      X21  : Unsigned_64;
      X20  : Unsigned_64;
      X19  : Unsigned_64;
      X18  : Unsigned_64;
      X17  : Unsigned_64;
      X16  : Unsigned_64;
      X15  : Unsigned_64;
      X14  : Unsigned_64;
      X13  : Unsigned_64;
      X12  : Unsigned_64;
      X11  : Unsigned_64;
      X10  : Unsigned_64;
      X9   : Unsigned_64;
      X8   : Unsigned_64;
      X7   : Unsigned_64;
      X6   : Unsigned_64;
      X5   : Unsigned_64;
      X4   : Unsigned_64;
      X3   : Unsigned_64;
      X2   : Unsigned_64;
      X1   : Unsigned_64;
      X0   : Unsigned_64;
   end record with Convention => C;

   procedure Exception_Handler (State : aliased in out Frame)
      with Convention => C, Export, External_Name => "exception_handler";

   procedure Syscall_Handler (State : aliased in out Frame);

   procedure Print_Fatal (Fr : Frame; Message : String) with No_Return;
end Arch.Interrupts;
