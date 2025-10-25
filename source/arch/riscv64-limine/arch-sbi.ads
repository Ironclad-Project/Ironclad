--  arch-sbi.ads: Supervisor Binary Interface (SBI) wrapper.
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

with Interfaces; use Interfaces;

package Arch.SBI is
   procedure Is_Present (Success : out Boolean);

   --  Returns True if that extension ID is implemented.
   procedure Probe_Extension (EID : Unsigned_64; Success : out Boolean);

   --  Set the timer interrupt to interrupt for the passed stime value.
   procedure Set_Timer (Microseconds : Unsigned_64; Success : out Boolean);

   --  marchid is not always supported or gives issues, so we can just SBI.
   procedure Get_Arch_ID (Result : out Unsigned_64; Success : out Boolean);

   --  marchid is not always supported or gives issues, so we can just SBI.
   --  Result is a JEDEC ID or 0.
   procedure Get_Vendor_ID (Result : out Unsigned_64; Success : out Boolean);

   --  Write to console.
   procedure Console_Write (Message : String; Success : out Boolean);

private

   SBI_SUCCESS             : constant := 0;
   SBI_ERR_FAILED          : constant := Unsigned_64'Last;
   SBI_ERR_NOT_SUPPORTED   : constant := Unsigned_64'Last - 1;
   SBI_ERR_INVALID_PARAM   : constant := Unsigned_64'Last - 2;
   SBI_ERR_DENIED          : constant := Unsigned_64'Last - 3;
   SBI_ERR_INVALID_ADDRESS : constant := Unsigned_64'Last - 4;
   SBI_ERR_ALREADY_AVAIL   : constant := Unsigned_64'Last - 5;
   SBI_ERR_ALREADY_STARTED : constant := Unsigned_64'Last - 6;
   SBI_ERR_ALREADY_STOPPED : constant := Unsigned_64'Last - 7;

   procedure SBI_ECall
     (Extension_ID : Unsigned_64;
      Function_ID  : Unsigned_64;
      Result       : out Unsigned_64;
      Error        : out Unsigned_64;
      Arg0         : Unsigned_64;
      Arg1         : Unsigned_64;
      Arg2         : Unsigned_64);
end Arch.SBI;
