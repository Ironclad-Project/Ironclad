--  arch-vmx.adb: Intel VT-x virtualization code.
--  Copyright (C) 2024 mintsuki
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
with Arch.Snippets;

package body Arch.VMX is
   function Is_Supported return Boolean is
      EAX, EBX, ECX, EDX : Unsigned_32;
      CPUID_Success : Boolean;
   begin
      Arch.Snippets.Get_CPUID
         (Leaf    => 1,
          Subleaf => 0,
          EAX     => EAX,
          EBX     => EBX,
          ECX     => ECX,
          EDX     => EDX,
          Success => CPUID_Success);
      if not CPUID_Success then
         return False;
      end if;
      return (ECX and Shift_Left (1, 5)) /= 0;
   end Is_Supported;

   procedure Initialize is
   begin
      return;
   end Initialize;
end Arch.VMX;
