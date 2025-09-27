--  arch-virtualization-svm.adb: AMD-V virtualization code.
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

with Interfaces; use Interfaces;
with Arch.Snippets;
with Messages;

package body Arch.Virtualization.SVM is
   EFER_MSR : constant := 16#C0000080#;

   procedure Initialize (Success : out Boolean) is
      EAX, EBX, ECX, EDX : Unsigned_32;
      Value : Unsigned_64;
   begin
      --  Check it is present.
      Arch.Snippets.Get_CPUID
         (Leaf    => 16#80000001#,
          Subleaf => 0,
          EAX     => EAX,
          EBX     => EBX,
          ECX     => ECX,
          EDX     => EDX,
          Success => Success);
      if not Success then
         return;
      end if;
      if (ECX and Shift_Left (1, 2)) = 0 then
         Success := False;
         return;
      end if;

      --  Enable the SVME bit in EFER.
      Value := Snippets.Read_MSR (EFER_MSR);
      Snippets.Write_MSR (EFER_MSR, Value or Shift_Left (1, 12));
      Messages.Put_Line ("SVM enabled");
      Success := True;
   end Initialize;
end Arch.Virtualization.SVM;
