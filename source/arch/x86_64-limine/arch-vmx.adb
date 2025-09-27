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
with System.Machine_Code;
with Arch.Snippets;
with Messages;
with Memory.Physical;
with Memory.MMU;
with Arch.MMU;

package body Arch.VMX is
   IA32_FEATURE_CONTROL_MSR : constant := 16#3A#;
   IA32_VMX_BASIC           : constant := 16#480#;

   VMXON_LOCK_FLAG   : constant := 2#001#;
   VMXON_ENABLE_FLAG : constant := 2#100#;

   VMXON_Region_Addr : Integer_Address;

   procedure Initialize (Success : out Boolean) is
      EAX, EBX, ECX, EDX : Unsigned_32;
      Value, Revision_ID : Unsigned_64;
      Success_U16 : Unsigned_16;
   begin
      --  Check it is present.
      Arch.Snippets.Get_CPUID
         (Leaf    => 1,
          Subleaf => 0,
          EAX     => EAX,
          EBX     => EBX,
          ECX     => ECX,
          EDX     => EDX,
          Success => Success);
      if not Success then
         return;
      end if;
      if (ECX and Shift_Left (1, 5)) = 0 then
         Success := False;
         return;
      end if;

      --  Make sure VMX is enabled in the MSR. If it is enabled already, we
      --  cannot enable it, thats how the CPU works.
      Value := Snippets.Read_MSR (IA32_FEATURE_CONTROL_MSR);
      if ((Value and VMXON_LOCK_FLAG) /= 0) then
         if (Value and VMXON_ENABLE_FLAG) = 0 then
            Success := False;
            return;
         end if;
      else
         Value := Value or VMXON_LOCK_FLAG or VMXON_ENABLE_FLAG;
         Snippets.Write_MSR (IA32_FEATURE_CONTROL_MSR, Value);
      end if;

      --  Enable NE in CR0 and VMX in CR4
      Value := Snippets.Read_CR0;
      Snippets.Write_CR0 (Value or 2#100000#);
      Value := Snippets.Read_CR4;
      Snippets.Write_CR4 (Value or Shift_Left (1, 13));

      --  Enable VMXON.
      Revision_ID := Snippets.Read_MSR (IA32_VMX_BASIC);
      Memory.Physical.Alloc (Memory.MMU.Page_Size, VMXON_Region_Addr);
      declare
         Region : Unsigned_32
            with Import, Address => To_Address (VMXON_Region_Addr);
      begin
         Region := Unsigned_32 (Revision_ID and 16#FFFFFFFF#);
      end;
      VMXON_Region_Addr := VMXON_Region_Addr - Arch.MMU.Memory_Offset;
      System.Machine_Code.Asm
         ("xorw %%dx, %%dx;" &
          "movw $0x1, %%cx;" &
          "vmxon %1;"        &
          "cmovnc %%cx, %%dx",
           Outputs  => Unsigned_16'Asm_Output ("=d", Success_U16),
           Inputs   => Integer_Address'Asm_Input ("m", VMXON_Region_Addr),
           Clobber  => "memory,cc,rcx",
           Volatile => True);
      Success := Success_U16 /= 0;
      if Success then
         Messages.Put_Line ("VMX enabled");
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end Initialize;
end Arch.VMX;
