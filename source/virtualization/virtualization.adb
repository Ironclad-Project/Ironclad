--  virtualization.adb: Virtualization module of the kernel.
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

with Arch.Virtualization;

package body Virtualization is
   function Is_Supported return Boolean is
   begin
      return Arch.Virtualization.Is_Supported;
   end Is_Supported;
   ----------------------------------------------------------------------------
   procedure Create_Machine (M : out Machine) is
   begin
      M := (others => False);
   end Create_Machine;

   procedure Close (M : in out Machine) is
   begin
      null;
   end Close;
   ----------------------------------------------------------------------------
   procedure Close (C : in out CPU) is
   begin
      null;
   end Close;
   ----------------------------------------------------------------------------
   procedure IO_Control
      (M         : Machine;
       Request   : Unsigned_64;
       Arg       : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean)
   is
      pragma Unreferenced (M);
      pragma Unreferenced (Arg);
   begin
      case Request is
         when KVM_CREATE_VCPU | KVM_GET_DIRTY_LOG | KVM_MEMORY_ENCRYPT_OP =>
            Has_Extra := False;
            Extra     := 0;
            Success   := False;
         when others =>
            Has_Extra := False;
            Extra     := 0;
            Success   := False;
      end case;
   end IO_Control;

   procedure IO_Control
      (C         : CPU;
       Request   : Unsigned_64;
       Arg       : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (Arg);
   begin
      case Request is
         when KVM_RUN | KVM_GET_REGS | KVM_SET_REGS | KVM_GET_SREGS |
              KVM_SET_SREGS | KVM_TRANSLATE | KVM_INTERRUPT | KVM_GET_MSRS |
              KVM_SET_MSRS | KVM_SET_CPUID | KVM_SET_SIGNAL_MASK |
              KVM_GET_FPU | KVM_SET_FPU =>
            Has_Extra := False;
            Extra     := 0;
            Success   := False;
         when others =>
            Has_Extra := False;
            Extra     := 0;
            Success   := False;
      end case;
   end IO_Control;
end Virtualization;
