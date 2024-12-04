--  devices-kvm.adb: KVM device.
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

with Virtualization; use Virtualization;

package body Devices.KVM is
   procedure Init (Success : out Boolean) is
      Device : Resource;
   begin
      if Virtualization.Is_Supported then
         Device :=
            (Data        => System.Null_Address,
             ID          => (others => 0),
             Is_Block    => False,
             Block_Size  => 4096,
             Block_Count => 0,
             Read        => null,
             Write       => null,
             Sync        => null,
             Sync_Range  => null,
             IO_Control  => IO_Control'Access,
             Mmap        => null,
             Poll        => null,
             Remove      => null);
         Register (Device, "kvm", Success);
      else
         Success := True;
      end if;
   end Init;

   procedure IO_Control
      (Key       : System.Address;
       Request   : Unsigned_64;
       Argument  : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Argument);
   begin
      case Request is
         when KVM_GET_API_VERSION =>
            Has_Extra := True;
            Extra     := KVM_Version;
            Success   := True;
         when KVM_CHECK_EXTENSION =>
            --  We only support the core, thus, we will always return 0, which
            --  means unsupported.
            Has_Extra := True;
            Extra     := 0;
            Success   := True;
         when KVM_GET_VCPU_MMAP_SIZE =>
            Has_Extra := True;
            Extra     := 4096;
            Success   := True;
         when KVM_CREATE_VM | KVM_GET_MSR_INDEX_LIST |
              KVM_MEMORY_ENCRYPT_REG_REGION |
              KVM_MEMORY_ENCRYPT_UNREG_REGION =>
            --  TODO: Actually implement.
            Has_Extra := False;
            Extra     := 0;
            Success   := False;
         when others =>
            Has_Extra := False;
            Extra     := 0;
            Success   := False;
      end case;
   end IO_Control;
end Devices.KVM;
