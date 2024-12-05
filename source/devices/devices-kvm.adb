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

with Virtualization;
with Userland.Process; use Userland.Process;
with System.Storage_Elements; use System.Storage_Elements;
with Arch.Local;
with Arch.Virtualization;

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

      Proc : constant PID := Arch.Local.Get_Current_Process;
   begin
      case Request is
         when Virtualization.KVM_GET_API_VERSION =>
            Has_Extra := True;
            Extra     := Virtualization.KVM_Version;
            Success   := True;
         when Virtualization.KVM_CHECK_EXTENSION =>
            if To_Integer (Argument) = Virtualization.KVM_CAP_USER_MEMORY then
               Extra := 1;
            else
               Extra := 0;
            end if;

            Has_Extra := True;
            Success   := True;
         when Virtualization.KVM_GET_VCPU_MMAP_SIZE =>
            Has_Extra := True;
            Extra     := 4096;
            Success   := True;
         when Virtualization.KVM_CREATE_VM =>
            declare
               Desc : File_Description_Acc := new File_Description'
                  (Children_Count => 0,
                   Is_Blocking    => True,
                   Description    => Description_VM,
                   Inner_VM       => <>);
               Returned_FD : Natural;
            begin
               Virtualization.Create_Machine (Desc.Inner_VM);
               Add_File (Proc, Desc, Returned_FD, Success);
               if Success then
                  Has_Extra := True;
                  Extra     := Unsigned_64 (Returned_FD);
                  Success   := True;
               else
                  Close (Desc);
                  Has_Extra := False;
                  Extra     := 0;
                  Success   := False;
               end if;
            end;
         when Virtualization.KVM_GET_MSR_INDEX_LIST =>
            Has_Extra := False;
            Extra     := 0;
            Success   := True;

            declare
               MSR_Count : Unsigned_32 with Import, Address => Argument;
               MSRs : Arch.Virtualization.MSR_List (1 .. Natural (MSR_Count))
                  with Import, Address => Argument + Storage_Offset (4);
            begin
               Arch.Virtualization.Get_MSR_List (MSRs, Natural (MSR_Count));
            end;
         when Virtualization.KVM_MEMORY_ENCRYPT_REG_REGION |
              Virtualization.KVM_MEMORY_ENCRYPT_UNREG_REGION =>
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
