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
with Ada.Unchecked_Deallocation;
with Userland.Process; use Userland.Process;
with Arch.Local;

package body Virtualization is
   function Is_Supported return Boolean is
   begin
      return Arch.Virtualization.Is_Supported;
   end Is_Supported;
   ----------------------------------------------------------------------------
   function Create_Machine return Machine_Acc is
   begin
      return new Machine'
         (Is_Read_Only   => False,
          Memory_Size    => 0,
          Userspace_Addr => 0,
          CPUs           => [others => (False, 0)]);
   end Create_Machine;

   procedure Close (M : in out Machine_Acc) is
      procedure Free is new Ada.Unchecked_Deallocation (Machine, Machine_Acc);
   begin
      Free (M);
   end Close;
   ----------------------------------------------------------------------------
   procedure Get_CPU
      (M       : Machine_Acc;
       C       : out CPU_Handle;
       Success : out Boolean)
   is
   begin
      C       := 1;
      Success := False;

      for Idx in M.CPUs'Range loop
         if not M.CPUs (Idx).Is_Present then
            M.CPUs (Idx).Is_Present := True;
            C       := Idx;
            Success := True;
            exit;
         end if;
      end loop;
   end Get_CPU;
   ----------------------------------------------------------------------------
   procedure IO_Control
      (M         : Machine_Acc;
       Request   : Unsigned_64;
       Arg       : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean)
   is
      KVM_MEM_READONLY : constant := 2#10#;
      type KVM_Userspace_Memory_Region is record
         Slot            : Unsigned_32;
         Flags           : Unsigned_32;
         Guest_Phys_Addr : Unsigned_64;
         Memory_Size     : Unsigned_64;
         Userspace_Addr  : Unsigned_64;
      end record with Pack;

      Proc : constant PID := Arch.Local.Get_Current_Process;
   begin
      case Request is
         when KVM_CREATE_VCPU =>
            declare
               Desc : File_Description_Acc := new File_Description'
                  (Children_Count   => 0,
                   Is_Blocking      => True,
                   Description      => Description_VCPU,
                   Inner_VCPU_Owner => M,
                   Inner_VCPU       => <>);
               Returned_FD : Natural;
            begin
               Get_CPU (M, Desc.Inner_VCPU, Success);
               if not Success then
                  Has_Extra := False;
                  Extra     := 0;
                  Success   := False;
                  return;
               end if;

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
         when KVM_SET_USER_MEMORY_REGION =>
            Has_Extra := False;
            Extra     := 0;
            Success   := False;

            declare
               Info : KVM_Userspace_Memory_Region with Import, Address => Arg;
            begin
               if Info.Guest_Phys_Addr = 0 then
                  M.Is_Read_Only   := (Info.Flags and KVM_MEM_READONLY) /= 0;
                  M.Memory_Size    := Info.Memory_Size;
                  M.Userspace_Addr := Info.Userspace_Addr;
                  Success          := True;
               end if;
            end;
         when others  =>
            Has_Extra := False;
            Extra     := 0;
            Success   := False;
      end case;
   end IO_Control;

   procedure IO_Control
      (M         : Machine_Acc;
       C         : CPU_Handle;
       Request   : Unsigned_64;
       Arg       : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean)
   is
      pragma Unreferenced (M);
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
