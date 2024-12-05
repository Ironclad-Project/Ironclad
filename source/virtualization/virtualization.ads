--  virtualization.ads: Virtualization module of the kernel.
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

with System;
with Interfaces; use Interfaces;

package Virtualization is
   --  This module implements a KVM compatible interface, KVM's specification
   --  can be found at https://docs.kernel.org/virt/kvm/api.html

   --  KVM version that this module implements, we implement version 12, that
   --  means all ioctls marked as basic will be implemented.
   KVM_Version : constant := 12;
   ----------------------------------------------------------------------------
   --  Mandatory ioctl calls to be provided by KVM version 12.
   --  They are listed here, but they may be implemented across the kernel.

   --  System ioctl (to /dev/kvm).
   KVM_GET_API_VERSION             : constant := 1;
   KVM_CREATE_VM                   : constant := 2;
   KVM_GET_MSR_INDEX_LIST          : constant := 3;
   KVM_CHECK_EXTENSION             : constant := 4;
   KVM_GET_VCPU_MMAP_SIZE          : constant := 5;
   KVM_MEMORY_ENCRYPT_REG_REGION   : constant := 6;
   KVM_MEMORY_ENCRYPT_UNREG_REGION : constant := 7;

   --  VM ioctl.
   KVM_CREATE_VCPU            : constant := 8;
   KVM_GET_DIRTY_LOG          : constant := 9;
   KVM_MEMORY_ENCRYPT_OP      : constant := 10;
   KVM_SET_USER_MEMORY_REGION : constant := 11;

   --  VCPU ioctl.
   KVM_RUN             : constant := 12;
   KVM_GET_REGS        : constant := 13;
   KVM_SET_REGS        : constant := 14;
   KVM_GET_SREGS       : constant := 15;
   KVM_SET_SREGS       : constant := 16;
   KVM_TRANSLATE       : constant := 17;
   KVM_INTERRUPT       : constant := 18;
   KVM_GET_MSRS        : constant := 19;
   KVM_SET_MSRS        : constant := 20;
   KVM_SET_CPUID       : constant := 21;
   KVM_SET_SIGNAL_MASK : constant := 22;
   KVM_GET_FPU         : constant := 23;
   KVM_SET_FPU         : constant := 24;

   --  KVM extensions the kernel supports that can be querried with
   --  KVM_CHECK_EXTENSION.
   KVM_CAP_USER_MEMORY : constant := 1;
   ----------------------------------------------------------------------------
   --  Returns True if virtualization is supported.
   function Is_Supported return Boolean;
   ----------------------------------------------------------------------------
   --  Type to represent a virtual machine.
   --  Virtual machines have an address space, as well as some cores inside.
   type Machine is private;

   --  Create a virtual machine.
   procedure Create_Machine (M : out Machine);

   --  Close a virtual machine.
   procedure Close (M : in out Machine);
   ----------------------------------------------------------------------------
   --  Type to represent a virtual CPU inside a virtual machine.
   type CPU is private;

   --  Close a virtual CPU.
   procedure Close (C : in out CPU);
   ----------------------------------------------------------------------------
   --  Userland IO control operations.
   procedure IO_Control
      (M         : Machine;
       Request   : Unsigned_64;
       Arg       : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean);

   procedure IO_Control
      (C         : CPU;
       Request   : Unsigned_64;
       Arg       : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean);

private

   type Machine is record
      Placeholder : Boolean;
   end record;

   type CPU is record
      Placeholder : Boolean;
   end record;
end Virtualization;
