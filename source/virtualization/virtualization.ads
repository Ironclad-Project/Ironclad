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

with Interfaces; use Interfaces;

package Virtualization is
   --  This module implements a KVM compatible interface, KVM's specification
   --  can be found at https://docs.kernel.org/virt/kvm/api.html

   --  KVM version that this module implements, we implement version 2, which
   --  means we provide VCPU-stop.
   NVMM_Version : constant := 2;

   --  Capabilities of this implementation.
   State_Size           : constant := 0; --  TODO.
   Max_Virtual_Machines : constant := 128;
   Max_CPUs_Per_VM      : constant := 4;
   Max_RAM_Per_VM       : constant := Unsigned_64'Last;
   ----------------------------------------------------------------------------
   --  Returns True if virtualization is supported.
   function Is_Supported return Boolean;

   procedure Initialize;
end Virtualization;
