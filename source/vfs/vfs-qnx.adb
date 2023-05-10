--  vfs-qnx.adb: QNX6 FS driver.
--  Copyright (C) 2023 streaksu
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

package body VFS.QNX with SPARK_Mode => Off is
   function Probe
      (Handle       : Device_Handle;
       Do_Read_Only : Boolean) return System.Address
   is
      pragma Unreferenced (Handle);
      pragma Unreferenced (Do_Read_Only);
   begin
      return Null_Address;
   end Probe;

   procedure Unmount (FS : in out System.Address) is
   begin
      FS := System.Null_Address;
   end Unmount;
end VFS.QNX;
