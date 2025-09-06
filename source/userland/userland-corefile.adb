--  userland-corefile.adb: Corefile generator.
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

with Ada.Unchecked_Conversion;
with Devices;
with Arch.Local;
with Userland.Process; use Userland.Process;
with Userland.MAC;     use Userland.MAC;
with Messages;
with VFS; use VFS;
with Interfaces; use Interfaces;

package body Userland.Corefile is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   procedure Generate_Corefile (Ctx : Arch.Context.GP_Context) is
      subtype Ctx_Data is Devices.Operation_Data (1 .. Ctx'Size / 8);
      function Conv is new Ada.Unchecked_Conversion
         (Arch.Context.GP_Context, Ctx_Data);

      Proc     : constant Process.PID := Arch.Local.Get_Current_Process;
      PID_Val  : Natural;
      Success  : VFS.FS_Status;
      Core_FS  : VFS.FS_Handle;
      Core_Ino : VFS.File_Inode_Number;
      Limit    : Limit_Value;
      Ctx_Len  : Natural;
      To_Write : Natural;
      Data     : constant Ctx_Data := Conv (Ctx);
   begin
      if Proc = Error_PID then
         return;
      end if;

      To_Write := Data'Length;
      Limit    := Get_Limit (Proc, Core_Size_Limit);
      if Limit.Soft_Limit < Unsigned_64 (To_Write) then
         To_Write := Natural (Limit.Soft_Limit);
      end if;
      Limit := Get_Limit (Proc, File_Size_Limit);
      if Limit.Soft_Limit < Unsigned_64 (To_Write) then
         To_Write := Natural (Limit.Soft_Limit);
      end if;

      if To_Write = 0 then
         return;
      end if;

      PID_Val := Process.Convert (Proc);

      declare
         File_Path : constant String := "/tmp/" & PID_Val'Image & ".core";
      begin
         VFS.Create_Node (File_Path, VFS.File_Regular, 8#777#, Success, 0, 0);
         if Success /= VFS.FS_Success then
            Messages.Put_Line ("Could not create core file " & File_Path);
            return;
         end if;

         VFS.Open (File_Path, Core_FS, Core_Ino, Success, 0, True, True);
         if Success /= VFS.FS_Success then
            Messages.Put_Line ("Could not open core file " & File_Path);
            return;
         end if;

         VFS.Write (Core_FS, Core_Ino, 0, Data (1 .. To_Write), Ctx_Len,
            True, Success);
         if Success /= VFS.FS_Success or Ctx_Len /= To_Write then
            Messages.Put_Line ("Could not write core file " & File_Path);
            return;
         end if;

         if VFS.Synchronize (Core_FS, Core_Ino, False) = VFS.FS_Success then
            Messages.Put_Line ("Dumped core at " & File_Path);
         else
            Messages.Put_Line ("Failed dump at " & File_Path);
         end if;
      end;
   end Generate_Corefile;
end Userland.Corefile;
