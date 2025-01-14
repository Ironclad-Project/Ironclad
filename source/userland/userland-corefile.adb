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

with Devices;
with Interfaces; use Interfaces;
with Arch.Local;
with Userland.Process; use Userland.Process;
with Userland.MAC;     use Userland.MAC;
with Lib.Messages;
with VFS; use VFS;

package body Userland.Corefile is
   procedure Generate_Corefile (Ctx : Arch.Context.GP_Context) is
      --  TODO: Take into account the file size limit for the corefile, and not
      --  only the core file size.

      Inner_Ct : aliased constant Arch.Context.GP_Context := Ctx;
      Proc     : constant Process.PID := Arch.Local.Get_Current_Process;
      PID_Val  : Unsigned_64;
      PID_Len  : Natural;
      PID_Buf  : Lib.Messages.Translated_String;
      Success  : VFS.FS_Status;
      Core_FS  : VFS.FS_Handle;
      Core_Ino : VFS.File_Inode_Number;
      Ctx_Len  : Natural;
      Ctx_Data : constant Devices.Operation_Data (1 .. Inner_Ct'Size / 8)
         with Import, Address => Inner_Ct'Address;
   begin
      if Proc = Error_PID or else Get_Limit (Proc, Core_Size_Limit) = 0 then
         return;
      end if;

      PID_Val := Unsigned_64 (Process.Convert (Proc));
      Lib.Messages.Image (PID_Val, PID_Buf, PID_Len);

      declare
         File_Path : constant String := "/tmp/" &
            PID_Buf (PID_Buf'Last - PID_Len + 1 .. PID_Buf'Last) & ".core";
      begin
         VFS.Create_Node (File_Path, VFS.File_Regular, 8#777#, Success, 0);
         if Success /= VFS.FS_Success then
            Lib.Messages.Put_Line ("Could not create core file " & File_Path);
            return;
         end if;

         VFS.Open (File_Path, Core_FS, Core_Ino, Success, 0, True, True);
         if Success /= VFS.FS_Success then
            Lib.Messages.Put_Line ("Could not open core file " & File_Path);
            return;
         end if;

         VFS.Write (Core_FS, Core_Ino, 0, Ctx_Data, Ctx_Len, True, Success);
         if Success /= VFS.FS_Success or Ctx_Len /= Ctx_Data'Length then
            Lib.Messages.Put_Line ("Could not write core file " & File_Path);
            return;
         end if;

         if VFS.Synchronize (Core_FS, Core_Ino, False) = VFS.FS_Success then
            Lib.Messages.Put_Line ("Dumped core at " & File_Path);
         else
            Lib.Messages.Put_Line ("Failed dump at " & File_Path);
         end if;
      end;
   end Generate_Corefile;
end Userland.Corefile;
