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
      Proc     : constant Process.PID := Arch.Local.Get_Current_Process;
      PID_Val  : constant Unsigned_64 := Unsigned_64 (Process.Convert (Proc));
      Core_Lim : constant Limit_Value := Get_Limit (Proc, Core_Size_Limit);
      File_Lim :          Limit_Value := Get_Limit (Proc, File_Size_Limit);
      PID_Len  : Natural;
      PID_Buf  : Lib.Messages.Translated_String;
      Success  : VFS.FS_Status;
      Core_FS  : VFS.FS_Handle;
      Core_Ino : VFS.File_Inode_Number;
      Ctx_Len  : Natural;
      Ctx_Data : Devices.Operation_Data (1 .. Ctx'Size / 8)
         with Import, Address => Ctx'Address;
   begin
      if Core_Lim = 0 then
         return;
      elsif Core_Lim < File_Lim then
         File_Lim := Core_Lim;
      end if;

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

         Success := VFS.Synchronize (Core_FS, Core_Ino, False);
         Lib.Messages.Put_Line
            ("Dumped core at " & File_Path & " (" &
             VFS.FS_Status'Image (Success) & ")");
      end;
   end Generate_Corefile;
end Userland.Corefile;
