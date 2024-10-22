--  userland-oom_failure.adb: Code to manage OOM failures.
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

with Interfaces;       use Interfaces;
with Arch.MMU;         use Arch.MMU;
with Userland.Process; use Userland.Process;
with Userland.Syscall;
with Lib.Messages;

package body Userland.OOM_Failure is
   procedure Get_Killing_Config (Enabled : out Boolean) is
   begin
      Seize (Config_Mutex);
      Enabled := Is_Killing_Allowed;
      Release (Config_Mutex);
   end Get_Killing_Config;

   procedure Configure_Killing (Enabled : Boolean) is
   begin
      Seize (Config_Mutex);
      Is_Killing_Allowed := Enabled;
      Release (Config_Mutex);
   end Configure_Killing;

   procedure Handle_Failure is
      Count       : Natural;
      Items       : Process.Process_Info_Arr (1 .. 10);
      Map         : Arch.MMU.Page_Table_Acc;
      Guilty      : Natural;
      Guilty_Size : Unsigned_64;
      Size        : Unsigned_64;
      Str         : Lib.Messages.Translated_String;
   begin
      --  TODO: Free internal memory.
      if Is_Killing_Allowed then
         --  Get a selection of 10 processes.
         Process.List_All (Items, Count);
         if Count > Items'Length then
            Count := Items'Last;
         end if;

         --  Check the one with the highest memory mapped of the 10.
         Guilty      := 0;
         Guilty_Size := 0;
         for I in Items'First .. Count loop
            Map := Process.Get_Common_Map (Items (I).Process);
            if Map /= null then
               Size := Arch.MMU.Get_User_Mapped_Size (Map);
               if Size > Guilty_Size then
                  Guilty      := I;
                  Guilty_Size := Size;
               end if;
            end if;
         end loop;

         --  Once a guilty process has been chosen, move to execution.
         if Guilty /= 0 and then Items (Guilty).Process /= Process.Error_PID
         then
            Lib.Messages.Image
               (Unsigned_64 (Convert (Items (Guilty).Process)), Str, Count);
            Lib.Messages.Put_Line
               ("Killing PID "                         &
                Str (Str'Last - Count + 1 .. Str'Last) &
                " '"                                   &
                Items (Guilty).Identifier (1 .. Items (Guilty).Identifier_Len)
                &
                "' after being deemed guilty");
            Syscall.Do_Exit (Items (Guilty).Process, 1);
         end if;
      end if;
   end Handle_Failure;
end Userland.OOM_Failure;
