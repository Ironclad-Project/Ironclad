--  entrypoint.adb: Specification of the main function's package.
--  Copyright (C) 2021 streaksu
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
with Lib.Messages;
with Lib.Panic;
with Config;

package body Arch.Entrypoint is
   procedure Bootstrap_Main (Blob : DTB.Header_Acc) is
      Reservation_Addr : System.Address;
   begin
      Lib.Messages.Put      (Config.Name & " " & Config.Version & " ");
      Lib.Messages.Put_Line ("booted from aarch64-virt");
      Lib.Messages.Put      ("Please report errors and issues to ");
      Lib.Messages.Put_Line (Config.Bug_Site);

      if not DTB.Is_Valid (Blob) then
         Lib.Panic.Hard_Panic ("The passed DTB is not valid");
      end if;

      Reservation_Addr := DTB.Get_Memory_Reservation (Blob);
      loop
         declare
            Reservation : DTB.Reservation with Address => Reservation_Addr;
         begin
            exit when Reservation.Size = 0 and Reservation.Address = 0;
            Lib.Messages.Put (Reservation.Address, False, True);
            Lib.Messages.Put ("+");
            Lib.Messages.Put (Reservation.Size, False, True);
            Reservation_Addr := Reservation_Addr + DTB.Reservation'Size / 8;
         end;
      end loop;

      Lib.Panic.Hard_Panic ("End of kernel");
   end Bootstrap_Main;
end Arch.Entrypoint;
