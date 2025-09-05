--  memory-userland_transfer.ads: Userland copy to/from userland.
--  Copyright (C) 2025 streaksu
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

with Memory.MMU;
with System; use System;

generic
   type T is private;
package Memory.Userland_Transfer is
   --  Copy an object from userland to kernel memory.
   --  Accessibility checks are done by checking against the passed MMU table.
   procedure Take_From_Userland
      (Map     : Memory.MMU.Page_Table_Acc;
       Data    : out T;
       Addr    : System.Address;
       Success : out Boolean);

   --  Copy an object from kernel memory to userland.
   --  Accessibility checks are done by checking against the passed MMU table.
   procedure Paste_Into_Userland
      (Map     : Memory.MMU.Page_Table_Acc;
       Data    : T;
       Addr    : System.Address;
       Success : out Boolean);

   --  Accessibility checks are done by checking against the passed MMU table.
   procedure Check_Access
      (Map     : Memory.MMU.Page_Table_Acc;
       Addr    : System.Address;
       Write   : Boolean;
       Success : out Boolean);
end Memory.Userland_Transfer;
