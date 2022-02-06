--  lib-panic.ads: Specification of the panic function package.
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

package Lib.Panic is
   --  Handler index to hardcode the panic handler to be to.
   Panic_Vector : constant := 16#69#;

   --  Warns about a weird situation, but doesnt die like a hard panic would.
   procedure Soft_Panic (Message : String);

   --  Panics for good, for cases when soft reboot is risky.
   procedure Hard_Panic (Message : String);
   pragma No_Return (Hard_Panic);

   --  Handler to be installed at Panic_Vector.
   procedure Panic_Handler;
end Lib.Panic;
