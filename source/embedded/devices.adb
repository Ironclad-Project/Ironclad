--  devices.adb: Device management.
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

with Devices.E9;
with Devices.Serial;
with Devices.Streams;
with Lib.Panic;

package body Devices is
   procedure Init (Fb : access Arch.Stivale2.Framebuffer_Tag) is
      pragma Unreferenced (Fb);
   begin
      --  Initialize physical devices.
      if not E9.Init     then goto Error; end if;
      if not Serial.Init then goto Error; end if;

      --  Initialize virtual devices.
      if not Streams.Init then goto Error; end if;
      return;

   <<Error>>
      Lib.Panic.Soft_Panic ("Could not start devices");
   end Init;
end Devices;
