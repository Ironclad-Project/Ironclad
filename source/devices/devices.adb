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

with Devices.Streams;
with Devices.TTY;
with Devices.Debug;
with Lib.Panic;
with Config;
with Arch.Hooks;

package body Devices with SPARK_Mode => Off is
   procedure Init is
   begin
      --  Initialize architectural devices.
      Arch.Hooks.Devices_Hook;

      --  Initialize non-embedded devices.
      if not Config.Is_Embedded then
         if not TTY.Init then goto Error; end if;
      end if;

      --  Initialize common devices.
      if not Debug.Init   then goto Error; end if;
      if not Streams.Init then goto Error; end if;
      return;

   <<Error>>
      Lib.Panic.Soft_Panic ("Could not start devices");
   end Init;
end Devices;
