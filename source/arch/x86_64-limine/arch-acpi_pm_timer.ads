--  arch-acpi_pm_timer.ads: ACPI power management timer support.
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

with Interfaces; use Interfaces;

package Arch.ACPI_PM_Timer is
   --  The timer counts up at this rate, in Hz, see ACPI 6.6, 4.8.2.1.
   Frequency : constant := 3_579_545;

   --  Find the timer, which is only used through I/O ports, as Limine does.
   --  @param Port    Port to read the count from, 32 bits at a time.
   --  @param Mask    Bits of the count that are valid, 24 or 32 of them.
   --  @param Success True if found, False otherwise.
   procedure Find
      (Port    : out Unsigned_16;
       Mask    : out Unsigned_32;
       Success : out Boolean);
end Arch.ACPI_PM_Timer;
