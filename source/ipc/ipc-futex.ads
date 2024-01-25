--  ipc-futex.ads: Fast userland mutex.
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

with Interfaces; use Interfaces;

package IPC.Futex is
   --  This module implements fast userland mutexes, despite the userland tag,
   --  they are perfectly usable inside kernel mode as well.
   --
   --  These futexes work by watching a memory address for an expected value,
   --  with some timeout limits. Once the expected value is found, the thread
   --  will wait for explicit wakeup.

   --  Information to depict an element to wait on.
   type Element is record
      Key      : access Unsigned_32;
      Expected : Unsigned_32;
   end record;
   type Element_Arr is array (Natural range <>) of Element;

   --  Wait for a set of keys.
   --  @param Keys Keys to wait for.
   --  @param Max_Seconds Maximum amount of seconds to wait for.
   --  @param Max_Nanos   Nanoseconds component of Max_Seconds.
   --  @return True in success, False in failure.
   function Wait
      (Keys        : Element_Arr;
       Max_Seconds : Unsigned_64;
       Max_Nanos   : Unsigned_64) return Boolean;

   --  Wake a set of keys.
   --  @param Keys Keys to wait for.
   procedure Wake (Keys : Element_Arr);
end IPC.Futex;
