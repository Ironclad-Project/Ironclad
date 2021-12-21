--  arch-stivale2.adb: Stivale2 utilities.
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

with System;              use System;
with System.Machine_Code; use System.Machine_Code;
with System.Address_To_Access_Conversions;

package body Arch.Stivale2 is
   package Convert is new System.Address_To_Access_Conversions (Tag);

   Terminal_Enabled    : Boolean;
   Terminal_Entrypoint : System.Address;

   function Get_Tag
      (Proto     : access Header;
      Identifier : Unsigned_64) return System.Address is
      searchAddress : System.Address := Proto.Tags;
      searchTag     : access Tag;
   begin
      while searchAddress /= System'To_Address (0) loop
         searchTag := Convert.To_Pointer (searchAddress);
         if searchTag.Identifier = Identifier then
            return searchAddress;
         end if;
         searchAddress := searchTag.Next;
      end loop;
      return System'To_Address (0);
   end Get_Tag;

   procedure Init_Terminal (Terminal : access TerminalTag) is
   begin
      Terminal_Enabled    := True;
      Terminal_Entrypoint := Terminal.TermWrite;
   end Init_Terminal;

   procedure Print_Terminal (Message : String) is
      use ASCII;
   begin
      if Terminal_Enabled then
         Asm ("push %%rdi" & LF & HT &
              "push %%rsi" & LF & HT &
              "call *%0"   & LF & HT &
              "pop %%rsi"  & LF & HT &
              "pop %%rdi",
              Inputs => (System.Address'Asm_Input ("rm", Terminal_Entrypoint),
                         System.Address'Asm_Input ("D",  Message'Address),
                         Unsigned_64'Asm_Input    ("S",  Message'Length)),
              Clobber  => "rax, rdx, rcx, r8, r9, r10, r11",
              Volatile => True);
      end if;
   end Print_Terminal;
end Arch.Stivale2;
