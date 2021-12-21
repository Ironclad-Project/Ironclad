--  arch-stivale2.ads: Specification of stivale2 utilities and tags.
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

with System;
with Interfaces; use Interfaces;

package Arch.Stivale2 is
   --  IDs of several tags.
   CmdlineID  : constant := 16#E5E76A1B4597A781#;
   TerminalID : constant := 16#C2B3F4C3233B0974#;

   --  Stivale2 header passed by the bootloader to kernel.
   type Header is record
      BootloaderBrand   : String (1 .. 64);
      BootloaderVersion : String (1 .. 64);
      Tags              : System.Address;
   end record;
   pragma Convention (C, Header);

   --  Stivale2 tag passed in front of all specialized tags.
   type Tag is record
      Identifier : Unsigned_64;
      Next       : System.Address;
   end record;
   pragma Convention (C, Tag);

   type CmdlineTag is record
      TagInfo : Tag;
      Cmdline : System.Address;
   end record;
   pragma Convention (C, CmdlineTag);

   type TerminalTag is record
      TagInfo   : Tag;
      Flags     : Unsigned_32;
      Cols      : Unsigned_16;
      Rows      : Unsigned_16;
      TermWrite : System.Address;
      MaxLength : Unsigned_64;
   end record;
   pragma Convention (C, TerminalTag);

   --  Find a header.
   function Get_Tag
      (Proto     : access Header;
      Identifier : Unsigned_64) return System.Address;

   --  Initialize the terminal with a header.
   procedure Init_Terminal (Terminal : access TerminalTag);

   --  Print a string using the stivale2 terminal.
   procedure Print_Terminal (Message : String);
end Arch.Stivale2;
