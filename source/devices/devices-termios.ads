--  devices-termios.ads: Virtual stream device library specification.
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

package Devices.TermIOs is
   --  Structures handled by the termios interface.
   type Special_Characters is array (1 .. 11) of Unsigned_32;
   type Local_Modes is record
      Echo_Input         : Boolean;
      Do_Erase           : Boolean;
      Do_Kill            : Boolean;
      Echo_Newline       : Boolean;
      Canonical_Mode     : Boolean;
      Do_Implementation  : Boolean;
      Generate_Signals   : Boolean;
      No_Flushing_Signal : Boolean;
      Send_Stop          : Boolean;
      Print_As_Erased    : Boolean;
   end record with Size => 32;
   for Local_Modes use record
      Echo_Input         at 0 range 0 .. 0;
      Do_Erase           at 0 range 1 .. 1;
      Do_Kill            at 0 range 2 .. 2;
      Echo_Newline       at 0 range 3 .. 3;
      Canonical_Mode     at 0 range 4 .. 4;
      Do_Implementation  at 0 range 5 .. 5;
      Generate_Signals   at 0 range 6 .. 6;
      No_Flushing_Signal at 0 range 7 .. 7;
      Send_Stop          at 0 range 8 .. 8;
      Print_As_Erased    at 0 range 9 .. 9;
   end record;
   type Main_Data is record
      Input_Modes   : Unsigned_32;
      Output_Modes  : Unsigned_32;
      Control_Modes : Unsigned_32;
      Local_Mode    : Local_Modes;
      Special_Chars : Special_Characters;
      Input_Baud    : Unsigned_32;
      Output_Baud   : Unsigned_32;
   end record with Size => 544;

   type Win_Size is record
      Rows     : Unsigned_16;
      Columns  : Unsigned_16;
      X_Pixels : Unsigned_16;
      Y_Pixels : Unsigned_16;
   end record;

   --  Arguments for TCFLSH.
   TCIFLUSH  : constant := 0; --  Flush only received and not read.
   TCOFLUSH  : constant := 1; --  Flush only written and not read.
   TCIOFLUSH : constant := 2; --  Do both!

   --  Arguments for TCXONC.
   TCOOFF : constant := 0; --  Suspend output.
   TCOON  : constant := 1; --  Restart suspended output.
   TCIOFF : constant := 2; --  Suspend transmitting.
   TCION  : constant := 3; --  Restart transmitting.

   --  Standard ioctls for devices implementing termios.
   TCGETS     : constant := 16#5401#;
   TCSETS     : constant := 16#5402#;
   TCSETSW    : constant := 16#5403#;
   TCSETSF    : constant := 16#5404#;
   TCXONC     : constant := 16#540A#; --  Set flow conditions.
   TCFLSH     : constant := 16#540B#; --  Flush pending data as described.
   TIOCGWINSZ : constant := 16#5413#;
   TIOCSWINSZ : constant := 16#5414#;
end Devices.TermIOs;
