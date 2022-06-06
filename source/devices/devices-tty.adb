--  devices-tty.adb: Expose a TTY device.
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Arch.Stivale2;
with VFS.File; use VFS.File;
with VFS;

package body Devices.TTY is
   type Termios_Special_Characters is array (1 .. 11) of Unsigned_32;
   type Termios_Local_Modes is record
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
   for Termios_Local_Modes use record
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
   type Termios is record
      Input_Modes   : Unsigned_32;
      Output_Modes  : Unsigned_32;
      Control_Modes : Unsigned_32;
      Local_Modes   : Termios_Local_Modes;
      Special_Chars : Termios_Special_Characters;
      Input_Baud    : Unsigned_32;
      Output_Baud   : Unsigned_32;
   end record with Size => 544;
   type Terminal_Data is record
      Terminal_Settings : Termios;
      Keyboard_File     : VFS.File.File_Acc;
   end record;
   type Terminal_Data_Acc is access Terminal_Data;

   function Init return Boolean is
      Dev  : VFS.Device_Data;
      Data : constant Terminal_Data_Acc := new Terminal_Data;
   begin
      Data.Terminal_Settings.Local_Modes.Canonical_Mode := True;
      Data.Terminal_Settings.Local_Modes.Echo_Input     := True;
      Data.Keyboard_File := Open ("/dev/ps2keyboard", Access_R);

      Dev.Name (1 .. 6)       := "ttydev";
      Dev.Name_Len            := 6;
      Dev.Data                := Data.all'Address;
      Dev.Stat.Type_Of_File   := VFS.File_Character_Device;
      Dev.Stat.Mode           := 8#660#;
      Dev.Stat.Byte_Size      := 0;
      Dev.Stat.IO_Block_Size  := 4096;
      Dev.Stat.IO_Block_Count := 0;
      Dev.Read                := Read'Access;
      Dev.Write               := Write'Access;
      Dev.IO_Control          := IO_Control'Access;
      return VFS.Register (Dev);
   end Init;

   function Read
      (Data     : System.Address;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Offset);
      Term : Terminal_Data with Address => Data, Import;
      Read_Count : Unsigned_64;
   begin
      if Term.Terminal_Settings.Local_Modes.Canonical_Mode then
         declare
            C            : Character;
            Discard      : Unsigned_64;
            Buffer       : String (1 .. 100);
            Buffer_Index : Natural := 0;
            Destination  : String (1 .. 100) with Address => To_Write;
         begin
            loop
               Discard := VFS.File.Read (Term.Keyboard_File, 1, C'Address);
               exit when Buffer_Index >= Buffer'Length;
               case C is
                  when LF =>
                     exit;
                  when BS =>
                     if Buffer_Index /= 0 then
                        Buffer_Index := Buffer_Index - 1;
                     end if;
                  when others =>
                     Buffer (Buffer_Index + 1) := C;
                     Buffer_Index := Buffer_Index + 1;
               end case;
               if Term.Terminal_Settings.Local_Modes.Echo_Input then
                  Arch.Stivale2.Print_Terminal (C);
               end if;
            end loop;
            Destination (1 .. Buffer_Index) := Buffer (1 .. Buffer_Index);
            return Unsigned_64 (Buffer_Index);
         end;
      else
         Read_Count := VFS.File.Read (Term.Keyboard_File, Count, To_Write);
         if Term.Terminal_Settings.Local_Modes.Echo_Input then
            declare
               S : String (1 .. Natural (Read_Count)) with Address => To_Write;
            begin
               Arch.Stivale2.Print_Terminal (S);
            end;
         end if;
         return Read_Count;
      end if;
   end Read;

   function Write
      (Data     : System.Address;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Offset);
      Message : String (1 .. Natural (Count)) with Address => To_Write;
   begin
      Arch.Stivale2.Print_Terminal (Message);
      return Count;
   end Write;

   IO_Control_TCGETS     : constant := 16#5401#;
   IO_Control_TCSETS     : constant := 16#5402#;
   IO_Control_TCSETSW    : constant := 16#5403#;
   IO_Control_TCSETSF    : constant := 16#5404#;
   IO_Control_TIOCGWINSZ : constant := 16#5413#;

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      type Window_Size is record
         Rows     : Unsigned_16;
         Columns  : Unsigned_16;
         X_Pixels : Unsigned_16;
         Y_Pixels : Unsigned_16;
      end record;

      Term : Terminal_Data with Address => Data, Import;
      Requested_Termios : Termios     with Address => Argument, Import;
      Requested_Window  : Window_Size with Address => Argument, Import;
   begin
      case Request is
         when IO_Control_TCGETS =>
            Requested_Termios := Term.Terminal_Settings;
            return True;
         when IO_Control_TCSETS | IO_Control_TCSETSW | IO_Control_TCSETSF =>
            Term.Terminal_Settings := Requested_Termios;
            return True;
         when IO_Control_TIOCGWINSZ =>
            --  TODO: This is hardcoded data assuming a 16x8 font.
            Requested_Window := (
               Rows     => 25,
               Columns  => 80,
               X_Pixels => 80 * 8,
               Y_Pixels => 25 * 16
            );
            return True;
         when others =>
            return False;
      end case;
   end IO_Control;
end Devices.TTY;
