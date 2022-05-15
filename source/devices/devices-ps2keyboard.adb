--  devices-ps2keyboard.adb: PS2 keyboard driver.
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
with Arch.IDT;
with Arch.APIC;
with Arch.CPU;
with Arch.Wrappers;
with Arch.Stivale2;
with VFS.Device;

package body Devices.PS2Keyboard is
   --  There can only be 1 PS2 keyboard, so we can store the private data here
   --  to avoid the indirection of having it in the root object.
   Is_Capslock_Active : Boolean;
   Is_Shift_Active    : Boolean;
   Is_Ctrl_Active     : Boolean;
   Has_Extra_Code     : Boolean;

   --  Buffer to be filled by the handler.
   Is_Reading    : Boolean with Volatile;
   Buffer_Length : Natural with Volatile;
   Key_Buffer    : String (1 .. 100);

   --  Special keys and notable values.
   Max_Code            : constant := 16#57#;
   Capslock_Press      : constant := 16#3A#;
   Right_Shift_Press   : constant := 16#36#;
   Right_Shift_Release : constant := 16#B6#;
   Left_Shift_Press    : constant := 16#2A#;
   Left_Shift_Release  : constant := 16#AA#;
   Ctrl_Press          : constant := 16#1D#;
   Ctrl_Release        : constant := 16#9D#;

   --  Different mappings for different modifiers.
   Capslock_Mapping : constant array (0 .. 57) of Character := (
      NUL, ESC, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', BS,
      HT, 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '[', ']', LF,
      NUL, 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ';', ''', '`', NUL,
      '\', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', ',', '.', '/', NUL, NUL, NUL, ' '
   );

   Shift_Mapping : constant array (0 .. 57) of Character := (
      NUL, ESC, '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', BS,
      HT, 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}', LF,
      NUL, 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':', '"', '~', NUL,
      '|', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?', NUL, NUL, NUL, ' '
   );

   Shift_Capslock_Mapping : constant array (0 .. 57) of Character := (
      NUL, ESC, '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', BS,
      HT, 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '{', '}', LF,
      NUL, 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ':', '"', '~', NUL,
      '|', 'z', 'x', 'c', 'v', 'b', 'n', 'm', '<', '>', '?', NUL, NUL, NUL, ' '
   );

   Normal_Mapping : constant array (0 .. 57) of Character := (
      NUL, ESC, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', BS,
      HT, 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', LF,
      NUL, 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', ''', '`', NUL,
      '\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/', NUL, NUL, NUL, ' '
   );

   function Init return Boolean is
      BSP_LAPIC : constant Unsigned_32 := Arch.CPU.Core_Locals (1).LAPIC_ID;
      Index     : Arch.IDT.IRQ_Index;
      Unused    : Unsigned_8;
      Dev       : VFS.Device.Device_Data;
   begin
      --  Set the interrupt up, which is always the 34 (we are 1 based).
      if not Arch.IDT.Load_ISR (Keyboard_Handler'Address, Index) then
         return False;
      end if;
      if not Arch.APIC.IOAPIC_Set_Redirect (BSP_LAPIC, 34, Index, True) then
         return False;
      end if;

      --  Read from port 0x60 to flush the PS/2 controller buffer.
      while (Arch.Wrappers.Port_In (16#64#) and 1) /= 0 loop
         Unused := Arch.Wrappers.Port_In (16#60#);
      end loop;

      Dev.Name              := "ps2keyb";
      Dev.Data              := System.Null_Address;
      Dev.Stat.Type_Of_File := VFS.File_Character_Device;
      Dev.Stat.Mode         := 8#660#;
      Dev.Sync              := null;
      Dev.Read              := Read'Access;
      Dev.Write             := null;
      Dev.IO_Control        := null;
      return VFS.Device.Register (Dev);
   end Init;

   function Read
      (Data   : System.Address;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Offset);
      To_Write : String (1 .. Natural (Count)) with Address => Desto;
   begin
      --  Set the info for the buffer.
      Buffer_Length := 0;

      --  Set that we are reading, and wait until done.
      Is_Reading := True;
      while Buffer_Length /= Natural (Count) loop null; end loop;

      --  Copy back.
      Is_Reading := False;
      To_Write := Key_Buffer (1 .. Natural (Count));
      return Count;
   end Read;

   procedure Keyboard_Handler is
      Input : constant Unsigned_8 := Arch.Wrappers.Port_In (16#60#);
      C     : Character;
   begin
      if not Is_Reading then
         goto EOI;
      end if;

      if Input = 16#E0# then
         Has_Extra_Code := True;
         goto EOI;
      end if;

      if Has_Extra_Code then
         case Input is
            when Ctrl_Press   => Is_Ctrl_Active := True;  goto EOI;
            when Ctrl_Release => Is_Ctrl_Active := False; goto EOI;
            when others       => goto EOI;
         end case;
      end if;

      case Input is
         when Left_Shift_Press    => Is_Shift_Active := True;  goto EOI;
         when Left_Shift_Release  => Is_Shift_Active := False; goto EOI;
         when Right_Shift_Press   => Is_Shift_Active := True;  goto EOI;
         when Right_Shift_Release => Is_Shift_Active := False; goto EOI;
         when Ctrl_Press          => Is_Ctrl_Active  := True;  goto EOI;
         when Ctrl_Release        => Is_Ctrl_Active  := False; goto EOI;
         when Capslock_Press      =>
            Is_Capslock_Active := not Is_Capslock_Active; goto EOI;
         when others => null;
      end case;

      if Input < Max_Code then
         if Is_Ctrl_Active then
            --  TODO: Caret notation.
            C := Capslock_Mapping (Integer (Input));
         elsif Is_Capslock_Active and Is_Shift_Active then
            C := Shift_Capslock_Mapping (Integer (Input));
         elsif not Is_Capslock_Active and Is_Shift_Active then
            C := Shift_Mapping (Integer (Input));
         elsif Is_Capslock_Active and not Is_Shift_Active then
            C := Capslock_Mapping (Integer (Input));
         else
            C := Normal_Mapping (Integer (Input));
         end if;

         Arch.Stivale2.Print_Terminal (C);
         Buffer_Length := Buffer_Length + 1;
         Key_Buffer (Buffer_Length) := C;
      end if;

   <<EOI>>
      Arch.APIC.LAPIC_EOI;
   end Keyboard_Handler;
end Devices.PS2Keyboard;
