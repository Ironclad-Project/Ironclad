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
with Arch.Snippets;

package body Devices.PS2Keyboard with SPARK_Mode => Off is
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
      Data      : Unsigned_8;
      Device    : Resource;
   begin
      --  Set the interrupt up, which is always the 34 (we are 1 based).
      if not Arch.IDT.Load_ISR (Keyboard_Handler'Address, Index) then
         return False;
      end if;
      if not Arch.APIC.IOAPIC_Set_Redirect (BSP_LAPIC, 34, Index, True) then
         return False;
      end if;

      --  Take the chance for initializing the PS2 controller.
      --  Disable primary and secondary PS/2 ports
      Arch.Snippets.Port_Out (16#64#, 16#AD#);
      Arch.Snippets.Port_Out (16#64#, 16#A7#);

      --  Read from port 0x60 to flush the PS/2 controller buffer.
      while (Arch.Snippets.Port_In (16#64#) and 1) /= 0 loop
         Data := Arch.Snippets.Port_In (16#60#);
      end loop;

      --  Enable keyboard interrupt and keyboard scancode translation.
      Data := Read_PS2_Config;
      Data := Data or Shift_Left (1, 0) or Shift_Left (1, 6);

      --  Enable mouse interrupt if any
      if (Data and Shift_Left (1, 5)) /= 0 then
         Data := Data or Shift_Left (1, 1);
      end if;

      --  Write changes, enable keyboard port, and enable mouse port if any.
      Write_PS2_Config (Data);
      Write_PS2 (16#64#, 16#AE#);
      if (Data and Shift_Left (1, 5)) /= 0 then
         Write_PS2 (16#64#, 16#A8#);
      end if;

      Device := (
         Data              => System.Null_Address,
         Mutex             => <>,
         Is_Block          => False,
         Block_Size        => 4096,
         Block_Count       => 0,
         Unique_Identifier => 0,
         Sync              => null,
         Read              => Read'Access,
         Write             => null,
         IO_Control        => null,
         Mmap              => null,
         Munmap            => null
      );

      return Register (Device, "ps2keyboard");
   end Init;

   function Read_PS2 return Unsigned_8 is
   begin
      while (Arch.Snippets.Port_In (16#64#) and 1) = 0 loop
         Arch.Snippets.Pause;
      end loop;
      return Arch.Snippets.Port_In (16#60#);
   end Read_PS2;

   procedure Write_PS2 (Port : Unsigned_16; Value : Unsigned_8) is
   begin
      while (Arch.Snippets.Port_In (16#64#) and 2) /= 0 loop
         Arch.Snippets.Pause;
      end loop;
      Arch.Snippets.Port_Out (Port, Value);
   end Write_PS2;

   function Read_PS2_Config return Unsigned_8 is
   begin
      Write_PS2 (16#64#, 16#20#);
      return Read_PS2;
   end Read_PS2_Config;

   procedure Write_PS2_Config (Value : Unsigned_8) is
   begin
      Write_PS2 (16#64#, 16#60#);
      Write_PS2 (16#60#, Value);
   end Write_PS2_Config;

   function Read
      (Data   : Resource_Acc;
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
      while Buffer_Length < Natural (Count) loop
         Arch.Snippets.Wait_For_Interrupt;
      end loop;
      Is_Reading := False;

      --  Copy back.
      To_Write := Key_Buffer (1 .. Natural (Count));
      return Count;
   end Read;

   procedure Keyboard_Handler is
      Input : constant Integer := Integer (Arch.Snippets.Port_In (16#60#));
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
         Has_Extra_Code := False;
         case Input is
            when Ctrl_Press   => Is_Ctrl_Active := True;
            when Ctrl_Release => Is_Ctrl_Active := False;
            when others       => null;
         end case;
         goto EOI;
      end if;

      case Input is
         when Left_Shift_Press    => Is_Shift_Active := True;
         when Left_Shift_Release  => Is_Shift_Active := False;
         when Right_Shift_Press   => Is_Shift_Active := True;
         when Right_Shift_Release => Is_Shift_Active := False;
         when Ctrl_Press          => Is_Ctrl_Active  := True;
         when Ctrl_Release        => Is_Ctrl_Active  := False;
         when Capslock_Press => Is_Capslock_Active := not Is_Capslock_Active;
         when others =>
            if Input >= Normal_Mapping'First and
               Input <= Normal_Mapping'Last
            then
               if Is_Ctrl_Active then
                  --  TODO: Caret notation.
                  C := Capslock_Mapping (Input);
               elsif Is_Capslock_Active and Is_Shift_Active then
                  C := Shift_Capslock_Mapping (Input);
               elsif not Is_Capslock_Active and Is_Shift_Active then
                  C := Shift_Mapping (Input);
               elsif Is_Capslock_Active and not Is_Shift_Active then
                  C := Capslock_Mapping (Input);
               else
                  C := Normal_Mapping (Input);
               end if;
               Buffer_Length := Buffer_Length + 1;
               Key_Buffer (Buffer_Length) := C;
            end if;
      end case;

   <<EOI>>
      Arch.APIC.LAPIC_EOI;
   end Keyboard_Handler;
end Devices.PS2Keyboard;
