--  devices-ps2.adb: PS2 keyboard and mouse driver.
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

with Arch.Clocks;
with Arch.IDT;
with Arch.APIC;
with Arch.CPU;
with Arch.Snippets;
with Scheduler;
with Synchronization;
with Ada.Unchecked_Conversion;
with Messages;

package body Devices.PS2 is
   --  Globals to communicate with the keyboard interrupt routine.
   Kb_Data_Mutex : aliased Synchronization.Binary_Semaphore
      := Synchronization.Unlocked_Semaphore;
   Kb_Has_Data   : Boolean := False with Volatile;
   Kb_Scan_Count : Natural := 0     with Volatile;
   Kb_Scancodes  : array (1 .. 10) of Unsigned_8 := [others => 0]
      with Volatile;

   --  Type used by userland to fetch mouse packets.
   type Mouse_Data is record
      X_Variation     : Integer;
      Y_Variation     : Integer;
      Z_Variation     : Integer;
      Is_Left_Click   : Boolean;
      Is_Right_Click  : Boolean;
      Is_Middle_Click : Boolean;
      Is_4th_Button   : Boolean;
      Is_5th_Button   : Boolean;
   end record;

   --  Types used by mouse internals.
   type Signed_8 is range -128 .. 127 with Size => 8;
   function To_Signed is new Ada.Unchecked_Conversion (Unsigned_8, Signed_8);

   --  Globals to communicate with the mouse interrupt routine.
   Ms_Data_Mutex : aliased Synchronization.Binary_Semaphore
      := Synchronization.Unlocked_Semaphore;
   Ms_Has_Returned      : Boolean    with Volatile;
   Ms_Current_Flags     : Unsigned_8;
   Ms_Has_4th_Packet    : Boolean := False;
   Ms_Has_Extra_Buttons : Boolean := False;
   Ms_Current_Cycle     : Integer range 1 .. 4 := 1;
   Ms_Return_Data       : Mouse_Data :=
      (X_Variation => 0,
       Y_Variation => 0,
       Z_Variation => 0,
       others      => False) with Volatile;

   function Init return Boolean is
      Index        : Arch.IDT.IRQ_Index;
      Data, Unused : Unsigned_8;
      Success      : Boolean;
      Spin_Counter : Integer := 0;
   begin
      --  Take the chance for initializing the PS2 controller.
      --  Disable primary and secondary PS/2 ports
      Arch.Snippets.Port_Out (16#64#, 16#AD#);
      Arch.Snippets.Port_Out (16#64#, 16#A7#);

      --  Read from port 0x60 to flush the PS/2 controller buffer with a simple
      --  timeout for checking dead devices and non-present controllers.
      loop
         exit when (Arch.Snippets.Port_In (16#64#) and 1) = 0;
         Data := Arch.Snippets.Port_In (16#60#);
         if Spin_Counter = 10 then
            return True;
         end if;
         Arch.Clocks.Busy_Monotonic_Sleep (1000); --  Purely vibes based.
         Spin_Counter := Spin_Counter + 1;
      end loop;

      --  Set the interrupt up, which is always the 34 (we are 1 based).
      Arch.IDT.Load_ISR (Keyboard_Handler'Address, Index, Success);
      if not Success then
         return False;
      end if;
      if not Arch.APIC.IOAPIC_Set_Redirect
         (Arch.CPU.Core_Locals (1).LAPIC_ID, 34, Index, True)
      then
         return False;
      end if;

      --  Set the interrupt up, which is always the 45 (we are 1 based).
      Arch.IDT.Load_ISR (Mouse_Handler'Address, Index, Success);
      if not Success then
         return False;
      end if;
      if not Arch.APIC.IOAPIC_Set_Redirect
         (Arch.CPU.Core_Locals (1).LAPIC_ID, 45, Index, True)
      then
         return False;
      end if;

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
      -------------------------------------------------------------------------
      --  Init the mouse.
      Write_PS2 (16#64#, 16#A8#);
      Write_PS2 (16#64#, 16#20#);
      Data   := Read_PS2;
      Unused := Read_PS2;
      Data   := Data or  Shift_Left (1, 1);
      Data   := Data and (not Shift_Left (1, 5));
      Write_PS2 (16#64#, 16#60#);
      Write_PS2 (16#60#, Data);
      Unused := Read_PS2;
      Mouse_Write (16#F6#);
      Unused := Read_PS2;
      Mouse_Write (16#F4#);
      Unused := Read_PS2;

      --  Try to enable scrollwheel and 4th/5th buttons.
      Set_Sample_Rate (200);
      Set_Sample_Rate (100);
      Set_Sample_Rate (80);
      Data := Identify_Mouse;
      if Data = 3 then
         Ms_Has_4th_Packet := True;
         Messages.Put_Line ("Scrollwheel support enabled for mouse");
         Set_Sample_Rate (200);
         Set_Sample_Rate (200);
         Set_Sample_Rate (80);
         Data := Identify_Mouse;
         if Data = 4 then
            Ms_Has_Extra_Buttons := True;
            Messages.Put_Line ("extra button support enabled for mouse");
         end if;
      end if;

      --  Restore sample rate to the highest (POWER!!!).
      Set_Sample_Rate (200);
      -------------------------------------------------------------------------
      Register
         ((Data        => System.Null_Address,
           ID          => [others => 0],
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Sync        => null,
           Sync_Range  => null,
           Read        => Kb_Read'Access,
           Write       => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => Kb_Poll'Access,
           Remove      => null), "ps2keyboard", Success);
      if not Success then
         return False;
      end if;
      Register
         ((Data        => System.Null_Address,
           ID          => [others => 0],
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Sync        => null,
           Sync_Range  => null,
           Read        => Ms_Read'Access,
           Write       => null,
           IO_Control  => Ms_IO_Control'Access,
           Mmap        => null,
           Poll        => Ms_Poll'Access,
           Remove      => null), "ps2mouse", Success);
      return Success;
   exception
      when Constraint_Error =>
         return False;
   end Init;
   ----------------------------------------------------------------------------
   procedure Kb_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset);
      Temp : Boolean;
   begin
      if Is_Blocking then
         loop
            Synchronization.Seize (Kb_Data_Mutex);
            Temp := Kb_Has_Data;
            exit when Temp;
            Synchronization.Release (Kb_Data_Mutex);
            Scheduler.Yield_If_Able;
         end loop;
      else
         Synchronization.Seize (Kb_Data_Mutex);
         Temp := Kb_Has_Data;
      end if;

      if Temp and Data'Length /= 0 then
         if Data'Length > Kb_Scan_Count then
            Ret_Count := Kb_Scan_Count;
         else
            Ret_Count := Data'Length;
         end if;

         Data (Data'First .. Data'First + Ret_Count - 1) :=
            Operation_Data (Kb_Scancodes (1 .. Ret_Count));
         if Ret_Count < Kb_Scan_Count then
            Kb_Scancodes (1 .. Ret_Count) :=
               Kb_Scancodes (Ret_Count + 1 .. Ret_Count * 2);
            Kb_Scan_Count := Kb_Scan_Count - Ret_Count;
         else
            Kb_Scan_Count := 0;
            Kb_Has_Data   := False;
         end if;

         Success := Dev_Success;
      else
         Success   := Dev_IO_Failure;
         Ret_Count := 0;
      end if;

      Synchronization.Release (Kb_Data_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Kb_Data_Mutex);
         Data      := [others => 0];
         Ret_Count := 0;
         Success   := Dev_IO_Failure;
   end Kb_Read;

   procedure Kb_Poll
      (Data      : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
      pragma Unreferenced (Data);
   begin
      Synchronization.Seize (Kb_Data_Mutex);
      Can_Read  := Kb_Has_Data;
      Can_Write := False;
      Is_Error  := False;
      Synchronization.Release (Kb_Data_Mutex);
   end Kb_Poll;
   ----------------------------------------------------------------------------
   procedure Ms_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key, Offset);
      Temp  : Boolean;
   begin
      if Is_Blocking then
         loop
            Synchronization.Seize (Ms_Data_Mutex);
            Temp := Ms_Has_Returned;
            exit when Temp;
            Synchronization.Release (Ms_Data_Mutex);
            Scheduler.Yield_If_Able;
         end loop;
      else
         Synchronization.Seize (Ms_Data_Mutex);
         Temp := Ms_Has_Returned;
      end if;

      if Temp then
         declare
            Data2 : Mouse_Data
               with Import, Address => Data (Data'First)'Address;
         begin
            Data2           := Ms_Return_Data;
            Ret_Count       := Ms_Return_Data'Size / 8;
            Success         := Dev_Success;
            Ms_Has_Returned := False;
         end;
      else
         Ret_Count := 0;
         Success   := Dev_IO_Failure;
      end if;

      Synchronization.Release (Ms_Data_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Ms_Data_Mutex);
         Ret_Count := 0;
         Success   := Dev_IO_Failure;
   end Ms_Read;

   procedure Ms_IO_Control
      (Key       : System.Address;
       Request   : Unsigned_64;
       Argument  : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean)
   is
      pragma Warnings (Off, "handler can never be entered", Reason => "Bug");
      pragma Unreferenced (Key);

      function To_Integer is
         new Ada.Unchecked_Conversion (System.Address, Unsigned_64);

      IOCTL_Enable_2_1_Scaling : constant := 1;
      IOCTL_Enable_1_1_Scaling : constant := 2;
      IOCTL_Set_Resolution     : constant := 3;
      IOCTL_Set_Sample_Rate    : constant := 4;

      Argument_Integer : constant Unsigned_64 := To_Integer (Argument);
      Unused : Unsigned_8;
   begin
      Success   := True;
      Has_Extra := False;
      Extra     := 0;

      Synchronization.Seize (Ms_Data_Mutex);

      case Request is
         when IOCTL_Enable_2_1_Scaling =>
            Mouse_Write (16#E7#);
            Unused := Read_PS2;
         when IOCTL_Enable_1_1_Scaling =>
            Mouse_Write (16#E6#);
            Unused := Read_PS2;
         when IOCTL_Set_Resolution =>
            if Argument_Integer <= 3 then
               Mouse_Write (16#E8#);
               Unused := Read_PS2;
               Mouse_Write (Unsigned_8 (Argument_Integer));
               Unused := Read_PS2;
            else
               Success := False;
               goto Cleanup;
            end if;
         when IOCTL_Set_Sample_Rate =>
            if Argument_Integer <= 200 then
               Mouse_Write (16#F3#);
               Unused := Read_PS2;
               Mouse_Write (Unsigned_8 (Argument_Integer));
               Unused := Read_PS2;
            else
               Success := False;
               goto Cleanup;
            end if;
         when others =>
            Success := False;
            goto Cleanup;
      end case;

      Ms_Current_Cycle := 1;

   <<Cleanup>>
      Synchronization.Release (Ms_Data_Mutex);
   exception
      when Constraint_Error =>
         Synchronization.Release (Ms_Data_Mutex);
         Success   := False;
         Has_Extra := False;
         Extra     := 0;
   end Ms_IO_Control;

   procedure Ms_Poll
      (Data      : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
      pragma Unreferenced (Data);
   begin
      Synchronization.Seize (Ms_Data_Mutex);
      Can_Read  := Ms_Has_Returned;
      Can_Write := False;
      Is_Error  := False;
      Synchronization.Release (Ms_Data_Mutex);
   end Ms_Poll;
   ----------------------------------------------------------------------------
   procedure Set_Sample_Rate (Rate : Unsigned_8) is
      Discard : Unsigned_8;
   begin
      Write_PS2 (16#64#, 16#D4#);
      Write_PS2 (16#60#, 16#F3#);
      Discard := Read_PS2;
      Write_PS2 (16#64#, 16#D4#);
      Write_PS2 (16#60#, Rate);
      Discard := Read_PS2;
   end Set_Sample_Rate;

   function Identify_Mouse return Unsigned_8 is
      Discard : Unsigned_8;
      Result  : Unsigned_8;
   begin
      Write_PS2 (16#64#, 16#D4#);
      Write_PS2 (16#60#, 16#F5#);
      Discard := Read_PS2;
      Write_PS2 (16#64#, 16#D4#);
      Write_PS2 (16#60#, 16#F2#);
      Discard := Read_PS2;
      Result  := Read_PS2;
      Write_PS2 (16#64#, 16#D4#);
      Write_PS2 (16#60#, 16#F4#);
      return Result;
   end Identify_Mouse;
   ----------------------------------------------------------------------------
   function Read_PS2 return Unsigned_8 is
   begin
      for I in 1 .. 100_000 loop
         exit when (Arch.Snippets.Port_In (16#64#) and 1) /= 0;
         Scheduler.Yield_If_Able;
      end loop;
      return Arch.Snippets.Port_In (16#60#);
   end Read_PS2;

   procedure Write_PS2 (Port : Unsigned_16; Value : Unsigned_8) is
   begin
      for I in 1 .. 100_000 loop
         exit when (Arch.Snippets.Port_In (16#64#) and 2) = 0;
         Scheduler.Yield_If_Able;
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

   procedure Mouse_Write (Data : Unsigned_8) is
   begin
      Write_PS2 (16#64#, 16#D4#);
      Write_PS2 (16#60#, Data);
   end Mouse_Write;
   ----------------------------------------------------------------------------
   procedure Keyboard_Handler is
      Input : constant Unsigned_8 := Arch.Snippets.Port_In (16#60#);
   begin
      Synchronization.Seize (Kb_Data_Mutex);

      if Kb_Scan_Count < Kb_Scancodes'Length then
         if Kb_Scan_Count > 1 and then
            Kb_Scancodes (Kb_Scan_Count - 1) = 16#E0#
         then
            if Kb_Scan_Count + 1 = Kb_Scancodes'Length then
               goto Cleanup;
            end if;

            Kb_Scancodes (Kb_Scancodes'First + Kb_Scan_Count) := Input;
            Kb_Has_Data := True;
         else
            Kb_Scancodes (Kb_Scancodes'First + Kb_Scan_Count) := Input;
            Kb_Has_Data := Input /= 16#E0#;
         end if;

         Kb_Scan_Count := Kb_Scan_Count + 1;
      end if;

   <<Cleanup>>
      Synchronization.Release (Kb_Data_Mutex);
      Arch.APIC.LAPIC_EOI;
   exception
      when Constraint_Error =>
         Synchronization.Release (Kb_Data_Mutex);
         Arch.APIC.LAPIC_EOI;
   end Keyboard_Handler;

   procedure Mouse_Handler is
      Data : Unsigned_8;
   begin
      Synchronization.Seize (Ms_Data_Mutex);
      case Ms_Current_Cycle is
         when 1 =>
            Data                := Arch.Snippets.Port_In (16#60#);
            Ms_Current_Flags := Data;

            if (Data and Shift_Left (1, 3)) /= 0 and
               (Data and Shift_Left (1, 6))  = 0 and
               (Data and Shift_Left (1, 7))  = 0
            then
               Ms_Current_Cycle := 2;
            end if;

            Ms_Return_Data.Is_Left_Click := (Data and Shift_Left (1, 0)) /= 0;
            Ms_Return_Data.Is_Right_Click := (Data and Shift_Left (1, 1)) /= 0;
            Ms_Return_Data.Is_Middle_Click := (Data and Shift_Left (1, 2))
               /= 0;
         when 2 =>
            Data             := Arch.Snippets.Port_In (16#60#);
            Ms_Current_Cycle := 3;
            if (Ms_Current_Flags and Shift_Left (1, 4)) /= 0 then
               Ms_Return_Data.X_Variation := Integer (To_Signed (Data));
            else
               Ms_Return_Data.X_Variation := Integer (Data);
            end if;
         when 3 =>
            Data := Arch.Snippets.Port_In (16#60#);

            if (Ms_Current_Flags and Shift_Left (1, 5)) /= 0 then
               Ms_Return_Data.Y_Variation := -Integer (To_Signed (Data));
            else
               Ms_Return_Data.Y_Variation := -Integer (Data);
            end if;

            if Ms_Has_4th_Packet then
               Ms_Current_Cycle := 4;
            else
               Ms_Current_Cycle := 1;
               Ms_Has_Returned  := True;
            end if;
         when 4 =>
            Data := Arch.Snippets.Port_In (16#60#);

            --  GNAT (maybe Ada?) doesnt support 4 bit integers, so we have to
            --  improvise to take the last 4 bits of the z variation.
            if (Data and 2#1000#) /= 0 then
               Ms_Return_Data.Z_Variation := -(Integer (Data and 2#111#) + 1);
            else
               Ms_Return_Data.Z_Variation := Integer (Data and 2#111#);
            end if;

            if Ms_Has_Extra_Buttons then
               Ms_Return_Data.Is_4th_Button :=
                  (Data and Shift_Left (1, 4)) /= 0;
               Ms_Return_Data.Is_5th_Button :=
                  (Data and Shift_Left (1, 5)) /= 0;
            end if;

            Ms_Current_Cycle := 1;
            Ms_Has_Returned := True;
      end case;

      Synchronization.Release (Ms_Data_Mutex);

      Arch.APIC.LAPIC_EOI;
   exception
      when Constraint_Error =>
         null;
   end Mouse_Handler;
end Devices.PS2;
