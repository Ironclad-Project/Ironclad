--  devices-ps2mouse.adb: PS2 mouse driver.
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

with Arch;
with Arch.IDT;
with Arch.APIC;
with Arch.CPU;
with Ada.Unchecked_Conversion;
with Arch.Snippets;

package body Devices.PS2Mouse with SPARK_Mode => Off is
   --  For return.
   type Mouse_Data is record
      X_Variation    : Integer;
      Y_Variation    : Integer;
      Is_Left_Click  : Boolean;
      Is_Right_Click : Boolean;
   end record;
   Has_Returned : Boolean    with Volatile;
   Return_Data  : Mouse_Data with Volatile;

   --  Data used for mouse operation.
   type Signed_8 is range -128 .. 127 with Size => 8;
   function To_Signed is
      new Ada.Unchecked_Conversion (Unsigned_8, Signed_8);
   type PS2_Mouse_Data is record
      X_Variation : Unsigned_8;
      Y_Variation : Unsigned_8;
      Flags       : Unsigned_8;
   end record;

   Current_Cycle_Data  : PS2_Mouse_Data;
   Current_Mouse_Cycle : Integer range 1 .. 3 := 1;

   function Init return Boolean is
      BSP_LAPIC    : constant Unsigned_32 := Arch.CPU.Core_Locals (1).LAPIC_ID;
      Index        : Arch.IDT.IRQ_Index;
      Data, Unused : Unsigned_8;
      Device       : Resource;
      Success      : Boolean;
   begin
      --  Set the interrupt up, which is always the 45 (we are 1 based).
      if not Arch.IDT.Load_ISR (Mouse_Handler'Address, Index) then
         return False;
      end if;
      if not Arch.APIC.IOAPIC_Set_Redirect (BSP_LAPIC, 45, Index, True) then
         return False;
      end if;

      --  Init the mouse.
      Mouse_Wait_Write;
      Arch.Snippets.Port_Out (16#64#, 16#A8#);
      Mouse_Wait_Write;
      Arch.Snippets.Port_Out (16#64#, 16#20#);
      Data   := Mouse_Read;
      Unused := Mouse_Read;
      Data   := Data or  Shift_Left (1, 1);
      Data   := Data and (not Shift_Left (1, 5));
      Mouse_Wait_Write;
      Arch.Snippets.Port_Out (16#64#, 16#60#);
      Mouse_Wait_Write;
      Arch.Snippets.Port_Out (16#60#, Data);
      Unused := Mouse_Read;
      Mouse_Write (16#F6#);
      Unused := Mouse_Read;
      Mouse_Write (16#F4#);
      Unused := Mouse_Read;

      Device := (
         Data        => System.Null_Address,
         Mutex       => <>,
         Is_Block    => False,
         Block_Size  => 4096,
         Block_Count => 0,
         Sync        => null,
         Read        => Read'Access,
         Write       => null,
         IO_Control  => IO_Control'Access,
         Mmap        => null,
         Munmap      => null
      );

      Register (Device, "ps2mouse", Success);
      return Success;
   end Init;

   function Read
      (Data   : Resource_Acc;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Count);
      Data2 : Mouse_Data with Address => Desto;
   begin
      Has_Returned := False;
      while not Has_Returned loop
         Arch.Snippets.Wait_For_Interrupt;
      end loop;

      Data2 := Return_Data;
      return Return_Data'Size / 8;
   end Read;

   function IO_Control
      (Data     : Resource_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      pragma Unreferenced (Data);

      function To_Integer is
         new Ada.Unchecked_Conversion (System.Address, Unsigned_64);

      IOCTL_Enable_2_1_Scaling : constant := 1;
      IOCTL_Enable_1_1_Scaling : constant := 2;
      IOCTL_Set_Resolution     : constant := 3;
      IOCTL_Set_Sample_Rate    : constant := 4;

      Argument_Integer : constant Unsigned_64 := To_Integer (Argument);
      Unused : Unsigned_8;
   begin
      case Request is
         when IOCTL_Enable_2_1_Scaling =>
            Mouse_Write (16#E7#);
            Unused := Mouse_Read;
         when IOCTL_Enable_1_1_Scaling =>
            Mouse_Write (16#E6#);
            Unused := Mouse_Read;
         when IOCTL_Set_Resolution =>
            if Argument_Integer <= 3 then
               Mouse_Write (16#E8#);
               Unused := Mouse_Read;
               Mouse_Write (Unsigned_8 (Argument_Integer));
               Unused := Mouse_Read;
            else
               return False;
            end if;
         when IOCTL_Set_Sample_Rate =>
            if Argument_Integer <= 200 then
               Mouse_Write (16#F3#);
               Unused := Mouse_Read;
               Mouse_Write (Unsigned_8 (Argument_Integer));
               Unused := Mouse_Read;
            else
               return False;
            end if;
         when others =>
            return False;
      end case;

      Current_Mouse_Cycle := 1;
      return True;
   end IO_Control;

   procedure Mouse_Handler is
   begin
      case Current_Mouse_Cycle is
         when 1 =>
            Current_Cycle_Data.Flags := Arch.Snippets.Port_In (16#60#);
            if (Current_Cycle_Data.Flags and Shift_Left (1, 3)) /= 0 and
               (Current_Cycle_Data.Flags and Shift_Left (1, 6))  = 0 and
               (Current_Cycle_Data.Flags and Shift_Left (1, 7))  = 0
            then
               Current_Mouse_Cycle := 2;
            end if;
         when 2 =>
            Current_Cycle_Data.X_Variation := Arch.Snippets.Port_In (16#60#);
            Current_Mouse_Cycle            := 3;
         when 3 =>
            Current_Cycle_Data.Y_Variation := Arch.Snippets.Port_In (16#60#);
            Current_Mouse_Cycle            := 1;

            --  Apply the flags and convert format.
            if (Current_Cycle_Data.Flags and Shift_Left (1, 0)) /= 0 then
               Return_Data.Is_Left_Click := True;
            else
               Return_Data.Is_Left_Click := False;
            end if;
            if (Current_Cycle_Data.Flags and Shift_Left (1, 1)) /= 0 then
               Return_Data.Is_Right_Click := True;
            else
               Return_Data.Is_Right_Click := False;
            end if;
            if (Current_Cycle_Data.Flags and Shift_Left (1, 4)) /= 0 then
               Return_Data.X_Variation :=
                  Integer (To_Signed (Current_Cycle_Data.X_Variation));
            else
               Return_Data.X_Variation :=
                  Integer (Current_Cycle_Data.X_Variation);
            end if;
            if (Current_Cycle_Data.Flags and Shift_Left (1, 5)) /= 0 then
               Return_Data.Y_Variation :=
                  -Integer (To_Signed (Current_Cycle_Data.Y_Variation));
            else
               Return_Data.Y_Variation :=
                  -Integer (Current_Cycle_Data.Y_Variation);
            end if;
            Has_Returned := True;
      end case;

      Arch.APIC.LAPIC_EOI;
   end Mouse_Handler;

   procedure Mouse_Wait_Read is
   begin
      for I in 1 .. 100_000 loop
         exit when (Arch.Snippets.Port_In (16#64#) and Shift_Left (1, 0)) /= 0;
         Arch.Snippets.Pause;
      end loop;
   end Mouse_Wait_Read;

   procedure Mouse_Wait_Write is
   begin
      for I in 1 .. 100_000 loop
         exit when (Arch.Snippets.Port_In (16#64#) and Shift_Left (1, 1)) = 0;
         Arch.Snippets.Pause;
      end loop;
   end Mouse_Wait_Write;

   function Mouse_Read return Unsigned_8 is
   begin
      Mouse_Wait_Read;
      return Arch.Snippets.Port_In (16#60#);
   end Mouse_Read;

   procedure Mouse_Write (Data : Unsigned_8) is
   begin
      Mouse_Wait_Write;
      Arch.Snippets.Port_Out (16#64#, 16#D4#);
      Mouse_Wait_Write;
      Arch.Snippets.Port_Out (16#60#, Data);
   end Mouse_Write;
end Devices.PS2Mouse;
