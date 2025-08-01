--  arch-acpi_pm_timer.adb: ACPI power management timer support.
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

with Memory; use Memory;
with Arch.ACPI;
with Arch.MMU;
with Arch.Snippets;

package body Arch.ACPI_PM_Timer with SPARK_Mode => Off is
   --  Forced by spec. In ns, Resolution = 1 / Freq.
   Timer_Resolution : constant := 279;

   Use_IO_Ports : Boolean;
   IO_Addr      : Integer_Address;

   function Init return Boolean is
      pragma Warnings (Off, "handler can never be entered", Reason => "Bug");
      ACPI_Address : ACPI.Table_Record;
   begin
      if not ACPI.Is_Supported then
         return False;
      end if;

      ACPI.FindTable (ACPI.FADT_Signature, ACPI_Address);
      if ACPI_Address.Virt_Addr = Null_Address then
         return False;
      end if;

      declare
         Success : Boolean;
         FADT    : ACPI.FADT
            with Import, Address => To_Address (ACPI_Address.Virt_Addr);
      begin
         if ACPI.Get_Revision >= 2 and then
            FADT.X_PMTImerBlock.Address /= 0
         then
            Use_IO_Ports := FADT.X_PMTImerBlock.Address_Space = 1;
            IO_Addr      := Integer_Address (FADT.X_PMTImerBlock.Address);
            if not Use_IO_Ports then
               MMU.Map_Range
                  (Map            => MMU.Kernel_Table,
                   Physical_Start => To_Address (IO_Addr),
                   Virtual_Start  => To_Address (IO_Addr + Memory_Offset),
                   Length         => MMU.Page_Size,
                   Permissions    =>
                    (Is_User_Accessible => False,
                     Can_Read          => True,
                     Can_Write         => True,
                     Can_Execute       => False,
                     Is_Global         => True),
                   Success        => Success,
                   Caching        => MMU.Uncacheable);
               if not Success then
                  return False;
               end if;

               IO_Addr := IO_Addr + Memory_Offset;
            end if;
         elsif FADT.PM_TMR_BLK /= 0 then
            Use_IO_Ports := True;
            IO_Addr      := Integer_Address (FADT.PM_TMR_BLK);
         else
            return False;
         end if;
      end;

      ACPI.Unref_Table (ACPI_Address);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Init;

   procedure Get_Resolution (Resolution : out Unsigned_64) is
   begin
      Resolution := Timer_Resolution;
   end Get_Resolution;

   procedure NSleep (Nanoseconds : Unsigned_64) is
      pragma Warnings (Off, "handler can never be entered", Reason => "Bug");
      Target  : Unsigned_64;
      Current : Unsigned_64 with Volatile;
   begin
      Get_Counter (Current);
      Target := Current + (Nanoseconds / Timer_Resolution);
      loop
         Get_Counter (Current);
         exit when Current >= Target;
         Snippets.Pause;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end NSleep;
   ----------------------------------------------------------------------------
   procedure Get_Counter (Counter : out Unsigned_64) is
   begin
      if Use_IO_Ports then
         Counter := Unsigned_64 (Snippets.Port_In32 (Unsigned_16 (IO_Addr)));
      else
         declare
            Orig_Counter : Unsigned_32
               with Import, Volatile, Address => To_Address (IO_Addr);
         begin
            Counter := Unsigned_64 (Orig_Counter);
         end;
      end if;
   exception
      when Constraint_Error =>
         Counter := 0;
   end Get_Counter;
end Arch.ACPI_PM_Timer;
