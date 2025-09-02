--  arch-clocks.adb: Architectural clock sources.
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

with Panic;
with Messages;
with Arch.ACPI;
with System.Machine_Code;

package body Arch.Clocks with
   Refined_State =>
      (RT_Clock_State => (null),
       Monotonic_Clock_State =>
         (Is_Initialized, TSC_Tick_Resolution, TSC_Ticks_Per_Res))
is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   Is_Initialized : Boolean := False;

   --  For monotonic, we use the TSC.
   TSC_Tick_Resolution : Unsigned_64 := 100_000;
   TSC_Ticks_Per_Res : Unsigned_64 := Nanoseconds_In_Second;

   procedure Initialize_Sources is
      ACPI_Address : Arch.ACPI.Table_Record;
   begin
      if not ACPI.Is_Supported then
         Panic.Hard_Panic ("Could not use ACPI to find out frequency");
      end if;

      ACPI.FindTable (ACPI.RHCT_Signature, ACPI_Address);
      if ACPI_Address.Virt_Addr = 0 then
         return;
      end if;

      declare
         Table : Arch.ACPI.RHCT
            with Import, Address => To_Address (ACPI_Address.Virt_Addr);
      begin
         --  XXX: Cap it to 10MHz as a safe minimum bet, replace this in the
         --  future with a detection algo.
         TSC_Ticks_Per_Res   := Table.Time_Base_Freq / 10_000_000;
         TSC_Tick_Resolution := 100;
         Arch.ACPI.Unref_Table (ACPI_Address);
      end;

      Messages.Put_Line
         ("Monotonic resolution fixed at 0x" &
          TSC_Ticks_Per_Res'Image    & " ticks / 0x" &
           TSC_Tick_Resolution'Image & " ns");

      Is_Initialized := True;
   end Initialize_Sources;

   procedure Get_Monotonic_Resolution (Stamp : out Time.Timestamp) is
   begin
      Stamp := (0, TSC_Tick_Resolution);
   end Get_Monotonic_Resolution;

   procedure Get_Monotonic_Time (Stamp : out Time.Timestamp) is
      pragma SPARK_Mode (Off); --  ASM is not SPARK-friendly.
      Cnt : Unsigned_64;
   begin
      if Is_Initialized then
         System.Machine_Code.Asm
            ("rdtime %0",
             Outputs  => Unsigned_64'Asm_Output ("=r", Cnt),
             Volatile => True);

         Stamp := To_Stamp ((Cnt / TSC_Ticks_Per_Res) * TSC_Tick_Resolution);
      else
         Stamp := (0, 0);
      end if;
   end Get_Monotonic_Time;

   procedure Busy_Monotonic_Sleep (Nanoseconds : Unsigned_64) is
      Curr, Tgt : Time.Timestamp;
   begin
      if Is_Initialized then
         Get_Monotonic_Time (Tgt);
         Tgt := Tgt + (0, Nanoseconds);
         loop
            Get_Monotonic_Time (Curr);
            exit when Curr >= Tgt;
         end loop;
      end if;
   end Busy_Monotonic_Sleep;

   procedure Get_Real_Time_Resolution (Stamp : out Time.Timestamp) is
   begin
      Stamp := (0, TSC_Tick_Resolution);
   end Get_Real_Time_Resolution;

   procedure Get_Real_Time (Stamp : out Time.Timestamp) is
   begin
      Stamp := (0, 0);
   end Get_Real_Time;

   procedure Set_Real_Time (Stamp : Time.Timestamp) is
      pragma Unreferenced (Stamp);
   begin
      null;
   end Set_Real_Time;

   function Ticks_Per_Microsecond return Unsigned_64 is
   begin
      return TSC_Ticks_Per_Res * (1000 / TSC_Tick_Resolution);
   end Ticks_Per_Microsecond;
end Arch.Clocks;
