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

with Arch.Snippets;
with Arch.RTC;
with Panic;
with Arch.HPET;
with Arch.ACPI_PM_Timer;
with Messages;

package body Arch.Clocks with
   Refined_State =>
      (RT_Clock_State => (RT_Timestamp, RT_Stored_Stamp),
       Monotonic_Clock_State =>
         (Is_Initialized, TSC_Tick_Resolution, TSC_Ticks_Per_Res))
is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   Is_Initialized : Boolean := False;

   --  The RTC is really slow and has inacceptably large resolutions, so we
   --  will cache RTC time as well as when it was cached in monotonic.
   --  That way, by adding deltas, we can build an okayish, finer-grained
   --  resolution clock.
   RT_Timestamp : Time.Timestamp := (0, 0);
   RT_Stored_Stamp : Time.Timestamp := (0, 0);

   --  For monotonic, we use the TSC.
   TSC_Tick_Resolution : Unsigned_64 := 100_000;
   TSC_Ticks_Per_Res : Unsigned_64 := Nanoseconds_In_Second;

   procedure Initialize_Sources is
      TSC_Start, TSC_End     : Unsigned_64;
      EAX, EBX, ECX, EDX     : Unsigned_32;
      EAX1, EBX1, ECX1, EDX1 : Unsigned_32;
      Success                : Boolean;
   begin
      --  We require invariant TSC.
      Snippets.Get_CPUID (16#80000007#, 0, EAX, EBX, ECX, EDX, Success);
      if not Success or else ((EDX and Shift_Left (1, 8)) = 0) then
         Messages.Put_Line ("No invariant TSC detected");
      end if;

      --  We need to calibrate the TSC. For this we can check CPUID.
      Snippets.Get_CPUID (16#15#, 0, EAX, EBX, ECX, EDX, Success);
      if Success then
         if EBX /= 0 and EBX /= 0 then
            Messages.Put_Line ("Monotonic TSC calibration using CPUID 1");
            TSC_Ticks_Per_Res := Unsigned_64 (ECX) * Unsigned_64 (EBX / EAX);
            Normalize_TSC_Hz;
            goto Found_TSC_Frequency;
         end if;

         Snippets.Get_CPUID (16#16#, 0, EAX1, EBX1, ECX1, EDX1, Success);
         if Success and EAX1 /= 0 then
            Messages.Put_Line ("Monotonic TSC calibration using CPUID 2");
            Snippets.Get_CPUID (16#16#, 0, EAX1, EBX1, ECX1, EDX1, Success);
            TSC_Ticks_Per_Res := Unsigned_64 (EAX1 * 10_000_000) *
                                 Unsigned_64 (EAX / EBX);
            Normalize_TSC_Hz;
            goto Found_TSC_Frequency;
         end if;
      end if;

      --  If CPUID does not have the info, we can start checking for clocks.
      --  First one we will use is the HPET.
      HPET.Init (Success);
      if Success then
         Messages.Put_Line ("Monotonic TSC calibration using HPET");
         HPET.Get_Resolution (TSC_Tick_Resolution);
         TSC_Start := Snippets.Read_TSC;
         HPET.NSleep (TSC_Tick_Resolution * 100_000);
         TSC_End := Snippets.Read_TSC;
         TSC_Ticks_Per_Res := TSC_End - TSC_Start;
         TSC_Ticks_Per_Res := TSC_Ticks_Per_Res / 100_000;
         goto Found_TSC_Frequency;
      end if;

      --  After that, we will use the ACPI PM timer.
      ACPI_PM_Timer.Init (Success);
      if Success then
         Messages.Put_Line ("Monotonic TSC calibration using ACPI PM Timer");
         ACPI_PM_Timer.Get_Resolution (TSC_Tick_Resolution);
         TSC_Start := Snippets.Read_TSC;
         ACPI_PM_Timer.NSleep (TSC_Tick_Resolution * 1000);
         TSC_End := Snippets.Read_TSC;
         TSC_Ticks_Per_Res := TSC_End - TSC_Start;
         TSC_Ticks_Per_Res := TSC_Ticks_Per_Res / 1000;
         goto Found_TSC_Frequency;
      end if;

      Panic.Hard_Panic
         ("Could not find a suitable TSC calibration source, tried:" &
          "CPUID leaf detection, HPET, ACPI PM Timer");

   <<Found_TSC_Frequency>>
      Messages.Put_Line
         ("Monotonic resolution fixed at 0x" &
          TSC_Ticks_Per_Res'Image    & " ticks / 0x" &
           TSC_Tick_Resolution'Image & " ns");

      Is_Initialized := True;

      --  Set the RTC base.
      Get_Monotonic_Time (RT_Timestamp);
      RTC.Get_RTC_Date (RT_Stored_Stamp.Seconds);
      RT_Stored_Stamp.Nanoseconds := 0;
   end Initialize_Sources;

   procedure Get_Monotonic_Resolution (Stamp : out Time.Timestamp) is
   begin
      Stamp := (0, TSC_Tick_Resolution);
   end Get_Monotonic_Resolution;

   procedure Get_Monotonic_Time (Stamp : out Time.Timestamp) is
      Cnt : constant Unsigned_64 := Arch.Snippets.Read_TSC;
   begin
      if Is_Initialized then
         Stamp := To_Stamp ((Cnt / TSC_Ticks_Per_Res) * TSC_Tick_Resolution);
      else
         Stamp := (0, 0);
      end if;
   end Get_Monotonic_Time;

   procedure Busy_Monotonic_Sleep (Nanoseconds : Unsigned_64) is
      Curr, Next : Time.Timestamp;
   begin
      if Is_Initialized then
         Get_Monotonic_Time (Next);
         Next := Next + (0, Nanoseconds);
         loop
            Get_Monotonic_Time (Curr);
            exit when Curr >= Next;
         end loop;
      end if;
   end Busy_Monotonic_Sleep;

   procedure Get_Real_Time_Resolution (Stamp : out Time.Timestamp) is
   begin
      Stamp := (0, TSC_Tick_Resolution);
   end Get_Real_Time_Resolution;

   procedure Get_Real_Time (Stamp : out Time.Timestamp) is
      Temp : Time.Timestamp;
   begin
      Get_Monotonic_Time (Temp);
      Temp := Temp - RT_Timestamp;
      Stamp := RT_Stored_Stamp + Temp;
   end Get_Real_Time;

   procedure Set_Real_Time (Stamp : Time.Timestamp) is
   begin
      RT_Stored_Stamp := Stamp;
      Get_Monotonic_Time (RT_Timestamp);
      RTC.Set_RTC_Date (Stamp.Seconds);
   end Set_Real_Time;
   ----------------------------------------------------------------------------
   procedure Normalize_TSC_Hz is
   begin
      --  Force a resolution rounding down to 100ns.
      TSC_Ticks_Per_Res   := TSC_Ticks_Per_Res / 10_000_000;
      TSC_Tick_Resolution := 100;
   end Normalize_TSC_Hz;
end Arch.Clocks;
