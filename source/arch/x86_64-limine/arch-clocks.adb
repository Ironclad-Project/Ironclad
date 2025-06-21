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
with Time; use Time;
with Arch.HPET;
with Messages;

package body Arch.Clocks with
   Refined_State =>
      (RT_Clock_State =>
         (Is_Initialized,
          RT_Timestamp_Seconds,
          RT_Timestamp_Nanoseconds,
          RT_Stored_Seconds,
          RT_Stored_Nanoseconds),
       Monotonic_Clock_State =>
         (TSC_Tick_Resolution, TSC_Ticks_Per_Res))
is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   Is_Initialized : Boolean := False;

   --  The RTC is really slow and has inacceptably large resolutions, so we
   --  will cache RTC time as well as when it was cached in monotonic.
   --  That way, by adding deltas, we can build an okayish, finer-grained
   --  resolution clock.
   RT_Timestamp_Seconds     : Unsigned_64;
   RT_Timestamp_Nanoseconds : Unsigned_64;
   RT_Stored_Seconds        : Unsigned_64;
   RT_Stored_Nanoseconds    : Unsigned_64;

   --  For monotonic, we use the TSC.
   Nanoseconds_In_Second : constant    := 1_000_000_000;
   TSC_Tick_Resolution   : Unsigned_64 := 100_000;
   TSC_Ticks_Per_Res     : Unsigned_64 := Nanoseconds_In_Second;

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
            TSC_Ticks_Per_Res   := Unsigned_64 (ECX) * Unsigned_64 (EBX / EAX);
            TSC_Ticks_Per_Res   := TSC_Ticks_Per_Res / 1_000_000;
            TSC_Tick_Resolution := 1_000;
            goto Found_TSC_Frequency;
         end if;

         Snippets.Get_CPUID (16#16#, 0, EAX1, EBX1, ECX1, EDX1, Success);
         if Success and EAX1 /= 0 then
            Messages.Put_Line ("Monotonic TSC calibration using CPUID 2");
            Snippets.Get_CPUID (16#16#, 0, EAX1, EBX1, ECX1, EDX1, Success);
            TSC_Ticks_Per_Res := Unsigned_64 (EAX1 * 10_000_000) *
                                 Unsigned_64 (EAX / EBX);
            TSC_Ticks_Per_Res   := TSC_Ticks_Per_Res / 1_000_000;
            TSC_Tick_Resolution := 1_000;
            goto Found_TSC_Frequency;
         end if;
      end if;

      --  If CPUID does not have the info, we can start checking for clocks.
      --  First one we will use is the HPET.
      if HPET.Init then
         Messages.Put_Line ("Monotonic TSC calibration using HPET");
         TSC_Tick_Resolution := 100_000; --  HPET limited.
         TSC_Start := Snippets.Read_TSC;
         HPET.NSleep (Natural (TSC_Tick_Resolution));
         TSC_End := Snippets.Read_TSC;
         TSC_Ticks_Per_Res := TSC_End - TSC_Start;
         goto Found_TSC_Frequency;
      end if;

      Panic.Hard_Panic
         ("Could not find a suitable TSC calibration source, tried:" &
          "CPUID leaf detection, HPET");

   <<Found_TSC_Frequency>>
      Messages.Put_Line
         ("Monotonic resolution fixed at 0x" &
          TSC_Ticks_Per_Res'Image    & " ticks / 0x" &
           TSC_Tick_Resolution'Image & " ns");

      Is_Initialized := True;

      --  Set the RTC base.
      Get_Monotonic_Time (RT_Timestamp_Seconds, RT_Timestamp_Nanoseconds);
      RTC.Get_RTC_Date (RT_Stored_Seconds);
      RT_Stored_Nanoseconds := 0;
   end Initialize_Sources;

   procedure Get_Monotonic_Resolution (Seconds, Nanoseconds : out Unsigned_64)
   is
   begin
      Seconds     := 0;
      Nanoseconds := TSC_Tick_Resolution;
   end Get_Monotonic_Resolution;

   procedure Get_Monotonic_Time (Seconds, Nanoseconds : out Unsigned_64) is
      Cnt : Unsigned_64;
   begin
      if Is_Initialized then
         Cnt         := Arch.Snippets.Read_TSC;
         Nanoseconds := (Cnt / TSC_Ticks_Per_Res) * TSC_Tick_Resolution;
         Seconds     := Nanoseconds / Nanoseconds_In_Second;
         Nanoseconds := Nanoseconds mod Nanoseconds_In_Second;
      else
         Seconds     := 0;
         Nanoseconds := 0;
      end if;
   end Get_Monotonic_Time;

   procedure Busy_Monotonic_Sleep (Nanoseconds : Unsigned_64) is
      Curr_Sec, Curr_Nano : Unsigned_64;
      Next_Sec, Next_Nano : Unsigned_64;
   begin
      if Is_Initialized then
         Get_Monotonic_Time (Next_Sec, Next_Nano);
         Time.Increment (Next_Sec, Next_Nano, 0, Nanoseconds);
         loop
            Get_Monotonic_Time (Curr_Sec, Curr_Nano);
            exit when Time.Is_Greater_Equal
               (Curr_Sec, Curr_Nano, Next_Sec, Next_Nano);
         end loop;
      end if;
   end Busy_Monotonic_Sleep;

   procedure Get_Real_Time_Resolution (Seconds, Nanoseconds : out Unsigned_64)
   is
   begin
      Seconds     := 0;
      Nanoseconds := TSC_Tick_Resolution;
   end Get_Real_Time_Resolution;

   procedure Get_Real_Time (Seconds, Nanoseconds : out Unsigned_64) is
      Temp1, Temp2 : Unsigned_64;
   begin
      Get_Monotonic_Time (Temp1, Temp2);
      Subtract (Temp1, Temp2, RT_Timestamp_Seconds, RT_Timestamp_Nanoseconds);
      Seconds := RT_Stored_Seconds + Temp1;
      Nanoseconds := RT_Stored_Nanoseconds + Temp2;
   end Get_Real_Time;

   procedure Set_Real_Time (Seconds, Nanoseconds : Unsigned_64) is
   begin
      RT_Stored_Seconds     := Seconds;
      RT_Stored_Nanoseconds := Nanoseconds;
      Get_Monotonic_Time (RT_Timestamp_Seconds, RT_Timestamp_Nanoseconds);
      RTC.Set_RTC_Date (Seconds);
   end Set_Real_Time;
end Arch.Clocks;
