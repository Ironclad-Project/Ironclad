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

   --  We do several rounds of calibration to avoid weird values.
   Calibration_Rounds : constant := 3;

   procedure Initialize_Sources is
      EA, EB, EC, ED : Unsigned_32;
      Success        : Boolean;
      Frequency      : Unsigned_64 := 0;
   begin
      --  We require invariant TSC.
      Snippets.Get_CPUID (16#80000007#, 0, EA, EB, EC, ED, Success);
      if not Success or else ((ED and Shift_Left (1, 8)) = 0) then
         Messages.Put_Line ("No invariant TSC detected");
      end if;

      --  CPUID gives the frequency where it enumerates the core crystal
      --  clock.
      Snippets.Get_CPUID (16#15#, 0, EA, EB, EC, ED, Success);
      if Success and EA /= 0 and EB /= 0 and EC /= 0 then
         Messages.Put_Line ("Monotonic TSC calibration using CPUID");
         Frequency := Unsigned_64 (EC) * Unsigned_64 (EB) / Unsigned_64 (EA);
      end if;

      if Frequency = 0 then
         Calibrate_With_PM_Timer (Frequency);
         if Frequency /= 0 then
            Messages.Put_Line ("Monotonic calibration using ACPI PM Timer");
         end if;
      end if;

      if Frequency = 0 then
         Calibrate_With_PIT (Frequency);
         if Frequency /= 0 then
            Messages.Put_Line ("Monotonic calibration using PIT");
         end if;
      end if;

      if Frequency = 0 then
         Panic.Hard_Panic
            ("Could not find a suitable TSC calibration source, tried: " &
             "CPUID leaf detection, ACPI PM Timer, PIT");
      end if;

      TSC_Ticks_Per_Res := Frequency;
      Normalize_TSC_Hz;
      if TSC_Ticks_Per_Res = 0 then
         Panic.Hard_Panic ("The TSC runs under 10 MHz, which is not enough");
      end if;

      Messages.Put_Line
         ("Monotonic resolution fixed at 0x" &
          TSC_Ticks_Per_Res'Image    & " ticks / 0x" &
           TSC_Tick_Resolution'Image & " ns");

      Is_Initialized := True;

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
      --  Force a resolution rounding down to 100ns. Once we do that, maybe we
      --  can go further.
      TSC_Ticks_Per_Res   := TSC_Ticks_Per_Res / 10_000_000;
      TSC_Tick_Resolution := 100;
      while TSC_Ticks_Per_Res > 100 and TSC_Tick_Resolution /= 1 loop
         TSC_Ticks_Per_Res   := TSC_Ticks_Per_Res / 10;
         TSC_Tick_Resolution := TSC_Tick_Resolution / 10;
      end loop;
   end Normalize_TSC_Hz;

   procedure Calibrate_With_PM_Timer (Frequency : out Unsigned_64) is
      Ticks : constant := ACPI_PM_Timer.Frequency / 100;

      --  A port that decodes to nothing reads a constant, so a count that
      --  does not move is given up on.
      Max_Polls         : constant := 1_000_000;
      Max_Stalled_Polls : constant := 1_000;

      Port                      : Unsigned_16;
      Mask                      : Unsigned_32;
      Found                     : Boolean;
      Value, Start              : Unsigned_32 := 0;
      Elapsed, Seen             : Unsigned_32;
      Stalled                   : Natural;
      Before, After             : Unsigned_64;
      Start_Before, Start_After : Unsigned_64;
      Quickest, Count           : Unsigned_64;
      Is_Quick                  : Boolean;
      Best                      : Unsigned_64 := 0;
      Best_Elapsed              : Unsigned_32 := 0;
   begin
      Frequency := 0;
      ACPI_PM_Timer.Find (Port, Mask, Found);
      if not Found then
         return;
      end if;

      for Round in 1 .. Calibration_Rounds loop
         --  The round starts at the quickest of a few reads.
         Start_Before := 0;
         Start_After  := Unsigned_64'Last;
         for Read in 1 .. 8 loop
            Before := Snippets.Read_TSC;
            Value  := Snippets.Port_In32 (Port) and Mask;
            After  := Snippets.Read_TSC;
            if After - Before < Start_After - Start_Before then
               Start_Before := Before;
               Start_After  := After;
               Start        := Value;
            end if;
         end loop;
         Quickest := Start_After - Start_Before;

         --  And ends at the first read past the count that was quick too,
         --  or past twice the count at the latest.
         Elapsed  := 0;
         Seen     := 0;
         Stalled  := 0;
         Before   := Start_Before;
         After    := Start_After;
         for Poll in 1 .. Max_Polls loop
            Before   := Snippets.Read_TSC;
            Elapsed  := ((Snippets.Port_In32 (Port) and Mask) - Start)
                        and Mask;
            After    := Snippets.Read_TSC;
            Quickest := Unsigned_64'Min (Quickest, After - Before);
            Is_Quick := After - Before <= 2 * Quickest;
            exit when Elapsed >= Ticks and (Is_Quick or Elapsed >= 2 * Ticks);
            if Elapsed /= Seen then
               Seen    := Elapsed;
               Stalled := 0;
            else
               Stalled := Stalled + 1;
               exit when Stalled >= Max_Stalled_Polls;
            end if;
         end loop;

         if Elapsed < Ticks then
            return;
         end if;

         --  The TSC at each end is taken as halfway through the read there,
         --  and the count reached is used rather than the one waited for.
         Count := (Before + (After - Before) / 2) -
                  (Start_Before + (Start_After - Start_Before) / 2);
         if Best = 0 or Count < Best then
            Best         := Count;
            Best_Elapsed := Elapsed;
         end if;
      end loop;

      if Best /= 0 and Best_Elapsed /= 0 then
         Frequency :=
            Best * ACPI_PM_Timer.Frequency / Unsigned_64 (Best_Elapsed);
      end if;
   end Calibrate_With_PM_Timer;

   procedure Calibrate_With_PIT (Frequency : out Unsigned_64) is
      --  The counters of the 8254 run at 1.193 MHz off the 14.31818 MHz
      --  clock.
      PIT_Frequency : constant := 1_193_182;
      PIT_Count     : constant := 11_932;
      Max_Polls     : constant := 1_000_000;

      --  NMI_SC and its bits.
      NMI_SC     : constant Unsigned_16 := 16#61#;
      Gate       : constant Unsigned_8  := 16#01#;
      Speaker    : constant Unsigned_8  := 16#02#;
      Out_Status : constant Unsigned_8  := 16#20#;
      Writable   : constant Unsigned_8  := 16#0F#;

      Original, Control         : Unsigned_8 := 0;
      Counted                   : Boolean;
      Before, After             : Unsigned_64;
      Start_Before, Start_After : Unsigned_64;
      Poll_Before, Poll_After   : Unsigned_64;
      Last_Before               : Unsigned_64;
      Quickest, Count           : Unsigned_64;
      Best                      : Unsigned_64 := 0;
      Best_Unchecked            : Unsigned_64 := 0;
   begin
      Frequency := 0;

      --  How long an access takes at the quickest, from a few reads.
      Quickest := Unsigned_64'Last;
      for Read in 1 .. 8 loop
         Before   := Snippets.Read_TSC;
         Original := Snippets.Port_In (NMI_SC);
         After    := Snippets.Read_TSC;
         Quickest := Unsigned_64'Min (Quickest, After - Before);
      end loop;
      Control := Original and Writable and not (Gate or Speaker);

      for Attempt in 1 .. Calibration_Rounds loop
         --  Counter 2 in mode 0, with the gate closed and the speaker off,
         --  for OUT to go high once the count has run out, see the 8254
         --  datasheet 231164-005, Mode 0.
         Snippets.Port_Out (NMI_SC, Control);
         Snippets.Port_Out (16#43#, 16#B0#);
         Snippets.Port_Out (16#42#, Unsigned_8 (PIT_Count mod 256));
         Snippets.Port_Out (16#42#, Unsigned_8 (PIT_Count / 256));

         --  OUT is low from the control word on, so a high one is not a
         --  counter that is there.
         if (Snippets.Port_In (NMI_SC) and Out_Status) = 0 then
            Start_Before := Snippets.Read_TSC;
            Snippets.Port_Out (NMI_SC, Control or Gate);
            Start_After  := Snippets.Read_TSC;

            Counted     := False;
            Poll_Before := Start_Before;
            Poll_After  := Start_After;
            Last_Before := Start_Before;
            for Poll in 1 .. Max_Polls loop
               Last_Before := Poll_Before;
               Poll_Before := Snippets.Read_TSC;
               Counted     := (Snippets.Port_In (NMI_SC) and Out_Status) /= 0;
               Poll_After  := Snippets.Read_TSC;
               Quickest    :=
                  Unsigned_64'Min (Quickest, Poll_After - Poll_Before);
               exit when Counted;
            end loop;

            --  A counter that never got there is not there either.
            exit when not Counted;

            if Best_Unchecked = 0 or Poll_After - Start_After < Best_Unchecked
            then
               Best_Unchecked := Poll_After - Start_After;
            end if;

            --  OUT went high between the read before the one that saw it
            --  and the end of that one, which are two reads, and the TSC
            --  there is taken as halfway through them. Unlike a read of the
            --  PM timer, that cannot be done again, so a round where either
            --  end was slow is run again, unless it keeps being.
            if Start_After - Start_Before <= 2 * Quickest and
               Poll_After - Last_Before <= 4 * Quickest
            then
               Count := (Last_Before + (Poll_After - Last_Before) / 2) -
                        (Start_Before + (Start_After - Start_Before) / 2);
               if Best = 0 or Count < Best then
                  Best := Count;
               end if;
            end if;
         end if;
      end loop;

      Snippets.Port_Out (NMI_SC, Original and Writable);

      if Best /= 0 then
         Frequency := Best * PIT_Frequency / PIT_Count;
      elsif Best_Unchecked /= 0 then
         Frequency := Best_Unchecked * PIT_Frequency / PIT_Count;
      end if;
   end Calibrate_With_PIT;
end Arch.Clocks;
