--  lib-kasan.adb: Kernel Address SANitizer.
--  Copyright (C) 2024 streaksu
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

with Lib.Messages;            use Lib.Messages;
with Interfaces;              use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Memory;

package body Lib.KASAN is
   Was_Init : Boolean := False;

   procedure Init is
   begin
      Was_Init := True;
   end Init;
   ----------------------------------------------------------------------------
   procedure Load1 (Addr : System.Address) is
   begin
      Check_Memory (Caller_Address (0), Addr, 1, True);
   end Load1;

   procedure Load2 (Addr : System.Address) is
   begin
      Check_Memory (Caller_Address (0), Addr, 2, True);
   end Load2;

   procedure Load4 (Addr : System.Address) is
   begin
      Check_Memory (Caller_Address (0), Addr, 4, True);
   end Load4;

   procedure Load8 (Addr : System.Address) is
   begin
      Check_Memory (Caller_Address (0), Addr, 8, True);
   end Load8;

   procedure Load16 (Addr : System.Address) is
   begin
      Check_Memory (Caller_Address (0), Addr, 16, True);
   end Load16;

   procedure Load_N (Addr : System.Address; Size : size_t) is
   begin
      Check_Memory (Caller_Address (0), Addr, Size, True);
   end Load_N;

   procedure Store1 (Addr : System.Address) is
   begin
      Check_Memory (Caller_Address (0), Addr, 1, False);
   end Store1;

   procedure Store2 (Addr : System.Address) is
   begin
      Check_Memory (Caller_Address (0), Addr, 2, False);
   end Store2;

   procedure Store4 (Addr : System.Address) is
   begin
      Check_Memory (Caller_Address (0), Addr, 4, False);
   end Store4;

   procedure Store8 (Addr : System.Address) is
   begin
      Check_Memory (Caller_Address (0), Addr, 8, False);
   end Store8;

   procedure Store16 (Addr : System.Address) is
   begin
      Check_Memory (Caller_Address (0), Addr, 16, False);
   end Store16;

   procedure Store_N (Addr : System.Address; Size : size_t) is
   begin
      Check_Memory (Caller_Address (0), Addr, Size, False);
   end Store_N;
   ----------------------------------------------------------------------------
   procedure Handle_No_Return is
   begin
      null; --  Do nothing! Linux and managarm leave this empty as well.
   end Handle_No_Return;
   ----------------------------------------------------------------------------
   procedure Check_Memory
      (Caller, Fault_Addr : System.Address;
       Size               : size_t;
       Was_Load           : Boolean)
   is
   begin
      if Size = 0 then
         return;
      end if;

      if Was_Init     and
         not Was_Load and
         To_Integer (Fault_Addr) >= Memory.Kernel_Offset
      then
         Report_Event (Caller, Fault_Addr, Size, Was_Load);
      end if;
   end Check_Memory;

   procedure Report_Event
      (Caller, Fault_Addr : System.Address;
       Size               : size_t;
       Was_Load           : Boolean)
   is
      Stp1, Stp2, Stp3 : Translated_String;
      Len1, Len2, Len3 : Natural;
   begin
      Image (Unsigned_64 (To_Integer (Fault_Addr)), Stp1, Len1, True);
      Image (Unsigned_64 (To_Integer (Caller)),     Stp2, Len2, True);
      Image (Unsigned_64 (Size),                    Stp3, Len3, False);

      Put_Line
         ("KASAN event at "                                           &
          Stp1 (Stp1'Last - Len1 + 1 .. Stp1'Last)                    &
          " (Size " & Stp3 (Stp3'Last - Len3 + 1 .. Stp3'Last) & ") " &
          Stp2 (Stp2'Last - Len2 + 1 .. Stp2'Last)                    &
          (if Was_Load then " (LOAD)" else " (STORE)"));
   end Report_Event;
end Lib.KASAN;
