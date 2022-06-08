--  lib-messages.adb: Utilities for reporting messages to the user.
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

with Ada.Characters.Latin_1;
with System.Storage_Elements; use System.Storage_Elements;
with Lib.Synchronization;
with Lib.InnerPrint;

package body Lib.Messages is
   Messages_Mutex : aliased Lib.Synchronization.Binary_Semaphore;

   procedure Put_Line (Message : String) is
   begin
      Lib.Synchronization.Seize (Messages_Mutex'Access);
      InnerPrint.Inner_Print (Message);
      InnerPrint.Inner_Print (Ada.Characters.Latin_1.LF);
      Lib.Synchronization.Release (Messages_Mutex'Access);
   end Put_Line;

   procedure Put (Message : String) is
   begin
      Lib.Synchronization.Seize (Messages_Mutex'Access);
      InnerPrint.Inner_Print (Message);
      Lib.Synchronization.Release (Messages_Mutex'Access);
   end Put;

   procedure Put (Message : Character) is
   begin
      Lib.Synchronization.Seize (Messages_Mutex'Access);
      InnerPrint.Inner_Print (Message);
      Lib.Synchronization.Release (Messages_Mutex'Access);
   end Put;

   procedure Put (Message : Integer; Pad, Use_Hex : Boolean := False) is
   begin
      Put (Integer_64 (Message), Pad, Use_Hex);
   end Put;

   procedure Put (Message : Integer_64; Pad, Use_Hex : Boolean := False) is
   begin
      if Message < 0 then
         Put ('-');
      end if;
      Put (Unsigned_64 (abs Message), Pad, Use_Hex);
   end Put;

   procedure Put (Message : Unsigned_64; Pad, Use_Hex : Boolean := False) is
      Conversion_Table : constant String      := "0123456789ABCDEF";
      To_Convert       : Unsigned_64          := Message;
      Base             : Unsigned_64          := 10;
      Written          : Integer              := 0;
      Result           : String (1 .. 20);
   begin
      Lib.Synchronization.Seize (Messages_Mutex'Access);
      if Use_Hex then
         InnerPrint.Inner_Print ("0x");
         Base := 16;
      end if;

      if Message = 0 and not Pad then
         InnerPrint.Inner_Print ("0");
         Lib.Synchronization.Release (Messages_Mutex'Access);
         return;
      end if;

      if Message /= 0 then
         while To_Convert /= 0 loop
            Written          := Written + 1;
            Result (Written) := Conversion_Table
               (Integer (To_Convert rem Base) + 1);
            To_Convert       := To_Convert / Base;
         end loop;
      end if;

      if Pad then
         for I in Written .. Result'Length loop
            InnerPrint.Inner_Print ('0');
         end loop;
      end if;

      for I in reverse 1 .. Written loop
         InnerPrint.Inner_Print (Result (I));
      end loop;
      Lib.Synchronization.Release (Messages_Mutex'Access);
   end Put;

   procedure Put (Message : System.Address; Pad : Boolean := False) is
   begin
      Put (Unsigned_64 (To_Integer (Message)), Pad, True);
   end Put;
end Lib.Messages;
