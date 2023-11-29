--  ipc-pty.adb: PTY creation and management.
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

with Ada.Unchecked_Deallocation;
with Scheduler;

package body IPC.PTY is
   pragma Suppress (All_Checks);

   procedure Free is new Ada.Unchecked_Deallocation (Inner, Inner_Acc);

   function Create
      (Termios     : Devices.TermIOs.Main_Data;
       Window_Size : Devices.TermIOs.Win_Size) return Inner_Acc
   is
   begin
      return new Inner'
         (Primary_Mutex     => Lib.Synchronization.Unlocked_Semaphore,
          Secondary_Mutex   => Lib.Synchronization.Unlocked_Semaphore,
          Global_Data_Mutex => Lib.Synchronization.Unlocked_Semaphore,
          Term_Info         => Termios,
          Term_Size         => Window_Size,
          Was_Closed        => False,
          Termios_Changed   => False,
          Primary_Length    => 0,
          Secondary_Length  => 0,
          Primary_Data      => (others => 0),
          Secondary_Data    => (others => 0));
   end Create;

   procedure Close (Closed : in out Inner_Acc) is
   begin
      Lib.Synchronization.Seize (Closed.Primary_Mutex);
      Lib.Synchronization.Seize (Closed.Secondary_Mutex);
      Lib.Synchronization.Seize (Closed.Global_Data_Mutex);
      if Closed.Was_Closed then
         Free (Closed);
      else
         Closed.Was_Closed := True;
         Lib.Synchronization.Release (Closed.Primary_Mutex);
         Lib.Synchronization.Release (Closed.Secondary_Mutex);
         Lib.Synchronization.Release (Closed.Global_Data_Mutex);
      end if;
   end Close;

   procedure Read_Primary
      (To_Read   : Inner_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural)
   is
   begin
      Read_From_End
         (To_Read.Primary_Mutex'Access, To_Read.Primary_Length'Access,
          To_Read.Primary_Data'Access, Data, Ret_Count);
   end Read_Primary;

   procedure Write_Primary
      (To_Write  : Inner_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural)
   is
   begin
      Write_To_End
         (To_Write.Secondary_Mutex'Access, To_Write.Secondary_Length'Access,
          To_Write.Secondary_Data'Access, Data, Ret_Count);
   end Write_Primary;

   procedure Read_Secondary
      (To_Read   : Inner_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural)
   is
   begin
      Read_From_End
         (To_Read.Secondary_Mutex'Access, To_Read.Secondary_Length'Access,
          To_Read.Secondary_Data'Access, Data, Ret_Count);
   end Read_Secondary;

   procedure Write_Secondary
      (To_Write  : Inner_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural)
   is
   begin
      Write_To_End
         (To_Write.Primary_Mutex'Access, To_Write.Primary_Length'Access,
          To_Write.Primary_Data'Access, Data, Ret_Count);
   end Write_Secondary;

   procedure Poll_Primary
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Can_Prio  : out Boolean)
   is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      Can_Prio := P.Termios_Changed;
      P.Termios_Changed := False;
      Lib.Synchronization.Release (P.Global_Data_Mutex);

      Lib.Synchronization.Seize (P.Primary_Mutex);
      Can_Read  := P.Primary_Length /= 0;
      Can_Write := P.Primary_Length /= P.Primary_Data'Length;
      Lib.Synchronization.Release (P.Primary_Mutex);
   end Poll_Primary;

   procedure Poll_Secondary
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean)
   is
   begin
      Lib.Synchronization.Seize (P.Secondary_Mutex);
      Can_Read  := P.Secondary_Length /= 0;
      Can_Write := P.Secondary_Length /= P.Secondary_Data'Length;
      Lib.Synchronization.Release (P.Secondary_Mutex);
   end Poll_Secondary;

   procedure Get_TermIOs (P : Inner_Acc; T : out Devices.TermIOs.Main_Data) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      T := P.Term_Info;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Get_TermIOs;

   procedure Set_TermIOs (P : Inner_Acc; T : Devices.TermIOs.Main_Data) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      P.Term_Info := T;
      P.Termios_Changed := True;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Set_TermIOs;

   procedure Get_WinSize (P : Inner_Acc; W : out Devices.TermIOs.Win_Size) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      W := P.Term_Size;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Get_WinSize;

   procedure Set_WinSize (P : Inner_Acc; W : Devices.TermIOs.Win_Size) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      P.Term_Size := W;
      P.Termios_Changed := True;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Set_WinSize;
   ----------------------------------------------------------------------------
   procedure Read_From_End
      (End_Mutex  : access Lib.Synchronization.Binary_Semaphore;
       Inner_Len  : access Data_Length;
       Inner_Data : access TTY_Data;
       Data       : out Devices.Operation_Data;
       Ret_Count  : out Natural)
   is
   begin
      Data := (others => 0);

      loop
         if Inner_Len.all /= 0 then
            Lib.Synchronization.Seize (End_Mutex.all);
            exit when Inner_Len.all /= 0;
            Lib.Synchronization.Release (End_Mutex.all);
         end if;
         Scheduler.Yield_If_Able;
      end loop;

      if Data'Length > Inner_Len.all then
         Ret_Count := Inner_Len.all;
      else
         Ret_Count := Data'Length;
      end if;
      if Data'First > Natural'Last - Ret_Count then
         Ret_Count := Natural'Last - Data'First;
      end if;

      Data (Data'First .. Data'First + Ret_Count - 1) :=
         Inner_Data (1 .. Ret_Count);
      for I in 1 .. Ret_Count loop
         for J in Inner_Data'First .. Inner_Data'Last - 1 loop
            Inner_Data (J) := Inner_Data (J + 1);
         end loop;
         if Inner_Len.all > 0 then
            Inner_Len.all := Inner_Len.all - 1;
         else
            exit;
         end if;
      end loop;

      Lib.Synchronization.Release (End_Mutex.all);
   end Read_From_End;

   procedure Write_To_End
      (End_Mutex  : access Lib.Synchronization.Binary_Semaphore;
       Inner_Len  : access Data_Length;
       Inner_Data : access TTY_Data;
       Data       : Devices.Operation_Data;
       Ret_Count  : out Natural)
   is
      Final : Natural;
   begin
      loop
         if Inner_Len.all /= Inner_Data'Length then
            Lib.Synchronization.Seize (End_Mutex.all);
            exit when Inner_Len.all /= Inner_Data'Length;
            Lib.Synchronization.Release (End_Mutex.all);
         end if;
         Scheduler.Yield_If_Able;
      end loop;

      if Data'Length > Inner_Data'Length or else
         Data'Length > Inner_Data'Length - Inner_Len.all
      then
         Final := Inner_Data'Length;
         Ret_Count := Inner_Data'Length - Inner_Len.all;
      else
         Final := Inner_Len.all + Data'Length;
         Ret_Count := Data'Length;
      end if;
      if Data'First > Natural'Last - Ret_Count then
         Ret_Count := Natural'Last - Data'First;
         Final := Inner_Len.all + Ret_Count;
      end if;

      Inner_Data (Inner_Len.all + 1 .. Final) :=
         Data (Data'First .. Data'First + Ret_Count - 1);
      Inner_Len.all := Final;
      Lib.Synchronization.Release (End_Mutex.all);
   end Write_To_End;
end IPC.PTY;
