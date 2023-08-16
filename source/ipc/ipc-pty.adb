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
with Arch.Snippets;

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
      Final_Len : Natural := Data'Length;
   begin
      Data := (others => 0);

      loop
         if To_Read.Primary_Length /= 0 then
            Lib.Synchronization.Seize (To_Read.Primary_Mutex);
            exit when To_Read.Primary_Length /= 0;
            Lib.Synchronization.Release (To_Read.Primary_Mutex);
         end if;
         Arch.Snippets.Pause;
      end loop;

      if Final_Len > To_Read.Primary_Length then
         Final_Len := To_Read.Primary_Length;
      end if;
      if Data'First > Natural'Last - Final_Len then
         Final_Len := Natural'Last - Data'First;
      end if;

      Data (Data'First .. Data'First + Final_Len - 1) :=
         To_Read.Primary_Data (1 .. Final_Len);
      for I in 1 .. Final_Len loop
         for J in To_Read.Primary_Data'First .. To_Read.Primary_Data'Last - 1
         loop
            To_Read.Primary_Data (J) := To_Read.Primary_Data (J + 1);
         end loop;
         if To_Read.Primary_Length > 0 then
            To_Read.Primary_Length := To_Read.Primary_Length - 1;
         else
            exit;
         end if;
      end loop;

      Lib.Synchronization.Release (To_Read.Primary_Mutex);
      Ret_Count := Final_Len;
   end Read_Primary;

   procedure Write_Primary
      (To_Write  : Inner_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural)
   is
      Len   : Natural := Data'Length;
      Final : Natural;
   begin
      loop
         if To_Write.Secondary_Length /= To_Write.Secondary_Data'Length then
            Lib.Synchronization.Seize (To_Write.Secondary_Mutex);
            exit when To_Write.Secondary_Length /=
                      To_Write.Secondary_Data'Length;
            Lib.Synchronization.Release (To_Write.Secondary_Mutex);
         end if;
         Arch.Snippets.Pause;
      end loop;

      if Len > To_Write.Secondary_Data'Length or else
         Len > To_Write.Secondary_Data'Length - To_Write.Secondary_Length
      then
         Final := To_Write.Secondary_Data'Length;
         Len   := To_Write.Secondary_Data'Length - To_Write.Secondary_Length;
      else
         Final := To_Write.Secondary_Length + Len;
      end if;
      if Data'First > Natural'Last - Len then
         Len   := Natural'Last - Data'First;
         Final := To_Write.Secondary_Length + Len;
      end if;

      To_Write.Secondary_Data (To_Write.Secondary_Length + 1 .. Final) :=
         Data (Data'First .. Data'First + Len - 1);
      To_Write.Secondary_Length := Final;

      Lib.Synchronization.Release (To_Write.Secondary_Mutex);
      Ret_Count := Len;
   end Write_Primary;

   procedure Read_Secondary
      (To_Read   : Inner_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural)
   is
      Final_Len : Natural := Data'Length;
   begin
      Data := (others => 0);

      loop
         if To_Read.Secondary_Length /= 0 then
            Lib.Synchronization.Seize (To_Read.Secondary_Mutex);
            exit when To_Read.Secondary_Length /= 0;
            Lib.Synchronization.Release (To_Read.Secondary_Mutex);
         end if;
         Arch.Snippets.Pause;
      end loop;

      if Final_Len > To_Read.Secondary_Length then
         Final_Len := To_Read.Secondary_Length;
      end if;
      if Data'First > Natural'Last - Final_Len then
         Final_Len := Natural'Last - Data'First;
      end if;

      Data (Data'First .. Data'First + Final_Len - 1) :=
         To_Read.Secondary_Data (1 .. Final_Len);
      for I in 1 .. Final_Len loop
         for J in To_Read.Secondary_Data'First ..
                  To_Read.Secondary_Data'Last - 1
         loop
            To_Read.Secondary_Data (J) := To_Read.Secondary_Data (J + 1);
         end loop;
         if To_Read.Secondary_Length > 0 then
            To_Read.Secondary_Length := To_Read.Secondary_Length - 1;
         else
            exit;
         end if;
      end loop;

      Lib.Synchronization.Release (To_Read.Secondary_Mutex);
      Ret_Count := Final_Len;
   end Read_Secondary;

   procedure Write_Secondary
      (To_Write  : Inner_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural)
   is
      Len   : Natural := Data'Length;
      Final : Natural;
   begin
      loop
         if To_Write.Primary_Length /= To_Write.Primary_Data'Length then
            Lib.Synchronization.Seize (To_Write.Primary_Mutex);
            exit when To_Write.Primary_Length /= To_Write.Primary_Data'Length;
            Lib.Synchronization.Release (To_Write.Primary_Mutex);
         end if;
         Arch.Snippets.Pause;
      end loop;

      if Len > To_Write.Primary_Data'Length or else
         Len > To_Write.Primary_Data'Length - To_Write.Primary_Length
      then
         Final := To_Write.Primary_Data'Length;
         Len   := To_Write.Primary_Data'Length - To_Write.Primary_Length;
      else
         Final := To_Write.Primary_Length + Len;
      end if;
      if Data'First > Natural'Last - Len then
         Len   := Natural'Last - Data'First;
         Final := To_Write.Primary_Length + Len;
      end if;

      To_Write.Primary_Data (To_Write.Primary_Length + 1 .. Final) :=
         Data (Data'First .. Data'First + Len - 1);
      To_Write.Primary_Length := Final;

      Lib.Synchronization.Release (To_Write.Primary_Mutex);
      Ret_Count := Len;
   end Write_Secondary;

   procedure Poll_Primary
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean)
   is
   begin
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
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Set_WinSize;
end IPC.PTY;
