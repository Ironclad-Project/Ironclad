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

package body IPC.PTY is
   pragma Suppress (All_Checks);

   procedure Free is new Ada.Unchecked_Deallocation (Inner, Inner_Acc);

   function Create
      (Termios     : Devices.TermIOs.Main_Data;
       Window_Size : Devices.TermIOs.Win_Size) return Inner_Acc
   is
      Prim  : constant FIFO.Inner_Acc := FIFO.Create (Is_Blocking => True);
      Secon : constant FIFO.Inner_Acc := FIFO.Create (Is_Blocking => True);
   begin
      return new Inner'
         (Mutex          => Lib.Synchronization.Unlocked_Semaphore,
          Secondary_Pipe => Secon,
          Primary_Pipe   => Prim,
          Term_Info      => Termios,
          Term_Size      => Window_Size,
          Was_Closed     => False);
   end Create;

   procedure Close (Closed : in out Inner_Acc) is
   begin
      Lib.Synchronization.Seize (Closed.Mutex);
      if Closed.Was_Closed then
         FIFO.Close (Closed.Primary_Pipe);
         FIFO.Close (Closed.Secondary_Pipe);
         Free (Closed);
      else
         Closed.Was_Closed := True;
         Lib.Synchronization.Release (Closed.Mutex);
      end if;
   end Close;

   procedure Read_Primary
      (To_Read   : Inner_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural)
   is
      Discard : IPC.FIFO.Pipe_Status;
   begin
      FIFO.Read (To_Read.Primary_Pipe, Data, Ret_Count, Discard);
   end Read_Primary;

   procedure Write_Primary
      (To_Write  : Inner_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural)
   is
      Discard : IPC.FIFO.Pipe_Status;
   begin
      FIFO.Write (To_Write.Secondary_Pipe, Data, Ret_Count, Discard);
   end Write_Primary;

   procedure Read_Secondary
      (To_Read   : Inner_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural)
   is
      Discard : IPC.FIFO.Pipe_Status;
   begin
      FIFO.Read (To_Read.Secondary_Pipe, Data, Ret_Count, Discard);
   end Read_Secondary;

   procedure Write_Secondary
      (To_Write  : Inner_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural)
   is
      Discard : IPC.FIFO.Pipe_Status;
   begin
      FIFO.Write (To_Write.Primary_Pipe, Data, Ret_Count, Discard);
   end Write_Secondary;

   procedure Poll_Primary
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean)
   is
      Prim : FIFO.Inner_Acc renames P.Primary_Pipe;
      Discard_1, Discard_2, Discard_3 : Boolean;
   begin
      FIFO.Poll_Reader (Prim, Can_Read,  Discard_1, Discard_2, Discard_3);
      FIFO.Poll_Writer (Prim, Discard_1, Can_Write, Discard_2, Discard_3);
   end Poll_Primary;

   procedure Poll_Secondary
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean)
   is
      Snd : FIFO.Inner_Acc renames P.Secondary_Pipe;
      Discard_1, Discard_2, Discard_3 : Boolean;
   begin
      FIFO.Poll_Reader (Snd, Can_Read,  Discard_1, Discard_2, Discard_3);
      FIFO.Poll_Writer (Snd, Discard_1, Can_Write, Discard_2, Discard_3);
   end Poll_Secondary;

   procedure Get_TermIOs (P : Inner_Acc; T : out Devices.TermIOs.Main_Data) is
   begin
      Lib.Synchronization.Seize (P.Mutex);
      T := P.Term_Info;
      Lib.Synchronization.Release (P.Mutex);
   end Get_TermIOs;

   procedure Set_TermIOs (P : Inner_Acc; T : Devices.TermIOs.Main_Data) is
   begin
      Lib.Synchronization.Seize (P.Mutex);
      P.Term_Info := T;
      Lib.Synchronization.Release (P.Mutex);
   end Set_TermIOs;

   procedure Get_WinSize (P : Inner_Acc; W : out Devices.TermIOs.Win_Size) is
   begin
      Lib.Synchronization.Seize (P.Mutex);
      W := P.Term_Size;
      Lib.Synchronization.Release (P.Mutex);
   end Get_WinSize;

   procedure Set_WinSize (P : Inner_Acc; W : Devices.TermIOs.Win_Size) is
   begin
      Lib.Synchronization.Seize (P.Mutex);
      P.Term_Size := W;
      Lib.Synchronization.Release (P.Mutex);
   end Set_WinSize;
end IPC.PTY;
