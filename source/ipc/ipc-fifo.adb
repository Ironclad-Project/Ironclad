--  ipc-fifo.adb: Pipe creation and management.
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

with Ada.Unchecked_Deallocation;
with Scheduler;

package body IPC.FIFO is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   procedure Free is new Ada.Unchecked_Deallocation (Inner, Inner_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
      (Devices.Operation_Data, Devices.Operation_Data_Acc);

   function Create return Inner_Acc is
      Data : Devices.Operation_Data_Acc;
   begin
      Data := new Devices.Operation_Data'(1 .. Default_Data_Length => 0);
      return new Inner'
         (Reader_Closed => False,
          Writer_Closed => False,
          Mutex         => Synchronization.Unlocked_Semaphore,
          Data_Count    => 0,
          Data          => Data);
   end Create;

   procedure Poll_Reader
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean;
       Is_Broken : out Boolean)
   is
   begin
      Synchronization.Seize (P.Mutex);
      Can_Read  := P.Data_Count /= 0;
      Can_Write := False;
      Is_Error  := False;
      Is_Broken := P.Writer_Closed or P.Reader_Closed;
      Synchronization.Release (P.Mutex);
   end Poll_Reader;

   procedure Poll_Writer
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean;
       Is_Broken : out Boolean)
   is
   begin
      Synchronization.Seize (P.Mutex);
      Can_Read  := False;
      Can_Write := P.Data_Count /= P.Data'Length;
      Is_Error  := False;
      Is_Broken := False;
      Synchronization.Release (P.Mutex);
   end Poll_Writer;

   procedure Is_Empty (P : Inner_Acc; Is_Empty : out Boolean) is
   begin
      Synchronization.Seize (P.Mutex);
      Is_Empty := P.Data_Count = 0;
      Synchronization.Release (P.Mutex);
   end Is_Empty;

   procedure Close_Reader (To_Close : in out Inner_Acc) is
   begin
      Synchronization.Seize (To_Close.Mutex);
      To_Close.Reader_Closed := True;
      Common_Close (To_Close);
   end Close_Reader;

   procedure Close_Writer (To_Close : in out Inner_Acc) is
   begin
      Synchronization.Seize (To_Close.Mutex);
      To_Close.Writer_Closed := True;
      Common_Close (To_Close);
   end Close_Writer;

   procedure Close (To_Close : in out Inner_Acc) is
   begin
      Synchronization.Seize (To_Close.Mutex);
      To_Close.Reader_Closed := True;
      To_Close.Writer_Closed := True;
      Common_Close (To_Close);
   end Close;

   procedure Get_Size (P : Inner_Acc; Size : out Natural) is
   begin
      Synchronization.Seize (P.Mutex);
      Size := P.Data'Length;
      Synchronization.Release (P.Mutex);
   end Get_Size;

   procedure Set_Size (P : Inner_Acc; Size : Natural; Success : out Boolean) is
      New_Buffer : Devices.Operation_Data_Acc;
   begin
      Synchronization.Seize (P.Mutex);
      if Size >= P.Data_Count then
         New_Buffer := new Devices.Operation_Data'[1 .. Size => 0];
         New_Buffer (1 .. P.Data_Count) := P.Data (1 .. P.Data_Count);
         Free (P.Data);
         P.Data  := New_Buffer;
         Success := True;
      else
         Success := False;
      end if;
      Synchronization.Release (P.Mutex);
   end Set_Size;

   procedure Read
      (To_Read     : Inner_Acc;
       Data        : out Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Pipe_Status)
   is
      Final_Len : Natural := Data'Length;
   begin
      Data := [others => 0];

      if Is_Blocking then
         loop
            Synchronization.Seize (To_Read.Mutex);
            exit when To_Read.Data_Count /= 0;
            if To_Read.Writer_Closed then
               Ret_Count := 0;
               Success   := Pipe_Success;
               Synchronization.Release (To_Read.Mutex);
               return;
            end if;
            Synchronization.Release (To_Read.Mutex);
            Scheduler.Yield_If_Able;
         end loop;
      else
         Synchronization.Seize (To_Read.Mutex);
         if To_Read.Data_Count = 0 then
            Ret_Count := 0;
            if To_Read.Writer_Closed then
               Success := Pipe_Success;
            else
               Success := Would_Block_Failure;
            end if;
            Synchronization.Release (To_Read.Mutex);
            return;
         end if;
      end if;

      if Final_Len > To_Read.Data_Count then
         Final_Len := To_Read.Data_Count;
      end if;
      if Data'First > Natural'Last - Final_Len then
         Final_Len := Natural'Last - Data'First;
      end if;

      Data (Data'First .. Data'First + Final_Len - 1) :=
         To_Read.Data (1 .. Final_Len);
      for I in 1 .. Final_Len loop
         for J in To_Read.Data'First .. To_Read.Data'Last - 1 loop
            To_Read.Data (J) := To_Read.Data (J + 1);
         end loop;
         if To_Read.Data_Count > 0 then
            To_Read.Data_Count := To_Read.Data_Count - 1;
         else
            exit;
         end if;
      end loop;

      Synchronization.Release (To_Read.Mutex);
      Ret_Count := Final_Len;
      Success   := Pipe_Success;
   end Read;

   procedure Write
      (To_Write    : Inner_Acc;
       Data        : Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Pipe_Status)
   is
      Len   : Natural := Data'Length;
      Final : Natural;
   begin
      if To_Write.Reader_Closed then
         Ret_Count := 0;
         Success   := Broken_Failure;
         return;
      end if;

      if Is_Blocking then
         loop
            Synchronization.Seize (To_Write.Mutex);
            exit when To_Write.Data_Count /= To_Write.Data'Length;
            Synchronization.Release (To_Write.Mutex);
            Scheduler.Yield_If_Able;
         end loop;
      else
         Synchronization.Seize (To_Write.Mutex);
         if To_Write.Data_Count = To_Write.Data'Length then
            Ret_Count := 0;
            Success   := Would_Block_Failure;
            Synchronization.Release (To_Write.Mutex);
            return;
         end if;
      end if;

      if Len > To_Write.Data'Length or else
         Len > To_Write.Data'Length - To_Write.Data_Count
      then
         Final := To_Write.Data'Length;
         Len   := To_Write.Data'Length - To_Write.Data_Count;
      else
         Final := To_Write.Data_Count + Len;
      end if;
      if Data'First > Natural'Last - Len then
         Len   := Natural'Last - Data'First;
         Final := To_Write.Data_Count + Len;
      end if;

      if To_Write.Data_Count /= Natural'Last then
         To_Write.Data (To_Write.Data_Count + 1 .. Final) :=
            Data (Data'First .. Data'First + Len - 1);
         To_Write.Data_Count := Final;
         Ret_Count := Len;
         Success   := Pipe_Success;
      else
         Ret_Count := 0;
         Success   := Would_Block_Failure;
      end if;

      Synchronization.Release (To_Write.Mutex);
   end Write;
   ----------------------------------------------------------------------------
   procedure Common_Close (To_Close : in out Inner_Acc) is
      pragma Annotate
         (GNATprove,
          False_Positive,
          "memory leak",
          "Cannot verify that the pipes have only 1 reference, but they do");
   begin
      if To_Close.Reader_Closed and To_Close.Writer_Closed then
         Free (To_Close.Data);
         Free (To_Close);
      else
         Synchronization.Release (To_Close.Mutex);
      end if;
      To_Close := null;
   end Common_Close;
end IPC.FIFO;
