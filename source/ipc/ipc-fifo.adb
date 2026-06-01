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
          Read_Index    => 1,
          Write_Index   => 1,
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
      New_Buffer, Old_Buffer : Devices.Operation_Data_Acc := null;
      New_Idx : Natural := 1;
   begin
      New_Buffer := new Devices.Operation_Data'[1 .. Size => 0];

      Synchronization.Seize (P.Mutex);
      if Size = P.Data_Count then
         Success := True;
      elsif Size > P.Data_Count then
         while P.Read_Index /= P.Write_Index loop
            New_Buffer (New_Idx) := P.Data (P.Read_Index);
            Advance_Index (P, P.Read_Index);
            New_Idx := New_Idx + 1;
         end loop;
         Old_Buffer := P.Data;
         P.Data := New_Buffer;
         P.Read_Index := 1;
         P.Write_Index := P.Data_Count + 1;
         Success := True;
      else
         Success := False;
      end if;
      Synchronization.Release (P.Mutex);
      if Success and Old_Buffer /= null then
         Free (Old_Buffer);
      end if;
   end Set_Size;

   procedure Read
      (To_Read     : Inner_Acc;
       Data        : out Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Pipe_Status)
   is
      Read_Count : Natural := 0;
   begin
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

      for C of Data loop
         exit when To_Read.Data_Count = 0;
         C := To_Read.Data (To_Read.Read_Index);
         Advance_Index (To_Read, To_Read.Read_Index);
         Read_Count := Read_Count + 1;
         To_Read.Data_Count := To_Read.Data_Count - 1;
      end loop;

      Synchronization.Release (To_Read.Mutex);
      Ret_Count := Read_Count;
      Success   := Pipe_Success;
   end Read;

   procedure Write
      (To_Write    : Inner_Acc;
       Data        : Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Pipe_Status)
   is
      Written_Count : Natural := 0;
   begin
      if Is_Blocking then
         loop
            Synchronization.Seize (To_Write.Mutex);
            --  Check Reader_Closed inside lock to avoid TOCTOU race
            if To_Write.Reader_Closed then
               Synchronization.Release (To_Write.Mutex);
               Ret_Count := 0;
               Success   := Broken_Failure;
               return;
            end if;
            exit when To_Write.Data_Count /= To_Write.Data'Length;
            Synchronization.Release (To_Write.Mutex);
            Scheduler.Yield_If_Able;
         end loop;
      else
         Synchronization.Seize (To_Write.Mutex);
         --  Check Reader_Closed inside lock to avoid TOCTOU race
         if To_Write.Reader_Closed then
            Synchronization.Release (To_Write.Mutex);
            Ret_Count := 0;
            Success   := Broken_Failure;
            return;
         end if;
         if To_Write.Data_Count = To_Write.Data'Length then
            Synchronization.Release (To_Write.Mutex);
            Ret_Count := 0;
            Success   := Would_Block_Failure;
            return;
         end if;
      end if;

      for C of Data loop
         exit when To_Write.Data_Count = To_Write.Data'Length;
         To_Write.Data (To_Write.Write_Index) := C;
         Advance_Index (To_Write, To_Write.Write_Index);
         Written_Count := Written_Count + 1;
         To_Write.Data_Count := To_Write.Data_Count + 1;
      end loop;

      Synchronization.Release (To_Write.Mutex);
      Ret_Count := Written_Count;
      Success   := Pipe_Success;
   end Write;
   ----------------------------------------------------------------------------
   procedure Advance_Index (P : Inner_Acc; Idx : in out Natural) is
   begin
      Idx := (if Idx = P.Data'Last then P.Data'First else Idx + 1);
   end Advance_Index;

   procedure Common_Close (To_Close : in out Inner_Acc) is
      pragma Annotate
         (GNATprove,
          False_Positive,
          "memory leak",
          "Cannot verify that the pipes have only 1 reference, but they do");
      Must_Free : Boolean;
   begin
      --  Binary semaphores in ironclad keep track of interrupt state, so we
      --  must unlock to avoid interrupt deadlock even when freeing.
      Must_Free := To_Close.Reader_Closed and To_Close.Writer_Closed;
      Synchronization.Release (To_Close.Mutex);
      if Must_Free then
         Free (To_Close.Data);
         Free (To_Close);
      else
         To_Close := null;
      end if;
   end Common_Close;
end IPC.FIFO;
