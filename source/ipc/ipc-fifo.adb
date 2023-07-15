--  ipc-fifo.adb: Pipe creation and management.
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

package body IPC.FIFO is
   pragma Suppress (All_Checks);

   procedure Free is new Ada.Unchecked_Deallocation (Inner, Inner_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
      (Devices.Operation_Data, Devices.Operation_Data_Acc);

   function Create (Is_Blocking : Boolean := True) return Inner_Acc is
      Data : Devices.Operation_Data_Acc;
   begin
      Data := new Devices.Operation_Data'(1 .. Default_Data_Length => 0);
      return new Inner'
         (Reader_Closed     => False,
          Writer_Closed     => False,
          Mutex             => Lib.Synchronization.Unlocked_Semaphore,
          Is_Read_Blocking  => Is_Blocking,
          Is_Write_Blocking => Is_Blocking,
          Data_Count        => 0,
          Data              => Data);
   end Create;

   function Is_Read_Blocking (P : Inner_Acc) return Boolean is
   begin
      return P.Is_Read_Blocking;
   end Is_Read_Blocking;

   function Is_Write_Blocking (P : Inner_Acc) return Boolean is
   begin
      return P.Is_Write_Blocking;
   end Is_Write_Blocking;

   procedure Set_Read_Blocking (P : Inner_Acc; B : Boolean) is
   begin
      Lib.Synchronization.Seize (P.Mutex);
      P.Is_Read_Blocking := B;
      Lib.Synchronization.Release (P.Mutex);
   end Set_Read_Blocking;

   procedure Set_Write_Blocking (P : Inner_Acc; B : Boolean) is
   begin
      Lib.Synchronization.Seize (P.Mutex);
      P.Is_Write_Blocking := B;
      Lib.Synchronization.Release (P.Mutex);
   end Set_Write_Blocking;

   procedure Poll_Reader
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean;
       Is_Broken : out Boolean)
   is
   begin
      Can_Read  := not Is_Empty (P);
      Can_Write := False;
      Is_Error  := False;
      Is_Broken := P.Writer_Closed or P.Reader_Closed;
   end Poll_Reader;

   procedure Poll_Writer
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean;
       Is_Broken : out Boolean)
   is
   begin
      Can_Read  := False;
      Can_Write := P.Data_Count /= P.Data'Length;
      Is_Error  := False;
      Is_Broken := False;
   end Poll_Writer;

   function Is_Empty (P : Inner_Acc) return Boolean is
   begin
      return P.Data_Count = 0;
   end Is_Empty;

   procedure Close_Reader (To_Close : in out Inner_Acc) is
   begin
      Lib.Synchronization.Seize (To_Close.Mutex);
      To_Close.Reader_Closed := True;
      Common_Close (To_Close);
   end Close_Reader;

   procedure Close_Writer (To_Close : in out Inner_Acc) is
   begin
      Lib.Synchronization.Seize (To_Close.Mutex);
      To_Close.Writer_Closed := True;
      Common_Close (To_Close);
   end Close_Writer;

   procedure Close (To_Close : in out Inner_Acc) is
   begin
      Close_Reader (To_Close);
      if To_Close /= null and then To_Close.Data /= null then
         Close_Writer (To_Close);
      end if;
   end Close;

   procedure Get_Size (P : Inner_Acc; Size : out Natural) is
   begin
      Lib.Synchronization.Seize (P.Mutex);
      Size := P.Data'Length;
      Lib.Synchronization.Release (P.Mutex);
   end Get_Size;

   procedure Set_Size (P : Inner_Acc; Size : Natural; Success : out Boolean) is
      New_Buffer : Devices.Operation_Data_Acc;
   begin
      Lib.Synchronization.Seize (P.Mutex);
      if Size >= P.Data_Count then
         New_Buffer := new Devices.Operation_Data'(1 .. Size => 0);
         New_Buffer (1 .. P.Data_Count) := P.Data (1 .. P.Data_Count);
         Free (P.Data);
         P.Data  := New_Buffer;
         Success := True;
      else
         Success := False;
      end if;
      Lib.Synchronization.Release (P.Mutex);
   end Set_Size;

   procedure Read
      (To_Read   : Inner_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Pipe_Status)
   is
      Final_Len : Natural := Data'Length;
   begin
      Data := (others => 0);

      if To_Read.Writer_Closed and To_Read.Data_Count = 0 then
         Ret_Count := 0;
         Success   := Pipe_Success;
         return;
      end if;

      if To_Read.Is_Read_Blocking then
         loop
            exit when To_Read.Data_Count /= 0;
            Arch.Snippets.Pause;
         end loop;
      end if;

      Lib.Synchronization.Seize (To_Read.Mutex);
      if not To_Read.Is_Read_Blocking and To_Read.Data_Count = 0 then
         Ret_Count := 0;
         Success   := Would_Block_Failure;
         Lib.Synchronization.Release (To_Read.Mutex);
         return;
      end if;

      if Final_Len > To_Read.Data_Count then
         Final_Len := To_Read.Data_Count;
      end if;

      Data (Data'First .. Data'First + Final_Len - 1) :=
         To_Read.Data (1 .. Final_Len);
      for I in 1 .. Final_Len loop
         for J in To_Read.Data'First .. To_Read.Data'Last - 1 loop
            To_Read.Data (J) := To_Read.Data (J + 1);
         end loop;
         To_Read.Data_Count := To_Read.Data_Count - 1;
      end loop;

      Lib.Synchronization.Release (To_Read.Mutex);
      Ret_Count := Final_Len;
      Success   := Pipe_Success;
   end Read;

   procedure Write
      (To_Write  : Inner_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Pipe_Status)
   is
      Len   : Natural := Data'Length;
      Final : Natural;
   begin
      Lib.Synchronization.Seize (To_Write.Mutex);

      if To_Write.Reader_Closed then
         Ret_Count := 0;
         Success   := Broken_Failure;
         goto Cleanup;
      elsif not To_Write.Is_Write_Blocking and
            To_Write.Data_Count = To_Write.Data'Length
      then
         Ret_Count := 0;
         Success   := Would_Block_Failure;
         goto Cleanup;
      else
         loop
            Lib.Synchronization.Release (To_Write.Mutex);
            Arch.Snippets.Pause;
            Lib.Synchronization.Seize (To_Write.Mutex);
            exit when To_Write.Data_Count /= To_Write.Data'Length;
         end loop;
      end if;

      if Len > To_Write.Data'Length or else
         Len + To_Write.Data_Count > To_Write.Data'Length
      then
         Final := To_Write.Data'Length;
         Len   := To_Write.Data'Length - To_Write.Data_Count;
      else
         Final := To_Write.Data_Count + Len;
      end if;

      To_Write.Data (To_Write.Data_Count + 1 .. Final) :=
         Data (Data'First .. Data'First + Len - 1);
      To_Write.Data_Count := Final;
      Ret_Count := Len;
      Success   := Pipe_Success;

   <<Cleanup>>
      Lib.Synchronization.Release (To_Write.Mutex);
   end Write;
   ----------------------------------------------------------------------------
   procedure Common_Close (To_Close : in out Inner_Acc) is
   begin
      if To_Close.Reader_Closed and To_Close.Writer_Closed then
         Free (To_Close.Data);
         Free (To_Close);
      else
         Lib.Synchronization.Release (To_Close.Mutex);
      end if;
   end Common_Close;
end IPC.FIFO;
