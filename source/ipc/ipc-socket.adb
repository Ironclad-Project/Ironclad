--  ipc-socket.adb: Socket creation and management.
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

package body IPC.Socket with SPARK_Mode => Off is
   procedure Free is new Ada.Unchecked_Deallocation (Socket, Socket_Acc);

   function Create
      (Dom   : Domain;
       Typ   : DataType;
       Proto : Protocol) return Socket_Acc
   is
   begin
      if Typ /= Stream then
         return null;
      end if;

      return new Socket'
         (Mutex          => Lib.Synchronization.Unlocked_Semaphore,
          Inner_Domain   => Dom,
          Inner_Type     => Typ,
          Inner_Protocol => Proto,
          Is_Listener    => False,
          Is_Blocking    => False,
          Connected      => null,
          Pending_Accept => null,
          Data           => (others => 0));
   end Create;

   procedure Close (To_Close : in out Socket_Acc) is
   begin
      Lib.Synchronization.Seize (To_Close.Mutex);
      Lib.Synchronization.Seize (UNIX_Bound_Mutex);
      for Ent of UNIX_Bound_Sockets loop
         if Ent.Sock = To_Close then
            Ent.Sock := null;
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (UNIX_Bound_Mutex);
      Free (To_Close);
   end Close;

   function Is_Blocking (P : Socket_Acc) return Boolean is
      Result : Boolean;
   begin
      Lib.Synchronization.Seize (P.Mutex);
      Result := P.Is_Blocking;
      Lib.Synchronization.Release (P.Mutex);
      return Result;
   end Is_Blocking;

   procedure Set_Blocking (P : Socket_Acc; B : Boolean) is
   begin
      Lib.Synchronization.Seize (P.Mutex);
      P.Is_Blocking := B;
      Lib.Synchronization.Release (P.Mutex);
   end Set_Blocking;

   function Bind (Sock : Socket_Acc; Path : String) return Boolean is
      Success : Boolean := False;
   begin
      if Path'Length = 0 or Path'Length > Bind_Path_Max then
         return False;
      end if;

      Lib.Synchronization.Seize (UNIX_Bound_Mutex);

      for Ent of UNIX_Bound_Sockets loop
         if Ent.Sock = Sock then
            goto Cleanup;
         end if;
      end loop;
      for Ent of UNIX_Bound_Sockets loop
         if Ent.Sock = null then
            Ent.Sock := Sock;
            Ent.Path (1 .. Path'Length) := Path;
            Ent.Path_Len := Path'Length;
            Success := True;
            goto Cleanup;
         end if;
      end loop;

   <<Cleanup>>
      Lib.Synchronization.Release (UNIX_Bound_Mutex);
      return Success;
   end Bind;

   function Connect (Sock : Socket_Acc; Path : String) return Boolean is
      To_Connect : Socket_Acc;
      Success    : Boolean;
   begin
      Lib.Synchronization.Seize (UNIX_Bound_Mutex);
      To_Connect := Get_Bound (Path);
      if To_Connect = null then
         Success := False;
         goto End_Return;
      end if;

      Sock.Connected            := To_Connect;
      To_Connect.Pending_Accept := Sock;
      Success                   := True;

   <<End_Return>>
      Lib.Synchronization.Release (UNIX_Bound_Mutex);
      return Success;
   end Connect;

   function Listen (Sock : Socket_Acc; Backlog : Natural) return Boolean is
      pragma Unreferenced (Backlog);
   begin
      Lib.Synchronization.Seize (Sock.Mutex);
      Sock.Is_Listener := True;
      Lib.Synchronization.Release (Sock.Mutex);
      return True;
   end Listen;

   function Accept_Connection (Sock : Socket_Acc) return Socket_Acc is
      pragma Unreferenced (Sock);
   begin
      return null;
   end Accept_Connection;

   procedure Read
      (To_Read   : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
   is
   begin
      if To_Read.Is_Listener then
         Data      := (others => 0);
         Ret_Count := 0;
         Success   := Is_Bad_Type;
      else
         Data      := (others => 0);
         Ret_Count := 0;
         Success   := Plain_Success;
      end if;
   end Read;

   procedure Write
      (To_Write  : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
   is
      pragma Unreferenced (Data);
   begin
      if To_Write.Is_Listener then
         Ret_Count := 0;
         Success   := Is_Bad_Type;
      else
         Ret_Count := 0;
         Success   := Plain_Success;
      end if;
   end Write;

   function Get_Bound (Path : String) return Socket_Acc is
   begin
      for Ent of UNIX_Bound_Sockets loop
         if Ent.Sock /= null and then Ent.Path (1 .. Ent.Path_Len) = Path then
            return Ent.Sock;
         end if;
      end loop;
      return null;
   end Get_Bound;
end IPC.Socket;
