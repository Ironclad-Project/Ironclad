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
with Scheduler;

package body IPC.Socket with SPARK_Mode => Off is
   procedure Free is new Ada.Unchecked_Deallocation (Socket, Socket_Acc);

   function Create
      (Dom         : Domain;
       Typ         : DataType;
       Proto       : Protocol;
       Is_Blocking : Boolean := True) return Socket_Acc
   is
   begin
      case Typ is
         when Stream =>
            return new Socket'
             (Mutex          => Lib.Synchronization.Unlocked_Semaphore,
              Dom            => Dom,
              Typ            => Stream,
              Proto          => Proto,
              Is_Listener    => False,
              Is_Blocking    => Is_Blocking,
              Connected      => null,
              Pending_Accept => null,
              Established    => null,
              Data           => (others => 0),
              Data_Length    => 0);
         when Datagram =>
            return new Socket'
             (Mutex            => Lib.Synchronization.Unlocked_Semaphore,
              Dom              => Dom,
              Typ              => Datagram,
              Proto            => Proto,
              Is_Blocking      => Is_Blocking,
              Simple_Connected => null,
              Data             => (others => 0),
              Data_Length      => 0);
      end case;
   end Create;

   function Get_Type (Sock : Socket_Acc) return DataType is
   begin
      return Sock.Typ;
   end Get_Type;

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

   function Is_Blocking (Sock : Socket_Acc) return Boolean is
      Result : Boolean;
   begin
      Lib.Synchronization.Seize (Sock.Mutex);
      Result := Sock.Is_Blocking;
      Lib.Synchronization.Release (Sock.Mutex);
      return Result;
   end Is_Blocking;

   procedure Set_Blocking (Sock : Socket_Acc; Is_Blocking : Boolean) is
   begin
      Lib.Synchronization.Seize (Sock.Mutex);
      Sock.Is_Blocking := Is_Blocking;
      Lib.Synchronization.Release (Sock.Mutex);
   end Set_Blocking;

   procedure Poll
      (Sock      : Socket_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Broken : out Boolean;
       Is_Error  : out Boolean)
   is
   begin
      case Sock.Typ is
         when Stream =>
            if Sock.Is_Listener or Sock.Pending_Accept /= null then
               Can_Read  := True;
               Can_Write := True;
               Is_Broken := False;
               Is_Error  := False;
            else
               Can_Read  := Sock.Data_Length /= 0;
               Can_Write := Sock.Established /= null;
               Is_Broken := Sock.Established = null;
               Is_Error  := False;
            end if;
         when Datagram =>
            Can_Read  := Sock.Data_Length      /= 0;
            Can_Write := Sock.Simple_Connected /= null;
            Is_Broken := Sock.Simple_Connected = null;
            Is_Error  := False;
      end case;
   end Poll;

   procedure Get_Bound
      (Sock    : Socket_Acc;
       Path    : out String;
       Length  : out Natural;
       Success : out Boolean)
   is
   begin
      Path    := (others => ' ');
      Length  := 0;
      Success := False;

      Lib.Synchronization.Seize (UNIX_Bound_Mutex);
      for Ent of UNIX_Bound_Sockets loop
         if Ent.Sock = Sock then
            Path (Path'First .. Path'First + Ent.Path_Len - 1) :=
               Ent.Path (1 .. Ent.Path_Len);
            Length  := Ent.Path_Len;
            Success := True;
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (UNIX_Bound_Mutex);
   end Get_Bound;

   procedure Get_Peer
      (Sock    : Socket_Acc;
       Path    : out String;
       Length  : out Natural;
       Success : out Boolean)
   is
   begin
      Get_Bound (Sock.Established.Pending_Accept, Path, Length, Success);
   end Get_Peer;

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

      case Sock.Typ is
         when Stream =>
            Sock.Connected := To_Connect;
            loop
               if To_Connect.Pending_Accept = null then
                  To_Connect.Pending_Accept := Sock;
                  exit;
               end if;
               Scheduler.Yield_If_Able;
            end loop;

            loop
               if Sock.Established /= null then
                  exit;
               end if;
               Scheduler.Yield_If_Able;
            end loop;
         when Datagram =>
            Sock.Simple_Connected := To_Connect;
      end case;

      Success := True;

   <<End_Return>>
      Lib.Synchronization.Release (UNIX_Bound_Mutex);
      return Success;
   end Connect;

   function Shutdown
      (Sock             : Socket_Acc;
       Do_Receptions    : Boolean;
       Do_Transmissions : Boolean) return Boolean
   is
      pragma Unreferenced (Do_Receptions);
      pragma Unreferenced (Do_Transmissions);
   begin
      Sock.Established.Established := null;
      Sock.Established             := null;
      return True;
   end Shutdown;

   function Listen (Sock : Socket_Acc; Backlog : Natural) return Boolean is
      pragma Unreferenced (Backlog);
   begin
      Lib.Synchronization.Seize (Sock.Mutex);
      Sock.Is_Listener := True;
      Lib.Synchronization.Release (Sock.Mutex);
      return True;
   end Listen;

   procedure Accept_Connection
      (Sock                : Socket_Acc;
       Is_Blocking         : Boolean := True;
       Peer_Address        : out String;
       Peer_Address_Length : out Natural;
       Result              : out Socket_Acc)
   is
   begin
      Peer_Address        := (others => ' ');
      Peer_Address_Length := 0;

      Lib.Synchronization.Seize (Sock.Mutex);

      if Sock.Is_Listener then
         loop
            if Sock.Pending_Accept /= null then
               Result := Create
                  (Sock.Pending_Accept.Dom,
                   Sock.Pending_Accept.Typ,
                   Sock.Pending_Accept.Proto,
                   Is_Blocking);

               Sock.Pending_Accept.Established := Result;
               Result.Established := Sock.Pending_Accept;
               Result.Pending_Accept := Sock;
               Sock.Pending_Accept := null;
               exit;
            end if;

            if not Sock.Is_Blocking then
               exit;
            end if;
            Scheduler.Yield_If_Able;
         end loop;
      else
         Result := null;
      end if;

      Lib.Synchronization.Release (Sock.Mutex);
   end Accept_Connection;

   procedure Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
   is
   begin
      if Sock.Is_Blocking then
         while Sock.Data_Length = 0 loop
            Scheduler.Yield_If_Able;
         end loop;
      end if;

      case Sock.Typ is
         when Stream =>
            if Sock.Is_Listener or Sock.Established = null then
               Data      := (others => 0);
               Ret_Count := 0;
               Success   := Is_Bad_Type;
               return;
            elsif Sock.Data_Length = 0 then
               Data      := (others => 0);
               Ret_Count := 0;
               Success   := Would_Block;
               return;
            end if;

            if Sock.Data_Length <= Data'Length then
               Data (1 .. Sock.Data_Length) :=
                  Sock.Data (1 .. Sock.Data_Length);
               Sock.Data_Length := 0;
               Ret_Count        := Sock.Data_Length;
            else
               Data             := Sock.Data (1 .. Data'Length);
               Sock.Data_Length := Sock.Data_Length - Data'Length;
               Ret_Count        := Data'Length;
            end if;
            Success := Plain_Success;
         when Datagram =>
            if Sock.Data_Length <= Data'Length then
               Data (1 .. Sock.Data_Length) :=
                  Sock.Data (1 .. Sock.Data_Length);
               Ret_Count := Sock.Data_Length;
               Success   := Plain_Success;
            else
               Ret_Count := 0;
               Success   := Would_Block;
            end if;
      end case;
   end Read;

   procedure Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
   is
   begin
      case Sock.Typ is
         when Stream =>
            if Sock.Is_Listener or Sock.Established = null then
               Ret_Count := 0;
               Success   := Is_Bad_Type;
               return;
            end if;

            Sock.Established.Data (1 .. Data'Length) := Data;
            Sock.Established.Data_Length := Data'Length;
            Ret_Count := Data'Length;
            Success   := Plain_Success;
         when Datagram =>
            if Sock.Simple_Connected.Data_Length = 0 then
               Sock.Simple_Connected.Data (1 .. Data'Length) := Data;
               Sock.Simple_Connected.Data_Length := Data'Length;
               Ret_Count := Data'Length;
               Success   := Plain_Success;
            else
               Ret_Count := 0;
               Success   := Would_Block;
            end if;
      end case;
   end Write;
   ----------------------------------------------------------------------------
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
