--  ipc-socket.adb: Socket creation and management.
--  Copyright (C) 2025 streaksu
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
with Networking.IPv4;
with Networking.IPv6;
with Networking.Interfaces;

package body IPC.Socket with SPARK_Mode => Off is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   procedure Free is new Ada.Unchecked_Deallocation (Socket, Socket_Acc);
   procedure Free is new Ada.Unchecked_Deallocation
      (Operation_Data, Operation_Data_Acc);

   function Create (Dom : Domain; Kind : DataType) return Socket_Acc is
   begin
      case Dom is
         when IPv4 =>
            case Kind is
               when Raw =>
                  return new Socket'
                   (Mutex        => Synchronization.Unlocked_Mutex,
                    Dom          => IPv4,
                    Kind          => Raw,
                    IPv4_Cached_Address => [others => 0],
                    IPv4_Cached_Port => 0);
               when others =>
                  return null;
            end case;
         when IPv6 =>
            case Kind is
               when Raw =>
                  return new Socket'
                   (Mutex        => Synchronization.Unlocked_Mutex,
                    Dom          => IPv6,
                    Kind          => Raw,
                    IPv6_Cached_Address => [others => 0],
                    IPv6_Cached_Port => 0);
               when others =>
                  return null;
            end case;
         when UNIX =>
            case Kind is
               when Stream =>
                  return new Socket'
                   (Mutex          => Synchronization.Unlocked_Mutex,
                    Dom            => UNIX,
                    Kind            => Stream,
                    Has_Credentials => False,
                    Cred_PID => 0,
                    Cred_UID => 0,
                    Cred_GID => 0,
                    Is_Listener    => False,
                    Connected      => null,
                    Pending_Accept => null,
                    Established    => null,
                    Data           => <>,
                    Data_Length    => 0);
               when Datagram =>
                  return new Socket'
                   (Mutex            => Synchronization.Unlocked_Mutex,
                    Dom              => UNIX,
                    Kind              => Datagram,
                    Has_Credentials => False,
                    Cred_PID => 0,
                    Cred_UID => 0,
                    Cred_GID => 0,
                    Simple_Connected => null,
                    Data             => <>,
                    Data_Length      => 0);
               when Raw =>
                  return null;
            end case;
      end case;
   end Create;

   function Get_Domain (Sock : Socket_Acc) return Domain is
   begin
      return Sock.Dom;
   end Get_Domain;

   function Get_Type (Sock : Socket_Acc) return DataType is
   begin
      return Sock.Kind;
   end Get_Type;

   procedure Close (To_Close : in out Socket_Acc) is
   begin
      Synchronization.Seize (To_Close.Mutex);
      case To_Close.Dom is
         when IPv4 | IPv6 =>
            null;
         when UNIX =>
            Inner_UNIX_Close (To_Close);
      end case;
      Free (To_Close);
   end Close;

   procedure Poll
      (Sock      : Socket_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Broken : out Boolean;
       Is_Error  : out Boolean)
   is
   begin
      Synchronization.Seize (Sock.Mutex);
      case Sock.Dom is
         when IPv4 | IPv6 =>
            Can_Read  := False;
            Can_Write := False;
            Is_Broken := False;
            Is_Error  := False;
         when UNIX =>
            Inner_UNIX_Poll (Sock, Can_Read, Can_Write, Is_Broken, Is_Error);
      end case;
      Synchronization.Release (Sock.Mutex);
   end Poll;

   procedure Read
      (Sock        : Socket_Acc;
       Data        : out Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Socket_Status)
   is
   begin
      case Sock.Dom is
         when IPv4 =>
            Inner_IPv4_Read (Sock, Data, Ret_Count, Success);
         when IPv6 =>
            Inner_IPv6_Read (Sock, Data, Ret_Count, Success);
         when UNIX =>
            Inner_UNIX_Read (Sock, Data, Is_Blocking, Ret_Count, Success);
      end case;
   end Read;

   procedure Write
      (Sock        : Socket_Acc;
       Data        : Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Socket_Status)
   is
   begin
      case Sock.Dom is
         when IPv4 =>
            Inner_IPv4_Write (Sock, Data, Ret_Count, Success);
         when IPv6 =>
            Inner_IPv6_Write (Sock, Data, Ret_Count, Success);
         when UNIX =>
            Inner_UNIX_Write (Sock, Data, Is_Blocking, Ret_Count, Success);
      end case;
   end Write;

   procedure Shutdown
      (Sock             : Socket_Acc;
       Do_Receptions    : Boolean;
       Do_Transmissions : Boolean;
       Success          : out Boolean)
   is
   begin
      Synchronization.Seize (Sock.Mutex);
      case Sock.Dom is
         when IPv4 | IPv6 =>
            Success := False;
         when UNIX =>
            Inner_UNIX_Shutdown
               (Sock, Do_Receptions, Do_Transmissions, Success);
      end case;
      Synchronization.Release (Sock.Mutex);
   end Shutdown;

   procedure Is_Listening (Sock : Socket_Acc; Is_Listening : out Boolean) is
   begin
      Synchronization.Seize (Sock.Mutex);
      Is_Listening := Sock.Is_Listener;
      Synchronization.Release (Sock.Mutex);
   end Is_Listening;

   procedure Listen
      (Sock    : Socket_Acc;
       Backlog : Natural;
       Success : out Boolean)
   is
      pragma Unreferenced (Backlog);
   begin
      Synchronization.Seize (Sock.Mutex);
      Sock.Is_Listener := True;
      Synchronization.Release (Sock.Mutex);
      Success := True;
   end Listen;

   procedure Accept_Connection
      (Sock          : Socket_Acc;
       Is_Blocking   : Boolean := True;
       PID, UID, GID : Unsigned_32;
       Result        : out Socket_Acc)
   is
      Path : String (1 .. 0);
      Len  : Natural;
   begin
      Synchronization.Seize (Sock.Mutex);
      case Sock.Dom is
         when IPv4 | IPv6 =>
            Result := null;
         when UNIX =>
            Accept_Connection
               (Sock, Is_Blocking, Path, Len, PID, UID, GID, Result);
      end case;
      Synchronization.Release (Sock.Mutex);
   end Accept_Connection;

   procedure Pipe_Socket (Sock : Socket_Acc; Result : out Socket_Acc) is
   begin
      Synchronization.Seize (Sock.Mutex);
      case Sock.Dom is
         when IPv4 | IPv6 =>
            Result := null;
         when UNIX =>
            Direct_Connection (Sock, Result);
      end case;
      Synchronization.Release (Sock.Mutex);
   end Pipe_Socket;
   ----------------------------------------------------------------------------
   procedure Get_Bound
      (Sock    : Socket_Acc;
       Addr    : out Networking.IPv4_Address;
       Port    : out Networking.IPv4_Port;
       Success : out Boolean)
   is
      pragma Unreferenced (Sock);
   begin
      Addr := [others => 0];
      Port := 0;
      Success := False;
   end Get_Bound;

   procedure Get_Peer
      (Sock    : Socket_Acc;
       Addr    : out Networking.IPv4_Address;
       Port    : out Networking.IPv4_Port;
       Success : out Boolean)
   is
      pragma Unreferenced (Sock);
   begin
      Addr := [others => 0];
      Port := 0;
      Success := False;
   end Get_Peer;

   function Bind
      (Sock : Socket_Acc;
       Addr : Networking.IPv4_Address;
       Port : Networking.IPv4_Port) return Boolean
   is
      pragma Unreferenced (Sock);
      pragma Unreferenced (Addr);
      pragma Unreferenced (Port);
   begin
      return False;
   end Bind;

   procedure Connect
      (Sock    : Socket_Acc;
       Addr    : Networking.IPv4_Address;
       Port    : Networking.IPv4_Port;
       Success : out Boolean)
   is
   begin
      case Sock.Kind is
         when Raw =>
            Sock.IPv4_Cached_Address := Addr;
            Sock.IPv4_Cached_Port := Port;
            Success := True;
         when others =>
            Success := False;
      end case;
   end Connect;

   procedure Accept_Connection
      (Sock         : Socket_Acc;
       Is_Blocking  : Boolean := True;
       Peer_Address : out Networking.IPv4_Address;
       Peer_Port    : out Networking.IPv4_Port;
       Result       : out Socket_Acc)
   is
      pragma Unreferenced (Sock);
      pragma Unreferenced (Is_Blocking);
   begin
      Peer_Address := [others => 0];
      Peer_Port := 0;
      Result := null;
   end Accept_Connection;

   procedure Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Addr      : Networking.IPv4_Address;
       Port      : Networking.IPv4_Port;
       Success   : out Socket_Status)
   is
      pragma Unreferenced (Port);

      Succ : Devices.Dev_Status;
      Dev  : Devices.Device_Handle;
      Src  : Networking.IPv4_Address;
      Hdr_Size : constant Natural := Networking.IPv4.IPv4_Packet_Header'Size;
   begin
      case Sock.Kind is
         when Raw =>
            Networking.Interfaces.Get_Suitable_Interface (Addr, Dev);
            if Dev = Devices.Error_Handle then
               Data      := [others => 0];
               Ret_Count := 0;
               Success   := Would_Block;
               return;
            end if;
            Networking.Interfaces.Get_Interface_Address (Dev, Src);

            Devices.Read (Dev, 0, Data, Ret_Count, Succ);
            Data (Data'First .. Data'Last - (Hdr_Size / 8)) :=
               Data (Data'First + (Hdr_Size / 8) .. Data'Last);
            Ret_Count := Ret_Count - (Hdr_Size / 8);
            if Succ = Devices.Dev_Success then
               Success := Plain_Success;
            else
               Success := Would_Block;
            end if;
         when others =>
            Data := [others => 0];
            Ret_Count := 0;
            Success := Is_Bad_Type;
      end case;
   end Read;

   procedure Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Addr      : Networking.IPv4_Address;
       Port      : Networking.IPv4_Port;
       Success   : out Socket_Status)
   is
      pragma Unreferenced (Port);

      Succ : Devices.Dev_Status;
      Dev  : Devices.Device_Handle;
      Tmp_Data : Operation_Data_Acc;
      Src  : Networking.IPv4_Address;
      Hdr  : Networking.IPv4.IPv4_Packet_Header;

      Hdr_Data : Operation_Data (1 .. Hdr'Size / 8)
         with Import, Address => Hdr'Address;
   begin
      case Sock.Kind is
         when Raw =>
            Networking.Interfaces.Get_Suitable_Interface (Addr, Dev);
            if Dev = Devices.Error_Handle then
               Ret_Count := 0;
               Success   := Would_Block;
               return;
            end if;
            Networking.Interfaces.Get_Interface_Address (Dev, Src);

            Hdr := Networking.IPv4.Generate_Header (Src, Addr, Data'Length);
            Tmp_Data := new Operation_Data (1 .. Data'Length + (Hdr'Size / 8));
            Tmp_Data (1 .. Hdr_Data'Length) := Hdr_Data;
            Tmp_Data (Hdr_Data'Length + 1 .. Tmp_Data'Last) := Data;

            Devices.Write (Dev, 0, Tmp_Data.all, Ret_Count, Succ);
            Free (Tmp_Data);

            if Succ = Devices.Dev_Success then
               Success := Plain_Success;
            else
               Success := Would_Block;
            end if;
         when others =>
            Ret_Count := 0;
            Success := Is_Bad_Type;
      end case;
   end Write;
   ----------------------------------------------------------------------------
   procedure Get_Bound
      (Sock    : Socket_Acc;
       Addr    : out Networking.IPv6_Address;
       Port    : out Networking.IPv6_Port;
       Success : out Boolean)
   is
      pragma Unreferenced (Sock);
   begin
      Addr := [others => 0];
      Port := 0;
      Success := False;
   end Get_Bound;

   procedure Get_Peer
      (Sock    : Socket_Acc;
       Addr    : out Networking.IPv6_Address;
       Port    : out Networking.IPv6_Port;
       Success : out Boolean)
   is
      pragma Unreferenced (Sock);
   begin
      Addr := [others => 0];
      Port := 0;
      Success := False;
   end Get_Peer;


   function Bind
      (Sock : Socket_Acc;
       Addr : Networking.IPv6_Address;
       Port : Networking.IPv6_Port) return Boolean
   is
      pragma Unreferenced (Sock);
      pragma Unreferenced (Addr);
      pragma Unreferenced (Port);
   begin
      return False;
   end Bind;

   procedure Connect
      (Sock    : Socket_Acc;
       Addr    : Networking.IPv6_Address;
       Port    : Networking.IPv6_Port;
       Success : out Boolean)
   is
   begin
      case Sock.Kind is
         when Raw =>
            Sock.IPv6_Cached_Address := Addr;
            Sock.IPv6_Cached_Port := Port;
            Success := True;
         when others =>
            Success := False;
      end case;
   end Connect;

   procedure Accept_Connection
      (Sock         : Socket_Acc;
       Is_Blocking  : Boolean := True;
       Peer_Address : out Networking.IPv6_Address;
       Peer_Port    : out Networking.IPv6_Port;
       Result       : out Socket_Acc)
   is
      pragma Unreferenced (Sock);
      pragma Unreferenced (Is_Blocking);
   begin
      Peer_Address := [others => 0];
      Peer_Port := 0;
      Result := null;
   end Accept_Connection;

   procedure Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Addr      : Networking.IPv6_Address;
       Port      : Networking.IPv6_Port;
       Success   : out Socket_Status)
   is
      pragma Unreferenced (Port);

      Succ : Devices.Dev_Status;
      Dev  : Devices.Device_Handle;
      Src  : Networking.IPv6_Address;
      Hdr_Size : constant Natural := Networking.IPv6.IPv6_Packet_Header'Size;
   begin
      case Sock.Kind is
         when Raw =>
            Networking.Interfaces.Get_Suitable_Interface (Addr, Dev);
            if Dev = Devices.Error_Handle then
               Data      := [others => 0];
               Ret_Count := 0;
               Success   := Would_Block;
               return;
            end if;
            Networking.Interfaces.Get_Interface_Address (Dev, Src);

            Devices.Read (Dev, 0, Data, Ret_Count, Succ);
            Data (Data'First .. Data'Last - (Hdr_Size / 8)) :=
               Data (Data'First + (Hdr_Size / 8) .. Data'Last);
            Ret_Count := Ret_Count - (Hdr_Size / 8);
            if Succ = Devices.Dev_Success then
               Success := Plain_Success;
            else
               Success := Would_Block;
            end if;
         when others =>
            Ret_Count := 0;
            Success   := Is_Bad_Type;
      end case;
   end Read;

   procedure Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Addr      : Networking.IPv6_Address;
       Port      : Networking.IPv6_Port;
       Success   : out Socket_Status)
   is
      pragma Unreferenced (Port);

      Succ : Devices.Dev_Status;
      Dev  : Devices.Device_Handle;
      Tmp_Data : Operation_Data_Acc;
      Src  : Networking.IPv6_Address;
      Hdr  : Networking.IPv6.IPv6_Packet_Header;

      Hdr_Data : Operation_Data (1 .. Hdr'Size / 8)
         with Import, Address => Hdr'Address;
   begin
      case Sock.Kind is
         when Raw =>
            Networking.Interfaces.Get_Suitable_Interface (Addr, Dev);
            if Dev = Devices.Error_Handle then
               Ret_Count := 0;
               Success   := Would_Block;
               return;
            end if;
            Networking.Interfaces.Get_Interface_Address (Dev, Src);

            Hdr := Networking.IPv6.Generate_Header (Src, Addr, Data'Length);
            Tmp_Data := new Operation_Data (1 .. Data'Length + (Hdr'Size / 8));
            Tmp_Data (1 .. Hdr_Data'Length) := Hdr_Data;
            Tmp_Data (Hdr_Data'Length + 1 .. Tmp_Data'Last) := Data;

            Devices.Write (Dev, 0, Tmp_Data.all, Ret_Count, Succ);
            Free (Tmp_Data);

            if Succ = Devices.Dev_Success then
               Success := Plain_Success;
            else
               Success := Would_Block;
            end if;
         when others =>
            Ret_Count := 0;
            Success   := Is_Bad_Type;
      end case;
   end Write;
   ----------------------------------------------------------------------------
   procedure Get_Bound
      (Sock    : Socket_Acc;
       Path    : out String;
       Length  : out Natural;
       Success : out Boolean)
   is
   begin
      Path    := [others => ' '];
      Length  := 0;
      Success := False;

      Synchronization.Seize (UNIX_Bound_Mutex);
      for Ent of UNIX_Bound_Sockets loop
         if Ent.Sock = Sock then
            Path (Path'First .. Path'First + Ent.Path_Len - 1) :=
               Ent.Path (1 .. Ent.Path_Len);
            Length  := Ent.Path_Len;
            Success := True;
            exit;
         end if;
      end loop;
      Synchronization.Release (UNIX_Bound_Mutex);
   end Get_Bound;

   procedure Get_Peer
      (Sock    : Socket_Acc;
       Path    : out String;
       Length  : out Natural;
       Success : out Boolean)
   is
   begin
      Path    := [others => ' '];
      Length  := 0;
      Success := False;

      Synchronization.Seize (UNIX_Bound_Mutex);
      for Ent of UNIX_Bound_Sockets loop
         if Ent.Sock = Sock.Established then
            if Path'Length >= Ent.Path_Len then
               Path (Path'First .. Path'First + Ent.Path_Len - 1) :=
                  Ent.Path (1 .. Ent.Path_Len);
            else
               Path := Ent.Path (1 .. Path'Length);
            end if;
            Length  := Ent.Path_Len;
            Success := True;
            exit;
         end if;
      end loop;
      Synchronization.Release (UNIX_Bound_Mutex);
   end Get_Peer;

   procedure Bind (Sock : Socket_Acc; Path : String; Success : out Boolean) is
   begin
      Success := False;

      if Path'Length = 0 or Path'Length > Bind_Path_Max then
         return;
      end if;

      Synchronization.Seize (UNIX_Bound_Mutex);

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
      Synchronization.Release (UNIX_Bound_Mutex);
   end Bind;

   procedure Connect
      (Sock    : Socket_Acc;
       Path    : String;
       PID     : Unsigned_32;
       UID     : Unsigned_32;
       GID     : Unsigned_32;
       Success : out Boolean)
   is
      To_Connect : Socket_Acc;
   begin
      Synchronization.Seize (UNIX_Bound_Mutex);

      To_Connect := Get_Bound (Path);
      if To_Connect = null then
         Success := False;
         goto End_Return;
      end if;
      case Sock.Kind is
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
               exit when Sock.Pending_Accept /= null;
               Scheduler.Yield_If_Able;
            end loop;
         when others =>
            Sock.Simple_Connected := To_Connect;
      end case;

      Sock.Has_Credentials := True;
      Sock.Cred_GID := GID;
      Sock.Cred_UID := UID;
      Sock.Cred_PID := PID;
      Success := True;

   <<End_Return>>
      Synchronization.Release (UNIX_Bound_Mutex);
   end Connect;

   procedure Accept_Connection
      (Sock                : Socket_Acc;
       Is_Blocking         : Boolean := True;
       Peer_Address        : out String;
       Peer_Address_Length : out Natural;
       PID, UID, GID       : Unsigned_32;
       Result              : out Socket_Acc)
   is
      Tmp : Socket_Acc;
   begin
      Peer_Address        := [others => ' '];
      Peer_Address_Length := 0;
      Result              := null;

      Synchronization.Seize (Sock.Mutex);

      if Sock.Is_Listener then
         loop
            if Sock.Pending_Accept /= null then
               Result := Create
                  (Sock.Pending_Accept.Dom,
                   Sock.Pending_Accept.Kind);

               Tmp := Sock.Pending_Accept;
               Result.Established := Sock;
               Tmp.Pending_Accept := Result;
               Result.Pending_Accept := Tmp;
               Sock.Pending_Accept := null;

               Result.Has_Credentials := True;
               Result.Cred_GID := GID;
               Result.Cred_UID := UID;
               Result.Cred_PID := PID;
               exit;
            end if;
            exit when not Is_Blocking;
            Scheduler.Yield_If_Able;
         end loop;
      end if;

      Synchronization.Release (Sock.Mutex);
   end Accept_Connection;

   procedure Direct_Connection (Sock : Socket_Acc; Result : out Socket_Acc) is
   begin
      Result := Create (Sock.Dom, Sock.Kind);
      Result.Pending_Accept := Sock;
      Sock.Pending_Accept   := Result;
   end Direct_Connection;

   procedure Read
      (Sock          : Socket_Acc;
       Data          : out Devices.Operation_Data;
       Is_Blocking   : Boolean;
       Ret_Count     : out Natural;
       Path          : String;
       PID, UID, GID : Unsigned_32;
       Success       : out Socket_Status)
   is
      Discard : Boolean;
      Temp : constant Socket_Acc := Sock.Simple_Connected;
   begin
      Connect (Sock, Path, PID, UID, GID, Discard);
      Inner_UNIX_Read (Sock, Data, Is_Blocking, Ret_Count, Success);
      Sock.Simple_Connected := Temp;
   end Read;

   procedure Write
      (Sock          : Socket_Acc;
       Data          : Devices.Operation_Data;
       Is_Blocking   : Boolean;
       Ret_Count     : out Natural;
       Path          : String;
       PID, UID, GID : Unsigned_32;
       Success       : out Socket_Status)
   is
      Discard : Boolean;
      Temp : constant Socket_Acc := Sock.Simple_Connected;
   begin
      Connect (Sock, Path, PID, UID, GID, Discard);
      Inner_UNIX_Write (Sock, Data, Is_Blocking, Ret_Count, Success);
      Sock.Simple_Connected := Temp;
   end Write;

   procedure Get_Peer_Credentials
      (Sock    : Socket_Acc;
       PID     : out Unsigned_32;
       UID     : out Unsigned_32;
       GID     : out Unsigned_32;
       Success : out Socket_Status)
   is
   begin
      PID := 0;
      GID := 0;
      UID := 0;
      Success := Is_Bad_Type;

      Synchronization.Seize (Sock.Mutex);
      if Sock.Pending_Accept /= null then
         Synchronization.Seize (Sock.Pending_Accept.Mutex);
         if Sock.Pending_Accept.Has_Credentials then
            PID := Sock.Pending_Accept.Cred_PID;
            UID := Sock.Pending_Accept.Cred_UID;
            GID := Sock.Pending_Accept.Cred_GID;
            Success := Plain_Success;
         end if;
         Synchronization.Release (Sock.Pending_Accept.Mutex);
      end if;
      Synchronization.Release (Sock.Mutex);
   end Get_Peer_Credentials;
   ----------------------------------------------------------------------------
   procedure Inner_IPv4_Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
   is
   begin
      Read
         (Sock      => Sock,
          Data      => Data,
          Ret_Count => Ret_Count,
          Addr      => Sock.IPv4_Cached_Address,
          Port      => Sock.IPv4_Cached_Port,
          Success   => Success);
   end Inner_IPv4_Read;

   procedure Inner_IPv4_Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
   is
   begin
      Write
         (Sock      => Sock,
          Data      => Data,
          Ret_Count => Ret_Count,
          Addr      => Sock.IPv4_Cached_Address,
          Port      => Sock.IPv4_Cached_Port,
          Success   => Success);
   end Inner_IPv4_Write;
   ----------------------------------------------------------------------------
   procedure Inner_IPv6_Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
   is
   begin
      Read
         (Sock      => Sock,
          Data      => Data,
          Ret_Count => Ret_Count,
          Addr      => Sock.IPv6_Cached_Address,
          Port      => Sock.IPv6_Cached_Port,
          Success   => Success);
   end Inner_IPv6_Read;

   procedure Inner_IPv6_Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
   is
   begin
      Write
         (Sock      => Sock,
          Data      => Data,
          Ret_Count => Ret_Count,
          Addr      => Sock.IPv6_Cached_Address,
          Port      => Sock.IPv6_Cached_Port,
          Success   => Success);
   end Inner_IPv6_Write;
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

   procedure Inner_UNIX_Close (To_Close : Socket_Acc) is
   begin
      Synchronization.Seize (UNIX_Bound_Mutex);
      for Ent of UNIX_Bound_Sockets loop
         if Ent.Sock = To_Close then
            Ent.Sock := null;
            exit;
         end if;
      end loop;
      Synchronization.Release (UNIX_Bound_Mutex);

      if To_Close.Kind = Stream    and then
         not To_Close.Is_Listener and then
         To_Close.Established /= null
      then
         To_Close.Established.Pending_Accept := null;
      end if;
   end Inner_UNIX_Close;

   procedure Inner_UNIX_Read
      (Sock        : Socket_Acc;
       Data        : out Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Socket_Status)
   is
      Len : Natural := Data'Length;
   begin
   <<Retry>>
      if Is_Blocking then
         loop
            Synchronization.Seize (Sock.Mutex);
            exit when Sock.Data_Length /= 0;
            Synchronization.Release (Sock.Mutex);
            Scheduler.Yield_If_Able;
         end loop;
      else
         Synchronization.Seize (Sock.Mutex);
      end if;

      case Sock.Kind is
         when Stream =>
            if Sock.Is_Listener or Sock.Pending_Accept = null then
               Data      := [others => 0];
               Ret_Count := 0;
               Success   := Is_Bad_Type;
               goto Cleanup;
            elsif Sock.Data_Length = 0 then
               if Is_Blocking then
                  Synchronization.Release (Sock.Mutex);
                  goto Retry;
               else
                  Data      := [others => 0];
                  Ret_Count := 0;
                  Success   := Would_Block;
                  goto Cleanup;
               end if;
            end if;

            if Len > Sock.Data_Length then
               Len := Sock.Data_Length;
            end if;
            if Data'First > Natural'Last - Len then
               Len := Natural'Last - Data'First;
            end if;

            Data (Data'First .. Data'First + Len - 1) :=
               Sock.Data (1 .. Len);
            for I in 1 .. Len loop
               for J in Sock.Data'First .. Sock.Data'Last - 1 loop
                  Sock.Data (J) := Sock.Data (J + 1);
               end loop;
               if Sock.Data_Length > 0 then
                  Sock.Data_Length := Sock.Data_Length - 1;
               else
                  exit;
               end if;
            end loop;

            Ret_Count := Len;
            Success := Plain_Success;
         when others =>
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

   <<Cleanup>>
      Synchronization.Release (Sock.Mutex);
   end Inner_UNIX_Read;

   procedure Inner_UNIX_Write
      (Sock        : Socket_Acc;
       Data        : Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Socket_Status)
   is
      Len   : Natural := Data'Length;
      Final : Natural;
   begin
      case Sock.Kind is
         when Stream =>
            if Sock.Is_Listener or Sock.Pending_Accept = null then
               Ret_Count := 0;
               Success   := Is_Bad_Type;
               return;
            end if;

         <<Retry>>
            if Is_Blocking then
               loop
                  Synchronization.Seize (Sock.Pending_Accept.Mutex);
                  exit when Sock.Pending_Accept.Data_Length /=
                     Default_Socket_Size;
                  Synchronization.Release (Sock.Pending_Accept.Mutex);
                  Scheduler.Yield_If_Able;
               end loop;
            else
               Synchronization.Seize (Sock.Pending_Accept.Mutex);
            end if;

            Synchronization.Seize (Sock.Mutex);

            if Sock.Pending_Accept.Data_Length = Default_Socket_Size then
               if Is_Blocking then
                  Synchronization.Release (Sock.Mutex);
                  goto Retry;
               else
                  Ret_Count := 0;
                  Success   := Would_Block;
                  goto Cleanup;
               end if;
            end if;

            if Len > Default_Socket_Size or else
               Len > Default_Socket_Size - Sock.Pending_Accept.Data_Length
            then
               Final := Default_Socket_Size;
               Len   := Default_Socket_Size - Sock.Pending_Accept.Data_Length;
            else
               Final := Sock.Pending_Accept.Data_Length + Len;
            end if;

            Sock.Pending_Accept.Data
               (Sock.Pending_Accept.Data_Length + 1 .. Final) :=
               Data (Data'First .. Data'First + Len - 1);
            Sock.Pending_Accept.Data_Length := Final;

            Ret_Count := Len;
            Success   := Plain_Success;
         <<Cleanup>>
            Synchronization.Release (Sock.Pending_Accept.Mutex);
            Synchronization.Release (Sock.Mutex);
         when others =>
            if Sock.Simple_Connected = null or
               Data'Length <= Default_Socket_Size
            then
               Ret_Count := 0;
               Success   := Would_Block;
               return;
            end if;


            Synchronization.Seize (Sock.Simple_Connected.Mutex);
            if Sock.Simple_Connected.Data_Length = 0 and then
               Data'Length <= Default_Socket_Size
            then
               Sock.Simple_Connected.Data (1 .. Data'Length) := Data;
               Sock.Simple_Connected.Data_Length := Data'Length;
               Ret_Count := Data'Length;
               Success   := Plain_Success;
            else
               Ret_Count := 0;
               Success   := Would_Block;
            end if;
            Synchronization.Release (Sock.Simple_Connected.Mutex);
      end case;
   end Inner_UNIX_Write;

   procedure Inner_UNIX_Poll
      (Sock      : Socket_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Broken : out Boolean;
       Is_Error  : out Boolean)
   is
   begin
      case Sock.Kind is
         when Stream =>
            if Sock.Is_Listener then
               Can_Read  := Sock.Pending_Accept /= null;
               Can_Write := Can_Read;
               Is_Broken := False;
               Is_Error  := False;
            else
               Can_Read  := Sock.Data_Length /= 0;
               Can_Write := Sock.Pending_Accept /= null and then
                  Sock.Pending_Accept.Data_Length /= Default_Socket_Size;
               Is_Broken := False;
               Is_Error  := False;
            end if;
         when others =>
            Can_Read  := Sock.Data_Length      /= 0;
            Can_Write := Sock.Simple_Connected /= null;
            Is_Broken := False;
            Is_Error  := False;
      end case;
   end Inner_UNIX_Poll;

   procedure Inner_UNIX_Shutdown
      (Sock             : Socket_Acc;
       Do_Receptions    : Boolean;
       Do_Transmissions : Boolean;
       Success          : out Boolean)
   is
      pragma Unreferenced (Do_Receptions);
      pragma Unreferenced (Do_Transmissions);
   begin
      if Sock.Established /= null then
         Sock.Established.Established := null;
         Sock.Established             := null;
      end if;
      Success := True;
   end Inner_UNIX_Shutdown;
end IPC.Socket;
