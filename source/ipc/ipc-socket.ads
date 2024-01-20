--  ipc-socket.ads: Socket creation and management.
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

with Lib.Synchronization;
with Devices; use Devices;
with Networking;

package IPC.Socket is
   --  Here lies the implementation of the quintessential POSIX IPC, be it
   --  local IPC or remote internet access.

   type Domain is
      (IPv4,  --  IPv4-based networking.
       IPv6,  --  IPv6-based networking.
       UNIX); --  UNIX-domain sockets for local IPC.

   type DataType is
      (Stream,   --  Connection-based streams. TCP for networked domains.
       Datagram, --  Connection-less datagrams. UDP for networked domains.
       Raw);     --  Raw non-cleaned packets, not always available (UNIX).

   --  Data structures to define a socket.
   type Socket (Dom : Domain; Typ : DataType) is private;
   type Socket_Acc is access Socket;

   --  Returned status of a socket operation.
   type Socket_Status is
      (Plain_Success, --  Unconditional success.
       Is_Bad_Type,   --  A listener is needed but the socket is not one, etc.
       Would_Block);  --  The socket would block, and we don't want that.

   --  Default size in bytes of a socket buffer.
   Default_Socket_Size : constant Natural;
   ----------------------------------------------------------------------------
   --  Socket independent operations.

   --  Create a fresh socket.
   --  @param Dom         Domain of the socket.
   --  @param Typ         Type of socket.
   --  @param Is_Blocking True if blocking, False if not blocking.
   --  @return The socket on success, null on failure.
   function Create
      (Dom         : Domain;
       Typ         : DataType;
       Is_Blocking : Boolean := True) return Socket_Acc;

   --  Get the domain of a socket, which is good to know because data changes.
   --  @param Sock Socket to check.
   --  @return Domain of the socket.
   function Get_Domain (Sock : Socket_Acc) return Domain
      with Pre => Sock /= null;

   --  Get the type of a socket, which is good to know because only some
   --  sockets implement some operations!
   --  @param Sock Socket to check.
   --  @return Data type of the socket.
   function Get_Type (Sock : Socket_Acc) return DataType
      with Pre => Sock /= null;

   --  Close the socket, if possible.
   --  @param To_Close Socket to free and close, will be always set to null.
   procedure Close (To_Close : in out Socket_Acc)
      with Pre => To_Close /= null, Post => To_Close = null;

   --  Get whether the socket is blocking.
   --  @param Sock Socket to check.
   --  @return True if blocking, False if not.
   function Is_Blocking (Sock : Socket_Acc) return Boolean
      with Pre => Sock /= null;

   --  Set whether the socket is blocking.
   --  @param Sock        Socket to modify.
   --  @param Is_Blocking True if blocking, False if not.
   procedure Set_Blocking (Sock : Socket_Acc; Is_Blocking : Boolean)
      with Pre => Sock /= null;

   --  Poll the availability of operations that can be done to a socket.
   --  @param Sock      Socket to operate on.
   --  @param Can_Read  True if a read operation would not block.
   --  @param Can_Write True if a write operation would not block.
   --  @param Is_Broken True if the other end has closed.
   --  @param Is_Error  True if an error condition was triggered.
   procedure Poll
      (Sock      : Socket_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Broken : out Boolean;
       Is_Error  : out Boolean)
      with Pre => Sock /= null;

   --  Read from a socket.
   --  @param Sock      Socket to read from, or its connection.
   --  @param Data      Data to read to.
   --  @param Ret_Count Count of data read.
   --  @param Success   Resulting status of the operation.
   procedure Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
      with Pre => Sock /= null;

   --  Write to a socket.
   --  @param Sock      Socket to write to, or its connection.
   --  @param Data      Data to write.
   --  @param Ret_Count Count of written data.
   --  @param Success   Resulting status of the operation.
   procedure Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
      with Pre => Sock /= null;

   --  Disconnect the socket from all or part of a duplex connection (TX-RX).
   --  When doing this with connection-based sockets will cause in the ceasing
   --  of communication, while doing so with connection-less sockets will cause
   --  on the effects of 'connect' to be forgotten for the passed channel.
   --  @param Sock             Socket to disconnect
   --  @param Do_Receptions    If True, disable future RX for this connection.
   --  @param Do_Transmissions If True, disable future TX for this connection.
   --  @return True on success, False on failure (not connected?).
   function Shutdown
      (Sock             : Socket_Acc;
       Do_Receptions    : Boolean;
       Do_Transmissions : Boolean) return Boolean;

   --  Get whether the socket is listening.
   --  @param Sock Socket to check.
   --  @return True if listening, False if not.
   function Is_Listening (Sock : Socket_Acc) return Boolean
      with Pre => Sock /= null;

   --  Make a socket into a listener, which makes the socket only able of
   --  accepting connections using Accept.
   --  @param Sock    Socket to use for listening, must be stream.
   --  @param Backlog Hint as to how many connections to prepare for.
   --  @return True on success, False on failure.
   function Listen (Sock : Socket_Acc; Backlog : Natural) return Boolean
      with Pre => Sock /= null and then Get_Type (Sock) = Stream;

   --  Accept a new connection, creating a connected socket for interfacing.
   --  @param Sock        Server listening socket to use for accepting.
   --  @param Is_Blocking True to make the accepted socket blocking.
   --  @param Result      New accepted socket, or null on failure.
   procedure Accept_Connection
      (Sock        : Socket_Acc;
       Is_Blocking : Boolean := True;
       Result      : out Socket_Acc)
      with Pre => Sock /= null and then Get_Type (Sock) = Stream;
   ----------------------------------------------------------------------------
   --  IPv4-specific versions of operations, along with domain-specific stuff.
   --  These operations use IPv4 addresses and ports.

   --  Get the address of the passed socket.
   --  @param Sock    Socket to get the address of.
   --  @param Addr    Fetched address.
   --  @param Port    Fetched port.
   --  @param Success True in success, False if not supported / not bound.
   procedure Get_Bound
      (Sock    : Socket_Acc;
       Addr    : out Networking.IPv4_Address;
       Port    : out Networking.IPv4_Port;
       Success : out Boolean)
      with Pre => Sock /= null and then Get_Domain (Sock) = IPv4;

   --  Get the address of the passed socket's peer set with 'connect'.
   --  @param Sock    Socket to get the address of.
   --  @param Addr    Fetched address.
   --  @param Port    Fetched port.
   --  @param Success True in success, False if not supported / not connected.
   procedure Get_Peer
      (Sock    : Socket_Acc;
       Addr    : out Networking.IPv4_Address;
       Port    : out Networking.IPv4_Port;
       Success : out Boolean)
      with Pre => Sock /= null and then Get_Domain (Sock) = IPv4;

   --  Bind a socket to an address.
   --  @param Sock Socket to bind to an address.
   --  @param Addr Fetched address.
   --  @param Port Fetched port.
   --  @return True on success, False on failure.
   function Bind
      (Sock : Socket_Acc;
       Addr : Networking.IPv4_Address;
       Port : Networking.IPv4_Port) return Boolean
      with Pre => Sock /= null and then Get_Domain (Sock) = IPv4;

   --  Connect a socket, if connection-based, the function will do handshake
   --  and all the shinenigans. Connection-less sockets will use from now on
   --  this address only for sending and receiving.
   --  @param Sock Socket to use to connect.
   --  @param Addr Fetched address.
   --  @param Port Fetched port.
   --  @return True on success, False on failure.
   function Connect
      (Sock : Socket_Acc;
       Addr : Networking.IPv4_Address;
       Port : Networking.IPv4_Port) return Boolean
      with Pre => Sock /= null and then Get_Domain (Sock) = IPv4;

   --  Accept a new connection, creating a connected socket for interfacing.
   --  @param Sock         Server listening socket to use for accepting.
   --  @param Is_Blocking  True to make the accepted socket blocking.
   --  @param Peer_Address Address of the connected.
   --  @param Peer_Port    Port of the connected.
   --  @param Result       ew accepted socket, or null on failure.
   procedure Accept_Connection
      (Sock         : Socket_Acc;
       Is_Blocking  : Boolean := True;
       Peer_Address : out Networking.IPv4_Address;
       Peer_Port    : out Networking.IPv4_Port;
       Result       : out Socket_Acc)
      with Pre => Sock /= null             and then
                  Get_Domain (Sock) = IPv4 and then
                  Get_Type (Sock) = Stream;

   --  Read from a connection-less socket.
   --  @param Sock      Socket to read from, or its connection.
   --  @param Data      Data to read to.
   --  @param Ret_Count Count of data read.
   --  @param Addr      IPv4 address to read from.
   --  @param Port      IPv4 port.
   --  @param Success   Resulting status of the operation.
   procedure Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Addr      : Networking.IPv4_Address;
       Port      : Networking.IPv4_Port;
       Success   : out Socket_Status)
      with Pre => Sock /= null             and then
                  Get_Domain (Sock) = IPv4 and then
                  Get_Type (Sock) /= Stream;

   --  Write to a connection-less socket.
   --  @param Sock      Socket to write to, or its connection.
   --  @param Data      Data to write.
   --  @param Ret_Count Count of written data.
   --  @param Addr      IPv4 address to read from.
   --  @param Port      IPv4 port.
   --  @param Success   Resulting status of the operation.
   procedure Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Addr      : Networking.IPv4_Address;
       Port      : Networking.IPv4_Port;
       Success   : out Socket_Status)
      with Pre => Sock /= null             and then
                  Get_Domain (Sock) = IPv4 and then
                  Get_Type (Sock) /= Stream;
   ----------------------------------------------------------------------------
   --  IPv6-specific versions of operations, along with domain-specific stuff.
   --  These operations use IPv6 addresses and ports.

   --  Get the address of the passed socket.
   --  @param Sock    Socket to get the address of.
   --  @param Addr    Fetched address.
   --  @param Port    Fetched port.
   --  @param Success True in success, False if not supported / not bound.
   procedure Get_Bound
      (Sock    : Socket_Acc;
       Addr    : out Networking.IPv6_Address;
       Port    : out Networking.IPv6_Port;
       Success : out Boolean)
      with Pre => Sock /= null and then Get_Domain (Sock) = IPv4;

   --  Get the address of the passed socket's peer set with 'connect'.
   --  @param Sock    Socket to get the address of.
   --  @param Addr    Fetched address.
   --  @param Port    Fetched port.
   --  @param Success True in success, False if not supported / not connected.
   procedure Get_Peer
      (Sock    : Socket_Acc;
       Addr    : out Networking.IPv6_Address;
       Port    : out Networking.IPv6_Port;
       Success : out Boolean)
      with Pre => Sock /= null and then Get_Domain (Sock) = IPv6;

   --  Bind a socket to an address.
   --  @param Sock Socket to bind to an address.
   --  @param Addr Fetched address.
   --  @param Port Fetched port.
   --  @return True on success, False on failure.
   function Bind
      (Sock : Socket_Acc;
       Addr : Networking.IPv6_Address;
       Port : Networking.IPv6_Port) return Boolean
      with Pre => Sock /= null and then Get_Domain (Sock) = IPv6;

   --  Connect a socket, if connection-based, the function will do handshake
   --  and all the shinenigans. Connection-less sockets will use from now on
   --  this address only for sending and receiving.
   --  @param Sock Socket to use to connect.
   --  @param Addr Fetched address.
   --  @param Port Fetched port.
   --  @return True on success, False on failure.
   function Connect
      (Sock : Socket_Acc;
       Addr : Networking.IPv6_Address;
       Port : Networking.IPv6_Port) return Boolean
      with Pre => Sock /= null and then Get_Domain (Sock) = IPv6;

   --  Accept a new connection, creating a connected socket for interfacing
   --  with it. If blocking, the operation will block.
   --  @param Sock         Server listening socket to use for accepting.
   --  @param Is_Blocking  True to make the accepted socket blocking.
   --  @param Peer_Address Address of the connected.
   --  @param Peer_Port    Port of the connected.
   --  @param Result       ew accepted socket, or null on failure.
   procedure Accept_Connection
      (Sock         : Socket_Acc;
       Is_Blocking  : Boolean := True;
       Peer_Address : out Networking.IPv6_Address;
       Peer_Port    : out Networking.IPv6_Port;
       Result       : out Socket_Acc)
      with Pre => Sock /= null             and then
                  Get_Domain (Sock) = IPv6 and then
                  Get_Type (Sock) = Stream;

   --  Read from a connection-less socket.
   --  @param Sock      Socket to read from, or its connection.
   --  @param Data      Data to read to.
   --  @param Ret_Count Count of data read.
   --  @param Addr      IPv6 address to read from.
   --  @param Port      IPv6 port.
   --  @param Success   Resulting status of the operation.
   procedure Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Addr      : Networking.IPv6_Address;
       Port      : Networking.IPv6_Port;
       Success   : out Socket_Status)
      with Pre => Sock /= null             and then
                  Get_Domain (Sock) = IPv6 and then
                  Get_Type (Sock) /= Stream;

   --  Write to a socket.
   --  @param Sock      Socket to write to, or its connection.
   --  @param Data      Data to write.
   --  @param Ret_Count Count of written data.
   --  @param Addr      IPv6 address to read from.
   --  @param Port      IPv6 port.
   --  @param Success   Resulting status of the operation.
   procedure Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Addr      : Networking.IPv6_Address;
       Port      : Networking.IPv6_Port;
       Success   : out Socket_Status)
      with Pre => Sock /= null             and then
                  Get_Domain (Sock) = IPv6 and then
                  Get_Type (Sock) /= Stream;
   ----------------------------------------------------------------------------
   --  UNIX-specific versions of operations, along with domain-specific stuff.
   --  These operations use paths.

   --  Get the address of the passed socket. Since only UNIX sockets
   --  are supported, these functions handle paths.
   --  @param Sock    Socket to get the address of.
   --  @param Path    Buffer to write the fetched path.
   --  @param Length  Output path length, may be bigger if not fit.
   --  @param Success True in success, False if not supported / not bound.
   procedure Get_Bound
      (Sock    : Socket_Acc;
       Path    : out String;
       Length  : out Natural;
       Success : out Boolean)
      with Pre => Sock /= null and then Get_Domain (Sock) = UNIX;

   --  Get the address of the passed socket's peer.
   --  @param Sock    Socket to get the address of.
   --  @param Path    Buffer to write the fetched path.
   --  @param Length  Output path length, may be bigger if not fit.
   --  @param Success True in success, False if not supported / not connected.
   procedure Get_Peer
      (Sock    : Socket_Acc;
       Path    : out String;
       Length  : out Natural;
       Success : out Boolean)
      with Pre => Sock /= null and then Get_Domain (Sock) = UNIX;

   --  Bind a socket to an address, since we only support UNIX sockets, this
   --  takes the shape of a path, but will probably change in the future.
   --  @param Sock Socket to bind to an address.
   --  @param Path Path to bind the socket to, must be unique.
   --  @return True on success, False on failure.
   function Bind (Sock : Socket_Acc; Path : String) return Boolean
      with Pre => Sock /= null and then Get_Domain (Sock) = UNIX;

   --  Connect a socket to a socket previously bound with Bind.
   --  This process initiates the handshake that the other end will continue
   --  with its functions Listen and Accept, unless blocking, this function
   --  will block until accepted.
   --  @param Sock Socket to use to connect.
   --  @param Path Previously bound path to connect to.
   --  @return True on success, False on failure.
   function Connect (Sock : Socket_Acc; Path : String) return Boolean
      with Pre => Sock /= null and then Get_Domain (Sock) = UNIX;

   --  Accept a new connection, creating a connected socket for interfacing
   --  with it. If blocking, the operation will block.
   --  @param Sock                Server listening socket to use for accepting.
   --  @param Is_Blocking         True to make the accepted socket blocking.
   --  @param Peer_Address        Address of the connected socket.
   --  @param Peer_Address_Length Stored length, may be longer if doesnt fit.
   --  @param Result              New accepted socket, or null on failure.
   procedure Accept_Connection
      (Sock                : Socket_Acc;
       Is_Blocking         : Boolean := True;
       Peer_Address        : out String;
       Peer_Address_Length : out Natural;
       Result              : out Socket_Acc)
      with Pre => Sock /= null             and then
                  Get_Domain (Sock) = UNIX and then
                  Get_Type (Sock) = Stream;

   --  Read from a connection-less socket.
   --  @param Sock      Socket to read from, or its connection.
   --  @param Data      Data to read to.
   --  @param Ret_Count Count of data read.
   --  @param Path      Path to read from.
   --  @param Success   Resulting status of the operation.
   procedure Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Path      : String;
       Success   : out Socket_Status)
      with Pre => Sock /= null             and then
                  Get_Domain (Sock) = IPv6 and then
                  Get_Type (Sock) /= Stream;

   --  Write to a socket.
   --  @param Sock      Socket to write to, or its connection.
   --  @param Data      Data to write.
   --  @param Ret_Count Count of written data.
   --  @param Path      Path to write to.
   --  @param Success   Resulting status of the operation.
   procedure Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Path      : String;
       Success   : out Socket_Status)
      with Pre => Sock /= null             and then
                  Get_Domain (Sock) = IPv6 and then
                  Get_Type (Sock) /= Stream;

private

   Default_Socket_Size : constant Natural := 16#2000#;

   type Socket (Dom : Domain; Typ : DataType) is record
      Mutex       : aliased Lib.Synchronization.Binary_Semaphore;
      Is_Blocking : Boolean;

      case Dom is
         when UNIX =>
            Data        : Devices.Operation_Data (1 .. Default_Socket_Size);
            Data_Length : Natural range 0 .. Default_Socket_Size;
            case Typ is
               when Stream =>
                  Is_Listener    : Boolean;
                  Connected      : Socket_Acc;
                  Pending_Accept : Socket_Acc;
                  Established    : Socket_Acc;
               when others =>
                  Simple_Connected : Socket_Acc;
            end case;
         when IPv4 =>
            case Typ is
               when Raw =>
                  IPv4_Cached_Address : Networking.IPv4_Address;
                  IPv4_Cached_Port    : Networking.IPv4_Port;
               when others =>
                  null;
            end case;
         when IPv6 =>
            case Typ is
               when Raw =>
                  IPv6_Cached_Address : Networking.IPv6_Address;
                  IPv6_Cached_Port    : Networking.IPv6_Port;
               when others =>
                  null;
            end case;
      end case;
   end record;
   ----------------------------------------------------------------------------
   --  IPv4 functions.
   procedure Inner_IPv4_Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status);

   procedure Inner_IPv4_Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status);
   ----------------------------------------------------------------------------
   --  IPv6 functions.
   procedure Inner_IPv6_Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status);

   procedure Inner_IPv6_Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status);
   ----------------------------------------------------------------------------
   --  UNIX socket fun.
   Bind_Path_Max : constant := 100;
   type Bound_Socket is record
      Sock     : Socket_Acc;
      Path     : String (1 .. Bind_Path_Max);
      Path_Len : Natural range 1 .. Bind_Path_Max;
   end record;

   UNIX_Bound_Mutex   : aliased Lib.Synchronization.Binary_Semaphore :=
      Lib.Synchronization.Unlocked_Semaphore;
   UNIX_Bound_Sockets : array (1 .. 10) of Bound_Socket :=
      (others => (Sock => null, Path => (others => ' '), Path_Len => 1));

   function Get_Bound (Path : String) return Socket_Acc;

   procedure Inner_UNIX_Close (To_Close : Socket_Acc);

   procedure Inner_UNIX_Read
      (Sock      : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status);

   procedure Inner_UNIX_Write
      (Sock      : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status);

   procedure Inner_UNIX_Poll
      (Sock      : Socket_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Broken : out Boolean;
       Is_Error  : out Boolean);

   function Inner_UNIX_Shutdown
      (Sock             : Socket_Acc;
       Do_Receptions    : Boolean;
       Do_Transmissions : Boolean) return Boolean;
end IPC.Socket;
