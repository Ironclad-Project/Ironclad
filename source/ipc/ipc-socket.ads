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
with Devices;

package IPC.Socket is
   --  Here lies the implementation of the quintessential POSIX IPC, be it
   --  local IPC or remote internet access.
   --
   --  A socket is defined by a domain, type, and protocol, these are the ones
   --  we support.
   type Domain   is (UNIX);
   type DataType is (Stream, Datagram);
   type Protocol is (Default);

   --  Data structures to define a socket.
   type Socket (Dom : Domain; Typ : DataType; Proto : Protocol) is private;
   type Socket_Acc is access Socket;

   --  Create a fresh socket.
   --  @param Dom         Domain of the socket.
   --  @param Typ         Type of socket.
   --  @param Proto       Socket protocol, or default for default values.
   --  @param Is_Blocking True if blocking, False if not blocking.
   --  @return The socket on success, null on failure.
   function Create
      (Dom         : Domain;
       Typ         : DataType;
       Proto       : Protocol;
       Is_Blocking : Boolean := True) return Socket_Acc;

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
      with Pre => Sock /= null;

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
      with Pre => Sock /= null;

   --  Bind a socket to an address, since we only support UNIX sockets, this
   --  takes the shape of a path, but will probably change in the future.
   --  @param Sock Socket to bind to an address.
   --  @param Path Path to bind the socket to, must be unique.
   --  @return True on success, False on failure.
   function Bind (Sock : Socket_Acc; Path : String) return Boolean
      with Pre => Sock /= null;

   --  Connect a socket to a socket previously bound with Bind.
   --  This process initiates the handshake that the other end will continue
   --  with its functions Listen and Accept, unless blocking, this function
   --  will block until accepted.
   --  @param Sock Socket to use to connect.
   --  @param Path Previously bound path to connect to.
   --  @return True on success, False on failure.
   function Connect (Sock : Socket_Acc; Path : String) return Boolean
      with Pre => Sock /= null;

   --  Disconnect the socket from all or part of a duplex connection (TX-RX).
   --  @param Sock             Socket to disconnect
   --  @param Do_Receptions    If True, disable future RX for this connection.
   --  @param Do_Transmissions If True, disable future TX for this connection.
   --  @return True on success, False on failure (not connected?).
   function Shutdown
      (Sock             : Socket_Acc;
       Do_Receptions    : Boolean;
       Do_Transmissions : Boolean) return Boolean;

   --  Make a socket into a listener, which makes the socket only able of
   --  accepting connections using Accept. Only stream sockets can be used for
   --  this operation, as this operation lacks meaning otherwise.
   --  @param Sock    Socket to use for listening.
   --  @param Backlog Hint as to how many connections to prepare for.
   --  @return True on success, False on failure.
   function Listen (Sock : Socket_Acc; Backlog : Natural) return Boolean
      with Pre => Sock /= null and then Get_Type (Sock) = Stream;

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
      with Pre => Sock /= null and then Get_Type (Sock) = Stream;

   --  Returned status of a socket operation.
   type Socket_Status is
      (Plain_Success, --  Unconditional success.
       Is_Bad_Type,   --  A listener is needed but the socket is not one, etc.
       Would_Block);  --  The socket would block, and we don't want that.

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

private

   Default_Socket_Size : constant := 16#2000#;

   type Socket (Dom : Domain; Typ : DataType; Proto : Protocol) is record
      Mutex       : aliased Lib.Synchronization.Binary_Semaphore;
      Is_Blocking : Boolean;
      Data        : Devices.Operation_Data (1 .. Default_Socket_Size);
      Data_Length : Natural range 0 .. Default_Socket_Size;

      case Typ is
         when Stream =>
            Is_Listener    : Boolean;
            Connected      : Socket_Acc;
            Pending_Accept : Socket_Acc;
            Established    : Socket_Acc;
         when Datagram =>
            Simple_Connected : Socket_Acc;
      end case;
   end record;

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
end IPC.Socket;
