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
with Arch.MMU;

package IPC.Socket is
   --  Implementation for Ironclad of the quintessential epitome of POSIX IPC.
   --  Sockets are to UNIX as ketchup is to mcdonald's fries.

   --  A socket is defined by a domain, type, and protocol, these are the ones
   --  we support.
   type Domain   is (UNIX);
   type DataType is (Stream, Datagram);
   type Protocol is (Default);

   --  Data structures to define a socket.
   type Socket     is private;
   type Socket_Acc is access Socket;

   --  Create a fresh socket. The socket is made blocking by default.
   --  @return The socket on success, null on failure.
   function Create
      (Dom   : Domain;
       Typ   : DataType;
       Proto : Protocol) return Socket_Acc;

   --  Close it possible.
   procedure Close (To_Close : in out Socket_Acc) with Pre => To_Close /= null;

   --  Get and set whether the passed socket is blocking or non blocking.
   function Is_Blocking (P : Socket_Acc) return Boolean with Pre => P /= null;
   procedure Set_Blocking (P : Socket_Acc; B : Boolean) with Pre => P /= null;

   --  Bind a socket to a string (ideally a path).
   --  @return True on success, False on failure.
   function Bind (Sock : Socket_Acc; Path : String) return Boolean
      with Pre => Sock /= null;

   --  Connect a socket to a string previously bound.
   --  @return True on success, False on failure.
   function Connect (Sock : Socket_Acc; Path : String) return Boolean;

   --  Make a socket into a listener, which makes the socket only able of
   --  accepting connections using Accept. Only stream sockets can be used.
   --  @return True on success, False on failure.
   function Listen (Sock : Socket_Acc; Backlog : Natural) return Boolean
      with Pre => Sock /= null;

   --  Accept a new connection, creating a connected socket for it.
   --  @return The new connection socket, or null if non-blocking and none
   --  are available.
   function Accept_Connection (Sock : Socket_Acc) return Socket_Acc
      with Pre => Sock /= null;

   --  Returned status of a socket operation.
   type Socket_Status is
      (Plain_Success, --  Unconditional success.
       Is_Bad_Type,   --  A listener is needed but the socket is not one, etc.
       Would_Block);  --  The socket would block, and we don't want that.

   --  Read and write.
   procedure Read
      (To_Read   : Socket_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
      with Pre => To_Read /= null;

   procedure Write
      (To_Write  : Socket_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Socket_Status)
      with Pre => To_Write /= null;

private

   type Socket is record
      Mutex          : aliased Lib.Synchronization.Binary_Semaphore;
      Inner_Domain   : Domain;
      Inner_Type     : DataType;
      Inner_Protocol : Protocol;
      Is_Listener    : Boolean;
      Is_Blocking    : Boolean;
      Connected      : Socket_Acc;
      Pending_Accept : Socket_Acc;
      Data           : Devices.Operation_Data (1 .. Arch.MMU.Page_Size);
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
