--  ipc-pty.ads: PTY creation and management.
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

with Devices.TermIOs;
with Lib.Synchronization;
with Arch.MMU;

package IPC.PTY is
   --  PTYs are an IPC method meant to provide bidirectional communication
   --  between 2 read/write ports, along with that, what makes it special
   --  is storage and awareness of configurable termios data. Usually, the
   --  ports are called master/slave, in Ironclad its primary/secondary. A
   --  PTY must be closed twice, one for each end.
   --
   --  These properties make the most common use of PTYs terminal emulation
   --  and as building block for user interfaces. Only blocking operation is
   --  available.
   --
   --  Unlike most UNIXy kernels out there, PTYs in Ironclad are not bound to
   --  files, in this aspect, they act as an unbound FIFO would.

   --  Objects to represent a PTY.
   type Inner     is private;
   type Inner_Acc is access Inner;

   --  Results from operations on PTYs.
   type Status is (PTY_Success, PTY_Would_Block);

   --  Create a fresh PTY with 1 refcount.
   function Create return Inner_Acc;

   --  Decrease the refcount of a PTY, if zero, free it.
   --  @param Closed PTY to decrease the refcount of.
   procedure Close (Closed : in out Inner_Acc) with Pre => Is_Valid (Closed);

   --  Read from the primary end of the PTY.
   procedure Read_Primary
      (To_Read   : Inner_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Status)
      with Pre => Is_Valid (To_Read);

   --  Write to the primary end of the PTY.
   procedure Write_Primary
      (To_Write  : Inner_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Status)
      with Pre => Is_Valid (To_Write);

   --  Read from the secondary end of the PTY.
   procedure Read_Secondary
      (To_Read   : Inner_Acc;
       Data      : out Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Status)
      with Pre => Is_Valid (To_Read);

   --  Write to the secondary end of the PTY.
   procedure Write_Secondary
      (To_Write  : Inner_Acc;
       Data      : Devices.Operation_Data;
       Ret_Count : out Natural;
       Success   : out Status)
      with Pre => Is_Valid (To_Write);

   --  Poll the state of a PTY's primary end.
   procedure Poll_Primary
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Can_Prio  : out Boolean)
      with Pre => Is_Valid (P);

   --  Poll the state of a PTY's secondary end.
   procedure Poll_Secondary
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean)
      with Pre => Is_Valid (P);

   --  Get whether the passed PTY is blocking for the primary end.
   procedure Is_Primary_Blocking (P : Inner_Acc; Blocking : out Boolean)
      with Pre => Is_Valid (P);

   --  Set whether the passed PTY is blocking for the primary end.
   procedure Set_Primary_Blocking (P : Inner_Acc; Blocking : Boolean)
      with Pre => Is_Valid (P);

   --  Get whether the passed PTY is blocking for the secondary end.
   procedure Is_Secondary_Blocking (P : Inner_Acc; Blocking : out Boolean)
      with Pre => Is_Valid (P);

   --  Set whether the passed PTY is blocking for the secondary end.
   procedure Set_Secondary_Blocking (P : Inner_Acc; Blocking : Boolean)
      with Pre => Is_Valid (P);

   --  Get the termios data for the PTY.
   procedure Get_TermIOs (P : Inner_Acc; T : out Devices.TermIOs.Main_Data)
      with Pre => Is_Valid (P);

   --  Set the termios data for the PTY.
   procedure Set_TermIOs (P : Inner_Acc; T : Devices.TermIOs.Main_Data)
      with Pre => Is_Valid (P);

   --  Get terminal dimensions.
   procedure Get_WinSize (P : Inner_Acc; W : out Devices.TermIOs.Win_Size)
      with Pre => Is_Valid (P);

   --  Set terminal dimensions.
   procedure Set_WinSize (P : Inner_Acc; W : Devices.TermIOs.Win_Size)
      with Pre => Is_Valid (P);

   --  Get the associated VFS entity for the PTY.
   procedure Get_Name (P : Inner_Acc; Str : out String; Len : out Natural)
      with Pre => Is_Valid (P);

   --  Flush the pending data to read or write of the primary end.
   procedure Flush_Primary (P : Inner_Acc; To_Read, To_Transmit : Boolean)
      with Pre => Is_Valid (P);

   --  Flush the pending data to read or write of the secondary end.
   procedure Flush_Secondary (P : Inner_Acc; To_Read, To_Transmit : Boolean)
      with Pre => Is_Valid (P);

   --  Flush the pending data to read or write of the secondary end.
   procedure Start_Primary (P : Inner_Acc; To_Read, To_Transmit : Boolean)
      with Pre => Is_Valid (P);

   --  Flush the pending data to read or write of the secondary end.
   procedure Start_Secondary (P : Inner_Acc; To_Read, To_Transmit : Boolean)
      with Pre => Is_Valid (P);

   --  Flush the pending data to read or write of the secondary end.
   procedure Stop_Primary (P : Inner_Acc; To_Read, To_Transmit : Boolean)
      with Pre => Is_Valid (P);

   --  Flush the pending data to read or write of the secondary end.
   procedure Stop_Secondary (P : Inner_Acc; To_Read, To_Transmit : Boolean)
      with Pre => Is_Valid (P);

   --  Ghost function for checking whether a PTY is properly initialized.
   function Is_Valid (P : Inner_Acc) return Boolean with Ghost;

private

   subtype Data_Length is Natural range 0 .. Arch.MMU.Page_Size;
   subtype TTY_Data    is Devices.Operation_Data (1 .. Arch.MMU.Page_Size);
   type Inner is record
      Primary_Mutex      : aliased Lib.Synchronization.Binary_Semaphore;
      Secondary_Mutex    : aliased Lib.Synchronization.Binary_Semaphore;
      Global_Data_Mutex  : aliased Lib.Synchronization.Binary_Semaphore;
      Primary_Block      : Boolean;
      Secondary_Block    : Boolean;
      Primary_Read       : Boolean;
      Primary_Transmit   : Boolean;
      Secondary_Read     : Boolean;
      Secondary_Transmit : Boolean;
      Name_Index         : Natural;
      Term_Info          : Devices.TermIOs.Main_Data;
      Term_Size          : Devices.TermIOs.Win_Size;
      Was_Closed         : Boolean;
      Termios_Changed    : Boolean;
      Primary_Length     : aliased Data_Length;
      Secondary_Length   : aliased Data_Length;
      Primary_Data       : aliased TTY_Data;
      Secondary_Data     : aliased TTY_Data;
   end record;

   function Is_Valid (P : Inner_Acc) return Boolean is (P /= null);

   procedure Read_From_End
      (End_Mutex   : access Lib.Synchronization.Binary_Semaphore;
       Inner_Len   : access Data_Length;
       Inner_Data  : access TTY_Data;
       Is_Blocking : Boolean;
       Is_Able_To  : Boolean;
       Data        : out Devices.Operation_Data;
       Ret_Count   : out Natural);

   procedure Write_To_End
      (End_Mutex   : access Lib.Synchronization.Binary_Semaphore;
       Inner_Len   : access Data_Length;
       Inner_Data  : access TTY_Data;
       Is_Blocking : Boolean;
       Is_Able_To  : Boolean;
       Data        : Devices.Operation_Data;
       Ret_Count   : out Natural);
end IPC.PTY;
