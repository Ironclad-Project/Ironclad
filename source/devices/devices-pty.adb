--  devices-pty.adb: PTY creation and management.
--  Copyright (C) 2021 streaksu
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
with IPC.Pipe; use IPC.Pipe;
with Arch.Local;
with Userland.Process; use Userland.Process;
with VFS.File; use VFS.File;

package body Devices.PTY with SPARK_Mode => Off is
   --  Structures for master and slave ptys.
   --  Master PTYs have internally 2 pipes, one for communication with them,
   --  and other with communication from the slave to the master.
   --  Internal state is handled by the master.
   type Master_PTY;
   type Master_PTY_Acc is access all Master_PTY;
   type Slave_PTY;
   type Slave_PTY_Acc is access all Slave_PTY;
   type Master_PTY is record
      Reader_To_Slave, Reader_To_Master : IPC.Pipe.Pipe_Reader_Acc;
      Writer_To_Slave, Writer_To_Master : IPC.Pipe.Pipe_Writer_Acc;
      Slave     : Slave_PTY_Acc;
      Term_Info : TermIOs.Main_Data;
      Term_Size : TermIOs.Win_Size;
      Index     : Natural;
   end record;
   type Slave_PTY is record
      Master : System.Address;
   end record;

   --  Global master pty counter and base name, to ensure they dont collide.
   Next_PTY             : Natural range 0 .. 9 := 0;
   Master_PTY_Base_Name : constant String := "pty0";
   Slave_PTY_Base_Name  : constant String := "pts0";

   function Init return Boolean is
      PTMX_Dev : constant Resource := (
         Data              => System.Null_Address,
         Mutex             => Lib.Synchronization.Unlocked_Semaphore,
         Is_Block          => False,
         Block_Size        => 4096,
         Block_Count       => 0,
         Unique_Identifier => 0,
         Sync              => null,
         Read              => null,
         Write             => null,
         IO_Control        => PTMX_IO_Control'Access,
         Mmap              => null,
         Munmap            => null
      );
   begin
      --  Register /dev/ptmx, used exclusively for opening master ptys.
      --  Just instead of doing it with `open`, for API limitations, we do it
      --  with an IOCTL (lame I know).
      return Register (PTMX_Dev, "ptmx");
   end Init;

   function PTMX_IO_Control
      (Data     : Resource_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Request);
      Result : Integer with Import, Address => Argument;
      Master_Dev : Resource;
      Master     : Master_PTY_Acc;
      Name       : String := Master_PTY_Base_Name;
   begin
      --  Initialize the master PTY.
      Master := new Master_PTY'
         (Slave     => null,
          Term_Info =>
            (Input_Modes   => 0,
             Output_Modes  => 0,
             Control_Modes => 0,
             Local_Mode    => (others => False),
             Special_Chars => (others => 0),
             Input_Baud    => 0,
             Output_Baud   => 0),
          Term_Size => (others => 0),
          Index     => Next_PTY,
          others    => <>);
      Create_Pair (Master.Writer_To_Master, Master.Reader_To_Master, True);
      Create_Pair (Master.Writer_To_Slave,  Master.Reader_To_Slave,  True);
      Next_PTY := Next_PTY + 1;

      --  Give it a cool name and register it.
      Name (Name'Last) := Character'Val (Master.Index + Character'Pos ('0'));
      Master_Dev := (
         Data              => Master.all'Address,
         Mutex             => Lib.Synchronization.Unlocked_Semaphore,
         Is_Block          => False,
         Block_Size        => 4096,
         Block_Count       => 0,
         Unique_Identifier => 0,
         Sync              => null,
         Read              => Master_Read'Access,
         Write             => Master_Write'Access,
         IO_Control        => Master_IO_Control'Access,
         Mmap              => null,
         Munmap            => null
      );

      if not Register (Master_Dev, Name) then
         goto Error;
      end if;

      --  Register the user file.
      Result := Add_File ("/dev/" & Name);
      return Result /= -1;

   <<Error>>
      Result := -1;
      return False;
   end PTMX_IO_Control;
   ----------------------------------------------------------------------------
   function Master_Read
      (Data   : Resource_Acc;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Offset);
      Master : Master_PTY with Import, Address => Data.Data;
   begin
      return Read (Master.Reader_To_Master, Count, Desto);
   end Master_Read;

   function Master_Write
      (Data     : Resource_Acc;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Offset);
      Master  : Master_PTY with Import, Address => Data.Data;
      Discard : Unsigned_64;
   begin
      --  Handle echo.
      Discard := Write (Master.Writer_To_Master, Count, To_Write);
      return Write (Master.Writer_To_Slave, Count, To_Write);
   end Master_Write;

   function Master_IO_Control
      (Data     : Resource_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      Master : aliased Master_PTY with Import, Address => Data.Data;
      Result : Integer with Import, Address => Argument;
      Slave_Dev : Resource;
      Slave     : Slave_PTY_Acc;
      Name      : String := Slave_PTY_Base_Name;
   begin
      if Request /= 0 then
         return Termios_IO_Control (Data.Data, Request, Argument);
      end if;

      --  Initialize the slave PTY and notify the master.
      Slave := new Slave_PTY'(Master => Data.Data);
      Master.Slave := Slave;

      --  Give it a cool name and register it.
      Name (Name'Last) := Character'Val (Master.Index + Character'Pos ('0'));
      Slave_Dev := (
         Data              => Slave.all'Address,
         Mutex             => Lib.Synchronization.Unlocked_Semaphore,
         Is_Block          => False,
         Block_Size        => 4096,
         Block_Count       => 0,
         Unique_Identifier => 0,
         Sync              => null,
         Read              => Slave_Read'Access,
         Write             => Slave_Write'Access,
         IO_Control        => Slave_IO_Control'Access,
         Mmap              => null,
         Munmap            => null
      );

      if not Register (Slave_Dev, Name) then
         goto Error;
      end if;

      --  Register the user file.
      Result := Add_File ("/dev/" & Name);
      return Result /= -1;

   <<Error>>
      Result := -1;
      return False;
   end Master_IO_Control;
   ----------------------------------------------------------------------------
   function Slave_Read
      (Data   : Resource_Acc;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Offset);
      Slave  : Slave_PTY  with Import, Address => Data.Data;
      Master : Master_PTY with Import, Address => Slave.Master;
   begin
      return Read (Master.Reader_To_Slave, Count, Desto);
   end Slave_Read;

   function Slave_Write
      (Data     : Resource_Acc;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      pragma Unreferenced (Offset);
      Slave  : Slave_PTY  with Import, Address => Data.Data;
      Master : Master_PTY with Import, Address => Slave.Master;
   begin
      return Write (Master.Writer_To_Master, Count, To_Write);
   end Slave_Write;

   function Slave_IO_Control
      (Data     : Resource_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      Slave  : Slave_PTY with Import, Address => Data.Data;
   begin
      return Termios_IO_Control (Slave.Master, Request, Argument);
   end Slave_IO_Control;
   ----------------------------------------------------------------------------
   function Add_File (Path : String) return Integer is
      Proc   : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
      Result : Integer;
      Descr  : File_Description_Acc;
      File   : VFS.File.File_Acc;
   begin
      File := VFS.File.Open (Path, Read_Write);
      if File = null then
         goto Error;
      end if;
      Descr := new File_Description'(
         Close_On_Exec => False,
         Description   => Description_File,
         Inner_File    => File
      );
      if not Userland.Process.Add_File (Proc, Descr, Result) then
         goto Error;
      end if;

      return Result;

   <<Error>>
      return -1;
   end Add_File;

   function Termios_IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      Master      : Master_PTY        with Import, Address => Data;
      Result_Info : TermIOs.Main_Data with Import, Address => Argument;
      Result_Size : TermIOs.Win_Size  with Import, Address => Argument;
   begin
      case Request is
         when TermIOs.TCGETS =>
            Result_Info := Master.Term_Info;
         when TermIOs.TCSETS | TermIOs.TCSETSW | TermIOs.TCSETSF =>
            Master.Term_Info := Result_Info;
         when TermIOs.TIOCGWINSZ =>
            Result_Size := Master.Term_Size;
         when TermIOs.TIOCSWINSZ =>
            Master.Term_Size := Result_Size;
         when others =>
            return False;
      end case;
      return True;
   end Termios_IO_Control;
end Devices.PTY;
