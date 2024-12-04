--  devices-tty.adb: Virtual TTY device.
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

with Arch.Local;
with Userland.Process; use Userland.Process;
with IPC.PTY; use IPC.PTY;

package body Devices.TTY is
   procedure Init (Success : out Boolean) is
   begin
      Register
         ((Data        => System.Null_Address,
           ID          => (others => 0),
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => Read'Access,
           Write       => Write'Access,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => IO_Control'Access,
           Mmap        => null,
           Poll        => null,
           Remove      => null), "tty", Success);
   end Init;
   ----------------------------------------------------------------------------
   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);

      Proc : constant PID := Arch.Local.Get_Current_Process;
      PTY  : IPC.PTY.Inner_Acc;
      Succ : Status;
   begin
      Get_Controlling_TTY (Proc, PTY);
      if PTY = null then
         Data      := [others => 0];
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      IPC.PTY.Read_Secondary
         (To_Read     => PTY,
          Data        => Data,
          Is_Blocking => Is_Blocking,
          Ret_Count   => Ret_Count,
          Success     => Succ);
      Success := Succ = PTY_Success;
   end Read;

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);

      Proc : constant PID := Arch.Local.Get_Current_Process;
      PTY  : IPC.PTY.Inner_Acc;
      Succ : Status;
   begin
      Get_Controlling_TTY (Proc, PTY);
      if PTY = null then
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      IPC.PTY.Write_Secondary
         (To_Write    => PTY,
          Data        => Data,
          Is_Blocking => Is_Blocking,
          Ret_Count   => Ret_Count,
          Success     => Succ);
      Success := Succ = PTY_Success;
   end Write;

   procedure IO_Control
      (Key       : System.Address;
       Request   : Unsigned_64;
       Argument  : System.Address;
       Has_Extra : out Boolean;
       Extra     : out Unsigned_64;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);

      Proc : constant PID := Arch.Local.Get_Current_Process;
      PTY  : IPC.PTY.Inner_Acc;
   begin
      Get_Controlling_TTY (Proc, PTY);
      if PTY = null then
         Success := IPC.PTY.IO_Control
            (PTY        => PTY,
             Is_Primary => False,
             Request    => Request,
             Argument   => Argument);
      else
         Success := False;
      end if;
      Has_Extra := False;
      Extra     := 0;
   end IO_Control;
end Devices.TTY;
