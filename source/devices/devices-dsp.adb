--  devices-dsp.adb: Driver for OSS sound interfacing.
--  Copyright (C) 2026 streaksu
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

with Devices.Mixer;

package body Devices.DSP is
   procedure Init (Success : out Boolean) is
      Device : Resource;
   begin
      Device :=
         (Data        => System.Null_Address,
          Is_Block    => False,
          Block_Size  => 4096,
          Block_Count => 0,
          Read        => Read'Access,
          Write       => Write'Access,
          Sync        => null,
          Sync_Range  => null,
          IO_Control  => IO_Control'Access,
          Mmap        => null,
          Poll        => Poll'Access,
          Remove      => null,
          IO_Argument => IO_Argument'Access);
      Register (Device, "dsp", Success);
   end Init;
   ----------------------------------------------------------------------------
   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      Hand : constant Devices.Device_Handle := Fetch ("dsp0");
   begin
      if Hand /= Devices.Error_Handle then
         Devices.Read (Hand, Offset, Data, Ret_Count, Success, Is_Blocking);
      else
         Data := [others => 0];
         Ret_Count := 0;
         Success := Dev_Not_Supported;
      end if;
   end Read;

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      Hand : constant Devices.Device_Handle := Fetch ("dsp0");
   begin
      if Hand /= Devices.Error_Handle then
         Devices.Write (Hand, Offset, Data, Ret_Count, Success, Is_Blocking);
      else
         Ret_Count := 0;
         Success := Dev_Not_Supported;
      end if;
   end Write;

   procedure Poll
      (Key       : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
      pragma Unreferenced (Key);
      Hand : constant Devices.Device_Handle := Fetch ("dsp0");
   begin
      if Hand /= Devices.Error_Handle then
         Devices.Poll (Hand, Can_Read, Can_Write, Is_Error);
      else
         Can_Read  := False;
         Can_Write := False;
         Is_Error  := True;
      end if;
   end Poll;

   procedure IO_Argument
      (Key     : System.Address;
       Request : Unsigned_64;
       Usage   : out IO_Usage;
       Size    : out Natural)
   is
      pragma Unreferenced (Key);
      Hand : Devices.Device_Handle;
   begin
      --  Requests of the device itself are described by it.
      Devices.Mixer.Common_OSS_IO_Argument (Request, Usage, Size);
      if Usage = IO_Unknown then
         Hand := Fetch ("dsp0");
         if Hand /= Devices.Error_Handle then
            Devices.IO_Argument (Hand, Request, Usage, Size);
         end if;
      end if;
   end IO_Argument;

   procedure IO_Control
      (Key      : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address;
       Extra    : out Unsigned_64;
       Success  : out Boolean)
   is
      pragma Unreferenced (Key);
      Hand : Devices.Device_Handle;
   begin
      Devices.Mixer.Common_OSS_IO_Control (Request, Argument, Extra, Success);
      if Success then
         return;
      end if;

      Hand := Fetch ("dsp0");
      if Hand /= Devices.Error_Handle then
         Devices.IO_Control (Hand, Request, Argument, Extra, Success);
      else
         Extra := 0;
         Success := False;
      end if;
   end IO_Control;
end Devices.DSP;
