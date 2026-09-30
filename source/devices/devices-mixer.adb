--  devices-mixer.adb: Driver for OSS mixing.
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

with Sound;
with Ada.Characters.Latin_1;
with Sound.OSS_IOCTL; use Sound.OSS_IOCTL;

package body Devices.Mixer is
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
          Poll        => null,
          Remove      => null,
          IO_Argument => IO_Argument'Access);
      Register (Device, "mixer", Success);
   end Init;
   ----------------------------------------------------------------------------
   procedure Common_OSS_IO_Argument
      (Request : Unsigned_64;
       Usage   : out IO_Usage;
       Size    : out Natural)
   is
   begin
      case Request is
         when OSS_GETVERSION =>
            Usage := IO_Write;
            Size  := Unsigned_32'Object_Size / 8;
         when SNDCTL_SYSINFO =>
            Usage := IO_Write;
            Size  := OSS_SysInfo'Object_Size / 8;
         when SNDCTL_MIXERINFO =>
            Usage := IO_Read_Write;
            Size  := OSS_MixerInfo'Object_Size / 8;
         when SNDCTL_CARDINFO =>
            Usage := IO_Read_Write;
            Size  := OSS_CardInfo'Object_Size / 8;
         when SNDCTL_AUDIOINFO | SNDCTL_AUDIOINFO_EX | SNDCTL_ENGINEINFO =>
            Usage := IO_Read_Write;
            Size  := OSS_AudioInfo'Object_Size / 8;
         when others =>
            Usage := IO_Unknown;
            Size  := 0;
      end case;
   exception
      when Constraint_Error =>
         Usage := IO_Unknown;
         Size  := 0;
   end Common_OSS_IO_Argument;

   procedure Common_OSS_IO_Control
      (Request  : Unsigned_64;
       Argument : System.Address;
       Extra    : out Unsigned_64;
       Success  : out Boolean)
   is
      use Ada.Characters;

      SysVers : Unsigned_32   with Import, Address => Argument;
      Sysinfo : OSS_SysInfo   with Import, Address => Argument;
      Mixinfo : OSS_MixerInfo with Import, Address => Argument;
      Audinfo : OSS_AudioInfo with Import, Address => Argument;
      Carinfo : OSS_CardInfo  with Import, Address => Argument;

      Tmp1, Tmp2, Tmp3 : Natural;
      Tmp4, Formats, Caps : Unsigned_32;
      Is_In, Is_Out : Boolean;
   begin
      Extra   := 0;
      Success := True;

      case Request is
         when OSS_GETVERSION =>
            SysVers := Sound.Version_Final;
         when SNDCTL_SYSINFO =>
            Sysinfo :=
               (Product       => "Ironclad Audio" & [1 .. 18 => Latin_1.NUL],
                Version       => Sound.Version & [1 .. 28 => Latin_1.NUL],
                Version_Num   => Sound.Version_Final,
                Options       => [others => Latin_1.NUL],
                Num_Audios    => Unsigned_32 (Sound.Get_Audio_Device_Count),
                Opened_Audios => [others => 0],
                Num_Synths    => 0,
                Num_MIDIs     => 0,
                Num_Timers    => 0,
                Num_Mixers    => Unsigned_32 (Sound.Get_Mixer_Count),
                Opened_MIDIs  => [others => 0],
                Num_Cards     => Unsigned_32 (Sound.Get_Card_Count),
                Num_Audio_Eng => Unsigned_32 (Sound.Get_Audio_Device_Count),
                License       => "GPL" & [1 .. 13 => Latin_1.NUL],
                Revision      => [others => Latin_1.NUL],
                Filler        => [others => 0]);
         when SNDCTL_MIXERINFO =>
            if Mixinfo.Dev = Unsigned_32'Last then
               Tmp4 := 0;
            else
               Tmp4 := Mixinfo.Dev;
            end if;
            Mixinfo :=
               (Dev            => Tmp4,
                ID             => [others => Latin_1.NUL],
                Name           => [others => Latin_1.NUL],
                Modify_Counter => 0,
                Card_Number    => 0,
                Port_Number    => 0,
                Handle         => [others => Latin_1.NUL],
                Magic          => 0,
                Enabled        => 1,
                Caps           => 0,
                Flags          => 0,
                NRExt          => 0,
                Prio           => 0,
                DevNode        => [others => Latin_1.NUL],
                Legacy_Device  => Tmp4,
                Filler         => [others => 0]);
            Sound.Get_Mixer_Properties
               (Idx      => Tmp4,
                ID       => Mixinfo.ID (1 .. Mixinfo.ID'Last - 1),
                ID_Len   => Tmp1,
                Name     => Mixinfo.Name (1 .. Mixinfo.Name'Last - 1),
                Name_Len => Tmp2,
                Card_Idx => Mixinfo.Card_Number);
            if Tmp1 = 0 then
               Success := False;
               return;
            end if;
            Mixinfo.ID (Tmp1 + 1) := Latin_1.NUL;
            Mixinfo.Name (Tmp2 + 1) := Latin_1.NUL;
            declare
               DevNode : constant String := "/dev/mixer" & Tmp4'Image;
            begin
               Mixinfo.DevNode (1 .. DevNode'Length) := DevNode;
            end;
         when SNDCTL_CARDINFO =>
            Tmp4 := Carinfo.Card;
            Carinfo :=
               (Card       => Tmp4,
                Short_Name => [others => Latin_1.NUL],
                Long_Name  => [others => Latin_1.NUL],
                Flags      => 0,
                HW_Info    => [others => Latin_1.NUL],
                Int_Count  => 0,
                Ack_Count  => 0,
                Filler     => [others => 0]);
            Sound.Get_Card_Name
               (Idx            => Tmp4,
                Name_Short     =>
                   Carinfo.Short_Name (1 .. Carinfo.Short_Name'Last - 1),
                Name_Long      =>
                   Carinfo.Long_Name (1 .. Carinfo.Long_Name'Last - 1),
                HW_Info        =>
                   Carinfo.HW_Info (1 .. Carinfo.HW_Info'Last - 1),
                Name_Short_Len => Tmp1,
                Name_Long_Len  => Tmp2,
                HW_Info_Len    => Tmp3);
            if Tmp1 = 0 then
               Success := False;
            else
               Carinfo.Short_Name (Tmp1 + 1) := Latin_1.NUL;
               Carinfo.Long_Name (Tmp2 + 1) := Latin_1.NUL;
               Carinfo.HW_Info (Tmp3 + 1) := Latin_1.NUL;
            end if;
         when SNDCTL_AUDIOINFO | SNDCTL_AUDIOINFO_EX | SNDCTL_ENGINEINFO =>
            if Audinfo.Dev = Unsigned_32'Last then
               Tmp4 := 0;
            else
               Tmp4 := Audinfo.Dev;
            end if;
            Audinfo :=
               (Dev            => Tmp4,
                Name           => [others => Latin_1.NUL],
                Busy           => 0,
                PID            => Unsigned_32'Last,
                Caps           => 0,
                IFormats       => 0,
                OFormats       => 0,
                Magic          => 0,
                Command        => [others => Latin_1.NUL],
                Card_Number    => 0,
                Port_Number    => 0,
                Mixer_Device   => 0,
                Legacy_Device  => Tmp4,
                Enabled        => 1,
                Flags          => 0,
                Min_Rate       => 0,
                Max_Rate       => 0,
                Min_Channels   => 0,
                Max_Channels   => 0,
                Binding        => 0,
                Rate_Source    => 0,
                Handle         => [others => Latin_1.NUL],
                NRates         => 0,
                Rates          => [others => 0],
                Song_Name      => [others => Latin_1.NUL],
                Label          => [others => Latin_1.NUL],
                Latency        => 0,
                Dev_Node       => [others => Latin_1.NUL],
                Next_Play_Eng  => 0,
                Next_Rec_Eng   => 0,
                Filler         => [others => 0]);
            Sound.Get_Audio_Device_Properties
               (Idx       => Tmp4,
                Name      => Audinfo.Name (1 .. Audinfo.Name'Last - 1),
                Name_Len  => Tmp1,
                Song    => Audinfo.Song_Name (1 .. Audinfo.Song_Name'Last - 1),
                Song_Len  => Tmp2,
                Label     => Audinfo.Label (1 .. Audinfo.Label'Last - 1),
                Label_Len => Tmp3,
                Is_Input  => Is_In,
                Is_Output => Is_Out,
                Card_Idx  => Audinfo.Card_Number,
                Mixer_Idx => Audinfo.Mixer_Device);
            if Tmp1 = 0 then
               Success := False;
               return;
            end if;
            Audinfo.Name (Tmp1 + 1) := Latin_1.NUL;
            Audinfo.Song_Name (Tmp2 + 1) := Latin_1.NUL;
            Audinfo.Label (Tmp3 + 1) := Latin_1.NUL;
            declare
               DevNode : constant String := "/dev/dsp" & Tmp4'Image;
            begin
               Audinfo.Dev_Node (1 .. DevNode'Length) := DevNode;
            end;

            Sound.Get_Audio_Device_Limits
               (Idx          => Tmp4,
                Formats      => Formats,
                Min_Rate     => Audinfo.Min_Rate,
                Max_Rate     => Audinfo.Max_Rate,
                Min_Channels => Audinfo.Min_Channels,
                Max_Channels => Audinfo.Max_Channels,
                Caps         => Caps);
            Audinfo.Caps     := Caps;
            Audinfo.IFormats := (if Is_In  then Formats else 0);
            Audinfo.OFormats := (if Is_Out then Formats else 0);

            if Is_In then
               Audinfo.Caps := Audinfo.Caps or PCM_CAP_INPUT;
            end if;
            if Is_Out then
               Audinfo.Caps := Audinfo.Caps or PCM_CAP_OUTPUT;
            end if;
            if Is_In and Is_Out then
               Audinfo.Caps := Audinfo.Caps or PCM_CAP_DUPLEX;
            end if;
         when others =>
            Extra   := 0;
            Success := False;
      end case;
   exception
      when Constraint_Error =>
         Extra   := 0;
         Success := False;
   end Common_OSS_IO_Control;
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
      Hand : constant Devices.Device_Handle := Fetch ("mixer0");
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
      Hand : constant Devices.Device_Handle := Fetch ("mixer0");
   begin
      if Hand /= Devices.Error_Handle then
         Devices.Write (Hand, Offset, Data, Ret_Count, Success, Is_Blocking);
      else
         Ret_Count := 0;
         Success := Dev_Not_Supported;
      end if;
   end Write;

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
      Common_OSS_IO_Argument (Request, Usage, Size);
      if Usage = IO_Unknown then
         Hand := Fetch ("mixer0");
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
      Common_OSS_IO_Control (Request, Argument, Extra, Success);
      if Success then
         return;
      end if;

      Hand := Fetch ("mixer0");
      if Hand /= Devices.Error_Handle then
         Devices.IO_Control (Hand, Request, Argument, Extra, Success);
      else
         Extra := 0;
         Success := False;
      end if;
   exception
      when Constraint_Error =>
         Extra   := 0;
         Success := False;
   end IO_Control;
end Devices.Mixer;
