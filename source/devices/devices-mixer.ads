--  devices-mixer.ads: Driver for OSS mixing.
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

package Devices.Mixer is
   --  Initialize the device.
   procedure Init (Success : out Boolean);
   ----------------------------------------------------------------------------
   type Audios_Arr is array (Natural range <>) of Unsigned_32 with Pack;
   type OSS_SysInfo is record
      Product       : String (1 .. 32);
      Version       : String (1 .. 32);
      Version_Num   : Unsigned_32;
      Options       : String (1 .. 128);
      Num_Audios    : Unsigned_32;
      Opened_Audios : Audios_Arr (1 .. 8);
      Num_Synths    : Unsigned_32;
      Num_MIDIs     : Unsigned_32;
      Num_Timers    : Unsigned_32;
      Num_Mixers    : Unsigned_32;
      Opened_MIDIs  : Audios_Arr (1 .. 8);
      Num_Cards     : Unsigned_32;
      Num_Audio_Eng : Unsigned_32;
      License       : String (1 .. 16);
      Revision      : String (1 .. 256);
      Filler        : Audios_Arr (1 .. 172);
   end record with Pack;

   type OSS_MixerInfo is record
      Dev            : Unsigned_32;
      ID             : String (1 .. 16);
      Name           : String (1 .. 32);
      Modify_Counter : Unsigned_32;
      Card_Number    : Unsigned_32;
      Port_Number    : Unsigned_32;
      Handle         : String (1 .. 32);
      Magic          : Unsigned_32;
      Enabled        : Unsigned_32;
      Caps           : Unsigned_32;
      Flags          : Unsigned_32;
      NRExt          : Unsigned_32;
      Prio           : Unsigned_32;
      DevNode        : String (1 .. 32);
      Legacy_Device  : Unsigned_32;
      Filler         : Audios_Arr (1 .. 245);
   end record with Pack;

   PCM_CAP_DUPLEX : constant := 16#00100#;
   PCM_CAP_INPUT  : constant := 16#10000#;
   PCM_CAP_OUTPUT : constant := 16#20000#;
   type OSS_AudioInfo is record
      Dev            : Unsigned_32;
      Name           : String (1 .. 64);
      Busy           : Unsigned_32;
      PID            : Unsigned_32;
      Caps           : Unsigned_32;
      IFormats       : Unsigned_32;
      OFormats       : Unsigned_32;
      Magic          : Unsigned_32;
      Command        : String (1 .. 64);
      Card_Number    : Unsigned_32;
      Port_Number    : Unsigned_32;
      Mixer_Device   : Unsigned_32;
      Legacy_Device  : Unsigned_32;
      Enabled        : Unsigned_32;
      Flags          : Unsigned_32;
      Min_Rate       : Unsigned_32;
      Max_Rate       : Unsigned_32;
      Min_Channels   : Unsigned_32;
      Max_Channels   : Unsigned_32;
      Binding        : Unsigned_32;
      Rate_Source    : Unsigned_32;
      Handle         : String (1 .. 32);
      NRates         : Unsigned_32;
      Rates          : Audios_Arr (1 .. 20);
      Song_Name      : String (1 .. 64);
      Label          : String (1 .. 16);
      Latency        : Unsigned_32;
      Dev_Node       : String (1 .. 32);
      Next_Play_Eng  : Unsigned_32;
      Next_Rec_Eng   : Unsigned_32;
      Filler         : Audios_Arr (1 .. 184);
   end record with Pack;

   type OSS_CardInfo is record
      Card       : Unsigned_32;
      Short_Name : String (1 .. 16);
      Long_Name  : String (1 .. 128);
      Flags      : Unsigned_32;
      HW_Info    : String (1 .. 400);
      Int_Count  : Unsigned_32;
      Ack_Count  : Unsigned_32;
      Filler     : Audios_Arr (1 .. 154);
   end record;

   --  Check size of the arguments below.
   procedure Common_OSS_IO_Argument
      (Request : Unsigned_64;
       Usage   : out IO_Usage;
       Size    : out Natural);

   --  Under OSS, all devices need to support some common IOCTLs, this call
   --  does it.
   procedure Common_OSS_IO_Control
      (Request  : Unsigned_64;
       Argument : System.Address;
       Extra    : out Unsigned_64;
       Success  : out Boolean);

private

   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean);

   procedure IO_Argument
      (Key     : System.Address;
       Request : Unsigned_64;
       Usage   : out IO_Usage;
       Size    : out Natural);

   procedure IO_Control
      (Key      : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address;
       Extra    : out Unsigned_64;
       Success  : out Boolean);
end Devices.Mixer;
