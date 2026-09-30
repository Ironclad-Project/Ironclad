--  sound.ads: Driver for OSS.
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

with Interfaces; use Interfaces;
with Devices;

package Sound is
   --  Versions of OSS supported by the subsystem.
   --  http://manuals.opensound.com/developer/
   Version       : constant String := "4.0b";
   Version_Major : constant Unsigned_32 := 4;
   Version_Minor : constant Unsigned_32 := 0;
   Version_Rev   : constant Unsigned_32 := 2;
   Version_Final : constant Unsigned_32 :=
      Shift_Left (Version_Major, 16) or
      Shift_Left (Version_Minor,  8) or
      Version_Rev;

   --  Get count of different resources.
   function Get_Mixer_Count return Natural;
   function Get_Audio_Device_Count return Natural;
   function Get_Card_Count return Natural;
   ----------------------------------------------------------------------------
   Mixer_ID_Len   : constant := 15;
   Mixer_Name_Len : constant := 31;
   procedure Add_Mixer
      (ID       : String;
       Name     : String;
       Res      : Devices.Resource;
       Card_Idx : Unsigned_32;
       Idx      : out Unsigned_32;
       Success  : out Boolean)
      with Pre => (ID'Length <= Mixer_ID_Len) and
                  (Name'Length <= Mixer_Name_Len);

   procedure Get_Mixer_Properties
      (Idx      : Unsigned_32;
       ID       : out String;
       ID_Len   : out Natural;
       Name     : out String;
       Name_Len : out Natural;
       Card_Idx : out Unsigned_32)
      with Pre => (ID'Length   = Mixer_ID_Len) and
                  (Name'Length = Mixer_Name_Len);
   ----------------------------------------------------------------------------
   Audio_Name_Len : constant := 63;
   Song_Name_Len  : constant := 63;
   Label_Name_Len : constant := 15;
   procedure Add_Audio_Device
      (Name      : String;
       Res       : Devices.Resource;
       Mixer_Idx : Unsigned_32;
       Card_Idx  : Unsigned_32;
       Idx       : out Unsigned_32;
       Success   : out Boolean)
      with Pre => (Name'Length <= Audio_Name_Len);

   --  Record the sample formats, as an OSS format mask, the rates, and the
   --  channel counts an audio device takes, and the OSS capabilities it has
   --  other than those for input and output.
   procedure Set_Audio_Device_Limits
      (Idx          : Unsigned_32;
       Formats      : Unsigned_32;
       Min_Rate     : Unsigned_32;
       Max_Rate     : Unsigned_32;
       Min_Channels : Unsigned_32;
       Max_Channels : Unsigned_32;
       Caps         : Unsigned_32);

   procedure Get_Audio_Device_Limits
      (Idx          : Unsigned_32;
       Formats      : out Unsigned_32;
       Min_Rate     : out Unsigned_32;
       Max_Rate     : out Unsigned_32;
       Min_Channels : out Unsigned_32;
       Max_Channels : out Unsigned_32;
       Caps         : out Unsigned_32);

   procedure Get_Audio_Device_Properties
      (Idx       : Unsigned_32;
       Name      : out String;
       Name_Len  : out Natural;
       Is_Input  : out Boolean;
       Is_Output : out Boolean;
       Label     : out String;
       Label_Len : out Natural;
       Song      : out String;
       Song_Len  : out Natural;
       Card_Idx  : out Unsigned_32;
       Mixer_Idx : out Unsigned_32)
      with Pre => (Name'Length  = Audio_Name_Len) and
                  (Label'Length = Label_Name_Len) and
                  (Song'Length  = Song_Name_Len);
   ----------------------------------------------------------------------------
   Card_Short_Name_Len : constant := 15;
   Card_Long_Name_Len  : constant := 127;
   Card_HW_Info_Len    : constant := 399;

   --  Add a card name for only tracking purposes.
   procedure Add_Card
      (Name_Short : String;
       Name_Long  : String;
       HW_Info    : String;
       Idx        : out Unsigned_32;
       Success    : out Boolean)
      with Pre => (Name_Short'Length <= Card_Short_Name_Len) and
                  (Name_Long'Length  <= Card_Long_Name_Len)  and
                  (HW_Info'Length    <= Card_HW_Info_Len);

   procedure Get_Card_Name
      (Idx            : Unsigned_32;
       Name_Short     : out String;
       Name_Long      : out String;
       HW_Info        : out String;
       Name_Short_Len : out Natural;
       Name_Long_Len  : out Natural;
       HW_Info_Len    : out Natural)
      with Pre => (Name_Short'Length = Card_Short_Name_Len) and
                  (Name_Long'Length  = Card_Long_Name_Len)  and
                  (HW_Info'Length    = Card_HW_Info_Len);
end Sound;
