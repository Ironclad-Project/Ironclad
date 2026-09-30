--  sound.adb: Driver for OSS.
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

package body Sound is
   type Mixer_Info is record
      ID       : String (1 .. Mixer_ID_Len);
      ID_Len   : Natural;
      Name     : String (1 .. Mixer_Name_Len);
      Name_Len : Natural;
      Card_Idx : Unsigned_32;
   end record;
   type Mixer_Info_Acc is access Mixer_Info;
   type Mixer_Info_Arr is array (Unsigned_32 range 0 .. 20) of
      Mixer_Info_Acc;
   Mixer_Devs : Mixer_Info_Arr := [others => null];

   type Device_Info is record
      Name      : String (1 .. Audio_Name_Len);
      Name_Len  : Natural;
      Mixer_Idx : Unsigned_32;
      Card_Idx  : Unsigned_32;
      Can_Read  : Boolean;
      Can_Write : Boolean;
      Label     : String (1 .. Label_Name_Len);
      Label_Len : Natural;
      Song      : String (1 .. Song_Name_Len);
      Song_Len  : Natural;
      Formats      : Unsigned_32;
      Min_Rate     : Unsigned_32;
      Max_Rate     : Unsigned_32;
      Min_Channels : Unsigned_32;
      Max_Channels : Unsigned_32;
      Caps         : Unsigned_32;
   end record;
   type Device_Info_Acc is access Device_Info;
   type Device_Info_Arr is array (Unsigned_32 range 0 .. 20) of
      Device_Info_Acc;
   Audio_Devs : Device_Info_Arr := [others => null];

   type Card_Info is record
      Short_Name : String (1 .. Card_Short_Name_Len);
      Short_Len  : Natural;
      Long_Name  : String (1 .. Card_Long_Name_Len);
      Long_Len   : Natural;
      HW_Info    : String (1 .. Card_HW_Info_Len);
      Info_Len   : Natural;
   end record;
   type Card_Info_Acc is access Card_Info;
   type Card_Info_Arr is array (Unsigned_32 range 0 .. 20) of Card_Info_Acc;
   Cards : Card_Info_Arr := [others => null];

   function Get_Mixer_Count return Natural is
      Count : Natural := 0;
   begin
      for Dev of Mixer_Devs loop
         if Dev /= null then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   exception
      when Constraint_Error =>
         return 0;
   end Get_Mixer_Count;

   function Get_Audio_Device_Count return Natural is
      Count : Natural := 0;
   begin
      for Dev of Audio_Devs loop
         if Dev /= null then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   exception
      when Constraint_Error =>
         return 0;
   end Get_Audio_Device_Count;

   function Get_Card_Count return Natural is
      Count : Natural := 0;
   begin
      for Card of Cards loop
         if Card /= null then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   exception
      when Constraint_Error =>
         return 0;
   end Get_Card_Count;
   ----------------------------------------------------------------------------
   procedure Add_Mixer
      (ID       : String;
       Name     : String;
       Res      : Devices.Resource;
       Card_Idx : Unsigned_32;
       Idx      : out Unsigned_32;
       Success  : out Boolean)
   is
   begin
      for I in Mixer_Devs'Range loop
         if Mixer_Devs (I) = null then
            Mixer_Devs (I) := new Mixer_Info;
            Mixer_Devs (I).ID (1 .. ID'Length) := ID;
            Mixer_Devs (I).ID_Len := ID'Length;
            Mixer_Devs (I).Name (1 .. Name'Length) := Name;
            Mixer_Devs (I).Name_Len := Name'Length;
            Mixer_Devs (I).Card_Idx := Card_Idx;
            Idx := I;
            Devices.Register (Res, "mixer" & Natural (I)'Image, Success);
            return;
         end if;
      end loop;
      Success := False;
      Idx := 0;
   exception
      when Constraint_Error =>
         Success := False;
         Idx := 0;
   end Add_Mixer;

   procedure Get_Mixer_Properties
      (Idx      : Unsigned_32;
       ID       : out String;
       ID_Len   : out Natural;
       Name     : out String;
       Name_Len : out Natural;
       Card_Idx : out Unsigned_32)
   is
   begin
      if (Idx in Mixer_Devs'Range) and then (Mixer_Devs (Idx) /= null) then
         ID (ID'First .. ID'First - 1 + Mixer_ID_Len) :=
            Mixer_Devs (Idx).ID;
         ID_Len := Mixer_Devs (Idx).ID_Len;
         Name (Name'First .. Name'First - 1 + Mixer_Name_Len) :=
            Mixer_Devs (Idx).Name;
         Name_Len := Mixer_Devs (Idx).Name_Len;
         Card_Idx := Mixer_Devs (Idx).Card_Idx;
      else
         ID := [others => ' '];
         ID_Len := 0;
         Name := [others => ' '];
         Name_Len := 0;
         Card_Idx := 0;
      end if;
   exception
      when others =>
         ID := [others => ' '];
         ID_Len := 0;
         Name := [others => ' '];
         Name_Len := 0;
         Card_Idx := 0;
   end Get_Mixer_Properties;
   ----------------------------------------------------------------------------
   procedure Add_Audio_Device
      (Name      : String;
       Res       : Devices.Resource;
       Mixer_Idx : Unsigned_32;
       Card_Idx  : Unsigned_32;
       Idx       : out Unsigned_32;
       Success   : out Boolean)
   is
   begin
      for I in Audio_Devs'Range loop
         if Audio_Devs (I) = null then
            Audio_Devs (I) := new Device_Info;
            Audio_Devs (I).Name (1 .. Name'Length) := Name;
            Audio_Devs (I).Name_Len := Name'Length;
            Audio_Devs (I).Mixer_Idx := Mixer_Idx;
            Audio_Devs (I).Card_Idx := Card_Idx;
            Audio_Devs (I).Can_Read := Res.Read /= null;
            Audio_Devs (I).Can_Write := Res.Write /= null;
            Audio_Devs (I).Label := [others => ' '];
            Audio_Devs (I).Label_Len := 0;
            Audio_Devs (I).Song := [others => ' '];
            Audio_Devs (I).Song_Len := 0;
            Audio_Devs (I).Formats := 0;
            Audio_Devs (I).Min_Rate := 0;
            Audio_Devs (I).Max_Rate := 0;
            Audio_Devs (I).Min_Channels := 0;
            Audio_Devs (I).Max_Channels := 0;
            Audio_Devs (I).Caps := 0;
            Idx := I;
            Devices.Register (Res, "dsp" & Natural (I)'Image, Success);
            return;
         end if;
      end loop;
      Success := False;
      Idx := 0;
   exception
      when Constraint_Error =>
         Success := False;
         Idx := 0;
   end Add_Audio_Device;

   procedure Set_Audio_Device_Limits
      (Idx          : Unsigned_32;
       Formats      : Unsigned_32;
       Min_Rate     : Unsigned_32;
       Max_Rate     : Unsigned_32;
       Min_Channels : Unsigned_32;
       Max_Channels : Unsigned_32;
       Caps         : Unsigned_32)
   is
   begin
      if (Idx in Audio_Devs'Range) and then (Audio_Devs (Idx) /= null) then
         Audio_Devs (Idx).Formats      := Formats;
         Audio_Devs (Idx).Min_Rate     := Min_Rate;
         Audio_Devs (Idx).Max_Rate     := Max_Rate;
         Audio_Devs (Idx).Min_Channels := Min_Channels;
         Audio_Devs (Idx).Max_Channels := Max_Channels;
         Audio_Devs (Idx).Caps         := Caps;
      end if;
   exception
      when Constraint_Error =>
         null;
   end Set_Audio_Device_Limits;

   procedure Get_Audio_Device_Limits
      (Idx          : Unsigned_32;
       Formats      : out Unsigned_32;
       Min_Rate     : out Unsigned_32;
       Max_Rate     : out Unsigned_32;
       Min_Channels : out Unsigned_32;
       Max_Channels : out Unsigned_32;
       Caps         : out Unsigned_32)
   is
   begin
      Formats      := 0;
      Min_Rate     := 0;
      Max_Rate     := 0;
      Min_Channels := 0;
      Max_Channels := 0;
      Caps         := 0;
      if (Idx in Audio_Devs'Range) and then (Audio_Devs (Idx) /= null) then
         Formats      := Audio_Devs (Idx).Formats;
         Min_Rate     := Audio_Devs (Idx).Min_Rate;
         Max_Rate     := Audio_Devs (Idx).Max_Rate;
         Min_Channels := Audio_Devs (Idx).Min_Channels;
         Max_Channels := Audio_Devs (Idx).Max_Channels;
         Caps         := Audio_Devs (Idx).Caps;
      end if;
   exception
      when Constraint_Error =>
         null;
   end Get_Audio_Device_Limits;

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
   is
   begin
      if (Idx in Audio_Devs'Range) and then (Audio_Devs (Idx) /= null) then
         Name (Name'First .. Name'First - 1 + Audio_Name_Len) :=
            Audio_Devs (Idx).Name;
         Name_Len := Audio_Devs (Idx).Name_Len;
         Card_Idx := Audio_Devs (Idx).Card_Idx;
         Mixer_Idx := Audio_Devs (Idx).Mixer_Idx;
         Is_Input := Audio_Devs (Idx).Can_Read;
         Is_Output := Audio_Devs (Idx).Can_Write;
         Label (Label'First .. Label'First - 1 + Label_Name_Len) :=
            Audio_Devs (Idx).Label;
         Label_Len := Audio_Devs (Idx).Label_Len;
         Song (Song'First .. Song'First - 1 + Song_Name_Len) :=
            Audio_Devs (Idx).Song;
         Song_Len := Audio_Devs (Idx).Song_Len;
      else
         Name := [others => ' '];
         Name_Len := 0;
         Is_Input := False;
         Is_Output := False;
         Card_Idx := 0;
         Mixer_Idx := 0;
         Song := [others => ' '];
         Song_Len := 0;
         Label := [others => ' '];
         Label_Len := 0;
      end if;
   exception
      when others =>
         Name := [others => ' '];
         Is_Input := False;
         Is_Output := False;
         Name_Len := 0;
         Card_Idx := 0;
         Mixer_Idx := 0;
         Song := [others => ' '];
         Song_Len := 0;
         Label := [others => ' '];
         Label_Len := 0;
   end Get_Audio_Device_Properties;
   ----------------------------------------------------------------------------
   procedure Add_Card
      (Name_Short : String;
       Name_Long  : String;
       HW_Info    : String;
       Idx        : out Unsigned_32;
       Success    : out Boolean)
   is
   begin
      for I in Cards'Range loop
         if Cards (I) = null then
            Cards (I) := new Card_Info;
            Cards (I).Short_Name (1 .. Name_Short'Length) := Name_Short;
            Cards (I).Long_Name (1 .. Name_Long'Length) := Name_Long;
            Cards (I).HW_Info (1 .. HW_Info'Length) := HW_Info;
            Cards (I).Short_Len := Name_Short'Length;
            Cards (I).Long_Len := Name_Long'Length;
            Cards (I).Info_Len := HW_Info'Length;
            Idx := I;
            Success := True;
            return;
         end if;
      end loop;
      Success := False;
      Idx := 0;
   exception
      when Constraint_Error =>
         Success := False;
         Idx := 0;
   end Add_Card;

   procedure Get_Card_Name
      (Idx            : Unsigned_32;
       Name_Short     : out String;
       Name_Long      : out String;
       HW_Info        : out String;
       Name_Short_Len : out Natural;
       Name_Long_Len  : out Natural;
       HW_Info_Len    : out Natural)
   is
      NS : String renames Name_Short;
      NL : String renames Name_Long;
      HW : String renames HW_Info;
   begin
      if (Idx in Cards'Range) and then (Cards (Idx) /= null) then
         NS (NS'First .. NS'First - 1 + Card_Short_Name_Len) :=
            Cards (Idx).Short_Name;
         NL (NL'First .. NL'First - 1 + Card_Long_Name_Len) :=
            Cards (Idx).Long_Name;
         HW (HW'First .. HW'First - 1 + Card_HW_Info_Len) :=
            Cards (Idx).HW_Info;
         Name_Short_Len := Cards (Idx).Short_Len;
         Name_Long_Len := Cards (Idx).Long_Len;
         HW_Info_Len := Cards (Idx).Info_Len;
      else
         NS := [others => ' '];
         NL := [others => ' '];
         HW := [others => ' '];
         Name_Short_Len := 0;
         Name_Long_Len := 0;
         HW_Info_Len := 0;
      end if;
   exception
      when others =>
         NS := [others => ' '];
         NL := [others => ' '];
         HW := [others => ' '];
         Name_Short_Len := 0;
         Name_Long_Len := 0;
         HW_Info_Len := 0;
   end Get_Card_Name;
end Sound;
