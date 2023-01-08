--  devices-ata.adb: ATA driver.
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

with Arch.Snippets; use Arch.Snippets;
with System.Address_To_Access_Conversions;
with Lib.Messages;
with Devices.Partitions;

package body Devices.ATA with SPARK_Mode => Off is
   package Con is new System.Address_To_Access_Conversions (ATA_Data);

   function Init return Boolean is
      Base_Name  : String := "ata0";
      Drive_Data : ATA_Data_Acc;
   begin
      for I in 0 .. 3 loop
         Drive_Data := Init_Port (I);
         if Drive_Data /= null then
            Base_Name (4) := Character'Val (I + Character'Pos ('1'));
            if not Register ((
               Data => Con.To_Address (Con.Object_Pointer (Drive_Data)),
               Mutex             => Lib.Synchronization.Unlocked_Semaphore,
               Is_Block          => True,
               Block_Size        => Sector_Size,
               Block_Count       => Drive_Data.Sector_Count,
               Unique_Identifier => 0,
               Sync              => null,
               Read              => Read'Access,
               Write             => Write'Access,
               IO_Control        => null,
               Mmap              => null,
               Munmap            => null
            ), Base_Name) or
               not Partitions.Parse_Partitions (Base_Name, Fetch (Base_Name))
            then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Init;

   function Init_Port (Port_Index : Natural) return ATA_Data_Acc is
      Base_Port         : constant Unsigned_16 := Pair_Ports (Port_Index / 2);
      Is_Master         : constant Boolean     := (Port_Index mod 2) = 0;
      Data_Port         : constant Unsigned_16 := Base_Port;
      Error_Port        : constant Unsigned_16 := Base_Port + 16#001#;
      Sector_Count_Port : constant Unsigned_16 := Base_Port + 16#002#;
      LBA_Low_Port      : constant Unsigned_16 := Base_Port + 16#003#;
      LBA_Mid_Port      : constant Unsigned_16 := Base_Port + 16#004#;
      LBA_High_Port     : constant Unsigned_16 := Base_Port + 16#005#;
      Device_Port       : constant Unsigned_16 := Base_Port + 16#006#;
      Command_Port      : constant Unsigned_16 := Base_Port + 16#007#;
      Control_Port      : constant Unsigned_16 := Base_Port + 16#206#;
      Identify_Info     : ATA_Identify         := (others => 0);
      Timeout           : Natural              := 0;
      Status            : Unsigned_8;
   begin
      --  Identify the drive.
      Port_Out (Device_Port, (if Is_Master then 16#A0# else 16#B0#));
      Port_Out (Sector_Count_Port, 0);
      Port_Out (LBA_Low_Port, 0);
      Port_Out (LBA_Mid_Port, 0);
      Port_Out (LBA_High_Port, 0);
      Port_Out (Command_Port, 16#EC#);
      if Port_In (Command_Port) = 0 then
         return null;
      end if;
      while (Port_In (Command_Port) and 2#10000000#) /= 0 loop
         if Timeout = 100_000 then
            Lib.Messages.Warn ("ATA drive timed out, skipping it...");
            return null;
         end if;
         Timeout := Timeout + 1;
      end loop;

      --  Check for non-standard ATAPI.
      if Port_In (LBA_Mid_Port) /= 0 or Port_In (LBA_High_Port) /= 0 then
         Lib.Messages.Warn ("Ignoring non-standard ATAPI ATA drive");
         return null;
      end if;

      --  Check for results.
      Timeout := 0;
      loop
         Status := Port_In (Command_Port);
         if (Status and 1) /= 0 then
            Lib.Messages.Warn ("ATA drive errored out during identify");
            return null;
         end if;
         exit when (Status and 2#1000#) /= 0;
         if Timeout = 100_000 then
            Lib.Messages.Warn ("ATA drive timed out in identify");
            return null;
         end if;
         Timeout := Timeout + 1;
      end loop;

      --  Store the identify.
      for Id of Identify_Info loop
         Id := Port_In16 (Data_Port);
      end loop;

      --  Return the initialized drive.
      declare
         Sector_Count : Unsigned_64 with
            Import, Address => Identify_Info (101)'Address;
      begin
         return new ATA_Data'(
            Is_Master         => Is_Master,
            Identify          => Identify_Info,
            Data_Port         => Data_Port,
            Error_Port        => Error_Port,
            Sector_Count_Port => Sector_Count_Port,
            LBA_Low_Port      => LBA_Low_Port,
            LBA_Mid_Port      => LBA_Mid_Port,
            LBA_High_Port     => LBA_High_Port,
            Device_Port       => Device_Port,
            Command_Port      => Command_Port,
            Control_Port      => Control_Port,
            Sector_Count      => Sector_Count
         );
      end;
   end Init_Port;

   function Read_Write
      (Drive         : ATA_Data_Acc;
       Offset_Sector : Unsigned_64;
       Count_Sector  : Unsigned_64;
       Is_Write      : Boolean;
       Data_Addr     : System.Address) return Boolean
   is
      Data_Buffer : array (1 .. Count_Sector * Sector_Size) of Unsigned_8
         with Import, Address => Data_Addr;
      Index   : Unsigned_64 := 1;
      Request : Unsigned_64;
      LBA0, LBA8, LBA16, LBA24, LBA32, LBA40 : Unsigned_8;
      Status : Unsigned_8;
      Data   : Unsigned_16;
   begin
      for Sector in 1 .. Count_Sector loop
         --  Separate the address into LBA.
         Request := Offset_Sector + Sector - 1;
         LBA0  := Unsigned_8 (Shift_Right (Request and 16#0000000000FF#,  0));
         LBA8  := Unsigned_8 (Shift_Right (Request and 16#00000000FF00#,  8));
         LBA16 := Unsigned_8 (Shift_Right (Request and 16#000000FF0000#, 16));
         LBA24 := Unsigned_8 (Shift_Right (Request and 16#0000FF000000#, 24));
         LBA32 := Unsigned_8 (Shift_Right (Request and 16#00FF00000000#, 32));
         LBA40 := Unsigned_8 (Shift_Right (Request and 16#FF0000000000#, 40));

         if Drive.Is_Master then
            Port_Out (Drive.Device_Port, (16#40#));
         else
            Port_Out (Drive.Device_Port, (16#50#));
         end if;
         Port_Out (Drive.Sector_Count_Port, 0);
         Port_Out (Drive.LBA_Low_Port,  LBA24);
         Port_Out (Drive.LBA_Mid_Port,  LBA32);
         Port_Out (Drive.LBA_High_Port, LBA40);
         Port_Out (Drive.Sector_Count_Port, 1);
         Port_Out (Drive.LBA_Low_Port,  LBA0);
         Port_Out (Drive.LBA_Mid_Port,  LBA8);
         Port_Out (Drive.LBA_High_Port, LBA16);
         Port_Out (Drive.Command_Port, (if Is_Write then 16#34# else 16#24#));

         Status := Port_In (Drive.Command_Port);
         while (Status and 16#80#)  = 16#80# and
               (Status and 16#01#) /= 16#01#
         loop
            Status := Port_In (Drive.Command_Port);
         end loop;

         if (Status and 16#01#) /= 0 then
            Lib.Messages.Warn ("ATA error while operating on a sector");
            return False;
         end if;

         if Is_Write then
            for I in 1 .. 256 loop
               Data := Shift_Left (Unsigned_16 (Data_Buffer (Index)), 8) or
                       Unsigned_16 (Data_Buffer (Index));
               Index := Index + 2;
               Port_Out16 (Drive.Data_Port, Data);
            end loop;
         else
            for I in 1 .. 256 loop
               Data                    := Port_In16 (Drive.Data_Port);
               Data_Buffer (Index)     := Unsigned_8 (Data and 16#FF#);
               Data_Buffer (Index + 1) := Unsigned_8 (Shift_Right (Data, 8));
               Index                   := Index + 2;
            end loop;
         end if;
      end loop;
      return True;
   end Read_Write;
   ----------------------------------------------------------------------------
   function Read
      (Data   : Resource_Acc;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      Success : Boolean;
      Drive   : constant ATA_Data_Acc :=
         ATA_Data_Acc (Con.To_Pointer (Data.Data));
   begin
      --  TODO: Instead of returning EOF, return error.
      if Offset mod Sector_Size /= 0 or Count mod Sector_Size /= 0 then
         return 0;
      end if;

      Lib.Synchronization.Seize (Data.Mutex);
      Success := Read_Write (
         Drive         => Drive,
         Offset_Sector => Offset / Sector_Size,
         Count_Sector  => (Count / Sector_Size) + 1,
         Is_Write      => False,
         Data_Addr     => Desto
      );
      Lib.Synchronization.Release (Data.Mutex);
      if Success then
         return Count;
      else
         return 0;
      end if;
   end Read;

   function Write
      (Data     : Resource_Acc;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      Success : Boolean;
      Drive   : constant ATA_Data_Acc :=
         ATA_Data_Acc (Con.To_Pointer (Data.Data));
   begin
      --  TODO: Instead of returning EOF, return error.
      if Offset mod Sector_Size /= 0 or Count mod Sector_Size /= 0 then
         return 0;
      end if;

      Lib.Synchronization.Seize (Data.Mutex);
      Success := Read_Write (
         Drive         => Drive,
         Offset_Sector => Offset / Sector_Size,
         Count_Sector  => (Count / Sector_Size) + 1,
         Is_Write      => True,
         Data_Addr     => To_Write
      );
      Lib.Synchronization.Release (Data.Mutex);
      if Success then
         return Count;
      else
         return 0;
      end if;
   end Write;
end Devices.ATA;
