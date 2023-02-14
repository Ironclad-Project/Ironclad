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
      Success    : Boolean;
   begin
      for I in 0 .. 3 loop
         Drive_Data := Init_Port (I);
         if Drive_Data /= null then
            Base_Name (4) := Character'Val (I + 1 + Character'Pos ('0'));
            Register (
               (Data => Con.To_Address (Con.Object_Pointer (Drive_Data)),
                Mutex       => Lib.Synchronization.Unlocked_Semaphore,
                Is_Block    => True,
                Block_Size  => Sector_Size,
                Block_Count => Drive_Data.Sector_Count,
                Safe_Read   => Read'Access,
                Safe_Write  => Write'Access,
                Sync        => Sync'Access,
                Read        => null,
                Write       => null,
                IO_Control  => null,
                Mmap        => null,
                Munmap      => null), Base_Name, Success);
            if not Success or else
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
         Sectors : Unsigned_64 with Address => Identify_Info (101)'Address;
      begin
         return new ATA_Data'
            (Is_Master     => Is_Master,
             Identify      => Identify_Info,
             Data_Port     => Data_Port,
             Error_Port    => Error_Port,
             Count_Port    => Sector_Count_Port,
             LBA_Low_Port  => LBA_Low_Port,
             LBA_Mid_Port  => LBA_Mid_Port,
             LBA_High_Port => LBA_High_Port,
             Device_Port   => Device_Port,
             Command_Port  => Command_Port,
             Control_Port  => Control_Port,
             Sector_Count  => Sectors,
             Caches        => (others => (Is_Used => False, others => <>)),
             Next_Evict    => 1);
      end;
   end Init_Port;

   function Read_Sector
      (Drive       : ATA_Data_Acc;
       LBA         : Unsigned_64;
       Data_Buffer : out Sector_Data) return Boolean
   is
      Data  : Unsigned_16;
      Index : Natural := 1;
   begin
      if not Issue_Command (Drive, LBA, 16#24#) then
         return False;
      end if;

      for I in 1 .. 256 loop
         Data                    := Port_In16 (Drive.Data_Port);
         Data_Buffer (Index)     := Unsigned_8 (Data and 16#FF#);
         Data_Buffer (Index + 1) := Unsigned_8 (Shift_Right (Data, 8));
         Index                   := Index + 2;
      end loop;

      return True;
   end Read_Sector;

   function Write_Sector
      (Drive       : ATA_Data_Acc;
       LBA         : Unsigned_64;
       Data_Buffer : Sector_Data) return Boolean
   is
      Data  : Unsigned_16;
      Index : Natural := 1;
   begin
      if not Issue_Command (Drive, LBA, 16#34#) then
         return False;
      end if;

      for I in 1 .. 256 loop
         Data := Shift_Left (Unsigned_16 (Data_Buffer (Index + 1)), 8) or
                 Unsigned_16 (Data_Buffer (Index));
         Index := Index + 2;
         Port_Out16 (Drive.Data_Port, Data);
      end loop;

      Port_Out (Drive.Command_Port, 16#EA#);
      if not Poll_Error (Drive.Command_Port) then
         Lib.Messages.Warn ("ATA error while flushing");
         return False;
      end if;

      return True;
   end Write_Sector;

   function Issue_Command
      (Drive : ATA_Data_Acc;
       LBA   : Unsigned_64;
       Cmd   : Unsigned_8) return Boolean
   is
      LBA0, LBA8, LBA16, LBA24, LBA32, LBA40 : Unsigned_8;
   begin
      LBA0  := Unsigned_8 (Shift_Right (LBA and 16#0000000000FF#,  0));
      LBA8  := Unsigned_8 (Shift_Right (LBA and 16#00000000FF00#,  8));
      LBA16 := Unsigned_8 (Shift_Right (LBA and 16#000000FF0000#, 16));
      LBA24 := Unsigned_8 (Shift_Right (LBA and 16#0000FF000000#, 24));
      LBA32 := Unsigned_8 (Shift_Right (LBA and 16#00FF00000000#, 32));
      LBA40 := Unsigned_8 (Shift_Right (LBA and 16#FF0000000000#, 40));

      if Drive.Is_Master then
         Port_Out (Drive.Device_Port, (16#40#));
      else
         Port_Out (Drive.Device_Port, (16#50#));
      end if;

      Port_Out (Drive.Count_Port, 0);
      Port_Out (Drive.LBA_Low_Port,  LBA24);
      Port_Out (Drive.LBA_Mid_Port,  LBA32);
      Port_Out (Drive.LBA_High_Port, LBA40);
      Port_Out (Drive.Count_Port, 1);
      Port_Out (Drive.LBA_Low_Port,  LBA0);
      Port_Out (Drive.LBA_Mid_Port,  LBA8);
      Port_Out (Drive.LBA_High_Port, LBA16);
      Port_Out (Drive.Command_Port, Cmd);

      if not Poll_Error (Drive.Command_Port) then
         Lib.Messages.Warn ("ATA error while operating on a sector");
         return False;
      else
         return True;
      end if;
   end Issue_Command;

   function Poll_Error (Port : Unsigned_16) return Boolean is
      Status : Unsigned_8;
   begin
      Status := Port_In (Port);
      while (Status and 16#80#) = 16#80# and (Status and 16#01#) /= 16#01# loop
         Status := Port_In (Port);
      end loop;
      return (Status and 16#01#) = 0;
   end Poll_Error;

   function Get_Cache_Index
      (Drive : ATA_Data_Acc;
       LBA   : Unsigned_64) return Natural
   is
      Success  : Boolean;
      Returned : Natural := 0;
   begin
      for I in Drive.Caches'Range loop
         if Drive.Caches (I).Is_Used and Drive.Caches (I).LBA_Offset = LBA then
            return I;
         elsif not Drive.Caches (I).Is_Used and Returned = 0 then
            Returned := I;
         end if;
      end loop;

      if Returned = 0 then
         Returned := Drive.Next_Evict;

         if Drive.Caches (Drive.Next_Evict).Is_Dirty then
            Success := Write_Sector
               (Drive       => Drive,
                LBA         => Drive.Caches (Returned).LBA_Offset,
                Data_Buffer => Drive.Caches (Returned).Data);
            if not Success then
               Lib.Messages.Warn ("ata could not write on cache fetching!");
            end if;
         end if;

         if Drive.Next_Evict = Drive.Caches'Last then
            Drive.Next_Evict := Drive.Caches'First;
         else
            Drive.Next_Evict := Drive.Next_Evict + 1;
         end if;
      end if;

      Drive.Caches (Returned) :=
         (Is_Used    => True,
          LBA_Offset => LBA,
          Is_Dirty   => False,
          Data       => <>);

      Success := Read_Sector
         (Drive       => Drive,
          LBA         => Drive.Caches (Returned).LBA_Offset,
          Data_Buffer => Drive.Caches (Returned).Data);
      if not Success then
         Lib.Messages.Warn ("ata could not read on cache fetching!");
      end if;

      return Returned;
   end Get_Cache_Index;
   ----------------------------------------------------------------------------
   procedure Read
      (Key       : Resource_Acc;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      D : constant ATA_Data_Acc := ATA_Data_Acc (Con.To_Pointer (Key.Data));
      Cache_Idx, Progress, Copy_Count, Cache_Offset : Natural := 0;
      Current_LBA : Unsigned_64;
   begin
      if Data'Length = 0 then
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      Lib.Synchronization.Seize (Key.Mutex);
      while Progress < Data'Length loop
         Current_LBA  := (Offset + Unsigned_64 (Progress)) / Sector_Size;
         Cache_Idx    := Get_Cache_Index (D, Current_LBA);
         Copy_Count   := Data'Length - Progress;
         Cache_Offset := Natural ((Offset + Unsigned_64 (Progress)) mod
                                  Sector_Size);
         if Copy_Count > Sector_Size - Cache_Offset then
            Copy_Count := Sector_Size - Cache_Offset;
         end if;
         Data (Data'First + Progress .. Data'First + Progress + Copy_Count - 1)
            := D.Caches (Cache_Idx).Data (Cache_Offset + 1 ..
                                          Cache_Offset + Copy_Count);
         Progress := Progress + Copy_Count;
      end loop;
      Lib.Synchronization.Release (Key.Mutex);

      Ret_Count := Progress;
      Success   := True;
   end Read;

   procedure Write
      (Key       : Resource_Acc;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      D : constant ATA_Data_Acc := ATA_Data_Acc (Con.To_Pointer (Key.Data));
      Cache_Idx, Progress, Copy_Count, Cache_Offset : Natural := 0;
      Current_LBA : Unsigned_64;
   begin
      if Data'Length = 0 then
         Ret_Count := 0;
         Success   := False;
         return;
      end if;

      Lib.Synchronization.Seize (Key.Mutex);
      while Progress < Data'Length loop
         Current_LBA  := (Offset + Unsigned_64 (Progress)) / Sector_Size;
         Cache_Idx    := Get_Cache_Index (D, Current_LBA);
         Copy_Count   := Data'Length - Progress;
         Cache_Offset := Natural ((Offset + Unsigned_64 (Progress)) mod
                                  Sector_Size);
         if Copy_Count > Sector_Size - Cache_Offset then
            Copy_Count := Sector_Size - Cache_Offset;
         end if;
         D.Caches (Cache_Idx).Data (Cache_Offset + 1 ..
                                    Cache_Offset + Copy_Count) :=
            Data (Data'First + Progress ..
                  Data'First + Progress + Copy_Count - 1);
         D.Caches (Cache_Idx).Is_Dirty := True;
         Progress := Progress + Copy_Count;
      end loop;
      Lib.Synchronization.Release (Key.Mutex);

      Ret_Count := Progress;
      Success   := True;
   end Write;

   procedure Sync (Key : Resource_Acc) is
      Success : Boolean;
      D : constant ATA_Data_Acc := ATA_Data_Acc (Con.To_Pointer (Key.Data));
   begin
      Lib.Synchronization.Seize (Key.Mutex);
      for Cache of D.Caches loop
         if Cache.Is_Used and Cache.Is_Dirty then
            Success := Write_Sector
               (Drive       => D,
                LBA         => Cache.LBA_Offset,
                Data_Buffer => Cache.Data);
            if not Success then
               Lib.Messages.Warn ("ata could not write on sync!");
            end if;

            Cache.Is_Dirty := False;
         end if;
      end loop;
      Lib.Synchronization.Release (Key.Mutex);
   end Sync;
end Devices.ATA;
