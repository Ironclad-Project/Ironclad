--  devices-ata.adb: ATA driver.
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

with Arch.Snippets; use Arch.Snippets;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;
with Lib.Messages;
with Devices.Partitions;
with Arch.PCI;

package body Devices.ATA is
   package Con is new System.Address_To_Access_Conversions (ATA_Data);

   function Init return Boolean is
      PCI_Dev    : Arch.PCI.PCI_Device;
      PCI_BAR0   : Arch.PCI.Base_Address_Register;
      PCI_BAR2   : Arch.PCI.Base_Address_Register;
      Drive_Data : ATA_Data_Acc;
      Drive_Idx  : Natural := 0;
      Success    : Boolean;
      Pair_Ports : array (0 .. 1) of Unsigned_16;
      Base_Name  : constant String := "ata";
      Num_Str    : Lib.Messages.Translated_String;
      Num_Len    : Natural;
   begin
      for Idx in 1 .. Arch.PCI.Enumerate_Devices (16#1#, 16#1#, 16#80#) loop
         Arch.PCI.Search_Device (16#1#, 16#1#, 16#80#, Idx, PCI_Dev, Success);
         if not Success then
            return False;
         end if;

         Arch.PCI.Get_BAR (PCI_Dev, 0, PCI_BAR0, Success);
         if not Success then
            PCI_BAR0.Base := 16#1F0#;
         end if;
         Arch.PCI.Get_BAR (PCI_Dev, 2, PCI_BAR2, Success);
         if not Success then
            PCI_BAR2.Base := 16#170#;
         end if;

         Pair_Ports (0) := Unsigned_16 (PCI_BAR0.Base and 16#FFFF#);
         Pair_Ports (1) := Unsigned_16 (PCI_BAR2.Base and 16#FFFF#);

         for I in 0 .. 3 loop
            Drive_Data := Init_Port (Pair_Ports (I / 2), (I mod 2) = 0);
            if Drive_Data /= null then
               Drive_Idx := Drive_Idx + 1;

               Lib.Messages.Image (Unsigned_32 (Drive_Idx), Num_Str, Num_Len);

               declare
                  Final_Name : constant String := Base_Name &
                     Num_Str (Num_Str'Last - Num_Len + 1 .. Num_Str'Last);
               begin
                  Register (
                     (Data => Con.To_Address (Con.Object_Pointer (Drive_Data)),
                      ID          => (others => 0),
                      Is_Block    => True,
                      Block_Size  => Sector_Size,
                      Block_Count => Drive_Data.Sector_Count,
                      Read        => Read'Access,
                      Write       => Write'Access,
                      Sync        => Sync'Access,
                      Sync_Range  => Sync_Range'Access,
                      IO_Control  => null,
                      Mmap        => null,
                      Poll        => null), Final_Name, Success);
                  if not Success or else
                     not Partitions.Parse_Partitions
                        (Final_Name, Fetch (Final_Name))
                  then
                     return False;
                  end if;
               end;
            end if;
         end loop;
      end loop;

      return True;
   end Init;

   function Init_Port
      (Base_Port : Unsigned_16;
       Is_Master : Boolean) return ATA_Data_Acc
   is
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
            Lib.Messages.Put_Line ("ATA drive timed out, skipping it...");
            return null;
         end if;
         Timeout := Timeout + 1;
      end loop;

      --  Check for non-standard ATAPI.
      if Port_In (LBA_Mid_Port) /= 0 or Port_In (LBA_High_Port) /= 0 then
         Lib.Messages.Put_Line ("Ignoring non-standard ATAPI ATA drive");
         return null;
      end if;

      --  Check for results.
      Timeout := 0;
      loop
         Status := Port_In (Command_Port);
         if (Status and 1) /= 0 then
            Lib.Messages.Put_Line ("ATA drive errored out during identify");
            return null;
         end if;
         exit when (Status and 2#1000#) /= 0;
         if Timeout = 100_000 then
            Lib.Messages.Put_Line ("ATA drive timed out in identify");
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
            (Mutex         => Lib.Synchronization.Unlocked_Semaphore,
             Is_Master     => Is_Master,
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

   procedure Read_Sector
      (Drive       : ATA_Data_Acc;
       LBA         : Unsigned_64;
       Data_Buffer : out Sector_Data;
       Success     : out Boolean)
   is
      Data  : Unsigned_16;
      Index : Natural := 1;
   begin
      if not Issue_Command (Drive, LBA, 16#24#) then
         Success := False;
         return;
      end if;

      for I in 1 .. 256 loop
         Data                    := Port_In16 (Drive.Data_Port);
         Data_Buffer (Index)     := Unsigned_8 (Data and 16#FF#);
         Data_Buffer (Index + 1) := Unsigned_8 (Shift_Right (Data, 8));
         Index                   := Index + 2;
      end loop;

      Success := True;
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
         Lib.Messages.Put_Line ("ATA error while flushing");
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
         Lib.Messages.Put_Line ("ATA error while operating on a sector");
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

   procedure Get_Cache_Index
      (Drive   : ATA_Data_Acc;
       LBA     : Unsigned_64;
       Idx     : out Natural;
       Success : out Boolean)
   is
   begin
      Idx := 0;

      for I in Drive.Caches'Range loop
         if Drive.Caches (I).Is_Used and Drive.Caches (I).LBA_Offset = LBA then
            Idx     := I;
            Success := True;
            return;
         elsif not Drive.Caches (I).Is_Used and Idx = 0 then
            Idx := I;
         end if;
      end loop;

      if Idx = 0 then
         Idx := Drive.Next_Evict;

         if Drive.Caches (Drive.Next_Evict).Is_Dirty then
            Success := Write_Sector
               (Drive       => Drive,
                LBA         => Drive.Caches (Idx).LBA_Offset,
                Data_Buffer => Drive.Caches (Idx).Data);
            if not Success then
               Lib.Messages.Put_Line ("ATA could not write on cache fetch!");
               return;
            end if;
         end if;

         if Drive.Next_Evict = Drive.Caches'Last then
            Drive.Next_Evict := Drive.Caches'First;
         else
            Drive.Next_Evict := Drive.Next_Evict + 1;
         end if;
      end if;

      Drive.Caches (Idx) :=
         (Is_Used    => True,
          LBA_Offset => LBA,
          Is_Dirty   => False,
          Data       => <>);

      Read_Sector
         (Drive       => Drive,
          LBA         => Drive.Caches (Idx).LBA_Offset,
          Data_Buffer => Drive.Caches (Idx).Data,
          Success     => Success);
      if not Success then
         Lib.Messages.Put_Line ("ATA could not read on cache fetch!");
      end if;
   end Get_Cache_Index;
   ----------------------------------------------------------------------------
   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Is_Blocking);
      D : constant ATA_Data_Acc := ATA_Data_Acc (Con.To_Pointer (Key));
      Cache_Idx, Progress, Copy_Count, Cache_Offset : Natural := 0;
      Current_LBA : Unsigned_64;
   begin
      Success := True;
      Lib.Synchronization.Seize (D.Mutex);
      while Progress < Data'Length loop
         Current_LBA := (Offset + Unsigned_64 (Progress)) / Sector_Size;

         Get_Cache_Index
          (Drive   => D,
           LBA     => Current_LBA,
           Idx     => Cache_Idx,
           Success => Success);
         if not Success then
            Success := True;
            goto Cleanup;
         end if;

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

   <<Cleanup>>
      Lib.Synchronization.Release (D.Mutex);
      Ret_Count := Progress;
   end Read;

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Is_Blocking);
      D : constant ATA_Data_Acc := ATA_Data_Acc (Con.To_Pointer (Key));
      Cache_Idx, Progress, Copy_Count, Cache_Offset : Natural := 0;
      Current_LBA : Unsigned_64;
   begin
      Success := True;
      Lib.Synchronization.Seize (D.Mutex);
      while Progress < Data'Length loop
         Current_LBA := (Offset + Unsigned_64 (Progress)) / Sector_Size;

         Get_Cache_Index
          (Drive   => D,
           LBA     => Current_LBA,
           Idx     => Cache_Idx,
           Success => Success);
         if not Success then
            Success := Progress /= 0;
            goto Cleanup;
         end if;

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

   <<Cleanup>>
      Lib.Synchronization.Release (D.Mutex);
      Ret_Count := Progress;
   end Write;

   function Sync (Key : System.Address) return Boolean is
      Drive   : constant ATA_Data_Acc := ATA_Data_Acc (Con.To_Pointer (Key));
      Success : Boolean := True;
   begin
      Lib.Synchronization.Seize (Drive.Mutex);
      for Cache of Drive.Caches loop
         if Cache.Is_Used and Cache.Is_Dirty then
            if not Write_Sector (Drive, Cache.LBA_Offset, Cache.Data) then
               Success := False;
               goto Cleanup;
            end if;
            Cache.Is_Dirty := False;
         end if;
      end loop;

   <<Cleanup>>
      Lib.Synchronization.Release (Drive.Mutex);
      return Success;
   end Sync;

   function Sync_Range
      (Key    : System.Address;
       Offset : Unsigned_64;
       Count  : Unsigned_64) return Boolean
   is
      Drive     : constant ATA_Data_Acc := ATA_Data_Acc (Con.To_Pointer (Key));
      First_LBA : constant  Unsigned_64 := Offset / Sector_Size;
      Last_LBA  : constant  Unsigned_64 := (Offset + Count) / Sector_Size;
      Success   : Boolean := True;
   begin
      Lib.Synchronization.Seize (Drive.Mutex);

      for Cache of Drive.Caches loop
         if Cache.Is_Used                 and
            Cache.Is_Dirty                and
            Cache.LBA_Offset >= First_LBA and
            Cache.LBA_Offset <= Last_LBA
         then
            if not Write_Sector (Drive, Cache.LBA_Offset, Cache.Data) then
               Success := False;
               goto Cleanup;
            end if;
            Cache.Is_Dirty := False;
         end if;
      end loop;

   <<Cleanup>>
      Lib.Synchronization.Release (Drive.Mutex);
      return Success;
   end Sync_Range;
end Devices.ATA;
