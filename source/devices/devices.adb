--  devices.adb: Device management.
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

with Devices.Console;
with Devices.Loopback;
with Devices.Streams;
with Lib.Panic;
with Arch.Hooks;

package body Devices is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   procedure Init is
      pragma SPARK_Mode (Off); --  Some devices here are not verified.
      Success : Boolean;
   begin
      Devices_Data := new Device_Arr;
      for Dev of Devices_Data.all loop
         Dev.Is_Present := False;
      end loop;


      Console.Init (Success);
      if not Success then goto Panic_Error; end if;
      Loopback.Init (Success);
      if not Success then goto Panic_Error; end if;
      Streams.Init (Success);
      if not Success or else not Arch.Hooks.Devices_Hook then
         goto Panic_Error;
      end if;

      return;

   <<Panic_Error>>
      Lib.Panic.Hard_Panic ("Some devices could not be added");
   end Init;

   procedure Register (Dev : Resource; Name : String; Success : out Boolean) is
   begin
      --  Search if the name is already taken.
      Success := False;
      for E of Devices_Data.all loop
         if E.Is_Present and then E.Name (1 .. E.Name_Len) = Name then
            return;
         end if;
      end loop;

      --  Allocate.
      for I in Devices_Data'Range loop
         if not Devices_Data (I).Is_Present then
            Devices_Data (I).Is_Present              := True;
            Devices_Data (I).Name (1 .. Name'Length) := Name;
            Devices_Data (I).Name_Len                := Name'Length;
            Devices_Data (I).Contents                := Dev;
            Success := True;
            exit;
         end if;
      end loop;
   end Register;

   function Fetch (Name : String) return Device_Handle is
   begin
      for I in Devices_Data'Range loop
         if Devices_Data (I).Is_Present and
            Devices_Data (I).Name (1 .. Devices_Data (I).Name_Len) = Name
         then
            return I;
         end if;
      end loop;
      return Error_Handle;
   end Fetch;

   function Fetch (ID : UUID) return Device_Handle is
   begin
      if ID /= Zero_UUID then
         for I in Devices_Data'Range loop
            if Devices_Data (I).Is_Present and
               Devices_Data (I).Contents.ID = ID
            then
               return I;
            end if;
         end loop;
      end if;
      return Error_Handle;
   end Fetch;

   function Fetch_UUID (ID : UUID_String) return Device_Handle is
      Result   : UUID := Zero_UUID;
      Returned : Device_Handle;
   begin
      --  UUID string formats are pretty messy. Representation is integer
      --  based, but different UUID versions have different endianess, either
      --  mixed or BE-only, so we need to try both.

      --  Check validity.
      for I in ID'Range loop
         case I is
            when 9 | 14 | 19 | 24 =>
               if ID (I) /= '-' then
                  return Error_Handle;
               end if;
            when others =>
               if not (ID (I) in '0' .. '9') and
                  not (ID (I) in 'a' .. 'f') and
                  not (ID (I) in 'A' .. 'F')
               then
                  return Error_Handle;
               end if;
         end case;
      end loop;

      --  Try LE-BE mix first.
      Convert_LE (UUID_Fragment (Result (1 .. 4)),  ID (1 .. 8));
      Convert_LE (UUID_Fragment (Result (5 .. 6)),  ID (10 .. 13));
      Convert_LE (UUID_Fragment (Result (7 .. 8)),  ID (15 .. 18));
      Convert_BE (UUID_Fragment (Result (9 .. 10)), ID (20 .. 23));
      Convert_BE (UUID_Fragment (Result (11 .. 16)), ID (25 .. 36));
      Returned := Fetch (Result);
      if Returned /= Error_Handle then
         return Returned;
      end if;

      --  Try all BE.
      Convert_BE (UUID_Fragment (Result (1 .. 4)),  ID (1 .. 8));
      Convert_BE (UUID_Fragment (Result (5 .. 6)),  ID (10 .. 13));
      Convert_BE (UUID_Fragment (Result (7 .. 8)),  ID (15 .. 18));
      Convert_BE (UUID_Fragment (Result (9 .. 10)), ID (20 .. 23));
      Convert_BE (UUID_Fragment (Result (11 .. 16)), ID (25 .. 36));
      return Fetch (Result);
   end Fetch_UUID;

   function Fetch (Dev : Device_Handle) return UUID is
   begin
      return Devices_Data (Dev).Contents.ID;
   end Fetch;

   procedure Fetch_Name
      (Handle : Device_Handle;
       Name   : out String;
       Length : out Natural)
   is
   begin
      Name   := Devices_Data (Handle).Name;
      Length := Devices_Data (Handle).Name_Len;
   end Fetch_Name;

   procedure List (Buffer : out Device_List; Total : out Natural) is
      Curr_Index : Natural := 0;
   begin
      Total  := 0;
      Buffer := (others => Error_Handle);

      for I in Devices_Data'Range loop
         pragma Loop_Invariant (Total <= Devices_Data'Length);
         if Devices_Data (I).Is_Present then
            Total := Total + 1;
            if Curr_Index < Buffer'Length then
               Buffer (Buffer'First + Curr_Index) :=  I;
               Curr_Index := Curr_Index + 1;
            end if;
         end if;
      end loop;

      for I in Total + 1 .. Buffer'Length loop
         Buffer (Buffer'First + I - 1) := Error_Handle;
      end loop;
   end List;
   ----------------------------------------------------------------------------
   function Is_Block_Device (Handle : Device_Handle) return Boolean is
   begin
      return Devices_Data (Handle).Contents.Is_Block;
   end Is_Block_Device;

   function Get_Block_Count (Handle : Device_Handle) return Unsigned_64 is
   begin
      return Devices_Data (Handle).Contents.Block_Count;
   end Get_Block_Count;

   function Get_Block_Size (Handle : Device_Handle) return Natural is
   begin
      return Devices_Data (Handle).Contents.Block_Size;
   end Get_Block_Size;

   function Get_Unique_ID (Handle : Device_Handle) return Natural is
   begin
      --  Since handles are unique, reuse them as unique ID.
      return Natural (Handle);
   end Get_Unique_ID;

   function From_Unique_ID (ID : Natural) return Device_Handle is
   begin
      return Device_Handle (ID);
   end From_Unique_ID;

   function Is_Read_Only (Handle : Device_Handle) return Boolean is
   begin
      return Devices_Data (Handle).Contents.Write = null;
   end Is_Read_Only;

   function Synchronize (Handle : Device_Handle) return Boolean is
      Success : Boolean := True;
   begin
      if Devices_Data (Handle).Contents.Sync /= null then
         Success := Devices_Data (Handle).Contents.Sync
            (Devices_Data (Handle).Contents.Data);
      end if;
      return Success;
   end Synchronize;

   function Synchronize
      (Handle : Device_Handle;
       Offset : Unsigned_64;
       Count  : Unsigned_64) return Boolean
   is
      Success : Boolean := True;
   begin
      if Devices_Data (Handle).Contents.Sync_Range /= null then
         Success := Devices_Data (Handle).Contents.Sync_Range
            (Devices_Data (Handle).Contents.Data, Offset, Count);
      elsif Devices_Data (Handle).Contents.Sync /= null then
         Success := Devices_Data (Handle).Contents.Sync
            (Devices_Data (Handle).Contents.Data);
      end if;
      return Success;
   end Synchronize;

   procedure Read
      (Handle      : Device_Handle;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean := True)
   is
   begin
      if Devices_Data (Handle).Contents.Read /= null then
         Devices_Data (Handle).Contents.Read
            (Key       => Devices_Data (Handle).Contents.Data,
             Offset    => Offset,
             Data      => Data,
             Ret_Count => Ret_Count,
             Success   => Success,
             Is_Blocking => Is_Blocking);
      else
         Data      := (others => 0);
         Ret_Count := 0;
         Success   := False;
      end if;
   end Read;

   procedure Write
      (Handle      : Device_Handle;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean := True)
   is
   begin
      if Devices_Data (Handle).Contents.Write /= null then
         Devices_Data (Handle).Contents.Write
            (Key       => Devices_Data (Handle).Contents.Data,
             Offset    => Offset,
             Data      => Data,
             Ret_Count => Ret_Count,
             Success   => Success,
             Is_Blocking => Is_Blocking);
      else
         Ret_Count := 0;
         Success   := False;
      end if;
   end Write;

   function IO_Control
      (Handle   : Device_Handle;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
   begin
      if Devices_Data (Handle).Contents.IO_Control /= null then
         return Devices_Data (Handle).Contents.IO_Control
            (Devices_Data (Handle).Contents.Data, Request, Argument);
      else
         return False;
      end if;
   end IO_Control;

   function Mmap
      (Handle  : Device_Handle;
       Map     : Arch.MMU.Page_Table_Acc;
       Address : Memory.Virtual_Address;
       Length  : Unsigned_64;
       Flags   : Arch.MMU.Page_Permissions) return Boolean
   is
   begin
      if Devices_Data (Handle).Contents.Mmap /= null then
         return Devices_Data (Handle).Contents.Mmap
            (Devices_Data (Handle).Contents.Data, Map, Address, Length, Flags);
      else
         return False;
      end if;
   end Mmap;

   procedure Poll
      (Handle    : Device_Handle;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
   begin
      if Devices_Data (Handle).Contents.Poll /= null then
         Devices_Data (Handle).Contents.Poll
            (Devices_Data (Handle).Contents.Data, Can_Read, Can_Write,
             Is_Error);
      else
         Can_Read  := False;
         Can_Write := False;
         Is_Error  := True;
      end if;
   end Poll;
   ----------------------------------------------------------------------------
   function To_Integer (C : Character) return Unsigned_8 is
      Result : Natural;
   begin
      if C in '0' .. '9' then
         Result := Character'Pos (C) - Character'Pos ('0');
      elsif C in 'a' .. 'f' then
         Result := Character'Pos (C) - Character'Pos ('a') + 10;
      else
         Result := Character'Pos (C) - Character'Pos ('A') + 10;
      end if;
      return Unsigned_8 (Result);
   end To_Integer;

   procedure Convert_LE (Frag : out UUID_Fragment; Val : String) is
      Res : Unsigned_8;
      Idx : Natural := 0;
   begin
      Frag := (others => 0);
      for I in reverse Val'Range loop
         Res := To_Integer (Val (I));

         if (I - Val'First) mod 2 /= 0 then
            Frag (Frag'First + Idx) := Res;
         else
            Frag (Frag'First + Idx) := Frag (Frag'First + Idx) or
                                       Shift_Left (Res, 4);
            Idx := Idx + 1;
         end if;
      end loop;
   end Convert_LE;

   procedure Convert_BE (Frag : out UUID_Fragment; Val : String) is
      Res : Unsigned_8;
      Idx : Natural := 0;
   begin
      Frag := (others => 0);
      for I in Val'Range loop
         Res := To_Integer (Val (I));

         if (I - Val'First) mod 2 /= 0 then
            Frag (Frag'First + Idx) := Frag (Frag'First + Idx) or Res;
            Idx := Idx + 1;
         else
            Frag (Frag'First + Idx) := Shift_Left (Res, 4);
         end if;
      end loop;
   end Convert_BE;
end Devices;
