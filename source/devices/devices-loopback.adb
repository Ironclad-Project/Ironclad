--  devices-loopback.adb: Network loopback device.
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

with Networking.Interfaces;
with Scheduler;
with Devices.NetInter;

package body Devices.Loopback is
   procedure Init (Success : out Boolean) is
      Device : Resource;
      Dev    : Device_Handle;
      Data   : constant Loopback_Data_Acc := new Loopback_Data'
         (Data => new Loopback_Buffer,
          Len  => 0);
   begin
      Device :=
         (Data        => Data.all'Address,
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
          Poll        => null);
      Register (Device, "loopback", Success);
      if Success then
         Dev := Fetch ("loopback");
         Networking.Interfaces.Register_Interface
            (Interfaced  => Dev,
             IPv4        => (127, 0, 0, 1),
             IPv4_Subnet => (255, 0, 0, 0),
             IPv6        => (1 .. 15 => 0, 16 => 1),
             IPv6_Subnet => (others => 16#FF#),
             Success     => Success);
         Networking.Interfaces.Block (Dev, False, Success);
      end if;
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
      pragma Unreferenced (Offset);
      pragma Unreferenced (Is_Blocking);
      Dev : Loopback_Data with Import, Address => Key;
   begin
      while Dev.Len = 0 loop
         Scheduler.Yield_If_Able;
      end loop;

      if Data'Length > Dev.Len then
         Data (Data'First .. Data'First + Dev.Len - 1) :=
            Dev.Data (1 .. Dev.Len);
         Ret_Count := Dev.Len;
         Dev.Len := 0;
      else
         Data := Dev.Data (1 .. Data'Length);
         Ret_Count := Data'Length;
         Dev.Len := Dev.Len - Data'Length;
      end if;

      Success := True;
   end Read;

   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset);
      pragma Unreferenced (Is_Blocking);
      Dev : Loopback_Data with Import, Address => Key;
   begin
      while Dev.Len /= 0 loop
         Scheduler.Yield_If_Able;
      end loop;

      if Data'Length <= Dev.Data.all'Length then
         Dev.Data (1 .. Data'Length) := Data;
         Dev.Len := Data'Length;
         Ret_Count := Data'Length;
         Success := True;
      else
         Ret_Count := 0;
         Success := False;
      end if;
   end Write;

   function IO_Control
      (Data     : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      pragma Unreferenced (Data);
   begin
      case Request is
         when NetInter.NET_GETMAC =>
            declare
               Addr : Networking.MAC_Address with Import, Address => Argument;
            begin
               Addr := (others => 1);
            end;
         when others =>
            return False;
      end case;

      return True;
   end IO_Control;
end Devices.Loopback;
