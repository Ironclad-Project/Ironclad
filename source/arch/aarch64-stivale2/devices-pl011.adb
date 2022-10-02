--  devices-pl011.adb: PL011-compatible driver.
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

with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;
with Arch.Snippets;
with Memory; use Memory;
with Lib.Synchronization;
with Devices.TermIOs;

package body Devices.PL011 with SPARK_Mode => Off is
   PL011_Data    : Unsigned_32 with Import, Volatile;
   PL011_Status  : Unsigned_32 with Import, Volatile;
   PL011_I_Baud  : Unsigned_32 with Import, Volatile;
   PL011_F_Baud  : Unsigned_32 with Import, Volatile;
   PL011_Control : Unsigned_32 with Import, Volatile;
   for PL011_Data'Address    use To_Address (Memory_Offset + 16#9000000#);
   for PL011_Status'Address  use To_Address (Memory_Offset + 16#9000018#);
   for PL011_I_Baud'Address  use To_Address (Memory_Offset + 16#9000024#);
   for PL011_F_Baud'Address  use To_Address (Memory_Offset + 16#9000028#);
   for PL011_Control'Address use To_Address (Memory_Offset + 16#9000030#);

   type PL011_Info is record
      Baud : Unsigned_32;
   end record;
   type PL011_Info_Acc is access PL011_Info;
   package Conv is new System.Address_To_Access_Conversions (PL011_Info);

   Default_Baud : constant := 115200;

   procedure Configure is
   begin
      Set_Baud (Default_Baud);
   end Configure;

   procedure Print (Message : Character) is
   begin
      while (PL011_Status and 16#100000#) /= 0 loop
         Arch.Snippets.Pause;
      end loop;
      PL011_Data := Character'Pos (Message);
   end Print;

   function Register return Boolean is
      Stat : constant VFS.File_Stat := (
         Unique_Identifier => 0,
         Type_Of_File      => VFS.File_Character_Device,
         Mode              => 8#660#,
         Hard_Link_Count   => 1,
         Byte_Size         => 0,
         IO_Block_Size     => 4096,
         IO_Block_Count    => 0
      );
      Data : constant PL011_Info_Acc := new PL011_Info'(
         Baud => Default_Baud
      );
      Device : VFS.Resource := (
         Data       => Conv.To_Address (Conv.Object_Pointer (Data)),
         Mutex      => <>,
         Stat       => Stat,
         Sync       => null,
         Read       => null,
         Write      => Write'Access,
         IO_Control => IO_Control'Access,
         Mmap       => null,
         Munmap     => null
      );
   begin
      Lib.Synchronization.Release (Device.Mutex);
      return VFS.Register (Device, "pl011");
   end Register;

   function Write
      (Data     : VFS.Resource_Acc;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      Write_Data : array (1 .. Count) of Character
         with Import, Address => To_Write;
      pragma Unreferenced (Offset);
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      for I of Write_Data loop
         Print (I);
      end loop;
      Lib.Synchronization.Release (Data.Mutex);
      return Count;
   end Write;

   function IO_Control
      (Data     : VFS.Resource_Acc;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      Port_Data : constant not null Conv.Object_Pointer :=
         Conv.To_Pointer (Data.Data);
      Returned : TermIOs.Main_Data with Import, Address => Argument;
      Success  : Boolean := False;
   begin
      Lib.Synchronization.Seize (Data.Mutex);
      case Request is
         when TermIOs.TCGETS =>
            Returned.Output_Baud := Port_Data.Baud;
            Success := True;
         when TermIOs.TCSETS | TermIOs.TCSETSW | TermIOs.TCSETSF =>
            Set_Baud (Returned.Output_Baud);
            Port_Data.Baud := Returned.Output_Baud;
            Success := True;
         when others =>
            null;
      end case;
      Lib.Synchronization.Release (Data.Mutex);
      return Success;
   end IO_Control;

   procedure Set_Baud (Baud : Unsigned_32) is
      Clock  : constant Unsigned_64 := 24000000;
      A_Baud : constant Unsigned_64 := Unsigned_64 (16 * Baud);
      I_Part : constant Unsigned_64 := Clock / A_Baud;
      F_Part : constant Unsigned_64 := (((Clock * 1000) /
         A_Baud - (I_Part * 1000)) * 64 + 500) / 1000;
   begin
      PL011_I_Baud := Unsigned_32 (I_Part and 16#FFFFFFFF#);
      PL011_F_Baud := Unsigned_32 (F_Part and 16#FFFFFFFF#);
   end Set_Baud;
end Devices.PL011;
