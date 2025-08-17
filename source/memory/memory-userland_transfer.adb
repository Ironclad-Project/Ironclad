--  memory-userland_transfer.adb: Userland copy to/from userland.
--  Copyright (C) 2025 streaksu
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

with Arch.Snippets;
with Alignment;

package body Memory.Userland_Transfer is
   package A is new Alignment (Integer_Address);

   pragma Warnings
      (Off, "address specification on ""Mem_Data"" is imprecisely supported",
       Reason => "Clear a bit of warning spam");

   procedure Take_From_Userland
      (Map     : Memory.MMU.Page_Table_Acc;
       Data    : out T;
       Addr    : System.Address;
       Success : out Boolean)
   is
      Start  : Integer_Address := To_Integer (Addr);
      Length : Integer_Address;
      Result : System.Address;
      Is_Mapped, Is_Readable, Is_Writeable, Is_Executable : Boolean;
      Is_User_Accessible : Boolean;
   begin
      Length := Integer_Address (T'Object_Size / 8);
      A.Align_Memory_Range (Start, Length, Memory.MMU.Page_Size);
      Memory.MMU.Translate_Address
         (Map                => Map,
          Virtual            => To_Address (Start),
          Length             => Storage_Count (Length),
          Physical           => Result,
          Is_Mapped          => Is_Mapped,
          Is_User_Accessible => Is_User_Accessible,
          Is_Readable        => Is_Readable,
          Is_Writeable       => Is_Writeable,
          Is_Executable      => Is_Executable);
      if not Is_User_Accessible or else not Is_Readable then
         Success := False;
         return;
      end if;

      declare
         Mem_Data : constant T with Import, Convention => C, Address => Addr;
      begin
         Arch.Snippets.Enable_Userland_Memory_Access;
         Data    := Mem_Data;
         Success := True;
         Arch.Snippets.Disable_Userland_Memory_Access;
      end;
   exception
      when Constraint_Error =>
         Success := False;
   end Take_From_Userland;

   procedure Paste_Into_Userland
      (Map     : Memory.MMU.Page_Table_Acc;
       Data    : T;
       Addr    : System.Address;
       Success : out Boolean)
   is
      Start  : Integer_Address := To_Integer (Addr);
      Length : Integer_Address;
      Result : System.Address;
      Is_Mapped, Is_Readable, Is_Writeable, Is_Executable : Boolean;
      Is_User_Accessible : Boolean;
   begin
      Length := Integer_Address (T'Object_Size / 8);
      A.Align_Memory_Range (Start, Length, Memory.MMU.Page_Size);
      Memory.MMU.Translate_Address
         (Map                => Map,
          Virtual            => To_Address (Start),
          Length             => Storage_Count (Length),
          Physical           => Result,
          Is_Mapped          => Is_Mapped,
          Is_User_Accessible => Is_User_Accessible,
          Is_Readable        => Is_Readable,
          Is_Writeable       => Is_Writeable,
          Is_Executable      => Is_Executable);
      if not Is_User_Accessible or else not Is_Writeable then
         Success := False;
         return;
      end if;

      declare
         Mem_Data : T with Import, Convention => C, Address => Addr;
      begin
         Arch.Snippets.Enable_Userland_Memory_Access;
         Mem_Data := Data;
         Success  := True;
         Arch.Snippets.Disable_Userland_Memory_Access;
      end;
   exception
      when Constraint_Error =>
         Success := False;
   end Paste_Into_Userland;
end Memory.Userland_Transfer;
