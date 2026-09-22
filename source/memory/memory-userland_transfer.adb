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

   procedure Take_From_Userland
      (Map     : Memory.MMU.Page_Table_Acc;
       Data    : out T;
       Addr    : System.Address;
       Success : out Boolean)
   is
      pragma SPARK_Mode (Off); --  Copying through addresses is against SPARK.
      pragma Suppress (All_Checks); --  An object size cannot be out of range.
      Length, Left : Unsigned_64;
   begin
      Check_Access (Map, Addr, False, Success);
      if not Success then
         return;
      end if;

      --  The userland side may still go away while it is read, which ends
      --  the copy short instead of faulting the kernel.
      Length := Unsigned_64 (T'Object_Size / 8);
      Arch.Snippets.Enable_Userland_Memory_Access;
      Arch.Snippets.Full_Memory_Load_Store_Barrier;
      Arch.Snippets.Copy_Userland (Data'Address, Addr, Length, 0, Left);
      Arch.Snippets.Disable_Userland_Memory_Access;
      Success := Left = 0;
   end Take_From_Userland;

   procedure Paste_Into_Userland
      (Map     : Memory.MMU.Page_Table_Acc;
       Data    : T;
       Addr    : System.Address;
       Success : out Boolean)
   is
      pragma SPARK_Mode (Off); --  Copying through addresses is against SPARK.
      pragma Suppress (All_Checks); --  An object size cannot be out of range.
      Length, Left : Unsigned_64;
   begin
      Check_Access (Map, Addr, True, Success);
      if not Success then
         return;
      end if;

      --  As above, the userland side may go away while it is written.
      Length := Unsigned_64 (T'Object_Size / 8);
      Arch.Snippets.Enable_Userland_Memory_Access;
      Arch.Snippets.Copy_Userland (Data'Address, Addr, Length, 1, Left);
      Arch.Snippets.Full_Memory_Load_Store_Barrier;
      Arch.Snippets.Disable_Userland_Memory_Access;
      Success := Left = 0;
   end Paste_Into_Userland;

   procedure Check_Access
      (Map     : Memory.MMU.Page_Table_Acc;
       Addr    : System.Address;
       Write   : Boolean;
       Success : out Boolean)
   is
      Start  : Integer_Address := To_Integer (Addr);
      Length : Integer_Address;
      Result : System.Address;
      Is_Mapped, Is_Readable, Is_Writeable, Is_Executable : Boolean;
      Is_User_Accessible : Boolean;
   begin
      --  An empty object is never read nor written, so it is always
      --  accessible. Its range would be empty if it started on a page
      --  boundary, which the page walk below reports as unmapped.
      Length := Integer_Address (T'Object_Size / 8);
      if Length = 0 then
         Success := True;
         return;
      end if;

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
      if Write then
         Success := Is_User_Accessible and Is_Readable and Is_Writeable;
      else
         Success := Is_User_Accessible and Is_Readable;
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end Check_Access;
end Memory.Userland_Transfer;
