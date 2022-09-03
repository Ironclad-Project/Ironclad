--  arch-stivale2.adb: Stivale2 utilities.
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

with System.Address_To_Access_Conversions;
with Arch.Wrappers;
with Memory.Virtual; use Memory.Virtual;
with Lib.Synchronization;
with Arch.Snippets;

package body Arch.Stivale2 with SPARK_Mode => Off is
   Terminal_Entrypoint : Terminal_Function_Acc := null;
   Terminal_Mutex      : aliased Lib.Synchronization.Binary_Semaphore;

   function Get_Tag
     (Proto     : access Header;
     Identifier : Unsigned_64) return Virtual_Address
   is
      package Convert is new System.Address_To_Access_Conversions (Tag);

      Search_Address : Virtual_Address := Proto.Tags;
      Search_Tag     : access Tag;
   begin
      while Search_Address /= Null_Address loop
         Search_Tag := Convert.To_Pointer (To_Address (Search_Address));
         if Search_Tag.Identifier = Identifier then
            return Search_Address;
         end if;
         Search_Address := Search_Tag.Next;
      end loop;
      return Null_Address;
   end Get_Tag;

   procedure Init_Terminal (Terminal : access Terminal_Tag) is
   begin
      Terminal_Entrypoint := Terminal.TermWrite;
      Lib.Synchronization.Release (Terminal_Mutex);
   end Init_Terminal;

   procedure Print_Terminal (Message : String) is
   begin
      Inner_Terminal (Message'Address, Message'Length);
   end Print_Terminal;

   procedure Print_Terminal (Message : Character) is
   begin
      Inner_Terminal (Message'Address, 1);
   end Print_Terminal;

   procedure Inner_Terminal (Message : System.Address; Length : Natural) is
      Discard : Boolean;
   begin
      if Terminal_Entrypoint = null then
         return;
      end if;

      declare
         Message_Str  : String (1 .. Length) with Address => Message, Import;
         Message_Copy : String (1 .. Length);
         Current_CR3  : constant Unsigned_64 := Wrappers.Read_CR3;
      begin
         --  Make a copy to avoid the issue of unmapping the memory when
         --  switching pagemaps.
         for I in 1 .. Length loop
            Message_Copy (I) := Message_Str (I);
         end loop;

         --  TODO: If the terminal gets preempted here, we will end with
         --  bad memory maps when switching back and we will triplefault.
         --  A fix for this would be ISTs.
         Snippets.Disable_Interrupts;

         --  Ensure we are using the kernel pagemap, as the lower half needs
         --  to be identity mapped, and only the kernel pagemap does that.
         if Kernel_Map /= null then
            Discard := Make_Active (Kernel_Map);
         end if;

         --  The terminal doesn't have internal locking so we need to lock it
         --  ourselves.
         Lib.Synchronization.Seize (Terminal_Mutex);
         Terminal_Entrypoint.all (Message_Copy'Address, Length);
         Lib.Synchronization.Release (Terminal_Mutex);

         if Kernel_Map /= null then
            Wrappers.Write_CR3 (Current_CR3);
         end if;

         Snippets.Enable_Interrupts;
      end;
   end Inner_Terminal;
end Arch.Stivale2;
