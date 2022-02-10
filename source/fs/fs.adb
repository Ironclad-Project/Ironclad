--  fs.adb: FS registry and dispatching.
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

with System; use System;
with Lib.Synchronization;
with FS.File;

package body FS is
   --  Registry of roots and lock for modifying such list.
   type Root_Container is record
      Is_Present : Boolean;
      Contents   : Root;
   end record;
   type Root_Container_Arr is array (1 .. 10) of Root_Container;
   Roots_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
   Roots       : access Root_Container_Arr;

   procedure Init is
   begin
      Roots := new Root_Container_Arr;
      Lib.Synchronization.Release (Roots_Mutex'Access);

      --  Initialize the file registry.
      FS.File.Init;
   end Init;

   function Register_Root (R : Root) return Boolean is
      Returned : Boolean := False;
   begin
      Lib.Synchronization.Seize (Roots_Mutex'Access);
      --  Search if the name is already taken.
      for E of Roots.all loop
         if E.Is_Present and E.Contents.Name = R.Name then
            goto Return_Code;
         end if;
      end loop;

      --  Allocate.
      for I in Roots'First .. Roots'Last loop
         if not Roots (I).Is_Present then
            Roots (I).Is_Present    := True;
            Roots (I).Contents      := R;
            Returned                := True;
            if Roots (I).Contents.Init /= null then
               Roots (I).Contents.Data :=
                  Roots (I).Contents.Init.all (Roots (I).Contents.Data);
            end if;
            exit;
         end if;
      end loop;

   <<Return_Code>>
      Lib.Synchronization.Release (Roots_Mutex'Access);
      return Returned;
   end Register_Root;

   function Get_Root (Name : Root_Name; Result : out Root) return Boolean is
      Returned : Boolean := False;
   begin
      Lib.Synchronization.Seize (Roots_Mutex'Access);
      for E of Roots.all loop
         if E.Is_Present and E.Contents.Name = Name then
            Result   := E.Contents;
            Returned := True;
            exit;
         end if;
      end loop;
      Lib.Synchronization.Release (Roots_Mutex'Access);
      return Returned;
   end Get_Root;

   procedure Unload_Root (Name : Root_Name) is
   begin
      Lib.Synchronization.Seize (Roots_Mutex'Access);
      for E of Roots.all loop
         if E.Is_Present and E.Contents.Name = Name then
            E.Is_Present := False;
            if E.Contents.Data   /= Error_Value and
               E.Contents.Unload /= null
            then
               E.Contents.Unload (E.Contents.Data);
            end if;
         end if;
      end loop;
      Lib.Synchronization.Release (Roots_Mutex'Access);
   end Unload_Root;

   function List_Roots return Root_List_Acc is
      Returned       : Root_List_Acc;
      Root_Count     : Natural := 0;
      Returned_Index : Natural := 1;
   begin
      Lib.Synchronization.Seize (Roots_Mutex'Access);

      --  Count how many active roots we have.
      for E of Roots.all loop
         if E.Is_Present then
            Root_Count := Root_Count + 1;
         end if;
      end loop;

      --  Allocate and fill.
      Returned := new Root_List (1 .. Root_Count);
      for E of Roots.all loop
         if E.Is_Present then
            Returned (Returned_Index) := E.Contents.Name;
            Returned_Index := Returned_Index + 1;
         end if;
      end loop;

      Lib.Synchronization.Release (Roots_Mutex'Access);
      return Returned;
   end List_Roots;
end FS;
