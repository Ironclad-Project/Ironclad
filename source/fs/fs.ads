--  fs.ads: FS registry and dispatching library specification.
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

with System;

package FS is
   --  The VFS of Ironclad consists on a list of root devices that are mounted
   --  as their own root, instead of having a single root.
   --  Inside each of these roots, the layout of the several files and folders
   --  is entirely dependant on said root, along with the operations supported.
   --  The functions below are a skelleton, different root implementations are
   --  free to implement A R B I T R A R Y behaviour, and they may be null for
   --  non-implemented functions.
   --
   --  (When in the functions below it is mentioned to read or write the root,
   --  it means for devices like disks or such that can be operated raw).
   subtype Root_Data is System.Address;
   subtype Object    is System.Address;
   subtype Root_Name is String (1 .. 7);
   Error_Value : constant Root_Data := Root_Data (System'To_Address (0));
   type Root is record
      Name   : Root_Name; -- Name of the root.
      Data   : Root_Data; --  Instance-specific untouchable data.
      Init   : access function (Data : Root_Data) return Root_Data; --  Init.
      Unload : access procedure (Data : Root_Data); --  Deinitialize the root.
      Sync   : access procedure (Data : Root_Data); --  Commits all writes.

      --  Create an object on the root, and return it.
      Create : access function
         (Data  : Root_Data;
          Name  : String;
          Flags : Positive) return Boolean;

      --  Open an object.
      Open : access function (Data : Root_Data; Name : String) return Object;

      --  Close an object.
      Close : access procedure (Data : Root_Data; Obj : Object);

      --  Read object, if object = 0, read the root itself.
      Read : access function
         (Data   : Root_Data;
          Obj    : Object;
          Offset : System.Address;
          Count  : Positive;
          Desto  : System.Address) return Natural;

      --  Write object, if object = 0, write the root itself.
      Write : access function
         (Data     : Root_Data;
          Obj      : Object;
          Offset   : System.Address;
          Count    : Positive;
          To_Write : System.Address) return Natural;
   end record;

   --  Initialize the FS registry.
   procedure Init;

   --  Register a root, and display it to the user, and return success.
   function Register_Root (R : Root) return Boolean;

   --  Get the root structure, and return success.
   function Get_Root (Name : Root_Name; Result : out Root) return Boolean;

   --  Unload the root that answers to the passed name.
   procedure Unload_Root (Name : Root_Name);

   --  Get an allocated list of the active roots.
   type Root_List is array (Natural range <>) of Root_Name;
   type Root_List_Acc is access Root_List;
   function List_Roots return Root_List_Acc;
end FS;
