--  fs-file.ads: File creation and management.
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

package FS.File is
   --  File descriptor and notable values.
   type FD is new Integer;
   Error_FD : constant FD := 0;

   --  Initialize the file registry.
   procedure Init;

   --  Functions to interact with files.
   type Access_Mode is (Access_R, Access_W, Access_RW);
   function Open (Name : String; Flags : Access_Mode) return FD;
   procedure Close (ID : FD);
   function Read
      (ID    : FD;
       Count : Integer;
       Desto : System.Address) return Natural;
   function Write
      (ID    : FD;
       Count : Integer;
       Data  : System.Address) return Natural;
   function Get_Size (ID : FD) return Natural;
   procedure Set_Index (ID : FD; Index : Natural);
   procedure Reset (ID : FD);
end FS.File;
