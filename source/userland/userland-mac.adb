--  userland-mac.adb: Mandatory access control.
--  Copyright (C) 2023 streaksu
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

package body Userland.MAC is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   function Get_Enforcement (Ctx : Context) return Enforcement is
   begin
      return Ctx.Action;
   end Get_Enforcement;

   procedure Set_Enforcement (Ctx : in out Context; Act : Enforcement) is
   begin
      Ctx.Action := Act;
   end Set_Enforcement;

   function Get_Capabilities (Ctx : Context) return Capabilities is
   begin
      return Ctx.Caps;
   end Get_Capabilities;

   procedure Set_Capabilities (Ctx : in out Context; Caps : Capabilities) is
   begin
      Ctx.Caps := Caps;
   end Set_Capabilities;

   function Check_Permissions
      (Data : Context;
       FS   : VFS.FS_Handle;
       Ino  : VFS.File_Inode_Number) return Permissions
   is
      Has_Elements : Boolean := False;
   begin
      for E of Data.Filters loop
         if E.Is_Used then
            Has_Elements := True;
            if not E.Is_Device and E.FS = FS and E.Ino = Ino then
               return E.Perms;
            end if;
         end if;
      end loop;

      if Has_Elements then
         return (others => False);
      else
         return (Can_Append_Only => False, others => True);
      end if;
   end Check_Permissions;

   procedure Add_Entity
      (Data   : in out Context;
       FS     : VFS.FS_Handle;
       Ino    : VFS.File_Inode_Number;
       Perms  : Permissions;
       Status : out Addition_Status)
   is
   begin
      for E of Data.Filters loop
         if E.Is_Used and not E.Is_Device and E.FS = FS and E.Ino = Ino then
            if Perms = E.Perms then
               Status := Success;
            else
               Status := Is_Conflicting;
            end if;
            return;
         end if;
      end loop;

      Add_Filter
         (Data   => Data,
          Filt   => (True, False, FS, Ino, Perms),
          Status => Status);
   end Add_Entity;

   function Get_Limit
      (Data     : Context;
       Resource : Limit_Type) return Limit_Value
   is
   begin
      return Data.Limits (Resource);
   end Get_Limit;

   procedure Set_Limit
      (Data      : in out Context;
       Resource  : Limit_Type;
       Limit     : Limit_Value;
       Could_Set : out Boolean)
   is
   begin
      if Limit <= Data.Limits (Resource) then
         Data.Limits (Resource) := Limit;
         Could_Set              := True;
      else
         Could_Set := False;
      end if;
   end Set_Limit;
   ----------------------------------------------------------------------------
   procedure Add_Filter
      (Data   : in out Context;
       Filt   : Filter;
       Status : out Addition_Status)
   is
   begin
      for E of Data.Filters loop
         if not E.Is_Used then
            E      := Filt;
            Status := Success;
            return;
         end if;
      end loop;

      Status := No_Space;
   end Add_Filter;
end Userland.MAC;
