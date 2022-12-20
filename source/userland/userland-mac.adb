--  userland-mac.adb: Mandatory access control.
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

with VFS;

package body Userland.MAC is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   function Is_Conflicting (F : Filter; Filters : Filter_Arr) return Boolean is
   begin
      if not VFS.Is_Canonical (F.Path (1 .. F.Length)) then
         return True;
      end if;

      for It of Filters loop
         --  Checking:
         --  1: Same path, we would be redefining an existing filter.
         --  2: Filter of contents of an already filtered dir.
         --  3: Filtering a dir containing other filters.
         --  TODO: This assumes well formed paths with no pending '/' or
         --  anything, the user could theoretically not provide that.
         if F.Path (1 .. F.Length) = It.Path (1 .. It.Length) then
            return True;
         elsif F.Length > It.Length and
               F.Path (1 .. It.Length) = It.Path (1 .. It.Length)
         then
            return It.Perms.Includes_Files or It.Perms.Includes_Directories;
         elsif F.Length < It.Length and
               F.Path (1 .. F.Length) = It.Path (1 .. F.Length)
         then
            return F.Perms.Includes_Files or F.Perms.Includes_Directories;
         end if;
      end loop;

      return False;
   end Is_Conflicting;

   function Check_Path_Permissions
      (Path    : String;
       Filters : Filter_Arr) return Filter_Permissions
   is
      Has_Matched        : Boolean := False;
      Matched_Char_Count : Natural := 0;
      Best_Match_Index   : Integer := Filters'First;
   begin
      if Path'Length = 0 then
         goto Error_Return;
      end if;

      --  Check for best match, if any.
      for I in Filters'Range loop
         pragma Loop_Invariant (Best_Match_Index >= Filters'First and
            Best_Match_Index <= Filters'Last);

         if Path'Length >= Filters (I).Length and then
            Filters (I).Path (1 .. Filters (I).Length) =
            Path (Path'First .. Path'First + Filters (I).Length - 1)
         then
            if Matched_Char_Count < Filters (I).Length then
               Has_Matched        := True;
               Matched_Char_Count := Filters (I).Length;
               Best_Match_Index   := I;
            end if;
         end if;
      end loop;

      --  Check for perfect matches or directories containing the path.
      if Has_Matched then
         if Matched_Char_Count = Path'Length then
            return Filters (Best_Match_Index).Perms;
         elsif Filters (Best_Match_Index).Perms.Includes_Files or
               Filters (Best_Match_Index).Perms.Includes_Directories
         then
            return Filters (Best_Match_Index).Perms;
         end if;
      end if;

   <<Error_Return>>
      --  Default return is no permissions.
      return (others => False);
   end Check_Path_Permissions;
end Userland.MAC;
