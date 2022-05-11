--  vfs-path.adb: Path management.
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

package body VFS.Path is
   function Is_Absolute (Path : String) return Boolean is
   begin
      return Path'Length >= 1 and then Path (Path'First) = '/';
   end Is_Absolute;

   function Build_Path (CWD, Path : String) return Str_Acc is
      Length   : constant Natural := 1 + CWD'Length + 1 + Path'Length;
      New_Path : constant Str_Acc := new String (1 .. Length);
   begin
      New_Path (1)                        := '/';
      New_Path (2 .. CWD'Length + 1)      := CWD;
      New_Path (CWD'Length + 2)           := '/';
      New_Path (CWD'Length + 3 .. Length) := Path;
      return New_Path;
   end Build_Path;

   function Is_Canonical (Path : String) return Boolean is
   begin
      if not Is_Absolute (Path) then
         return False;
      end if;

      for I in Path'First .. Path'Last loop
         if Path (I) = '.' and Path'Last >= I + 1 then
            if Path (I + 1) = '.' then
               return False;
            end if;
         end if;

         if Path (I) = '/' and Path'Last >= I + 1 then
            if Path (I + 1) = '/' then
               return False;
            end if;
            if Path'Last >= I + 2 then
               if Path (I + 1) = '.' and Path (I + 2) = '/' then
                  return False;
               end if;
            end if;
         end if;
      end loop;

      if Path'Length > 1 and Path (Path'Last) = '/' then
         return False;
      end if;

      return True;
   end Is_Canonical;

   function Make_Canonical (Path : String) return Str_Acc is
   begin
      return new String'(Path);
   end Make_Canonical;
end VFS.Path;
