--  vfs-path.ads: Path management.
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

package VFS.Path is
   type Str_Acc is access all String;

   --  A path can be relative or absolute, an absolute path is determined by
   --  a leading '/'.
   function Is_Absolute (Path : String) return Boolean;

   --  Build a path from 2 components, the result must be deallocated, and is
   --  not canonical.
   function Build_Path (CWD, Path : String) return Str_Acc
      with Pre => (CWD'Length > 0 and Path'Length > 0);

   --  Make a cannonical path from an absolute path.
   --  A canonical path is the shortest possible absolute path to a
   --  location, with no ., .., or repeated /. Symbolic links are not removed.
   --  The returned path must be deallocated.
   function Is_Canonical (Path : String) return Boolean;
   function Make_Canonical (Path : String) return Str_Acc
      with Pre => (not Is_Canonical (Path));
end VFS.Path;
