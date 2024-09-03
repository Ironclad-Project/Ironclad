--  arch-dtb.adb: Device-tree blob parsing.
--  Copyright (C) 2024 streaksu
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

package body Arch.DTB with SPARK_Mode => Off is
   function Init return Boolean is
      DTBPonse : Arch.Limine.DTB_Response
         with Import, Address => DTB_Request.Response;
   begin
      if DTB_Request.Response = System.Null_Address then
         return False;
      end if;

      declare
         Header : FDT_Header with Import, Address => DTBPonse.DTB_Addr;
      begin
         return Header.Magic = FDT_Magic;
      end;
   end Init;
end Arch.DTB;
