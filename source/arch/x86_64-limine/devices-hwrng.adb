--  devices-hwrng.adb: Hardware random number generator.
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

with System.Machine_Code;
with Arch.Snippets;

package body Devices.HWRNG is
   function Init return Boolean is
      EAX, EBX, ECX, EDX : Unsigned_32;
      Success : Boolean;
   begin
      --  Check for RDRAND.
      Arch.Snippets.Get_CPUID (1, 0, EAX, EBX, ECX, EDX);
      if (ECX and Shift_Left (1, 30)) = 0 then
         return True;
      end if;

      Register
         ((Data        => System.Null_Address,
           ID          => (others => 0),
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Read        => Read'Access,
           Write       => null,
           Sync        => null,
           Sync_Range  => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => null), "hwrng", Success);
      return True;
   end Init;
   ----------------------------------------------------------------------------
   procedure Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      pragma Unreferenced (Is_Blocking);

      Val : Unsigned_16;
   begin
      Data := (others => 0);

      for O of Data loop
         System.Machine_Code.Asm
            ("1: rdrand %0; jnc 1b",
             Outputs  => Unsigned_16'Asm_Output ("=r", Val),
             Clobber  => "memory",
             Volatile => True);

         O := Unsigned_8 (Val and 16#FF#);
      end loop;

      Ret_Count := Data'Length;
      Success   := True;
   end Read;
end Devices.HWRNG;
