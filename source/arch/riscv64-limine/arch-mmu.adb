--  arch-mmu.adb: Architecture-specific MMU code.
--  Copyright (C) 2025 streaksu
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
with System; use System;
with Arch.Limine;
with Arch.CPU; use Arch.CPU;

package body Arch.MMU is
   --  Bits in the 4K page entries.
   Page_P     : constant Unsigned_64 := Shift_Left (1, 0);
   Page_R     : constant Unsigned_64 := Shift_Left (1, 1);
   Page_W     : constant Unsigned_64 := Shift_Left (1, 2);
   Page_X     : constant Unsigned_64 := Shift_Left (1, 3);
   Page_U     : constant Unsigned_64 := Shift_Left (1, 4);
   Page_G     : constant Unsigned_64 := Shift_Left (1, 5);
   Page_Acc   : constant Unsigned_64 := Shift_Left (1, 6);
   Page_Dirty : constant Unsigned_64 := Shift_Left (1, 7);

   --  Response is a pointer to an Kernel_Address_Response.
   Address_Request : Arch.Limine.Request :=
      (ID       => Arch.Limine.Kernel_Address_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   procedure Get_Load_Addr (A : out System.Address; Success : out Boolean) is
      PhysPonse : Arch.Limine.Kernel_Address_Response
         with Import, Address => Address_Request.Response;
   begin
      if Address_Request.Response /= System.Null_Address then
         A       := PhysPonse.Phys_Addr;
         Success := True;
      else
         A       := System.Null_Address;
         Success := False;
      end if;
   end Get_Load_Addr;

   function Clean_Entry (Entry_Body : Unsigned_64) return Integer_Address is
   begin
      return Integer_Address
         (Shift_Left (Entry_Body and 16#003ffffffffffc00#, 2));
   end Clean_Entry;

   function Clean_Entry_Perms (Entr : Unsigned_64) return Page_Permissions is
   begin
      return
         (Is_User_Accessible => (Entr and Page_U) /= 0,
          Can_Read           => (Entr and Page_R) /= 0,
          Can_Write          => (Entr and Page_W) /= 0,
          Can_Execute        => (Entr and Page_X) /= 0,
          Is_Global          => (Entr and Page_G) /= 0);
   end Clean_Entry_Perms;

   function Construct_Entry
      (Addr    : System.Address;
       Perm    : Page_Permissions;
       Caching : Caching_Model) return Unsigned_64
   is
      Result : Unsigned_64;
   begin
      Result :=
         (if Perm.Can_Execute        then Page_X else 0) or
         (if Perm.Can_Read           then Page_R else 0) or
         (if Perm.Can_Write          then Page_W else 0) or
         (if Perm.Is_Global          then Page_G else 0) or
         (if Perm.Is_User_Accessible then Page_U else 0) or
         Page_P or Page_Acc or Page_Dirty;

      return Shift_Right (Unsigned_64 (To_Integer (Addr)), 2) or Result;
   exception
      when Constraint_Error =>
         return 0;
   end Construct_Entry;

   function Construct_Level (Addr : System.Address) return Unsigned_64 is
   begin
      return Shift_Right (Unsigned_64 (To_Integer (Addr)), 2) or Page_P;
   end Construct_Level;

   function Is_Entry_Present (Entry_Body : Unsigned_64) return Boolean is
   begin
      return (Entry_Body and Page_P) /= 0;
   end Is_Entry_Present;

   function Make_Not_Present (Entry_Body : Unsigned_64) return Unsigned_64 is
   begin
      return Entry_Body and not Page_P;
   end Make_Not_Present;

   procedure Flush_TLBs (Map, Addr : System.Address; Len : Storage_Count) is
      pragma Unreferenced (Map, Addr, Len);
   begin
      System.Machine_Code.Asm
         ("sfence.vma",
          Clobber  => "memory",
          Volatile => True);
   end Flush_TLBs;

   procedure Get_Current_Table (Addr : out System.Address) is
      Ret : Unsigned_64;
   begin
      System.Machine_Code.Asm
         ("csrr %0, satp",
          Outputs  => Unsigned_64'Asm_Output ("=r", Ret),
          Clobber  => "memory",
          Volatile => True);
      Addr := To_Address (Integer_Address
         (Shift_Left (Ret and (Shift_Left (1, 44) - 1), 12)));
   end Get_Current_Table;

   procedure Set_Current_Table (Addr : System.Address; Success : out Boolean)
   is
      Mode : constant Unsigned_64 := 8 + (4 - 3);
      Add2 :          Unsigned_64 := Unsigned_64 (To_Integer (Addr));
   begin
      Add2 := Shift_Right (Add2, 12) or Shift_Left (Mode, 60);
      System.Machine_Code.Asm
         ("csrw satp, %0; sfence.vma",
          Inputs   => Unsigned_64'Asm_Input ("r", Add2),
          Clobber  => "memory",
          Volatile => True);
      Success := True;
   end Set_Current_Table;
end Arch.MMU;
