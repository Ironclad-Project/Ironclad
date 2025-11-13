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
   Page_User  : constant Unsigned_64 := Shift_Left (1, 8);
   PMBT_NC    : constant Unsigned_64 := Shift_Left (1, 61);
   PMBT_IO    : constant Unsigned_64 := Shift_Left (2, 61);

   --  Response is a pointer to an Kernel_Address_Response.
   Address_Request : Arch.Limine.Request :=
      (ID       => Arch.Limine.Kernel_Address_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export;

   --  Response is a pointer to an HHDM_Response.
   HHDM_Request : Arch.Limine.Request :=
      (ID       => Arch.Limine.HHDM_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export;

   --  Paging level request.
   Paging_Request : Arch.Limine.Paging_Mode_Request :=
      (Base =>
         (ID       => Arch.Limine.Paging_Mode_ID,
          Revision => 0,
          Response => System.Null_Address),
       Preferred_Mode => Arch.Limine.Paging_RISCV_64_SV57,
       Max_Mode => Arch.Limine.Paging_RISCV_64_SV57,
       Min_Mode => Arch.Limine.Paging_RISCV_64_SV39)
      with Export;

   function Paging_Levels return Levels is
      PagingPonse : Arch.Limine.Paging_Mode_Response
         with Import, Address => Paging_Request.Base.Response;
   begin
      case PagingPonse.Mode is
         when Arch.Limine.Paging_RISCV_64_SV39 => return Three_Level_Paging;
         when Arch.Limine.Paging_RISCV_64_SV48 => return Four_Level_Paging;
         when others => return Five_Level_Paging;
      end case;
   end Paging_Levels;

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

   function Memory_Offset return Integer_Address is
      HHDM_Response : Arch.Limine.HHDM_Response
         with Import, Address => HHDM_Request.Response;
   begin
      return HHDM_Response.Offset;
   end Memory_Offset;

   function Kernel_Offset return Integer_Address is
   begin
      --  Established in the linker script.
      return 16#FFFFFFFF80000000#;
   end Kernel_Offset;

   function Clean_Entry (Entry_Body : Unsigned_64) return Integer_Address is
   begin
      return Integer_Address
         (Shift_Left (Entry_Body and 16#FFFFFFFFFFFFFC00#, 2));
   end Clean_Entry;

   function Clean_Entry_Perms (Entr : Unsigned_64) return Clean_Result is
      Result : Clean_Result;
   begin
      Result :=
         (User_Flag => (Entr and Page_User) /= 0,
          Perms     =>
            (Is_User_Accessible => (Entr and Page_U) /= 0,
             Can_Read           => (Entr and Page_R) /= 0,
             Can_Write          => (Entr and Page_W) /= 0,
             Can_Execute        => (Entr and Page_X) /= 0,
             Is_Global          => (Entr and Page_G) /= 0),
          Caching => Write_Back);

      if (Entr and PMBT_NC) = PMBT_NC then
         Result.Caching := Write_Combining;
      elsif (Entr and PMBT_IO) = PMBT_IO then
         Result.Caching := Uncacheable;
      end if;

      return Result;
   end Clean_Entry_Perms;

   function Construct_Entry
      (Addr      : System.Address;
       Perm      : Page_Permissions;
       Caching   : Caching_Model;
       User_Flag : Boolean) return Unsigned_64
   is
      Result : Unsigned_64;
   begin
      Result :=
         (if Perm.Can_Execute        then Page_X else 0) or
         (if Perm.Can_Read           then Page_R else 0) or
         (if Perm.Can_Write          then Page_W else 0) or
         (if Perm.Is_Global          then Page_G else 0) or
         (if Perm.Is_User_Accessible then Page_U else 0) or
         (if User_Flag then Page_User else 0) or
         Page_P or Page_Acc or Page_Dirty;

      case Caching is
         when Write_Back | Write_Through => null;
         when Write_Combining => Result := Result or PMBT_NC;
         when Uncacheable => Result := Result or PMBT_IO;
      end case;

      return Shift_Right (Unsigned_64 (To_Integer (Addr)), 2) or Result;
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
      pragma Unreferenced (Entry_Body);
   begin
      return 0;
   end Make_Not_Present;

   procedure Flush_TLBs (Map, Addr : System.Address; Len : Storage_Count) is
      pragma Unreferenced (Map, Addr, Len);
      pragma SPARK_Mode (Off); --  ASM is not SPARK-friendly.
   begin
      System.Machine_Code.Asm
         ("sfence.vma",
          Clobber  => "memory",
          Volatile => True);
   end Flush_TLBs;

   procedure Get_Current_Table (Addr : out System.Address) is
      pragma SPARK_Mode (Off); --  ASM is not SPARK-friendly.
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
      pragma SPARK_Mode (Off); --  ASM is not SPARK-friendly.
      Mode : Unsigned_64;
      Add2 : Unsigned_64 := Unsigned_64 (To_Integer (Addr));
   begin
      Mode := 8 +
         (case Paging_Levels is
            when Three_Level_Paging => 0,
            when Four_Level_Paging => 1,
            when Five_Level_Paging => 2);

      Add2 := Shift_Right (Add2, 12) or Shift_Left (Mode, 60);
      System.Machine_Code.Asm
         ("csrw satp, %0; sfence.vma",
          Inputs   => Unsigned_64'Asm_Input ("r", Add2),
          Clobber  => "memory",
          Volatile => True);
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end Set_Current_Table;
end Arch.MMU;
