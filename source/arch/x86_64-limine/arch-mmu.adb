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

with System; use System;
with Arch.Limine;
with Arch.Snippets;
with Arch.CPU; use Arch.CPU;
with Arch.APIC;
with Arch.Interrupts;
with Userland.Process;
with Synchronization;

package body Arch.MMU is
   --  Bits in the 4K page entries.
   Page_P     : constant Unsigned_64 := Shift_Left (1,  0);
   Page_RW    : constant Unsigned_64 := Shift_Left (1,  1);
   Page_U     : constant Unsigned_64 := Shift_Left (1,  2);
   Page_PWT   : constant Unsigned_64 := Shift_Left (1,  3);
   Page_PCD   : constant Unsigned_64 := Shift_Left (1,  4);
   Page_PAT   : constant Unsigned_64 := Shift_Left (1,  7);
   Page_G     : constant Unsigned_64 := Shift_Left (1,  8);
   Page_USER  : constant Unsigned_64 := Shift_Left (1,  9);
   Page_NX    : constant Unsigned_64 := Shift_Left (1, 63);

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
       Prefered_Mode => Arch.Limine.Paging_x86_64_5LVL,
       Max_Mode      => Arch.Limine.Paging_x86_64_5LVL,
       Min_Mode      => Arch.Limine.Paging_x86_64_4LVL)
      with Export;

   function Paging_Levels return Levels is
      PagingPonse : Arch.Limine.Paging_Mode_Response
         with Import, Address => Paging_Request.Base.Response;
   begin
      case PagingPonse.Mode is
         when Arch.Limine.Paging_x86_64_4LVL => return Four_Level_Paging;
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
      return Integer_Address (Entry_Body and 16#FFFFFFF000#);
   end Clean_Entry;

   function Clean_Entry_Perms (Entr : Unsigned_64) return Clean_Result is
      Result : Clean_Result;
   begin
      Result.User_Flag := (Entr and Page_USER) /= 0;
      Result.Perms :=
         (Is_User_Accessible => (Entr and Page_U) /= 0,
          Can_Read           => True,
          Can_Write          => (Entr and Page_RW) /= 0,
          Can_Execute        => (Entr and Page_NX) = 0,
          Is_Global          => (Entr and Page_G) /= 0);
      if (Entr and (Page_PWT and Page_PCD)) /= 0 then
         Result.Caching := Uncacheable;
      elsif (Entr and (Page_PWT and Page_PAT)) /= 0 then
         Result.Caching := Write_Combining;
      elsif (Entr and Page_PWT) /= 0 then
         Result.Caching := Write_Through;
      else
         Result.Caching := Write_Back;
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
         (if Perm.Can_Execute        then 0         else Page_NX) or
         (if Perm.Can_Write          then Page_RW   else       0) or
         (if Perm.Is_Global          then Page_G    else       0) or
         (if Perm.Is_User_Accessible then Page_U    else       0) or
         (if User_Flag               then Page_USER else       0) or
         Page_P;

      case Caching is
         when Write_Back      => null;
         when Write_Through   => Result := Result or Page_PWT;
         when Write_Combining => Result := Result or Page_PAT or Page_PWT;
         when Uncacheable     => Result := Result or Page_PWT or Page_PCD;
      end case;

      return Unsigned_64 (To_Integer (Addr)) or Result;
   exception
      when Constraint_Error =>
         return 0;
   end Construct_Entry;

   function Construct_Level (Addr : System.Address) return Unsigned_64 is
   begin
      return Unsigned_64 (To_Integer (Addr)) or Page_P or Page_U or Page_RW;
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
      pragma SPARK_Mode (Off);
      use Userland.Process;
      A1    : constant    Unsigned_64 := Unsigned_64 (To_Integer (Map));
      Final : constant System.Address := Addr + Len;
      Curr  :          System.Address := Addr;
      Proc  : Userland.Process.PID;
   begin
      --  First, invalidate for ourselves.
      if Arch.Snippets.Read_CR3 = A1 then
         while To_Integer (Curr) < To_Integer (Final) loop
            Arch.Snippets.Invalidate_Page (To_Integer (Curr));
            Curr := Curr + Page_Size;
         end loop;
      end if;

      --  If we are running on a process, and said process is running with more
      --  than one thread, we need to invalidate using funky IPIs.
      if Arch.CPU.Core_Locals = null then
         return;
      end if;

      Proc := Arch.CPU.Get_Local.Current_Process;

      for I in Arch.CPU.Core_Locals.all'Range loop
         if I /= Arch.CPU.Get_Local.Number and then
            Arch.CPU.Core_Locals (I).Current_Process = Proc
         then
            Synchronization.Seize (Arch.CPU.Core_Locals (I).Invalidate_Lock);
            Arch.CPU.Core_Locals (I).Invalidate_Map   := A1;
            Arch.CPU.Core_Locals (I).Invalidate_Start := Addr;
            Arch.CPU.Core_Locals (I).Invalidate_End   := Final;
            Synchronization.Release (Arch.CPU.Core_Locals (I).Invalidate_Lock);
            Arch.APIC.LAPIC_Send_IPI
               (Arch.CPU.Core_Locals (I).LAPIC_ID,
                Arch.Interrupts.Invalidate_Interrupt);
         end if;
      end loop;
   exception
      when Constraint_Error =>
         return;
   end Flush_TLBs;

   procedure Get_Current_Table (Addr : out System.Address) is
   begin
      Addr := To_Address (Integer_Address (Arch.Snippets.Read_CR3));
   end Get_Current_Table;

   procedure Set_Current_Table (Addr : System.Address; Success : out Boolean)
   is
      Val : Unsigned_64;
   begin
      Val := Unsigned_64 (To_Integer (Addr));
      if Arch.Snippets.Read_CR3 /= Val then
         Arch.Snippets.Write_CR3 (Val);
      end if;
      Success := True;
   end Set_Current_Table;
end Arch.MMU;
