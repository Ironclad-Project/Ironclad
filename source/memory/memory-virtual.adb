--  memory-virtual.adb: Virtual memory manager.
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

with Arch.Wrappers;
with Lib.Panic;

package body Memory.Virtual is
   procedure Init
      (Memmap : access Arch.Stivale2.Memmap_Tag;
       PMRs   : access Arch.Stivale2.PMR_Tag) is
      Index : Physical_Address := 0;
      Flags : constant Page_Flags :=
         (Present         => True,
          Read_Write      => True,
          User_Supervisor => False,
          Write_Through   => False,
          Cache_Disable   => False,
          Accessed        => False,
          Dirty           => False,
          PAT             => False,
          Global          => False);
   begin
      --  Initialize the kernel pagemap.
      Kernel_Map := new Page_Map;
      Lib.Synchronization.Release (Kernel_Map.Mutex'Access);

      --  Map PMRs.
      for E of PMRs.Entries loop
         declare
            I         : Physical_Address          := 0;
            Phys_Addr : constant Physical_Address := E.Base - Kernel_Offset;
            Virt_Addr : constant Virtual_Address  := E.Base;
            Flags     : Page_Flags :=
               (Present => True, Read_Write => True, User_Supervisor => False,
                Write_Through => False, Cache_Disable => False,
                Accessed => False, Dirty => False, PAT => False,
                Global => False);
            Not_Execute : Boolean := True;
         begin
            if (E.Permissions and Arch.Stivale2.PMR_Executable_Mask) /= 0 then
               Not_Execute := False;
            end if;

            if (E.Permissions and Arch.Stivale2.PMR_Writable_Mask) /= 0 then
               Flags.Read_Write := True;
            elsif (E.Permissions and Arch.Stivale2.PMR_Readable_Mask) /= 0 then
               Flags.Read_Write := False;
            end if;

            while I < E.Length loop
               Map_Page (Kernel_Map.all,
                         Virt_Addr + I, Phys_Addr + I, Flags, Not_Execute);
               I := I + 16#1000#;
            end loop;
         end;
      end loop;

      --  Map the first 2 GiB to the higher half and identity mapped.
      while Index < 16#100000000# loop
         Map_Page (Kernel_Map.all, Index,                 Index, Flags, False);
         Map_Page (Kernel_Map.all, Index + Memory_Offset, Index, Flags, False);
         Index := Index + 16#1000#;
      end loop;

      --  Make active.
      Make_Active (Kernel_Map.all);
   end Init;

   procedure Make_Active (Map : in out Page_Map) is
      Addr : constant Physical_Address := Physical_Address
         (To_Integer (Map.PML4_Level'Address) - Memory_Offset);
   begin
      --  Make the pagemap active on the callee core by writting the top-level
      --  address to CR3.
      Lib.Synchronization.Seize (Map.Mutex'Access);
      Arch.Wrappers.Write_CR3 (Unsigned_64 (Addr));
      Lib.Synchronization.Release (Map.Mutex'Access);
   end Make_Active;

   procedure Map_Page
      (Map         : in out Page_Map;
       Virtual     : Virtual_Address;
       Physical    : Physical_Address;
       Flags       : Page_Flags;
       Not_Execute : Boolean) is
      Page_Address : Virtual_Address;
   begin
      Lib.Synchronization.Seize (Map.Mutex'Access);

      Page_Address := Get_Page (Map, Virtual, True);

      declare
         Entry_Body : Page with Address => To_Address (Page_Address);
      begin
         Entry_Body.Addr  := Chomp_Flags (Physical);
         Entry_Body.Flags := Flags;
         Entry_Body.NX    := Not_Execute;
      end;

      Lib.Synchronization.Release (Map.Mutex'Access);
   end Map_Page;

   procedure Unmap_Page (Map : in out Page_Map; Virtual : Virtual_Address) is
      Page_Address : Virtual_Address;
      Addr4        : constant Physical_Address :=
         To_Integer (Map.PML4_Level'Address) - Memory_Offset;
   begin
      Lib.Synchronization.Seize (Map.Mutex'Access);

      Page_Address := Get_Page (Map, Virtual, True);

      declare
         Entry_Body : Page with Address => To_Address (Page_Address);
      begin
         Entry_Body.Flags.Present := False;
      end;

      --  Check whether we have to invalidate.
      if Arch.Wrappers.Read_CR3 = Unsigned_64 (Addr4) then
         Arch.Wrappers.Invalidate_Page (Virtual);
      end if;

      Lib.Synchronization.Release (Map.Mutex'Access);
   end Unmap_Page;

   procedure Change_Page_Flags
      (Map     : in out Page_Map;
       Virtual : Virtual_Address;
       Flags   : Page_Flags) is
      Page_Address : Virtual_Address;
   begin
      Lib.Synchronization.Seize (Map.Mutex'Access);

      Page_Address := Get_Page (Map, Virtual, True);

      declare
         Entry_Body : Page with Address => To_Address (Page_Address);
      begin
         Entry_Body.Flags := Flags;
      end;

      Lib.Synchronization.Release (Map.Mutex'Access);
   end Change_Page_Flags;

   function Get_Address_Components
      (Virtual : Virtual_Address) return Address_Components is
      Addr   : constant Unsigned_64 := Unsigned_64 (Virtual);
      PML4_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 39);
      PML3_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 30);
      PML2_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 21);
      PML1_E : constant Unsigned_64 := Addr and Shift_Left (16#1FF#, 12);
   begin
      return (PML4_Entry => Shift_Right (PML4_E, 39),
              PML3_Entry => Shift_Right (PML3_E, 30),
              PML2_Entry => Shift_Right (PML2_E, 21),
              PML1_Entry => Shift_Right (PML1_E, 12));
   end Get_Address_Components;

   function Get_Next_Level
      (Current_Level       : Physical_Address;
       Index               : Unsigned_64;
       Create_If_Not_Found : Boolean) return Physical_Address is
      Entry_Addr : constant Virtual_Address :=
         Current_Level + Memory_Offset + Physical_Address (Index * 8);
      Entry_Body : Page_Table_Entry with Address => To_Address (Entry_Addr);
   begin
      if Entry_Body.Flags.Present then
         return Add_Flags (Entry_Body.Addr);
      elsif Create_If_Not_Found then
         --  Allocate and put some default flags.
         declare
            type PML4_Acc is access PML4;
            New_Entry      : constant PML4_Acc := new PML4;
            New_Entry_Addr : constant Physical_Address :=
               To_Integer (New_Entry.all'Address) - Memory_Offset;
         begin
            Entry_Body.Addr := Chomp_Flags (New_Entry_Addr);
            Entry_Body.Flags.Present    := True;
            Entry_Body.Flags.Read_Write := True;
            Entry_Body.Flags.User_Supervisor := True;
            return New_Entry_Addr;
         end;
      end if;
      return Null_Address;
   end Get_Next_Level;

   function Get_Page
      (Map      : in out Page_Map;
       Virtual  : Virtual_Address;
       Allocate : Boolean) return Virtual_Address is
      Addr  : constant Address_Components := Get_Address_Components (Virtual);
      Addr4 : constant Physical_Address :=
         To_Integer (Map.PML4_Level'Address) - Memory_Offset;
      Addr3 : Physical_Address := Null_Address;
      Addr2 : Physical_Address := Null_Address;
      Addr1 : Physical_Address := Null_Address;
   begin
      --  Find the entries.
      Addr3 := Get_Next_Level (Addr4, Addr.PML4_Entry, Allocate);
      Addr2 := Get_Next_Level (Addr3, Addr.PML3_Entry, Allocate);
      Addr1 := Get_Next_Level (Addr2, Addr.PML2_Entry, Allocate);
      if not Allocate and (Addr3 = Null_Address or Addr2 = Null_Address or
         Addr1 = Null_Address)
      then
         Lib.Panic.Soft_Panic ("Address could not be unmapped");
         return Null_Address;
      end if;

      return Addr1 + Memory_Offset + Physical_Address (Addr.PML1_Entry) * 8;
   end Get_Page;

   function Chomp_Flags (Address : Physical_Address) return Physical_Address is
      Addr_Int : constant Unsigned_64 := Unsigned_64 (Address);
   begin
      return Physical_Address (Shift_Right (Addr_Int, 12));
   end Chomp_Flags;

   function Add_Flags (Address : Physical_Address) return Physical_Address is
      Addr_Int : constant Unsigned_64 := Unsigned_64 (Address);
   begin
      return Physical_Address (Shift_Left (Addr_Int, 12));
   end Add_Flags;
end Memory.Virtual;
