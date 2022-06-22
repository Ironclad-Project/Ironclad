--  arch.adb: Architecture-specific package.
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

with System.Address_To_Access_Conversions;
with System.Machine_Code;
with Arch.Wrappers;
with Arch.Stivale2;
with Arch.Entrypoint;
with Arch.Paging;
pragma Unreferenced (Arch.Entrypoint);
with Lib;
with Devices.BootFB;
with Devices.PS2Mouse;
with Devices.PS2Keyboard;
with Devices.Serial;
with Lib.Panic;
with Config;
with Interfaces; use Interfaces;

package body Arch is
   function Get_Info return Boot_Information is
      package ST renames Stivale2;
      package C1 is new System.Address_To_Access_Conversions (ST.Cmdline_Tag);
      package C2 is new System.Address_To_Access_Conversions (ST.Modules_Tag);
      package C3 is new System.Address_To_Access_Conversions (ST.Memmap_Tag);

      Cmdline : constant access ST.Cmdline_Tag :=
      C1.To_Pointer (To_Address (ST.Get_Tag (ST.Stivale_Tag, ST.Cmdline_ID)));
      Modules : constant access ST.Modules_Tag :=
      C2.To_Pointer (To_Address (ST.Get_Tag (ST.Stivale_Tag, ST.Modules_ID)));
      Memmap : constant access ST.Memmap_Tag :=
      C3.To_Pointer (To_Address (ST.Get_Tag (ST.Stivale_Tag, ST.Memmap_ID)));

      Cmd_Len : constant Natural := Lib.C_String_Length (Cmdline.Inner);
      Cmd_Str : String (1 .. Cmd_Len) with Address => Cmdline.Inner;

      Ret : Boot_Information;
   begin
      Ret.Cmdline (1 .. Cmd_Len) := Cmd_Str;
      Ret.Cmdline_Len            := Cmd_Len;

      Ret.Memmap_Len := 0;
      for I in Memmap.Entries'First .. Memmap.Entries'Last loop
         exit when I > Ret.Memmap'Length;
         Ret.Memmap (Ret.Memmap_Len + 1) := (
            Start   => To_Address (Integer_Address (Memmap.Entries (I).Base)),
            Length  => Storage_Count (Memmap.Entries (I).Length),
            Is_Free => Memmap.Entries (I).EntryType = ST.Memmap_Entry_Usable
         );
         Ret.Memmap_Len := Ret.Memmap_Len + 1;
      end loop;

      Ret.RAM_Files_Len := 0;
      for I in Modules.Entries'First .. Modules.Entries'Last loop
         exit when I > Ret.RAM_Files'Length;
         Ret.RAM_Files (Ret.RAM_Files_Len + 1) := (
            Start  => Modules.Entries (I).Begin_Address,
            Length => Storage_Count (
                        To_Integer (Modules.Entries (I).End_Address) -
                        To_Integer (Modules.Entries (I).Begin_Address)
                      )
         );
         Ret.RAM_Files_Len := Ret.RAM_Files_Len + 1;
      end loop;

      return Ret;
   end Get_Info;
   ----------------------------------------------------------------------------
   procedure HCF is
   begin
      --  Interrupts ought to be disabled every iteration and not only once
      --  because of spurious interrupts.
      loop
         Disable_Interrupts;
         Wait_For_Interrupt;
      end loop;
   end HCF;

   procedure Enable_Interrupts is
   begin
      System.Machine_Code.Asm ("sti", Volatile => True);
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      System.Machine_Code.Asm ("cli", Volatile => True);
   end Disable_Interrupts;

   procedure Wait_For_Interrupt is
   begin
      System.Machine_Code.Asm ("hlt", Volatile => True);
   end Wait_For_Interrupt;

   procedure Pause is
   begin
      System.Machine_Code.Asm ("pause", Volatile => True);
   end Pause;
   ----------------------------------------------------------------------------
   procedure Debug_Print (Message : Character) is
   begin
      Wrappers.Port_Out (16#E9#, Character'Pos (Message));
      if not Config.Is_Embedded then
         Arch.Stivale2.Print_Terminal (Message);
      end if;
   end Debug_Print;

   procedure Debug_Print (Message : String) is
   begin
      for C of Message loop
         Debug_Print (C);
      end loop;
   end Debug_Print;
   ----------------------------------------------------------------------------
   procedure Devices_Hook is
      package ST renames Stivale2;
      package C is new
         System.Address_To_Access_Conversions (ST.Framebuffer_Tag);

      Fb : constant access ST.Framebuffer_Tag := C.To_Pointer
         (To_Address (ST.Get_Tag (ST.Stivale_Tag, ST.Framebuffer_ID)));
   begin
      if not Config.Is_Embedded then
         if not Devices.BootFB.Init (Fb) then goto Error; end if;
         if not Devices.PS2Mouse.Init    then goto Error; end if;
         if not Devices.PS2Keyboard.Init then goto Error; end if;
      end if;

      if not Devices.Serial.Init then goto Error; end if;
      return;

   <<Error>>
      Lib.Panic.Soft_Panic ("Architectural VFS hook failed");
   end Devices_Hook;

   function PRCTL_Hook (Code : Natural; Arg : System.Address) return Boolean is
      Int_Arg : constant Unsigned_64 := Unsigned_64 (To_Integer (Arg));
      A : Unsigned_64 with Address => Arg;
   begin
      case Code is
         when 1 => Wrappers.Write_FS (Int_Arg);
         when 2 => A := Wrappers.Read_FS;
         when 3 => Wrappers.Write_GS (Int_Arg);
         when 4 => A := Wrappers.Read_GS;
         when others => return False;
      end case;
      return True;
   end PRCTL_Hook;
   ----------------------------------------------------------------------------
   package Conv is new System.Address_To_Access_Conversions (Paging.Page_Map);

   function Create_Table return Page_Table is
      Map : constant Paging.Page_Map_Acc := Paging.New_Map;
   begin
      return Page_Table (Conv.To_Address (Conv.Object_Pointer (Map)));
   end Create_Table;

   function Destroy_Table return Boolean is
   begin
      return True;
   end Destroy_Table;

   function Make_Active (Map : Page_Table) return Boolean is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      Paging.Make_Active (Table);
      return True;
   end Make_Active;

   function Is_Active (Map : Page_Table) return Boolean is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      return Paging.Is_Loaded (Table);
   end Is_Active;

   function Translate_Address
      (Map     : Page_Table;
       Virtual : System.Address) return System.Address
   is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
      Addr : constant Integer_Address := To_Integer (Virtual);
   begin
      return To_Address (Paging.Virtual_To_Physical (Table, Addr));
   end Translate_Address;

   function Map_Range
      (Map            : Page_Table;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions) return Boolean
   is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      Paging.Map_Range (
         Table,
         To_Integer (Virtual_Start),
         To_Integer (Physical_Start),
         Unsigned_64 (Length),
         (
            Present         => True,
            Read_Write      => not Permissions.Read_Only,
            User_Supervisor => Permissions.User_Accesible,
            Write_Through   => False,
            Cache_Disable   => False,
            Accessed        => False,
            Dirty           => False,
            PAT             => False,
            Global          => Permissions.Global
         ),
         not Permissions.Executable
      );
      return True;
   end Map_Range;

   function Remap_Range
      (Map           : Page_Table;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions) return Boolean
   is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      Paging.Remap_Range (
         Table,
         To_Integer (Virtual_Start),
         Unsigned_64 (Length),
         (
            Present         => True,
            Read_Write      => not Permissions.Read_Only,
            User_Supervisor => Permissions.User_Accesible,
            Write_Through   => False,
            Cache_Disable   => False,
            Accessed        => False,
            Dirty           => False,
            PAT             => False,
            Global          => Permissions.Global
         ),
         not Permissions.Executable
      );
      return True;
   end Remap_Range;

   function Unmap_Range
      (Map           : Page_Table;
       Virtual_Start : System.Address;
       Length        : Storage_Count) return Boolean
   is
      Table : constant Paging.Page_Map_Acc :=
         Paging.Page_Map_Acc (Conv.To_Pointer (System.Address (Map)));
   begin
      Paging.Unmap_Range (
         Table,
         To_Integer (Virtual_Start),
         Unsigned_64 (Length)
      );
      return True;
   end Unmap_Range;

   --  TODO: Code this 4 bad boys once the VMM makes use of them.

   procedure Flush_Local_TLB (Addr : System.Address) is
   begin
      null;
   end Flush_Local_TLB;

   procedure Flush_Local_TLB (Addr : System.Address; Len : Storage_Count) is
   begin
      null;
   end Flush_Local_TLB;

   procedure Flush_Global_TLBs (Addr : System.Address) is
   begin
      null;
   end Flush_Global_TLBs;

   procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count) is
   begin
      null;
   end Flush_Global_TLBs;
end Arch;
