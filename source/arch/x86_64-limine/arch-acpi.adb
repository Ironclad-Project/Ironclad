--  arch-acpi.adb: ACPI parsing and scanning.
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

with Arch.Limine;
with Arch.MMU;
with Lib.Messages;
with Lib.Alignment;

package body Arch.ACPI is
   --  Request to get the RSDP.
   --  Response is a pointer to an RSDP_Response.
   RSDP_Request : Limine.Request :=
      (ID => (Limine.Limine_Common_Magic_1, Limine.Limine_Common_Magic_2,
              16#c5e77b6b397e7b43#, 16#27637845accdcf3c#),
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   --  Globals to keep track of scanned tables.
   Use_XSDT     : Boolean         := False;
   Root_Address : Virtual_Address := Null_Address;

   package A is new Lib.Alignment (Integer_Address);

   function ScanTables return Boolean is
      RSDPonse : Limine.RSDP_Response
         with Import, Address => RSDP_Request.Response;
      Table : RSDP
         with Import, Address =>
            To_Address (To_Integer (RSDPonse.Addr) + Memory_Offset);

      Map_Addr : Integer_Address;
      Map_Len  : Integer_Address;
   begin
      Map_Addr := A.Align_Down (To_Integer (RSDPonse.Addr), MMU.Page_Size);
      Map_Len  := To_Integer (RSDPonse.Addr) - Map_Addr;
      Map_Len  := A.Align_Up (MMU.Page_Size + Map_Len, MMU.Page_Size);

      if not MMU.Map_Range
         (Map            => MMU.Kernel_Table,
          Physical_Start => To_Address (Map_Addr),
          Virtual_Start  => To_Address (Memory_Offset + Map_Addr),
          Length         => Storage_Count (Map_Len),
          Permissions    =>
            (Is_User_Accesible => False,
             Can_Read          => True,
             Can_Write         => False,
             Can_Execute       => False,
             Is_Global         => True))
      then
         Lib.Messages.Put_Line ("Failed to map RSD* table");
         return False;
      end if;

      if Table.Signature /= "RSD PTR " then
         return False;
      end if;

      if Table.Revision >= 2 and Table.XSDT_Address /= Null_Address then
         Use_XSDT     := True;
         Root_Address := Physical_Address (Table.XSDT_Address);
      else
         Use_XSDT     := False;
         Root_Address := Physical_Address (Table.RSDT_Address);
      end if;

      Root_Address := Memory_Offset + Root_Address;
      Map_Addr     := A.Align_Down (Root_Address, MMU.Page_Size);

      --  TODO: Ideally we should map every table on demand, but that is
      --  really really tedious, so we just map 10 pages at the beginning of
      --  the root address, and that should be enough on 90% of the scenarios
      --  out there. And when it isnt, welp, we will implement it.
      if not MMU.Map_Range
         (Map            => MMU.Kernel_Table,
          Physical_Start => To_Address (Map_Addr - Memory_Offset),
          Virtual_Start  => To_Address (Map_Addr),
          Length         => MMU.Page_Size * 10,
          Permissions    =>
            (Is_User_Accesible => False,
             Can_Read          => True,
             Can_Write         => False,
             Can_Execute       => False,
             Is_Global         => True))
      then
         Lib.Messages.Put_Line ("Failed to map final RSD* table");
         return False;
      end if;

      return True;
   end ScanTables;

   function FindTable (Signature : SDT_Signature) return Virtual_Address is
      Root : RSDT with Import, Address => To_Address (Root_Address);

      Limit : constant Natural :=
         (Natural (Root.Header.Length) - Root.Header'Size / 8) /
         (if Use_XSDT then 8 else 4);

      Returned_Virt :  Virtual_Address := Null_Address;
      Returned_Phys : Physical_Address := Null_Address;
   begin
      for I in 1 .. Limit loop
         if Use_XSDT then
            declare
               Entries : XSDT_Entries (1 .. Limit)
                  with Import, Address => Root.Entries'Address;
            begin
               Returned_Phys := Physical_Address (Entries (I));
            end;
         else
            declare
               Entries : RSDT_Entries (1 .. Limit)
                  with Import, Address => Root.Entries'Address;
            begin
               Returned_Phys := Physical_Address (Entries (I));
            end;
         end if;

         Returned_Virt := Returned_Phys + Memory_Offset;

         declare
            Test_Header : SDT_Header
               with Import, Address => To_Address (Returned_Virt);
         begin
            if Test_Header.Signature = Signature then
               return Returned_Virt;
            end if;
         end;
      end loop;

      return Null_Address;
   end FindTable;
end Arch.ACPI;
