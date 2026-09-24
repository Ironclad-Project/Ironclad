--  arch-pci.adb: Architecture-specific PCI code.
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

with Arch.ACPI;

package body Arch.PCI with SPARK_Mode => Off is
   procedure Fetch_ECAM_Address (ECAM : out System.Address) is
      ACPI_Address : ACPI.Table_Record;
   begin
      ECAM := System.Null_Address;

      if not ACPI.Is_Supported then
         return;
      end if;

      ACPI.FindTable (ACPI.MCFG_Signature, ACPI_Address);
      if ACPI_Address.Virt_Addr = 0 then
         return;
      end if;

      declare
         Table : Arch.ACPI.MCFG
            with Import, Address => To_Address (ACPI_Address.Virt_Addr);
      begin
         ECAM := To_Address (Integer_Address (Table.Root_ECAM_Addr));
         Arch.ACPI.Unref_Table (ACPI_Address);
      end;
   end Fetch_ECAM_Address;

   --  State of a search for host bridges, passed to Found_Bridge.
   type Bridge_Search is record
      Roots : Bus_Set;
      Count : Natural;
   end record;

   --  uACPI functions, returning 0 (UACPI_STATUS_OK) on success.
   function Find_Devices
      (HID  : System.Address;
       CB   : System.Address;
       User : System.Address) return Unsigned_32
      with Import, Convention => C, External_Name => "uacpi_find_devices";

   function Eval_Simple_Integer
      (Parent : System.Address;
       Path   : System.Address;
       Value  : System.Address) return Unsigned_32
      with Import, Convention => C,
           External_Name => "uacpi_eval_simple_integer";

   --  Called by uACPI for every host bridge found, with the search as User.
   --  Returns 0 (UACPI_ITERATION_DECISION_CONTINUE) to keep iterating.
   function Found_Bridge
      (User  : System.Address;
       Node  : System.Address;
       Depth : Unsigned_32) return Unsigned_32
      with Convention => C;

   function Found_Bridge
      (User  : System.Address;
       Node  : System.Address;
       Depth : Unsigned_32) return Unsigned_32
   is
      pragma Unreferenced (Depth);
      SEG_Path : constant String := "_SEG" & Character'Val (0);
      BBN_Path : constant String := "_BBN" & Character'Val (0);
      Search   : Bridge_Search with Import, Address => User;
      Segment  : Unsigned_64 := 0;
      BBN      : Unsigned_64 := 0;
      Bus      : Unsigned_8;
   begin
      --  A missing _SEG means segment 0, and a missing _BBN is taken as bus
      --  0. Only their low 16 and 8 bits are the segment and the bus.
      if Eval_Simple_Integer (Node, SEG_Path'Address, Segment'Address) /= 0
      then
         Segment := 0;
      end if;
      if Eval_Simple_Integer (Node, BBN_Path'Address, BBN'Address) /= 0 then
         BBN := 0;
      end if;
      Bus := Unsigned_8 (BBN and 16#FF#);

      --  PCIe bridges are found by both searches, and count once.
      if (Segment and 16#FFFF#) = 0 and not Search.Roots (Bus) then
         Search.Roots (Bus) := True;
         Search.Count := Search.Count + 1;
      end if;
      return 0;
   exception
      when Constraint_Error =>
         return 0;
   end Found_Bridge;

   procedure Find_Root_Buses (Roots : out Bus_Set; Count : out Natural) is
      PCIe_HID : constant String := "PNP0A08" & Character'Val (0);
      PCI_HID  : constant String := "PNP0A03" & Character'Val (0);
      Search   : Bridge_Search;
      Discard  : Unsigned_32;
   begin
      Roots := [others => False];
      Count := 0;
      if not ACPI.Is_Supported then
         return;
      end if;

      Search := (Roots => [others => False], Count => 0);
      Discard := Find_Devices
         (PCIe_HID'Address, Found_Bridge'Address, Search'Address);
      Discard := Find_Devices
         (PCI_HID'Address, Found_Bridge'Address, Search'Address);
      Roots := Search.Roots;
      Count := Search.Count;
   end Find_Root_Buses;
end Arch.PCI;
