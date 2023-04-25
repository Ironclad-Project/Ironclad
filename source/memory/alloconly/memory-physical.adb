--  memory-physical.adb: Physical memory allocator and other utils.
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
with Interfaces; use Interfaces;
with Lib.Synchronization; use Lib.Synchronization;
with Lib.Panic;
with Lib.Alignment;
with Memory.Virtual;

package body Memory.Physical with SPARK_Mode => Off is
   --  Memory region info that sits in the front of the free memory region.
   type Region_Info;
   type Region_Info_Acc is access all Region_Info;
   type Region_Info is record
      Alloc_Address : System.Address;  --  From the end of this struct.
      End_Address   : System.Address;  --  From the end of this struct.
      Next_Region   : aliased Region_Info_Acc; --  0 if end.
   end record;

   package Conv is new System.Address_To_Access_Conversions (Region_Info);

   --  Information that the allocator keeps track of.
   Total_Memory, Available_Memory, Free_Memory : Memory.Size;
   Alloc_Mutex  : aliased Binary_Semaphore;
   First_Region : aliased Region_Info_Acc;

   procedure Init_Allocator (Memmap : Arch.Boot_Memory_Map) is
      Region_Info_Sz : constant Integer_Address := Region_Info'Size / 8;
      Current_Region : access Region_Info_Acc   := First_Region'Access;
      Region_Start : Integer_Address;
      Region_Size  : Integer_Address;
   begin
      --  Find available memory regions and count available memory.
      Total_Memory     := 0;
      Available_Memory := 0;
      Free_Memory      := 0;
      for E of Memmap loop
         if E.MemType = Arch.Memory_Free and
            Integer_Address (E.Length) > Region_Info_Sz
         then
            Region_Start := To_Integer (E.Start) + Memory_Offset;
            Region_Size  := Integer_Address (E.Length) - Region_Info_Sz;

            Current_Region.all :=
               Region_Info_Acc (Conv.To_Pointer (To_Address (Region_Start)));
            Current_Region.all.all := (
               Alloc_Address => To_Address (Region_Start + Region_Info_Sz),
               End_Address   => To_Address (Region_Start + Region_Size),
               Next_Region   => null
            );
            Free_Memory      := Free_Memory      + Size (Region_Size);
            Available_Memory := Available_Memory + Size (E.Length);
            Current_Region   := Current_Region.all.Next_Region'Access;
         end if;
         Total_Memory := Total_Memory + Size (E.Length);
      end loop;

      --  Last touches and go on with our day.
      if First_Region = null then
         Lib.Panic.Hard_Panic ("No memory for the system");
      end if;
      Lib.Synchronization.Release (Alloc_Mutex);
   end Init_Allocator;

   function Alloc (Sz : Interfaces.C.size_t) return Virtual_Address is
      package A is new Lib.Alignment (Integer_Address);

      Region    : Region_Info_Acc := First_Region;
      Size      : Integer_Address := Integer_Address (Sz);
      Alignment : Integer_Address;
      Test      : Integer_Address;
   begin
      --  Check the specific GNAT semantics.
      if Size = Integer_Address (Interfaces.C.size_t'Last) then
         Lib.Panic.Hard_Panic ("Storage error (size_t'Last passed)");
      elsif Size = 0 then
         Size := 1;
      end if;

      --  Get the appropiate alignment as per API.
      if Size >= Memory.Virtual.Page_Size then
         Alignment := Memory.Virtual.Page_Size;
      else
         Alignment := 1;
      end if;

      --  Find an allocation aligned to 4K.
      --  XXX: Requiring 4K decreases the efficiency of this allocator by
      --  quite a bit, but thats required by the API.
      --  Maybe lift that requirement?
      Lib.Synchronization.Seize (Alloc_Mutex);
      while Region /= null loop
         Test := A.Align_Up (To_Integer (Region.Alloc_Address), Alignment);
         if Test + Size <= To_Integer (Region.End_Address) then
            goto Found_Region;
         else
            Region := Region.Next_Region;
         end if;
      end loop;

      --  Handle OOM.
      Lib.Panic.Hard_Panic ("Memory exhausted (OOM)");

   <<Found_Region>>
      Free_Memory          := Free_Memory - Memory.Size (Size);
      Region.Alloc_Address := To_Address (Test + Size);
      Lib.Synchronization.Release (Alloc_Mutex);
      return Test;
   end Alloc;

   procedure Free (Address : Interfaces.C.size_t) is
      pragma Unreferenced (Address);
   begin
      return;
   end Free;

   procedure Get_Statistics (Stats : out Statistics) is
   begin
      Lib.Synchronization.Seize (Alloc_Mutex);
      Stats :=
         (Total     => Total_Memory,
          Available => Available_Memory,
          Free      => Free_Memory);
      Lib.Synchronization.Release (Alloc_Mutex);
   end Get_Statistics;
end Memory.Physical;
