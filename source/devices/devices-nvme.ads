--  devices-nvme.ads: NVMe driver.
--  Copyright (C) 2025 no92
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

with Lib.Synchronization;

package Devices.NVMe with SPARK_Mode => Off is
   function Init return Boolean;

private
   pragma Warnings (Off, "size clause forces biased representation");

   type Unsigned_1 is mod 2 ** 1;
   type Unsigned_2 is mod 2 ** 2;
   type Unsigned_3 is mod 2 ** 3;
   type Unsigned_4 is mod 2 ** 4;
   type Unsigned_6 is mod 2 ** 6;
   type Unsigned_10 is mod 2 ** 10;
   type Unsigned_12 is mod 2 ** 12;
   type Unsigned_13 is mod 2 ** 13;
   type Unsigned_14 is mod 2 ** 14;
   type Unsigned_15 is mod 2 ** 15;
   type Unsigned_24 is mod 2 ** 24;
   type Unsigned_128 is mod 2 ** 128;

   type NVMe_Padding is array (Natural range <>) of Unsigned_8;
   type NVMe_Boolean is (False, True);
   for NVMe_Boolean use (
      False => 0,
      True => 1
   );
   for NVMe_Boolean'Size use Unsigned_1'Size;

   --  common types reused throughout
   type NVMe_GUID is array (1 .. 16) of Unsigned_8;
   subtype NVMe_NS_Id is Unsigned_32;

   --  CAP @ MMIO 0x00 (NVMe 1.3d 3.1.1)
   type NVMe_Register_CAP_MQES is range 1 .. 65536;
   for NVMe_Register_CAP_MQES'Size use Unsigned_16'Size;

   type NVMe_Register_Controller_Capabilities is record
      Maximum_Queue_Entries_Supported : NVMe_Register_CAP_MQES;
      Contiguous_Queues_Required : Boolean;
      Arbitration_Mechanism_Supported : Unsigned_2;
      Timeout : Unsigned_8;
      Doorbell_Stride : Unsigned_4;
      NVM_Subsystem_Reset_Supported : Boolean;
      Command_Sets_Supported : Unsigned_8;
      Boot_Partition_Support : Boolean;
      Memory_Page_Size_Minimum : Unsigned_4;
      Memory_Page_Size_Maximum : Unsigned_4;
   end record with Size => 64, Volatile;
   for NVMe_Register_Controller_Capabilities use record
      Maximum_Queue_Entries_Supported at 0 range 0 .. 15;
      Contiguous_Queues_Required at 0 range 16 .. 16;
      Arbitration_Mechanism_Supported at 0 range 17 .. 18;
      Timeout at 0 range 24 .. 31;
      Doorbell_Stride at 0 range 32 .. 35;
      NVM_Subsystem_Reset_Supported at 0 range 36 .. 36;
      Command_Sets_Supported at 0 range 37 .. 44;
      Boot_Partition_Support at 0 range 45 .. 45;
      Memory_Page_Size_Minimum at 0 range 48 .. 51;
      Memory_Page_Size_Maximum at 0 range 52 .. 55;
   end record;

   --  VS @ MMIO 0x08 (NVMe 1.3d 3.1.2)
   type NVMe_Register_Version is record
      Tertiary : Unsigned_8;
      Minor : Unsigned_8;
      Major : Unsigned_16;
   end record with Size => 32, Volatile;

   --  CC @ MMIO 0x14 (NVMe 1.3d 3.1.5)
   type NVMe_Register_Controller_Configuration is record
      Enable : Boolean;
      IO_Command_Set_Selected : Unsigned_3;
      Memory_Page_Size : Unsigned_4;
      Arbitration_Mechanism_Selected : Unsigned_3;
      Shutdown_Notification : Unsigned_2;
      IO_Submission_Queue_Entry_Size : Unsigned_4;
      IO_Completion_Queue_Entry_Size : Unsigned_4;
   end record with Size => 32, Volatile;
   for NVMe_Register_Controller_Configuration use record
      Enable at 0 range 0 .. 0;
      IO_Command_Set_Selected at 0 range 4 .. 6;
      Memory_Page_Size at 0 range 7 .. 10;
      Arbitration_Mechanism_Selected at 0 range 11 .. 13;
      Shutdown_Notification at 0 range 14 .. 15;
      IO_Submission_Queue_Entry_Size at 0 range 16 .. 19;
      IO_Completion_Queue_Entry_Size at 0 range 20 .. 23;
   end record;

   --  CSTS @ MMIO 0x1C (NVMe 1.3d 3.1.6)
   type NVMe_Register_Controller_Status is record
      Ready : Boolean;
      Controller_Fatal_Status : Boolean;
      Shutdown_Status : Unsigned_2;
      NVM_Subsystem_Reset_Occurred : Boolean;
      Processing_Paused : Boolean;
   end record with Size => 32, Volatile;
   for NVMe_Register_Controller_Status use record
      Ready at 0 range 0 .. 0;
      Controller_Fatal_Status at 0 range 1 .. 1;
      Shutdown_Status at 0 range 2 .. 3;
      NVM_Subsystem_Reset_Occurred at 0 range 4 .. 4;
      Processing_Paused at 0 range 5 .. 5;
   end record;

   --  AQA @ MMIO 0x24 (NVMe 1.3d 3.1.8)
   type NVMe_AQA_Admin_Queue_Size is range 1 .. 4096;
   for NVMe_AQA_Admin_Queue_Size'Size use Unsigned_12'Size;

   type NVMe_Register_Admin_Queue_Attributes is record
      Admin_Submission_Queue_Size : NVMe_AQA_Admin_Queue_Size;
      Reserved_0 : Unsigned_4 := 0;
      Admin_Completion_Queue_Size : NVMe_AQA_Admin_Queue_Size;
      Reserved_1 : Unsigned_4 := 0;
   end record with Size => 32, Volatile;
   for NVMe_Register_Admin_Queue_Attributes use record
      Admin_Submission_Queue_Size at 0 range 0 .. 11;
      Reserved_0 at 0 range 12 .. 15;
      Admin_Completion_Queue_Size at 0 range 16 .. 27;
      Reserved_1 at 0 range 28 .. 31;
   end record;

   --  *DBL @ MMIO 0x1000 (NVMe 1.3d 3.1.16 + 3.1.17)
   type NVMe_Doorbell is record
      Doorbell : Unsigned_16 := 0;
      Reserved : Unsigned_16 := 0;
   end record with Size => 32, Volatile;
   for NVMe_Doorbell use record
      Doorbell at 0 range 0 .. 15;
      Reserved at 0 range 16 .. 31;
   end record;
   type NVMe_Doorbell_Acc is access all NVMe_Doorbell;

   --  MMIO Register Layout (NVMe 1.3d 3.1)
   type NVMe_Registers is record
      Controller_Capabilities : NVMe_Register_Controller_Capabilities;
      Version : NVMe_Register_Version;
      Interrupt_Mask_Set : Unsigned_32;
      Interrupt_Mask_Clear : Unsigned_32;
      Controller_Configuration : NVMe_Register_Controller_Configuration;
      Reserved : NVMe_Padding (16#18# .. 16#1B#);
      Controller_Status : NVMe_Register_Controller_Status;
      NVM_Subsystem_Reset : Unsigned_32;
      Admin_Queue_Attributes : NVMe_Register_Admin_Queue_Attributes;
      Admin_Submission_Queue_Base_Address : Unsigned_64;
      Admin_Completion_Queue_Base_Address : Unsigned_64;
      Unimplemented : NVMe_Padding (16#38# .. 16#FFF#);
   end record with Size => 16#1000# * 8, Volatile;
   type NVMe_Registers_Acc is access all NVMe_Registers;

   --  Submission Queue Entry Layout (NVMe 1.3d 4.2)
   type Fused_Operation_Type is (Normal, Fused_First, Fused_Second);
   for Fused_Operation_Type use (
      Normal => 2#00#,
      Fused_First => 2#01#,
      Fused_Second => 2#10#
   );
   for Fused_Operation_Type'Size use Unsigned_2'Size;

   type Data_Transfer_Type is (PRP, SGL_Single_Buffer, SGL_Segment);
   for Data_Transfer_Type use (
      PRP => 2#00#,
      SGL_Single_Buffer => 2#01#,
      SGL_Segment => 2#10#
   );
   for Data_Transfer_Type'Size use Unsigned_2'Size;

   type Submission_Queue_Entry_Dword_0 is record
      Opcode : Unsigned_8;
      Fused_Operation : Fused_Operation_Type := Normal;
      Data_Transfer : Data_Transfer_Type := PRP;
      Command_Id : Unsigned_16;
   end record with Size => 32, Volatile;
   for Submission_Queue_Entry_Dword_0 use record
      Opcode at 0 range 0 .. 7;
      Fused_Operation at 0 range 8 .. 9;
      Data_Transfer at 0 range 14 .. 15;
      Command_Id at 0 range 16 .. 31;
   end record;

   type Submission_Queue_Entry_Data_Pointer is array (1 .. 2) of Unsigned_64;
   type Submission_Queue_Entry_Dwords is array (10 .. 15) of Unsigned_32;

   --  Admin command set commands (NVMe 1.3d 5)
   type Admin_Opcode is
      (Create_IO_SQ, Create_IO_CQ, Identify, Set_Features, Reserved);
   for Admin_Opcode use (
      Create_IO_SQ => 1,
      Create_IO_CQ => 5,
      Identify => 6,
      Set_Features => 9,
      Reserved => 16#FF#
   );
   for Admin_Opcode'Size use Unsigned_8'Size;

   type Queue_Priority_Type is (Urgent, High, Medium, Low);
   for Queue_Priority_Type use (
      Urgent => 2#00#,
      High => 2#01#,
      Medium => 2#10#,
      Low => 2#11#
   );
   for Queue_Priority_Type'Size use Unsigned_2'Size;

   type Identify_CNS_Type is (
      Identify_Namespace,
      Identify_Controller,
      Active_Namespaces
   );
   for Identify_CNS_Type use (
      Identify_Namespace => 0,
      Identify_Controller => 1,
      Active_Namespaces => 2
   );
   for Identify_CNS_Type'Size use Unsigned_8'Size;

   type Set_Feature_Id_Type is (Reserved, Number_Of_Queues);
   for Set_Feature_Id_Type use (
      Reserved => 0,
      Number_Of_Queues => 7
   );
   for Set_Feature_Id_Type'Size use Unsigned_8'Size;

   type Set_Features_Command_Dwords is array (11 .. 15) of Unsigned_32;
   type Set_Features_Command (Id : Set_Feature_Id_Type := Reserved) is record
      Reserved_0 : Unsigned_24 := 0;

      case Id is
         --  NVMe 1.3d 5.21.1.7
         when Number_Of_Queues =>
            IO_Submission_Queues : Unsigned_16;
            IO_Completion_Queues : Unsigned_16;
         when others =>
            Dword : Set_Features_Command_Dwords := [others => 0];
      end case;
   end record with Size => 192;
   for Set_Features_Command use record
      Id at 0 range 0 .. 7;
      Reserved_0 at 0 range 8 .. 31;
   end record;

   type IO_Queue_Size is range 1 .. 65536;
   for IO_Queue_Size'Size use Unsigned_16'Size;

   type Admin_Submission_Queue_Entry (Opcode : Admin_Opcode := Reserved)
   is record
      Fused_Operation : Fused_Operation_Type := Normal;
      Data_Transfer : Data_Transfer_Type := PRP;
      Command_Id : Unsigned_16 := 0;
      Namespace_Id : NVMe_NS_Id := 0;
      Metadata_Pointer : Unsigned_64 := 0;
      Data_Pointer : Submission_Queue_Entry_Data_Pointer := [others => 0];

      case Opcode is
         --  NVMe 1.3d 5.4
         when Create_IO_SQ =>
            --  Dword 10
            SQ_Queue_Id : Unsigned_16;
            SQ_Queue_Size : IO_Queue_Size;
            --  Dword 11
            SQ_Physically_Contiguous : Boolean;
            SQ_Queue_Priority : Queue_Priority_Type := Urgent;
            SQ_Reserved : Unsigned_13 := 0;
            SQ_Completion_Queue_Id : Unsigned_16;
         --  NVMe 1.3d 5.3
         when Create_IO_CQ =>
            --  Dword 10
            CQ_Queue_Id : Unsigned_16;
            CQ_Queue_Size : IO_Queue_Size;
            --  Dword 11
            CQ_Physically_Contiguous : Boolean;
            CQ_Interrupts_Enabled : Boolean;
            CQ_Reserved : Unsigned_14 := 0;
            CQ_Interrupt_Vector : Unsigned_16 := 0;
         --  NVMe 1.3d 5.15.1
         when Identify =>
            --  Dword 10
            Identify_CNS : Identify_CNS_Type;
            Identify_Reserved : Unsigned_8 := 0;
            Identify_Controller_Id : Unsigned_16 := 0;
         --  NVMe 1.3d 5.21
         when Set_Features =>
            --  Dwords 10 - 15
            Set_Feature : Set_Features_Command;
         when others =>
            Dwords : Submission_Queue_Entry_Dwords := [others => 0];
      end case;
   end record with Size => 512, Pack, Volatile;
   for Admin_Submission_Queue_Entry use record
      Opcode at 0 range 0 .. 7;

      Fused_Operation at 0 range 8 .. 9;
      Data_Transfer at 0 range 14 .. 15;
      Command_Id at 0 range 16 .. 31;
      Namespace_Id at 4 range 0 .. 31;
      Metadata_Pointer at 16 range 0 .. 63;
      Data_Pointer at 24 range 0 .. 127;
   end record;

   type Admin_Submission_Queue_Entries is array (Natural range <>)
      of aliased Admin_Submission_Queue_Entry with Pack;

   --  I/O command set commands
   type IO_Opcode is (Write, Read, Reserved);
   for IO_Opcode use (
      Write => 1,
      Read => 2,
      Reserved => 16#FF#
   );
   for IO_Opcode'Size use Unsigned_8'Size;

   type IO_Submission_Queue_Entry (Opcode : IO_Opcode := Reserved) is record
      Fused_Operation : Fused_Operation_Type := Normal;
      Data_Transfer : Data_Transfer_Type := PRP;
      Command_Id : Unsigned_16;
      Namespace_Id : NVMe_NS_Id := 0;
      Metadata_Pointer : Unsigned_64 := 0;
      Data_Pointer : Submission_Queue_Entry_Data_Pointer := [others => 0];

      case Opcode is
         when Write =>
            --  Dwords 10/11
            Write_Starting_LBA : Unsigned_64;
            --  Dword 12
            Write_Number_Of_LBAs : Unsigned_16;
            Write_Reserved_0 : Unsigned_4 := 0;
            Write_Directive_Type : Unsigned_4 := 0;
            Write_Reserved_1 : Unsigned_2 := 0;
            Write_Protection_Information : Unsigned_4 := 0;
            Write_Force_Unit_Access : Boolean := False;
            Write_Limited_Retry : Boolean := False;
            --  Dword 13
            Write_Dataset_Management : Unsigned_8 := 0;
            Write_Reserved_2 : Unsigned_24 := 0;
            --  Dword 14
            Write_Expected_Initial_Logical_Block_Reference_Tag : Unsigned_32
               := 0;
            --  Dword 15
            Write_Expected_Logical_Block_Application_Tag : Unsigned_16 := 0;
            Write_Expected_Logical_Block_Application_Tag_Mask : Unsigned_16
               := 0;
         --  NVMe 1.3d 6.9
         when Read =>
            --  Dwords 10/11
            Read_Starting_LBA : Unsigned_64;
            --  Dword 12
            Read_Number_Of_LBAs : Unsigned_16;
            Read_Reserved_0 : Unsigned_10 := 0;
            Read_Protection_Information : Unsigned_4 := 0;
            Read_Force_Unit_Access : Boolean := False;
            Read_Limited_Retry : Boolean := False;
            --  Dword 13
            Read_Dataset_Management : Unsigned_8 := 0;
            Read_Reserved_1 : Unsigned_24 := 0;
            --  Dword 14
            Read_Expected_Initial_Logical_Block_Reference_Tag : Unsigned_32
               := 0;
            --  Dword 15
            Read_Expected_Logical_Block_Application_Tag : Unsigned_16 := 0;
            Read_Expected_Logical_Block_Application_Tag_Mask : Unsigned_16
               := 0;
         when others =>
            Dwords : Submission_Queue_Entry_Dwords := [others => 0];
      end case;
   end record with Size => 512, Pack, Volatile;
   for IO_Submission_Queue_Entry use record
      Opcode at 0 range 0 .. 7;

      Fused_Operation at 0 range 8 .. 9;
      Data_Transfer at 0 range 14 .. 15;
      Command_Id at 0 range 16 .. 31;
      Namespace_Id at 4 range 0 .. 31;
      Metadata_Pointer at 16 range 0 .. 63;
      Data_Pointer at 24 range 0 .. 127;
   end record;

   type IO_Submission_Queue_Entries is array (Natural range <>)
      of aliased IO_Submission_Queue_Entry with Pack;

   --  Completion Queue Entry Layout (NVMe 1.3d 4.6)
   type Completion_Queue_Entry_Dword3 is record
      Command_Id : Unsigned_16 := 0;
      Phase_Tag : Boolean := False;
      Status : Unsigned_15 := 0;
   end record with Size => 32, Volatile;
   for Completion_Queue_Entry_Dword3 use record
      Command_Id at 0 range 0 .. 15;
      Phase_Tag at 0 range 16 .. 16;
      Status at 0 range 17 .. 31;
   end record;

   type Completion_Queue_Entry is record
      Dword0 : Unsigned_32 := 0;
      Reserved : Unsigned_32 := 0;
      SQ_Head_Pointer : Unsigned_16 := 0;
      SQ_ID : Unsigned_16 := 0;
      Dword3 : Completion_Queue_Entry_Dword3 := (others => <>);
   end record with Size => 128, Volatile;
   type Completion_Queue_Entries is array (Natural range <>)
      of aliased Completion_Queue_Entry with Pack;

   --  Queue Data structures
   type Queue_Slot_Id is mod 2 ** 16;
   subtype Admin_Queue_Depth is Natural range 2 .. 4096;

   type Admin_Queue is record
      Queue_Id : Natural;
      Depth : Admin_Queue_Depth;

      --  Needed for setting/unsetting the Interrupt Mask/Clear registers
      Controller_Regs : NVMe_Registers_Acc;
      SQ_Tail_Doorbell : NVMe_Doorbell_Acc;
      CQ_Head_Doorbell : NVMe_Doorbell_Acc;

      SQ_Addr : Memory.Virtual_Address;
      CQ_Addr : Memory.Virtual_Address;

      SQ_Tail : Unsigned_16 := 0;
      CQ_Head : Unsigned_16 := 0;
      Slot : Queue_Slot_Id := 0;

      CQ_Phase : Boolean := True;
   end record;
   type Admin_Queue_Acc is access all Admin_Queue;

   function Setup_Admin_Queue
      (M : NVMe_Registers_Acc;
       Id : Natural;
       Depth : Admin_Queue_Depth) return Admin_Queue_Acc;

   subtype IO_Queue_Depth is Natural range 2 .. 65536;

   type IO_Queue is record
      Queue_Id : Natural;
      Depth : IO_Queue_Depth;

      --  Needed for setting/unsetting the Interrupt Mask/Clear registers
      Controller_Regs : NVMe_Registers_Acc;
      SQ_Tail_Doorbell : NVMe_Doorbell_Acc;
      CQ_Head_Doorbell : NVMe_Doorbell_Acc;

      SQ_Addr : Memory.Virtual_Address;
      CQ_Addr : Memory.Virtual_Address;

      SQ_Tail : Unsigned_16 := 0;
      CQ_Head : Unsigned_16 := 0;
      Slot : Queue_Slot_Id := 0;

      CQ_Phase : Boolean := True;
   end record;
   type IO_Queue_Acc is access all IO_Queue;

   function Setup_IO_Queue
      (M : NVMe_Registers_Acc;
       Id : Natural;
       Admin_Queue : Admin_Queue_Acc) return IO_Queue_Acc;

   --  Commands
   function Submit_Admin_Command
      (Queue : Admin_Queue_Acc;
       Command : in out Admin_Submission_Queue_Entry)
      return Completion_Queue_Entry;

   function Submit_IO_Command
      (Queue : IO_Queue_Acc;
       Command : in out IO_Submission_Queue_Entry)
      return Completion_Queue_Entry;

   --  Identify Namespace (CNS 0x00) (NVMe 1.3d 5.15.2)
   type Identify_LBA_Format is record
      Metadata_Size : Unsigned_16;
      LBA_Data_Size : Unsigned_8;
      Relative_Performance : Unsigned_2;
      Reserved : Unsigned_6;
   end record with Size => 32, Pack;
   type Identify_LBA_Formats is array (Natural range <>)
      of aliased Identify_LBA_Format with Pack;

   type Identify_Namespace_Data is record
      Namespace_Size : Unsigned_64;
      Namespace_Capacity : Unsigned_64;
      Namespace_Utilization : Unsigned_64;
      Namespace_Features : Unsigned_8;
      Number_of_LBA_Formats : Unsigned_8;
      Formatted_LBA_Size : Unsigned_8;
      Metadata_Capabilities : Unsigned_8;
      End_To_End_Data_Protection_Capabilities : Unsigned_8;
      End_To_End_Data_Protection_Type_Settings : Unsigned_8;
      Namespace_Multipath_IO_Namespace_Sharing_Capabilities : Unsigned_8;
      Reservation_Capabilities : Unsigned_8;
      Format_Progress_Indicator : Unsigned_8;
      Deallocate_Logical_Block_Features : Unsigned_8;
      Namespace_Atomic_Write_Unit_Normal : Unsigned_16;
      Namespace_Atomic_Write_Unit_Power_Fail : Unsigned_16;
      Namespace_Atomic_Compare_Write_unit : Unsigned_16;
      Namespace_Atomic_Boundary_Size_Normal : Unsigned_16;
      Namespace_Atomic_Boundary_Offset : Unsigned_16;
      Namespace_Atomic_Boundary_Size_Power_Fail : Unsigned_16;
      Namespace_Optimal_IO_Boundary : Unsigned_16;
      NVM_Capacity : Unsigned_128;
      Reserved_0 : NVMe_Padding (64 .. 103);
      Namespace_GUID : NVMe_GUID;
      IEEE_Extended_Unique_Identifier : Unsigned_64;
      LBA_Format_Support : Identify_LBA_Formats (0 .. 15);
      Reserved_1 : NVMe_Padding (192 .. 383);
      Vendor_Specific : NVMe_Padding (384 .. 4095);
   end record with Size => 32768, Pack;
   type Identify_Namespace_Data_Acc is access all Identify_Namespace_Data;

   function Namespace_Identify
      (Q : Admin_Queue_Acc;
       Namespace : NVMe_NS_Id) return Identify_Namespace_Data_Acc;

   --  Identify Controller (CNS 0x01) (NVMe 1.3d 5.15.3)
   type Identify_Controller_Data is record
      PCI_Vendor_ID : Unsigned_16;
      PCI_Subsystem_Vendor_ID : Unsigned_16;
      Serial_Number : String (1 .. 20);
      Model_Number : String (1 .. 40);
      Firmware_Revision : String (1 .. 8);
      Recommended_Arbitration_Burst : Unsigned_8;
      IEEE_OUI_Identifier : Unsigned_24;
      Controller_Multipath_IO_Namespace_Sharing_Capabilities : Unsigned_8;
      Maximum_Data_Transfer_size : Unsigned_8;
      Controller_Id : Unsigned_16;
      --  Same value as reported in the register for 1.2+
      Version : NVMe_Register_Version;
      RTD3_Resume_Latency : Unsigned_32;
      RTD3_Entry_Latency : Unsigned_32;
      Optional_Asynchronous_Events_Supported : Unsigned_32;
      Controller_Attributes : Unsigned_32;
      Reserved_0 : NVMe_Padding (100 .. 111);
      FRU_GUID : NVMe_GUID;
      Reserved_1 : NVMe_Padding (128 .. 239);
      Reserved_2 : NVMe_Padding (240 .. 255);
      Optional_Admin_Command_Support : Unsigned_16;
      Abort_Command_Limit : Unsigned_8;
      Asynchronous_Event_Request_Limit : Unsigned_8;
      Firmware_Updates : Unsigned_8;
      Log_Page_Attributes : Unsigned_8;
      Error_Log_Page_Entries : Unsigned_8;
      Number_Of_Power_States_Support : Unsigned_8;
      Admin_Vendor_Specific_Command_Configuration : Unsigned_8;
      Autonomous_Power_State_Transition_Attributes : Unsigned_8;
      Warning_Composite_Temperature_Threshold : Unsigned_16;
      Critical_Composite_Temperature_Threshold : Unsigned_16;
      Maximum_Time_For_Firmware_Activation : Unsigned_16;
      Host_Memory_Buffer_Preferred_Size : Unsigned_32;
      Host_Memory_Buffer_Minimum_Size : Unsigned_32;
      Total_NVM_Capacity : Unsigned_128;
      Unallocated_NVM_Capacity : Unsigned_128;
      Replay_Protected_Memory_Block_Support : Unsigned_32;
      Extended_Device_Selftest_Time : Unsigned_16;
      Device_Selftest_Options : Unsigned_8;
      Firmware_Update_Granularity : Unsigned_8;
      Keep_Alive_Support : Unsigned_16;
      Host_Controlled_Thermal_Management_Attributes : Unsigned_16;
      Minimum_Thermal_Management_Temperature : Unsigned_16;
      Maximum_Thermal_Management_Temperature : Unsigned_16;
      Sanitize_Capabilities : Unsigned_32;
      Reserved_3 : NVMe_Padding (332 .. 511);
      Submission_Queue_Entry_Size : Unsigned_8;
      Completion_Queue_Entry_Size : Unsigned_8;
      Maximum_Outstanding_Commands : Unsigned_16;
      Number_Of_Namespaces : Unsigned_32;
      Optional_NVM_Command_Support : Unsigned_16;
      Fused_Operation_Support : Unsigned_16;
      Format_NVM_Attributes : Unsigned_8;
      Volatile_Write_Cache : Unsigned_8;
      Atomic_Write_Unit_Normal : Unsigned_16;
      Atomic_Write_Unit_Power_Fail : Unsigned_16;
      NVM_Vendor_Specific_Command_Configuration : Unsigned_8;
      Reserved_4 : Unsigned_8;
      Atomic_Compare_Write_Unit : Unsigned_16;
      Reserved_5 : Unsigned_16;
      SGL_Support : Unsigned_32;
      Reserved_6 : NVMe_Padding (540 .. 767);
      NVM_Subsystem_NVMe_Qualified_Name : String (1 .. 256);
      Reserved_7 : NVMe_Padding (1024 .. 1791);
      Fabrics_Reserved : NVMe_Padding (1792 .. 2047);
      Power_State_Descriptors : NVMe_Padding (2048 .. 3071);
      Vendor_Specific : NVMe_Padding (3072 .. 4095);
   end record with Size => 32768, Pack;
   type Identify_Controller_Data_Acc is access all Identify_Controller_Data;

   function Controller_Identify
      (Q : Admin_Queue_Acc) return Identify_Controller_Data_Acc;

   --  Identify Active Namespaces (CNS 0x02) (NVMe 1.3d 5.15.4)
   type Active_Namespace_Id_List is array (1 .. 1024) of NVMe_NS_Id;
   type Active_Namespace_Id_List_Acc is access all Active_Namespace_Id_List;

   function Active_Namespaces
      (Q : Admin_Queue_Acc) return Active_Namespace_Id_List_Acc;

   --  I/O functions
   subtype Sector_Data is Operation_Data (1 .. 512);
   type Sector_Cache is record
      Data : Sector_Data := [others => 0];
      LBA_Offset : Unsigned_64 := 0;
      Is_Used : Boolean := False;
      Is_Dirty : Boolean := False;
   end record with Alignment => 16;
   type Sector_Caches is array (Natural range <>) of Sector_Cache;

   type Namespace_Data is record
      Queue : IO_Queue_Acc;
      Namespace_Id : Unsigned_32;
      LBA_Size : Natural;
      LBA_Count : Unsigned_64;
      Caches : Sector_Caches (1 .. 8000) := [others => <>];
      Next_Evict : Natural range 1 .. 8000 := 1;
      Mutex : aliased Lib.Synchronization.Mutex
         := Lib.Synchronization.Unlocked_Mutex;
   end record;
   type Namespace_Data_Acc is access all Namespace_Data;

   function NS_Read
      (D : Namespace_Data_Acc;
       LBA : Unsigned_64;
       Data_Buffer : out Sector_Data) return Boolean;

   function NS_Write
      (D : Namespace_Data_Acc;
       LBA : Unsigned_64;
       Data_Buffer : Sector_Data) return Boolean;

   procedure Get_Cache_Index
      (Drive : Namespace_Data_Acc;
       LBA : Unsigned_64;
       Idx : out Natural;
       Success : out Boolean);

   procedure Read
      (Key : System.Address;
       Offset : Unsigned_64;
       Data : out Operation_Data;
       Ret_Count : out Natural;
       Success : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Write
      (Key : System.Address;
       Offset : Unsigned_64;
       Data : Operation_Data;
       Ret_Count : out Natural;
       Success : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Sync (Key : System.Address; Success : out Boolean);

   procedure Sync_Range
      (Key     : System.Address;
       Offset  : Unsigned_64;
       Count   : Unsigned_64;
       Success : out Boolean);

   --  Operations on the Controller
   procedure Controller_Await_Ready
      (M : NVMe_Registers_Acc; Enabled : Boolean);
   procedure Disable_Controller (M : NVMe_Registers_Acc);
   procedure Enable_Controller (M : NVMe_Registers_Acc);

   procedure Request_IO_Queues
      (Admin_Queue : Admin_Queue_Acc;
       SQ_Count : Unsigned_16;
       CQ_Count : Unsigned_16);

   --  Attempts to initialize a namespace.
   --  This does not necessarily succeed, for example when the namespace is
   --  not reporting as active or has capacity 0.
   procedure Init_Namespace
      (NS_Id : NVMe_NS_Id;
       Drive_Idx : Natural;
       Admin_Queue : Admin_Queue_Acc;
       IO_Queue : IO_Queue_Acc);
end Devices.NVMe;
