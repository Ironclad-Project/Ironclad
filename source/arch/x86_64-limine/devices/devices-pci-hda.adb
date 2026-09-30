--  devices-pci-hda.adb: Intel HDA driver.
--  Copyright (C) 2026 streaksu
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
with Alignment;
with Arch.APIC;
with Arch.Clocks;
with Arch.CPU;
with Arch.IDT;
with Arch.MMU;
with Arch.Snippets;
with Devices.Mixer;
with Memory.MMU;
with Messages;
with Scheduler;
with Sound;
with Sound.OSS_IOCTL; use Sound.OSS_IOCTL;

package body Devices.PCI.HDA with SPARK_Mode => Off is
   package C1 is new System.Address_To_Access_Conversions (Controller);
   package A  is new Alignment (Unsigned_64);

   --  Controllers that take interrupts, for Interrupt_Handler to find.
   Controllers : array (1 .. 4) of Controller_Acc := [others => null];

   procedure Init (Success : out Boolean) is
      PCI_Dev : Devices.PCI.PCI_Device;
      Found   : Boolean;
   begin
      --  HDA controllers are multimedia devices of subclass 03h, programming
      --  interface 00h (Intel ICH10 319973-003, 18.1.6 to 18.1.8).
      for Idx in 1 .. Devices.PCI.Enumerate_Devices (4, 3, 0) loop
         Devices.PCI.Search_Device (4, 3, 0, Idx, PCI_Dev, Found);
         exit when not Found;
         Init_Controller (PCI_Dev, Found);
      end loop;

      --  A controller that cannot be used is no reason to stop other drivers.
      Success := True;
   exception
      when Constraint_Error =>
         Success := True;
   end Init;

   procedure Init_Controller
      (PCI_Dev : Devices.PCI.PCI_Device;
       Success : out Boolean)
   is
      C       : constant Controller_Ref := new Controller;
      BAR     : Devices.PCI.Base_Address_Register;
      Caps    : Unsigned_16;
      Card    : Unsigned_32;
      Mixer   : Unsigned_32;
      Dev     : Unsigned_32;
      Handle  : Device_Handle;
      Lowest  : Unsigned_32 := Rate_Table (Rate_48K).Hz;
      Highest : Unsigned_32 := Rate_Table (Rate_48K).Hz;
   begin
      Devices.PCI.Get_BAR (PCI_Dev, 0, BAR, Success);
      if not Success or else not BAR.Is_MMIO then
         Messages.Put_Line ("hda: controller has no register BAR");
         Success := False;
         return;
      end if;

      Devices.PCI.Enable_Bus_Mastering (PCI_Dev);

      C.PCI_Dev := PCI_Dev;
      C.MMIO    := BAR.Base + Memory.Memory_Offset;
      Memory.MMU.Map_Range
         (Map            => Memory.MMU.Kernel_Table,
          Physical_Start => To_Address (BAR.Base),
          Virtual_Start  => To_Address (C.MMIO),
          Length         => Storage_Count (A.Align_Up
             (Unsigned_64'Max (BAR.Size, Memory.MMU.Page_Size),
              Memory.MMU.Page_Size)),
          Permissions    =>
             (Is_User_Accessible => False,
              Can_Read           => True,
              Can_Write          => True,
              Can_Execute        => False,
              Is_Global          => True),
          Success        => Success,
          Caching        => Arch.MMU.Uncacheable);
      if not Success then
         Messages.Put_Line ("hda: could not map the registers");
         return;
      end if;

      Reset_Controller (C, Success);
      if not Success then
         Messages.Put_Line ("hda: controller did not leave reset");
         return;
      end if;

      --  Output stream descriptors come after the input ones, and so do their
      --  interrupt bits.
      Caps      := Read_16 (C, Reg_GCAP);
      C.Uses_64 := (Caps and 1) /= 0;
      if (Shift_Right (Caps, 12) and 16#F#) = 0 then
         Messages.Put_Line ("hda: controller has no output streams");
         Success := False;
         return;
      end if;
      C.SIE_Bit := Natural (Shift_Right (Caps, 8) and 16#F#);
      C.SD := Stream_Descriptors + Unsigned_32 (C.SIE_Bit) * Stream_Stride;

      Allocate_Buffers (C, Success);
      if not Success then
         Messages.Put_Line ("hda: could not allocate DMA memory");
         return;
      end if;

      Init_Commands (C, Success);
      if not Success then
         Messages.Put_Line ("hda: could not start the CORB and RIRB");
         return;
      end if;

      Find_Codec (C, Success);
      if not Success then
         Messages.Put_Line ("hda: no codec with an analog output");
         return;
      end if;
      Configure_Routes (C);

      --  OSS devices start out as 8 bit unsigned mono at 8 kHz.
      C.App_Format   := AFMT_U8;
      C.App_Channels := 1;
      C.Rate         := Nearest_Rate (C, 8_000);

      Setup_Interrupt (C, Success);
      if Success then
         Write_32 (C, Reg_INTCTL, INTCTL_GIE or Shift_Left (1, C.SIE_Bit));
      else
         Messages.Put_Line ("hda: no interrupt, output will not stop");
      end if;

      Sound.Add_Card ("hda", "Intel HD Audio", "", Card, Success);
      if not Success then
         return;
      end if;

      Sound.Add_Mixer
         (ID       => "hda",
          Name     => "Intel HD Audio mixer",
          Res      =>
             (Data        => C1.To_Address (C1.Object_Pointer (C)),
              Is_Block    => False,
              Block_Size  => 4096,
              Block_Count => 0,
              Read        => null,
              Write       => null,
              Sync        => null,
              Sync_Range  => null,
              IO_Control  => Mixer_IO_Control'Access,
              IO_Argument => Mixer_IO_Argument'Access,
              Mmap        => null,
              Poll        => null,
              Remove      => null),
          Card_Idx => Card,
          Idx      => Mixer,
          Success  => Success);
      if not Success then
         return;
      end if;

      Sound.Add_Audio_Device
         (Name      => "Intel HD Audio playback",
          Res       =>
             (Data        => C1.To_Address (C1.Object_Pointer (C)),
              Is_Block    => False,
              Block_Size  => 4096,
              Block_Count => 0,
              Read        => null,
              Write       => Write'Access,
              Sync        => null,
              Sync_Range  => null,
              IO_Control  => DSP_IO_Control'Access,
              IO_Argument => DSP_IO_Argument'Access,
              Mmap        => null,
              Poll        => Poll'Access,
              Remove      => null),
          Mixer_Idx => Mixer,
          Card_Idx  => Card,
          Idx       => Dev,
          Success   => Success);
      if not Success then
         return;
      end if;

      for I in Rate_Table'Range loop
         if (C.Rates and Shift_Left (1, I)) /= 0 then
            Lowest  := Unsigned_32'Min (Lowest,  Rate_Table (I).Hz);
            Highest := Unsigned_32'Max (Highest, Rate_Table (I).Hz);
         end if;
      end loop;
      Sound.Set_Audio_Device_Limits
         (Idx          => Dev,
          Formats      => Supported_Formats,
          Min_Rate     => Lowest,
          Max_Rate     => Highest,
          Min_Channels => 1,
          Max_Channels => 2,
          Caps         => PCM_CAP_TRIGGER or PCM_CAP_ANALOGOUT);

      --  The interrupt wakes those waiting on the device, and on the default
      --  device if it is this one.
      Handle := Devices.Fetch ("dsp" & Dev'Image);
      if Handle /= Error_Handle then
         Devices.Set_Wait_Key (Handle, C.all'Address);
      end if;
      Handle := Devices.Fetch ("dsp");
      if Dev = 0 and Handle /= Error_Handle then
         Devices.Set_Wait_Key (Handle, C.all'Address);
      end if;

      Messages.Put_Line ("hda: playback is on dsp" & Dev'Image);
   exception
      when Constraint_Error =>
         Messages.Put_Line ("hda: controller setup failed");
         Success := False;
   end Init_Controller;

   procedure Reset_Controller (C : Controller_Ref; Success : out Boolean) is
      Caps        : constant Unsigned_16 := Read_16 (C, Reg_GCAP);
      Descriptors : constant Unsigned_32 :=
         Unsigned_32 (Shift_Right (Caps, 12) and 16#F#) +
         Unsigned_32 (Shift_Right (Caps, 8)  and 16#F#) +
         Unsigned_32 (Shift_Right (Caps, 3)  and 16#1F#);
      SD          : Unsigned_32;
      Stopped     : Boolean;
   begin
      --  The CORB, the RIRB, and every stream are stopped before the reset is
      --  asserted.
      Write_8 (C, Reg_CORBCTL, Read_8 (C, Reg_CORBCTL) and not CORBCTL_Run);
      Write_8 (C, Reg_RIRBCTL, Read_8 (C, Reg_RIRBCTL) and not 2#111#);
      for I in 1 .. Descriptors loop
         SD := Stream_Descriptors + (I - 1) * Stream_Stride;
         Write_8 (C, SD + SD_CTL, Read_8 (C, SD + SD_CTL) and not SD_CTL_RUN);
         Wait_8 (C, SD + SD_CTL, SD_CTL_RUN, 0, Stopped);
      end loop;
      Wait_8 (C, Reg_CORBCTL, CORBCTL_Run, 0, Stopped);
      Wait_8 (C, Reg_RIRBCTL, RIRBCTL_DMA, 0, Stopped);

      --  The link reset is held for longer than the 100 us the codecs need to
      --  lock on the bit clock.
      Write_32 (C, Reg_GCTL, Read_32 (C, Reg_GCTL) and not GCTL_CRST);
      Wait_8 (C, Reg_GCTL, GCTL_CRST, 0, Success);
      if not Success then
         return;
      end if;
      Arch.Clocks.Busy_Monotonic_Sleep (1_000_000);
      Write_32 (C, Reg_GCTL, Read_32 (C, Reg_GCTL) or GCTL_CRST);
      Wait_8 (C, Reg_GCTL, GCTL_CRST, GCTL_CRST, Success);
      if not Success then
         return;
      end if;

      --  Codecs ask for an address within 25 frames, 521 us, of the reset
      --  being lifted.
      Arch.Clocks.Busy_Monotonic_Sleep (1_000_000);

      --  No wake events, and no interrupts until the stream is set up.
      Write_16 (C, Reg_WAKEEN, Read_16 (C, Reg_WAKEEN) and 16#8000#);
      Write_32 (C, Reg_INTCTL, 0);
   end Reset_Controller;

   procedure Allocate_Buffers (C : Controller_Ref; Success : out Boolean) is
      DMA_Mem  : constant not null DMA_Page_Acc    := new DMA_Page;
      Ring_Mem : constant not null Ring_Buffer_Acc := new Ring_Buffer;
      BDL      : Integer_Address;
      Period   : Unsigned_64;
   begin
      --  Kernel allocations are page aligned, which is past the 128 byte
      --  alignment all of these need.
      for B of DMA_Mem.all loop
         B := 0;
      end loop;
      for B of Ring_Mem.all loop
         B := 0;
      end loop;
      C.DMA_Base  := To_Integer (DMA_Mem.all'Address);
      C.Ring_Base := To_Integer (Ring_Mem.all'Address);

      --  Without 64 bit addressing, it all has to be in the low 4 GiB.
      if not C.Uses_64 and
         (Physical (C.DMA_Base) + DMA_Page'Length > 2 ** 32 or
          Physical (C.Ring_Base) + Ring_Size > 2 ** 32)
      then
         Success := False;
         return;
      end if;

      --  Each period is a BDL entry, and interrupts once fetched.
      BDL := C.DMA_Base + BDL_Offset;
      for I in Unsigned_64 range 0 .. Period_Count - 1 loop
         Period := Physical (C.Ring_Base) + I * Period_Size;
         Store_32 (BDL + Integer_Address (I * 16),      Low (Period));
         Store_32 (BDL + Integer_Address (I * 16 + 4),  High (Period));
         Store_32 (BDL + Integer_Address (I * 16 + 8),  Period_Size);
         Store_32 (BDL + Integer_Address (I * 16 + 12), 1);
      end loop;
      Barrier;
      Success := True;
   end Allocate_Buffers;

   procedure Init_Commands (C : Controller_Ref; Success : out Boolean) is
      CORB  : constant Unsigned_64 := Physical (C.DMA_Base);
      RIRB  : constant Unsigned_64 := CORB + RIRB_Offset;
      Reset : Boolean;
   begin
      --  The CORB is stopped, sized, pointed at its memory, and has both its
      --  pointers reset before being started.
      Write_8 (C, Reg_CORBCTL, Read_8 (C, Reg_CORBCTL) and not CORBCTL_Run);
      Wait_8 (C, Reg_CORBCTL, CORBCTL_Run, 0, Success);
      if not Success then
         return;
      end if;
      Set_Ring_Size (C, Reg_CORBSIZE, C.CORB_Mask);
      Write_32 (C, Reg_CORBLBASE, Low (CORB));
      Write_32 (C, Reg_CORBUBASE, High (CORB));

      --  The read pointer reset should read back as 1 before being cleared,
      --  but some controllers, the one of QEMU among them, never report that,
      --  so only the clear is checked.
      Write_16 (C, Reg_CORBRP,
                (Read_16 (C, Reg_CORBRP) and 16#7F00#) or CORBRP_Reset);
      Wait_16 (C, Reg_CORBRP, CORBRP_Reset, CORBRP_Reset, 1_000, Reset);
      Write_16 (C, Reg_CORBRP, Read_16 (C, Reg_CORBRP) and 16#7F00#);
      Wait_16 (C, Reg_CORBRP, CORBRP_Reset, 0, Register_Timeout, Success);
      if not Success then
         return;
      end if;
      Write_16 (C, Reg_CORBWP, Read_16 (C, Reg_CORBWP) and 16#FF00#);
      Write_8 (C, Reg_CORBCTL, Read_8 (C, Reg_CORBCTL) or CORBCTL_Run);
      Wait_8 (C, Reg_CORBCTL, CORBCTL_Run, CORBCTL_Run, Success);
      if not Success then
         return;
      end if;

      --  The RIRB goes the same way, with a response interrupt after every
      --  response. It is polled for them, but some controllers, again the one
      --  of QEMU among them, stop fetching verbs once the response count is
      --  reached until the flag of the interrupt is cleared, which is what
      --  enabling it is for. With CIE clear, it does not reach the CPU (HDA
      --  rev 1.0a, 4.4.2.2, 3.3.25 to 3.3.31, 3.5).
      Write_8 (C, Reg_RIRBCTL, Read_8 (C, Reg_RIRBCTL) and not 2#111#);
      Wait_8 (C, Reg_RIRBCTL, RIRBCTL_DMA, 0, Success);
      if not Success then
         return;
      end if;
      Set_Ring_Size (C, Reg_RIRBSIZE, C.RIRB_Mask);
      Write_32 (C, Reg_RIRBLBASE, Low (RIRB));
      Write_32 (C, Reg_RIRBUBASE, High (RIRB));
      Write_16 (C, Reg_RIRBWP, RIRBWP_Reset);
      Write_16 (C, Reg_RINTCNT, (Read_16 (C, Reg_RINTCNT) and 16#FF00#) or 1);
      Write_8 (C, Reg_RIRBSTS, RIRBSTS_Clear);
      Write_8 (C, Reg_RIRBCTL, (Read_8 (C, Reg_RIRBCTL) and not 2#111#) or
               RIRBCTL_DMA or RIRBCTL_RINTCTL);
      C.RIRB_Read := 0;
   end Init_Commands;

   procedure Set_Ring_Size
      (C    : Controller_Ref;
       Reg  : Unsigned_32;
       Mask : out Unsigned_32)
   is
      Size     : constant Unsigned_8 := Read_8 (C, Reg);
      Selected : Unsigned_8;
   begin
      --  The largest size of those offered.
      if (Size and 16#40#) /= 0 then
         Selected := 2#10#;
         Mask     := 255;
      elsif (Size and 16#20#) /= 0 then
         Selected := 2#01#;
         Mask     := 15;
      else
         Selected := 2#00#;
         Mask     := 1;
      end if;
      Write_8 (C, Reg, (Size and 16#FC#) or Selected);
   end Set_Ring_Size;
   ----------------------------------------------------------------------------
   procedure Command
      (C        : Controller_Ref;
       Node     : Node_ID;
       Verb     : Unsigned_32;
       Response : out Unsigned_32;
       Success  : out Boolean)
   is
      RIRB  : constant Integer_Address := C.DMA_Base + RIRB_Offset;
      Extra : Unsigned_32;
      WP    : Unsigned_32;
      Limit : Time.Timestamp;
   begin
      Response := 0;
      Success  := False;
      Synchronization.Seize (C.Cmd_Mutex);

      --  Responses left over from a verb that timed out are skipped.
      C.RIRB_Read := Unsigned_32 (Read_16 (C, Reg_RIRBWP)) and C.RIRB_Mask;
      Write_8 (C, Reg_RIRBSTS, RIRBSTS_Clear);

      --  The verb goes after the last one given, which the write pointer then
      --  points to.
      WP := (Unsigned_32 (Read_16 (C, Reg_CORBWP)) + 1) and C.CORB_Mask;
      Store_32 (C.DMA_Base + Integer_Address (WP * 4),
                Shift_Left (C.Codec, 28) or Shift_Left (Node, 20) or
                (Verb and 16#F_FFFF#));
      Barrier;
      Write_16
         (C, Reg_CORBWP,
          (Read_16 (C, Reg_CORBWP) and 16#FF00#) or Unsigned_16'Mod (WP));

      --  The response lands after the last one, with a flag telling it apart
      --  from unsolicited ones.
      Limit := Deadline (Command_Timeout);
      loop
         if (Unsigned_32 (Read_16 (C, Reg_RIRBWP)) and C.RIRB_Mask) /=
            C.RIRB_Read
         then
            C.RIRB_Read := (C.RIRB_Read + 1) and C.RIRB_Mask;
            Response := Load_32 (RIRB + Integer_Address (C.RIRB_Read * 8));
            Extra := Load_32 (RIRB + Integer_Address (C.RIRB_Read * 8 + 4));
            Write_8 (C, Reg_RIRBSTS, RIRBSTS_Clear);
            if (Extra and RIRB_Unsolicited) = 0 then
               Success := True;
               exit;
            end if;
         elsif Expired (Limit) then
            Response := 0;
            exit;
         else
            Arch.Snippets.Pause;
         end if;
      end loop;

      Synchronization.Release (C.Cmd_Mutex);
   end Command;

   procedure Send (C : Controller_Ref; Node : Node_ID; Verb : Unsigned_32) is
      Response : Unsigned_32;
      Success  : Boolean;
   begin
      Command (C, Node, Verb, Response, Success);
   end Send;

   procedure Get_Parameter
      (C       : Controller_Ref;
       Node    : Node_ID;
       Param   : Unsigned_32;
       Value   : out Unsigned_32;
       Success : out Boolean)
   is
   begin
      Command (C, Node, Short_Verb (Verb_Get_Parameter, Param), Value,
               Success);
   end Get_Parameter;

   procedure Power_Up (C : Controller_Ref; Node : Node_ID) is
      Limit   : constant Time.Timestamp := Deadline (Register_Timeout);
      State   : Unsigned_32;
      Success : Boolean;
   begin
      --  D0, which is reached once the actual state reads so.
      Send (C, Node, Short_Verb (Verb_Set_Power, 0));
      loop
         Command (C, Node, Short_Verb (Verb_Get_Power, 0), State, Success);
         exit when not Success or (State and 16#F0#) = 0 or Expired (Limit);
         Arch.Snippets.Pause;
      end loop;
   end Power_Up;
   ----------------------------------------------------------------------------
   procedure Find_Codec (C : Controller_Ref; Success : out Boolean) is
      Present : constant Unsigned_16 := Read_16 (C, Reg_STATESTS) and 16#7FFF#;
   begin
      --  Codecs that answered the reset have their bit set, which is cleared.
      Write_16 (C, Reg_STATESTS, Present);
      for CAd in 0 .. 14 loop
         if (Present and Shift_Left (Unsigned_16 (1), CAd)) /= 0 then
            Probe_Codec (C, Unsigned_32 (CAd), Success);
            if Success then
               return;
            end if;
         end if;
      end loop;
      Success := False;
   end Find_Codec;

   procedure Probe_Codec
      (C       : Controller_Ref;
       CAd     : Unsigned_32;
       Success : out Boolean)
   is
      Vendor : Unsigned_32;
      Nodes  : Unsigned_32;
      Kind   : Unsigned_32;
      Group  : Unsigned_32;
   begin
      --  The root node has the IDs of the codec and the range of its function
      --  groups, of which the first audio one with an output is used.
      C.Codec := CAd;
      Get_Parameter (C, 0, Param_Vendor_ID, Vendor, Success);
      if Success then
         Get_Parameter (C, 0, Param_Node_Count, Nodes, Success);
      end if;
      if not Success then
         return;
      end if;
      Messages.Put_Line
         ("hda: codec " & CAd'Image & " is " &
          Unsigned_64 (Shift_Right (Vendor, 16))'Image & ":" &
          Unsigned_64 (Vendor and 16#FFFF#)'Image);

      for I in 1 .. (Nodes and 16#FF#) loop
         Group := (Shift_Right (Nodes, 16) and 16#FF#) + I - 1;
         exit when Group > Node_ID'Last;
         Get_Parameter (C, Group, Param_Group_Type, Kind, Success);
         if Success and then (Kind and 16#FF#) = 1 then
            Probe_Group (C, Group, Success);
            if Success then
               return;
            end if;
         end if;
      end loop;
      Success := False;
   exception
      when Constraint_Error =>
         Success := False;
   end Probe_Codec;

   procedure Probe_Group
      (C       : Controller_Ref;
       Group   : Node_ID;
       Success : out Boolean)
   is
      PCM     : Unsigned_32 := 0;
      In_Amp  : Unsigned_32 := 0;
      Out_Amp : Unsigned_32 := 0;
      Nodes   : Unsigned_32;
      Node    : Unsigned_32;
   begin
      --  The group holds the defaults for the parameters of its widgets, and
      --  the range of their node IDs.
      Power_Up (C, Group);
      Get_Parameter (C, Group, Param_PCM, PCM, Success);
      Get_Parameter (C, Group, Param_In_Amp_Caps, In_Amp, Success);
      Get_Parameter (C, Group, Param_Out_Amp_Caps, Out_Amp, Success);
      Get_Parameter (C, Group, Param_Node_Count, Nodes, Success);
      if not Success then
         return;
      end if;

      for W of C.Widgets loop
         W.Present := False;
      end loop;
      for I in 1 .. (Nodes and 16#FF#) loop
         Node := (Shift_Right (Nodes, 16) and 16#FF#) + I - 1;
         exit when Node > Node_ID'Last;
         Probe_Widget (C, Node, PCM, In_Amp, Out_Amp);
      end loop;

      Find_Routes (C);
      Success := C.Route_Count /= 0;
   exception
      when Constraint_Error =>
         Success := False;
   end Probe_Group;

   procedure Probe_Widget
      (C       : Controller_Ref;
       Node    : Node_ID;
       PCM     : Unsigned_32;
       In_Amp  : Unsigned_32;
       Out_Amp : Unsigned_32)
   is
      Caps    : Unsigned_32;
      Success : Boolean;
   begin
      Get_Parameter (C, Node, Param_Widget_Caps, Caps, Success);
      if not Success then
         return;
      end if;

      --  Widgets may have format and amplifier parameters of their own, else
      --  those of the group apply.
      declare
         W : Widget renames C.Widgets (Node);
      begin
         W :=
            (Present    => True,
             Caps       => Caps,
             Pin_Caps   => 0,
             Config     => 0,
             PCM        => PCM,
             In_Amp     => In_Amp,
             Out_Amp    => Out_Amp,
             Conn_Count => 0,
             Conns      => [others => 0]);
         if (Caps and Caps_Format) /= 0 then
            Get_Parameter (C, Node, Param_PCM, W.PCM, Success);
         end if;
         if (Caps and Caps_Amp_Params) /= 0 then
            Get_Parameter (C, Node, Param_In_Amp_Caps, W.In_Amp, Success);
            Get_Parameter (C, Node, Param_Out_Amp_Caps, W.Out_Amp, Success);
         end if;
         if Widget_Type (W) = Widget_Pin then
            Get_Parameter (C, Node, Param_Pin_Caps, W.Pin_Caps, Success);
            Command (C, Node, Short_Verb (Verb_Get_Config, 0), W.Config,
                     Success);
         end if;
      end;
      if (Caps and Caps_Conn_List) /= 0 then
         Read_Connections (C, Node);
      end if;
   exception
      when Constraint_Error =>
         null;
   end Probe_Widget;

   procedure Read_Connections (C : Controller_Ref; Node : Node_ID) is
      Length   : Unsigned_32;
      Response : Unsigned_32;
      Value    : Unsigned_32;
      Is_Long  : Boolean;
      Is_Range : Boolean;
      Per_Read : Unsigned_32;
      Index    : Unsigned_32 := 0;
      Previous : Unsigned_32 := 0;
      Success  : Boolean;
   begin
      --  The list is read a few entries at a time, and an entry may close a
      --  range opened by the one before it, which is expanded, so indexes of
      --  inputs are those of the expanded list.
      Get_Parameter (C, Node, Param_Conn_Length, Length, Success);
      if not Success then
         return;
      end if;
      Is_Long  := (Length and 16#80#) /= 0;
      Per_Read := (if Is_Long then 2 else 4);

      while Index < (Length and 16#7F#) loop
         Command (C, Node, Short_Verb (Verb_Get_Connection, Index), Response,
                  Success);
         exit when not Success;
         for K in 0 .. Per_Read - 1 loop
            exit when Index + K >= (Length and 16#7F#);
            if Is_Long then
               Value    := Shift_Right (Response, Natural (16 * K)) and
                           16#FFFF#;
               Is_Range := (Value and 16#8000#) /= 0;
               Value    := Value and 16#7FFF#;
            else
               Value    := Shift_Right (Response, Natural (8 * K)) and 16#FF#;
               Is_Range := (Value and 16#80#) /= 0;
               Value    := Value and 16#7F#;
            end if;

            if Is_Range and Previous < Value then
               for N in Previous + 1 .. Value loop
                  Add_Connection (C.Widgets (Node), N);
               end loop;
            else
               Add_Connection (C.Widgets (Node), Value);
            end if;
            Previous := Value;
         end loop;
         Index := Index + Per_Read;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Read_Connections;

   procedure Add_Connection (W : in out Widget; Node : Unsigned_32) is
   begin
      --  Nodes past short form IDs are never used, and take the place of the
      --  root node, so that the indexes of the rest are kept.
      if W.Conn_Count < Max_Connections then
         W.Conn_Count := W.Conn_Count + 1;
         W.Conns (W.Conn_Count) := (if Node <= Node_ID'Last then Node else 0);
      end if;
   exception
      when Constraint_Error =>
         null;
   end Add_Connection;

   procedure Find_Routes (C : Controller_Ref) is
      R     : Route;
      Found : Boolean;
      Rates : Unsigned_32 := 16#7FF#;
   begin
      C.Route_Count := 0;
      for N in Node_ID loop
         if Is_Output_Pin (C.Widgets (N)) and C.Route_Count < Max_Routes then
            Find_Path (C, N, R, Found);
            if Found then
               C.Route_Count := C.Route_Count + 1;
               C.Routes (C.Route_Count) := R;
               Rates := Rates and C.Widgets (R.DAC).PCM;
               Messages.Put_Line
                  ("hda: pin " & N'Image & " is fed by converter " &
                   R.DAC'Image);
            end if;
         end if;
      end loop;

      --  Every converter plays the stream, so only rates all of them can play
      --  are used. 48 kHz is one all codecs support.
      C.Rates := Rates and 16#7FF#;
      if C.Rates = 0 then
         C.Rates := Shift_Left (1, Rate_48K);
      end if;
   exception
      when Constraint_Error =>
         C.Rates := Shift_Left (1, Rate_48K);
   end Find_Routes;

   procedure Find_Path
      (C     : Controller_Ref;
       Pin   : Node_ID;
       R     : out Route;
       Found : out Boolean)
   is
      Seen   : array (Node_ID) of Boolean := [others => False];
      Parent : array (Node_ID) of Node_ID := [others => 0];
      Input  : array (Node_ID) of Natural := [others => 0];
      Depth  : array (Node_ID) of Natural := [others => 0];
      Queue  : array (1 .. 128) of Node_ID := [others => 0];
      Head   : Natural := 1;
      Tail   : Natural := 1;
      Node   : Node_ID;
      Next   : Node_ID;
      Back   : Node_ID;
      Kind   : Unsigned_32;
   begin
      --  Breadth first from the pin to the closest analog converter able to
      --  take 16 bit samples, through mixers and selectors.
      R     := (Hops => [others => (0, 0)], Length => 0, DAC => 0);
      Found := False;
      Seen (Pin)  := True;
      Depth (Pin) := 1;
      Queue (1)   := Pin;
      while Head <= Tail loop
         Node := Queue (Head);
         Head := Head + 1;
         for I in 1 .. C.Widgets (Node).Conn_Count loop
            Next := C.Widgets (Node).Conns (I);
            if C.Widgets (Next).Present and not Seen (Next) then
               Seen (Next)   := True;
               Parent (Next) := Node;
               Input (Next)  := I - 1;
               Depth (Next)  := Depth (Node) + 1;
               Kind          := Widget_Type (C.Widgets (Next));
               if Kind = Widget_Output and
                  (C.Widgets (Next).Caps and Caps_Digital) = 0 and
                  (C.Widgets (Next).PCM and PCM_B16) /= 0
               then
                  R.Length := Depth (Next);
                  R.DAC    := Next;
                  R.Hops (R.Length) := (Next, 0);
                  Back := Next;
                  for K in reverse 1 .. R.Length - 1 loop
                     R.Hops (K) := (Parent (Back), Input (Back));
                     Back := Parent (Back);
                  end loop;
                  Found := True;
                  return;
               elsif (Kind = Widget_Mixer or Kind = Widget_Selector) and
                     Depth (Next) < Max_Hops and Tail < Queue'Last
               then
                  Tail := Tail + 1;
                  Queue (Tail) := Next;
               end if;
            end if;
         end loop;
      end loop;
   exception
      when Constraint_Error =>
         Found := False;
   end Find_Path;

   procedure Configure_Routes (C : Controller_Ref) is
      Node    : Node_ID;
      Index   : Unsigned_32;
      Kind    : Unsigned_32;
      Control : Unsigned_32;
      Value   : Unsigned_32;
      Success : Boolean;
   begin
      --  Every widget of a route is powered, set to take the input the route
      --  goes through, and has its amplifiers unmuted at 0 dB, which is the
      --  step at their offset. Pins are enabled for output, headphone ones
      --  with their amplifier, and have their external amplifier powered.
      for R of C.Routes (1 .. C.Route_Count) loop
         for K in 1 .. R.Length loop
            Node  := R.Hops (K).Node;
            Index := Unsigned_32 (R.Hops (K).Index);
            Kind  := Widget_Type (C.Widgets (Node));

            if (C.Widgets (Node).Caps and Caps_Power) /= 0 then
               Send (C, Node, Short_Verb (Verb_Set_Power, 0));
            end if;

            if K < R.Length then
               if (Kind = Widget_Pin or Kind = Widget_Selector) and
                  C.Widgets (Node).Conn_Count > 1
               then
                  Send (C, Node, Short_Verb (Verb_Set_Connection, Index));
               end if;
               if (Kind = Widget_Mixer or Kind = Widget_Selector) and
                  (C.Widgets (Node).Caps and Caps_In_Amp) /= 0
               then
                  Send (C, Node, Long_Verb (Verb_Set_Amplifier, Amp_Payload
                     (False, True, True, Index, False,
                      C.Widgets (Node).In_Amp and 16#7F#)));
               end if;
            end if;

            if (C.Widgets (Node).Caps and Caps_Out_Amp) /= 0 then
               Send (C, Node, Long_Verb (Verb_Set_Amplifier, Amp_Payload
                  (True, True, True, 0, False,
                   C.Widgets (Node).Out_Amp and 16#7F#)));
            end if;

            if Kind = Widget_Pin then
               Control := Pin_Control_Out;
               if (C.Widgets (Node).Pin_Caps and Pin_Headphone) /= 0 and
                  (Shift_Right (C.Widgets (Node).Config, 20) and 16#F#) = 2
               then
                  Control := Control or Pin_Control_HP;
               end if;
               Send (C, Node, Short_Verb (Verb_Set_Pin, Control));

               if (C.Widgets (Node).Pin_Caps and Pin_EAPD) /= 0 then
                  Command (C, Node, Short_Verb (Verb_Get_EAPD, 0), Value,
                           Success);
                  Send (C, Node,
                        Short_Verb (Verb_Set_EAPD, (Value and 2#101#) or
                                    2#010#));
               end if;
            end if;
         end loop;
      end loop;

      Apply_Volume (C);
   exception
      when Constraint_Error =>
         null;
   end Configure_Routes;

   procedure Apply_Volume (C : Controller_Ref) is
      Node : Node_ID;
      Caps : Unsigned_32;
   begin
      --  The first amplifier with gain steps from the converter on sets the
      --  volume, 100 being 0 dB, the step at its offset, and 0 muting it if it
      --  can be.
      for R of C.Routes (1 .. C.Route_Count) loop
         for K in reverse 1 .. R.Length loop
            Node := R.Hops (K).Node;
            Caps := C.Widgets (Node).Out_Amp;
            if (C.Widgets (Node).Caps and Caps_Out_Amp) /= 0 and
               (Shift_Right (Caps, 8) and 16#7F#) /= 0
            then
               Send (C, Node, Long_Verb (Verb_Set_Amplifier, Amp_Payload
                  (True, True, False, 0,
                   C.Volume_Left = 0 and (Caps and 16#8000_0000#) /= 0,
                   (Caps and 16#7F#) * C.Volume_Left / 100)));
               Send (C, Node, Long_Verb (Verb_Set_Amplifier, Amp_Payload
                  (True, False, True, 0,
                   C.Volume_Right = 0 and (Caps and 16#8000_0000#) /= 0,
                   (Caps and 16#7F#) * C.Volume_Right / 100)));
               exit;
            end if;
         end loop;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Apply_Volume;

   procedure Set_Volume (C : Controller_Ref; Value : Unsigned_32) is
   begin
      --  Left in the low byte, right in the next one, 0 to 100 each.
      C.Volume_Left  := Unsigned_32'Min (Value and 16#FF#, 100);
      C.Volume_Right :=
         Unsigned_32'Min (Shift_Right (Value, 8) and 16#FF#, 100);
      Apply_Volume (C);
   end Set_Volume;
   ----------------------------------------------------------------------------
   procedure Setup_Interrupt (C : Controller_Ref; Success : out Boolean) is
      Index   : Arch.IDT.IRQ_Index;
      Has_MSI : Boolean;
      Has_X   : Boolean;
      Line    : Unsigned_8;
      PCI_Cmd : Unsigned_16;
      Slot    : Natural := 0;
   begin
      for I in Controllers'Range loop
         if Controllers (I) = null then
            Slot := I;
            exit;
         end if;
      end loop;
      if Slot = 0 then
         Success := False;
         return;
      end if;

      Arch.IDT.Load_ISR (Interrupt_Handler'Address, Index, Success);
      if not Success then
         return;
      end if;
      C.Vector := Integer (Index) - 1;
      Controllers (Slot) := C;

      --  A message signaled interrupt if there is one, which leaves the INTx#
      --  pin unused, else that pin, as the firmware routed it.
      Devices.PCI.Get_MSI_Support (C.PCI_Dev, Has_MSI, Has_X);
      Devices.PCI.Read16 (C.PCI_Dev, 4, PCI_Cmd);
      if Has_MSI then
         Devices.PCI.Set_MSI_Vector
            (Dev         => C.PCI_Dev,
             Vector      => Unsigned_8 (C.Vector),
             Destination => Arch.CPU.Core_Locals (1).LAPIC_ID);
         Devices.PCI.Write16 (C.PCI_Dev, 4, PCI_Cmd or 16#400#);
      else
         Devices.PCI.Read8 (C.PCI_Dev, 16#3C#, Line);
         if Line > 15 then
            Success := False;
            return;
         end if;
         Arch.APIC.IOAPIC_Set_Redirect
            (LAPIC_ID  => Arch.CPU.Core_Locals (1).LAPIC_ID,
             IRQ       => Arch.IDT.IRQ_Index (Natural (Line) + 33),
             IDT_Entry => Index,
             Enable    => True,
             Success   => Success);
         Devices.PCI.Write16 (C.PCI_Dev, 4, PCI_Cmd and not 16#400#);
      end if;
   exception
      when Constraint_Error =>
         Success := False;
   end Setup_Interrupt;

   procedure Interrupt_Handler (Vector : Integer) is
   begin
      for C of Controllers loop
         if C /= null and then C.Vector = Vector then
            Service_Interrupt (C);
            exit;
         end if;
      end loop;
      Arch.APIC.LAPIC_EOI;
   exception
      when Constraint_Error =>
         Arch.APIC.LAPIC_EOI;
   end Interrupt_Handler;

   procedure Service_Interrupt (C : Controller_Ref) is
      Status : Unsigned_8;
   begin
      --  Status bits are cleared by writing them back, a byte at a time, and
      --  the rest of the descriptor is only touched with the lock held, here
      --  as everywhere else.
      if (Read_32 (C, Reg_INTSTS) and Shift_Left (1, C.SIE_Bit)) /= 0 then
         Synchronization.Seize (C.Lock);
         Status := Read_8 (C, C.SD + SD_STS);
         Write_8 (C, C.SD + SD_STS, Status and SD_STS_All);
         if (Status and SD_STS_DESE) /= 0 and C.State = Running then
            Stop_DMA (C);
            Reset_Positions (C);
         else
            Check_Drain (C);
         end if;
         Synchronization.Release (C.Lock);
         Scheduler.Wake_Event (C.all'Address);
      end if;
   end Service_Interrupt;
   ----------------------------------------------------------------------------
   procedure Update_Position (C : Controller_Ref) is
      LPIB : constant Unsigned_32 := Read_32 (C, C.SD + SD_LPIB) and Ring_Mask;
   begin
      --  The link position counts up to the cyclic buffer length and starts
      --  over, so it is only read as an offset.
      C.Play_Total := C.Play_Total +
         Unsigned_64 ((LPIB - C.Last_LPIB) and Ring_Mask);
      C.Last_LPIB := LPIB;
   end Update_Position;

   procedure Check_Drain (C : Controller_Ref) is
   begin
      --  Once the silence after what was queued has been fetched for long
      --  enough, the stream is stopped, before stale data comes up.
      if C.State = Running then
         Update_Position (C);
         if C.Play_Total >=
            A.Align_Up (C.Write_Total, Period_Size) +
            Tail_Periods * Period_Size
         then
            Stop_DMA (C);
            Reset_Positions (C);
         end if;
      end if;
   end Check_Drain;

   procedure Stop_DMA (C : Controller_Ref) is
   begin
      Write_8 (C, C.SD + SD_CTL, Read_8 (C, C.SD + SD_CTL) and not SD_CTL_RUN);
   end Stop_DMA;

   procedure Reset_Positions (C : Controller_Ref) is
   begin
      C.Played_Base := C.Played_Base +
         Unsigned_64'Min (C.Play_Total, C.Write_Total);
      C.Write_Total := 0;
      C.Play_Total  := 0;
      C.Zero_Total  := 0;
      C.Last_LPIB   := 0;
      C.State       := Idle;
   end Reset_Positions;

   procedure Queue_Frames
      (C      : Controller_Ref;
       Data   : Operation_Data;
       First  : Natural;
       Frames : Unsigned_32)
   is
      Ring   : Ring_Buffer with Import, Address => To_Address (C.Ring_Base);
      Target : Ring_Index := Ring_Index'Mod (C.Write_Total);
      Size   : Natural;
      Step   : Natural;
      Source : Natural;
      Left   : Unsigned_16;
      Right  : Unsigned_16;
   begin
      Size   := Natural (Sample_Size (C.App_Format));
      Step   := Size * Natural (C.App_Channels);
      Source := First;
      for F in 1 .. Frames loop
         Left  := Sample (C.App_Format, Data, Source);
         Right := (if C.App_Channels = 2
                   then Sample (C.App_Format, Data, Source + Size)
                   else Left);
         Ring (Target)     := Unsigned_8'Mod (Left);
         Ring (Target + 1) := Unsigned_8'Mod (Shift_Right (Left, 8));
         Ring (Target + 2) := Unsigned_8'Mod (Right);
         Ring (Target + 3) := Unsigned_8'Mod (Shift_Right (Right, 8));
         Target := Target + Frame_Size;
         Source := Source + Step;
         C.Write_Total := C.Write_Total + Frame_Size;
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Queue_Frames;

   procedure Extend_Silence (C : Controller_Ref) is
      Ring   : Ring_Buffer with Import, Address => To_Address (C.Ring_Base);
      Target : constant Unsigned_64 :=
         A.Align_Up (C.Write_Total, Period_Size) +
         Silence_Periods * Period_Size;
      From   : Unsigned_64 := Unsigned_64'Max (C.Zero_Total, C.Write_Total);
   begin
      while From < Target loop
         Ring (Ring_Index'Mod (From)) := 0;
         From := From + 1;
      end loop;
      C.Zero_Total := Unsigned_64'Max (C.Zero_Total, Target);
   end Extend_Silence;
   ----------------------------------------------------------------------------
   procedure Write_Frames
      (C           : Controller_Ref;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Is_Blocking : Boolean)
   is
      Frame  : Operation_Data (1 .. 8) := [others => 0];
      Step   : Natural;
      Done   : Natural := 0;
      Next   : Natural;
      Wanted : Unsigned_32;
      Frames : Unsigned_32;
      Carry  : Boolean := False;
      Queued : Boolean;
      Start  : Boolean;
   begin
      Synchronization.Seize (C.Dev_Mutex);
      begin
         Step := Natural (Sample_Size (C.App_Format) * C.App_Channels);

         --  A frame split between writes is completed first.
         if C.Partial_Len /= 0 then
            while Natural (C.Partial_Len) < Step and Done < Data'Length loop
               C.Partial_Len := C.Partial_Len + 1;
               C.Partial (C.Partial_Len) := Data (Data'First + Done);
               Done := Done + 1;
            end loop;
            Carry := Natural (C.Partial_Len) = Step;
            for I in 1 .. Step loop
               Frame (I) := C.Partial (Unsigned_32 (I));
            end loop;
         end if;

         loop
            Wanted := Unsigned_32 ((Data'Length - Done) / Step);
            Next   := Data'First + Done;
            Frames := 0;
            Queued := False;

            Synchronization.Seize (C.Lock);
            Check_Drain (C);

            --  Past running out, what comes next is played as soon as it can.
            if C.State = Running and C.Play_Total > C.Write_Total then
               C.Write_Total := A.Align_Up (C.Play_Total, Frame_Size);
            end if;

            if Carry and Free_Frames (C) /= 0 then
               Queue_Frames (C, Frame, Frame'First, 1);
               C.Partial_Len := 0;
               Carry  := False;
               Queued := True;
            end if;
            if not Carry then
               Frames := Unsigned_32'Min
                  (Unsigned_32'Min (Free_Frames (C), Period_Frames), Wanted);
               if Frames /= 0 then
                  Queue_Frames (C, Data, Next, Frames);
                  Queued := True;
               end if;
            end if;

            if Queued then
               Extend_Silence (C);
               if C.State = Idle then
                  C.State := Primed;
               end if;
            end if;
            Start := C.State = Primed and C.Trigger;
            Synchronization.Release (C.Lock);

            Done := Done + Natural (Frames) * Step;
            if Start then
               Start_Stream (C);
            end if;
            exit when not Carry and Data'Length - Done < Step;
            if not Queued then
               exit when not Is_Blocking;
               Wait_For_Space (C);
               exit when Scheduler.Is_Doomed;
            end if;
         end loop;

         --  What is left of a frame waits for the rest of it.
         if not Carry and C.Partial_Len = 0 and Data'Length - Done < Step then
            while Done < Data'Length loop
               C.Partial_Len := C.Partial_Len + 1;
               C.Partial (C.Partial_Len) := Data (Data'First + Done);
               Done := Done + 1;
            end loop;
         end if;
      exception
         when Constraint_Error =>
            null;
      end;

      Synchronization.Release (C.Dev_Mutex);
      Ret_Count := Done;
   end Write_Frames;

   procedure Start_Stream (C : Controller_Ref) is
      Format : constant Unsigned_32 := Stream_Format (C.Rate);
      BDL    : constant Unsigned_64 := Physical (C.DMA_Base) + BDL_Offset;
      Ready  : Boolean;
   begin
      Synchronization.Seize (C.Lock);
      Ready := C.State = Primed;
      Synchronization.Release (C.Lock);
      if not Ready then
         return;
      end if;

      --  Converters are given the format of the stream and its number.
      if C.Codec_Format /= Format then
         for R of C.Routes (1 .. C.Route_Count) loop
            Send (C, R.DAC, Long_Verb (Verb_Set_Format, Format));
         end loop;
         C.Codec_Format := Format;
      end if;
      for R of C.Routes (1 .. C.Route_Count) loop
         Send (C, R.DAC,
               Short_Verb (Verb_Set_Stream, Shift_Left (Stream_Tag, 4)));
      end loop;

      --  The descriptor, stopped, is reset and set up with the cyclic buffer
      --  and the format. The interrupt handler leaves it alone for
      --  as long as the stream is not running.
      Wait_8 (C, C.SD + SD_CTL, SD_CTL_RUN, 0, Ready);
      Write_8 (C, C.SD + SD_CTL, SD_CTL_SRST);
      Wait_8 (C, C.SD + SD_CTL, SD_CTL_SRST, SD_CTL_SRST, Ready);
      Write_8 (C, C.SD + SD_CTL, 0);
      Wait_8 (C, C.SD + SD_CTL, SD_CTL_SRST, 0, Ready);
      if not Ready then
         Messages.Put_Line ("hda: output stream did not leave reset");
         return;
      end if;
      Write_8  (C, C.SD + SD_STS, SD_STS_All);
      Write_32 (C, C.SD + SD_CBL, Ring_Size);
      Write_16 (C, C.SD + SD_LVI,
                (Read_16 (C, C.SD + SD_LVI) and 16#FF00#) or
                (Period_Count - 1));
      Write_16 (C, C.SD + SD_FMT, Unsigned_16'Mod (Format));
      Write_32 (C, C.SD + SD_BDPL, Low (BDL));
      Write_32 (C, C.SD + SD_BDPU, High (BDL));
      Write_8  (C, C.SD + SD_CTL_Stream, Shift_Left (Stream_Tag, 4));
      Barrier;

      Synchronization.Seize (C.Lock);
      if C.State = Primed then
         C.Last_LPIB := 0;
         C.State     := Running;
         Write_8 (C, C.SD + SD_CTL, SD_CTL_IOCE or SD_CTL_DEIE or SD_CTL_RUN);
      end if;
      Synchronization.Release (C.Lock);
   exception
      when Constraint_Error =>
         null;
   end Start_Stream;

   procedure Halt (C : Controller_Ref) is
   begin
      Synchronization.Seize (C.Lock);
      if C.State = Running then
         Update_Position (C);
         Stop_DMA (C);
      end if;
      Reset_Positions (C);
      C.Partial_Len := 0;
      Synchronization.Release (C.Lock);
   end Halt;

   procedure Drain (C : Controller_Ref) is
      Registered : Boolean;
      Is_Idle    : Boolean;
      Start      : Boolean;
   begin
      Synchronization.Seize (C.Lock);
      Start := C.State = Primed and C.Trigger;
      Synchronization.Release (C.Lock);
      if Start then
         Start_Stream (C);
      end if;

      --  Stopping is checked for here as well, for when no interrupt comes.
      Scheduler.Begin_Wait;
      Scheduler.Add_Wait_Key (C.all'Address, Registered);
      loop
         Scheduler.Clear_Wake;
         Synchronization.Seize (C.Lock);
         Check_Drain (C);
         Is_Idle := C.State /= Running;
         Synchronization.Release (C.Lock);
         exit when Is_Idle or Scheduler.Is_Doomed;
         Scheduler.Wait_Event
            (Scheduler.No_Deadline, Scheduler.Polled_Sleep_Micros);
      end loop;
      Scheduler.End_Wait;
   end Drain;

   procedure Wait_For_Space (C : Controller_Ref) is
      Registered : Boolean;
      Has_Space  : Boolean;
   begin
      Scheduler.Begin_Wait;
      Scheduler.Add_Wait_Key (C.all'Address, Registered);
      loop
         Scheduler.Clear_Wake;
         Synchronization.Seize (C.Lock);
         Check_Drain (C);
         Has_Space := Free_Frames (C) /= 0;
         Synchronization.Release (C.Lock);
         exit when Has_Space or Scheduler.Is_Doomed;
         Scheduler.Wait_Event
            (Scheduler.No_Deadline, Scheduler.Polled_Sleep_Micros);
      end loop;
      Scheduler.End_Wait;
   end Wait_For_Space;
   ----------------------------------------------------------------------------
   function Can_Queue (C : Controller_Ref) return Boolean is
      Result : Boolean;
   begin
      Synchronization.Seize (C.Lock);
      Check_Drain (C);
      Result := Free_Frames (C) >= Period_Frames;
      Synchronization.Release (C.Lock);
      return Result;
   end Can_Queue;

   procedure DSP_Request
      (C        : Controller_Ref;
       Request  : Unsigned_64;
       Argument : System.Address;
       Success  : out Boolean)
   is
      Value : Unsigned_32 with Import, Address => Argument;
      Info  : array (1 .. 4) of Unsigned_32 with Import, Address => Argument;
      Step    : constant Unsigned_32 :=
         Sample_Size (C.App_Format) * C.App_Channels;
      Total   : Unsigned_64;
      Base    : Unsigned_64;
      Played  : Unsigned_64;
      Written : Unsigned_64;
   begin
      Success := True;
      case Request is
         when SNDCTL_DSP_HALT | SNDCTL_DSP_HALT_OUTPUT =>
            Synchronization.Seize (C.Dev_Mutex);
            Halt (C);
            Synchronization.Release (C.Dev_Mutex);
         when SNDCTL_DSP_SYNC =>
            Synchronization.Seize (C.Dev_Mutex);
            Drain (C);
            Synchronization.Release (C.Dev_Mutex);
         when SNDCTL_DSP_POST | SNDCTL_DSP_NONBLOCK | SNDCTL_DSP_SILENCE |
              SNDCTL_DSP_SKIP | SNDCTL_DSP_SUBDIVIDE |
              SNDCTL_DSP_SETFRAGMENT | SNDCTL_DSP_COOKEDMODE |
              SNDCTL_DSP_PROFILE | SNDCTL_DSP_LOW_WATER | SNDCTL_DSP_POLICY |
              SNDCTL_SETSONG =>
            null;
         when SNDCTL_DSP_SPEED =>
            Synchronization.Seize (C.Dev_Mutex);
            if Value /= 0 and then Nearest_Rate (C, Value) /= C.Rate then
               --  What was queued plays out at the rate it was queued for.
               Drain (C);
               C.Rate := Nearest_Rate (C, Value);
            end if;
            Value := C.Rate;
            Synchronization.Release (C.Dev_Mutex);
         when SNDCTL_DSP_STEREO =>
            Synchronization.Seize (C.Dev_Mutex);
            C.App_Channels := (if Value = 0 then 1 else 2);
            C.Partial_Len  := 0;
            Value := C.App_Channels - 1;
            Synchronization.Release (C.Dev_Mutex);
         when SNDCTL_DSP_CHANNELS =>
            Synchronization.Seize (C.Dev_Mutex);
            if Value /= 0 then
               C.App_Channels := (if Value = 1 then 1 else 2);
               C.Partial_Len  := 0;
            end if;
            Value := C.App_Channels;
            Synchronization.Release (C.Dev_Mutex);
         when SNDCTL_DSP_SETFMT =>
            Synchronization.Seize (C.Dev_Mutex);
            if Value /= 0 then
               --  One of the formats, else the one of the stream.
               C.App_Format :=
                  (if (Value and Supported_Formats) = Value and
                      (Value and (Value - 1)) = 0
                   then Value
                   else AFMT_S16_LE);
               C.Partial_Len := 0;
            end if;
            Value := C.App_Format;
            Synchronization.Release (C.Dev_Mutex);
         when SNDCTL_DSP_GETFMTS =>
            Value := Supported_Formats;
         when SNDCTL_DSP_GETBLKSIZE =>
            Value := Period_Frames * Step;
         when SNDCTL_DSP_GETCAPS =>
            Value := PCM_CAP_OUTPUT or PCM_CAP_TRIGGER or PCM_CAP_ANALOGOUT or
                     1;
         when SNDCTL_DSP_GETTRIGGER =>
            Value := (if C.Trigger then PCM_ENABLE_OUTPUT else 0);
         when SNDCTL_DSP_SETTRIGGER =>
            Synchronization.Seize (C.Dev_Mutex);
            C.Trigger := (Value and PCM_ENABLE_OUTPUT) /= 0;
            if C.Trigger then
               Start_Stream (C);
            else
               Halt (C);
            end if;
            Synchronization.Release (C.Dev_Mutex);
         when SNDCTL_DSP_GETODELAY =>
            Synchronization.Seize (C.Lock);
            Check_Drain (C);
            Total := Pending_Bytes (C);
            Synchronization.Release (C.Lock);
            Value := Unsigned_32'Mod (Shift_Right (Total, Frame_Shift)) *
                     Step + C.Partial_Len;
         when SNDCTL_DSP_GETOSPACE =>
            --  audio_buf_info: fragments, fragstotal, fragsize, bytes.
            Synchronization.Seize (C.Lock);
            Check_Drain (C);
            Total := Unsigned_64 (Free_Frames (C));
            Synchronization.Release (C.Lock);
            Info (1) := Unsigned_32'Mod (Shift_Right (Total, Period_Shift));
            Info (2) := Max_Pending / Period_Size;
            Info (3) := Period_Frames * Step;
            Info (4) := Unsigned_32'Mod (Total) * Step;
         when SNDCTL_DSP_GETOPTR =>
            --  count_info: bytes, blocks, ptr.
            Synchronization.Seize (C.Lock);
            Check_Drain (C);
            Base    := C.Played_Base;
            Played  := C.Play_Total;
            Written := C.Write_Total;
            Synchronization.Release (C.Lock);
            Total := Shift_Right
               (Base + Unsigned_64'Min (Played, Written), Frame_Shift);
            Info (1) := Unsigned_32'Mod (Total * Unsigned_64 (Step));
            Info (2) := Unsigned_32'Mod
               (Shift_Right (Total, Period_Shift) - C.Reported);
            Info (3) := Shift_Right
               (Unsigned_32'Mod (Played and Ring_Mask), Frame_Shift) * Step;
            C.Reported := Shift_Right (Total, Period_Shift);
         when SNDCTL_DSP_GETPLAYVOL =>
            Value := C.Volume_Left or Shift_Left (C.Volume_Right, 8);
         when SNDCTL_DSP_SETPLAYVOL =>
            Set_Volume (C, Value);
            Value := C.Volume_Left or Shift_Left (C.Volume_Right, 8);
         when others =>
            Success := False;
      end case;
   end DSP_Request;

   procedure Mixer_Request
      (C        : Controller_Ref;
       Request  : Unsigned_64;
       Argument : System.Address;
       Success  : out Boolean)
   is
      Value : Unsigned_32 with Import, Address => Argument;
   begin
      --  The volume of the device is the only control, as the main volume
      --  and the PCM one alike.
      Success := True;
      case Request is
         when SNDCTL_MIX_READ_VOLUME | SNDCTL_MIX_READ_PCM =>
            Value := C.Volume_Left or Shift_Left (C.Volume_Right, 8);
         when SNDCTL_MIX_WRITE_VOLUME | SNDCTL_MIX_WRITE_PCM =>
            Set_Volume (C, Value);
            Value := C.Volume_Left or Shift_Left (C.Volume_Right, 8);
         when SNDCTL_MIX_READ_DEVMASK | SNDCTL_MIX_READ_STEREODEVS =>
            Value := SOUND_MASK_VOLUME or SOUND_MASK_PCM;
         when SNDCTL_MIX_READ_RECMASK | SNDCTL_MIX_READ_RECSRC |
              SNDCTL_MIX_READ_CAPS    | SNDCTL_MIX_WRITE_RECSRC =>
            Value := 0;
         when others =>
            Success := False;
      end case;
   end Mixer_Request;

   function Nearest_Rate
      (C  : Controller_Ref;
       Hz : Unsigned_32) return Unsigned_32
   is
      Best     : Unsigned_32 := Rate_Table (Rate_48K).Hz;
      Distance : Unsigned_32 := Unsigned_32'Last;
      Current  : Unsigned_32;
   begin
      for I in Rate_Table'Range loop
         if (C.Rates and Shift_Left (1, I)) /= 0 then
            Current := (if Rate_Table (I).Hz > Hz
                        then Rate_Table (I).Hz - Hz
                        else Hz - Rate_Table (I).Hz);
            if Current < Distance then
               Best     := Rate_Table (I).Hz;
               Distance := Current;
            end if;
         end if;
      end loop;
      return Best;
   end Nearest_Rate;

   function Stream_Format (Hz : Unsigned_32) return Unsigned_32 is
   begin
      for R of Rate_Table loop
         if R.Hz = Hz then
            return R.Format or Format_16_Bit_Stereo;
         end if;
      end loop;
      return Rate_Table (Rate_48K).Format or Format_16_Bit_Stereo;
   end Stream_Format;

   function Sample_Size (Format : Unsigned_32) return Unsigned_32 is
   begin
      case Format is
         when AFMT_U8 | AFMT_S8         => return 1;
         when AFMT_S24_LE | AFMT_S32_LE => return 4;
         when others                    => return 2;
      end case;
   end Sample_Size;

   function Sample
      (Format : Unsigned_32;
       Data   : Operation_Data;
       Pos    : Natural) return Unsigned_16
   is
   begin
      --  The 16 most significant bits of the sample, 24 bit samples being in
      --  the low bytes of 32 bit containers.
      case Format is
         when AFMT_U8 =>
            return Shift_Left (Unsigned_16 (Data (Pos) xor 16#80#), 8);
         when AFMT_S8 =>
            return Shift_Left (Unsigned_16 (Data (Pos)), 8);
         when AFMT_S24_LE =>
            return Unsigned_16 (Data (Pos + 1)) or
                   Shift_Left (Unsigned_16 (Data (Pos + 2)), 8);
         when AFMT_S32_LE =>
            return Unsigned_16 (Data (Pos + 2)) or
                   Shift_Left (Unsigned_16 (Data (Pos + 3)), 8);
         when others =>
            return Unsigned_16 (Data (Pos)) or
                   Shift_Left (Unsigned_16 (Data (Pos + 1)), 8);
      end case;
   exception
      when Constraint_Error =>
         return 0;
   end Sample;
   ----------------------------------------------------------------------------
   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset);
   begin
      Write_Frames
         (Controller_Acc (C1.To_Pointer (Key)), Data, Ret_Count, Is_Blocking);
      Success := Dev_Success;
   exception
      when Constraint_Error =>
         Ret_Count := 0;
         Success   := Dev_IO_Failure;
   end Write;

   procedure Poll
      (Key       : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean)
   is
   begin
      Can_Read  := False;
      Is_Error  := False;
      Can_Write := Can_Queue (Controller_Acc (C1.To_Pointer (Key)));
   exception
      when Constraint_Error =>
         Can_Write := False;
         Is_Error  := True;
   end Poll;

   procedure DSP_IO_Argument
      (Key     : System.Address;
       Request : Unsigned_64;
       Usage   : out IO_Usage;
       Size    : out Natural)
   is
      pragma Unreferenced (Key);
   begin
      Devices.Mixer.Common_OSS_IO_Argument (Request, Usage, Size);
      if Usage /= IO_Unknown then
         return;
      end if;

      Size := 4;
      case Request is
         when SNDCTL_DSP_HALT | SNDCTL_DSP_HALT_OUTPUT | SNDCTL_DSP_SYNC |
              SNDCTL_DSP_POST | SNDCTL_DSP_NONBLOCK    | SNDCTL_DSP_SILENCE |
              SNDCTL_DSP_SKIP =>
            Usage := IO_No_Memory;
            Size  := 0;
         when SNDCTL_DSP_SPEED       | SNDCTL_DSP_STEREO     |
              SNDCTL_DSP_GETBLKSIZE  | SNDCTL_DSP_SETFMT     |
              SNDCTL_DSP_CHANNELS    | SNDCTL_DSP_SUBDIVIDE  |
              SNDCTL_DSP_SETFRAGMENT | SNDCTL_DSP_SETPLAYVOL =>
            Usage := IO_Read_Write;
         when SNDCTL_DSP_GETFMTS    | SNDCTL_DSP_GETCAPS   |
              SNDCTL_DSP_GETTRIGGER | SNDCTL_DSP_GETODELAY |
              SNDCTL_DSP_GETPLAYVOL =>
            Usage := IO_Write;
         when SNDCTL_DSP_SETTRIGGER | SNDCTL_DSP_COOKEDMODE |
              SNDCTL_DSP_PROFILE    | SNDCTL_DSP_LOW_WATER  |
              SNDCTL_DSP_POLICY =>
            Usage := IO_Read;
         when SNDCTL_DSP_GETOSPACE =>
            Usage := IO_Write;
            Size  := 16;
         when SNDCTL_DSP_GETOPTR =>
            Usage := IO_Write;
            Size  := 12;
         when SNDCTL_SETSONG =>
            Usage := IO_Read;
            Size  := 64;
         when others =>
            Usage := IO_Unknown;
            Size  := 0;
      end case;
   end DSP_IO_Argument;

   procedure DSP_IO_Control
      (Key      : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address;
       Extra    : out Unsigned_64;
       Success  : out Boolean)
   is
   begin
      Devices.Mixer.Common_OSS_IO_Control (Request, Argument, Extra, Success);
      if not Success then
         Extra := 0;
         DSP_Request
            (Controller_Acc (C1.To_Pointer (Key)), Request, Argument, Success);
      end if;
   exception
      when Constraint_Error =>
         Extra   := 0;
         Success := False;
   end DSP_IO_Control;

   procedure Mixer_IO_Argument
      (Key     : System.Address;
       Request : Unsigned_64;
       Usage   : out IO_Usage;
       Size    : out Natural)
   is
      pragma Unreferenced (Key);
   begin
      Devices.Mixer.Common_OSS_IO_Argument (Request, Usage, Size);
      if Usage /= IO_Unknown then
         return;
      end if;

      Size := 4;
      case Request is
         when SNDCTL_MIX_READ_VOLUME  | SNDCTL_MIX_READ_PCM        |
              SNDCTL_MIX_READ_DEVMASK | SNDCTL_MIX_READ_STEREODEVS |
              SNDCTL_MIX_READ_RECMASK | SNDCTL_MIX_READ_RECSRC     |
              SNDCTL_MIX_READ_CAPS =>
            Usage := IO_Write;
         when SNDCTL_MIX_WRITE_VOLUME | SNDCTL_MIX_WRITE_PCM |
              SNDCTL_MIX_WRITE_RECSRC =>
            Usage := IO_Read_Write;
         when others =>
            Usage := IO_Unknown;
            Size  := 0;
      end case;
   end Mixer_IO_Argument;

   procedure Mixer_IO_Control
      (Key      : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address;
       Extra    : out Unsigned_64;
       Success  : out Boolean)
   is
   begin
      Devices.Mixer.Common_OSS_IO_Control (Request, Argument, Extra, Success);
      if not Success then
         Extra := 0;
         Mixer_Request
            (Controller_Acc (C1.To_Pointer (Key)), Request, Argument, Success);
      end if;
   exception
      when Constraint_Error =>
         Extra   := 0;
         Success := False;
   end Mixer_IO_Control;
   ----------------------------------------------------------------------------
   function Read_8 (C : Controller_Ref; Reg : Unsigned_32) return Unsigned_8
   is
      Value : Unsigned_8
         with Import, Atomic, Address =>
            To_Address (C.MMIO + Integer_Address (Reg));
   begin
      return Value;
   end Read_8;

   function Read_16 (C : Controller_Ref; Reg : Unsigned_32) return Unsigned_16
   is
      Value : Unsigned_16
         with Import, Atomic, Address =>
            To_Address (C.MMIO + Integer_Address (Reg));
   begin
      return Value;
   end Read_16;

   function Read_32 (C : Controller_Ref; Reg : Unsigned_32) return Unsigned_32
   is
      Value : Unsigned_32
         with Import, Atomic, Address =>
            To_Address (C.MMIO + Integer_Address (Reg));
   begin
      return Value;
   end Read_32;

   procedure Write_8 (C : Controller_Ref; Reg : Unsigned_32; V : Unsigned_8)
   is
      Value : Unsigned_8
         with Import, Atomic, Address =>
            To_Address (C.MMIO + Integer_Address (Reg));
   begin
      Value := V;
   end Write_8;

   procedure Write_16
      (C   : Controller_Ref;
       Reg : Unsigned_32;
       V   : Unsigned_16)
   is
      Value : Unsigned_16
         with Import, Atomic, Address =>
            To_Address (C.MMIO + Integer_Address (Reg));
   begin
      Value := V;
   end Write_16;

   procedure Write_32
      (C   : Controller_Ref;
       Reg : Unsigned_32;
       V   : Unsigned_32)
   is
      Value : Unsigned_32
         with Import, Atomic, Address =>
            To_Address (C.MMIO + Integer_Address (Reg));
   begin
      Value := V;
   end Write_32;

   function Load_32 (Addr : Integer_Address) return Unsigned_32 is
      Value : Unsigned_32 with Import, Atomic, Address => To_Address (Addr);
   begin
      return Value;
   end Load_32;

   procedure Store_32 (Addr : Integer_Address; V : Unsigned_32) is
      Value : Unsigned_32 with Import, Atomic, Address => To_Address (Addr);
   begin
      Value := V;
   end Store_32;

   procedure Barrier is
   begin
      System.Machine_Code.Asm
         ("mfence", Clobber => "memory", Volatile => True);
   end Barrier;

   procedure Wait_8
      (C       : Controller_Ref;
       Reg     : Unsigned_32;
       Mask    : Unsigned_8;
       Value   : Unsigned_8;
       Success : out Boolean)
   is
      Limit : constant Time.Timestamp := Deadline (Register_Timeout);
   begin
      loop
         if (Read_8 (C, Reg) and Mask) = Value then
            Success := True;
            return;
         elsif Expired (Limit) then
            Success := False;
            return;
         end if;
         Arch.Snippets.Pause;
      end loop;
   end Wait_8;

   procedure Wait_16
      (C       : Controller_Ref;
       Reg     : Unsigned_32;
       Mask    : Unsigned_16;
       Value   : Unsigned_16;
       Micros  : Unsigned_64;
       Success : out Boolean)
   is
      Limit : constant Time.Timestamp := Deadline (Micros);
   begin
      loop
         if (Read_16 (C, Reg) and Mask) = Value then
            Success := True;
            return;
         elsif Expired (Limit) then
            Success := False;
            return;
         end if;
         Arch.Snippets.Pause;
      end loop;
   end Wait_16;

   function Deadline (Micros : Unsigned_64) return Time.Timestamp is
      Now : Time.Timestamp;
   begin
      Arch.Clocks.Get_Monotonic_Time (Now);
      return Time."+" (Now, Time.To_Stamp (Micros * 1_000));
   end Deadline;

   function Expired (Limit : Time.Timestamp) return Boolean is
      Now : Time.Timestamp;
   begin
      Arch.Clocks.Get_Monotonic_Time (Now);
      return Time.">=" (Now, Limit);
   end Expired;
end Devices.PCI.HDA;
