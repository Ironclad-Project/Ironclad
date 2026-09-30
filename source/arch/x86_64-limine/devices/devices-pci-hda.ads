--  devices-pci-hda.ads: Intel HDA driver.
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

with Synchronization;
with Time;

package Devices.PCI.HDA with SPARK_Mode => Off is
   procedure Init (Success : out Boolean);

private

   --  Controller registers, as offsets into the memory BAR.
   Reg_GCAP      : constant := 16#00#;
   Reg_GCTL      : constant := 16#08#;
   Reg_WAKEEN    : constant := 16#0C#;
   Reg_STATESTS  : constant := 16#0E#;
   Reg_INTCTL    : constant := 16#20#;
   Reg_INTSTS    : constant := 16#24#;
   Reg_CORBLBASE : constant := 16#40#;
   Reg_CORBUBASE : constant := 16#44#;
   Reg_CORBWP    : constant := 16#48#;
   Reg_CORBRP    : constant := 16#4A#;
   Reg_CORBCTL   : constant := 16#4C#;
   Reg_CORBSIZE  : constant := 16#4E#;
   Reg_RIRBLBASE : constant := 16#50#;
   Reg_RIRBUBASE : constant := 16#54#;
   Reg_RIRBWP    : constant := 16#58#;
   Reg_RINTCNT   : constant := 16#5A#;
   Reg_RIRBCTL   : constant := 16#5C#;
   Reg_RIRBSTS   : constant := 16#5D#;
   Reg_RIRBSIZE  : constant := 16#5E#;

   --  Stream descriptors start at 80h, 20h bytes each, input ones first,
   --  then output ones, then bidirectional ones.
   --  These are offsets into a descriptor.
   Stream_Descriptors : constant := 16#80#;
   Stream_Stride      : constant := 16#20#;
   SD_CTL             : constant := 16#00#;
   SD_CTL_Stream      : constant := 16#02#;
   SD_STS             : constant := 16#03#;
   SD_LPIB            : constant := 16#04#;
   SD_CBL             : constant := 16#08#;
   SD_LVI             : constant := 16#0C#;
   SD_FMT             : constant := 16#12#;
   SD_BDPL            : constant := 16#18#;
   SD_BDPU            : constant := 16#1C#;

   --  Register bits.
   GCTL_CRST       : constant := 16#01#;        --  3.3.7
   CORBRP_Reset    : constant := 16#8000#;      --  3.3.21
   CORBCTL_Run     : constant := 16#02#;        --  3.3.22
   RIRBWP_Reset    : constant := 16#8000#;      --  3.3.27
   RIRBCTL_RINTCTL : constant := 16#01#;        --  3.3.29
   RIRBCTL_DMA     : constant := 16#02#;
   RIRBSTS_Clear   : constant := 16#05#;        --  3.3.30
   RIRB_Unsolicited : constant := 16#10#;       --  3.6.5
   INTCTL_GIE      : constant := 16#8000_0000#; --  3.3.14
   SD_CTL_SRST     : constant := 16#01#;        --  3.3.35
   SD_CTL_RUN      : constant := 16#02#;
   SD_CTL_IOCE     : constant := 16#04#;
   SD_CTL_DEIE     : constant := 16#10#;
   SD_STS_BCIS     : constant := 16#04#;        --  3.3.36
   SD_STS_FIFOE    : constant := 16#08#;
   SD_STS_DESE     : constant := 16#10#;
   SD_STS_All      : constant := SD_STS_BCIS + SD_STS_FIFOE + SD_STS_DESE;

   --  Verbs, with 12 bit identifiers and 8 bit payloads, or 4 bit ones and
   --  16 bit payloads.
   Verb_Get_Parameter  : constant := 16#F00#;   --  7.3.3.1
   Verb_Set_Connection : constant := 16#701#;   --  7.3.3.2
   Verb_Get_Connection : constant := 16#F02#;   --  7.3.3.3
   Verb_Set_Amplifier  : constant := 16#3#;     --  7.3.3.7
   Verb_Set_Format     : constant := 16#2#;     --  7.3.3.8
   Verb_Get_Power      : constant := 16#F05#;   --  7.3.3.10
   Verb_Set_Power      : constant := 16#705#;
   Verb_Set_Stream     : constant := 16#706#;   --  7.3.3.11
   Verb_Set_Pin        : constant := 16#707#;   --  7.3.3.13
   Verb_Get_EAPD       : constant := 16#F0C#;   --  7.3.3.16
   Verb_Set_EAPD       : constant := 16#70C#;
   Verb_Get_Config     : constant := 16#F1C#;   --  7.3.3.31

   --  Parameters, read with Verb_Get_Parameter.
   Param_Vendor_ID    : constant := 16#00#;     --  7.3.4.1
   Param_Node_Count   : constant := 16#04#;     --  7.3.4.3
   Param_Group_Type   : constant := 16#05#;     --  7.3.4.4
   Param_Widget_Caps  : constant := 16#09#;     --  7.3.4.6
   Param_PCM          : constant := 16#0A#;     --  7.3.4.7
   Param_Pin_Caps     : constant := 16#0C#;     --  7.3.4.9
   Param_In_Amp_Caps  : constant := 16#0D#;     --  7.3.4.10
   Param_Conn_Length  : constant := 16#0E#;     --  7.3.4.11
   Param_Out_Amp_Caps : constant := 16#12#;     --  7.3.4.10

   --  Audio Widget Capabilities bits and widget types.
   Caps_In_Amp      : constant := 16#002#;
   Caps_Out_Amp     : constant := 16#004#;
   Caps_Amp_Params  : constant := 16#008#;
   Caps_Format      : constant := 16#010#;
   Caps_Conn_List   : constant := 16#100#;
   Caps_Digital     : constant := 16#200#;
   Caps_Power       : constant := 16#400#;
   Widget_Output    : constant := 16#0#;
   Widget_Mixer     : constant := 16#2#;
   Widget_Selector  : constant := 16#3#;
   Widget_Pin       : constant := 16#4#;

   --  Pin Capabilities bits.
   Pin_Headphone : constant := 16#0000_0008#;
   Pin_Output    : constant := 16#0000_0010#;
   Pin_HDMI      : constant := 16#0000_0080#;
   Pin_EAPD      : constant := 16#0001_0000#;
   Pin_DP        : constant := 16#0100_0000#;

   --  Pin Widget Control bits.
   Pin_Control_HP  : constant := 16#80#;
   Pin_Control_Out : constant := 16#40#;

   --  Supported PCM Size, Rates bit for 16 bit samples.
   PCM_B16 : constant := 16#0002_0000#;

   --  Stream format bits for 16 bit, 2 channel samples.
   Format_16_Bit_Stereo : constant := 16#0011#;

   --  Stream number the output stream uses, as 0 means unused.
   Stream_Tag : constant := 1;

   --  The cyclic buffer, split in periods, each one a Buffer Descriptor List
   --  entry with Interrupt on Completion.
   Period_Size  : constant := 4096;
   Period_Count : constant := 16;
   Ring_Size    : constant := Period_Size * Period_Count;

   --  Past the last byte written, the buffer holds Silence_Periods periods of
   --  silence, and the stream is stopped once Tail_Periods of them have been
   --  fetched, so that what came before is out of any FIFO and nothing stale
   --  is ever fetched. The rest is what may be queued at once.
   Silence_Periods : constant := 4;
   Tail_Periods    : constant := 3;
   Max_Pending     : constant :=
      Ring_Size - (Silence_Periods + 1) * Period_Size;

   --  Every sample the hardware plays is 16 bit and stereo, so a frame is 4
   --  bytes, and a period is 1024 frames. These powers of two are shifted by
   --  and masked with rather than divided by, which cannot fail.
   Frame_Size    : constant := 4;
   Frame_Shift   : constant := 2;
   Period_Frames : constant := Period_Size / Frame_Size;
   Period_Shift  : constant := 10;
   Ring_Mask     : constant := Ring_Size - 1;

   --  How long register handshakes and codec responses may take, in
   --  microseconds. Codecs respond on the frame after a verb.
   Register_Timeout : constant := 100_000;
   Command_Timeout  : constant := 10_000;

   --  Layout of the page holding the CORB, the RIRB and the BDL, each on a
   --  128 byte boundary.
   RIRB_Offset : constant := 1024;
   BDL_Offset  : constant := 3072;

   --  Sample rates the stream format can express, in the order of the rate
   --  bits R1 to R11 of the Supported PCM Size, Rates parameter, with their
   --  BASE, MULT and DIV fields.
   type Rate_Info is record
      Hz     : Unsigned_32;
      Format : Unsigned_32;
   end record;
   type Rate_Info_Arr is array (0 .. 10) of Rate_Info;
   Rate_Table : constant Rate_Info_Arr :=
      [(8_000,   16#0500#),
       (11_025,  16#4300#),
       (16_000,  16#0200#),
       (22_050,  16#4100#),
       (32_000,  16#0A00#),
       (44_100,  16#4000#),
       (48_000,  16#0000#),
       (88_200,  16#4800#),
       (96_000,  16#0800#),
       (176_400, 16#5800#),
       (192_000, 16#1800#)];
   Rate_48K : constant := 6;

   --  OSS requests and values.
   SOUND_MASK_VOLUME           : constant := 16#01#;
   SOUND_MASK_PCM              : constant := 16#10#;
   AFMT_U8                     : constant := 16#0008#;
   AFMT_S16_LE                 : constant := 16#0010#;
   AFMT_S8                     : constant := 16#0040#;
   AFMT_S32_LE                 : constant := 16#1000#;
   AFMT_S24_LE                 : constant := 16#8000#;
   Supported_Formats           : constant :=
      AFMT_U8 + AFMT_S8 + AFMT_S16_LE + AFMT_S24_LE + AFMT_S32_LE;
   PCM_CAP_TRIGGER             : constant := 16#0000_1000#;
   PCM_CAP_OUTPUT              : constant := 16#0002_0000#;
   PCM_CAP_ANALOGOUT           : constant := 16#0010_0000#;
   PCM_ENABLE_OUTPUT           : constant := 16#02#;
   ----------------------------------------------------------------------------
   --  Codec nodes, with short form, 7 bit.
   subtype Node_ID is Unsigned_32 range 0 .. 127;

   Max_Connections : constant := 32;
   type Connection_Arr is array (1 .. Max_Connections) of Node_ID;

   type Widget is record
      Present    : Boolean;
      Caps       : Unsigned_32;
      Pin_Caps   : Unsigned_32;
      Config     : Unsigned_32;
      PCM        : Unsigned_32;
      In_Amp     : Unsigned_32;
      Out_Amp    : Unsigned_32;
      Conn_Count : Natural range 0 .. Max_Connections;
      Conns      : Connection_Arr;
   end record;
   type Widget_Arr is array (Node_ID) of Widget;

   --  A path from an output pin to the converter that feeds it, each hop
   --  with the index of the input of its node that the path goes through.
   Max_Hops : constant := 6;
   type Hop is record
      Node  : Node_ID;
      Index : Natural;
   end record;
   type Hop_Arr is array (1 .. Max_Hops) of Hop;

   type Route is record
      Hops   : Hop_Arr;
      Length : Natural range 0 .. Max_Hops;
      DAC    : Node_ID;
   end record;
   Max_Routes : constant := 8;
   type Route_Arr is array (1 .. Max_Routes) of Route;

   --  Idle:    Nothing queued, the stream is stopped.
   --  Primed:  Data is queued from the start of the buffer, not yet played.
   --  Running: The stream is running.
   type Stream_State is (Idle, Primed, Running);

   type Ring_Index is mod Ring_Size;
   type Ring_Buffer is array (Ring_Index) of Unsigned_8;
   type Ring_Buffer_Acc is access Ring_Buffer;
   type DMA_Page is array (Unsigned_32 range 0 .. 4095) of Unsigned_8;
   type DMA_Page_Acc is access DMA_Page;
   type Partial_Frame is array (Unsigned_32 range 1 .. 8) of Unsigned_8;

   type Controller is record
      PCI_Dev : Devices.PCI.PCI_Device;
      MMIO    : Integer_Address := 0;
      Vector  : Integer         := 0;
      Uses_64 : Boolean         := False;

      --  Command transport (HDA rev 1.0a, 4.4). The CORB and RIRB share a
      --  page, which the BDL shares too, and the masks wrap their pointers.
      Cmd_Mutex : aliased Synchronization.Mutex :=
         Synchronization.Unlocked_Mutex;
      DMA_Base  : Integer_Address := 0;
      CORB_Mask : Unsigned_32     := 0;
      RIRB_Mask : Unsigned_32     := 0;
      RIRB_Read : Unsigned_32     := 0;

      --  Codec in use and its output routes.
      Codec       : Unsigned_32 := 0;
      Widgets     : Widget_Arr;
      Routes      : Route_Arr;
      Route_Count : Natural range 0 .. Max_Routes := 0;
      Rates       : Unsigned_32 := 0;

      --  Output stream, and its position as byte counts since it started.
      SD           : Unsigned_32     := 0;
      SIE_Bit      : Natural range 0 .. 15 := 0;
      Ring_Base    : Integer_Address := 0;
      Dev_Mutex    : aliased Synchronization.Mutex :=
         Synchronization.Unlocked_Mutex;
      Lock         : aliased Synchronization.Binary_Semaphore :=
         Synchronization.Unlocked_Semaphore;
      State        : Stream_State := Idle;
      Trigger      : Boolean      := True;
      Write_Total  : Unsigned_64  := 0;
      Play_Total   : Unsigned_64  := 0;
      Zero_Total   : Unsigned_64  := 0;
      Last_LPIB    : Unsigned_32  := 0;
      Played_Base  : Unsigned_64  := 0;  --  Bytes played before the start.
      Reported     : Unsigned_64  := 0;
      Codec_Format : Unsigned_32  := 16#FFFF_FFFF#;

      --  Format of the data written to the device, converted to the one of
      --  the stream as it is queued, and bytes of a frame not written whole.
      App_Format   : Unsigned_32 := 0;
      App_Channels : Unsigned_32 := 1;
      Rate         : Unsigned_32 := 0;
      Partial      : Partial_Frame := [others => 0];
      Partial_Len  : Unsigned_32 range 0 .. 8 := 0;

      Volume_Left  : Unsigned_32 := 100;
      Volume_Right : Unsigned_32 := 100;
   end record;
   type Controller_Acc is access all Controller;
   subtype Controller_Ref is not null Controller_Acc;

   procedure Init_Controller
      (PCI_Dev : Devices.PCI.PCI_Device;
       Success : out Boolean);

   procedure Reset_Controller (C : Controller_Ref; Success : out Boolean);
   procedure Allocate_Buffers (C : Controller_Ref; Success : out Boolean);

   --  Start the CORB and RIRB.
   procedure Init_Commands (C : Controller_Ref; Success : out Boolean);

   --  Set the CORB or RIRB with its size register at Reg to its largest
   --  size, and return the mask its pointers wrap with.
   procedure Set_Ring_Size
      (C    : Controller_Ref;
       Reg  : Unsigned_32;
       Mask : out Unsigned_32);
   ----------------------------------------------------------------------------
   procedure Command
      (C        : Controller_Ref;
       Node     : Node_ID;
       Verb     : Unsigned_32;
       Response : out Unsigned_32;
       Success  : out Boolean);

   procedure Send (C : Controller_Ref; Node : Node_ID; Verb : Unsigned_32);

   procedure Get_Parameter
      (C       : Controller_Ref;
       Node    : Node_ID;
       Param   : Unsigned_32;
       Value   : out Unsigned_32;
       Success : out Boolean);

   procedure Power_Up (C : Controller_Ref; Node : Node_ID);

   function Short_Verb (ID, Payload : Unsigned_32) return Unsigned_32 is
      (Shift_Left (ID, 8) or (Payload and 16#FF#));
   function Long_Verb (ID, Payload : Unsigned_32) return Unsigned_32 is
      (Shift_Left (ID, 16) or (Payload and 16#FFFF#));

   function Amp_Payload
      (Output : Boolean;
       Left   : Boolean;
       Right  : Boolean;
       Index  : Unsigned_32;
       Mute   : Boolean;
       Gain   : Unsigned_32) return Unsigned_32
   is ((if Output then 16#8000# else 16#4000#) or
       (if Left   then 16#2000# else 0)        or
       (if Right  then 16#1000# else 0)        or
       Shift_Left (Index and 16#F#, 8)         or
       (if Mute   then 16#80#   else 0)        or
       (Gain and 16#7F#));
   ----------------------------------------------------------------------------
   procedure Find_Codec (C : Controller_Ref; Success : out Boolean);
   procedure Probe_Codec
      (C       : Controller_Ref;
       CAd     : Unsigned_32;
       Success : out Boolean);
   procedure Probe_Group
      (C       : Controller_Ref;
       Group   : Node_ID;
       Success : out Boolean);
   procedure Probe_Widget
      (C       : Controller_Ref;
       Node    : Node_ID;
       PCM     : Unsigned_32;
       In_Amp  : Unsigned_32;
       Out_Amp : Unsigned_32);

   procedure Read_Connections (C : Controller_Ref; Node : Node_ID);
   procedure Add_Connection (W : in out Widget; Node : Unsigned_32);

   function Widget_Type (W : Widget) return Unsigned_32 is
      (Shift_Right (W.Caps, 20) and 16#F#);
   function Is_Output_Pin (W : Widget) return Boolean is
      (W.Present
       and then Widget_Type (W) = Widget_Pin
       and then (W.Pin_Caps and Pin_Output) /= 0
       and then (W.Pin_Caps and (Pin_HDMI or Pin_DP)) = 0
       and then (W.Caps and Caps_Digital) = 0
       and then Shift_Right (W.Config, 30) /= 2#01#
       and then (Shift_Right (W.Config, 20) and 16#F#) <= 2);
   procedure Find_Routes (C : Controller_Ref);
   procedure Find_Path
      (C     : Controller_Ref;
       Pin   : Node_ID;
       R     : out Route;
       Found : out Boolean);
   procedure Configure_Routes (C : Controller_Ref);
   procedure Apply_Volume (C : Controller_Ref);
   procedure Set_Volume (C : Controller_Ref; Value : Unsigned_32);
   ----------------------------------------------------------------------------
   procedure Setup_Interrupt (C : Controller_Ref; Success : out Boolean);
   procedure Service_Interrupt (C : Controller_Ref);
   procedure Update_Position (C : Controller_Ref);
   procedure Check_Drain (C : Controller_Ref);
   procedure Stop_DMA (C : Controller_Ref);
   procedure Reset_Positions (C : Controller_Ref);

   procedure Queue_Frames
      (C      : Controller_Ref;
       Data   : Operation_Data;
       First  : Natural;
       Frames : Unsigned_32);

   procedure Extend_Silence (C : Controller_Ref);

   function Pending_Bytes (C : Controller_Ref) return Unsigned_64 is
      (if C.Write_Total > C.Play_Total
       then C.Write_Total - C.Play_Total
       else 0);
   function Free_Frames (C : Controller_Ref) return Unsigned_32 is
      (if Pending_Bytes (C) >= Max_Pending
       then 0
       else Unsigned_32'Mod
          (Shift_Right (Max_Pending - Pending_Bytes (C), Frame_Shift)));

   procedure Write_Frames
      (C           : Controller_Ref;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Is_Blocking : Boolean);

   procedure Start_Stream (C : Controller_Ref);
   procedure Halt (C : Controller_Ref);
   procedure Drain (C : Controller_Ref);
   procedure Wait_For_Space (C : Controller_Ref);
   ----------------------------------------------------------------------------
   function Can_Queue (C : Controller_Ref) return Boolean;

   procedure DSP_Request
      (C        : Controller_Ref;
       Request  : Unsigned_64;
       Argument : System.Address;
       Success  : out Boolean);
   procedure Mixer_Request
      (C        : Controller_Ref;
       Request  : Unsigned_64;
       Argument : System.Address;
       Success  : out Boolean);

   function Nearest_Rate
      (C  : Controller_Ref;
       Hz : Unsigned_32) return Unsigned_32;

   function Stream_Format (Hz : Unsigned_32) return Unsigned_32;

   function Sample_Size (Format : Unsigned_32) return Unsigned_32;
   function Sample
      (Format : Unsigned_32;
       Data   : Operation_Data;
       Pos    : Natural) return Unsigned_16;
   ----------------------------------------------------------------------------
   function Read_8 (C : Controller_Ref; Reg : Unsigned_32) return Unsigned_8;
   function Read_16 (C : Controller_Ref; Reg : Unsigned_32) return Unsigned_16;
   function Read_32 (C : Controller_Ref; Reg : Unsigned_32) return Unsigned_32;
   procedure Write_8 (C : Controller_Ref; Reg : Unsigned_32; V : Unsigned_8);
   procedure Write_16
      (C   : Controller_Ref;
       Reg : Unsigned_32;
       V   : Unsigned_16);
   procedure Write_32
      (C   : Controller_Ref;
       Reg : Unsigned_32;
       V   : Unsigned_32);
   function Load_32 (Addr : Integer_Address) return Unsigned_32;
   procedure Store_32 (Addr : Integer_Address; V : Unsigned_32);
   procedure Barrier;

   function Physical (Addr : Integer_Address) return Unsigned_64 is
      (Unsigned_64 (Addr - Memory.Memory_Offset));
   function Low (Value : Unsigned_64) return Unsigned_32 is
      (Unsigned_32'Mod (Value));
   function High (Value : Unsigned_64) return Unsigned_32 is
      (Unsigned_32'Mod (Shift_Right (Value, 32)));

   procedure Wait_8
      (C       : Controller_Ref;
       Reg     : Unsigned_32;
       Mask    : Unsigned_8;
       Value   : Unsigned_8;
       Success : out Boolean);
   procedure Wait_16
      (C       : Controller_Ref;
       Reg     : Unsigned_32;
       Mask    : Unsigned_16;
       Value   : Unsigned_16;
       Micros  : Unsigned_64;
       Success : out Boolean);

   function Deadline (Micros : Unsigned_64) return Time.Timestamp;
   function Expired (Limit : Time.Timestamp) return Boolean;
   ----------------------------------------------------------------------------
   procedure Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean);

   procedure Poll
      (Key       : System.Address;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Is_Error  : out Boolean);

   procedure DSP_IO_Argument
      (Key     : System.Address;
       Request : Unsigned_64;
       Usage   : out IO_Usage;
       Size    : out Natural);

   procedure DSP_IO_Control
      (Key      : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address;
       Extra    : out Unsigned_64;
       Success  : out Boolean);

   procedure Mixer_IO_Argument
      (Key     : System.Address;
       Request : Unsigned_64;
       Usage   : out IO_Usage;
       Size    : out Natural);

   procedure Mixer_IO_Control
      (Key      : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address;
       Extra    : out Unsigned_64;
       Success  : out Boolean);

   procedure Interrupt_Handler (Vector : Integer) with Convention => C;
end Devices.PCI.HDA;
