--  sound.ads: Driver for OSS.
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

package Sound.OSS_IOCTL is
   --  Generic device IOCTLs.
   OSS_GETVERSION      : constant := 16#80044D76#;
   SNDCTL_SYSINFO      : constant := 16#84E05801#;
   SNDCTL_MIXERINFO    : constant := 16#C470580A#;
   SNDCTL_CARDINFO     : constant := 16#C498580B#;
   SNDCTL_AUDIOINFO    : constant := 16#C49C5807#;
   SNDCTL_AUDIOINFO_EX : constant := 16#C49C580D#;
   SNDCTL_ENGINEINFO   : constant := 16#C49C580C#;

   --  DSP IOCTLs.
   SNDCTL_DSP_BIND_CHANNEL      : constant := 16#C0045041#;
   SNDCTL_DSP_CHANNELS          : constant := 16#C0045006#;
   SNDCTL_DSP_COOKEDMODE        : constant := 16#4004501E#;
   SNDCTL_DSP_CURRENT_IPTR      : constant := 16#80905023#;
   SNDCTL_DSP_CURRENT_OPTR      : constant := 16#80905024#;
   SNDCTL_DSP_GETBLKSIZE        : constant := 16#C0045004#;
   SNDCTL_DSP_GETCAPS           : constant := 16#8004500F#;
   SNDCTL_DSP_GETCHANNELMASK    : constant := 16#C0045040#;
   SNDCTL_DSP_GET_CHNORDER      : constant := 16#8008502A#;
   SNDCTL_DSP_GETERROR          : constant := 16#80685019#;
   SNDCTL_DSP_GETFMTS           : constant := 16#8004500B#;
   SNDCTL_DSP_GETIPEAKS         : constant := 16#8100502B#;
   SNDCTL_DSP_GETIPTR           : constant := 16#800C5011#;
   SNDCTL_DSP_GETISPACE         : constant := 16#8010500D#;
   SNDCTL_DSP_GETODELAY         : constant := 16#80045017#;
   SNDCTL_DSP_GETOPEAKS         : constant := 16#8100502C#;
   SNDCTL_DSP_GETOPTR           : constant := 16#800C5012#;
   SNDCTL_DSP_GETOSPACE         : constant := 16#8010500C#;
   SNDCTL_DSP_GET_PLAYTGT_NAMES : constant := 16#8DC85027#;
   SNDCTL_DSP_GETPLAYVOL        : constant := 16#80045018#;
   SNDCTL_DSP_GET_RECSRC_NAMES  : constant := 16#8DC85025#;
   SNDCTL_DSP_GET_RECSRC        : constant := 16#80045026#;
   SNDCTL_DSP_GETRECVOL         : constant := 16#80045029#;
   SNDCTL_DSP_GETTRIGGER        : constant := 16#80045010#;
   SNDCTL_DSP_HALT_INPUT        : constant := 16#5021#;
   SNDCTL_DSP_HALT_OUTPUT       : constant := 16#5022#;
   SNDCTL_DSP_HALT              : constant := 16#5000#;
   SNDCTL_DSP_LOW_WATER         : constant := 16#40045022#;
   SNDCTL_DSP_NONBLOCK          : constant := 16#500E#;
   SNDCTL_DSP_POLICY            : constant := 16#4004502D#;
   SNDCTL_DSP_POST              : constant := 16#5008#;
   SNDCTL_DSP_READCTL           : constant := 16#C11C501A#;
   SNDCTL_DSP_SETDUPLEX         : constant := 16#5016#;
   SNDCTL_DSP_SETFMT            : constant := 16#C0045005#;
   SNDCTL_DSP_SETFRAGMENT       : constant := 16#C004500A#;
   SNDCTL_DSP_SET_PLAYTGT       : constant := 16#C0045028#;
   SNDCTL_DSP_SETPLAYVOL        : constant := 16#C0045018#;
   SNDCTL_DSP_SET_RECSRC        : constant := 16#C0045026#;
   SNDCTL_DSP_SETRECVOL         : constant := 16#C0045029#;
   SNDCTL_DSP_SETSYNCRO         : constant := 16#5015#;
   SNDCTL_DSP_SETTRIGGER        : constant := 16#40045010#;
   SNDCTL_DSP_SILENCE           : constant := 16#501F#;
   SNDCTL_DSP_SKIP              : constant := 16#5020#;
   SNDCTL_DSP_SPEED             : constant := 16#C0045002#;
   SNDCTL_DSP_SUBDIVIDE         : constant := 16#C0045009#;
   SNDCTL_DSP_SYNCGROUP         : constant := 16#C048501C#;
   SNDCTL_DSP_SYNC              : constant := 16#5001#;
   SNDCTL_DSP_SYNCSTART         : constant := 16#4004501D#;
   SNDCTL_DSP_WRITECTL          : constant := 16#C11C501B#;
   SNDCTL_DSP_PROFILE           : constant := 16#40045017#;
   SNDCTL_SETSONG               : constant := 16#40405902#;
   SNDCTL_DSP_STEREO            : constant := 16#C0045003#;

   --  MIDI IOCTLs.
   SNDCTL_MIDI_INFO     : constant := 16#C074510C#;
   SNDCTL_MIDI_MTCINPUT : constant := 16#C0046D03#;
   SNDCTL_MIDI_PRETIME  : constant := 16#C0046D00#;
   SNDCTL_MIDI_SETMODE  : constant := 16#C0046D06#;

   --  Mixer IOCTLs.
   SNDCTL_MIX_READ_VOLUME     : constant := 16#80044D00#;
   SNDCTL_MIX_READ_PCM        : constant := 16#80044D04#;
   SNDCTL_MIX_READ_STEREODEVS : constant := 16#80044DFB#;
   SNDCTL_MIX_READ_CAPS       : constant := 16#80044DFC#;
   SNDCTL_MIX_READ_RECMASK    : constant := 16#80044DFD#;
   SNDCTL_MIX_READ_DEVMASK    : constant := 16#80044DFE#;
   SNDCTL_MIX_READ_RECSRC     : constant := 16#80044DFF#;
   SNDCTL_MIX_WRITE_VOLUME    : constant := 16#C0044D00#;
   SNDCTL_MIX_WRITE_PCM       : constant := 16#C0044D04#;
   SNDCTL_MIX_WRITE_RECSRC    : constant := 16#C0044DFF#;
end Sound.OSS_IOCTL;
