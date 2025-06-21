--  ipc-pty.adb: PTY creation and management.
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

with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Scheduler;
with Devices.TermIOs; use Devices.TermIOs;
with Userland.Process; use Userland.Process;
with Arch.Local;

package body IPC.PTY is
   pragma Suppress (All_Checks); --  Unit passes AoRTE checks.

   procedure Free is new Ada.Unchecked_Deallocation (Inner, Inner_Acc);
   package   Conv is new System.Address_To_Access_Conversions (Inner);

   Tracked_Lock : aliased Synchronization.Mutex :=
      Synchronization.Unlocked_Mutex;
   Tracked_Name : Natural := 1;

   procedure Create (Result : out Inner_Acc) is
      Name_Index : Natural;
      Resource   : Devices.Resource;
      Success    : Boolean;
      Termios_D  : Devices.TermIOs.Main_Data;
   begin
      Synchronization.Seize (Tracked_Lock);
      Name_Index := Tracked_Name;
      Tracked_Name := Tracked_Name + 1;
      Synchronization.Release (Tracked_Lock);

      --  Some sane defaults.
      Termios_D :=
         (Input_Modes   => 0,
          Output_Modes  => OPOST or ONLCR,
          Control_Modes => 0,
          Local_Mode    => (others => False),
          Special_Chars => [others => 0],
          Input_Baud    => 0,
          Output_Baud   => 0);

      Result := new Inner'
         (Primary_Mutex      => Synchronization.Unlocked_Mutex,
          Secondary_Mutex    => Synchronization.Unlocked_Mutex,
          Global_Data_Mutex  => Synchronization.Unlocked_Mutex,
          Primary_Read       => True,
          Primary_Transmit   => True,
          Secondary_Read     => True,
          Secondary_Transmit => True,
          Device_Handle      => Devices.Error_Handle,
          Name_Index         => Name_Index,
          Term_Info          => Termios_D,
          Term_Size          => (others => 0),
          Was_Closed         => False,
          Termios_Changed    => False,
          Primary_Length     => 0,
          Secondary_Length   => 0,
          Primary_Data       => [others => 0],
          Secondary_Data     => [others => 0]);

      Resource :=
         (Data        => Conv.To_Address (Conv.Object_Pointer (Result)),
          ID          => Devices.Zero_UUID,
          Is_Block    => False,
          Block_Size  => 4096,
          Block_Count => 0,
          Read        => Dev_Read'Access,
          Write       => Dev_Write'Access,
          Sync        => null,
          Sync_Range  => null,
          IO_Control  => null,
          Mmap        => null,
          Poll        => null,
          Remove      => null);

      declare
         Final_Name : constant String := "pty" & Name_Index'Image;
      begin
         Devices.Register (Resource, Final_Name, Success);
         if Success then
            Result.Device_Handle := Devices.Fetch (Final_Name);
         else
            Free (Result);
            Result := null;
         end if;
      end;
   end Create;

   procedure Close (Closed : in out Inner_Acc) is
      Discard : Boolean;
   begin
      Synchronization.Seize (Closed.Primary_Mutex);
      Synchronization.Seize (Closed.Secondary_Mutex);
      Synchronization.Seize (Closed.Global_Data_Mutex);
      if Closed.Was_Closed then
         Devices.Remove (Closed.Device_Handle, Discard);
         Free (Closed);
      else
         Closed.Was_Closed := True;
         Synchronization.Release (Closed.Primary_Mutex);
         Synchronization.Release (Closed.Secondary_Mutex);
         Synchronization.Release (Closed.Global_Data_Mutex);
      end if;
   end Close;

   procedure Read_Primary
      (To_Read     : Inner_Acc;
       Data        : out Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Status)
   is
   begin
      Read_From_End
         (To_Read.Primary_Mutex'Access, To_Read.Primary_Length'Access,
          To_Read.Primary_Data'Access, Is_Blocking,
          To_Read.Primary_Read, Data, Ret_Count);
      Success := PTY_Success;
   end Read_Primary;

   procedure Write_Primary
      (To_Write    : Inner_Acc;
       Data        : Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Status)
   is
   begin
      Write_To_End
         (To_Write.Secondary_Mutex'Access, To_Write.Secondary_Length'Access,
          To_Write.Secondary_Data'Access, Is_Blocking,
          To_Write.Primary_Transmit, Data, To_Write.Term_Info, False,
          Ret_Count);
      Success := PTY_Success;
   end Write_Primary;

   procedure Read_Secondary
      (To_Read     : Inner_Acc;
       Data        : out Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Status)
   is
   begin
      Read_From_End
         (To_Read.Secondary_Mutex'Access, To_Read.Secondary_Length'Access,
          To_Read.Secondary_Data'Access, Is_Blocking,
          To_Read.Secondary_Read, Data, Ret_Count);
      Success := PTY_Success;
   end Read_Secondary;

   procedure Write_Secondary
      (To_Write    : Inner_Acc;
       Data        : Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Status)
   is
   begin
      Write_To_End
         (To_Write.Primary_Mutex'Access, To_Write.Primary_Length'Access,
          To_Write.Primary_Data'Access, Is_Blocking,
          To_Write.Secondary_Transmit, Data, To_Write.Term_Info, True,
          Ret_Count);
      Success := PTY_Success;
   end Write_Secondary;

   procedure Poll_Primary
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Can_Prio  : out Boolean)
   is
   begin
      Synchronization.Seize (P.Global_Data_Mutex);
      Can_Prio := P.Termios_Changed;
      P.Termios_Changed := False;
      Synchronization.Release (P.Global_Data_Mutex);

      Synchronization.Seize (P.Primary_Mutex);
      Can_Read  := P.Primary_Length /= 0;
      Can_Write := P.Primary_Length /= P.Primary_Data'Length;
      Synchronization.Release (P.Primary_Mutex);
   end Poll_Primary;

   procedure Poll_Secondary
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean)
   is
   begin
      Synchronization.Seize (P.Secondary_Mutex);
      Can_Read  := P.Secondary_Length /= 0;
      Can_Write := P.Secondary_Length /= P.Secondary_Data'Length;
      Synchronization.Release (P.Secondary_Mutex);
   end Poll_Secondary;

   procedure Get_TermIOs (P : Inner_Acc; T : out Devices.TermIOs.Main_Data) is
   begin
      Synchronization.Seize (P.Global_Data_Mutex);
      T := P.Term_Info;
      Synchronization.Release (P.Global_Data_Mutex);
   end Get_TermIOs;

   procedure Set_TermIOs (P : Inner_Acc; T : Devices.TermIOs.Main_Data) is
   begin
      Synchronization.Seize (P.Global_Data_Mutex);
      P.Term_Info := T;
      P.Termios_Changed := True;
      Synchronization.Release (P.Global_Data_Mutex);
   end Set_TermIOs;

   procedure Get_WinSize (P : Inner_Acc; W : out Devices.TermIOs.Win_Size) is
   begin
      Synchronization.Seize (P.Global_Data_Mutex);
      W := P.Term_Size;
      Synchronization.Release (P.Global_Data_Mutex);
   end Get_WinSize;

   procedure Set_WinSize (P : Inner_Acc; W : Devices.TermIOs.Win_Size) is
   begin
      Synchronization.Seize (P.Global_Data_Mutex);
      P.Term_Size := W;
      P.Termios_Changed := True;
      Synchronization.Release (P.Global_Data_Mutex);
   end Set_WinSize;

   procedure Get_Name (P : Inner_Acc; Str : out String; Len : out Natural) is
      Root_Name : constant String := "/dev/pty";
      Idx       : constant String := P.Name_Index'Image;
   begin
      Len := Root_Name'Length + Idx'Length;
      if Str'Length >= Len then
         Str (Str'First .. Str'First + Root_Name'Length - 1) := Root_Name;
         Str (Str'First + Root_Name'Length .. Str'First + Len - 1) := Idx;
      end if;
   end Get_Name;

   procedure Flush_Primary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      if To_Read then
         Synchronization.Seize (P.Primary_Mutex);
         P.Primary_Data   := [others => 0];
         P.Primary_Length := 0;
         Synchronization.Release (P.Primary_Mutex);
      end if;
      if To_Transmit then
         Synchronization.Seize (P.Secondary_Mutex);
         P.Secondary_Data   := [others => 0];
         P.Secondary_Length := 0;
         Synchronization.Release (P.Secondary_Mutex);
      end if;
   end Flush_Primary;

   procedure Flush_Secondary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      if To_Read then
         Synchronization.Seize (P.Secondary_Mutex);
         P.Secondary_Data   := [others => 0];
         P.Secondary_Length := 0;
         Synchronization.Release (P.Secondary_Mutex);
      end if;
      if To_Transmit then
         Synchronization.Seize (P.Primary_Mutex);
         P.Primary_Data   := [others => 0];
         P.Primary_Length := 0;
         Synchronization.Release (P.Primary_Mutex);
      end if;
   end Flush_Secondary;

   procedure Start_Primary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      Synchronization.Seize (P.Global_Data_Mutex);
      if To_Read then
         P.Primary_Read := True;
      end if;
      if To_Transmit then
         P.Primary_Transmit := True;
      end if;
      Synchronization.Release (P.Global_Data_Mutex);
   end Start_Primary;

   procedure Start_Secondary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      Synchronization.Seize (P.Global_Data_Mutex);
      if To_Read then
         P.Secondary_Read := True;
      end if;
      if To_Transmit then
         P.Secondary_Transmit := True;
      end if;
      Synchronization.Release (P.Global_Data_Mutex);
   end Start_Secondary;

   procedure Stop_Primary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      Synchronization.Seize (P.Global_Data_Mutex);
      if To_Read then
         P.Primary_Read := False;
      end if;
      if To_Transmit then
         P.Primary_Transmit := False;
      end if;
      Synchronization.Release (P.Global_Data_Mutex);
   end Stop_Primary;

   procedure Stop_Secondary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      Synchronization.Seize (P.Global_Data_Mutex);
      if To_Read then
         P.Secondary_Read := False;
      end if;
      if To_Transmit then
         P.Secondary_Transmit := False;
      end if;
      Synchronization.Release (P.Global_Data_Mutex);
   end Stop_Secondary;

   procedure IO_Control
      (PTY        : Inner_Acc;
       Is_Primary : Boolean;
       Request    : Unsigned_64;
       Argument   : System.Address;
       Success    : out Boolean)
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Result_Info : Main_Data with Import, Address => Argument;
      Result_Size :  Win_Size with Import, Address => Argument;
      Action      :   Integer with Import, Address => Argument;
      Do_R, Do_T  :   Boolean;
   begin
      Success := True;
      case Request is
         when TCGETS =>
            Get_TermIOs (PTY, Result_Info);
         when TCSETS | TCSETSW | TCSETSF =>
            Set_TermIOs (PTY, Result_Info);
         when TIOCGWINSZ =>
            Get_WinSize (PTY, Result_Size);
         when TIOCSWINSZ =>
            Set_WinSize (PTY, Result_Size);
         when TCFLSH =>
            case Action is
               when TCIFLUSH | TCOFLUSH | TCIOFLUSH =>
                  Do_R := Action = TCIFLUSH or Action = TCIOFLUSH;
                  Do_T := Action = TCOFLUSH or Action = TCIOFLUSH;
                  if Is_Primary then
                     Flush_Primary (PTY, Do_R, Do_T);
                  else
                     Flush_Secondary (PTY, Do_R, Do_T);
                  end if;
               when others =>
                  Success := False;
            end case;
         when TCXONC =>
            case Action is
               when TCOOFF | TCIOFF =>
                  Do_R := Action = TCOOFF;
                  Do_T := Action = TCIOFF;
                  if Is_Primary then
                     Stop_Primary (PTY, Do_R, Do_T);
                  else
                     Stop_Secondary (PTY, Do_R, Do_T);
                  end if;
               when TCOON | TCION =>
                  Do_R := Action = TCOON;
                  Do_T := Action = TCION;
                  if Is_Primary then
                     Start_Primary (PTY, Do_R, Do_T);
                  else
                     Start_Secondary (PTY, Do_R, Do_T);
                  end if;
               when others =>
                  Success := False;
            end case;
         when TIOCSCTTY =>
            Set_Controlling_TTY (Proc, PTY, Success);
         when TIOCNOTTY =>
            Clear_Controlling_TTY (Proc, PTY, Success);
         when others =>
            Success := False;
      end case;
   end IO_Control;
   ----------------------------------------------------------------------------
   procedure Read_From_End
      (End_Mutex   : access Synchronization.Mutex;
       Inner_Len   : access Data_Length;
       Inner_Data  : access TTY_Data;
       Is_Blocking : Boolean;
       Is_Able_To  : Boolean;
       Data        : out Devices.Operation_Data;
       Ret_Count   : out Natural)
   is
   begin
      Data := [others => 0];
      if not Is_Able_To then
         Ret_Count := 0;
         return;
      end if;

      if Is_Blocking then
         loop
            if Inner_Len.all /= 0 then
               Synchronization.Seize (End_Mutex.all);
               exit when Inner_Len.all /= 0;
               Synchronization.Release (End_Mutex.all);
            end if;
            Scheduler.Yield_If_Able;
         end loop;
      else
         Synchronization.Seize (End_Mutex.all);
         if Inner_Len.all = 0 then
            Ret_Count := 0;
            Synchronization.Release (End_Mutex.all);
            return;
         end if;
      end if;

      if Data'Length > Inner_Len.all then
         Ret_Count := Inner_Len.all;
      else
         Ret_Count := Data'Length;
      end if;
      if Data'First > Natural'Last - Ret_Count then
         Ret_Count := Natural'Last - Data'First;
      end if;

      Data (Data'First .. Data'First + Ret_Count - 1) :=
         Inner_Data (1 .. Ret_Count);
      for I in 1 .. Ret_Count loop
         for J in Inner_Data'First .. Inner_Data'Last - 1 loop
            Inner_Data (J) := Inner_Data (J + 1);
         end loop;
         if Inner_Len.all > 0 then
            Inner_Len.all := Inner_Len.all - 1;
         else
            exit;
         end if;
      end loop;

      Synchronization.Release (End_Mutex.all);
   end Read_From_End;

   procedure Write_To_End
      (End_Mutex     : access Synchronization.Mutex;
       Inner_Len     : access Data_Length;
       Inner_Data    : access TTY_Data;
       Is_Blocking   : Boolean;
       Is_Able_To    : Boolean;
       Data          : Devices.Operation_Data;
       Termios       : Devices.TermIOs.Main_Data;
       Is_To_Primary : Boolean;
       Ret_Count     : out Natural)
   is
      procedure Free is new Ada.Unchecked_Deallocation
         (Devices.Operation_Data, Devices.Operation_Data_Acc);

      Final : Natural;
   begin
      if Is_To_Primary                         and then
         (Termios.Output_Modes and OPOST) /= 0 and then
         (Termios.Output_Modes and ONLCR) /= 0
      then
         --  Check whether there are new lines, and if they are, make a
         --  substring, replace there the newlines, and use that.
         Final := 0;
         for C of Data loop
            if C = Unsigned_8 (Character'Pos (Ada.Characters.Latin_1.LF)) then
               Final := Final + 1;
            end if;
            Final := Final + 1;
         end loop;

         if Final /= Data'Length then
            declare
               Tmp      : Devices.TermIOs.Main_Data := Termios;
               New_Data : Devices.Operation_Data_Acc :=
                  new Devices.Operation_Data'[1 .. Final => 0];
            begin
               Final := 1;
               for C of Data loop
                  if C = Unsigned_8 (Character'Pos (Ada.Characters.Latin_1.LF))
                  then
                     New_Data (Final) :=
                        Unsigned_8 (Character'Pos (Ada.Characters.Latin_1.CR));
                     Final := Final + 1;
                  end if;
                  New_Data (Final) := C;
                  Final := Final + 1;
               end loop;

               Tmp.Output_Modes := 0;
               Write_To_End
                  (End_Mutex     => End_Mutex,
                   Inner_Len     => Inner_Len,
                   Inner_Data    => Inner_Data,
                   Is_Blocking   => Is_Blocking,
                   Is_Able_To    => Is_Able_To,
                   Data          => New_Data.all,
                   Termios       => Tmp,
                   Is_To_Primary => Is_To_Primary,
                   Ret_Count     => Ret_Count);

               Final := Ret_Count;
               for C of New_Data (1 .. Final) loop
                  if C = Unsigned_8 (Character'Pos (Ada.Characters.Latin_1.CR))
                  then
                     Ret_Count := Ret_Count - 1;
                  end if;
               end loop;

               Free (New_Data);
               return;
            end;
         end if;
      end if;

      if not Is_Able_To then
         Ret_Count := 0;
         return;
      end if;

      if Is_Blocking then
         loop
            if Inner_Len.all /= Inner_Data'Length then
               Synchronization.Seize (End_Mutex.all);
               exit when Inner_Len.all /= Inner_Data'Length;
               Synchronization.Release (End_Mutex.all);
            end if;
            Scheduler.Yield_If_Able;
         end loop;
      else
         Synchronization.Seize (End_Mutex.all);
         if Inner_Len.all = Data'Length then
            Ret_Count := 0;
            Synchronization.Release (End_Mutex.all);
            return;
         end if;
      end if;

      if Data'Length > Inner_Data'Length or else
         Data'Length > Inner_Data'Length - Inner_Len.all
      then
         Final := Inner_Data'Length;
         Ret_Count := Inner_Data'Length - Inner_Len.all;
      else
         Final := Inner_Len.all + Data'Length;
         Ret_Count := Data'Length;
      end if;
      if Data'First > Natural'Last - Ret_Count then
         Ret_Count := Natural'Last - Data'First;
         Final := Inner_Len.all + Ret_Count;
      end if;

      Inner_Data (Inner_Len.all + 1 .. Final) :=
         Data (Data'First .. Data'First + Ret_Count - 1);
      Inner_Len.all := Final;
      Synchronization.Release (End_Mutex.all);
   end Write_To_End;
   ----------------------------------------------------------------------------
   procedure Dev_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Devices.Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset);
      PTY  : constant Inner_Acc := Inner_Acc (Conv.To_Pointer (Key));
      Succ : Status;
   begin
      Read_Secondary
         (To_Read     => PTY,
          Data        => Data,
          Is_Blocking => Is_Blocking,
          Ret_Count   => Ret_Count,
          Success     => Succ);
      Success := (if Succ = PTY_Success then Dev_Success else Dev_IO_Failure);
   end Dev_Read;

   procedure Dev_Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Devices.Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Dev_Status;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset);
      PTY  : constant Inner_Acc := Inner_Acc (Conv.To_Pointer (Key));
      Succ : Status;
   begin
      Write_Secondary
         (To_Write    => PTY,
          Data        => Data,
          Is_Blocking => Is_Blocking,
          Ret_Count   => Ret_Count,
          Success     => Succ);
      Success := (if Succ = PTY_Success then Dev_Success else Dev_IO_Failure);
   end Dev_Write;

   function Dev_IO_Control
      (Key      : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
      Success : Boolean;
   begin
      IO_Control
         (PTY        => Inner_Acc (Conv.To_Pointer (Key)),
          Is_Primary => False,
          Request    => Request,
          Argument   => Argument,
          Success    => Success);
      return Success;
   end Dev_IO_Control;
end IPC.PTY;
