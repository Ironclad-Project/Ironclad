--  userland-mac.ads: Mandatory access control.
--  Copyright (C) 2023 streaksu
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

with Interfaces; use Interfaces;
with VFS;        use VFS;

package Userland.MAC is
   --  Capabilities a process can have, which rule kinds of operations it can
   --  access.
   type Capabilities is record
      Can_Change_Scheduling : Boolean;
      Can_Spawn_Others      : Boolean;
      Can_Access_Entropy    : Boolean;
      Can_Modify_Memory     : Boolean;
      Can_Use_Networking    : Boolean;
      Can_Manage_Networking : Boolean;
      Can_Manage_Mounts     : Boolean;
      Can_Manage_Power      : Boolean;
      Can_Trace_Children    : Boolean;
      Can_Change_UIDs       : Boolean;
      Can_Manage_MAC        : Boolean;
      Can_Use_Clocks        : Boolean;
      Can_Signal_All        : Boolean;
      Can_Change_GIDs       : Boolean;
      Can_Bypass_IPC_Checks : Boolean;
   end record;

   --  Permissions a file can have.
   type Permissions is record
      Includes_Contents : Boolean; --  Affects children if a directory.
      Can_Read          : Boolean; --  Read permissions.
      Can_Write         : Boolean; --  Write permissions.
      Can_Execute       : Boolean; --  Execute permissions.
      Can_Append_Only   : Boolean; --  Can open files append only.
      Can_Lock_Files    : Boolean; --  Can lock the affected files.
   end record;

   --  When a MAC failure is encountered, we can do a series of things.
   type Enforcement is
      (Deny,            --  Just deny the operation with an appropiate error.
       Deny_And_Scream, --  Deny and obnoxiously report the error.
       Kill);           --  Don't panic process, but you are already dead.

   --  MAC allows specifying limits for several resources.
   type Limit_Value is new Unsigned_64;
   type Limit_Type  is
      (Core_Size_Limit,    --  Core file dump size limit.
       CPU_Time_Limit,     --  Total CPU time in seconds.
       File_Size_Limit,    --  Total file size.
       Opened_File_Limit,  --  Limit on opened file descriptor for the process.
       Stack_Size_Limit,   --  Limit on stack size.
       Memory_Size_Limit); --  Limit on total virtual memory size.

   --  Type to wrap all the storage and permissions for a MAC context.
   type Context is private;
   Default_Context : constant Context;

   --  Get the associated action for enforcement with the context.
   --  @param Ctx The context to operate on.
   --  @return The enforcement.
   function Get_Enforcement (Ctx : Context) return Enforcement;

   --  Get the associated action for enforcement with the context.
   --  @param Ctx The context to operate on.
   --  @param Act The enforcement to set.
   procedure Set_Enforcement (Ctx : in out Context; Act : Enforcement);

   --  Get the capabilities on the passed context.
   --  @param Ctx The context to operate on.
   --  @return The capabilities.
   function Get_Capabilities (Ctx : Context) return Capabilities;

   --  Set the capabilities on the passed context.
   --  @param Ctx  The context to operate on.
   --  @param Caps Capabilities to set.
   procedure Set_Capabilities (Ctx : in out Context; Caps : Capabilities);

   --  Check permissions associated to an already added inode and FS combo.
   --  If no files are added to the context with permissions, all files return
   --  permission.
   --  @param Data MAC instance to use.
   --  @param FS   FS handle to check.
   --  @param Ino  Inode to check.
   --  @return Returned permissions.
   function Check_Permissions
      (Data : Context;
       FS   : VFS.FS_Handle;
       Ino  : VFS.File_Inode_Number) return Permissions;

   --  Status one can return from an addition of a path to MAC permissions.
   type Addition_Status is (Success, No_Space, Is_Conflicting);

   --  Add an entity for MAC.
   --  @param Data   MAC instance to modify.
   --  @param FS     Filesystem to check.
   --  @param Ino    Inode to check.
   --  @param Perms  Permissions to add.
   --  @param Status Status of the operation.
   procedure Add_Entity
      (Data   : in out Context;
       FS     : VFS.FS_Handle;
       Ino    : VFS.File_Inode_Number;
       Perms  : Permissions;
       Status : out Addition_Status)
   with Pre => FS /= VFS.Error_Handle;

   --  Get the limit of a resource.
   --  @param Data     MAC instance to modify.
   --  @param Resource Resource to limit.
   --  @param Limit    Limit of the resource.
   function Get_Limit
      (Data     : Context;
       Resource : Limit_Type) return Limit_Value;

   --  Set the limit of a resource.
   --  @param Data      MAC instance to modify.
   --  @param Resource  Resource to limit.
   --  @param Limit     Limit of the resource.
   --  @param Could_Set True if the value was lower than previous and was set.
   procedure Set_Limit
      (Data      : in out Context;
       Resource  : Limit_Type;
       Limit     : Limit_Value;
       Could_Set : out Boolean);

private

   Filter_Path_Length : constant := 75;
   type Filter is record
      Is_Used   : Boolean;
      Is_Device : Boolean;
      FS        : VFS.FS_Handle;
      Ino       : VFS.File_Inode_Number;
      Perms     : Permissions;
   end record;

   type Filter_Arr is array (Natural range <>) of Filter;
   type Limit_Arr  is array (Limit_Type)       of Limit_Value;
   type Context is record
      Action  : Enforcement;
      Caps    : Capabilities;
      Limits  : Limit_Arr;
      Filters : Filter_Arr (1 .. 30);
   end record;

   Default_Context : constant Context :=
      (Action  => Deny,
       Caps    => (others => True),
       Limits  =>
         (Core_Size_Limit   => 0, --  Disabled by default due to privacy.
          CPU_Time_Limit    => Limit_Value'Last,
          File_Size_Limit   => Limit_Value'Last,
          Opened_File_Limit => Limit_Value'Last,
          Stack_Size_Limit  => 16#400000#, --  4 MiB by default.
          Memory_Size_Limit => Limit_Value'Last),
       Filters => (others =>
         (Is_Used   => False,
          Is_Device => False,
          FS        => VFS.Error_Handle,
          Ino       => 0,
          Perms     => (others => False))));

   procedure Add_Filter
      (Data   : in out Context;
       Filt   : Filter;
       Status : out Addition_Status);
end Userland.MAC;
