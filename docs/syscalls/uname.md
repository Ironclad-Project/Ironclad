<!---
uname.md: uname syscall.
Copyright (C) 2021 streaksu

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-->

# `uname`

## Prototype

```c
struct utsname {
   char sysname[64];    // Kernel name (e.g., "Ironclad")
   char nodename[64];   // Hostname of the machine.
   char release[64];    // Kernel release (e.g., "2.6.28")
   char version[64];    // Kernel build information.
   char machine[64];    // Hardware identifier (e.g., "x86")
   char domainname[64]; // NIS or YP domain name.
};

int uname(struct utsname +name);
```

## Description

This syscall reports kernel information, like version, name, and hostname.

## Return values and errno

This syscall returns `0` on success, and `-1` on failure, with the only errno
being `EFAULT` if the passed pointer is in an invalid address.
