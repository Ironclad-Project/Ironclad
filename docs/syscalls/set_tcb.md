<!---
set_tcb.md: TCB and thread-local storage syscalls
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

# `set_tcb`

## Prototype

```c
int set_tcb(size_t address);
```

## Description

This syscall enables a region in memory pointed by the passed address to be
thread-local storage.

## Return values and errno

This syscall returns `0` on success, and non-zero on failure.
errno is to be set to `EINVAL` if the address is `0`.
