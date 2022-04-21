<!---
exit.md: exit syscall
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

# `exit`

## Prototype

```c
void exit(uint64_t status);
```

## Description

This syscall terminates the calling process "immediately".
Any open file descriptors belonging to the process to be closed, and any
threads of execution are terminated.

## Return values and errno

This syscall does not return, thus, it has no return value or errno.
