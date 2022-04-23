<!---
fork.md: fork syscall
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

# `fork`

## Prototype

```c
pid_t fork(void);
```

## Description

This syscall creates a new process by duplicating the calling process.
The new process is referred to as the child process.The calling process is
referred to as the parent process. The processes run on copied memory, and at
the time of cloning have the same file descriptors open and memory mappings.

## Return values and errno

This syscall returns `0` on success for the child, and the children PID to
the parent, in failure, the parent gets `-1` with the following errno:

- `EAGAIN`: The system could not fork right now.
