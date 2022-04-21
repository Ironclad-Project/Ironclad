<!---
pid.md: PID and process related syscalls
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

# `getpid`/`getppid`

## Prototypes

```c
int getpid();
int getppid();
```

## Description

`getpid` returns the process ID (PID) of the calling process. `getppid` does
the same but it returns the one of the parent, which is the process that
created the callee by a myriad of ways.

## Return values and errno

This functions are always successful.
