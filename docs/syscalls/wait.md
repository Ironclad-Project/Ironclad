<!---
wait.md: wait syscall
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

# `wait`

## Prototype

```c
pid_t wait(pid_t pid, int *status, int options);
```

## Description

This syscall suspends execution until the passed pid exits, to then store the
exit code in `status`.

`pid` can be a PID the callee is a parent of, or `-1` to wait on all the PIDs
the callee has as children. `0`, which waits on all the children of a process
group, is not implemented yet.

## Return values and errno

This syscall returns the PID waited on or `-1` on failure, along with the
following errno:
- `ECHILD`: The passed PID does not exist.
- `EINVAL`: The passed options are not correct, or `pid` is 0.
