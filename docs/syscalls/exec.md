<!---
exec.md: exec syscall
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

# `exec`

## Prototype

```c
int exec(const char *path, char *const argv[], char *const envp[]);
```

## Description

This syscall executes the program passed with the passed argv and evp, closing
all the threads of the callee process and putting a single one in place for the
new program. Other process-specific elements like file descriptors are
untouched.

## Return values and errno

This syscall only returns in failure with `-1` with the following errno:

- `ENOENT`: The file passed in path doesnt exist.
- `EACCES`: The file couldn't be launched.
