<!---
cwd.md: Current working directory syscalls
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

# `getcwd`/`chdir`

## Prototype

```c
char *getcwd(char *buf, size_t size);
int chdir(const char *path);
```

## Description

This syscalls manage the current working directory of the callee process.
`getcwd` will fetch it as an absolute path, while `chdir` will set it with a
C-style string.

## Return values and errno

`getcwd` returns a pointer to the string passed, or `NULL` if the string is not
big enough, including NUL, and sets the following errno:

- `EFAULT`: The passed buffer has a bad address.
- `ERANGE`: The passed buffer + size is not big enough for the path, and it is
safe to reallocate and try again.
- `EINVAL`: The length is `0`.

`chdir` returns `0` on success and `-1` on failure, with the following errno:

- `EFAULT`: The passed path points to a bad address.
- `ENAMETOOLONG`: The passed path is too long.
- `ENOTDIR`: The passed path is not a directory or has 0 length.
