<!---
seek.md: seek syscall
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

# `seek`

## Prototype

```c
off_t seek(int fd, off_t offset, int whence);
```

## Description

This syscall repositions the file offset of the passed file description
to the passed offset according to the directive whence as follows:

- `SEEK_SET` (1): Set to offset bytes.
- `SEEK_CUR` (2): Set to its current location plus offset bytes.
- `SEEK_END` (4): Set to the size of the file plus offset bytes.

## Return values and errno

This syscall returns the resulting offset, or `-1` on failure.
errno is to be set to:

- `EBADF`: Bad file descriptor.
- `EINVAL`: The whence is malformed or the resulting offset would be invalid.
- `ESPIPE`: Seek called on a TTY or pipe.
