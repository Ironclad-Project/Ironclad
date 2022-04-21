<!---
read.md: basic read/write IO syscalls.
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

# `read`/`write`

## Prototypes

```c
ssize_t read(int fd, void *buffer, size_t count);
ssize_t write(int fd, void *buffer, size_t count);
```

## Description

These syscalls attempts to read or write up to passed count from the passed
file descriptor.

On files that support seeking, the operation commences at the file offset,
and the file offset is incremented by the number of bytes read or written. If
the file offset is at or past the end of file, no bytes are read or written,
and the operation returns zero.
    
## Return values and errno

These syscalls returns the number of bytes operated on, or `-1` on failure.
errno is to be set to:

- `EBADF`: Bad file descriptor.
- `EINVAL`: The buffer is not accessible.
