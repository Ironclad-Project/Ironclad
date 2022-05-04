<!---
hostname.md: hostname-related syscalls.
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

# `sethostname`

## Prototype

```c
int sethostname(const char *buffer, size_t length);
```

## Description

This syscall sets the kernel hostname to the passed string.

## Return values and errno

`0` is returned on success and `-1` on failure, with the following errno:
- `EFAULT`: The passed buffer points to an invalid address.
- `EINVAL`: The passed length is bigger than the kernel can handle or 0.
