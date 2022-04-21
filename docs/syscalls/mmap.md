<!---
mmap.md: mmap syscall
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

# `mmap`/`munmap`

## Prototypes

```c
void *mmap(void *hint, size_t length, int protection, int flags, int fd, off_t offset);
int munmap(void *address, size_t length);
```

## Description

`mmap` creates a new mapping in the virtual address space of the calling
process.

An address can be passed, if it is `null`, then the kernel gets to choose the
address, else, it is taken as a hint about where to place the mapping.

`protection` and `flags` are a bitfield of the following flags:

| Flag         | Meaning                           | Value  |
| ------------ | --------------------------------- | ------ |
| `PROT_NONE`  | No specific protection            | 0b0000 |
| `PROT_READ`  | Read permissions                  | 0b0001 |
| `PROT_WRITE` | Write permissions                 | 0b0010 |
| `MAP_FIXED`  | Use `hint` as a hard requirement  | 0b0100 |
| `MAP_ANON`   | Mapping is not backed by any file | 0b1000 |

## Return values and errno

`mmap` returns a pointer to the allocated area, or `-1` on failure.
`munmap` returns `0` on success, `-1` on failure.
Both functions set `EINVAL` for bad hints or parameters.
