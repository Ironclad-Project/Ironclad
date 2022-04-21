<!---
syscalls.md: Syscall list and errno reference.
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

# Syscalls in Ironclad

Ironclad uses `int` for syscalls, unlike a lot of other OSes that use
`syscall`, or other mechanisms, this document details the ABI, and the list
of syscalls.

## ABI

Syscalls are invoked in Ironclad by triggering an interrupt to vector 80
in hex, as such:

```x86asm
int $0x80
```

The index of the syscall is passed on `%rax`.

The return value is returned on `%rax`, errno is returned on `%rdx`, arguments
are passed over `%rdi`, `%rsi`, `%rdx`, `%rcx`, `%r8`, and `%r9`, following the
SysV ABI.

## Errno list

Errno are values returned by the kernel to detail the nature of an error in
depth, this is the list of available values:

| Meaning         | C `errno.h` name | Value |
| --------------- | ---------------- | ----- |
| No Error        |                  | 0     |
| Bad Access      | `EACCES`         | 1002  |
| Invalid Value   | `EINVAL`         | 1026  |
| No entity       | `ENOENT`         | 1043  |
| Not Implemented | `ENOSYS`         | 1051  |
| Not Supported   | `ENOSUP`         | 1057  |
| Invalid seek    | `ESPIPE`         | 1069  |
| Bad FD          | `EBADFD`         | 1081  |

## Syscall list

This are the syscalls, their prototypes, and a short description.

| Syscall                                     | Purpose              | Index |
| ------------------------------------------- | -------------------- | ----- |
| [exit](syscalls/exit.md)                    | Code flow control    | 0     |
| [set_tcb](syscalls/set_tcb.md)              | Thread-local storage | 1     |
| [open](syscalls/open.md)                    | Synchronous File IO  | 2     |
| [close](syscalls/open.md)                   | Synchronous File IO  | 3     |
| [read](syscalls/read.md)                    | Synchronous File IO  | 4     |
| [write](syscalls/read.md)                   | Synchronous File IO  | 5     |
| [seek](syscalls/seek.md)                    | Synchronous File IO  | 6     |
| [mmap](syscalls/mmap.md)                    | Memory mapping       | 7     |
| [munmap](syscalls/mmap.md)                  | Memory mapping       | 8     |
| [getpid](syscalls/pid.md)                   | Process management   | 9     |
| [getppid](syscalls/pid.md)                  | Process management   | 10    |
| [thread_preference](syscalls/preference.md) | Process management   | 11    |
| [exec](syscalls/exec.md)                    | Process management   | 12    |
