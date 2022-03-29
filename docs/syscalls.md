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
| Invalid Value   | `EINVAL`         | 1026  |
| Not Implemented | `ENOSYS`         | 1051  |
| Not Supported   | `ENOSUP`         | 1057  |
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
