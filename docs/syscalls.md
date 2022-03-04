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
| Bad FD          | `EBADFD`         | 1081  |

## Syscall list

This are the syscalls, their prototypes, and a short description.

| Syscall                        | Purpose              | Index |
| ------------------------------ | -------------------- | ----- |
| [log](syscalls/log.md)         | Message reporting    | 0     |
| [exit](syscalls/exit.md)       | Code flow control    | 1     |
| [set_tcb](syscalls/set_tcb.md) | Thread-local storage | 2     |
| [open](syscalls/open.md)       | Synchronous File IO  | 3     |
| [close](syscalls/close.md)     | Synchronous File IO  | 4     |
| [read](syscalls/read.md)       | Synchronous File IO  | 5     |
| [write](syscalls/write.md)     | Synchronous File IO  | 6     |
| [seek](syscalls/seek.md)       | Synchronous File IO  | 7     |
