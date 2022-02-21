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
| Not Implemented | `ENOSYS`         | 1051  |

## Syscall list

This are the syscalls and their prototypes.

| Syscall                 | Purpose                                               | Index |
| ----------------------- | ----------------------------------------------------- | ----- |
| `log(uint64_t address)` | Passes a C null-terminated string to kernel reporting | 0     |
