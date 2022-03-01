# `exit`

## Prototype

```c
void exit(uint64_t status);
```

## Description

This syscall terminates the calling process "immediately".

Any state acquired by the process is not finalized as an implementation
short-coming. In the future, the intention is for:

- Any open file descriptors belonging to the process to be closed.
- For any children of the process to be finalized and removed.
- For the process's parent to be sent a `SIGCHLD` signal.

## Return values and errno

This syscall does not return, thus, it has no return value or errno.
