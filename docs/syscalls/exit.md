# `exit`

## Prototype

```c
void exit(uint64_t status);
```

## Description

This syscall terminates the calling process "immediately".
Any open file descriptors belonging to the process to be closed, and any
threads of execution are terminated.

## Return values and errno

This syscall does not return, thus, it has no return value or errno.
