# `open`

## Prototype

```c
int close(int fd);
```

## Description

This syscall closes an open file descriptor. Once no open references exist
of a file descriptor, its resources are freed, and the file deleted if needed.

## Return values and errno

This syscall returns 0 on success and -1 in failure.
errno is to `EBADF` if the passed file descriptor is not valid.
