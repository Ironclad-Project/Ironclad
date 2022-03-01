# `set_tcb`

## Prototype

```c
int set_tcb(size_t address);
```

## Description

This syscall enables a region in memory pointed by the passed address to be
thread-local storage.

## Return values and errno

This syscall returns `0` on success, and non-zero on failure.
errno is to be set to `EINVAL` if the address is `0`.
