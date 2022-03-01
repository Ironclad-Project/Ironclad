# `log`

## Prototype

```c
int log(char *status);
```

## Description

This syscall reports a text message to the kernel, for it to report it to
its internal registries and displays for debug purposes. Normally, this means
the message being displayed on screen and debug channels, like qemu's `E9`.

## Return values and errno

This syscall returns `0` on success, and non-zero on failure.
errno is to be set to `EINVAL` if the `status` string is `0`.
