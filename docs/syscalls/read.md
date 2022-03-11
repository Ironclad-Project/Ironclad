# `read`/`write`

## Prototypes

```c
ssize_t read(int fd, void *buffer, size_t count);
ssize_t write(int fd, void *buffer, size_t count);
```

## Description

These syscalls attempts to read or write up to passed count from the passed
file descriptor.

On files that support seeking, the operation commences at the file offset,
and the file offset is incremented by the number of bytes read or written. If
the file offset is at or past the end of file, no bytes are read or written,
and the operation returns zero.
    
## Return values and errno

These syscalls returns the number of bytes operated on, or `-1` on failure.
errno is to be set to:

- `EBADF`: Bad file descriptor.
- `EINVAL`: The buffer is not accessible.
