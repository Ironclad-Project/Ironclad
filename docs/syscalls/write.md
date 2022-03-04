# `write`

## Prototype

```c
ssize_t write(int fd, void *buffer, size_t count);
```

## Description

This syscall attempts to write up to passed count to the file descriptor from
the buffer.

On files that support seeking, the write operation commences at the file offset,
and the file offset is incremented by the number of bytes written. If the file
offset is at or past the end of file, no bytes are written, and write
returns zero.
    
## Return values and errno

This syscall returns the number of bytes written, or `-1` on failure.
errno is to be set to:

- `EBADF`: Bad file descriptor.
- `EINVAL`: The buffer is not accessible.
