# `read`

## Prototype

```c
ssize_t read(int fd, void *buffer, size_t count);
```

## Description

This syscall attempts to read up to passed count from the file descriptor into
the buffer.

On files that support seeking, the read operation commences at the file offset,
and the file offset is incremented by the number of bytes read. If the file
offset is at or past the end of file, no bytes are read, and read returns zero.
    
## Return values and errno

This syscall returns the number of bytes read, or `-1` on failure.
errno is to be set to:

- `EBADF`: Bad file descriptor.
- `EINVAL`: The buffer is not accessible.
