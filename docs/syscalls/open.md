# `open`/`close`

## Prototypes

```c
int open(char *path, int flags);
int close(int fd);
```

## Description

`open` opens the passed file, depending on the flags passed, it may
create it if not present. By default, the file descriptor will remain open
accross an `execve`, and the file offset is set to the beggining.

The flags can be an OR'd field of the following elements:

| Flag       | Meaning           | Value  |
| ---------- | ----------------- | ------ |
| `O_RDONLY` | Read only         | 0b0001 |
| `O_WRONLY` | Write only        | 0b0010 |
| `O_RDWR`   | Read/Write        | 0b0011 |
| `O_APPEND` | Open at the end   | 0b0100 |
| `O_CREAT`  | Create the file   | 0b1000 |

`close` closes an open file descriptor. Once no open references exist
of a file descriptor, its resources are freed, and the file deleted if needed.

## Return values and errno

`open` returns the opened file descriptor or -1 on error.
`close` returns 0 on success and -1 in failure.

errno is to be set according to this conditions for both functions:

- `EINVAL`: The flags of `open` are inconsistent, or the path for is not valid.
- `EBADF`: The passed file to `close` is not valid.