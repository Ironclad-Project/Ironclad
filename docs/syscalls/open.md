# `open`

## Prototype

```c
int open(char *path, int flags);
```

## Description

This syscall opens the passed file, depending on the flags passed, it may
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

## Return values and errno

This syscall returns the opened file descriptor or -1 on error.
errno is to be set according to this conditions:

- `EINVAL`: Flags are inconsistent, or the path for opening is not valid.
