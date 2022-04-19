# `exec`

## Prototype

```c
int exec(const char *path, char *const argv[], char *const envp[]);
```

## Description

This syscall executes the program passed with the passed argv and evp, closing
all the threads of the callee process and putting a single one in place for the
new program. Other process-specific elements like file descriptors are
untouched.

## Return values and errno

This syscall only returns in failure with `-1` with the following errno:

- `ENOENT`: The file passed in path doesnt exist.
- `EACCES`: The file couldn't be launched.
