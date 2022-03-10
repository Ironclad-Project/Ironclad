# `mmap`/`munmap`

## Prototypes

```c
void *mmap(void *hint, size_t length, int protection, int flags, int fd, off_t offset);
int munmap(void *address, size_t length);
```

## Description

`mmap` creates a new mapping in the virtual address space of the calling
process.

An address can be passed, if it is `null`, then the kernel gets to choose the
address, else, it is taken as a hint about where to place the mapping.

`protection` and `flags` are a bitfield of the following flags:

| Flag         | Meaning                           | Value  |
| ------------ | --------------------------------- | ------ |
| `PROT_NONE`  | No specific protection            | 0b0000 |
| `PROT_READ`  | Read permissions                  | 0b0001 |
| `PROT_WRITE` | Write permissions                 | 0b0010 |
| `MAP_FIXED`  | Use `hint` as a hard requirement  | 0b0100 |
| `MAP_ANON`   | Mapping is not backed by any file | 0b1000 |

## Return values and errno

`mmap` returns a pointer to the allocated area, or `-1` on failure.
`munmap` returns `0` on success, `-1` on failure.
Both functions set `EINVAL` for bad hints or parameters.
