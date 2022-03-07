# `alloc`/`free`

## Prototypes

```c
void *alloc(size_t count);
void free(void *pointer);
```

## Description

This syscalls allocate and free regions of memory for the callee
process, mapping the memory too with permisions for read, write, and execute.

## Return values and errno

`alloc` returns a pointer to the allocated block on success, or `NULL`
on failure, and errno can be set to `ENOMEM` on failure.

`free` cannot fail, and thus, it does not set errno values.
