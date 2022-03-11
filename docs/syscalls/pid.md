# `getpid`/`getppid`

## Prototypes

```c
int getpid();
int getppid();
```

## Description

`getpid` returns the process ID (PID) of the calling process. `getppid` does
the same but it returns the one of the parent, which is the process that
created the callee by a myriad of ways.

## Return values and errno

This functions are always successful.
