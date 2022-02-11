# Command line specification and its arguments

Ironclad takes, as part of its boot protocol, a series of options and values.
The parsing of this options is architecture and platform independent, while the
ability to modify said options and values is entirely up to the platform and
bootloader.

## Format

The format is the following:

```
key1=value1 key2=value2 key3=value3
```

And must be passed as a NUL-terminated, C-style string.

## Keys and values

This are the keys and values the kernel takes, and under which circumstances:

- `init=<path>`: Checked for an init program, if any.
