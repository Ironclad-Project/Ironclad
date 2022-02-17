# Command line specification and its arguments

Ironclad takes, as part of its boot protocol, a series of options and values.
The parsing of this options is architecture and platform independent, while the
ability to modify said options and values is entirely up to the platform and
bootloader.

## Format

The format is a list of keys that can have arguments or not, as such:

```
key1=value1 key2 key3 ... keyN
```

The list must be passed as a NUL-terminated, C-style string.

## Keys and values

This are the keys and values the kernel takes, and under which circumstances:

- `init=<path>`: Checked for an init program, if any.
- `memtracing`:  Enables memory tracing at the expense of performance.
