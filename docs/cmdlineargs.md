<!---
cmdlineargs.md: Command-line argument reference.
Copyright (C) 2021 streaksu

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-->

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

- `root=<device>`: Device to mount as root, if any.
- `init=<path>`: Checked for an init program, if any.
- `memtracing`:  Enables memory tracing at the expense of performance.
- `syscalltracing`: Enables tracing syscalls and their arguments, really
performance taxing, but essential for userland kernel debugging.
