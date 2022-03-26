# Contributing to Ironclad

Thanks for considering contributing to Ironclad, this document details
practices, coding standards, and other useful information for your life to be
easier.

## Useful flags for debugging and diagnostics

When configuring Ironclad for some development, a useful command to do so is

```bash
./autogen.sh ADAFLAGS='-O2 -g -Wall -gnaty'
```

`-Wall` and `-gnaty` are good for general development and coding style errors,
apart of that, in some cases it can be good to disable optimization, or run
QEMU with no KVM support for testing certain hardware bugs.

## Documenting code

Each code contribution is expected to add the relevant documentation as
comments and standalone documentation in the case of syscalls or kernel
interfaces.

## Licensing code

You may only contribute code that complies with the project's license, or code
ruled by GPL-compatible licenses under certain conditions, but this should not
be done without an extensive explanation.

When adding new code, be sure to check the changes have a valid license header,
and detail the authors of the code. For files with a main author and small
touches being done on top of it, you can update the authors in the copyright
to `Copyright (C) 2021 [main author's name], et. al.`, or add your name or
pseudonym for a major contribution to the comma-separated list.
