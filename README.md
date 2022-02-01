# Ironclad

<img align="center" height="300" alt="Seal of approval" src="banner.png"/>

Ironclad is a kernel for x86_64 devices exclusively (for now), written in Ada.

## Building

If the project's `configure` has not been generated, there is a `autogen.sh`.
The steps for a generated project are the usual:

```bash
./configure
make
make test # If wanted.
make install-strip
```

Several flags and variables are available for tuning, `./configure --help` will
display and explain them.

A list of the tools needed for compilation and checked by `configure` is:

- `autoconf` 2.69 and `automake`.
- An ADA compiler, preferably `gcc`.
- A standard linker and GAS assembler.
- `xorriso` and QEMU for testing.

## Thanks to

- [Mintsuki](https://github.com/mintsuki) et al - For the
[Limine](https://github.com/limine-bootloader/limine) project, used for testing
and code reference.

## Licensing

<img align="right" height="68" alt="License logo" src="https://www.gnu.org/graphics/gplv3-with-text-136x68.png"/>

This project and its components are licensed under the GPLv3 license.
The license in full is provided in `LICENSE.md`, and a little header is
displayed on the pertinent source.
