Ironclad is a kernel for x86_64 devices exclusively (for now) with an enphasis
on security without a compromise in speed or complexity, written in Ada.

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

## Thanks to

- [Mintsuki](https://github.com/mintsuki) et al - For the
[Limine](https://github.com/limine-bootloader/limine) project, used for testing
and code reference.

## Licensing

This project and its components are licensed under the GPLv3 license.
The license in full is provided in `LICENSE.md`, and a little header is
displayed on the pertinent source.
