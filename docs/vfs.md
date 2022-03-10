# Virtual filesystem, roots, objects, and the like

Ironclad has quite a peculiar way of handling devices and exposing them to
userspace which sets it apart from other kernels like Linux. 

## Root devices

Ironclad lacks the idea of a centralized root, instead, all devices (when
it makes sense), from virtual ones like streams, to physical ones like HDDs, get
a root of their own, isolated from the rest of devices and files. Each of this
roots is free to define read, write, open, and close operations, along other
details, like internal file hierarchy or partitions, to their own liking
and accord. Ironclad automounts and manages permissions for each root.

Ironclad denotes roots with an `@` sign, followed by a fixed 7 characters long
string. The following roots will always be added by the system:

- `@zerodev`: Equivalent to `/dev/zero` on a traditional Unix-like system.
- `@nulldev`: Equivalent to `/dev/null` on a traditional Unix-like system.
- `@kernout`: Used for kernel error-reporting.

## Path building

For example, while in a Unix-like environment you would have an absolute path
be `/usr/bin/myprogram`, in Ironclad paths with a leading `/` are relative to
the root of the current path. For it to be truly absolute, the root would
need to be specified, for example: `@hdddev1/usr/bin/myprogram`.

Relative paths work as one would expect, with the exception that you cannot
change directory to the one below if you are already in a root, as they are
isolated, and not folders of an overarching root.

## Reserved characters and messages

Given the use of `@` to denote root devices, it is illegal to use the symbol
to lead a filename. If Ironclad encounters said symbol in the beggining of a
filename when mounting, it will be automatically replaced by `_` virtually.

Along with a leading `@`, `/` is not allowed for its role in path
denotations, and non-printable characters.
