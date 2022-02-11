# Virtual filesystem, roots, objects, and the like

Ironclad has quite a peculiar way of handling devices and exposing them to
userspace which sets it apart from other kernels like Linux. 

## Path building

For example, while in an environment like Linux you would have a global path
be `/usr/bin/myprogram`, this is illegal on ironclad, and the drive refered
would need to be specified for it to be global: `hdddev1:usr/bin/myprogram`.

Relative paths work as one would expect, with the exception that you cannot
change directory to the one below if you are already in a root, as they are
isolated, and not folders of an overarching root.

## Root devices

Ironclad lacks the idea of a centralized root, instead, all devices (when
it makes sense), from virtual ones like streams, to physical ones like HDDs, get
a root of their own, isolated from the rest of devices and files. Each of this
roots is free to define read, write, open, and close operations, along other
details, like internal file hierarchy or partitions, to their own liking
and accord. Ironclad automounts and manages permissions for each root.

Ironclad denotes roots with a fixed 7 characters long string. The following
roots will always be added by the system:

- `zerodev`: Equivalent to `/dev/zero` on a traditional Linux system.
- `nulldev`: Equivalent to `/dev/null` on a traditional Linux system.
