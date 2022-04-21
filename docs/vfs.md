<!---
vfs.md: VFS description.
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
