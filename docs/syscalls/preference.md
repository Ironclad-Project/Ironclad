<!---
preference.md: thread preference syscalls.
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

# `thread_preference`

## Prototypes

```c
int thread_preference(int new_value);
```

## Description

This syscall returns or set the preference of the callee core.

Preference is a metric for how much execution time will be assigned a given
thread, in Ironclad, its represented with a value from 1 to value, with the
higher the value, the more time will be spent on the thread.

The actual maximum value of preference is not set, but it is always guaranteed
that it starts at 1, and higher means relatively greater.

## Return values and errno

When passed `0`, the current thread preference will be returned, else, it
sets it to the passed value, and returns the new value.
Values higher than the maximum preference will just represent the maximum
preference.

In error, `-1` will be returned, and the following errno will be set:

`EINVAL`: The argument is not valid.
`ENOTSUP`: The operation could not be completed.
