This file details all of Ironclad's releases, along with their changes and
other details, in a more readable and accessible way than commit history.

The project uses [semantic versioning](https://semver.org/) for its releases.
Statements on stability as far as semantic versioning is concerned are done
regarding userland compatibility. No guarantees of kernel code stability inside
the kernel are done.

Ironclad, so far, is "pre-stable release" software, so no stability or
support guarantees are made between releases. In a future, when Ironclad is
stable, this section will have a table of all supported releases.

# Version history

Here is a history of all releases ordered by release date including upcoming
release changes.

## Upcoming release

### Non-breaking changes

- Several fixes on x86_64-limine's PCI and NVMe code.
- Implemented SO_PEERCRED for getsockopt.
- Fixed a bug that made the `create_thread` syscall ignore the passed
  argument for the new thread.

### Breaking changes

- Moved from KVM to NVMM for Ironclad's virtualization interface.
- Now `AT_SECURE` is assed only when the `euid` of the process is 0.
- Removed the `pread`/`pwrite` syscalls in favour of arguments to the
  `read`/`write` syscalls, saving a bunch of code.

### Non-code related changes

- 🦗🦗🦗

### The people behind this release

- streaksu - <streaksu@ironclad-os.org> | https://codeberg.org/streaksu
- no92 - https://codeberg.org/no92 | https://github.com/no92

## 0.7.0 - May 27, 2025

### Non-breaking changes

- Greatly improved block device (SATA, NVMe) caching. Leading to big
  performance improvements.
- Added a basic NVMe driver.
- Implement support for x86's SMAP, and expose it in an architecture-independent way.
- Harden syscall memory argument checks.
- Added AT_REMOVEDIR for the `unlink` syscall.
- Added a kernel-side terminal for easier early boot debugging for the
  x86_64-limine target.
- Improve PS2 device detection as to avoid false positives and false negatives.
- Reworked x86_64-limine's clock subsystem.
- Add support for pipe sockets with `socketpair`.
- Improved internal kernel printing by adding support for more `'Image` uses.
- Better handle Out Of Memory (OOM) conditions with fallible allocations.
- Improve ACPI power button handling by exposing them to userland with the
  `pwrbutton`/`sleepbutton` devices, which can be polled by userland daemons
  for power commands.
- Expand program loading ASLR applications by also using it for dynamic
  program bases.
- Tons of bug fixes.
- Improve signal handling.

### Breaking changes

- Removed the `spawn` syscall, modified the `fork` syscall to take a
  `FORK_VFORK` argument that makes it not copy the parent process's tables. For
  the rationale, visit [our blog's article on the matter](https://blog.ironclad-os.org/modernizing-process-initialization-in-ironclad/).
- Removed power handling interfaces replaced by the `pwrbutton`/`sleepbutton`
  changes.
- Changed the mapping of the signal numbers handled by Ironclad due to POSIX
  requirements, affecting SIGHUP, SIGINT, SIGQUIT, SIGABRT, SIGKILL, SIGALRM,
  and SIGTERM.
- Replaced the `getrandom` syscall for the POSIX-compliant `getentropy`.

### Non-code related changes

- Moved our infrastructure from Savannah and Github to Codeberg.
- Coalesced project web presence to https://ironclad-os.org.
- Started work on the [project's blog](https://blog.ironclad-os.org), a
  hub for Ironclad news, articles, and all Ironclad, with RSS feeds for easy
  following.

### The people behind this release

- streaksu - <streaksu@ironclad-os.org> | https://codeberg.org/streaksu
- mintsuki - https://codeberg.org/mintsuki
- no92 - https://github.com/no92
- zhml - https://codeberg.org/zhml

### Feedback

For any feedback, please contact us at one of
[Ironclad's communities](https://ironclad-os.org/community.html), or, as a
second option, contact the people behind this release.

## 0.6.0 - Jan 31, 2025

This release comes after a long gap of releases, and has had massive work done
on almost every aspect of the kernel. Because of this, I find it hard to
summarize the changes, but a gist of it is:

This is a non-exhaustive list of the big changes included in this release:

- Change from the multiboot family of boot protocols to limine.
- Added x86_64 ACPI support for poweroff and reboot.
- Added initial riscv64-limine port.
- Replace signaling posts with POSIX signals.
- Massively improve the VFS layer.
- Add interruptible clustering to the scheduler.
- Support for shared memory segments, improved socket support, greatly
- improved PTY support.
- Dozens of bug fixes.

## 0.5.0 - Oct 31, 2023

This release brings a lot of improvements to mainly the scheduling, time
keeping, userland, and networking subsystems.

This is a non-exhaustive list of the big changes included in this release:

- Implemented a new scheduling algorithm, based on ARINC partitions.
- Added support for monotonic and realtime clocks.
- Added support for signaling posts as the main method for signaling in
  Ironclad.
- Added IPv4 and IPv6 local networking thru the new device loopback.
- Implemented device enumeration using UUIDs.

## 0.4.0 - Aug 31, 2023

This release brings a lot of improvements on almost all subsystems of the
kernel.

This is a non-exhaustive list of the big changes included in this release:

- Formally verified big chunks of architecture-independent code for AoRTE,
  SPARKified what was not verified.
- Improved mandatory access control (MAC).
- Reworked syscall mechanism for x86_64-multiboot2, with big performance gains.
- Added a hardware watchdog framework.
- Implement UIDs, limits, and corefile dumping.
- Start work on an arm-raspi2 port.
- Improved filesystem support, work started on QNXFS.

## 0.3.0 - Apr 2, 2023

This release brings a lot of improvements on almost all subsystems of the
kernel.

This is a terse list of some of the changes included in this release:

- Added support for ATA/SATA drives for x86 machines.
- Added support for PTYs and other kernel-level IPC primitives.
- A new and improved scheduler for real time.
- New autotools-based buildsystem.
- Added integrity-ensuring and advanced debugging facilities.
- Formally verified the device management layer, along with some drivers.
- Improved EXT and FAT support.

## 0.2.0 - Nov 16, 2022

This release is really security and cryptography heavy, with the addition of a
lot of important tools in the task of making Ironclad a more resilient and
secure platform.

This is a terse changelog of the changes included in this release:

- Added a new Mandatory Access Control (MAC) framework.
- Implemented hardware-accelerated AES and several AES-based ciphers for
  userland use.
- Implemented an initial version of Address Space Layout Randomization (ASLR).
- Added unit-tests for a small set of kernel utilities that can easily be
  tested on a host system, like ciphers or general library utilities.
- New architecture-independent RTC interfaces thru /dev/rtc.
- Bug fixes and several small improvements.

## 0.1.0 - Oct 16, 2022

This release marks the start of versioned Ironclad development, following
semver rules.

Major release 0 will be maintained until API and features become stable.
