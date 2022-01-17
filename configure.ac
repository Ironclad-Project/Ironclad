# configure.ac: Configure script template for the project.
# Copyright (C) 2021 streaksu
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

AC_PREREQ(2.71)
AC_INIT(Ironclad, 0.0.1, https://github.com/streaksu/ironclad/issues, ironclad)

# Add configure flags for Ada and other needed programs, and put defaults.
AC_ARG_VAR(ADAC, [Ada compiler command [gcc]])
AC_ARG_VAR(ADAFLAGS, [Ada compiler flags [-O2 -g]])
AC_ARG_VAR(AS, [Assembler command [gcc]])
AC_ARG_VAR(ASFLAGS, [Assembler flags [-O2 -g]])
AC_ARG_VAR(LD, [Linker command [gcc]])
AC_ARG_VAR(LDFLAGS, [Linker flags [-O2 -g]])
AC_ARG_VAR(QEMU, [QEMU emulator command [qemu-system-x86_64]])
AC_ARG_VAR(QEMUFLAGS, [QEMU emulator flags [-m 2G -smp 4 -debugcon stdio -enable-kvm]])

test -z "$ADAC"      && ADAC="gcc"
test -z "$ADAFLAGS"  && ADAFLAGS="-O2 -g"
test -z "$AS"        && AS="gcc"
test -z "$ASFLAGS"   && ASFLAGS="-O2 -g"
test -z "$LD"        && LD="gcc"
test -z "$LDFLAGS"   && LDFLAGS="-O2 -g"
test -z "$QEMU"      && QEMU="qemu-system-x86_64"
test -z "$QEMUFLAGS" && QEMUFLAGS="-m 2G -smp 4 -debugcon stdio -enable-kvm"

# Put defaults for prefix and output dirs.
AC_PREFIX_DEFAULT(/boot)

# Replace the variables in the makefile.
AC_CONFIG_FILES(GNUmakefile source/config.ads)
AC_OUTPUT
