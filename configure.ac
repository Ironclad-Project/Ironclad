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

AC_PREREQ(2.69)
AC_INIT(Ironclad, 0.0.1, https://github.com/streaksu/ironclad/issues, ironclad)

# Search for some programs.
AC_PROG_INSTALL

# Add configure flags for Ada and other needed programs, and put defaults.
AC_ARG_VAR(ADAC, [Ada compiler command [gcc]])
AC_ARG_VAR(ADAFLAGS, [Ada compiler flags [-O2 -g]])
AC_ARG_VAR(AS, [Assembler command [as]])
AC_ARG_VAR(ASFLAGS, [Assembler flags [-O2 -g]])
AC_ARG_VAR(LD, [Linker command [ld]])
AC_ARG_VAR(LDFLAGS, [Linker flags [-O2 -g]])
AC_ARG_VAR(QEMU, [QEMU emulator command [qemu-system-x86_64]])
AC_ARG_VAR(QEMUFLAGS, [QEMU emulator flags [-m 2G -smp 4 -debugcon stdio]])

AC_CHECK_TOOL(ADAC, gcc, gcc)
test -z "$ADAFLAGS" && ADAFLAGS="-O2 -g"
AC_CHECK_TOOL(AS, as, as)
test -z "$ASFLAGS" && ASFLAGS="-O2 -g"
AC_CHECK_TOOL(LD, ld, ld)
test -z "$LDFLAGS" && LDFLAGS="-O2 -g"
AC_CHECK_TOOL(QEMU, qemu-system-x86_64, qemu-system-x86_64)
test -z "$QEMUFLAGS" && QEMUFLAGS="-m 2G -smp 4 -debugcon stdio"

# Replace the variables in the makefile.
CONFIG_DATE="$(date -R)"
AC_SUBST([PACKAGE_CONFIG_DATE], [$CONFIG_DATE])
AC_CONFIG_FILES(GNUmakefile config.ads)
AC_OUTPUT
