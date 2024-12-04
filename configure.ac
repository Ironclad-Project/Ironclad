#  configure.ac: Configure template of the project.
#  Copyright (C) 2023 streaksu
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Project initialization.
AC_INIT([Ironclad], [m4_esyscmd([./version.sh])], [https://ironclad.nongnu.org/usersupport.html], [ironclad])
AC_PREREQ([2.69])
###############################################################################
# General project options.
AC_CONFIG_AUX_DIR([build-aux])

SRCDIR="$(cd "$srcdir" && pwd -P)"
BUILDDIR="$(pwd -P)"
AC_SUBST([SRCDIR])
AC_SUBST([BUILDDIR])

werror_state="no"
AC_ARG_ENABLE([werror],
    [AS_HELP_STRING([--enable-werror], [treat warnings as errors])],
    [werror_state="$enableval"])

MEMORY_ALLOCONLY="False"
AC_ARG_ENABLE([alloconly],
    [AS_HELP_STRING([--enable-alloconly], [Use the alloconly allocator instead of standard])],
    [test "$enableval" = "yes" && MEMORY_ALLOCONLY="True"])
AC_SUBST([MEMORY_ALLOCONLY])

m4_define([DEFAULT_ADAFLAGS], [-g -O2 -pipe -gnaty -Wall -Wextra])
AC_ARG_VAR([ADAFLAGS], [Ada flags @<:@default: ]DEFAULT_ADAFLAGS[@:>@])
test "x${ADAFLAGS+set}" = "x" && ADAFLAGS="DEFAULT_ADAFLAGS"
if test "$werror_state" = "yes"; then
    ADAFLAGS="$ADAFLAGS -Werror"
fi

m4_define([DEFAULT_ASFLAGS], [-g -O2 -pipe -Wall])
AC_ARG_VAR([ASFLAGS], [Assembler flags @<:@default: ]DEFAULT_ASFLAGS[@:>@])
test "x${ASFLAGS+set}" = "x" && ASFLAGS="DEFAULT_ASFLAGS"
if test "$werror_state" = "yes"; then
    ASFLAGS="$ASFLAGS -Werror"
fi

m4_define([DEFAULT_LDFLAGS], [])
AC_ARG_VAR([LDFLAGS], [Linker flags @<:@default: ]DEFAULT_LDFLAGS[@:>@])
test "x${LDFLAGS+set}" = "x" && LDFLAGS="DEFAULT_LDFLAGS"
###############################################################################
# A ton of tools use source date epoch in order to not use date in the effort
# of doing reproducible builds.
# This command uses git, but thats no biggie because there is a fallback if
# no git is present and it is meant to only be run when regenerating, so
# tarball users will (ideally) never see this.
SOURCE_DATE_EPOCH="m4_esyscmd([if git log -1 >/dev/null 2>&1; then git log -1 --pretty=%ct | tr -d '\n'; else printf 1546300800; fi])"
AC_SUBST([SOURCE_DATE_EPOCH])
###############################################################################
# Get programs and put them in comfortable variables.

# Convert relative paths starting in '.' into absolute paths.
AC_DEFUN([PROG_ABSPATH], [
    case "$$1" in
        .*) $1="$(cd "$(dirname "$$1")" && pwd -P)/$(basename "$$1")" ;;
    esac
])

# $1 - UPERCASEVAR, $2 - default program
AC_DEFUN([GET_PROG_FROM_TOOLCHAIN], [
    AC_ARG_VAR([$1], [$2 command])

    if ! test -z "$$1"; then
        if test -x "$$1"; then
            $1_USER_FOUND=yes
        else
            AC_CHECK_PROG([$1_USER_FOUND], [$$1], [yes])
        fi

        if ! test "x$$1_USER_FOUND" = "xyes"; then
            AC_MSG_ERROR([$$1 not found, please install $$1 before configuring])
        fi
    else
        $1="$($CC -dumpmachine)"-$2
        AC_CHECK_PROG([$1_FOUND], [$$1], [yes])
        if ! test "x$$1_FOUND" = "xyes"; then
            $1=$2
            AC_CHECK_PROG([$1_DEFAULT_FOUND], [$$1], [yes])
            if ! test "x$$1_DEFAULT_FOUND" = "xyes"; then
                AC_MSG_ERROR([$$1 not found, please install $$1 before configuring])
            fi
        fi
    fi

    AC_SUBST([$1])
])

# $1 - UPERCASEVAR, $2 - default program, $3 - nice name
m4_define([GET_PROG], [
    m4_define([DEFAULT_$1], [$2])
    AC_ARG_VAR([$1], [$3 executable @<:@default: ]DEFAULT_$1[@:>@])
    test "x$$1" = "x" && $1="DEFAULT_$1"
    AC_CHECK_PROG([$1_FOUND], [$$1], [$$1])
    if ! test "x$$1_FOUND" = "x$$1"; then
        AC_MSG_WARN([$3 not found, please install $3 or set $1 to a valid executable])
    else
        PROG_ABSPATH([$1])
    fi
])

AC_PROG_MKDIR_P
PROG_ABSPATH([MKDIR_P])
AC_PROG_INSTALL
PROG_ABSPATH([INSTALL])

AC_PROG_AR
PROG_ABSPATH([AR])
AC_PROG_RANLIB
PROG_ABSPATH([RANLIB])

GET_PROG([PERL], [perl], [perl])
GET_PROG([MAKEINFO], [makeinfo], [makeinfo])
GET_PROG([TEXI2DVI], [texi2dvi], [texi2dvi])
GET_PROG([TEXI2HTML], [makeinfo --no-split --html], [texi2html])
GET_PROG([GPRBUILD], [gprbuild], [gprbuild])
GET_PROG([GNATPROVE], [gnatprove], [gnatprove])

AC_ARG_VAR([GPRLIB], [gprlib executable path])
if test "x$GPRLIB" = "x"; then
    GPRLIB=/usr/libexec/gprbuild/gprlib
    if ! test -x $GPRLIB; then
        GPRLIB=/usr/lib/gprbuild/gprlib
    fi
    if ! test -x $GPRLIB; then
        AC_MSG_ERROR([No recorded path to gprlib valid])
    fi
else
    if ! test -x $GPRLIB; then
        AC_MSG_ERROR([Provided gprlib path not valid])
    fi
fi
###############################################################################
# Target acquisition and checking.
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

case $target_cpu-$target_vendor in
    riscv64-limine|x86_64-limine)
        ;;
    *)
        AC_MSG_ERROR([$target_cpu-$target_vendor is not available for compilation])
        ;;
esac

AC_LANG([C])
AC_PROG_CC
PROG_ABSPATH([CC])

GET_PROG_FROM_TOOLCHAIN([STRIP], [strip])
GET_PROG_FROM_TOOLCHAIN([OBJDUMP], [objdump])

GPRBUILD_TARGET=$($CC -dumpmachine)
AC_SUBST([GPRBUILD_TARGET])
###############################################################################
# Prepare an RTS from the cooked, pretty sources in the kernel.
mkdir -p rtsdir
mkdir -p rtsdir/adainclude
mkdir -p rtsdir/adalib
cp "$SRCDIR"/source/rts/*.adb rtsdir/adainclude
cp "$SRCDIR"/source/rts/*.ads rtsdir/adainclude
cp "$SRCDIR"/source/rts/$target_cpu-$target_vendor/*.ads rtsdir/adainclude
###############################################################################
# Finalize the settings and output the files.
AC_PREFIX_DEFAULT([/usr/local])
AC_CONFIG_FILES([GNUmakefile ironclad.gpr rts.gpr gprconfig/compilers.xml gprconfig/linker.xml])
AC_OUTPUT
