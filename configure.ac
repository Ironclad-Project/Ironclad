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
test "x$ADAFLAGS" = "x" && ADAFLAGS="DEFAULT_ADAFLAGS"
if test "$werror_state" = "yes"; then
    ADAFLAGS="$ADAFLAGS -Werror"
fi

m4_define([DEFAULT_ASFLAGS], [-g -O2 -pipe -Wall])
AC_ARG_VAR([ASFLAGS], [Assembler flags @<:@default: ]DEFAULT_ASFLAGS[@:>@])
test "x$ASFLAGS" = "x" && ASFLAGS="DEFAULT_ASFLAGS"
if test "$werror_state" = "yes"; then
    ASFLAGS="$ASFLAGS -Werror"
fi

m4_define([DEFAULT_LDFLAGS], [-g -O2 -pipe -Wall])
AC_ARG_VAR([LDFLAGS], [Linker flags @<:@default: ]DEFAULT_LDFLAGS[@:>@])
test "x$LDFLAGS" = "x" && LDFLAGS="DEFAULT_LDFLAGS"
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

# $1 - UPERCASEVAR, $2 - default program, $3 - nice name
m4_define([GET_PROG], [
    m4_define([DEFAULT_$1], [$2])
    AC_ARG_VAR([$1], [$3 executable @<:@default: ]DEFAULT_$1[@:>@])
    test "x$$1" = "x" && $1="DEFAULT_$1"
    AC_CHECK_PROG([$1_FOUND], [$$1], [$$1])
    if ! test "x$$1_FOUND" = "x$$1"; then
      AC_MSG_WARN([$3 not found, please install $3 or set $1 to a valid executable])
    fi
])

AC_PROG_MKDIR_P
AC_PROG_INSTALL

GET_PROG([PERL], [perl], [perl])
GET_PROG([OBJDUMP], [objdump], [objdump])
GET_PROG([GZIP], [gzip], [gzip])
GET_PROG([STRIP], [strip], [strip])
GET_PROG([MAKEINFO], [makeinfo], [makeinfo])
GET_PROG([TEXI2DVI], [texi2dvi], [texi2dvi])
GET_PROG([TEXI2HTML], [makeinfo --no-split --html], [texi2html])
GET_PROG([GPRBUILD], [gprbuild], [gprbuild])
GET_PROG([GNATPROVE], [gnatprove], [gnatprove])
###############################################################################
# Target acquisition and checking.
AC_CANONICAL_TARGET

case $target_cpu-$target_vendor in
    riscv64-limine|x86_64-limine)
        ;;
    *)
        AC_MSG_ERROR([$target_cpu-$target_vendor is not available for compilation])
        ;;
esac
###############################################################################
# Prepare an RTS from the cooked, pretty sources in the kernel.
mkdir -p rtsdir
mkdir -p rtsdir/adainclude
mkdir -p rtsdir/adalib
cp "$SRCDIR"/source/rts/*.adb rtsdir/adainclude
cp "$SRCDIR"/source/rts/*.ads rtsdir/adainclude
cp "$SRCDIR"/source/rts/$target_cpu-$target_vendor/*.ads rtsdir/adainclude
############## WARNING: DRAGONS LIE AHEAD #####################################
#           ____ __
#          { --.\  |          .)%%%)%%
#           '-._\\ | (\___   %)%%(%%(%%%
#               `\\|{/ ^ _)-%(%%%%)%%;%%%
#           .'^^^^^^^  /`    %%)%%%%)%%%'
#          //\   ) ,  /       '%%%%(%%'
#    ,  _.'/  `\<-- \<
#     `^^^`     ^^   ^^
###############################################################################
# Some distros (looking at you, debian), despite we providing our RTS, will
# require for their gprbuild to link against -lgnat-XX where XX is the system
# version, this is not always provided by the distributions (sigh).
# Since we need nothing from them apart of their existance, we are just going
# to generate every possible file from libgnat-1.a to libgnat-99.a.
# Thats 100 years of GCC (Sorry Ironclad users in 3026).
mkdir -p libgnat-hack
touch libgnat-hack/libgnat.a
for i in $(seq 99); do
   touch libgnat-hack/libgnat-$i.a
done
###############################################################################
# Finalize the settings and output the files.
AC_PREFIX_DEFAULT([/usr/local])
AC_CONFIG_FILES([GNUmakefile ironclad.gpr rts.gpr])
AC_OUTPUT
