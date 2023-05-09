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

AC_INIT([Ironclad], [m4_esyscmd([./version.sh])], [https://savannah.nongnu.org/bugs/?group=ironclad&func=additem], [ironclad])

AC_PREREQ([2.69])

AC_CONFIG_AUX_DIR([build-aux])

SRCDIR="$(cd "$srcdir" && pwd -P)"
BUILDDIR="$(pwd -P)"
AC_SUBST([SRCDIR])
AC_SUBST([BUILDDIR])

werror_state="no"
AC_ARG_ENABLE([werror],
    [AS_HELP_STRING([--enable-werror], [treat warnings as errors])],
    [werror_state="$enableval"])

SCHED_ASC="True"
AC_ARG_ENABLE([asc],
    [AS_HELP_STRING([--disable-asc], [Disable anti-starvation corrections (ASC)])],
    [test "$enableval" = "no" && SCHED_ASC="False"])
AC_SUBST([SCHED_ASC])

MEMORY_ALLOCONLY="standard"
AC_ARG_ENABLE([alloconly],
    [AS_HELP_STRING([--enable-alloconly], [Use the alloconly allocator instead of standard])],
    [test "$enableval" = "yes" && MEMORY_ALLOCONLY="alloconly"])
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

AC_PROG_MKDIR_P
AC_PROG_INSTALL

AC_CHECK_PROG([GPRBUILD_FOUND], [gprbuild], [yes])
if ! test "x$GPRBUILD_FOUND" = "xyes"; then
    AC_MSG_ERROR([gprbuild not found, please install gprbuild before configuring])
fi

AC_CHECK_PROG([GNATPROVE_FOUND], [gnatprove], [yes])
if ! test "x$GNATPROVE_FOUND" = "xyes"; then
    AC_MSG_WARN([gnatprove not found, formal proofing will not be available])
fi

AC_CHECK_PROG([MAKEINFO_FOUND], [makeinfo], [yes])
if ! test "x$MAKEINFO_FOUND" = "xyes"; then
    AC_MSG_WARN([makeinfo not found, generating documentation will not be available])
fi

AC_CANONICAL_TARGET

case $target_cpu-$target_vendor in
    aarch64-stivale2|sparc-leon3|x86_64-multiboot2)
        ;;
    *)
        AC_MSG_ERROR([The passed cpu-vendor is not available for compilation])
        ;;
esac

############## WARNING: DRAGONS LIE AHEAD ######################
#           ____ __
#          { --.\  |          .)%%%)%%
#           '-._\\ | (\___   %)%%(%%(%%%
#               `\\|{/ ^ _)-%(%%%%)%%;%%%
#           .'^^^^^^^  /`    %%)%%%%)%%%'
#          //\   ) ,  /       '%%%%(%%'
#    ,  _.'/  `\<-- \<
#     `^^^`     ^^   ^^
################################################################
# So you see, gprbuild loves linking with -lgnat-XX where XX is the system
# version, this is not always provided by the distributions.
# (because why would they). Since we need nothing from them apart of their
# existance and gprbuild in all of its wisdom will not let us remove the flag
# in any human way possible, we are just gonna generate every possible file
# from libgnat-1.a to libgnat-99.a. Thats 100 years of GCC.
mkdir -p libgnat-hack
touch libgnat-hack/libgnat.a
for i in $(seq 99); do
   touch libgnat-hack/libgnat-$i.a
done
################################################################

AC_PREFIX_DEFAULT([/usr/local])
AC_CONFIG_FILES([GNUmakefile ironclad.gpr])
AC_OUTPUT
