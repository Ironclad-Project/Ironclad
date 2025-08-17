#  configure.ac: Configure template of the project.
#  Copyright (C) 2025 streaksu
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
AC_INIT([Ironclad], [m4_esyscmd([./version.sh])], [https://ironclad-os.org/contributing.html], [ironclad])
AC_PREREQ([2.69])
###############################################################################
# General project options.
AC_CONFIG_AUX_DIR([build-aux])

SRCDIR="$(cd "$srcdir" && pwd -P)"
AC_SUBST([SRCDIR])

AC_CANONICAL_HOST
case "$host_cpu" in
    amd64) host_cpu=x86_64 ;;
esac

# Portably convert relative paths into absolute paths.
rel2abs() {
    rel2abs_first=true
    for i in $1; do
        if test $rel2abs_first = true; then
            case "$i" in
                /*)
                    printf "%s" "$i"
                    ;;
                */*)
                    if test -d "$(dirname "$i")"; then
                        printf "%s" "$(cd "$(dirname "$i")" && pwd -P)/$(basename "$i")"
                    else
                        printf "false"
                    fi
                    ;;
                *)
                    printf "%s" "$i"
                    ;;
            esac
            rel2abs_first=false
        else
            printf " %s" "$i"
        fi
    done
    printf "\n"
}

werror_state="no"
AC_ARG_ENABLE([werror],
    [AS_HELP_STRING([--enable-werror], [treat warnings as errors])],
    [werror_state="$enableval"])
if test "$werror_state" = "yes"; then
    AC_SUBST([WERROR_FLAG], [-Werror])
else
    AC_SUBST([WERROR_FLAG], [-Wno-error])
fi

AC_PROG_MKDIR_P
MKDIR_P="$(rel2abs "$MKDIR_P")"
AC_PROG_INSTALL
INSTALL="$(rel2abs "$INSTALL")"
AC_PROG_GREP
GREP="$(rel2abs "$GREP")"
AC_PROG_SED
SED="$(rel2abs "$SED")"

AC_CHECK_PROG([FIND_FOUND], [find], [yes])
if ! test "x$FIND_FOUND" = "xyes"; then
    AC_MSG_ERROR([find not found, please install find before configuring])
fi

# $1 - UPPERCASEVAR, $2 - default command, $3 - 'tool' if toolchain, $4 - 'no-err' to ignore errors
AC_DEFUN([GET_PROG], [
    if test "x${$1+set}" = "xset"; then
        first_elem="$(rel2abs $(echo "$$1" | cut -f 1 -d " "))"
        case "$first_elem" in
            /*)
                AC_MSG_CHECKING([for $first_elem])
                if test -f "$first_elem" && test -x "$first_elem"; then
                    $1="$(rel2abs "$$1")"
                    $1_FOUND=yes
                else
                    $1_FOUND=no
                fi
                AC_MSG_RESULT([$$1_FOUND])
                ;;
            *)
                AC_CHECK_PROG([$1_FOUND], [$first_elem], [yes])
                ;;
        esac
    else
        if ! test -z "$2"; then
            if test "x$3" = "xtool"; then
                AC_CHECK_TOOL([$1_FOUND], [$2], [no])
                if ! test "x$$1_FOUND" = "xno"; then
                    $1="$(rel2abs "$$1_FOUND")"
                    $1_FOUND=yes
                fi
            else
                first_elem="$(rel2abs $(echo "$2" | cut -f 1 -d " "))"
                AC_CHECK_PROG([$1_FOUND], [$first_elem], [yes])
                if test "x$$1_FOUND" = "xyes"; then
                    $1="$(rel2abs "$2")"
                fi
            fi
        else
            $1_FOUND=no
        fi
    fi

    if ! test "x$$1_FOUND" = "xyes"; then
        if test "x$4" = "xno-err"; then
            AC_MSG_WARN([$first_elem invalid, set $1 to a valid program])
        else
            AC_MSG_ERROR([$first_elem invalid, set $1 to a valid program])
        fi
    fi
])

AC_ARG_VAR([AR], [archiver command])
GET_PROG([AR], [gcc-ar], [tool])
AC_ARG_VAR([CC], [C compiler command])
GET_PROG([CC], [gcc], [tool])
AC_ARG_VAR([STRIP], [strip command])
GET_PROG([STRIP], [strip], [tool])

GCC_VERSION="$($CC -dumpversion)"
if test "${GCC_VERSION%%.*}" -lt "14"; then
    AC_MSG_ERROR([GCC version 14 or greater is required to build Ironclad])
fi

m4_define([DEFAULT_CFLAGS], [-g -O2 -pipe])
AC_ARG_VAR([CFLAGS], [C compiler flags @<:@default: ]DEFAULT_CFLAGS[@:>@])
test "x${CFLAGS+set}" = "x" && CFLAGS="DEFAULT_CFLAGS"

AC_ARG_VAR([ADAFLAGS], [Ada flags @<:@default: same as CFLAGS@:>@])
test "x${ADAFLAGS+set}" = "x" && ADAFLAGS="$CFLAGS"

AC_ARG_VAR([ASFLAGS], [assembler flags @<:@default: same as CFLAGS@:>@])
test "x${ASFLAGS+set}" = "x" && ASFLAGS="$CFLAGS"

m4_define([DEFAULT_LDFLAGS], [])
AC_ARG_VAR([LDFLAGS], [linker flags @<:@default: ]DEFAULT_LDFLAGS[@:>@])
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

AC_ARG_VAR([MAKEINFO], [makeinfo command])
GET_PROG([MAKEINFO], [makeinfo])
AC_ARG_VAR([TEXI2DVI], [texi2dvi command])
GET_PROG([TEXI2DVI], [texi2dvi], [], [no-err])
AC_ARG_VAR([TEXI2HTML], [texi2html command])
GET_PROG([TEXI2HTML], [makeinfo --no-split --html], [], [no-err])
AC_ARG_VAR([GPRBUILD], [gprbuild command])
GET_PROG([GPRBUILD], [gprbuild])
AC_ARG_VAR([GNATPROVE], [gnatprove command])
GET_PROG([GNATPROVE], [gnatprove], [], [no-err])
AC_ARG_VAR([TYPOS], [typos command])
GET_PROG([TYPOS], [typos], [], [no-err])

# $1 - UPPERCASEVAR, $2 - default program
AC_DEFUN([GET_TOOL_FROM_GPRBUILD], [
    AC_ARG_VAR([$1], [$2 executable path])

    gprbuild_path="$(command -v "$(echo "$GPRBUILD" | cut -f 1 -d " ")")"

    if test "x$$1" = "x"; then
        $1="$(dirname "$gprbuild_path")"/../libexec/gprbuild/$2
        if ! ( test -f "$$1" && test -x "$$1"); then
            $1="$(dirname "$gprbuild_path")"/../lib/gprbuild/$2
        fi
        if ! ( test -f "$$1" && test -x "$$1"); then
            AC_MSG_ERROR([No recorded path to $2 valid])
        fi
    else
        if ! ( test -f "$$1" && test -x "$$1"); then
            AC_MSG_ERROR([Provided $2 path not valid])
        fi
    fi
])

GET_TOOL_FROM_GPRBUILD([GPRBIND], [gprbind])
GET_TOOL_FROM_GPRBUILD([GPRLIB], [gprlib])
###############################################################################
# Platform acquisition and checking.
AC_ARG_VAR([PLATFORM], [target platform to build Ironclad for @<:@default: limine@:>@])

if test "x$PLATFORM" = "x"; then
    PLATFORM=limine
fi

case $PLATFORM in
    limine)
        ;;
    *)
        AC_MSG_ERROR([$PLATFORM is not a valid target platform])
        ;;
esac

###############################################################################
# Finalize the settings and output the files.
AC_PREFIX_DEFAULT([/usr/local])
AC_CONFIG_FILES([GNUmakefile ironclad.gpr rts.gpr gprconfig/compilers.xml gprconfig/gprconfig.xml])
AC_OUTPUT
