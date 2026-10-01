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
AC_PREREQ([2.63])
AC_INIT([Ironclad], [m4_esyscmd([./version.sh])], [https://ironclad-os.org/contributing.html], [ironclad], [https://ironclad-os.org/])
###############################################################################
# General project options.
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])

SRCDIR="$(cd "$srcdir" && pwd -P)"
AC_SUBST([SRCDIR])

# The build directory ends up in debug information just like the source one,
# and is mapped away in the project files, to keep builds path independent.
BUILDDIR="$(pwd -P)"
AC_SUBST([BUILDDIR])

# The source path is re-evaluated by config.status and names make
# prerequisites, while the build path reaches the project files' string
# literals and gprconfig's comma separated command line. '&' otherwise yields
# an empty GNUmakefile with configure still exiting 0.
AC_MSG_CHECKING([whether the source and build directory paths are usable])
for bad_char in '"' "'" ':' '`' '#' '$' '&' ';' '|'; do
    AS_CASE(["$SRCDIR"],
        [*"$bad_char"*],
        [AC_MSG_RESULT([no])
         AC_MSG_ERROR([the source directory path contains '$bad_char'; move the source tree])])
done

for bad_char in '"' "'" ':' '&' ','; do
    AS_CASE(["$BUILDDIR"],
        [*"$bad_char"*],
        [AC_MSG_RESULT([no])
         AC_MSG_ERROR([the build directory path contains '$bad_char'; use a different build directory])])
done

# gprbuild rejects either inside a project file's string literal.
AS_CASE(["$SRCDIR$BUILDDIR"],
    [*"$(printf '\t')"*],
    [AC_MSG_RESULT([no])
     AC_MSG_ERROR([the source or build directory path contains a tab; move the tree])])

AS_IF([test "$(printf '%s' "$SRCDIR$BUILDDIR" | wc -l)" -gt 0],
    [AC_MSG_RESULT([no])
     AC_MSG_ERROR([the source or build directory path contains a newline; move the tree])])
AC_MSG_RESULT([yes])

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
                        return 1
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
AS_IF([test "x$werror_state" = "xyes"],
    [WERROR_FLAG="-Werror"],
    [WERROR_FLAG="-Wno-error"])
AC_SUBST([WERROR_FLAG])

AC_PROG_MKDIR_P
MKDIR_P="$(rel2abs "$MKDIR_P")"
AC_PROG_INSTALL
INSTALL="$(rel2abs "$INSTALL")"
AC_PROG_GREP
GREP="$(rel2abs "$GREP")"
AC_PROG_EGREP
EGREP="$(rel2abs "$EGREP")"
AC_PROG_SED
SED="$(rel2abs "$SED")"

AC_CHECK_PROG([FIND_FOUND], [find], [yes])
AS_IF([test "x$FIND_FOUND" != "xyes"],
    [AC_MSG_ERROR([find not found, please install find before configuring])])

AX_CHECK_GNU_MAKE([],
    [AC_MSG_ERROR([GNU make not found, please install GNU make 3.78.1 or later before configuring])])

# GNU make before 3.80 printed "GNU Make version X, by ..." rather than
# "GNU Make X", so take the version from either form.
gnu_make_version="$($_cv_gnu_make_command --version 2>/dev/null | $SED -n -e 's/^GNU Make version /GNU Make /' -e 's/^GNU Make \([[0-9]][[0-9.]]*\).*/\1/p' | $SED 1q)"
AC_MSG_CHECKING([whether $_cv_gnu_make_command is 3.78.1 or later])
AS_VERSION_COMPARE([$gnu_make_version], [3.78.1],
    [AC_MSG_RESULT([no])
     AC_MSG_ERROR([$_cv_gnu_make_command is GNU make $gnu_make_version, but 3.78.1 or later is required; set MAKE to a newer one])],
    [AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([yes])])

# $1 - UPPERCASEVAR, $2 - default command, $3 - 'tool' if toolchain, $4 - 'no-err' to ignore errors
AC_DEFUN([GET_PROG], [
    if test "x${$1+set}" = "xset"; then
        first_elem="$(rel2abs $(printf '%s\n' "$$1" | cut -f 1 -d " "))"
        case "$first_elem" in
            "")
                first_elem="$$1"
                $1_FOUND=no
                ;;
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
                first_elem="$(rel2abs $(printf '%s\n' "$2" | cut -f 1 -d " "))"
                if test -z "$first_elem"; then
                    first_elem="$2"
                    $1_FOUND=no
                else
                    AC_CHECK_PROG([$1_FOUND], [$first_elem], [yes])
                fi
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

# gprconfig searches PATH for the toolchain, which may not be the one CC points
# to, and the driver would then come from CC and the runtime from elsewhere.
# Point it at CC's directory when a GNAT lives there, else leave it to PATH, as
# is the case for compiler caches and other things that are not toolchains.
cc_cmd="$(printf '%s\n' "$CC" | cut -f 1 -d " ")"
cc_dir="$(dirname "$(command -v "$cc_cmd")")"
CC_DIR=""
for cc_gnatls in "$cc_dir"/*gnatls; do
    AS_IF([test -f "$cc_gnatls" && test -x "$cc_gnatls"],
        [CC_DIR="$cc_dir"])
done
AC_MSG_CHECKING([for a GNAT installation next to $cc_cmd])
AC_MSG_RESULT([${CC_DIR:-no, leaving the search to PATH}])
AC_SUBST([CC_DIR])
AC_ARG_VAR([STRIP], [strip command])
GET_PROG([STRIP], [strip], [tool])

AC_MSG_CHECKING([whether $CC is GCC 12 or later])
GCC_VERSION="$($CC -dumpversion)"
AS_VERSION_COMPARE([$GCC_VERSION], [12],
    [AC_MSG_RESULT([no])
     AC_MSG_ERROR([GCC version 12 or greater is required to build Ironclad, $GCC_VERSION found])],
    [AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([yes])])

m4_define([DEFAULT_CFLAGS], [-g -O2 -pipe])
AC_ARG_VAR([CFLAGS], [C compiler flags @<:@default: ]DEFAULT_CFLAGS[@:>@])
: "${CFLAGS=DEFAULT_CFLAGS}"

AC_ARG_VAR([ADAFLAGS], [Ada flags @<:@default: same as CFLAGS@:>@])
: "${ADAFLAGS=$CFLAGS}"

AC_ARG_VAR([ASFLAGS], [assembler flags @<:@default: same as CFLAGS@:>@])
: "${ASFLAGS=$CFLAGS}"

m4_define([DEFAULT_LDFLAGS], [])
AC_ARG_VAR([LDFLAGS], [linker flags @<:@default: ]DEFAULT_LDFLAGS[@:>@])
: "${LDFLAGS=DEFAULT_LDFLAGS}"

# Check if the linker supports -no-pie by searching its help, as GNU ld before
# 2.37 either rejects it or takes it for -n -o -pie. Help lines name a flag
# with one dash or two, and may follow it with a comma, an "=" or a "[" for
# its value.
AC_MSG_CHECKING([whether the linker accepts -no-pie])
AS_IF([LC_ALL=C $CC $LDFLAGS -Wl,--help 2>/dev/null | $EGREP -q '(^|[[[:space:]]])-?-no-pie([[[:space:],=@<:@]]|$)'],
    [AC_MSG_RESULT([yes])
     LD_HAS_NO_PIE="true"],
    [AC_MSG_RESULT([no])
     LD_HAS_NO_PIE="false"])
AC_SUBST([LD_HAS_NO_PIE])
###############################################################################
# A ton of tools use source date epoch in order to not use date in the effort
# of doing reproducible builds.
# This command uses git, but thats no biggie because there is a fallback if
# no git is present and it is meant to only be run when regenerating, so
# tarball users will (ideally) never see this.
SOURCE_DATE_EPOCH="m4_esyscmd([if git log -1 >/dev/null 2>&1; then git -c log.showSignature=false log -1 --pretty=%ct | tr -d '\n'; else printf 1546300800; fi])"
AC_SUBST([SOURCE_DATE_EPOCH])
# The same instant in the form touch(1) takes, for the release tarball.
SOURCE_DATE_EPOCH_TOUCH="m4_esyscmd([if git log -1 >/dev/null 2>&1; then TZ=UTC0 git -c log.showSignature=false log -1 --pretty=%cd --date='format-local:%Y%m%d%H%M' | tr -d '\n'; else printf 201901010000; fi])"
AC_SUBST([SOURCE_DATE_EPOCH_TOUCH])
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

# gprbuild does not detect compilers itself, it runs gprconfig, which several
# distributions package separately. It is looked for next to gprbuild first and
# in PATH second, so check both, else the build dies halfway through with a
# cryptic 'could not locate gprconfig for auto-configuration'.
AC_ARG_VAR([GPRCONFIG], [gprconfig command])
AS_IF([test "x${GPRCONFIG+set}" != "xset"], [
    gprbuild_cmd="$(printf '%s\n' "$GPRBUILD" | cut -f 1 -d " ")"
    gprconfig_path="$(dirname "$(command -v "$gprbuild_cmd")")/gprconfig"
    AS_IF([test -f "$gprconfig_path" && test -x "$gprconfig_path"],
        [GPRCONFIG="$gprconfig_path"])
])
GET_PROG([GPRCONFIG], [gprconfig])
AC_ARG_VAR([GNATPROVE], [gnatprove command])
GET_PROG([GNATPROVE], [gnatprove], [], [no-err])

# $1 - UPPERCASEVAR, $2 - default program
AC_DEFUN([GET_TOOL_FROM_GPRBUILD], [
    AC_ARG_VAR([$1], [$2 executable path])

    AC_MSG_CHECKING([for $2])
    gprbuild_path="$(command -v "$(printf '%s\n' "$GPRBUILD" | cut -f 1 -d " ")")"

    if test "x$$1" = "x"; then
        $1="$(dirname "$gprbuild_path")"/../libexec/gprbuild/$2
        if ! ( test -f "$$1" && test -x "$$1"); then
            $1="$(dirname "$gprbuild_path")"/../lib/gprbuild/$2
        fi
        if ! ( test -f "$$1" && test -x "$$1"); then
            AC_MSG_RESULT([no])
            AC_MSG_ERROR([No recorded path to $2 valid])
        fi
    else
        if ! ( test -f "$$1" && test -x "$$1"); then
            AC_MSG_RESULT([no])
            AC_MSG_ERROR([Provided $2 path not valid])
        fi
    fi
    AC_MSG_RESULT([$$1])
])

GET_TOOL_FROM_GPRBUILD([GPRBIND], [gprbind])
GET_TOOL_FROM_GPRBUILD([GPRLIB], [gprlib])
###############################################################################
# Platform acquisition and checking.
AC_ARG_VAR([PLATFORM], [target platform to build Ironclad for @<:@default: limine@:>@])
: "${PLATFORM:=limine}"

AC_MSG_CHECKING([whether $PLATFORM is a valid target platform])
AS_CASE([$PLATFORM],
    [limine], [AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])
     AC_MSG_ERROR([$PLATFORM is not a valid target platform])])

# Check if the platform supports the architecture, as the build would
# otherwise only fail once it is under way.
AC_MSG_CHECKING([whether the $PLATFORM platform supports $host_cpu])
AS_CASE([$host_cpu-$PLATFORM],
    [riscv64-limine|x86_64-limine], [AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])
     AC_MSG_ERROR([$host_cpu is not a supported architecture for the $PLATFORM platform])])

###############################################################################
# Finalize the settings and output the files.
AC_PREFIX_DEFAULT([/usr/local])
AC_CONFIG_FILES([GNUmakefile ironclad.gpr rts.gpr gprconfig/compilers.xml gprconfig/gprconfig.xml])
AC_OUTPUT
