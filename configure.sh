#!/bin/sh
# configure.sh: Configure script.
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

print_help() {
   echo "Ironclad's configure script"
   echo "Available options:"
   echo "--skip-menu    Skip the configure menu and use sensible defaults"
   echo "-h|--help      This help message"
   exit 0
}

# Find the source root.
script_name="$(basename "$0")"
script_dir="$(dirname "$0")"
if [ "$script_dir" = "." ] || [ -z "$script_dir" ]; then
   if echo "$0" | grep "/" >/dev/null 2>&1; then
      script_dir=.
   else
      script_dir="$(dirname $(which "${script_name}"))"
   fi
fi
script_dir="$(cd "${script_dir}" && pwd -P)"

# Parse command line.
for i in "$@"; do
   case "$i" in
      --skip-menu) skip_menu="yes";;
      -h|--help)   print_help;;
   esac
done

# Defaults for all options ever.
ARCH="x86_64-multiboot2"
X86_64_BOOTFB="True"
X86_64_PS2="True"
X86_64_RTC="True"
X86_64_ATA="True"
X86_64_SERIAL="True"
DEVICES_STREAMS="True"
DEVICES_RNG="True"
DEVICES_PTY="True"
MEMORY_ALLOCONLY="standard"

# Check whether we have to skip the menu or not.
if ! [ "$skip_menu" = "yes" ]; then
   main_choice_exit="0"
   geton() {
      if [ $1 = $2 ]; then echo "on"; else echo "off"; fi
   }
   while [ "$main_choice_exit" = "0" ]; do
      main_choice=$(dialog --stdout --ok-label "Next" --cancel-label "Done" \
         --menu "Options:" 0 0 0                \
         1  "Architectural/aarch64-stivale2"    \
         2  "Architectural/sparc-leon3"         \
         3  "Architectural/x86_64-multiboot2"   \
         4  "Cryptography"                      \
         5  "Architecture-independent devices"  \
         6  "Memory management"                 \
         7  "Networking"                        \
         8  "Userland options"                  \
         9  "VFS Options")
      main_choice_exit="$?"

      case "$main_choice" in
      3)
         # arch/x86_64-multiboot2
         x86_64_choices=$(dialog --stdout --separate-output \
            --ok-label "Save" --cancel-label "Back" \
            --checklist "Architectural x86_64-multiboot2 Options:" 0 0 0          \
            1 "Boot protocol framebuffer support" $(geton $X86_64_BOOTFB "True")  \
            2 "PS2 Keyboard/Mouse support"        $(geton $X86_64_PS2    "True")  \
            3 "RTC (Real Time Clock) support"     $(geton $X86_64_RTC    "True")  \
            4 "ATA drive support"                 $(geton $X86_64_ATA    "True")
            5 "Serial (COM) support"              $(geton $X86_64_SERIAL "True"))
         X86_64_BOOTFB="False"
         X86_64_PS2="False"
         X86_64_RTC="False"
         X86_64_ATA="False"
         X86_64_SERIAL="False"
         for c in $x86_64_choices; do case "$c" in
            1) X86_64_BOOTFB="True" ;;
            2) X86_64_PS2="True"    ;;
            3) X86_64_RTC="True"    ;;
            4) X86_64_ATA="True"    ;;
            5) X86_64_SERIAL="True" ;;
         esac; done
         ;;
      5)
         # devices
         devices_choices=$(dialog --stdout --separate-output \
            --ok-label "Save" --cancel-label "Back" \
            --checklist "Devices Options:" 0 0 0 \
            1 "Stream device support (zero, null)"    $(geton $DEVICES_STREAMS "True") \
            2 "RNG streams support (random, urandom)" $(geton $DEVICES_RNG     "True") \
            3 "PTY (Pseudo-terminal) support"         $(geton $DEVICES_PTY     "True"))
         DEVICES_STREAMS="False"
         DEVICES_RNG="False"
         DEVICES_PTY="False"
         for c in $devices_choices; do case "$c" in
            1) DEVICES_STREAMS="True" ;;
            2) DEVICES_RNG="True"     ;;
            3) DEVICES_PTY="True"     ;;
         esac; done
         ;;
      6)
         # memory
         memory_choices=$(dialog --stdout --separate-output \
            --ok-label "Save" --cancel-label "Back" \
            --checklist "Memory Options:" 0 0 0 \
            1 "Use alloc-only instead of standard" $(geton $MEMORY_ALLOCONLY "alloconly"))
         MEMORY_ALLOCONLY="standard"
         for c in $memory_choices; do case "$c" in
            1) MEMORY_ALLOCONLY="alloconly" ;;
         esac; done
         ;;
      esac
   done

   set -e
   stty sane
   clear
fi

# Create the final gprbuild and sed out the values.
cp "$script_dir"/ironclad.gpr.in ironclad.gpr
replace_in_file() {
   sed "s|@@$1@@|$2|g" < ironclad.gpr > ironclad.gpr.tmp
   mv ironclad.gpr.tmp ironclad.gpr
}

replace_in_file SRC              $script_dir
replace_in_file ARCH             $ARCH
replace_in_file X86_64_BOOTFB    $X86_64_BOOTFB
replace_in_file X86_64_PS2       $X86_64_PS2
replace_in_file X86_64_RTC       $X86_64_RTC
replace_in_file X86_64_ATA       $X86_64_ATA
replace_in_file X86_64_SERIAL    $X86_64_SERIAL
replace_in_file DEVICES_STREAMS  $DEVICES_STREAMS
replace_in_file DEVICES_RNG      $DEVICES_RNG
replace_in_file DEVICES_PTY      $DEVICES_PTY
replace_in_file MEMORY_ALLOCONLY $MEMORY_ALLOCONLY

############## WARNING: DRAGONS LIE AHEAD ######################
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
