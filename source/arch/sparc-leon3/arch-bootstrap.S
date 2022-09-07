# arch-bootstrap.S: Setup a kernel environment.
# Copyright (C) 2022 streaksu
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

.section .text

.globl bootstrap
bootstrap:
    # Clear the BSS.
    set __bss_start, %g2
    set __bss_end,   %g3
    mov %g0, %g1 ! So std has two zeros
    sub %g3, %g2, %g3
0:
    subcc  %g3, 8, %g3
    bge,a  0b
    std    %g0, [%g2 + %g3]

    # Enable traps, else QEMU faults when a trap happens.
    mov %psr, %l4
    or  %l4, 32, %l4
    mov %l4, %psr

    # Jump to Ada using the preexisting stack.
    call kernel_main
    nop
