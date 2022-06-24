/*
 * arch-bootstrap.ld: Bootstrap code of the port.
 * Copyright (C) 2021 streaksu
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

.section .bss
.align 16
// Stack for the bootstrap.
stack_bottom:
    .space 32768
stack_top:

.section .text
.global bootstrap_main
bootstrap_main:
    // Clear BSS, the symbols are declared in the linker file.
    ldr x5, =bss_start
    ldr x6, =bss_end
1:
    str	xzr, [x5], #8
    cmp	x5, x6
    b.lo 1b

    // Load some goodies and jump to the Ada kernel.
    ldr x1, =stack_top
    mov sp, x1
    // bl kernel_main

    // In case it returns, lock up.
    wfi
2:
    b 2b
