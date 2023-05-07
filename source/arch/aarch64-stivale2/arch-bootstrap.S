# header.S: Stivale2 header.
# Copyright (C) 2023 streaksu
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

.section ".stivale2hdr", "aw", @progbits
.globl stivale2hdr
stivale2hdr:
    .quad entrypoint_main // Alternative entrypoint, 0 is none.
    .quad stack_top       // Stack to be loaded for the kernel.
    .quad 0               // Flags to request, we need nothing.
    .quad smp_tag         // Start of tags.

.section .data

smp_tag:
    .quad 0x1ab015085f3273df // Identifier of the tag.
    .quad 0                  // Next one in line, 0 is none.
    .quad 0                  // Flags, we dont need anything in particular.

.section .bss
.align 16

stack:
    .space 32768
stack_top:

.section .text
.global entrypoint_main
entrypoint_main:
    // Disable interrupts.
    msr daifset, #0xf

    // Load the vector table.
    ldr x1, =execution_vectors
    msr vbar_el1, x1

    // Load the stack.
    msr spsel, #0
    ldr x1, =stack_top
    mov sp, x1

    // Jump to the kernel.
    b kernel_main

// Vector table, values in x0 match with arch-interrupts.ads
.balign 0x800
execution_vectors:
    // Current EL with SP0 handlers.
    .balign 0x80; b common_handler; // Synchronous.
    .balign 0x80; b common_handler; // IRQ.
    .balign 0x80; b common_handler; // FIQ.
    .balign 0x80; b common_handler; // SError.

    // Current EL with SPx handlers.
    .balign 0x80; b common_handler; // Synchronous.
    .balign 0x80; b common_handler; // IRQ.
    .balign 0x80; b common_handler; // FIQ.
    .balign 0x80; b common_handler; // SError.

    // Lower EL using AArch64.
    .balign 0x80; b common_handler; // Synchronous.
    .balign 0x80; b common_handler; // IRQ.
    .balign 0x80; b common_handler; // FIQ.
    .balign 0x80; b common_handler; // SError.

    // Lower EL using AArch32.
    .balign 0x80; b common_handler; // Synchronous.
    .balign 0x80; b common_handler; // IRQ.
    .balign 0x80; b common_handler; // FIQ.
    .balign 0x80; b common_handler; // SError.

common_handler:
    // Load the kernel stack back.
    ldr x1, =stack_top
    mov sp, x1

    // Save state
    stp x1,  x0,  [sp, #-0x10]!
    stp x3,  x2,  [sp, #-0x10]!
    stp x5,  x4,  [sp, #-0x10]!
    stp x7,  x6,  [sp, #-0x10]!
    stp x9,  x8,  [sp, #-0x10]!
    stp x11, x10, [sp, #-0x10]!
    stp x13, x12, [sp, #-0x10]!
    stp x15, x14, [sp, #-0x10]!
    stp x17, x16, [sp, #-0x10]!
    stp x19, x18, [sp, #-0x10]!
    stp x21, x20, [sp, #-0x10]!
    stp x23, x22, [sp, #-0x10]!
    stp x25, x24, [sp, #-0x10]!
    stp x27, x26, [sp, #-0x10]!
    stp x29, x28, [sp, #-0x10]!
    str x30,      [sp, #-0x08]!
    mrs x0, elr_el1
    mrs x1, spsr_el1
    stp x1, x0,   [sp, #-0x08]!

    // Jump to the kernel handler.
    mov x0, sp
    b exception_handler

    // Restore state.
    ldp x0,  x1,  [SP], 0x10
    msr spsr_el1, x0
    msr elr_el1,  x1
    ldr x30,      [sp], 0x08
    ldp x29, x28, [sp], 0x10
    ldp x27, x26, [sp], 0x10
    ldp x25, x24, [sp], 0x10
    ldp x23, x22, [sp], 0x10
    ldp x21, x20, [sp], 0x10
    ldp x19, x18, [sp], 0x10
    ldp x17, x16, [sp], 0x10
    ldp x15, x14, [sp], 0x10
    ldp x13, x12, [sp], 0x10
    ldp x11, x10, [sp], 0x10
    ldp x9,  x8,  [sp], 0x10
    ldp x7,  x6,  [sp], 0x10
    ldp x5,  x4,  [sp], 0x10
    ldp x3,  x2,  [sp], 0x10
    ldp x1,  x0,  [sp], 0x10

    // Return.
    eret
