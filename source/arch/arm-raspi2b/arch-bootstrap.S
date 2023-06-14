# arch-bootstrap.S: Bootstrap function.
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

.section ".text.boot"

.globl entrypoint_start
entrypoint_start:
    // r1 -> 0x00000C42 - machine id, r2 -> 0x00000100 - start of ATAGS

    // Shut off extra cores
    mrc p15, 0, r5, c0, c0, 5
    and r5, r5, #3
    cmp r5, #0
    bne 3f

    // Setup the stack.
    ldr r5, =stack_top
    mov sp, r5

    // Clear out bss.
    ldr r4, =__bss_start
    ldr r9, =__bss_end
    mov r5, #0
    mov r6, #0
    mov r7, #0
    mov r8, #0
    b       2f
1:
    stmia r4!, {r5-r8}
2:
    cmp r4, r9
    blo 1b

    // Move the exception table to 0.
    push    {r4, r5, r6, r7, r8, r9}
    ldr     r0, =exception_vectors
    mov     r1, #0x0000
    ldmia   r0!,{r2, r3, r4, r5, r6, r7, r8, r9}
    stmia   r1!,{r2, r3, r4, r5, r6, r7, r8, r9}
    ldmia   r0!,{r2, r3, r4, r5, r6, r7, r8}
    stmia   r1!,{r2, r3, r4, r5, r6, r7, r8}
    pop     {r4, r5, r6, r7, r8, r9}

    // Call kernel_main and thus enter Ada.
    ldr r3, =kernel_main
    blx r3

    // Halt to death.
3:
    wfe
    b 3b

.section .text
exception_vectors:
    ldr pc, reset_handler_abs_addr
    ldr pc, undefined_instruction_handler_abs_addr
    ldr pc, software_interrupt_handler_abs_addr
    ldr pc, prefetch_abort_handler_abs_addr
    ldr pc, data_abort_handler_abs_addr
    nop // Reserved
    ldr pc, irq_handler_abs_addr
    ldr pc, fast_irq_handler_abs_addr

reset_handler_abs_addr:                 .word reset_handler_entry
undefined_instruction_handler_abs_addr: .word ud_handler_entry
software_interrupt_handler_abs_addr:    .word swi_handler_entry
prefetch_abort_handler_abs_addr:        .word prefetch_handler
data_abort_handler_abs_addr:            .word data_handler
irq_handler_abs_addr:                   .word irq_handler
fast_irq_handler_abs_addr:              .word fiq_handler

.macro exception_handler callback
\callback\()_entry:
    sub     lr, lr, #4
    srsdb   sp!, #0x13
    cpsid   if, #0x13

    push {r0}
    push {r1}
    push {r2}
    push {r3}
    push {r4}
    push {r5}
    push {r6}
    push {r7}
    push {r8}
    push {r9}
    push {r10}
    push {r11}
    push {r12}
    push {r13}
    push {r14}
    push {r15}

    mov r0, sp
    bl \callback\()

    pop {r15}
    pop {r14}
    pop {r13}
    pop {r12}
    pop {r11}
    pop {r10}
    pop {r9}
    pop {r8}
    pop {r7}
    pop {r6}
    pop {r5}
    pop {r4}
    pop {r3}
    pop {r2}
    pop {r1}
    pop {r0}

    rfeia   sp!
.endm

exception_handler reset_handler
exception_handler ud_handler
exception_handler swi_handler
exception_handler prefetch_handler
exception_handler data_handler
exception_handler irq_handler
exception_handler fiq_handler

.section .bss
.align 16
stack:
    .space 0x10000
stack_top:
