# arch-interruptthunks.S: ASM thunks for handler callbacks.
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

.altmacro

.macro THUNK num
.global interrupt_thunk_\num
interrupt_thunk_\num:
# Push the values in the order of arch-interrupts.ads
# Account for the error codes on the relevant exceptions.
.if \num != 8 && \num != 10 && \num != 11 && \num != 12 && \num != 13 && \num != 14 && \num != 17 && \num != 30
    push $0
.endif
    # Check for the user 64 bit code selector in CS to swap our GS.
    cmpq $0x43, 16(%rsp)
    jne 1f
    swapgs
1:
    # Save state.
    push %r15
    push %r14
    push %r13
    push %r12
    push %r11
    push %r10
    push %r9
    push %r8
    push %rbp
    push %rdi
    push %rsi
    push %rdx
    push %rcx
    push %rbx
    push %rax

    # Calculate the index on the isr table and call the handler
    # using C calling convetion.
    cld
    mov $\num, %rdi
    mov $(\num * 8), %rax
    lea isr_table(%rip), %rbx
    add %rax, %rbx
    mov %rsp, %rsi
    xor %rbp, %rbp
    call *(%rbx)

    # Pop back state.
    pop %rax
    pop %rbx
    pop %rcx
    pop %rdx
    pop %rsi
    pop %rdi
    pop %rbp
    pop %r8
    pop %r9
    pop %r10
    pop %r11
    pop %r12
    pop %r13
    pop %r14
    pop %r15
    add $8, %rsp

    # Check for the user 64 bit code selector in CS to swap our GS.
    cmpq $0x43, 8(%rsp) # if user
    jne 1f
    swapgs
1:
    iretq
.endm

.macro thunkaddr num
    .quad interrupt_thunk_\num
.endm

.section .data

.global interrupt_thunk_list
interrupt_thunk_list:
.set i,0
.rept 256
    thunkaddr %i
    .set i,i+1
.endr

.section .text

.set i,0
.rept 256
    THUNK %i
    .set i,i+1
.endr

.section .note.GNU-stack, "", %progbits
