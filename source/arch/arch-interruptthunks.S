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
    push %rax
    push %rbx
    push %rcx
    push %rdx
    push %rsi
    push %rdi
    push %rbp
    push %r8
    push %r9
    push %r10
    push %r11
    push %r12
    push %r13
    push %r14
    push %r15

    # XXX: Restore segment registers since the limine terminal couldve
    # change them. Remove this if we were to move from it.
    mov %es, %eax
    push %rax
    mov %ds, %eax
    push %rax
    mov $0x30, %eax
    mov %eax, %ds
    mov %eax, %es
    mov %eax, %ss
    #

    cld
    mov $(\num * 8), %rax
    lea isr_table(%rip), %rbx
    add %rax, %rbx
    call *(%rbx)

    # Second part of above, restore to what they were.
    pop %rax
    mov %eax, %ds
    pop %rax
    mov %eax, %es
    #

    pop %r15
    pop %r14
    pop %r13
    pop %r12
    pop %r11
    pop %r10
    pop %r9
    pop %r8
    pop %rbp
    pop %rdi
    pop %rsi
    pop %rdx
    pop %rcx
    pop %rbx
    pop %rax
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
