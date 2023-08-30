# arch-syscall_handler.S: ASM for the syscall handler.
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

.global syscall_entry
syscall_entry:
    # Swap to kernel context and store kernel and user stacks.
    swapgs
    mov %rsp, %gs:16
    mov %gs:8, %rsp

    # These are here for simulating an average interrupt stack frame.
    push $0x3b
    push %gs:16
    push %r11
    push $0x43
    push %rcx
    push $0

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

    xor %rbp, %rbp
    mov %rsp, %rdi
    call syscall_handler

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

    # Reload context.
    mov %gs:16, %rsp

    # Swap context back and return.
    swapgs
    sysretq

.section .note.GNU-stack, "", %progbits
