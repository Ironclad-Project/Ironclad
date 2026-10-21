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

    # Load state.
    push $0x3b
    push %gs:16
    push %r11
    push $0x43
    push %rcx
    pushq %gs:16
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
    cld
    # Keep (%rsp + 8) 16-byte aligned at the handler's entry, as the x86-64
    # psABI 1.0 wants.
    sub $8, %rsp
    call syscall_handler
    add $8, %rsp

    # Pop back state.
    mov 16(%rsp), %rcx
    cmp 128(%rsp), %rcx
    jne 1f
    mov 80(%rsp), %r11
    cmp 144(%rsp), %r11
    jne 1f

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
    pop %rcx
    add $8, %rsp
    pop %r11
    pop %rsp

    # Swap context back and return.
    swapgs
    sysretq

1:
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
    add $8, %rsp        # The error code, which a system call has none of.
    swapgs
    iretq
