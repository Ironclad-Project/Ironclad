# lib-mem.S: mem* family runtime functions.
# Copyright (C) 2024 streaksu
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

.global memcpy
memcpy:
    mov %rdx, %rcx
    mov %rdi, %rax
    rep movsb
    ret

.global memset
memset:
    push %rdi
    mov %rsi, %rax
    mov %rdx, %rcx
    rep stosb
    pop %rax
    ret

.global memmove
memmove:
    mov %rdx, %rcx
    mov %rdi, %rax

    cmp %rsi, %rdi
    ja 0f

    rep movsb
    jmp 1f
0:
    lea -1(%rdi, %rcx), %rdi
    lea -1(%rsi, %rcx), %rsi
    std
    rep movsb
    cld
1:
    ret

.global memcmp
memcmp:
    mov %rdx, %rcx
    repe cmpsb
    je 2f

    mov -1(%rdi), %al
    sub -1(%rsi), %al
    movsx %al, %rax
    jmp 3f
2:
    xor %eax, %eax
3:
    ret

.section .note.GNU-stack, "", %progbits
