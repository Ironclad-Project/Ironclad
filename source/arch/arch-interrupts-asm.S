//  arch-interrupts-asm.S: ASM part of the handlers.
//  Copyright (C) 2021 streaksu
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#define PUSHAM \
    cld;       \
    push %rax; \
    push %rbx; \
    push %rcx; \
    push %rdx; \
    push %rsi; \
    push %rdi; \
    push %rbp; \
    push %r8;  \
    push %r9;  \
    push %r10; \
    push %r11; \
    push %r12; \
    push %r13; \
    push %r14; \
    push %r15

#define POPAM \
    pop %r15; \
    pop %r14; \
    pop %r13; \
    pop %r12; \
    pop %r11; \
    pop %r10; \
    pop %r9;  \
    pop %r8;  \
    pop %rbp; \
    pop %rdi; \
    pop %rsi; \
    pop %rdx; \
    pop %rcx; \
    pop %rbx; \
    pop %rax

.section .text

.global default_isr_handler
default_isr_handler:
    PUSHAM;
    call default_isr_body;
    POPAM;
    iretq;
