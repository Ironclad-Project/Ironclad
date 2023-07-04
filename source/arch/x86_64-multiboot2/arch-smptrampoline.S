# arch-smptrampoline.S: SMP trampoline.
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

# Code highly inspired from the limine bootloader code. Go give them a star.
# https://github.com/limine-bootloader/limine
# Distributed under the BSD 2-Clause license
# (https://opensource.org/licenses/BSD-2-Clause)

.code16
.section .rodata

.global smp_trampoline_start
smp_trampoline_start:
    cli
    cld

    // Get address where we are loaded.
    mov %cs, %ebx
    shl $4, %ebx

    data32 lidt %cs:(invalid_idt - smp_trampoline_start)
    add %ebx, %cs:(gdt_ptr + 2 - smp_trampoline_start)
    data32 lgdt %cs:(gdt_ptr - smp_trampoline_start)

    // Do a cool "indirect far jump".
    lea (2f - smp_trampoline_start)(%ebx), %eax
    mov %eax, %cs:(1f - smp_trampoline_start)

    // Jump to protected mode!
    mov $0x11, %eax
    mov %eax, %cr0
    data32 ljmp *%cs:(1f - smp_trampoline_start)

1:
    .long 0    // Offset
    .long 0x18 // Segment

.code32
2:
    mov $0x20, %eax // 32 bit GDT code segment.
    mov %eax, %ds
    mov %eax, %es
    mov %eax, %fs
    mov %eax, %gs
    mov %eax, %ss

    xor %eax, %eax
    lldt %ax
    mov %eax, %cr4

    lea (temp_stack_top - smp_trampoline_start)(%ebx), %esp

    // Enable PAE, LME, NX (NX may be used in the passed pagemap).
    mov %cr4, %eax
    bts $5, %eax
    mov %eax, %cr4
    mov $0xC0000080, %ecx
    mov $(1 << 8 | 1 << 11), %eax
    xor %edx, %edx
    wrmsr

    // XXX: This assumes the passed pagemaps are under 4GiB, may god have mercy
    // on us if they arent.
    mov (passed_info_pagemap - smp_trampoline_start)(%ebx), %eax
    mov %eax, %cr3

    // Enable paging.
    mov %cr0, %eax
    bts $31, %eax
    mov %eax, %cr0

    lea (3f - smp_trampoline_start)(%ebx), %eax
    push $0x28
    push %eax
    lretl

.code64
3:
    mov $0x30, %eax // 64 bit GDT code segment.
    mov %eax, %ds
    mov %eax, %es
    mov %eax, %fs
    mov %eax, %gs
    mov %eax, %ss

    // Load the proper stack, set the booted flag, and jump to Ada code
    mov (passed_info_final_stack - smp_trampoline_start)(%rbx), %rsp
    mov (passed_info_core_number - smp_trampoline_start)(%rbx), %rdi
    mov (passed_info_lapic_id    - smp_trampoline_start)(%rbx), %rsi
    mov $1, %eax
    lock xchg %rax, (passed_info_booted_flag - smp_trampoline_start)(%rbx)
    mov $init_core, %rax
    call *%rax

invalid_idt:
    .quad 0
    .quad 0

gdt_ptr:
    .short gdt_end - gdt - 1
    .quad gdt - smp_trampoline_start

gdt:
    .quad 0x0000000000000000 # null
    .quad 0x00009a000000ffff # 16-bit code
    .quad 0x000093000000ffff # 16-bit data
    .quad 0x00cf9a000000ffff # 32-bit code
    .quad 0x00cf93000000ffff # 32-bit data
    .quad 0x00af9b000000ffff # 64-bit code
    .quad 0x00af93000000ffff # 64-bit data
    .quad 0x00affb000000ffff # usermode 64-bit code
    .quad 0x00aff3000000ffff # usermode 64-bit data
gdt_end:

.align 16
temp_stack:
    .space 128
temp_stack_top:

passed_info_pagemap:
    .long 0
passed_info_final_stack:
    .quad 0
passed_info_core_number:
    .quad 0
passed_info_lapic_id:
    .quad 0
passed_info_booted_flag:
    .quad 0

smp_trampoline_end:

.global smp_trampoline_size
smp_trampoline_size:
    .quad smp_trampoline_end - smp_trampoline_start

.section .note.GNU-stack, "", %progbits
