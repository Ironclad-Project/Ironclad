# arch-start.S: multiboot2 header and bootstrap.
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

# Account for the fb tag length.
#define MB2_LENGTH       36
#define MB2_MAGIC        0xE85250D6
#define MB2_ARCHITECTURE 0
#define MB2_CHECKSUM     -(MB2_MAGIC + MB2_ARCHITECTURE + MB2_LENGTH)

.section ".multiboot2", "aw", @progbits
.globl mb2hdr
mb2hdr:
    .long MB2_MAGIC        # Magic value.
    .long MB2_ARCHITECTURE # Architecture, 0 means i386.
    .long MB2_LENGTH       # Length of the header.
    .long MB2_CHECKSUM     # Checksum.
    # Framebuffer info.
    .short  5 # Type of tag.
    .short  0 # Flags.
    .long  20 # Size.
    .long   0 # Width,  0 means no preference.
    .long   0 # Height, 0 means no preference.
    .long   0 # Depth,  0 means no preference.

.section .text

# For adjusting a static address to the lower-half, physical address.
#define HIGHER_HALF   0xffff800000000000
#define KERNEL_REGION 0xffffffff80000000
#define ADJUST_ADDR(addr) addr - KERNEL_REGION + 0x200000

.globl start
.code32
start:
    # eax -> multiboot signature
    # ebx -> multiboot structure

    # Load the stack.
    mov ADJUST_ADDR($stack_top), %esp

    # Load our temporary GDT.
    lgdt ADJUST_ADDR(gdt_pointer)

    # enable PAE.
    mov %cr4, %ecx
    or $(1 << 5), %ecx
    mov %ecx, %cr4

    # Enable EFER.LME and NX
    mov $0xC0000080, %ecx
    rdmsr
    or $(1 << 8 | 1 << 11), %eax
    wrmsr

    # Enable our cool pages
    mov ADJUST_ADDR($pml4), %ecx
    mov %ecx, %cr3

    # Enable CR0.PG
    mov %cr0, %ecx
    or $(1 << 31), %ecx
    mov %ecx, %cr0

    # Reload segments.
    ljmp $0x28, ADJUST_ADDR($1f)

.code64
1:
    mov $0x30, %eax
    mov %eax, %ds
    mov %eax, %es
    mov %eax, %fs
    mov %eax, %gs
    mov %eax, %ss

    # Adjust stack, kump to the kernel.
    mov $stack_top, %rsp
    mov %rbx, %rdi
    mov $kernel_main, %rdx
    jmp *%rdx

.section .bss
.align 16
stack:
    .space 0x2000
stack_top:

.section .rodata
.align 0x1000
pml4:
    .quad ADJUST_ADDR(pml3) | 3
    .space 255 * 8
    .quad ADJUST_ADDR(pml3) | 3
    .space 254 * 8
    .quad ADJUST_ADDR(pml3_kernel) | 3

pml3:
    .quad ADJUST_ADDR(pml2) + 0x0003
    .quad ADJUST_ADDR(pml2) + 0x1003
    .quad ADJUST_ADDR(pml2) + 0x2003
    .quad ADJUST_ADDR(pml2) + 0x3003
    .space 508 * 8

pml3_kernel:
    .space 510 * 8
    .quad ADJUST_ADDR(pml2_kernel) | 3
    .space 1 * 8

pml2:
.set i,0
.rept 2048
    .quad (i * 0x200000) | 3 | (1 << 7)
    .set i,i+1
.endr

pml2_kernel:
.set i,0
.rept 512
    .quad (i * 0x200000 + 0x200000) | 3 | (1 << 7)
    .set i,i+1
.endr

.align 16

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

gdt_pointer:
    .word (gdt_end - gdt) - 1
    .quad ADJUST_ADDR(gdt)

.section .note.GNU-stack, "", %progbits
