/*
 * linker.ld: Main linker script of the project.
 * Copyright (C) 2021 streaksu
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

ENTRY(start)

PHDRS
{
    text   PT_LOAD FLAGS((1 << 0) | (1 << 2)) ; /* Execute + Read  */
    rodata PT_LOAD FLAGS((1 << 2)) ;            /* Read only       */
    data   PT_LOAD FLAGS((1 << 1) | (1 << 2)) ; /* Write + Read    */
}

SECTIONS {
    VIRTUAL_BASE  = 0xFFFFFFFF80000000;
    PHYSICAL_BASE = 2M;
    . = VIRTUAL_BASE;

    /* Position in the front required by spec. */
    text_start = .;
    .multiboot2 : AT (ADDR (.multiboot2) - VIRTUAL_BASE + PHYSICAL_BASE) {
        KEEP(*(.multiboot2))
    } :text

    /* Actual kernel. */
    .text : AT (ADDR (.text) - VIRTUAL_BASE + PHYSICAL_BASE) {
        *(.text .text.*)
    } :text
    . = ALIGN(CONSTANT(MAXPAGESIZE));
    text_end = .;

    rodata_start = .;
    .rodata : AT (ADDR (.rodata) - VIRTUAL_BASE + PHYSICAL_BASE) {
        *(.rodata .rodata.*)
    } :rodata

    .note : AT (ADDR (.note) - VIRTUAL_BASE + PHYSICAL_BASE) {
        *(.note.gnu.build-id)
    } : rodata
    . = ALIGN(CONSTANT(MAXPAGESIZE));
    rodata_end = .;

    data_start = .;
    .data : AT (ADDR (.data) - VIRTUAL_BASE + PHYSICAL_BASE) {
        *(.data .data.*)
    } :data

    .bss : AT (ADDR (.bss) - VIRTUAL_BASE + PHYSICAL_BASE) {
        *(.bss .bss.*)
        *(COMMON)
    } :data
    . = ALIGN(CONSTANT(MAXPAGESIZE));
    data_end = .;

    /DISCARD/ : {
        *(.eh_frame)
        *(.note .note.*)
    }
}
