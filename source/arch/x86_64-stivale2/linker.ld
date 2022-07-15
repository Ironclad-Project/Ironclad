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

ENTRY(kernel_main)

PHDRS
{
    text    PT_LOAD    FLAGS((1 << 0) | (1 << 2)) ; /* Execute + Read  */
    rodata  PT_LOAD    FLAGS((1 << 2)) ;            /* Read only       */
    data    PT_LOAD    FLAGS((1 << 1) | (1 << 2)) ; /* Write + Read    */
    dynamic PT_DYNAMIC FLAGS((1 << 1) | (1 << 2)) ; /* Segment for PIE */
}

SECTIONS {
    . = 0xFFFFFFFF80000000 + 1M;

    /DISCARD/ : {
        *libgnat.a:*(*)
    }

    /* Actual kernel. */
    .text : {
        *(.text .text.*)
    } :text

    . += CONSTANT(MAXPAGESIZE);

    /* Name required by spec. */
    .stivale2hdr : {
        KEEP(*(.stivale2hdr))
    } :rodata

    .rodata : {
        *(.rodata .rodata.*)
    } :rodata

    .note : {
        *(.note.gnu.build-id)
    } : rodata

    . += CONSTANT(MAXPAGESIZE);

    .data : {
        *(.data .data.*)
    } :data

    .got : {
        *(.got .got.*)
    } :data

    .rela : {
        *(.rela .rela.*)
    } :data

    .dynamic : {
        *(.dynamic)
    } :data :dynamic

    .bss : {
        *(.bss .bss.*)
        *(COMMON)
    } :data

    /DISCARD/ : {
      *a:*(*)
      *o(*)
    }
}
