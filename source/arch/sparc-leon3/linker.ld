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

ENTRY(bootstrap)

PHDRS
{
    text    PT_LOAD    FLAGS((1 << 0) | (1 << 2)) ; /* Execute + Read  */
    rodata  PT_LOAD    FLAGS((1 << 2)) ;            /* Read only       */
    data    PT_LOAD    FLAGS((1 << 1) | (1 << 2)) ; /* Write + Read    */
}

SECTIONS {
    . = 16K;

    /DISCARD/ : {
        *libgnat.a:*(*)
    }

    /* Actual kernel. */
    .text : {
        *(.text .text.*)
    } :text

    .rodata : {
        *(.rodata .rodata.*)
    } :rodata

    .note : {
        *(.note.gnu.build-id)
    } : rodata

    .data : {
        *(.data .data.*)
    } :data

    .bss : {
        *(.bss .bss.*)
        *(COMMON)
    } :data

    /DISCARD/ : {
      *a:*(*)
      *o(*)
    }
}
