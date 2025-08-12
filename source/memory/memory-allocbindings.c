//  memory-allocbindings.c: Physical memory allocator bindings for generation.
//  Copyright (C) 2025 streaksu
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

#include <stddef.h>

extern void internal_alloc(size_t size, void **addr);
extern void internal_free(void *addr);

void *__gnat_malloc(size_t size) {
    void *result;
    internal_alloc(size, &result);
    return result;
}

void __gnat_free(void *addr) {
    internal_free(addr);
}
