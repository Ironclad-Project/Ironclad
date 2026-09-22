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
    beq a2, zero, 1f
    add a2, a1, a2
    mv a5, a0
0:
    lbu a4, 0(a1)
    addi a1, a1, 1
    addi a5, a5, 1
    sb a4, -1(a5)
    bne a1, a2, 0b
1:
    ret

.global memset
memset:
        mv      a5,a0
        add     a4,a0,a2
        beq     a2,zero,3f
2:
        sb      a1,0(a5)
        addi    a5,a5,1
        bne     a5,a4,2b
3:
        ret

.global memmove
memmove:
        bgeu    a0,a1,5f
        beq     a2,zero,6f
        add     a2,a1,a2
        mv      a5,a0
4:
        lbu     a4,0(a1)
        addi    a1,a1,1
        addi    a5,a5,1
        sb      a4,-1(a5)
        bne     a1,a2,4b
        ret
5:
        bgtu    a0,a1,7f
6:
        ret
7:
        beq     a2,zero,6b
        addi    a2,a2,-1
        add     a5,a1,a2
        lbu     a4,0(a5)
        add     a5,a0,a2
        sb      a4,0(a5)
        j       7b

.global memcmp
memcmp:
        beq     a2,zero,10f
        add     a2,a0,a2
        j       9f
8:
        beq     a0,a2,10f
9:
        lbu     a4,0(a0)
        lbu     a5,0(a1)
        addi    a0,a0,1
        addi    a1,a1,1
        beq     a4,a5,8b
        li      a0,1
        bgeu    a4,a5,11f
        li      a0,-1
        ret
10:
        li      a0,0
11:
        ret

# Copy between kernel memory and a userland address, for which the caller has
# opened userland access: a0 is the kernel side, a1 the userland side, a2 the
# length, a3 nonzero to copy to userland, and the bytes left uncopied are
# stored at a4. A page or access fault on the copy resumes at
# arch_copy_userland_fixup (see the trap handler) with a2 holding what is left.
.global arch_copy_userland
arch_copy_userland:
        mv      t0,a1
        mv      t1,a0
        beq     a3,zero,12f
        mv      t0,a0
        mv      t1,a1
12:
        beq     a2,zero,arch_copy_userland_fixup
.global arch_copy_userland_start
arch_copy_userland_start:
        lbu     t2,0(t0)
        sb      t2,0(t1)
.global arch_copy_userland_end
arch_copy_userland_end:
        addi    t0,t0,1
        addi    t1,t1,1
        addi    a2,a2,-1
        bne     a2,zero,arch_copy_userland_start
.global arch_copy_userland_fixup
arch_copy_userland_fixup:
        sd      a2,0(a4)
        ret
