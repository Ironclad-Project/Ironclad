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

.section .note.GNU-stack, "", %progbits
