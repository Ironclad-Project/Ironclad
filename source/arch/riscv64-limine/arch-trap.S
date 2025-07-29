#  arch-trap.S: RISC-V64 Trap Entry Assembly
#  Copyright (C) 2025 Sean C. Weeks - badrock1983
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Offsets of the interrupt frame.
.set FRAME_X1,        0
.set FRAME_X2,        8
.set FRAME_X3,       16
.set FRAME_X4,       24
.set FRAME_X5,       32
.set FRAME_X6,       40
.set FRAME_X7,       48
.set FRAME_X8,       56
.set FRAME_X9,       64
.set FRAME_X10,      72
.set FRAME_X11,      80
.set FRAME_X12,      88
.set FRAME_X13,      96
.set FRAME_X14,     104
.set FRAME_X15,     112
.set FRAME_X16,     120
.set FRAME_X17,     128
.set FRAME_X18,     136
.set FRAME_X19,     144
.set FRAME_X20,     152
.set FRAME_X21,     160
.set FRAME_X22,     168
.set FRAME_X23,     176
.set FRAME_X24,     184
.set FRAME_X25,     192
.set FRAME_X26,     200
.set FRAME_X27,     208
.set FRAME_X28,     216
.set FRAME_X29,     224
.set FRAME_X30,     232
.set FRAME_X31,     240
.set FRAME_SSTATUS, 248
.set FRAME_SEPC,    256
.set FRAME_SIZE,    272     # Total size of Frame (264) but 16‑byte aligned.

# Offsets of the CPU local.
.set KERN_STACK_OFF, 0
.set USER_STACK_OFF, 8
.set SCRATCH_OFF,    16

.section .text
.global trap_entry
.type   trap_entry, @function
.p2align 2
trap_entry:
    # Swap sscratch, which is where we store the CPU local, and tp, for a bit.
    # We will load the kernel stack from there.
    csrrw tp, sscratch, tp
    sd sp, USER_STACK_OFF(tp)
    sd a0, SCRATCH_OFF(tp)
    li a0, 0xFF00000000000000
    bltu sp, a0, 1f
    j 2f
1:
    ld sp, KERN_STACK_OFF(tp)
2:
    # Allocate full trap frame
    addi    sp, sp, -FRAME_SIZE

    # Save the context
    ld      a0, SCRATCH_OFF(tp)
    sd      x1, FRAME_X1(sp)
    sd      x3, FRAME_X3(sp)
    sd      x5, FRAME_X5(sp)
    sd      x6, FRAME_X6(sp)
    sd      x7, FRAME_X7(sp)
    sd      x8, FRAME_X8(sp)
    sd      x9, FRAME_X9(sp)
    sd     x10, FRAME_X10(sp)
    sd     x11, FRAME_X11(sp)
    sd     x12, FRAME_X12(sp)
    sd     x13, FRAME_X13(sp)
    sd     x14, FRAME_X14(sp)
    sd     x15, FRAME_X15(sp)
    sd     x16, FRAME_X16(sp)
    sd     x17, FRAME_X17(sp)
    sd     x18, FRAME_X18(sp)
    sd     x19, FRAME_X19(sp)
    sd     x20, FRAME_X20(sp)
    sd     x21, FRAME_X21(sp)
    sd     x22, FRAME_X22(sp)
    sd     x23, FRAME_X23(sp)
    sd     x24, FRAME_X24(sp)
    sd     x25, FRAME_X25(sp)
    sd     x26, FRAME_X26(sp)
    sd     x27, FRAME_X27(sp)
    sd     x28, FRAME_X28(sp)
    sd     x29, FRAME_X29(sp)
    sd     x30, FRAME_X30(sp)
    sd     x31, FRAME_X31(sp)
    csrr    t0, sstatus
    sd      t0, FRAME_SSTATUS(sp)
    csrr    t0, sepc
    sd      t0, FRAME_SEPC(sp)

    # Write the values we smashed with the stack setting.
    ld a0, USER_STACK_OFF(tp)
    sd a0, FRAME_X2(sp)
    csrrw tp, sscratch, tp
    sd x4, FRAME_X4(sp)

    # Call into Ada
    mv      a0, sp
    call    handle_trap

    # Restore the context
    ld      x1, FRAME_X1(sp)
    ld      x3, FRAME_X3(sp)
    ld      x4, FRAME_X4(sp)
    ld      x6, FRAME_X6(sp)
    ld      x7, FRAME_X7(sp)
    ld      x8, FRAME_X8(sp)
    ld      x9, FRAME_X9(sp)
    ld     x10, FRAME_X10(sp)
    ld     x11, FRAME_X11(sp)
    ld     x12, FRAME_X12(sp)
    ld     x13, FRAME_X13(sp)
    ld     x14, FRAME_X14(sp)
    ld     x15, FRAME_X15(sp)
    ld     x16, FRAME_X16(sp)
    ld     x17, FRAME_X17(sp)
    ld     x18, FRAME_X18(sp)
    ld     x19, FRAME_X19(sp)
    ld     x20, FRAME_X20(sp)
    ld     x21, FRAME_X21(sp)
    ld     x22, FRAME_X22(sp)
    ld     x23, FRAME_X23(sp)
    ld     x24, FRAME_X24(sp)
    ld     x25, FRAME_X25(sp)
    ld     x26, FRAME_X26(sp)
    ld     x27, FRAME_X27(sp)
    ld     x28, FRAME_X28(sp)
    ld     x29, FRAME_X29(sp)
    ld     x30, FRAME_X30(sp)
    ld     x31, FRAME_X31(sp)

    ld      x5, FRAME_SSTATUS(sp)
    csrw    sstatus, x5
    ld      x5, FRAME_SEPC(sp)
    csrw    sepc, x5

    ld      x5, FRAME_X5(sp)
    ld      x2, FRAME_X2(sp)

    # Return
    sret
