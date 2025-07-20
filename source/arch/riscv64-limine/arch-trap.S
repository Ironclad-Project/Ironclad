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

.set FRAME_RA,        0     # x1_ra
.set FRAME_TP,        8     # x4_tp
.set FRAME_T0,       16     # x5_t0
.set FRAME_T1,       24     # x6_t1
.set FRAME_T2,       32     # x7_t2

.set FRAME_A0,       40     # x10_a0
.set FRAME_A1,       48     # x11_a1
.set FRAME_A2,       56     # x12_a2
.set FRAME_A3,       64     # x13_a3
.set FRAME_A4,       72     # x14_a4
.set FRAME_A5,       80     # x15_a5
.set FRAME_A6,       88     # x16_a6
.set FRAME_A7,       96     # x17_a7

.set FRAME_T3,      104     # x28_t3
.set FRAME_T4,      112     # x29_t4
.set FRAME_T5,      120     # x30_t5
.set FRAME_T6,      128     # x31_t6

.set FRAME_SP,      136     # x2_sp
.set FRAME_GP,      144     # x3_gp
.set FRAME_S0,      152     # x8_s0
.set FRAME_S1,      160     # x9_s1
.set FRAME_S2,      168     # x18_s2
.set FRAME_S3,      176     # x19_s3
.set FRAME_S4,      184     # x20_s4
.set FRAME_S5,      192     # x21_s5
.set FRAME_S6,      200     # x22_s6
.set FRAME_S7,      208     # x23_s7
.set FRAME_S8,      216     # x24_s8
.set FRAME_S9,      224     # x25_s9
.set FRAME_S10,     232     # x26_s10
.set FRAME_S11,     240     # x27_s11

.set FRAME_SEPC,    248     # sepc
.set FRAME_SCAUSE,  256     # scause
.set FRAME_STVAL,   264     # stval
.set FRAME_SSTATUS, 272     # sstatus
.set FRAME_FCSR,    280     # fcsr
.set FRAME_FPCTX,   288     # FP_Context_Ptr

.set FRAME_SIZE,    304     # Total size of Frame (16‑byte aligned)

#  ----------------------------------------------------------------------------
#  TCB_* macros to access the current-context pointer in the TCB via TP
#  The TCB_CONTEXT_OFFSET in Ada is 208 bytes, so:
# Offset of the context-pointer field in the TCB
.set TCB_FPCTX,      368

.section .text
.global trap_entry
.type   trap_entry, @function

trap_entry:
    addi    sp, sp, -FRAME_SIZE         # Allocate full trap frame

    # --- save caller‑saved GPRs ---
    sd      ra, FRAME_RA(sp)            # x1
    sd      tp, FRAME_TP(sp)            # x4
    sd      t0, FRAME_T0(sp)            # x5
    sd      t1, FRAME_T1(sp)            # x6
    sd      t2, FRAME_T2(sp)            # x7

    sd      a0, FRAME_A0(sp)            # x10
    sd      a1, FRAME_A1(sp)            # x11
    sd      a2, FRAME_A2(sp)            # x12
    sd      a3, FRAME_A3(sp)            # x13
    sd      a4, FRAME_A4(sp)            # x14
    sd      a5, FRAME_A5(sp)            # x15
    sd      a6, FRAME_A6(sp)            # x16
    sd      a7, FRAME_A7(sp)            # x17

    sd      t3, FRAME_T3(sp)            # x28
    sd      t4, FRAME_T4(sp)            # x29
    sd      t5, FRAME_T5(sp)            # x30
    sd      t6, FRAME_T6(sp)            # x31

    # --- save callee‑saved GPRs ---
    sd      sp, FRAME_SP(sp)            # x2
    sd      gp, FRAME_GP(sp)            # x3
    sd      s0, FRAME_S0(sp)            # x8
    sd      s1, FRAME_S1(sp)            # x9
    sd      s2, FRAME_S2(sp)            # x18
    sd      s3, FRAME_S3(sp)            # x19
    sd      s4, FRAME_S4(sp)            # x20
    sd      s5, FRAME_S5(sp)            # x21
    sd      s6, FRAME_S6(sp)            # x22
    sd      s7, FRAME_S7(sp)            # x23
    sd      s8, FRAME_S8(sp)            # x24
    sd      s9, FRAME_S9(sp)            # x25
    sd      s10, FRAME_S10(sp)          # x26
    sd      s11, FRAME_S11(sp)          # x27

    # --- save CSRs ---
    csrr    t0, sepc
    sd      t0, FRAME_SEPC(sp)
    csrr    t0, scause
    sd      t0, FRAME_SCAUSE(sp)
    csrr    t0, stval
    sd      t0, FRAME_STVAL(sp)
    csrr    t0, sstatus
    sd      t0, FRAME_SSTATUS(sp)

    # --- save floating‑point control/status ---
    csrr    t0, fcsr
    sd      t0, FRAME_FCSR(sp)

    # --- save lazy-FP pointer from TCB ---
    # ld      t1, TCB_FPCTX(tp)
    # sd      t1, FRAME_FPCTX(sp)

    # --- re‑enable interrupts (for nested) ---
    li      t0, 2                   # SIE mask
    csrs    sstatus, t0

    # --- call into Ada trap handler ---
    mv      a0, sp
    call    handle_trap

    # --- restore caller‑saved GPRs ---
    ld      ra, FRAME_RA(sp)
    ld      tp, FRAME_TP(sp)
    ld      t0, FRAME_T0(sp)
    ld      t1, FRAME_T1(sp)
    ld      t2, FRAME_T2(sp)

    ld      a0, FRAME_A0(sp)
    ld      a1, FRAME_A1(sp)
    ld      a2, FRAME_A2(sp)
    ld      a3, FRAME_A3(sp)
    ld      a4, FRAME_A4(sp)
    ld      a5, FRAME_A5(sp)
    ld      a6, FRAME_A6(sp)
    ld      a7, FRAME_A7(sp)

    ld      t3, FRAME_T3(sp)
    ld      t4, FRAME_T4(sp)
    ld      t5, FRAME_T5(sp)
    ld      t6, FRAME_T6(sp)

    # --- restore callee‑saved GPRs ---
    ld      sp, FRAME_SP(sp)
    ld      gp, FRAME_GP(sp)
    ld      s0, FRAME_S0(sp)
    ld      s1, FRAME_S1(sp)
    ld      s2, FRAME_S2(sp)
    ld      s3, FRAME_S3(sp)
    ld      s4, FRAME_S4(sp)
    ld      s5, FRAME_S5(sp)
    ld      s6, FRAME_S6(sp)
    ld      s7, FRAME_S7(sp)
    ld      s8, FRAME_S8(sp)
    ld      s9, FRAME_S9(sp)
    ld      s10, FRAME_S10(sp)
    ld      s11, FRAME_S11(sp)

    # --- restore supervisor CSRs ---
    ld      t0, FRAME_SSTATUS(sp)
    csrw    sstatus, t0

    ld      t0, FRAME_SEPC(sp)
    csrw    sepc, t0

    # --- restore floating‑point control/status ---
    ld      t0, FRAME_FCSR(sp)
    csrw    fcsr, t0

    # --- tear down stack and return ---
    addi    sp, sp, FRAME_SIZE
    sret
