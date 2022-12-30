# Licensed to the .NET Foundation under one or more agreements.
# The .NET Foundation licenses this file to you under the MIT license.
#
# RISC-V RV64 Machine Description
#
# This file describes various properties of Mini instructions for RV64 and is
# read by genmdesc.py to generate a C header file used by various parts of the
# JIT.
#
# Lines are of the form:
#
#     <name>: len:<length> [dest:<rspec>] [src1:<rspec>] [src2:<rspec>] [src3:<rspec>] [clob:<cspec>]
#
# Here, <name> is the name of the instruction as specified in mini-ops.h.
# length is the maximum number of bytes that could be needed to generate native
# code for the instruction. dest, src1, src2, and src3 specify output and input
# registers needed by the instruction. <rspec> can be one of:
#
#     a    a0
#     i    any integer register
#     b    any integer register (used as a pointer)
#     f    any float register (a0 in soft float)
#
# clob specifies which registers are clobbered (i.e. overwritten with garbage)
# by the instruction. <cspec> can be one of:
#
#     a    a0
#     c    all caller-saved registers

nop: len:32
not_reached: len:0
not_null: src1:i len:0
dummy_use: src1:i len:0
il_seq_point: len:0

rethrow: src1:i len:32
get_ex_obj: dest:i len:32

br: len:32
call: dest:a len:32 clob:c
voidcall: len:32 clob:c
voidcall_reg: src1:i len:32 clob:c

storei1_membase_imm: dest:b len:32
store_membase_reg: dest:b src1:i len:32

load_membase: dest:i src1:b len:32
loadu4_membase: dest:i src1:b len:32

memory_barrier: len:32 clob:a

iconst: dest:i len:32
i8const: dest:i len:32
add_imm: dest:i src1:i len:32
long_add_imm: dest:i src1:i len:32
long_add: dest:i src1:i src2:i len:32

shr_un_imm: dest:i src1:i len:32
long_and_imm: dest:i src1:i len:32
long_and: dest:i src1:i src2:i len:32

riscv_beq: src1:i src2:i len:32
riscv_bne: src1:i src2:i len:32

gc_safe_point: src1:i len:32 clob:c