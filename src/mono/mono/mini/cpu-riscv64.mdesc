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

il_seq_point: len:0

voidcall: len:32 clob:c
voidcall_reg: src1:i len:32 clob:c

store_membase_reg: dest:b src1:i len:32

load_membase: dest:i src1:b len:32

iconst: dest:i len:32
add_imm: dest:i src1:i len:32