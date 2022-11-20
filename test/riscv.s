main: # @main
  addi sp, sp, -64
  sd ra, 56(sp)
  sd s0, 48(sp)
  addi s0, sp, 64

  addi a0, zero, 0
  sd a0, -16(s0)
  ld a0, -8(s0)
  ld a5, 8(a0)
  ld a0, -8(s0)
  ld a6, 0(a0)

  addi a0, s0, -16
  addi a1, s0, -32
  addi a2, s0, -40
  addi a3, s0, -48
  addi a4, s0, -56
  addi a5, a5, 0
  jalr ra, 0(a6)
  ld ra, -16(s0)

  addiw a0, a0, 2048


  