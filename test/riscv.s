main: # @main
  addi sp, sp, -32
  sd ra, 24(sp)
  sd fp,16(sp)
  addi fp, sp, 32
  sd a1, 8(sp)
  sd a0, 0(sp)

  # T0 = CallContext->stack_size
  ld t0, 192(a0)
  # SP = SP - T0
  sub sp, sp, t0
  # copy stack from the CallContext, T1 = dest, T2 = source
  addi t1, sp, 0
  # t2 = CallContext->stack
  ld t2, 200(a0) 
start_copy:
  beq t0, zero, exit_copy
  ld t3, 0(t2)
  sd t3, 0(t1)
  addi t1, t1, 8
  addi t2, t2, 8
  addi t0, t0, -8
  j start_copy
exit_copy:
  ret
  


  