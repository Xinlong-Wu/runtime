main:
    sub sp, sp, 32
    stp x29, x30, [sp, 24]!
    add fp, sp, 0

    str x1, [sp, 24]
    str x0, [sp, 32]

    # ; allocate the stack space necessary for the call
    ldrw x0, [x1, 192]
    add x16, sp, 0
    sub x16, x16, x0
    add sp, x16, 0

    # /* copy stack from the CallContext, IP0(x16) = dest, IP1(x17) = source */
    add x16, sp, 0
    ldrw x17, x1, 200
start_copy:
    cpm x0, 0
    b.eq exit_copy
    ldrw x2, [x17, 0]
    add x16,x16, 8
    add x17,x17, 8
    sub x0, x0, 8
    b start_copy
exit_copy:

