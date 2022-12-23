main:
    sub sp, sp, 32
    stp fp, x30, [sp, 0]!
    mov fp, sp

    # /* save CallContext* onto stack */
    str x1, [fp, 16]
    # /* save target address onto stack */
    str x0, [fp, 24]

    # /* allocate the stack space necessary for the call */
    ldr x0, [x1, 512]   # x0 是 CallContext->stack_size
    mov x16, sp
    sub x16, x16, x0
    mov sp, x16

    # /* copy stack from the CallContext, IP0(x16) = dest, IP1 = source */
    mov x16, sp
    ldr x17, [x1, 520] # x17 是 CallContext->greg

label_start_copy: 
    cmp x0, 0
    bcc label_exit_copy
    ldr x2, [x17, 0]
    str x2, [x16, 0]
    add x17, x17, 8
    add x16, x16, 8
    sub x0, x0, 8
    b label_start_copy
label_exit_copy:

    # /* Load CallContext* into IP0 */
    ldr x16, [fp, 16]

    # /* set all general purpose registers from CallContext */
    ldr x0, [x16, 0]
    ldr x1, [x16, 8]
    # ...

    # /* load target addr */
    ldr x16, [fp, 24]

    blr x16

    # /* load CallContext* */
    ldr x16, [fp, 16]

    # /* set all general purpose registers to CallContext */
    str x0, [x16, 0]
    str x1, [x16, 8]

    mov sp, fp
    ldp fp, lr, [sp, 0]
    add sp, sp, 32
    ret

