/*
 * Licensed to the .NET Foundation under one or more agreements.
 * The .NET Foundation licenses this file to you under the MIT license.
 */

#include <mono/utils/mono-hwcap.h>

#include "mini-runtime.h"
#include "ir-emit.h"

#ifdef TARGET_RISCV64
#include "cpu-riscv64.h"
#else
#include "cpu-riscv32.h"
#endif

static gboolean riscv_stdext_a, riscv_stdext_b, riscv_stdext_c,
                riscv_stdext_d, riscv_stdext_f, riscv_stdext_j,
                riscv_stdext_l, riscv_stdext_m, riscv_stdext_n,
                riscv_stdext_p, riscv_stdext_q, riscv_stdext_t,
                riscv_stdext_v;

void
mono_arch_cpu_init (void)
{
}

void
mono_arch_init (void)
{
	riscv_stdext_a = mono_hwcap_riscv_has_stdext_a;
	riscv_stdext_c = mono_hwcap_riscv_has_stdext_c;
	riscv_stdext_d = mono_hwcap_riscv_has_stdext_d;
	riscv_stdext_f = mono_hwcap_riscv_has_stdext_f;
	riscv_stdext_m = mono_hwcap_riscv_has_stdext_m;
}

void
mono_arch_finish_init (void)
{
}

void
mono_arch_register_lowlevel_calls (void)
{
}

void
mono_arch_cleanup (void)
{
}

void
mono_arch_set_target (char *mtriple)
{
	// riscv{32,64}[extensions]-[<vendor>-]<system>-<abi>

	size_t len = strlen (MONO_RISCV_ARCHITECTURE);

	if (!strncmp (mtriple, MONO_RISCV_ARCHITECTURE, len)) {
		mtriple += len;

		for (;;) {
			char c = *mtriple;

			if (!c || c == '-')
				break;

			// ISA manual says upper and lower case are both OK.
			switch (c) {
			case 'A':
			case 'a':
				riscv_stdext_a = TRUE;
				break;
			case 'B':
			case 'b':
				riscv_stdext_b = TRUE;
				break;
			case 'C':
			case 'c':
				riscv_stdext_c = TRUE;
				break;
			case 'D':
			case 'd':
				riscv_stdext_d = TRUE;
				break;
			case 'F':
			case 'f':
				riscv_stdext_f = TRUE;
				break;
			case 'J':
			case 'j':
				riscv_stdext_j = TRUE;
				break;
			case 'L':
			case 'l':
				riscv_stdext_l = TRUE;
				break;
			case 'M':
			case 'm':
				riscv_stdext_m = TRUE;
				break;
			case 'N':
			case 'n':
				riscv_stdext_n = TRUE;
				break;
			case 'P':
			case 'p':
				riscv_stdext_p = TRUE;
				break;
			case 'Q':
			case 'q':
				riscv_stdext_q = TRUE;
				break;
			case 'T':
			case 't':
				riscv_stdext_t = TRUE;
				break;
			case 'V':
			case 'v':
				riscv_stdext_v = TRUE;
				break;
			default:
				break;
			}

			mtriple++;
		}
	}
}

guint32
mono_arch_cpu_optimizations (guint32 *exclude_mask)
{
	*exclude_mask = 0;
	return 0;
}

gboolean
mono_arch_have_fast_tls (void)
{
	return TRUE;
}

gboolean
mono_arch_opcode_supported (int opcode)
{
	switch (opcode) {
	case OP_ATOMIC_ADD_I4:
	case OP_ATOMIC_EXCHANGE_I4:
	case OP_ATOMIC_CAS_I4:
	case OP_ATOMIC_LOAD_I1:
	case OP_ATOMIC_LOAD_I2:
	case OP_ATOMIC_LOAD_I4:
	case OP_ATOMIC_LOAD_U1:
	case OP_ATOMIC_LOAD_U2:
	case OP_ATOMIC_LOAD_U4:
	case OP_ATOMIC_STORE_I1:
	case OP_ATOMIC_STORE_I2:
	case OP_ATOMIC_STORE_I4:
	case OP_ATOMIC_STORE_U1:
	case OP_ATOMIC_STORE_U2:
	case OP_ATOMIC_STORE_U4:
#ifdef TARGET_RISCV64
	case OP_ATOMIC_ADD_I8:
	case OP_ATOMIC_EXCHANGE_I8:
	case OP_ATOMIC_CAS_I8:
	case OP_ATOMIC_LOAD_I8:
	case OP_ATOMIC_LOAD_U8:
	case OP_ATOMIC_STORE_I8:
	case OP_ATOMIC_STORE_U8:
#endif
		return riscv_stdext_a;
	case OP_ATOMIC_LOAD_R4:
	case OP_ATOMIC_STORE_R4:
#ifdef TARGET_RISCV64
	case OP_ATOMIC_LOAD_R8:
	case OP_ATOMIC_STORE_R8:
#endif
		return riscv_stdext_a && riscv_stdext_d;
	default:
		return FALSE;
	}
}

const char *
mono_arch_regname (int reg)
{
    static const char *names [RISCV_N_GREGS] = {
		"zero", "ra", "sp",  "gp",  "tp", "t0", "t1", "t2",
		"s0",   "s1", "a0",  "a1",  "a2", "a3", "a4", "a5",
		"a6",   "a7", "s2",  "s3",  "s4", "s5", "s6", "s7",
		"s8",   "s9", "s10", "s11", "t3", "t4", "t5", "t6",
    };

    if (reg >= 0 && reg < G_N_ELEMENTS (names))
        return names [reg];

    return "x?";
}

const char*
mono_arch_fregname (int reg)
{
    static const char *names [RISCV_N_FREGS] = {
		"ft0", "ft1", "ft2",  "ft3",  "ft4", "ft5", "ft6",  "ft7",
		"fs0", "fs1", "fa0",  "fa1",  "fa2", "fa3", "fa4",  "fa5",
		"fa6", "fa7", "fs2",  "fs3",  "fs4", "fs5", "fs6",  "fs7",
		"fs8", "fs9", "fs10", "fs11", "ft8", "ft9", "ft10", "ft11",
    };

    if (reg >= 0 && reg < G_N_ELEMENTS (names))
        return names [reg];

    return "f?";
}

gpointer
mono_arch_get_this_arg_from_call (host_mgreg_t *regs, guint8 *code)
{
	return (gpointer) regs [RISCV_A0];
}

MonoMethod *
mono_arch_find_imt_method (host_mgreg_t *regs, guint8 *code)
{
	return (MonoMethod *) regs [MONO_ARCH_IMT_REG];
}

MonoVTable *
mono_arch_find_static_call_vtable (host_mgreg_t *regs, guint8 *code)
{
	return (MonoVTable *) regs [MONO_ARCH_VTABLE_REG];
}

GSList*
mono_arch_get_cie_program (void)
{
	GSList *l = NULL;

	mono_add_unwind_op_def_cfa (l, (guint8*)NULL, (guint8*)NULL, RISCV_SP, 0);

	return l;
}

host_mgreg_t
mono_arch_context_get_int_reg (MonoContext *ctx, int reg)
{
	return ctx->gregs [reg];
}

host_mgreg_t*
mono_arch_context_get_int_reg_address (MonoContext *ctx, int reg)
{
	return &ctx->gregs [reg];
}

void
mono_arch_context_set_int_reg (MonoContext *ctx, int reg, host_mgreg_t val)
{
	ctx->gregs [reg] = val;
}

void
mono_arch_flush_register_windows (void)
{
}

void
mono_arch_flush_icache (guint8 *code, gint size)
{
#ifndef MONO_CROSS_COMPILE
	__builtin___clear_cache ((char *)code, (char *)code + size);
#endif
}

MonoDynCallInfo *
mono_arch_dyn_call_prepare (MonoMethodSignature *sig)
{
	NOT_IMPLEMENTED;
	return NULL;
}

void
mono_arch_dyn_call_free (MonoDynCallInfo *info)
{
	NOT_IMPLEMENTED;
}

int
mono_arch_dyn_call_get_buf_size (MonoDynCallInfo *info)
{
	NOT_IMPLEMENTED;
	return 0;
}

void
mono_arch_start_dyn_call (MonoDynCallInfo *info, gpointer **args, guint8 *ret,
                          guint8 *buf)
{
	NOT_IMPLEMENTED;
}

void
mono_arch_finish_dyn_call (MonoDynCallInfo *info, guint8 *buf)
{
	NOT_IMPLEMENTED;
}

int
mono_arch_get_argument_info (MonoMethodSignature *csig, int param_count,
                             MonoJitArgumentInfo *arg_info)
{
    NOT_IMPLEMENTED;
    return 0;
}

void
mono_arch_patch_code_new (MonoCompile *cfg, guint8 *code,
                          MonoJumpInfo *ji, gpointer target)
{
	NOT_IMPLEMENTED;
}

/* Set arguments in the ccontext (for i2n entry) */
void
mono_arch_set_native_call_context_args (CallContext *ccontext, gpointer frame, MonoMethodSignature *sig)
{
	NOT_IMPLEMENTED;
}

/* Set return value in the ccontext (for n2i return) */
void
mono_arch_set_native_call_context_ret (CallContext *ccontext, gpointer frame, MonoMethodSignature *sig, gpointer retp)
{
	NOT_IMPLEMENTED;
}

/* Gets the arguments from ccontext (for n2i entry) */
gpointer
mono_arch_get_native_call_context_args (CallContext *ccontext, gpointer frame, MonoMethodSignature *sig)
{
	NOT_IMPLEMENTED;
}

/* Gets the return value from ccontext (for i2n exit) */
void
mono_arch_get_native_call_context_ret (CallContext *ccontext, gpointer frame, MonoMethodSignature *sig)
{
	NOT_IMPLEMENTED;
}

#ifndef DISABLE_JIT

#ifdef MONO_ARCH_SOFT_FLOAT_FALLBACK

gboolean
mono_arch_is_soft_float (void)
{
	return !riscv_stdext_d;
}

#endif

gboolean
mono_arch_opcode_needs_emulation (MonoCompile *cfg, int opcode)
{
	switch (opcode) {
	case OP_IDIV:
	case OP_IDIV_UN:
	case OP_IREM:
	case OP_IREM_UN:
#ifdef TARGET_RISCV64
	case OP_LDIV:
	case OP_LDIV_UN:
	case OP_LREM:
	case OP_LREM_UN:
#endif
		return !riscv_stdext_m;
	default:
		return TRUE;
	}
}

gboolean
mono_arch_tailcall_supported (MonoCompile *cfg, MonoMethodSignature *caller_sig, MonoMethodSignature *callee_sig, gboolean virtual_)
{
	NOT_IMPLEMENTED;
}

gboolean
mono_arch_is_inst_imm (int opcode, int imm_opcode, gint64 imm)
{
	// TODO: Make a proper decision based on opcode.
	return TRUE;
}

GList *
mono_arch_get_allocatable_int_vars (MonoCompile *cfg)
{
	GList *vars = NULL;

	for (guint i = 0; i < cfg->num_varinfo; i++) {
		MonoInst *ins = cfg->varinfo [i];
		MonoMethodVar *vmv = MONO_VARINFO (cfg, i);

		if (vmv->range.first_use.abs_pos >= vmv->range.last_use.abs_pos)
			continue;

		if ((ins->flags & (MONO_INST_IS_DEAD | MONO_INST_VOLATILE | MONO_INST_INDIRECT)) ||
		    (ins->opcode != OP_LOCAL && ins->opcode != OP_ARG))
			continue;

		if (!mono_is_regsize_var (ins->inst_vtype))
			continue;

		vars = g_list_prepend (vars, vmv);
	}

	vars = mono_varlist_sort (cfg, vars, 0);

	return vars;
}

GList *
mono_arch_get_global_int_regs (MonoCompile *cfg)
{
	GList *regs = NULL;

	for (int i = RISCV_S0; i <= RISCV_S11; i++)
		regs = g_list_prepend (regs, GUINT_TO_POINTER (i));

	return regs;
}

guint32
mono_arch_regalloc_cost (MonoCompile *cfg, MonoMethodVar *vmv)
{
	return cfg->varinfo [vmv->idx]->opcode == OP_ARG ? 1 : 2;
}

#ifdef ENABLE_LLVM

LLVMCallInfo*
mono_arch_get_llvm_call_info (MonoCompile *cfg, MonoMethodSignature *sig)
{
	NOT_IMPLEMENTED;
}

#endif

/**
 * mono_arch_create_vars:
 *	before this function, mono_compile_create_vars() in mini.c
 *	has process vars in a genetic ways. So just do some Arch
 *	related process specified in ABI.
 */

void
mono_arch_create_vars (MonoCompile *cfg)
{
	// TODO: do not process any vars just for init implement.

	// NOT_IMPLEMENTED;
	// MonoMethodSignature *sig;

	// sig = mono_method_signature_internal (cfg->method);

	if (cfg->method->save_lmf) {
		cfg->lmf_ir = TRUE;
		cfg->create_lmf_var = TRUE;
	}
}

MonoInst *
mono_arch_emit_inst_for_method (MonoCompile *cfg, MonoMethod *cmethod,
                                MonoMethodSignature *fsig, MonoInst **args)
{
	return NULL;
}

/**
 * add_int32_arg:
 * 	Add Arguments into a0-a7 reg. 
 * 	if there is no available store it into stack.
 */
static void
add_arg(guint32 *nextArgReg, guint32 *stack_size, ArgInfo *ainfo) {

	ainfo->offset = *stack_size;
	// check if there is available Argument Regs
	if (*nextArgReg > RISCV_A7) {
		ainfo->storage = ArgOnStack;
		ainfo->reg = RISCV_SP; /* in the caller */
		// TODO: change stack size for different type
		stack_size += 4;
	}
	else {
		ainfo->storage = ArgInIReg;
		ainfo->reg = *nextArgReg;
		(*nextArgReg) ++;
	}
}

/**
 * get_call_info:
 * 	create call info here.
 *  allocate memory for *cinfo, and assign Regs for Arguments.
 */
CallInfo *
get_call_info(MonoMemPool *mp, MonoMethodSignature *sig){

	CallInfo *cinfo;
	int paramNum = sig->hasthis + sig->param_count;
	if (mp)
		cinfo = mono_mempool_alloc0 (mp, sizeof (CallInfo) + (sizeof (ArgInfo) * paramNum));
	else
		cinfo = g_malloc0 (sizeof (CallInfo) + (sizeof (ArgInfo) * paramNum));


	// process the store type of return val
	MonoType *ret_type = mini_get_underlying_type (sig->ret);
	switch (ret_type->type){
		case MONO_TYPE_VOID:
			cinfo->ret.storage = ArgNone;
			break;
		case MONO_TYPE_I:
			cinfo->ret.storage = ArgInIReg;
			cinfo->ret.reg = RISCV_RA;
			break;
		
		default:
			g_error ("Can't handle as return value 0x%x", ret_type->type);
			break;
	}

	guint32 nextArgReg = RISCV_A0;
	guint32 stack_size = 0;
	// add this pointer as first argument if hasthis == true
	if (sig->hasthis)
		add_arg(&nextArgReg, &stack_size, cinfo->args + 0);

	// TODO: only consider void return type for now, so skip the return reg.
	guint32 paramStart = 0;
	ArgStorage ret_storage = cinfo->ret.storage;
	if(ret_type->type != ArgNone){
		paramStart = 1;
	}

	// TODO: process if function call has variable parameter
	if (!sig->pinvoke && (sig->call_convention == MONO_CALL_VARARG) && (nextArgReg == RISCV_A0)) {
	}

	// process other general Arguments
	for(guint32 i = paramStart; i < sig->param_count; ++i){
		ArgInfo *ainfo = &cinfo->args [sig->hasthis + i];
		MonoType *ptype;

		// process the variable parameter sig->sentinelpos mark the first VARARG
		if (!sig->pinvoke && (sig->call_convention == MONO_CALL_VARARG) && (i == sig->sentinelpos)) {
			NOT_IMPLEMENTED;
		}

		ptype = mini_get_underlying_type (sig->params [i]);
		switch (ptype->type)
		{
			case MONO_TYPE_I1:
				ainfo->is_signed = 1;
			case MONO_TYPE_U1:
				add_arg (&nextArgReg, &stack_size, ainfo);
				ainfo->byte_arg_size = 1;
				break;
			case MONO_TYPE_I2:
				ainfo->is_signed = 1;
			case MONO_TYPE_U2:
				add_arg (&nextArgReg, &stack_size, ainfo);
				ainfo->byte_arg_size = 2;
				break;
			case MONO_TYPE_I4:
				ainfo->is_signed = 1;
			case MONO_TYPE_U4:
				add_arg (&nextArgReg, &stack_size, ainfo);
				ainfo->byte_arg_size = 4;
				break;
			case MONO_TYPE_I8:
				ainfo->is_signed = 1;
			case MONO_TYPE_U8:
				add_arg (&nextArgReg, &stack_size, ainfo);
				ainfo->byte_arg_size = 8;
				break;
			case MONO_TYPE_I:
				ainfo->is_signed = 1;
			case MONO_TYPE_U:
				add_arg (&nextArgReg, &stack_size, ainfo);
				break;
			
			default:
				g_error("Can't handle parameter with type value 0x%x", ret_type->type);
				break;
		}
	}

	cinfo->stack_usage = stack_size;
	cinfo->reg_usage = nextArgReg;
	return cinfo;
}

static void
add_outarg_reg (MonoCompile *cfg, MonoCallInst *call, ArgStorage storage, int reg, MonoInst *tree){
	MonoInst *ins;

	switch (storage) {
		default:
			NOT_IMPLEMENTED;
			break;
		case ArgInIReg:
			MONO_INST_NEW (cfg, ins, OP_MOVE);
			ins->dreg = mono_alloc_ireg_copy (cfg, tree->dreg);
			ins->sreg1 = tree->dreg;
			MONO_ADD_INS (cfg->cbb, ins);
			mono_call_inst_add_outarg_reg (cfg, call, ins->dreg, reg, FALSE);
			break;
	}
}

/**
 * mono_arch_emit_call:
 * 	we process all Args of a function call
 *  (return, parameters)
 */
void
mono_arch_emit_call (MonoCompile *cfg, MonoCallInst *call)
{
	MonoInst *in, *ins;
	MonoMethodSignature *sig;

	CallInfo *cinfo;
	int is_virtual = 0;

	sig = call->signature;
	int paramNum = sig->param_count + sig->hasthis;

	cinfo = get_call_info (cfg->mempool, sig);

	if (COMPILE_LLVM (cfg)) {
		/* We shouldn't be called in the llvm case */
		cfg->disable_llvm = TRUE;
		return;
	}

	/* 
	 * Emit all arguments which are passed on the stack to prevent register
	 * allocation problems.
	 */
	// TODO
	int i;
	for (i = 0; i < paramNum; i++)
	{
		ArgInfo *ainfo = cinfo->args + i;
		MonoType *t;

		if (sig->hasthis && i == 0)
			t = mono_get_object_type ();
		else
			t = sig->params [i - sig->hasthis];

		t = mini_get_underlying_type (t);
		if (ainfo->storage == ArgOnStack){
			NOT_IMPLEMENTED;
		}
	}
	

	/*
	 * Emit all parameters passed in registers in non-reverse order for better readability
	 * and to help the optimization in emit_prolog ().
	 */
	// TODO
	for (i = 0; i < paramNum; ++i) {
		ArgInfo *ainfo = cinfo->args + i;

		in = call->args [i];

		if (ainfo->storage == ArgInIReg)
			add_outarg_reg (cfg, call, ainfo->storage, ainfo->reg, in);
	}

	/* Handle the case where there are no implicit arguments */
	// TODO

	/* Emit the inst of return by return type */
	switch (cinfo->ret.storage){
		default:
			NOT_IMPLEMENTED;
			break;
		case ArgNone:
			break;
	}

	/* setup LMF */
	if (cfg->method->save_lmf) {
		MONO_INST_NEW (cfg, ins, OP_SAVE_LMF);
		MONO_ADD_INS (cfg->cbb, ins);
	}

}

void
mono_arch_emit_outarg_vt (MonoCompile *cfg, MonoInst *ins, MonoInst *src)
{
	NOT_IMPLEMENTED;
}

void
mono_arch_emit_setret (MonoCompile *cfg, MonoMethod *method, MonoInst *val)
{
	MonoMethodSignature *sig;
	CallInfo *cinfo;

	sig = mono_method_signature_internal (cfg->method);
	if (!cfg->arch.cinfo)
		cfg->arch.cinfo = get_call_info (cfg->mempool, sig);
	cinfo = cfg->arch.cinfo;

	switch (cinfo->ret.storage){
		case ArgNone:
			break;
		case ArgInIReg:
			MONO_EMIT_NEW_UNALU (cfg, OP_MOVE, cfg->ret->dreg, val->dreg);
			break;
		default:
			NOT_IMPLEMENTED;
	}
}

void
mono_arch_decompose_opts (MonoCompile *cfg, MonoInst *ins)
{
	switch (ins->opcode) {
		case OP_LADD_IMM:
			break;
		default:
			NOT_IMPLEMENTED;
	}
}

void
mono_arch_decompose_long_opts (MonoCompile *cfg, MonoInst *long_ins)
{
#ifdef TARGET_RISCV32
	NOT_IMPLEMENTED;
#endif
}

/*
 * Set var information according to the calling convention. RISCV version.
 */
void
mono_arch_allocate_vars (MonoCompile *cfg)
{
	MonoMethodSignature *sig;
	int stack_size = 0;
	gint32 *stack_alloc_res;
	guint32 locals_stack_size, locals_stack_align;
	CallInfo *cinfo;

	// save FP reg to stack firstly
	cfg->frame_reg = RISCV_FP;
	stack_size += sizeof (target_mgreg_t);

	sig = mono_method_signature_internal (cfg->method);
	if (!cfg->arch.cinfo)
		cfg->arch.cinfo = get_call_info (cfg->mempool, sig);
	cinfo = cfg->arch.cinfo;

	cfg->arch.saved_iregs = cfg->used_int_regs;
	if (cfg->method->save_lmf) {
		/* Save all callee-saved registers normally, and restore them when unwinding through an LMF */
		cfg->arch.saved_iregs |= MONO_ARCH_CALLEE_SAVED_REGS;
	}

	/* Reserve space for callee saved registers */
	for (int i = 0; i < RISCV_N_GREGS; ++i){
		if (MONO_ARCH_IS_CALLEE_SAVED_REG (i) && (cfg->arch.saved_iregs & (1 << i))) {
			stack_size += sizeof (target_mgreg_t);
		}
	}
	if (sig->ret->type != MONO_TYPE_VOID) {
		switch (cinfo->ret.storage){
			case ArgNone:
				break;
			case ArgInIReg:
				cfg->ret->opcode = OP_REGVAR;
				cfg->ret->inst_c0 = cinfo->ret.reg;
				cfg->ret->dreg = cinfo->ret.reg;
				break;
			default:
				g_print("Can't handle storage type %d\n",cinfo->ret.storage);
				NOT_IMPLEMENTED;
				break;
		}
	}

	/* Allocate locals */
	stack_alloc_res = mono_allocate_stack_slots (cfg, FALSE, &locals_stack_size, &locals_stack_align);
	if (locals_stack_align) {
		stack_size += (locals_stack_align - 1);
		stack_size &= ~(locals_stack_align - 1);
	}
	cfg->locals_min_stack_offset = - (stack_size + locals_stack_size);
	cfg->locals_max_stack_offset = - stack_size;

	for (int i = cfg->locals_start; i < cfg->num_varinfo; i++) {
		if (stack_alloc_res [i] != -1) {
			MonoInst *ins = cfg->varinfo [i];
			ins->opcode = OP_REGOFFSET;
			ins->inst_basereg = cfg->frame_reg;
			ins->inst_offset = - (stack_size + stack_alloc_res [i]);
			printf ("allocated local %d to %d; ", i, ins->inst_offset); mono_print_ins (ins);
		}
	}
	stack_size += locals_stack_size;

	if (!sig->pinvoke && (sig->call_convention == MONO_CALL_VARARG)) {
		NOT_IMPLEMENTED;
	}

	// allocate vars
	for (int i = 0; i < sig->param_count + sig->hasthis; ++i){
		MonoInst *ins = cfg->args [i];
		if (ins->opcode != OP_REGVAR) {
			ArgInfo *ainfo = &cinfo->args [i];
			gboolean inreg = MONO_ARCH_CHECK_IN_REG(ainfo->storage);

			/* FIXME: Allocate volatile arguments to registers */
			if (ins->flags & (MONO_INST_VOLATILE|MONO_INST_INDIRECT))
				inreg = FALSE;

			ins->opcode = OP_REGOFFSET;
			switch (ainfo->storage) {
				case ArgInIReg:
					ins->opcode = OP_REGVAR;
					ins->dreg = ainfo->reg;
					break;
				default:
					NOT_IMPLEMENTED;
					break;
			}

			/* following arguments are saved to the stack in the prolog */
			if (!inreg) {
				ins->opcode = OP_REGOFFSET;
				ins->inst_basereg = cfg->frame_reg;
				/* These arguments are saved to the stack in the prolog */
				stack_size = ALIGN_TO (stack_size, sizeof (target_mgreg_t));
				stack_size += sizeof (target_mgreg_t);
				ins->inst_offset = -stack_size;
				printf ("allocated local %d to %d; ", i, ins->inst_offset); mono_print_ins (ins);
			}

		}
	}

	cfg->stack_offset = stack_size;
}

#define NEW_INS(cfg,ins,dest,op) do {	\
		MONO_INST_NEW ((cfg), (dest), (op)); \
		(dest)->cil_code = (ins)->cil_code; \
		mono_bblock_insert_before_ins (bb, ins, (dest)); \
	} while (0)

/*
 * mono_arch_lowering_pass:
 *
 *  Converts complex opcodes into simpler ones so that each IR instruction
 * corresponds to one machine instruction.
 */
void
mono_arch_lowering_pass (MonoCompile *cfg, MonoBasicBlock *bb)
{
	MonoInst *ins,*n, *temp;
	if (cfg->verbose_level > 2) {
		int idx = 0;

		g_print ("BASIC BLOCK %d (before lowering)\n", bb->block_num);
		MONO_BB_FOR_EACH_INS (bb, ins) {
			mono_print_ins_index (idx++, ins);
		}
		
	}

	MONO_BB_FOR_EACH_INS_SAFE (bb, n, ins){
		switch (ins->opcode){
			case OP_IL_SEQ_POINT:
			case OP_VOIDCALL_REG:
				break;	
			// Inst S{B|H|W|D} use I-type Imm
			case OP_STORE_MEMBASE_IMM:
			// Inst L{B|H|W|D} use I-type Imm
			case OP_LOAD_MEMBASE:
			// Inst ADDI use I-type Imm
			case OP_ADD_IMM:
				if(! RISCV_VALID_I_IMM ((gint32) (gssize) (ins->inst_imm))){
					NEW_INS (cfg, ins, temp, OP_ICONST);
					temp->inst_c0 = ins->inst_imm;
					temp->dreg = mono_alloc_ireg (cfg);
					ins->sreg1 = temp->dreg;
				}
				break;
			case OP_MOVE:
				// mv ra, a1 -> addi ra, a1, 0
				ins->opcode = OP_ADD_IMM;
				ins->inst_imm = 0;
				break;
			default:
				printf ("unable to lowering following IR:"); mono_print_ins (ins);
				NOT_IMPLEMENTED;
				break;
		}
	}
}

void
mono_arch_peephole_pass_1 (MonoCompile *cfg, MonoBasicBlock *bb)
{
}

void
mono_arch_peephole_pass_2 (MonoCompile *cfg, MonoBasicBlock *bb)
{
}

// Uses at most 8 bytes on RV32I and 16 bytes on RV64I.
guint8 *
mono_riscv_emit_imm (guint8 *code, int rd, gsize imm)
{
#ifdef TARGET_RISCV64
	if (RISCV_VALID_I_IMM (imm)) {
		riscv_addi (code, rd, RISCV_ZERO, imm);
		return code;
	}

	/*
	 * This is not pretty, but RV64I doesn't make it easy to load constants.
	 * Need to figure out something better.
	 */
	riscv_jal (code, rd, sizeof (guint64));
	*(guint64 *) code = imm;
	code += sizeof (guint64);
	riscv_ld (code, rd, rd, 0);
#else
	if (RISCV_VALID_I_IMM (imm)) {
		riscv_addi (code, rd, RISCV_ZERO, imm);
		return code;
	}

	riscv_lui (code, rd, RISCV_BITS (imm, 12, 20));

	if (!RISCV_VALID_U_IMM (imm))
		riscv_ori (code, rd, rd, RISCV_BITS (imm, 0, 12));
#endif

	return code;
}

// Uses at most 16 bytes on RV32I and 24 bytes on RV64I.
guint8 *
mono_riscv_emit_load (guint8 *code, int rd, int rs1, gint32 imm)
{
	if (RISCV_VALID_I_IMM (imm)) {
#ifdef TARGET_RISCV64
		riscv_ld (code, rd, rs1, imm);
#else
		riscv_lw (code, rd, rs1, imm);
#endif
	} else {
		code = mono_riscv_emit_imm (code, rd, imm);
		riscv_add (code, rd, rs1, rd);
#ifdef TARGET_RISCV64
		riscv_ld (code, rd, rd, 0);
#else
		riscv_lw (code, rd, rd, 0);
#endif
	}

	return code;
}

// May clobber t1. Uses at most 16 bytes on RV32I and 24 bytes on RV64I.
guint8 *
mono_riscv_emit_store (guint8 *code, int rs1, int rs2, gint32 imm)
{
	if (RISCV_VALID_S_IMM (imm)) {
#ifdef TARGET_RISCV64
		riscv_sd (code, rs1, rs2, imm);
#else
		riscv_sw (code, rs1, rs2, imm);
#endif
	} else {
		code = mono_riscv_emit_imm (code, RISCV_T1, imm);
		riscv_add (code, RISCV_T1, rs2, RISCV_T1);
#ifdef TARGET_RISCV64
		riscv_sd (code, rs1, RISCV_T1, 0);
#else
		riscv_sw (code, rs1, RISCV_T1, 0);
#endif
	}

	return code;
}

/*
 * Stack frame layout:
 *  |--------------------------| -- <-- sp + stack_size (FP)
 *  | saved return value	   |
 *  |--------------------------| 
 * 	| saved FP reg			   |
 *  |--------------------------| 
 *  | param area			   |
 *  |--------------------------|
 * 	| MonoLMF structure		   |
 *  |--------------------------|
 *  | realignment			   |
 *  |--------------------------| -- <-- sp
 */
guint8 *
mono_arch_emit_prolog (MonoCompile *cfg)
{
	MonoMethod *method = cfg->method;
	MonoMethodSignature *sig;
	MonoInst *inst;
	guint8 *code;
	guint32 iregs_to_save = 0;
	int alloc_size;
	int cfa_offset;

	/* lmf_offset is the offset of the LMF from our stack pointer. */
	// guint32 lmf_offset = cfg->arch.lmf_offset;

	cfg->code_size = MAX (cfg->header->code_size * 4, 1024);;
	code = (unsigned char*)g_malloc (cfg->code_size);
	cfg->native_code = code;

	/* realigned */
	cfg->stack_offset = ALIGN_TO (cfg->stack_offset, MONO_ARCH_FRAME_ALIGNMENT);
	

	/* stack_offset should not be changed here. */
	alloc_size = cfg->stack_offset;
	cfg->stack_usage = alloc_size;

	iregs_to_save = (cfg->used_int_regs & MONO_ARCH_CALLEE_SAVED_REGS);

	/* set up frame */
	cfa_offset = 0;
	// mono_emit_unwind_op_def_cfa (cfg, code, RISCV_SP, cfa_offset);

	// set up stack pointer
	int stack_size = 0;
	riscv_addi(code,RISCV_SP,RISCV_SP,-alloc_size);
	MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);

	// save return value
	stack_size += sizeof(target_mgreg_t);
	code = mono_riscv_emit_store(code, RISCV_RA, RISCV_SP, alloc_size - stack_size);
	MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);

	// save a0(fp) value
	stack_size += sizeof(target_mgreg_t);
	code = mono_riscv_emit_store(code, RISCV_FP, RISCV_SP, alloc_size - stack_size);
	MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
	
	// set new a0(fp) value
	riscv_addi(code,RISCV_FP,RISCV_SP,alloc_size);
	MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);

	// save other registers
	// TODO

	g_assert(stack_size == alloc_size && "prologue emit error: there are stack not used");

	return code;
}

/*
 * emit_load_regset:
 *
 *   Emit code to load the registers in REGS from consecutive memory locations starting
 * at BASEREG+OFFSET.
 */
static __attribute__ ((__warn_unused_result__)) guint8*
emit_load_regset (guint8 *code, guint64 used_regs, int basereg, int offset){
	int i, pos;

	pos = 0;
	for (i = 0; i < 32; ++i) {
		if (used_regs & (1 << i)) {
			g_print("[Emit Epilogue]: Used Reg ID => %d\n", i);
		}
	}

	return code;
}

void
mono_arch_emit_epilog (MonoCompile *cfg)
{
	guint8 *code = NULL;
	CallInfo *cinfo;
	MonoMethod *method = cfg->method;
	int i;
	int max_epilog_size = 16 + 20*4;
	int alloc2_size = 0;

	code = realloc_code (cfg, max_epilog_size);

	if (cfg->method->save_lmf) {
		g_assert_not_reached();
	} else {
		/* Restore gregs */
		code = emit_load_regset (code, MONO_ARCH_CALLEE_SAVED_REGS & cfg->used_int_regs, RISCV_SP, cfg->stack_offset);
	}
	
	/* Load returned vtypes into registers if needed */
	cinfo = cfg->arch.cinfo;
	switch (cinfo->ret.storage) {
		case ArgNone:
			break;
		default:
			g_assert_not_reached();
	}

	/* Destroy frame */
	// 	Emits:
	//  ld s0,stack_size - 8(sp) # 8-byte Folded Reload
	// 		...
	// 	ld s0, 0(sp) # 8-byte Folded Reload
	//  addi sp,sp,stack_size
	
	g_assert("Check stack align\n" && (cfg->stack_offset == (cfg->stack_offset>>3)<<3));
	for(int offset = cfg->stack_offset; offset > 16; offset-=8){
		mono_riscv_emit_load(code, RISCV_FP, RISCV_SP, offset - 8);
		MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
	}
	mono_riscv_emit_load(code, RISCV_RA, RISCV_SP, 8);
	MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
	mono_riscv_emit_load(code, RISCV_S0, RISCV_SP, 0);
	MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
	riscv_addi(code, RISCV_SP, RISCV_SP, cfg->stack_offset);
	MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);

	if(cinfo->ret.storage == ArgNone)
		riscv_jalr(code, RISCV_X0, RISCV_X1, 0);
	else
		g_assert_not_reached();
	MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);

	g_assert (code - (cfg->native_code + cfg->code_len) < max_epilog_size);
	set_code_cursor (cfg, code);
}

void
mono_arch_output_basic_block (MonoCompile *cfg, MonoBasicBlock *bb)
{
	MonoInst *ins;
	MonoCallInst *call;
	guint8 *code = cfg->native_code + cfg->code_len;
	target_mgreg_t imm;
	int start_offset, max_len;
	int ins_cnt = 0;

	start_offset = code - cfg->native_code;
	g_assert (start_offset <= cfg->code_size);

	MONO_BB_FOR_EACH_INS (bb, ins) {
		guint offset = code - cfg->native_code;
		set_code_cursor (cfg, code);
		max_len = ins_get_size (ins->opcode);
		code = realloc_code (cfg, max_len);

		mono_debug_record_line_number (cfg, ins, offset);
		if (cfg->verbose_level > 2) {
			g_print ("    @ 0x%x\t", offset);
			mono_print_ins_index (ins_cnt++, ins);
		}

		/* Check for virtual regs that snuck by */
		g_assert ((ins->dreg >= -1) && (ins->dreg < 32));

		switch (ins->opcode) {
			case OP_IL_SEQ_POINT:
				mono_add_seq_point (cfg, bb, ins, code - cfg->native_code);
				break;
			default:
				printf ("unable to lowering following IR:"); mono_print_ins (ins);
				NOT_IMPLEMENTED;
				break;
		}

		g_assertf ((code - cfg->native_code - offset) <= max_len,
			   "wrong maximal instruction length of instruction %s (expected %d, got %d)",
			   mono_inst_name (ins->opcode), max_len, (int)(code - cfg->native_code - offset));
	}
	set_code_cursor (cfg, code);
}

void
mono_arch_emit_exceptions (MonoCompile *cfg)
{
	MonoJumpInfo *ji;
	// MonoClass *exc_class;
	guint8 *code;
	// const guint8* exc_throw_pos [MONO_EXC_INTRINS_NUM] = {NULL};
	guint8 exc_throw_found [MONO_EXC_INTRINS_NUM] = {0};
	int exc_id, max_epilog_size = 32;

	for (ji = cfg->patch_info; ji; ji = ji->next) {
		if (ji->type == MONO_PATCH_INFO_EXC) {
			exc_id = mini_exception_id_by_name ((const char*)ji->data.target);
			g_assert (exc_id < MONO_EXC_INTRINS_NUM);
			if (!exc_throw_found [exc_id]) {
				g_assert_not_reached();
				// max_epilog_size += 32; // TODO: how/why it be 32 ??
				// exc_throw_found [exc_id] = TRUE;
			}
		}
	}

	code = realloc_code (cfg, max_epilog_size);

	/* Emit code to raise corlib exceptions */
	for (ji = cfg->patch_info; ji; ji = ji->next) {
		if (ji->type != MONO_PATCH_INFO_EXC)
			continue;

		g_assert_not_reached();
	}

	set_code_cursor (cfg, code);
}

guint32
mono_arch_get_patch_offset (guint8 *code)
{
	NOT_IMPLEMENTED;
	return 0;
}

GSList *
mono_arch_get_trampolines (gboolean aot)
{
	NOT_IMPLEMENTED;
	return NULL;
}

#endif

#if defined(MONO_ARCH_SOFT_DEBUG_SUPPORTED)
void
mono_arch_set_breakpoint (MonoJitInfo *ji, guint8 *ip)
{
	NOT_IMPLEMENTED;
}

void
mono_arch_clear_breakpoint (MonoJitInfo *ji, guint8 *ip)
{
	NOT_IMPLEMENTED;
}

void
mono_arch_start_single_stepping (void)
{
	NOT_IMPLEMENTED;
}

void
mono_arch_stop_single_stepping (void)
{
	NOT_IMPLEMENTED;
}

gboolean
mono_arch_is_single_step_event (void *info, void *sigctx)
{
	NOT_IMPLEMENTED;
	return FALSE;
}

gboolean
mono_arch_is_breakpoint_event (void *info, void *sigctx)
{
	NOT_IMPLEMENTED;
	return FALSE;
}

void
mono_arch_skip_breakpoint (MonoContext *ctx, MonoJitInfo *ji)
{
	NOT_IMPLEMENTED;
}

void
mono_arch_skip_single_step (MonoContext *ctx)
{
	NOT_IMPLEMENTED;
}

SeqPointInfo*
mono_arch_get_seq_point_info (guint8 *code)
{
	NOT_IMPLEMENTED;
	return NULL;
}
#endif /* MONO_ARCH_SOFT_DEBUG_SUPPORTED */

gpointer
mono_arch_load_function (MonoJitICallId jit_icall_id)
{
	return NULL;
}
