/*
 * Licensed to the .NET Foundation under one or more agreements.
 * The .NET Foundation licenses this file to you under the MIT license.
 */

#include <mono/utils/mono-hwcap.h>

#include "mini-runtime.h"
#include "ir-emit.h"

#include <mono/metadata/tokentype.h>

#ifdef TARGET_RISCV64
#include "cpu-riscv64.h"
#else
#include "cpu-riscv32.h"
#endif

/* The single step trampoline */
static gpointer ss_trampoline;

/* The breakpoint trampoline */
static gpointer bp_trampoline;

gboolean riscv_stdext_a, riscv_stdext_b, riscv_stdext_c,
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

	if (!mono_aot_only)
		bp_trampoline = mini_get_breakpoint_trampoline ();
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

/**
 * Emits:
 *   ld ra, stack_offset-8(sp) # 8-byte Folded Reload
 *   ld s0, stack_offset-16(sp) # 8-byte Folded Reload
 * 	 addi sp,sp,stack_size
*/
guint8*
mono_riscv_emit_destroy_frame (guint8 *code, int stack_offset){
	g_assert("Check stack align\n" && (stack_offset == (stack_offset>>3)<<3));

	code = mono_riscv_emit_load(code, RISCV_RA, RISCV_SP, stack_offset - sizeof(host_mgreg_t), 0);
	code = mono_riscv_emit_load(code, RISCV_S0, RISCV_SP, stack_offset - sizeof(host_mgreg_t)*2, 0);
	riscv_addi(code, RISCV_SP, RISCV_SP, stack_offset);

	return code;
}

static guint8*
emit_thunk (guint8 *code, gconstpointer target)
{
	g_print("emit thunk at 0x%lx\n",code);
	guint8 *p = code;
	code = mono_riscv_emit_imm(code, RISCV_T0, (gsize)target);
	riscv_jalr (code, RISCV_ZERO, RISCV_T0,0);

	mono_arch_flush_icache (p, code - p);
	g_print("end of thunk at 0x%x\n",code);
	return code;
}

static gpointer
create_thunk (MonoCompile *cfg, guchar *code, const guchar *target){
	MonoJitInfo *ji;
	MonoThunkJitInfo *info;
	guint8 *thunks, *p;
	int thunks_size;
	guint8 *orig_target;
	guint8 *target_thunk;

	if (cfg) {
		/*
		 * This can be called multiple times during JITting,
		 * save the current position in cfg->arch to avoid
		 * doing a O(n^2) search.
		 */
		if (!cfg->arch.thunks) {
			cfg->arch.thunks = cfg->thunks;
			cfg->arch.thunks_size = cfg->thunk_area;
		}

		thunks = cfg->arch.thunks;
		thunks_size = cfg->arch.thunks_size;
		if (!thunks_size) {
			g_print ("thunk failed %p->%p, thunk space=%d method %s", code, target, thunks_size, mono_method_full_name (cfg->method, TRUE));
			g_assert_not_reached ();
		}

		g_assert (*(guint32*)thunks == 0);
		emit_thunk (thunks, target);

		cfg->arch.thunks += THUNK_SIZE;
		cfg->arch.thunks_size -= THUNK_SIZE;

		return thunks;
	}
	else {
		NOT_IMPLEMENTED;
	}
}

static void
riscv_patch_full (MonoCompile *cfg, guint8 *code, guint8 *target, int relocation){
	switch (relocation){
		case MONO_R_RISCV_JAL:{
			gint32 inst = *(gint32 *) code;
			gint32 rd = RISCV_BITS (inst, 7, 5);
			target = MINI_FTNPTR_TO_ADDR (target);
			if(riscv_is_jal_disp(code,target))
				riscv_jal(code, rd, riscv_get_jal_disp(code,target));
			else{
				gpointer thunk;
				thunk = create_thunk (cfg, code, target);
				g_assert (riscv_is_jal_disp (code, thunk));
				riscv_jal (code, rd, riscv_get_jal_disp(code,thunk));
			}
			break;
		}
		case MONO_R_RISCV_BEQ:
		case MONO_R_RISCV_BNE:
		case MONO_R_RISCV_BGE:
		case MONO_R_RISCV_BGEU:
		case MONO_R_RISCV_BLTU:{
			int offset = target - code;
			g_assert (RISCV_VALID_B_IMM ((gint32) (gssize) (offset)));

			gint32 inst = *(gint32 *) code;
			gint32 rs1 = RISCV_BITS (inst, 15, 5);
			gint32 rs2 = RISCV_BITS (inst, 20, 5);
			if(relocation == MONO_R_RISCV_BEQ)
				riscv_beq (code, rs1, rs2, offset);
			else if(relocation == MONO_R_RISCV_BNE)
				riscv_bne (code, rs1, rs2, offset);
			else if(relocation == MONO_R_RISCV_BGE)
				riscv_bge (code, rs1, rs2, offset);
			else if(relocation == MONO_R_RISCV_BGEU)
				riscv_bgeu (code, rs1, rs2, offset);
			else if(relocation == MONO_R_RISCV_BLTU)
				riscv_bltu (code, rs1, rs2, offset);
			else
				g_assert_not_reached();
			break;
		}
		default:
			NOT_IMPLEMENTED;
	}
}

static void
riscv_patch_rel (guint8 *code, guint8 *target, int relocation)
{
	riscv_patch_full (NULL, code, target, relocation);
}

void
mono_riscv_patch (guint8 *code, guint8 *target, int relocation)
{
	riscv_patch_rel (code, target, relocation);
}

void
mono_arch_patch_code_new (MonoCompile *cfg, guint8 *code,
                          MonoJumpInfo *ji, gpointer target)
{
	guint8 *ip;

	ip = ji->ip.i + code;
	switch (ji->type){
		case MONO_PATCH_INFO_METHOD_JUMP:
			/* ji->relocation is not set by the caller */
			riscv_patch_full (cfg, ip, (guint8*)target, MONO_R_RISCV_JAL);
			mono_arch_flush_icache (ip, 8);
			break;
		case MONO_PATCH_INFO_NONE:
			break;
		default:
			riscv_patch_full (cfg, ip, (guint8*)target, ji->relocation);
			break;
	}
}


/**
 * add_int32_arg:
 * 	Add Arguments into a0-a7 reg. 
 * 	if there is no available store it into stack.
 */
static void
add_arg(CallInfo *cinfo, ArgInfo *ainfo, int size, gboolean sign) {
	g_assert(size <= 8);
	g_assert(cinfo->next_areg >= RISCV_A0);

	if(cinfo->vararg){
		NOT_IMPLEMENTED;
	}

	// for RV64, length of all arg here will not wider then XLEN
	// store it normally.
	if(cinfo->next_areg <= RISCV_A7){
		// there is at least 1 avaliable reg
#ifdef TARGET_RISCV32
		// Scalars that are 2×XLEN bits wide are passed in a pair of argument registers
		if(size == 8){
			NOT_IMPLEMENTED;
			// If exactly one register is available, the low-order XLEN bits are 
			// passed in the register and the high-order XLEN bits are passed on the stack
			if(cinfo->next_areg == RISCV_A7){
				NOT_IMPLEMENTED;
			}
			return;
		}
#endif
		ainfo->storage = ArgInIReg;
		ainfo->reg = cinfo->next_areg;
		cinfo->next_areg ++;
	}
	// If no argument registers are available, the scalar is passed on the stack by value
	else{
		cinfo->stack_usage += size;
		ainfo->storage = ArgOnStack;
		ainfo->slot_size = size;
		ainfo->offset = cinfo->stack_usage;
		ainfo->is_signed = sign;
	}

}

static void
add_valuetype (CallInfo *cinfo, ArgInfo *ainfo, MonoType *t){
	int i, size, aligned_size;
	guint32 align;

	size = mini_type_stack_size_full (t, &align, cinfo->pinvoke);
	aligned_size = ALIGN_TO (size, align);

	// Scalars wider than 2×XLEN bits are passed by reference
	if (aligned_size > sizeof(host_mgreg_t)*2) {
		ainfo->storage = ArgVtypeOnStack;
		cinfo->stack_usage += aligned_size;
		ainfo->slot_size = size;
		ainfo->offset = cinfo->stack_usage;
	}
	// Scalars that are 2×XLEN bits wide are passed in a pair of argument registers
	else if(aligned_size == sizeof(host_mgreg_t)*2){
		// If no argument registers are available, the scalar is passed on the stack by value
		if(cinfo->next_areg > RISCV_A7){
			ainfo->storage = ArgVtypeOnStack;
			cinfo->stack_usage += sizeof(host_mgreg_t)*2;
			ainfo->slot_size = sizeof(host_mgreg_t)*2;
			ainfo->offset = cinfo->stack_usage;
		}
		// If exactly one register is available, the low-order XLEN bits are 
		// passed in the register and the high-order XLEN bits are passed on the stack
		else if(cinfo->next_areg == RISCV_A7){
			ainfo->storage = ArgVtypeInMixed;
			cinfo->stack_usage += sizeof(host_mgreg_t);
			ainfo->slot_size = sizeof(host_mgreg_t);
			ainfo->offset = cinfo->stack_usage;

			ainfo->reg = cinfo->next_areg;
			ainfo->size = sizeof(host_mgreg_t);
			ainfo->is_regpair = FALSE;

			cinfo->next_areg++;
		}
		// Scalars that are 2×XLEN bits wide are passed in a pair of argument 
		// registers, with the low-order XLEN bits in the lower-numbered register
		// and the high-order XLEN bits in the higher-numbered register
		else{
			ainfo->storage = ArgVtypeInIReg;
			ainfo->reg = cinfo->next_areg;
			ainfo->size = sizeof(host_mgreg_t)*2;
			ainfo->is_regpair = TRUE;

			cinfo->next_areg += 2;
		}
	}
	// Scalars that are at most XLEN bits wide are passed in a single argument register
	else{
		ainfo->storage = ArgVtypeInIReg;
		ainfo->reg = cinfo->next_areg;
		ainfo->size = sizeof(host_mgreg_t);
		ainfo->is_regpair = TRUE;

		cinfo->next_areg = 1;
	}
}

static void
add_param (CallInfo *cinfo, ArgInfo *ainfo, MonoType *t){
	MonoType *ptype;

	ptype = mini_get_underlying_type (t);
	// FIXME: May break some ABI rules
	switch (ptype->type){
		case MONO_TYPE_VOID:
			ainfo->storage = ArgNone;
			break;
		case MONO_TYPE_I1:
			add_arg (cinfo, ainfo, 1, TRUE);
			break;
		case MONO_TYPE_U1:
			add_arg (cinfo, ainfo, 1, FALSE);
			break;
		case MONO_TYPE_I2:
			add_arg (cinfo, ainfo, 2, TRUE);
			break;
		case MONO_TYPE_U2:
			add_arg (cinfo, ainfo, 2, FALSE);
			break;
		case MONO_TYPE_I:
		case MONO_TYPE_I4:
			add_arg (cinfo, ainfo, 4, TRUE);
			break;
		case MONO_TYPE_U:
		case MONO_TYPE_U4:
#ifdef TARGET_RISCV32
		case MONO_TYPE_PTR:
		case MONO_TYPE_OBJECT:
#endif
			add_arg (cinfo, ainfo, 4, FALSE);
			break;
		case MONO_TYPE_I8:
			add_arg (cinfo, ainfo, 8, TRUE);
			break;
		case MONO_TYPE_U8:
#ifdef TARGET_RISCV64
		case MONO_TYPE_PTR:
		case MONO_TYPE_OBJECT:
#endif
			add_arg (cinfo, ainfo, 8, FALSE);
			break;
		case MONO_TYPE_GENERICINST:{
			if (!mono_type_generic_inst_is_valuetype (ptype)) {
				add_arg (cinfo, ainfo, sizeof(host_mgreg_t), FALSE);
			} else if (mini_is_gsharedvt_variable_type (ptype)) {
				NOT_IMPLEMENTED;
			} else {
				add_valuetype (cinfo, ainfo, ptype);
			}
			break;
		}
		case MONO_TYPE_VALUETYPE:
			add_valuetype (cinfo, ainfo, ptype);
			break;
		
		default:
			g_print ("Can't handle as return value 0x%x\n", ptype->type);
			g_assert_not_reached();
			break;
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

	cinfo->nargs = paramNum;

	// return value
	cinfo->next_areg = RISCV_A0;
	add_param (cinfo, &cinfo->ret, sig->ret);

	// reset status
	cinfo->next_areg = RISCV_A0;
	cinfo->stack_usage = 0;

	// add this pointer as first argument if hasthis == true
	if (sig->hasthis)
		add_arg (cinfo, cinfo->args + 0, 8, FALSE);

	// other general Arguments
	guint32 paramStart = 0;
	for(guint32 i = paramStart; i < sig->param_count; ++i){
		ArgInfo *ainfo = cinfo->args + sig->hasthis + i;

		// process the variable parameter sig->sentinelpos mark the first VARARG
		if ((sig->call_convention == MONO_CALL_VARARG) && (i == sig->sentinelpos)) {
			NOT_IMPLEMENTED;
		}

		add_param (cinfo, ainfo, sig->params [i]);
	}

	cinfo->stack_usage = ALIGN_TO (cinfo->stack_usage, MONO_ARCH_FRAME_ALIGNMENT);
	
	return cinfo;
}

static int
arg_need_temp (ArgInfo *ainfo)
{
	if (ainfo->storage == ArgVtypeInMixed)
		return sizeof(host_mgreg_t) *2;
	return 0;
}

static gpointer
arg_get_storage (CallContext *ccontext, ArgInfo *ainfo){
	switch (ainfo->storage) {
		case ArgInIReg:
		case ArgVtypeInIReg:
			return &ccontext->gregs [ainfo->reg];
		case ArgInFReg:
			return &ccontext->fregs [ainfo->reg];
		case ArgOnStack:
		case ArgVtypeOnStack:
			return ccontext->stack + ainfo->offset;
		case ArgVtypeByRef:
			return (gpointer) ccontext->gregs [ainfo->reg];
		default:
			g_print("Can't process storage type %d\n", ainfo->storage);
			NOT_IMPLEMENTED;
	}
}

static void
arg_set_val (CallContext *ccontext, ArgInfo *ainfo, gpointer src)
{
	g_assert (arg_need_temp (ainfo));
	NOT_IMPLEMENTED;
}

static void
arg_get_val (CallContext *ccontext, ArgInfo *ainfo, gpointer dest)
{
	g_assert (arg_need_temp (ainfo));
	NOT_IMPLEMENTED;
}


/* Set arguments in the ccontext (for i2n entry) */
void
mono_arch_set_native_call_context_args (CallContext *ccontext, gpointer frame, MonoMethodSignature *sig)
{
	const MonoEECallbacks *interp_cb = mini_get_interp_callbacks ();
	CallInfo *cinfo = get_call_info (NULL, sig);
	gpointer storage;
	ArgInfo *ainfo;

	memset (ccontext, 0, sizeof (CallContext));

	ccontext->stack_size = ALIGN_TO (cinfo->stack_usage, MONO_ARCH_FRAME_ALIGNMENT);
	if (ccontext->stack_size)
		ccontext->stack = (guint8*)g_calloc (1, ccontext->stack_size);

	if (sig->ret->type != MONO_TYPE_VOID){
		ainfo = &cinfo->ret;
		if (ainfo->storage == ArgVtypeByRef) {
			storage = interp_cb->frame_arg_to_storage ((MonoInterpFrameHandle)frame, sig, -1);
			ccontext->gregs [cinfo->ret.reg] = (gsize)storage;
		}
	}

	g_assert (!sig->hasthis);

	for (int i = 0; i < sig->param_count; i++){
		ainfo = &cinfo->args [i];

		if (ainfo->storage == ArgVtypeByRef){
			ccontext->gregs [ainfo->reg] = (host_mgreg_t)interp_cb->frame_arg_to_storage ((MonoInterpFrameHandle)frame, sig, i);
			continue;
		}

		int temp_size = arg_need_temp (ainfo);

		if (temp_size)
			storage = alloca (temp_size); // FIXME? alloca in a loop
		else
			storage = arg_get_storage (ccontext, ainfo);

		interp_cb->frame_arg_to_data ((MonoInterpFrameHandle)frame, sig, i, storage);
		if (temp_size)
			arg_set_val (ccontext, ainfo, storage);
	}

	g_free (cinfo);
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
	const MonoEECallbacks *interp_cb;
	CallInfo *cinfo;
	ArgInfo *ainfo;
	gpointer storage;

	if (sig->ret->type == MONO_TYPE_VOID)
		return;

	interp_cb = mini_get_interp_callbacks ();
	cinfo = get_call_info (NULL, sig);
	ainfo = &cinfo->ret;

	if (ainfo->storage != ArgVtypeByRef){
		int temp_size = arg_need_temp (ainfo);

		if (temp_size) {
			storage = alloca (temp_size);
			arg_get_val (ccontext, ainfo, storage);
		}
		else{
			storage = arg_get_storage (ccontext, ainfo);
		}
		interp_cb->data_to_frame_arg ((MonoInterpFrameHandle)frame, sig, -1, storage);
	}

	g_free (cinfo);
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
	case OP_IMUL:
	case OP_MUL_IMM:
#ifdef TARGET_RISCV64
	case OP_LMUL_IMM:
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

gint
mono_arch_get_memory_ordering(int memory_barrier_kind){
	gint ordering;
	switch (memory_barrier_kind){
		case MONO_MEMORY_BARRIER_ACQ:
			ordering = RISCV_ORDER_AQ;
			break;
		case MONO_MEMORY_BARRIER_REL:
			ordering = RISCV_ORDER_RL;
			break;
		case MONO_MEMORY_BARRIER_SEQ:
			ordering = RISCV_ORDER_ALL;
		default:
			ordering = RISCV_ORDER_NONE;
			break;
	}
	return ordering;
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

	MonoMethodSignature *sig;
	CallInfo *cinfo;
	
	sig = mono_method_signature_internal (cfg->method);
	if (!cfg->arch.cinfo)
		cfg->arch.cinfo = get_call_info (cfg->mempool, sig);
	cinfo = cfg->arch.cinfo;

	if (cinfo->ret.storage == ArgVtypeByRef) {
		cfg->vret_addr = mono_compile_create_var (cfg, mono_get_int_type (), OP_LOCAL);
		cfg->vret_addr->flags |= MONO_INST_VOLATILE;
	}

	if (cfg->gen_sdb_seq_points) {
		MonoInst *ins;

		if (cfg->compile_aot) {
			ins = mono_compile_create_var (cfg, mono_get_int_type (), OP_LOCAL);
			ins->flags |= MONO_INST_VOLATILE;
			cfg->arch.seq_point_info_var = ins;
		}

		ins = mono_compile_create_var (cfg, mono_get_int_type (), OP_LOCAL);
		ins->flags |= MONO_INST_VOLATILE;
		cfg->arch.ss_tramp_var = ins;

		ins = mono_compile_create_var (cfg, mono_get_int_type (), OP_LOCAL);
		ins->flags |= MONO_INST_VOLATILE;
		cfg->arch.bp_tramp_var = ins;
	}

	if (0) {
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

static void
add_outarg_reg (MonoCompile *cfg, MonoCallInst *call, ArgStorage storage, int reg, MonoInst *tree){
	MonoInst *ins;

	switch (storage) {
		default:
			g_print("unable process storage type %d\n",storage);
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

/* 
 * take the arguments and generate the arch-specific
 * instructions to properly call the function in call.
 * This includes pushing, moving arguments to the right register
 * etc.
 */
static void
emit_sig_cookie (MonoCompile *cfg, MonoCallInst *call, CallInfo *cinfo){
	NOT_IMPLEMENTED;
	MonoMethodSignature *tmp_sig;
	MonoInst *sig_arg;

	if (MONO_IS_TAILCALL_OPCODE (call))
		NOT_IMPLEMENTED;

	/*
	 * mono_ArgIterator_Setup assumes the signature cookie is 
	 * passed first and all the arguments which were before it are
	 * passed on the stack after the signature. So compensate by 
	 * passing a different signature.
	 */
	tmp_sig = mono_metadata_signature_dup (call->signature);
	tmp_sig->param_count -= call->signature->sentinelpos;
	tmp_sig->sentinelpos = 0;
	memcpy (tmp_sig->params, call->signature->params + call->signature->sentinelpos, tmp_sig->param_count * sizeof (MonoType*));

	MONO_INST_NEW (cfg, sig_arg, OP_ICONST);
	sig_arg->dreg = mono_alloc_ireg (cfg);
	sig_arg->inst_p0 = tmp_sig;
	MONO_ADD_INS (cfg->cbb, sig_arg);

	MONO_EMIT_NEW_STORE_MEMBASE (cfg, OP_STORE_MEMBASE_REG, RISCV_SP, cinfo->sig_cookie.offset, sig_arg->dreg);
}

/**
 * mono_arch_emit_call:
 * 	we process all Args of a function call
 *  (return, parameters)
 */
void
mono_arch_emit_call (MonoCompile *cfg, MonoCallInst *call)
{
	MonoInst *arg, *vtarg;
	MonoMethodSignature *sig;
	ArgInfo *ainfo;
	CallInfo *cinfo;

	sig = call->signature;
	int paramNum = sig->param_count + sig->hasthis;

	cinfo = get_call_info (cfg->mempool, sig);

	/* Emit the inst of return at mono_arch_emit_setret() */
	switch (cinfo->ret.storage){
		case ArgVtypeInIReg:
			if (MONO_IS_TAILCALL_OPCODE (call))
				break;
			/*
			* The vtype is returned in registers, save the return area address in a local, and save the vtype into
			* the location pointed to by it after call in emit_move_return_value ().
			*/
			if (!cfg->arch.vret_addr_loc){
				cfg->arch.vret_addr_loc = mono_compile_create_var (cfg, mono_get_int_type (), OP_LOCAL);
				/* Prevent it from being register allocated or optimized away */
				cfg->arch.vret_addr_loc->flags |= MONO_INST_VOLATILE;
			}
			MONO_EMIT_NEW_UNALU (cfg, OP_MOVE, cfg->arch.vret_addr_loc->dreg, call->vret_var->dreg);
			break;
		case ArgVtypeByRef:
			g_assert (!MONO_IS_TAILCALL_OPCODE (call) || call->vret_var == cfg->vret_addr);
			MONO_INST_NEW (cfg, vtarg, OP_MOVE);
			vtarg->sreg1 = call->vret_var->dreg;
			vtarg->dreg = mono_alloc_preg (cfg);
			MONO_ADD_INS (cfg->cbb, vtarg);
			mono_call_inst_add_outarg_reg (cfg, call, vtarg->dreg, cinfo->ret.reg, FALSE);
			break;
		case ArgVtypeByRefOnStack:
			NOT_IMPLEMENTED;
			break;
		case ArgVtypeOnStack:
			NOT_IMPLEMENTED;
			break;
		case ArgVtypeInMixed:
			NOT_IMPLEMENTED;
			break;
		default:
			break;
	}

	// if (cinfo->struct_ret)
	// 	// call->used_iregs |= 1 << cinfo->struct_ret;
	// 	NOT_IMPLEMENTED;

	if (COMPILE_LLVM (cfg)) {
		/* We shouldn't be called in the llvm case */
		cfg->disable_llvm = TRUE;
		return;
	}

	for (int i = 0; i < paramNum; i++)
	{
		ainfo = cinfo->args + i;
		MonoType *t;

		if (sig->hasthis && i == 0)
			t = mono_get_object_type ();
		else
			t = sig->params [i - sig->hasthis];
		t = mini_get_underlying_type (t);

		if ((sig->call_convention == MONO_CALL_VARARG) && (i == sig->sentinelpos)) {
			/* Emit the signature cookie just before the implicit arguments */
			emit_sig_cookie (cfg, call, cinfo);
		}

		arg = call->args [i];
		switch (ainfo->storage){
			case ArgInIReg:
				add_outarg_reg (cfg, call, ainfo->storage, ainfo->reg, arg);
				break;
			case ArgOnStack:{
				switch (ainfo->slot_size){
				case 1:
					MONO_EMIT_NEW_STORE_MEMBASE (cfg, OP_STOREI1_MEMBASE_REG, RISCV_FP, -ainfo->offset, arg->dreg);
					break;
				case 2:
					MONO_EMIT_NEW_STORE_MEMBASE (cfg, OP_STOREI2_MEMBASE_REG, RISCV_FP, -ainfo->offset, arg->dreg);
					break;
				case 4:
					MONO_EMIT_NEW_STORE_MEMBASE (cfg, OP_STOREI4_MEMBASE_REG, RISCV_FP, -ainfo->offset, arg->dreg);
					break;
				case 8:
#ifdef TARGET_RISCV32
 					NOT_IMPLEMENTED;
					break;
#else // TARGET_RISCV64
					MONO_EMIT_NEW_STORE_MEMBASE (cfg, OP_STOREI8_MEMBASE_REG, RISCV_FP, -ainfo->offset, arg->dreg);
					break;
				// RV64 Only, XLEN*2 == 16
				case 16:
					MONO_EMIT_NEW_STORE_MEMBASE (cfg, OP_STOREI8_MEMBASE_REG, RISCV_FP, -ainfo->offset, arg->dreg);
					MONO_EMIT_NEW_STORE_MEMBASE (cfg, OP_STOREI8_MEMBASE_REG, RISCV_FP, -ainfo->offset + 8, arg->dreg+1);
					break;
#endif
				default:
					g_assert_not_reached();
 				}
			}
			case ArgVtypeInIReg :{
				MonoInst *ins;
				guint32 align;
				guint32 size;
				size = mono_class_value_size (arg->klass, &align);

				MONO_INST_NEW (cfg, ins, OP_OUTARG_VT);
				ins->sreg1 = arg->dreg;
				ins->klass = arg->klass;
				ins->backend.size = size;
				ins->inst_p0 = call;
				ins->inst_p1 = mono_mempool_alloc (cfg->mempool, sizeof (ArgInfo));
				memcpy (ins->inst_p1, ainfo, sizeof (ArgInfo));
				MONO_ADD_INS (cfg->cbb, ins);

				break;
			}
			default:
				g_print("can't process Storage type %d\n",ainfo->storage);
				NOT_IMPLEMENTED;
		}
	}
	
	/* Handle the case where there are no implicit arguments */
	if (!sig->pinvoke && (sig->call_convention == MONO_CALL_VARARG) && (paramNum == sig->sentinelpos))
		emit_sig_cookie (cfg, call, cinfo);

	call->call_info = cinfo;
	call->stack_usage = cinfo->stack_usage;
}

void
mono_arch_emit_outarg_vt (MonoCompile *cfg, MonoInst *ins, MonoInst *src)
{
	MonoCallInst *call = (MonoCallInst*)ins->inst_p0;
	ArgInfo *ainfo = (ArgInfo*)ins->inst_p1;
	MonoInst *load;
	int i,op_load = 0;

#ifdef TARGET_RISCV64
	op_load = OP_LOADI8_MEMBASE;
#else //TARGET_RISCV32
	op_load = OP_LOADI4_MEMBASE;
#endif

	if (ins->backend.size == 0)
		return;

	switch (ainfo->storage){
		case ArgVtypeInIReg:{
			MONO_INST_NEW (cfg, load, op_load);
			load->dreg = mono_alloc_ireg (cfg);
			load->inst_basereg = src->dreg;
			load->inst_offset = sizeof (host_mgreg_t);
			MONO_ADD_INS (cfg->cbb, load);
			add_outarg_reg (cfg, call, ArgInIReg, ainfo->reg, load);

			if(ainfo->size > sizeof (host_mgreg_t)){
				MONO_INST_NEW (cfg, load, op_load);
				load->dreg = mono_alloc_ireg (cfg);
				load->inst_basereg = src->dreg;
				load->inst_offset = 2 * sizeof (target_mgreg_t);
				MONO_ADD_INS (cfg->cbb, load);
				add_outarg_reg (cfg, call, ArgInIReg, ainfo->reg + 1, load);
			}
		}
		case ArgVtypeOnStack:{
			g_assert(ainfo->offset >= 0);
			for (i = 0; i < ainfo->slot_size; i += sizeof (target_mgreg_t)) {
				MONO_INST_NEW (cfg, load, op_load);
				load->dreg = mono_alloc_ireg (cfg);
				load->inst_basereg = src->dreg;
				load->inst_offset = i;
				MONO_ADD_INS (cfg->cbb, load);
				MONO_EMIT_NEW_STORE_MEMBASE (cfg, op_load, RISCV_FP, -ainfo->offset + i, load->dreg);
			}
			break;
		}
		default:
			NOT_IMPLEMENTED;
			break;
	}
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
			g_print("can't process Storage type %d\n",cinfo->ret.storage);
			NOT_IMPLEMENTED;
	}
}

void
mono_arch_decompose_opts (MonoCompile *cfg, MonoInst *ins)
{
	switch (ins->opcode) {
		case OP_LADD:
		case OP_LADD_IMM:
		case OP_IADD_IMM:
		case OP_LAND_IMM:
		case OP_ISUB:
		case OP_ISUB_IMM:
		case OP_LOR:
		case OP_ICONV_TO_I:
		case OP_ICONV_TO_U:
		case OP_ICONV_TO_I1:
		case OP_ICONV_TO_U1:
		case OP_ICONV_TO_I2:
		case OP_ICONV_TO_U2:
#ifdef TARGET_RISCV64
		case OP_ICONV_TO_I4:
		case OP_ICONV_TO_U4:
#endif
		case OP_LCONV_TO_I:
		case OP_LMUL_IMM:

		case OP_LADD_OVF_UN:
		case OP_LMUL_OVF_UN_OOM:
			break;
		default:
			g_print("Can't decompose the OP %s\n",mono_inst_name (ins->opcode));
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
	MonoInst *ins;
	CallInfo *cinfo;
	ArgInfo *ainfo;
	int offset, size, align;
	guint32 locals_stack_size, locals_stack_align;
	gint32 *local_stack;

	/*
	 * Allocate arguments and locals to either register (OP_REGVAR) or to a stack slot (OP_REGOFFSET).
	 * Compute cfg->stack_offset and update cfg->used_int_regs.
	 */
	sig = mono_method_signature_internal (cfg->method);

	if (!cfg->arch.cinfo)
		cfg->arch.cinfo = get_call_info (cfg->mempool, sig);
	cinfo = cfg->arch.cinfo;

	offset = 0;
	// save RA & FP reg to stack
	cfg->frame_reg = RISCV_FP;
	offset += sizeof (host_mgreg_t) * 2;

	if (0) {
		/* Save all callee-saved registers normally, and restore them when unwinding through an LMF */
		cfg->arch.saved_iregs |= MONO_ARCH_CALLEE_SAVED_REGS;
	}
	else {
		/* Callee saved regs */
		// saved_gregs_offset store the addr of firs reg
		for (guint i = 0; i < 32; ++i)
			if ((MONO_ARCH_CALLEE_SAVED_REGS & (1 << i)) && (cfg->used_int_regs & (1 << i))){
				g_print("save callee saved reg %s to %ld(s0/fp).\n", mono_arch_regname (i), -offset);
				offset += sizeof (host_mgreg_t);
			}
		cfg->arch.saved_gregs_offset = offset;
	}

	/* Return value */
	if (sig->ret->type != MONO_TYPE_VOID) {
		switch (cinfo->ret.storage){
			case ArgNone:
				break;
			case ArgInIReg:
				cfg->ret->opcode = OP_REGVAR;
				cfg->ret->inst_c0 = cinfo->ret.reg;
				cfg->ret->dreg = cinfo->ret.reg;
				break;
			case ArgVtypeInIReg:
				/* Allocate a local to hold the result, the epilog will copy it to the correct place */
				cfg->ret->opcode = OP_REGOFFSET;
				cfg->ret->inst_basereg = cfg->frame_reg;
				cfg->ret->inst_offset = offset;
				offset += sizeof (host_mgreg_t) * 2;
				break;
			default:
				g_print("Can't handle storage type %d\n",cinfo->ret.storage);
				NOT_IMPLEMENTED;
				break;
		}
	}

	/* Arguments */
	for (guint i = 0; i < sig->param_count + sig->hasthis; ++i){
		ainfo = cinfo->args + i;

		ins = cfg->args [i];

		if (ins->opcode == OP_REGVAR)
			continue;

		ins->opcode = OP_REGOFFSET;
		ins->inst_basereg = cfg->frame_reg;

		switch (ainfo->storage){
			case ArgInIReg:
			case ArgInFReg:
				offset += sizeof (host_mgreg_t);
				ins->inst_offset = -offset;
				break;
			case ArgOnStack:
			case ArgVtypeOnStack:
				/* These are in the parent frame */
				NOT_IMPLEMENTED;
				break;
			case ArgVtypeInIReg:
				ins->opcode = OP_REGOFFSET;
				ins->inst_basereg = cfg->frame_reg;
				/* These arguments are saved to the stack in the prolog */
				if(ainfo->is_regpair)
					offset += sizeof (host_mgreg_t);
				offset += sizeof (host_mgreg_t);
				ins->inst_offset = -offset;
				break;
			default:
				NOT_IMPLEMENTED;
				break;
		}
		if (cfg->verbose_level >= 2)
			g_print ("arg %d allocated to %ld(%s).\n", i, ins->inst_offset, mono_arch_regname (ins->inst_basereg));
	}

	/* OP_SEQ_POINT depends on these */
	// FIXME: Allocate these to registers
	ins = cfg->arch.seq_point_info_var;
	if (ins) {
		size = sizeof (host_mgreg_t);
		align = sizeof (host_mgreg_t);
		offset += align - 1;
		offset &= ~(align - 1);
		ins->opcode = OP_REGOFFSET;
		ins->inst_basereg = cfg->frame_reg;
		offset += size;
		ins->inst_offset = -offset;
		g_print("alloc seq_point_info_var to %ld(%s).\n", ins->inst_offset, mono_arch_regname (ins->inst_basereg));
	}
	ins = cfg->arch.ss_tramp_var;
	if (ins) {
		size = sizeof (host_mgreg_t);
		align = sizeof (host_mgreg_t);
		offset += align - 1;
		offset &= ~(align - 1);
		ins->opcode = OP_REGOFFSET;
		ins->inst_basereg = cfg->frame_reg;
		offset += size;
		ins->inst_offset = -offset;
		g_print("alloc ss_tramp_var to %ld(%s).\n", ins->inst_offset, mono_arch_regname (ins->inst_basereg));
	}
	ins = cfg->arch.bp_tramp_var;
	if (ins) {
		size = sizeof (host_mgreg_t);
		align = sizeof (host_mgreg_t);
		offset += align - 1;
		offset &= ~(align - 1);
		ins->opcode = OP_REGOFFSET;
		ins->inst_basereg = cfg->frame_reg;
		offset += size;
		ins->inst_offset = -offset;
		g_print("alloc bp_tramp_var to %ld(%s).\n", ins->inst_offset, mono_arch_regname (ins->inst_basereg));
	}

	/* Allocate locals */
	local_stack = mono_allocate_stack_slots (cfg, FALSE, &locals_stack_size, &locals_stack_align);
	if (locals_stack_align)
		offset = ALIGN_TO (offset, locals_stack_align);

	offset += locals_stack_size;
	for (guint i = cfg->locals_start; i < cfg->num_varinfo; i++){
		if (local_stack [i] != -1) {
			ins = cfg->varinfo [i];
			ins->opcode = OP_REGOFFSET;
			ins->inst_basereg = cfg->frame_reg;
			ins->inst_offset = -offset + local_stack [i];
			g_print ("allocated local %d to %ld(s0/fp); ", i, ins->inst_offset); mono_print_ins (ins);
		}
	}
	offset = ALIGN_TO (offset, MONO_ARCH_FRAME_ALIGNMENT);

	cfg->stack_offset = offset;

	g_print("Stack size: %ld\n",offset);
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
	MonoInst *ins, *last_ins,*n, *temp;
	if (cfg->verbose_level > 2) {
		// int idx = 0;

		g_print ("BASIC BLOCK %d (before lowering)\n", bb->block_num);
		MONO_BB_FOR_EACH_INS (bb, ins) {
			mono_print_ins (ins);
		}
		
	}

	MONO_BB_FOR_EACH_INS_SAFE (bb, n, ins){
loop_start:
		switch (ins->opcode){
			case OP_IL_SEQ_POINT:
			case OP_SEQ_POINT:
			case OP_GC_SAFE_POINT:
			case OP_BR:
			case OP_CALL:
			case OP_LCALL:
			case OP_VCALL2:
			case OP_VOIDCALL:
			case OP_START_HANDLER:
			case OP_CALL_HANDLER:
			case OP_ENDFINALLY:
			case OP_GET_EX_OBJ:
			case OP_GENERIC_CLASS_INIT:
			case OP_I8CONST:
			case OP_ICONST:
			case OP_MOVE:
			case OP_ISUB:
			case OP_IADD:
			case OP_LADD:
			case OP_CHECK_THIS:
			case OP_XOR_IMM:
			case OP_LOR:
			case OP_LOR_IMM:
			case OP_SHL_IMM:
			case OP_LSHL_IMM:
			case OP_SHR_IMM:
			case OP_SHR_UN_IMM:
			case OP_LOCALLOC:
			
			/* skip dummy IL */
			case OP_NOT_REACHED:
			case OP_NOT_NULL:
			case OP_DUMMY_USE:
			case OP_NOP:

			/* skip custom OP code*/
			case OP_RISCV_BEQ:
			case OP_RISCV_BNE:
			case OP_RISCV_BGE:
			case OP_RISCV_BGEU:
			case OP_RISCV_BLT:
			case OP_RISCV_BLTU:
			case OP_RISCV_EXC_BEQ:
			case OP_RISCV_EXC_BNE:
			case OP_RISCV_EXC_BGEU:
			case OP_RISCV_EXC_BLTU:
			case OP_RISCV_SLT:
			case OP_RISCV_SLTU:
			case OP_RISCV_SLTIU:
				break;
			
			/* Atomic Ext */
			case OP_MEMORY_BARRIER:
			case OP_ATOMIC_STORE_I4:
			case OP_ATOMIC_CAS_I4:
			case OP_ATOMIC_CAS_I8:
				break;

			case OP_LOCALLOC_IMM:{
				if (ins->inst_imm > 32){
					NEW_INS (cfg, ins, temp, OP_ICONST);
					temp->inst_c0 = ins->inst_imm;
					temp->dreg = mono_alloc_ireg (cfg);
					ins->sreg1 = temp->dreg;
					ins->opcode = OP_LOCALLOC;
				}
				break;
			}

			case OP_CALL_MEMBASE:
			case OP_VCALL2_MEMBASE:
			case OP_VOIDCALL_MEMBASE:
				if(!RISCV_VALID_J_IMM(ins->inst_offset)){
					NOT_IMPLEMENTED;
					NEW_INS (cfg, ins, temp, OP_ICONST);
					temp->dreg = mono_alloc_ireg (cfg);
					temp->inst_c0 = ins->inst_offset;

					ins->sreg1 = temp->dreg;

					NEW_INS (cfg, ins, temp, OP_LADD);
					temp->dreg = ins->sreg1;
					temp->sreg1 = ins->sreg1;
					temp->sreg2 = ins->inst_basereg;

					ins->inst_basereg = -1;
					ins->inst_offset = 0;
					ins->opcode = OP_VOIDCALL_REG;
				}
				break;
			case OP_VOIDCALL_REG:
				break;
			
			/* Throw */
			case OP_THROW:
			case OP_RETHROW:
				if (ins->sreg1 != RISCV_A0){
					NEW_INS (cfg, ins, temp, OP_MOVE);
					temp->dreg = RISCV_A0;
					temp->sreg1 = ins->sreg1;
					ins->sreg1 = RISCV_A0;
				}
				break;
			// RISC-V dosn't support store Imm to Memory directly
			// store Imm into Reg firstly.
			case OP_STOREI1_MEMBASE_IMM:
			case OP_STOREI4_MEMBASE_IMM:
#ifdef TARGET_RISCV64
			case OP_STOREI8_MEMBASE_IMM:
#endif
			case OP_STORE_MEMBASE_IMM:{
				if(ins->inst_imm != 0){
					NEW_INS (cfg, ins, temp, OP_ICONST);
					temp->inst_c0 = ins->inst_imm;
					temp->dreg = mono_alloc_ireg (cfg);
					
					ins->sreg1 = temp->dreg;
				}
				else{
					ins->sreg1 = RISCV_ZERO;
				}
				
				switch (ins->opcode){
					case OP_STORE_MEMBASE_IMM:
						ins->opcode = OP_STORE_MEMBASE_REG;
						break;
					case OP_STOREI1_MEMBASE_IMM:
						ins->opcode = OP_STOREI1_MEMBASE_REG;
						break;
					case OP_STOREI4_MEMBASE_IMM:
						ins->opcode = OP_STOREI4_MEMBASE_REG;
						break;
#ifdef TARGET_RISCV64
					case OP_STOREI8_MEMBASE_IMM:
						ins->opcode = OP_STOREI8_MEMBASE_REG;
						break;
#endif
					default:
						g_assert_not_reached();
						break;
				}
				goto loop_start; /* make it handle the possibly big ins->inst_offset */
			}
			// Inst S{B|H|W|D} use I-type Imm
			case OP_STOREI1_MEMBASE_REG:
			case OP_STOREI2_MEMBASE_REG:
			case OP_STOREI4_MEMBASE_REG:
#ifdef TARGET_RISCV64
			case OP_STOREI8_MEMBASE_REG:
#endif
			case OP_STORE_MEMBASE_REG:{
				// check if offset is valid I-type Imm
				if(! RISCV_VALID_I_IMM ((gint32) (gssize) (ins->inst_offset)))
					NOT_IMPLEMENTED;
				break;
			}
			// Inst L{B|H|W|D} use I-type Imm
			case OP_LOADI1_MEMBASE:
			case OP_LOADU1_MEMBASE:
			case OP_LOADI2_MEMBASE:
			case OP_LOADU2_MEMBASE:
			case OP_LOADI4_MEMBASE:
			case OP_LOADU4_MEMBASE:
#ifdef TARGET_RISCV64
			case OP_LOADI8_MEMBASE:
#endif		
			case OP_LOAD_MEMBASE:
				if(! RISCV_VALID_I_IMM ((gint32) (gssize) (ins->inst_imm))){
					NEW_INS (cfg, ins, temp, OP_ICONST);
					temp->inst_c0 = ins->inst_imm;
					temp->dreg = mono_alloc_ireg (cfg);
					ins->sreg1 = temp->dreg;
					ins->inst_imm = 0;
				}
				break;

			case OP_COMPARE_IMM:
			case OP_ICOMPARE_IMM:
			case OP_LCOMPARE_IMM:{
				if (ins->next){
					if(ins->next->opcode == OP_LCEQ || ins->next->opcode == OP_ICEQ){
						g_assert(RISCV_VALID_I_IMM(ins->inst_imm));
							
						ins->opcode = OP_ADD_IMM;
						ins->dreg = mono_alloc_ireg (cfg);
						ins->inst_imm = -ins->inst_imm;

						ins->next->opcode = OP_RISCV_SLTIU;
						ins->next->sreg1 = ins->dreg;
						ins->next->inst_imm = 1;
						break;
					}
					else if(ins->next->opcode == OP_LCGT_UN || ins->next->opcode == OP_ICGT_UN){
						g_assert(RISCV_VALID_I_IMM(ins->inst_imm + 1));
						ins->opcode = OP_RISCV_SLTIU;
						ins->dreg = ins->next->dreg;
						ins->sreg1 = ins->sreg1;
						ins->inst_imm = ins->inst_imm + 1;

						ins->next->opcode = OP_XOR_IMM;
						ins->next->dreg = ins->dreg;
						ins->next->sreg1 = ins->dreg;
						ins->next->inst_imm = 1;
						break;
					}
					else if(ins->next->opcode == OP_LCGT || ins->next->opcode == OP_ICGT){
						g_assert(RISCV_VALID_I_IMM(ins->inst_imm + 1));
						ins->opcode = OP_RISCV_SLTI;
						ins->dreg = ins->next->dreg;
						ins->sreg1 = ins->sreg1;
						ins->inst_imm = ins->inst_imm + 1;

						ins->next->opcode = OP_XOR_IMM;
						ins->next->dreg = ins->dreg;
						ins->next->sreg1 = ins->dreg;
						ins->next->inst_imm = 1;
						break;
					}
				}
				else
					g_assert_not_reached();

				if(ins->inst_imm == 0){
					ins->sreg2 = RISCV_ZERO;
				}
				else{
					NEW_INS (cfg, ins, temp, OP_ICONST);
					temp->inst_c0 = ins->inst_imm;
					temp->dreg = mono_alloc_ireg (cfg);
					ins->sreg2 = temp->dreg;
					ins->inst_imm = 0;
				}
				ins->opcode = OP_ICOMPARE;
				goto loop_start;
			}
			case OP_COMPARE:
			case OP_ICOMPARE:
			case OP_LCOMPARE:{
				if (ins->next){
					if(ins->next->opcode == OP_COND_EXC_EQ){
						ins->next->opcode = OP_RISCV_EXC_BEQ;
						ins->next->sreg1 = ins->sreg1;
						ins->next->sreg2 = ins->sreg2;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_COND_EXC_NE_UN){
						ins->next->opcode = OP_RISCV_EXC_BNE;
						ins->next->sreg1 = ins->sreg1;
						ins->next->sreg2 = ins->sreg2;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_COND_EXC_LT_UN){
						ins->next->opcode = OP_RISCV_EXC_BLTU;
						ins->next->sreg1 = ins->sreg1;
						ins->next->sreg2 = ins->sreg2;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_COND_EXC_LE_UN){
						ins->next->opcode = OP_RISCV_EXC_BGEU;
						ins->next->sreg1 = ins->sreg2;
						ins->next->sreg2 = ins->sreg1;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_LBEQ || ins->next->opcode == OP_IBEQ){
						ins->next->opcode = OP_RISCV_BEQ;
						ins->next->sreg1 = ins->sreg1;
						ins->next->sreg2 = ins->sreg2;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_LBNE_UN || ins->next->opcode == OP_IBNE_UN){
						ins->next->opcode = OP_RISCV_BNE;
						ins->next->sreg1 = ins->sreg1;
						ins->next->sreg2 = ins->sreg2;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_LBGE_UN || ins->next->opcode == OP_IBGE_UN){
						ins->next->opcode = OP_RISCV_BGEU;
						ins->next->sreg1 = ins->sreg1;
						ins->next->sreg2 = ins->sreg2;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_LBGE || ins->next->opcode == OP_IBGE){
						ins->next->opcode = OP_RISCV_BGE;
						ins->next->sreg1 = ins->sreg1;
						ins->next->sreg2 = ins->sreg2;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_LBGT_UN || ins->next->opcode == OP_IBGT_UN){
						ins->next->opcode = OP_RISCV_BLTU;
						ins->next->sreg1 = ins->sreg2;
						ins->next->sreg2 = ins->sreg1;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_LBGT || ins->next->opcode == OP_IBGT){
						ins->next->opcode = OP_RISCV_BLT;
						ins->next->sreg1 = ins->sreg2;
						ins->next->sreg2 = ins->sreg1;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_LBLE || ins->next->opcode == OP_IBLE){
						// ble rs1, rs2  -> bge rs2, rs1
						ins->next->opcode = OP_RISCV_BGE;
						ins->next->sreg1 = ins->sreg2;
						ins->next->sreg2 = ins->sreg1;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_LCLT || ins->next->opcode == OP_ICLT){
						ins->next->opcode = OP_RISCV_SLT;
						ins->next->sreg1 = ins->sreg1;
						ins->next->sreg2 = ins->sreg2;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_LCLT_UN || ins->next->opcode == OP_ICLT_UN){
						ins->next->opcode = OP_RISCV_SLTU;
						ins->next->sreg1 = ins->sreg1;
						ins->next->sreg2 = ins->sreg2;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_LCEQ || ins->next->opcode == OP_ICEQ){
						ins->next->opcode = OP_RISCV_SLTIU;
						ins->next->sreg1 = ins->sreg1;
						ins->next->sreg2 = ins->sreg2;
						NULLIFY_INS (ins);
					}
					else if(ins->next->opcode == OP_LCGT_UN || ins->next->opcode == OP_ICGT_UN){
						ins->next->opcode = OP_RISCV_SLTU;
						ins->next->sreg1 = ins->sreg2;
						ins->next->sreg2 = ins->sreg1;
						NULLIFY_INS (ins);
					}
					else {
						g_print("Unhandaled op %d following after OP_{I|L}COMPARE{|_IMM}\n",ins->next->opcode);
						NOT_IMPLEMENTED;
					}
				}
				else{
					g_assert_not_reached();
				}
				break;
			}

			/* Math */
			case OP_SUB_IMM:
			case OP_ISUB_IMM:
				ins->inst_imm = -ins->inst_imm;
				ins->opcode = OP_ADD_IMM;
				goto loop_start;
			// Inst ADDI use I-type Imm
			case OP_ADD_IMM:
			case OP_IADD_IMM:
			case OP_LADD_IMM:
				if(! RISCV_VALID_I_IMM ((gint32) (gssize) (ins->inst_imm))){
					NEW_INS (cfg, ins, temp, OP_ICONST);
					temp->inst_c0 = ins->inst_imm;
					temp->dreg = mono_alloc_ireg (cfg);
					ins->sreg2 = temp->dreg;
					ins->inst_imm = 0;
					// there is no OP_ADD opcode, use OP_IADD instead
					ins->opcode = OP_IADD;
				}
				break;
			case OP_ADDCC:{
				/**
				 * add t0, t1, t2
				 * slti t3, t2, 0
				 * slt t4,t0,t1
				 * bne t3, t4, overflow
				*/
				ins->opcode = OP_IADD;
				MonoInst *branch_ins = ins->next;
				if(branch_ins){
					if(branch_ins->opcode == OP_COND_EXC_C){
						// bne t3, t4, overflow
						branch_ins->opcode = OP_RISCV_EXC_BNE;
						branch_ins->sreg1 = mono_alloc_ireg (cfg);
						branch_ins->sreg2 = mono_alloc_ireg (cfg);

						// slti t3, t2, 0
						NEW_INS (cfg, branch_ins, temp, OP_RISCV_SLTI);
						temp->dreg = branch_ins->sreg1;
						temp->sreg1 = ins->sreg2;
						temp->inst_imm = 0;

						// slt t4,t0,t1
						NEW_INS (cfg, branch_ins, temp, OP_RISCV_SLT);
						temp->dreg = branch_ins->sreg2;
						temp->sreg1 = ins->dreg;
						temp->sreg2 = ins->sreg1;
					}
					else
						g_assert_not_reached();
				}
				break;
			}
			case OP_MUL_IMM:{
				g_assert(riscv_stdext_m);
				NEW_INS (cfg, ins, temp, OP_ICONST);
				temp->inst_c0 = ins->inst_imm;
				temp->dreg = mono_alloc_ireg (cfg);
				ins->sreg2 = temp->dreg;
				ins->inst_imm = 0;
				ins->opcode = OP_IMUL;
				break;
			}


			// Bit OP
			case OP_AND_IMM:
			case OP_LAND_IMM:
				if(! RISCV_VALID_I_IMM ((gint32) (gssize) (ins->inst_imm))){
					NEW_INS (cfg, ins, temp, OP_ICONST);
					temp->inst_c0 = ins->inst_imm;
					temp->dreg = mono_alloc_ireg (cfg);
					ins->sreg2 = temp->dreg;
					ins->inst_imm = 0;
					ins->opcode = OP_IAND;
				}
				break;
			case OP_ICONV_TO_U1:
				// andi rd, rs1, 255
				ins->opcode = OP_AND_IMM;
				ins->inst_imm = 255;
				break;
#ifdef TARGET_RISCV64:
			case OP_SEXT_I4:{
				ins->opcode = OP_RISCV_ADDIW;
				ins->inst_imm = 0;
			}
			case OP_ZEXT_I4:{
				// TODO: Add inst riscv_adduw in riscv-codegen.h
				// if(riscv_stdext_b){
				if(FALSE){
					NOT_IMPLEMENTED;
					ins->opcode = OP_RISCV_ADDUW;
					ins->sreg2 = RISCV_ZERO;
				}
				else{
					// slli a0, a1, 32
					// srli a0, a0, 32
					NEW_INS (cfg, ins, temp, OP_SHL_IMM);
					temp->dreg = ins->dreg;
					temp->sreg1 = ins->sreg1;
					temp->inst_imm = 32;

					ins->opcode = OP_SHR_UN_IMM;
					ins->sreg1 = temp->dreg;
					ins->inst_imm = 32;
				}
				break;
			}
#endif

			default:
				printf ("unable to lowering following IR:"); mono_print_ins (ins);
				NOT_IMPLEMENTED;
				break;
		}
		last_ins = ins;
	}

	bb->last_ins = last_ins;
	bb->max_vreg = cfg->next_vreg;

	if (cfg->verbose_level > 2) {
		// int idx = 0;

		g_print ("BASIC BLOCK %d (after lowering)\n", bb->block_num);
		MONO_BB_FOR_EACH_INS (bb, ins) {
			mono_print_ins (ins);
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

	/**
	 * use LUI & ADDIW load 32 bit Imm
	 * LUI: High 20 bit of imm
	 * ADDIW: Low 12 bit of imm
	*/
	if (RISCV_VALID_IMM (imm)){
		gint32 Hi = RISCV_BITS(imm,12, 20);
		gint32 Lo = RISCV_BITS(imm,0, 12);

		// Lo is in signed num
		// if Lo > 0x800
		// convert into ((Hi + 1) << 20) -  (0x1000 - Lo)
		if(Lo >= 0x800){
			Hi += 1;
			Lo = Lo - 0x1000;
		}

		// if Hi is 0 or overflow, skip
		if(Hi < 0xfffff){
			riscv_lui(code, rd, Hi);
		}
		riscv_addiw(code, rd, rd, Lo);
		return code;
	}

	/*
	 * This is not pretty, but RV64I doesn't make it easy to load constants.
	 * Need to figure out something better.
	 */
	riscv_jal (code, rd, sizeof (guint64) + 4);
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
mono_riscv_emit_nop (guint8 *code){
	// if(riscv_stdext_c){
	// }
	riscv_addi(code, RISCV_ZERO, RISCV_ZERO, 0);
	return code;
}

// Uses at most 16 bytes on RV32I and 24 bytes on RV64I.
// length == 0 means dont care
guint8 *
mono_riscv_emit_load (guint8 *code, int rd, int rs1, gint32 imm, int length)
{
	if (!RISCV_VALID_S_IMM (imm)){
		code = mono_riscv_emit_imm (code, RISCV_T6, imm);
		riscv_add (code, RISCV_T6, rs1, RISCV_T6);
		rs1 = RISCV_T6;
		imm = 0;
	}

	switch (length)
	{
	case 0:
#ifdef TARGET_RISCV64
		riscv_ld (code, rd, rs1, imm);
#else
		riscv_lw (code, rd, rs1, imm);
#endif
		break;
	case 1:
		riscv_lb (code, rd, rs1, imm);
		break;
	case 2:
		riscv_lh (code, rd, rs1, imm);
		break;
	case 4:
		riscv_lw (code, rd, rs1, imm);
		break;
#ifdef TARGET_RISCV64
	case 8:
		riscv_ld (code, rd, rs1, imm);
		break;
#endif
	default:
		g_assert_not_reached();
		break;
	}
	return code;
}

// Uses at most 16 bytes on RV32D and 24 bytes on RV64D.
guint8 *
mono_riscv_emit_fload (guint8 *code, int rd, int rs1, gint32 imm)
{
	if (RISCV_VALID_I_IMM (imm)) {
#ifdef TARGET_RISCV64
		riscv_fld (code, rd, rs1, imm);
#else
		riscv_flw (code, rd, rs1, imm);
#endif
	} else {
		code = mono_riscv_emit_imm (code, rd, imm);
		riscv_add (code, rd, rs1, rd);
#ifdef TARGET_RISCV64
		riscv_fld (code, rd, rd, 0);
#else
		riscv_flw (code, rd, rd, 0);
#endif
	}

	return code;
}

// May clobber t6. Uses at most 16 bytes on RV32I and 24 bytes on RV64I.
// length == 0 means dont care
guint8 *
mono_riscv_emit_store (guint8 *code, int rs2, int rs1, gint32 imm, int length)
{
	if (!RISCV_VALID_S_IMM (imm)){
		code = mono_riscv_emit_imm (code, RISCV_T6, imm);
		riscv_add (code, RISCV_T6, rs1, RISCV_T6);
		rs1 = RISCV_T6;
		imm = 0;
	}

	switch (length)
	{
	case 0:
#ifdef TARGET_RISCV64
		riscv_sd (code, rs2, rs1, imm);
#else
		riscv_sd (code, rs2, rs1, imm);
#endif
		break;
	case 1:
		riscv_sb (code, rs2, rs1, imm);
		break;
	case 2:
		riscv_sh (code, rs2, rs1, imm);
		break;
	case 4:
		riscv_sw (code, rs2, rs1, imm);
		break;
#ifdef TARGET_RISCV64
	case 8:
		riscv_sd (code, rs2, rs1, imm);
#endif
		break;
	default:
		g_assert_not_reached();
		break;
	}
	return code;
}

// May clobber t6. Uses at most 16 bytes on RV32I and 24 bytes on RV64I.
guint8 *
mono_riscv_emit_fstore(guint8 *code, int rs2, int rs1, gint32 imm){
	if (RISCV_VALID_S_IMM (imm)) {
#ifdef TARGET_RISCV64
		riscv_fsd (code, rs2, rs1, imm);
#else
		riscv_fsw (code, rs2, rs1, imm);
#endif
	} else {
		code = mono_riscv_emit_imm (code, RISCV_T6, imm);
		riscv_add (code, RISCV_T6, rs1, RISCV_T6);
#ifdef TARGET_RISCV64
		riscv_fsd (code, rs2, RISCV_T6, 0);
#else
		riscv_fsw (code, rs2, RISCV_T6, 0);
#endif
	}

	return code;
}

static guint8 *
mono_riscv_emit_call (MonoCompile *cfg, guint8* code, MonoJumpInfoType patch_type, gconstpointer data){

	mono_add_patch_info_rel (cfg, code - cfg->native_code, patch_type, data, MONO_R_RISCV_JAL);
	// only used as a placeholder
	riscv_jal(code, RISCV_RA, 0);
	cfg->thunk_area += THUNK_SIZE;
	return code;
}

/* This clobbers RA */
static guint8 *
mono_riscv_emit_branch_exc (MonoCompile *cfg, guint8 *code, const MonoInst ins, const char *exc_name){
	guint8 *p;

	riscv_auipc(code, RISCV_T0, 0);
	// load imm
	riscv_jal (code, RISCV_T1, sizeof (guint64) + 4);
	p = code;
	code += sizeof (guint64);
	riscv_ld (code, RISCV_T1, RISCV_T1, 0);
	// pc + imm
	riscv_add(code, RISCV_T0, RISCV_T0, RISCV_T1);

	*(guint64 *)p = (gsize)code;

	switch(ins.opcode){
		case OP_RISCV_EXC_BEQ:
			mono_add_patch_info_rel (cfg, code - cfg->native_code, MONO_PATCH_INFO_EXC, exc_name, MONO_R_RISCV_BEQ);
			riscv_beq(code, ins.sreg1, ins.sreg2, 0);
			break;
		case OP_RISCV_EXC_BNE:
			mono_add_patch_info_rel (cfg, code - cfg->native_code, MONO_PATCH_INFO_EXC, exc_name, MONO_R_RISCV_BNE);
			riscv_bne(code, ins.sreg1, ins.sreg2, 0);
			break;
		case OP_RISCV_EXC_BLTU:
			mono_add_patch_info_rel (cfg, code - cfg->native_code, MONO_PATCH_INFO_EXC, exc_name, MONO_R_RISCV_BLTU);
			riscv_bltu(code, ins.sreg1, ins.sreg2, 0);
			break;
		case OP_RISCV_EXC_BGEU:
			mono_add_patch_info_rel (cfg, code - cfg->native_code, MONO_PATCH_INFO_EXC, exc_name, MONO_R_RISCV_BGEU);
			riscv_bgeu(code, ins.sreg1, ins.sreg2, 0);
			break;
		default:
			g_print("can't emit exc branch %d\n", ins.opcode);
			NOT_IMPLEMENTED;
	}
	return code;
}

/*
 * emit_load_regarray:
 *
 *   Emit code to load the registers in REGS from the appropriate elements of
 * a register array at BASEREG+OFFSET.
 */
guint8*
emit_load_regarray (guint8 *code, guint64 regs, int basereg, int offset, MonoBoolean isFloat){
	int i;
	g_assert(basereg != RISCV_SP);

	if (!RISCV_VALID_S_IMM (offset)){
		code = mono_riscv_emit_imm (code, RISCV_T6, offset);
		riscv_add (code, RISCV_T6, basereg, RISCV_T6);
		basereg = RISCV_T6;
		offset = 0;
	}

	for (i = 0; i < 32; ++i){
		if (regs & (1 << i)) {
			if(!isFloat && i == RISCV_SP)
				g_assert_not_reached ();
			if(isFloat)
				code = mono_riscv_emit_fload (code, i, basereg, offset + (i * sizeof(host_mgreg_t)));
			else
				code = mono_riscv_emit_load (code, i, basereg, offset + (i * sizeof(host_mgreg_t)), 0);
		}
	}

	return code;
}

/*
 * emit_store_regarray:
 *
 *   Emit code to store the registers in REGS from the appropriate elements of
 * a register array at BASEREG+OFFSET.
 */
guint8*
emit_store_regarray (guint8 *code, guint64 regs, int basereg, int offset, MonoBoolean isFloat){
	int i;
	g_assert(basereg != RISCV_SP);

	if (!RISCV_VALID_S_IMM (offset)){
		code = mono_riscv_emit_imm (code, RISCV_T6, offset);
		riscv_add (code, RISCV_T6, basereg, RISCV_T6);
		basereg = RISCV_T6;
		offset = 0;
	}

	for (i = 0; i < 32; ++i){
		if (regs & (1 << i)) {
			if(!isFloat && i == RISCV_SP)
				g_assert_not_reached ();
			if(isFloat)
				code = mono_riscv_emit_fstore (code, i, basereg, offset + (i * sizeof(host_mgreg_t)));
			else
				code = mono_riscv_emit_store (code, i, basereg, offset + (i * sizeof(host_mgreg_t)), 0);
		}
	}

	return code;
}

/*
 * emit_load_stack:
 *
 *   Emit code to load the registers in REGS from stack or consecutive memory locations starting
 * at BASEREG+OFFSET.
 */
guint8*
emit_load_stack (guint8 *code, guint64 regs, int basereg, int offset, MonoBoolean isFloat){
	int i;
	int pos = 0;
	g_assert(basereg != RISCV_SP);

	if (!RISCV_VALID_S_IMM (offset)){
		code = mono_riscv_emit_imm (code, RISCV_T6, offset);
		riscv_add (code, RISCV_T6, basereg, RISCV_T6);
		basereg = RISCV_T6;
		offset = 0;
	}

	for (i = 0; i < 32; ++i) {
		if (regs & (1 << i)) {
			if(isFloat)
				code = mono_riscv_emit_fload (code, i, basereg, (offset + (pos * sizeof(host_mgreg_t))));
			else
				code = mono_riscv_emit_load(code, i, basereg, (offset + (pos * sizeof(host_mgreg_t))), 0);
			pos++;
		}
	}

	return code;
}

/*
 * emit_store_stack:
 *
 *   Emit code to store the registers in REGS into consecutive memory locations starting
 * at BASEREG+OFFSET.
 */
guint8*
emit_store_stack (guint8 *code, guint64 regs, int basereg, int offset, MonoBoolean isFloat){
	int i, pos = 0;
	g_assert(basereg != RISCV_SP);

	for (i = 0; i < 32; ++i) {
		if (regs & (1 << i)) {
			if(isFloat)
				code = mono_riscv_emit_fstore (code, i, basereg, (offset + (pos * sizeof(host_mgreg_t))));
			else
				code = mono_riscv_emit_store (code, i, basereg, (offset + (pos * sizeof(host_mgreg_t))), 0);
			pos++;
		}
	}
	return code;
}

/*
 * emit_setup_lmf:
 *
 *   Emit code to initialize an LMF structure at LMF_OFFSET.
 * Clobbers T6.
 */
static guint8*
emit_setup_lmf (MonoCompile *cfg, guint8 *code, gint32 lmf_offset, int cfa_offset){
	/*
	 * The LMF should contain all the state required to be able to reconstruct the machine state
	 * at the current point of execution. Since the LMF is only read during EH, only callee
	 * saved etc. registers need to be saved.
	 * FIXME: Save callee saved fp regs, JITted code doesn't use them, but native code does, and they
	 * need to be restored during EH.
	 */
	g_assert(lmf_offset <= 0);
	/* pc */
	code = mono_riscv_emit_imm (code, RISCV_T6, (gsize)code);
	code = mono_riscv_emit_store (code, RISCV_T6, RISCV_FP, lmf_offset + MONO_STRUCT_OFFSET (MonoLMF, pc), 0);
	/* gregs + fp + sp */
	/* Save caller fp */
	code = mono_riscv_emit_store(code, RISCV_FP, RISCV_FP, lmf_offset + MONO_STRUCT_OFFSET (MonoLMF, fp), 0);
	/* Save caller sp */
	code = mono_riscv_emit_store(code, RISCV_SP, RISCV_FP, lmf_offset + MONO_STRUCT_OFFSET (MonoLMF, sp), 0);

	// code = emit_store_regset_cfa (cfg, code, (MONO_ARCH_CALLEE_SAVED_REGS | (1 << RISCV_FP) | (1 << RISCV_SP)), RISCV_FP, lmf_offset + MONO_STRUCT_OFFSET (MonoLMF, gregs), cfa_offset, (1 << RISCV_FP) | (1 << RISCV_SP));
	code = emit_store_stack (code, (MONO_ARCH_CALLEE_SAVED_REGS), RISCV_FP, lmf_offset + MONO_STRUCT_OFFSET (MonoLMF, gregs) + sizeof(host_mgreg_t) * RISCV_N_GSREGS, FALSE);

	return code;
}

static guint8*
emit_move_args (MonoCompile *cfg, guint8 *code){
	MonoInst *ins;
	CallInfo *cinfo;
	ArgInfo *ainfo;
	int i, part;
	MonoMethodSignature *sig = mono_method_signature_internal (cfg->method);

	cinfo = cfg->arch.cinfo;
	g_assert (cinfo);

	for (i = 0; i < cinfo->nargs; ++i){
		ainfo = cinfo->args + i;
		ins = cfg->args [i];

		if (ins->opcode == OP_REGVAR){
			switch (ainfo->storage){
				case ArgInIReg:
					riscv_addi(code, ins->dreg, ainfo->reg, 0);
					if (i == 0 && sig->hasthis){
						mono_add_var_location (cfg, ins, TRUE, ainfo->reg, 0, 0, code - cfg->native_code);
						mono_add_var_location (cfg, ins, TRUE, ins->dreg, 0, code - cfg->native_code, 0);
					}
					break;
				
				default:
					NOT_IMPLEMENTED;
			}
		}
		else{
			if (ainfo->storage != ArgVtypeByRef && ainfo->storage != ArgVtypeByRefOnStack)
				g_assert (ins->opcode == OP_REGOFFSET);
			
			switch (ainfo->storage){
				case ArgInIReg:
					g_assert(ainfo->is_regpair == FALSE);
					code = mono_riscv_emit_store(code, ainfo->reg, ins->inst_basereg, ins->inst_offset, 0);
					if (i == 0 && sig->hasthis) {
						mono_add_var_location (cfg, ins, TRUE, ainfo->reg, 0, 0, code - cfg->native_code);
						mono_add_var_location (cfg, ins, FALSE, ins->inst_basereg, ins->inst_offset, code - cfg->native_code, 0);
					}
					break;
				case ArgVtypeInIReg:
					code = mono_riscv_emit_store(code, ainfo->reg, ins->inst_basereg, ins->inst_offset, 0);
					if(ainfo->is_regpair)
						code = mono_riscv_emit_store(code, ainfo->reg + 1, ins->inst_basereg, ins->inst_offset + sizeof(host_mgreg_t), 0);
					break;
				default:
					g_print("can't process Storage type %d\n",ainfo->storage);
					NOT_IMPLEMENTED;
			}
		}
	}

	return code;
}

static guint8*
emit_move_return_value (MonoCompile *cfg, guint8 * code, MonoInst *ins){
	CallInfo *cinfo;
	MonoCallInst *call;

	call = (MonoCallInst*)ins;
	cinfo = call->call_info;
	g_assert (cinfo);
	switch (cinfo->ret.storage){
		case ArgNone:
			break;
		case ArgInIReg:
			if (call->inst.dreg != cinfo->ret.reg){
				riscv_addi(code, call->inst.dreg, cinfo->ret.reg, 0);
			}
			break;
		case ArgVtypeInIReg:{
			MonoInst *loc = cfg->arch.vret_addr_loc;
			int i;

			/* Load the destination address */
			g_assert (loc && loc->opcode == OP_REGOFFSET);
			code = mono_riscv_emit_load (code, RISCV_T0, loc->inst_basereg, loc->inst_offset, 0);
			code = mono_riscv_emit_load (code, cinfo->ret.reg, RISCV_T0, 0, 0);
			if(cinfo->ret.is_regpair){
				code = mono_riscv_emit_load (code, cinfo->ret.reg + 1, RISCV_T0, sizeof(host_mgreg_t), 0);
			}
			break;
		}
		default:
			NOT_IMPLEMENTED;
			break;
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
 *  | callee saved regs		   |
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
	MonoBasicBlock *bb;
	guint8 *code;
	int cfa_offset, max_offset;

	sig = mono_method_signature_internal (method);
	cfg->code_size = MAX (cfg->header->code_size * 4, 1024);
	code = cfg->native_code = g_malloc (cfg->code_size);

	/* realigned */
	cfg->stack_offset = ALIGN_TO (cfg->stack_offset, MONO_ARCH_FRAME_ALIGNMENT);

	/*
	 * - Setup frame
	 */
	cfa_offset = 0;
	int stack_size = 0;
	mono_emit_unwind_op_def_cfa (cfg, code, RISCV_SP, 0);

	/* Setup frame */
	if (RISCV_VALID_I_IMM (-cfg->stack_offset)){
		riscv_addi(code,RISCV_SP,RISCV_SP,-cfg->stack_offset);

		// save return value
		stack_size += sizeof(target_mgreg_t);
		code = mono_riscv_emit_store(code, RISCV_RA, RISCV_SP, cfg->stack_offset - stack_size, 0);

		// save s0(fp) value
		stack_size += sizeof(target_mgreg_t);
		code = mono_riscv_emit_store(code, RISCV_FP, RISCV_SP, cfg->stack_offset - stack_size, 0);
	}
	else{
		NOT_IMPLEMENTED;
	}

	cfa_offset += cfg->stack_offset;
	mono_emit_unwind_op_def_cfa_offset (cfg, code, cfa_offset);
	mono_emit_unwind_op_offset (cfg, code, RISCV_RA, cfa_offset - sizeof(target_mgreg_t));
	mono_emit_unwind_op_offset (cfg, code, RISCV_FP, cfa_offset - (sizeof(target_mgreg_t) * 2));

	// set s0(fp) value
	riscv_addi(code,RISCV_FP,RISCV_SP,cfg->stack_offset);

	// save other registers
	if (cfg->param_area)
		/* The param area is below the stack pointer */
		riscv_addi(code, RISCV_SP, RISCV_SP, -cfg->param_area);


	if (0){
		g_assert(cfg->lmf_var->inst_offset <= 0);
		code = emit_setup_lmf (cfg, code, cfg->lmf_var->inst_offset, cfa_offset);
	}
	else{
		/* Save gregs */
		code = emit_store_stack (code, MONO_ARCH_CALLEE_SAVED_REGS & cfg->used_int_regs, RISCV_FP, -cfg->arch.saved_gregs_offset, FALSE);
	}
	
	/* Save return area addr received */
	if (cfg->vret_addr) {
		NOT_IMPLEMENTED;
	}

	/*
	 * Move arguments to their registers/stack locations.
	 */
	code = emit_move_args (cfg, code);

	/* Initialize seq_point_info_var */
	if (cfg->arch.seq_point_info_var){
		NOT_IMPLEMENTED;
	}
	else{
		MonoInst *ins;
		if (cfg->arch.ss_tramp_var) {
			/* Initialize ss_tramp_var */
			ins = cfg->arch.ss_tramp_var;
			g_assert (ins->opcode == OP_REGOFFSET);

			code = mono_riscv_emit_imm(code, RISCV_T0, (guint64)&ss_trampoline);
			code = mono_riscv_emit_store(code, RISCV_T0, ins->inst_basereg, ins->inst_offset, 0);
		}
		if (cfg->arch.bp_tramp_var){
			/* Initialize bp_tramp_var */
			ins = cfg->arch.bp_tramp_var;
			g_assert (ins->opcode == OP_REGOFFSET);
			code = mono_riscv_emit_imm(code, RISCV_T0, (guint64)bp_trampoline);
			code = mono_riscv_emit_store(code, RISCV_T0, ins->inst_basereg, ins->inst_offset, 0);
 		}
 	}
	
	return code;
}

void
mono_arch_emit_epilog (MonoCompile *cfg)
{
	guint8 *code = NULL;
	CallInfo *cinfo;
	// MonoMethod *method = cfg->method;
	int max_epilog_size = 16 + 20*4;
	// int alloc2_size = 0;

	code = realloc_code (cfg, max_epilog_size);

	if (0) {
		// same as emit_load_stack()
		g_assert(cfg->lmf_var->inst_offset <= 0);
		int basereg = RISCV_FP;
		int offset = cfg->lmf_var->inst_offset + MONO_STRUCT_OFFSET (MonoLMF, gregs) + sizeof(host_mgreg_t) * RISCV_N_GSREGS;
		int pos = 1;
		if (!RISCV_VALID_S_IMM (offset)){
			code = mono_riscv_emit_imm (code, RISCV_T6, offset);
			riscv_add (code, RISCV_T6, basereg, RISCV_T6);
			basereg = RISCV_T6;
			offset = 0;
		}

		for (int i = MONO_ARCH_FIRST_LMF_REG; i < 32; ++i) {
			if (MONO_ARCH_CALLEE_SAVED_REGS & (1 << i)){
				if (cfg->used_int_regs & (1 << i))
					code = mono_riscv_emit_load(code, i, basereg, (offset + (pos * sizeof(host_mgreg_t))), 0);
				pos++;
			}
		}
	} else {
		/* Restore gregs */
		code = emit_load_stack (code, MONO_ARCH_CALLEE_SAVED_REGS & cfg->used_int_regs, RISCV_FP, -cfg->arch.saved_gregs_offset, FALSE);
	}
	
	/* Load returned vtypes into registers if needed */
	cinfo = cfg->arch.cinfo;
	switch (cinfo->ret.storage) {
		case ArgNone:
		case ArgInIReg:
			break;
		case ArgVtypeInIReg:{
			MonoInst *ins = cfg->ret;
			code = mono_riscv_emit_load (code, cinfo->ret.reg, ins->inst_basereg, ins->inst_offset, 0);
			if(cinfo->ret.is_regpair)
				code = mono_riscv_emit_load (code, cinfo->ret.reg + 1, ins->inst_basereg, ins->inst_offset + sizeof(host_mgreg_t), 0);
			break;
		}
		default:
			g_print("Unable process returned storage %d(0x%x)\n",cinfo->ret.storage,cinfo->ret.storage);
			NOT_IMPLEMENTED;
	}

	/* Destroy frame */
	code = mono_riscv_emit_destroy_frame(code, cfg->stack_offset);

	riscv_jalr(code, RISCV_X0, RISCV_RA, 0);

	g_assert (code - (cfg->native_code + cfg->code_len) < max_epilog_size);
	set_code_cursor (cfg, code);
}

void
mono_arch_output_basic_block (MonoCompile *cfg, MonoBasicBlock *bb)
{
	MonoInst *ins;
	MonoCallInst *call;
	guint8 *code = cfg->native_code + cfg->code_len;
	// target_mgreg_t imm;
	int start_offset, max_len;
	int ins_cnt = 0;

	start_offset = code - cfg->native_code;
	g_assert (start_offset <= cfg->code_size);

	if (cfg->verbose_level > 2)
		g_print ("Basic block %d starting at offset 0x%x\n", bb->block_num, bb->native_offset);

	MONO_BB_FOR_EACH_INS (bb, ins) {
		guint offset = code - cfg->native_code;
		set_code_cursor (cfg, code);
		max_len = ins_get_size (ins->opcode);
		code = realloc_code (cfg, max_len);

		mono_debug_record_line_number (cfg, ins, offset);

		/* Check for virtual regs that snuck by */
		g_assert ((ins->dreg >= -1) && (ins->dreg < 32));

		switch (ins->opcode) {
			case OP_NOT_REACHED:
			case OP_NOT_NULL:
			case OP_DUMMY_USE:
				break;
			
			/* VM Related OP */
			case OP_CHECK_THIS:
			//  try to load 1 byte from ins->sreg to check it is null pionter
				code = mono_riscv_emit_load (code, RISCV_RA, ins->sreg1, 0, 1);
				break;
			case OP_GET_EX_OBJ:
				if (ins->dreg != RISCV_A0)
					// mv dreg, RISCV_A0
					riscv_addi(code, ins->dreg, RISCV_A0, 0);
				break;
			case OP_IL_SEQ_POINT:
				mono_add_seq_point (cfg, bb, ins, code - cfg->native_code);
				break;
			case OP_SEQ_POINT:{
				MonoInst *info_var = cfg->arch.seq_point_info_var;
				if (ins->flags & MONO_INST_SINGLE_STEP_LOC){
					MonoInst *var = cfg->arch.ss_tramp_var;

					g_assert (var);
					g_assert (var->opcode == OP_REGOFFSET);
					/* Load ss_tramp_var */
					/* This is equal to &ss_trampoline */
					code = mono_riscv_emit_load(code, RISCV_T0, var->inst_basereg, var->inst_offset, 0);
					/* Load the trampoline address */
					code = mono_riscv_emit_load(code, RISCV_T0, RISCV_T0, 0, 0);
					/* Call it if it is non-null */
					// In riscv, we use jalr to jump
					riscv_beq(code, RISCV_ZERO, RISCV_T0, 8);
					riscv_jalr(code, RISCV_ZERO, RISCV_T0, 0);
				}
				mono_add_seq_point (cfg, bb, ins, code - cfg->native_code);

				if (cfg->compile_aot){
					NOT_IMPLEMENTED;
				}
				else{
					MonoInst *var = cfg->arch.bp_tramp_var;
					g_assert (var);
					g_assert (var->opcode == OP_REGOFFSET);
					/* Load the address of the bp trampoline into IP0 */
					code = mono_riscv_emit_load (code, RISCV_T0, var->inst_basereg, var->inst_offset, 0);
					/* 
					* A placeholder for a possible breakpoint inserted by
					* mono_arch_set_breakpoint ().
					*/
					code = mono_riscv_emit_nop (code);
				}
				break;
			}
			case OP_LOCALLOC:{
				g_assert(MONO_ARCH_FRAME_ALIGNMENT == 16);
				guint8 *loop_start;
				// ins->sreg1 stores the size of new object.
				// 1. Align the object size to MONO_ARCH_FRAME_ALIGNMENT
				// RISCV_T0 = ALIGN_TO(ins->sreg1, MONO_ARCH_FRAME_ALIGNMENT)
				riscv_addi(code, RISCV_T0, ins->sreg1, (MONO_ARCH_FRAME_ALIGNMENT - 1));
				code = mono_riscv_emit_imm(code, RISCV_T1, -MONO_ARCH_FRAME_ALIGNMENT);
				riscv_and (code, RISCV_T0, RISCV_T0, RISCV_T1);

				// 2. extend sp for Object
				riscv_addi(code, RISCV_T1, RISCV_SP, 0);
				riscv_sub(code, RISCV_SP, RISCV_T1, RISCV_T0);

				// 3.
				/* Init */
				/* T0 = pointer, T1 = end */
				riscv_addi(code, RISCV_T0, RISCV_SP, 0);
				loop_start = code;
				riscv_beq(code, RISCV_T0, RISCV_T1, 0);
				code = mono_riscv_emit_store(code, RISCV_ZERO, RISCV_T0, 0, 0);
				code = mono_riscv_emit_store(code, RISCV_ZERO, RISCV_T0, sizeof(host_mgreg_t), 0), 0;
#ifdef TARGET_RISCV32
				code = mono_riscv_emit_store(code, RISCV_ZERO, RISCV_T0, sizeof(host_mgreg_t)*2, 0);
				code = mono_riscv_emit_store(code, RISCV_ZERO, RISCV_T0, sizeof(host_mgreg_t)*3, 0);
#endif
				riscv_addi(code, RISCV_T0, RISCV_T0, MONO_ARCH_FRAME_ALIGNMENT);
				riscv_jal(code, RISCV_ZERO, riscv_get_jal_disp(code, loop_start));
				riscv_patch_rel(loop_start, code, MONO_R_RISCV_BEQ);

				riscv_addi(code, ins->dreg, RISCV_SP, 0);
				if (cfg->param_area){
					g_assert(cfg->param_area > 0);
					riscv_addi(code, RISCV_SP, RISCV_SP, -cfg->param_area);
				}
				break;
			}
			case OP_LOCALLOC_IMM:{
				int aligned_imm, aligned_imm_offset;
				aligned_imm = ALIGN_TO (ins->inst_imm, MONO_ARCH_FRAME_ALIGNMENT);
				g_assert (RISCV_VALID_I_IMM(aligned_imm));
				riscv_addi(code, RISCV_SP, RISCV_SP, -aligned_imm);

				/* Init */
				g_assert (MONO_ARCH_FRAME_ALIGNMENT == 16);
				aligned_imm_offset = 0;
				while (aligned_imm_offset < aligned_imm){
					code = mono_riscv_emit_store(code, RISCV_ZERO, RISCV_T0, aligned_imm_offset + 0, 0);
					code = mono_riscv_emit_store(code, RISCV_ZERO, RISCV_T0, aligned_imm_offset + sizeof(host_mgreg_t), 0), 0;
#ifdef TARGET_RISCV32
					code = mono_riscv_emit_store(code, RISCV_ZERO, RISCV_T0, aligned_imm_offset + sizeof(host_mgreg_t)*2, 0);
					code = mono_riscv_emit_store(code, RISCV_ZERO, RISCV_T0, aligned_imm_offset + sizeof(host_mgreg_t)*3, 0);
#endif
					aligned_imm_offset += 16;
				}

				riscv_addi(code, ins->dreg, RISCV_SP, 0);
				if (cfg->param_area){
					g_assert(cfg->param_area > 0);
					riscv_addi(code, RISCV_SP, RISCV_SP, -cfg->param_area);
				}
				break;
			}
			case OP_GENERIC_CLASS_INIT:{
				int byte_offset;
				guint8 *branch_label;

				byte_offset = MONO_STRUCT_OFFSET (MonoVTable, initialized);
				
				/* Load vtable->initialized */
				code = mono_riscv_emit_load(code, RISCV_T0, ins->sreg1, byte_offset, 1);
				branch_label = code;
				riscv_bne(code, RISCV_ZERO, RISCV_T0, 0);

				/* Slowpath */
				g_assert (ins->sreg1 == RISCV_A0);
				code = mono_riscv_emit_call (cfg, code, MONO_PATCH_INFO_JIT_ICALL_ID,
								  GUINT_TO_POINTER (MONO_JIT_ICALL_mono_generic_class_init));

				mono_riscv_patch (branch_label, code, MONO_R_RISCV_BNE);
				break;
			}

			case OP_NOP:
				code = mono_riscv_emit_nop(code);
				break;
			case OP_MOVE:
				// mv ra, a1 -> addi ra, a1, 0
				riscv_addi(code, ins->dreg, ins->sreg1, 0);
				break;
			case OP_LOAD_MEMBASE:
				code = mono_riscv_emit_load(code, ins->dreg, ins->sreg1, ins->inst_offset, 0);
				break;
			case OP_LOADI1_MEMBASE:
			case OP_LOADU1_MEMBASE:
				code = mono_riscv_emit_load(code, ins->dreg, ins->sreg1, ins->inst_offset, 1);
				break;
			case OP_LOADI2_MEMBASE:
			case OP_LOADU2_MEMBASE:
				code = mono_riscv_emit_load(code, ins->dreg, ins->sreg1, ins->inst_offset, 2);
				break;
			case OP_LOADU4_MEMBASE:
			case OP_LOADI4_MEMBASE:
				code = mono_riscv_emit_load(code, ins->dreg, ins->sreg1, ins->inst_offset, 4);
				break;
			case OP_LOADI8_MEMBASE:
				code = mono_riscv_emit_load(code, ins->dreg, ins->sreg1, ins->inst_offset, 8);
				break;
			case OP_STORE_MEMBASE_REG:
				code = mono_riscv_emit_store(code, ins->sreg1, ins->dreg, ins->inst_offset, 0);
				break;
			case OP_STOREI1_MEMBASE_REG:
				code = mono_riscv_emit_store(code, ins->sreg1, ins->dreg, ins->inst_offset, 1);
				break;
			case OP_STOREI2_MEMBASE_REG:
				code = mono_riscv_emit_store(code, ins->sreg1, ins->dreg, ins->inst_offset, 2);
				break;
			case OP_STOREI4_MEMBASE_REG:
				code = mono_riscv_emit_store(code, ins->sreg1, ins->dreg, ins->inst_offset, 4);
				break;
			case OP_STOREI8_MEMBASE_REG:
				code = mono_riscv_emit_store(code, ins->sreg1, ins->dreg, ins->inst_offset, 8);
				break;
			case OP_ICONST:
			case OP_I8CONST:
				code = mono_riscv_emit_imm(code, ins->dreg, ins->inst_c0);
				break;
			case OP_IADD:
			case OP_LADD:
				riscv_add(code, ins->dreg, ins->sreg1, ins->sreg2);
				break;
			case OP_ADD_IMM:
			case OP_IADD_IMM:
			case OP_LADD_IMM:
				riscv_addi(code, ins->dreg, ins->sreg1, ins->inst_imm);
				break;
			case OP_ISUB:
				riscv_sub(code, ins->dreg, ins->sreg1, ins->sreg2);
				break;
			case OP_IMUL:
				g_assert(riscv_stdext_m);
				riscv_mul(code, ins->dreg, ins->sreg1, ins->sreg2);
				break;

			/* Bit/logic */
			case OP_IAND:
				riscv_and(code, ins->dreg, ins->sreg1, ins->sreg2);
				break;
			case OP_AND_IMM:
			case OP_LAND_IMM:
				riscv_andi(code, ins->dreg, ins->sreg1, ins->inst_imm);
				break;
			case OP_LOR_IMM:
			case OP_XOR_IMM:
				riscv_xori(code, ins->dreg, ins->sreg1, ins->inst_imm);
				break;
			case OP_LOR:
				riscv_xor(code, ins->dreg, ins->sreg1, ins->sreg2);
				break;
			case OP_RISCV_SLT:
				riscv_slt(code, ins->dreg, ins->sreg1, ins->sreg2);
				break;
			case OP_RISCV_SLTU:
				riscv_sltu(code, ins->dreg, ins->sreg1, ins->sreg2);
				break;
			case OP_RISCV_SLTI:
				riscv_slti(code, ins->dreg, ins->sreg1, ins->inst_imm);
				break;
			case OP_RISCV_SLTIU:
				riscv_sltiu(code, ins->dreg, ins->sreg1, ins->inst_imm);
				break;
			case OP_SHR_UN_IMM:
				riscv_srli(code, ins->dreg, ins->sreg1, ins->inst_imm);
				break;
			case OP_SHR_IMM:
				riscv_srai(code, ins->dreg, ins->sreg1, ins->inst_imm);
				break;
			case OP_SHL_IMM:
			case OP_LSHL_IMM:
				riscv_slli(code, ins->dreg, ins->sreg1, ins->inst_imm);
				break;
#if defined(TARGET_RISCV64)
			case OP_RISCV_ADDIW:
				riscv_addiw(code, ins->dreg, ins->sreg1, 0);
				break;
			case OP_RISCV_ADDUW:
				// TODO: Add inst riscv_adduw in riscv-codegen.h
				NOT_IMPLEMENTED;
				break;
#endif
			/* Atomic */
			case OP_MEMORY_BARRIER:
				riscv_fence(code, RISCV_FENCE_MEM, RISCV_FENCE_MEM);
				break;
			case OP_ATOMIC_STORE_I4:{
				riscv_fence(code, RISCV_FENCE_MEM, RISCV_FENCE_W);
				code = mono_riscv_emit_store (code, ins->sreg1, ins->inst_destbasereg, ins->inst_offset, 4);
				if (ins->backend.memory_barrier_kind == MONO_MEMORY_BARRIER_SEQ)
					NOT_IMPLEMENTED;
				break;
			}
			case OP_ATOMIC_CAS_I4:{
				g_assert(riscv_stdext_a);
				/**
				 * loop_start:
				 * 	lr.w	t0, rs1
				 * 	bne		t0, rs3, loop_end
				 * 	sc.w.rl t1, rs2, rs1
				 * 	bnez t1, loop_start
				 * loop_end:
				 * 	fence rw, rw
				 * 	mv rd, t0
				 * 
				*/
				guint8 *loop_start, *branch_label;
				/* sreg2 is the value, sreg3 is the comparand */
				loop_start = code;
				riscv_lr_w(code, RISCV_ORDER_NONE, RISCV_T0, ins->sreg1);
				branch_label = code;
				riscv_bne(code, RISCV_T0, ins->sreg3, 0);
				riscv_sc_w(code, RISCV_ORDER_RL, RISCV_T1, ins->sreg2, ins->sreg1);
				riscv_bne(code, RISCV_ZERO, RISCV_T1, loop_start - code);
				riscv_patch_rel(branch_label, code, MONO_R_RISCV_BNE);
				
				riscv_fence(code, RISCV_FENCE_MEM, RISCV_FENCE_MEM);
				riscv_addi(code, ins->dreg, RISCV_T0, 0);
				break;
			}
			case OP_ATOMIC_CAS_I8:{
				g_assert(riscv_stdext_a);
				guint8 *loop_start, *branch_label;
				/* sreg2 is the value, sreg3 is the comparand */
				loop_start = code;
				riscv_lr_d(code, RISCV_ORDER_NONE, RISCV_T0, ins->sreg1);
				branch_label = code;
				riscv_bne(code, RISCV_T0, ins->sreg3, 0);
				riscv_sc_d(code, RISCV_ORDER_RL, RISCV_T1, ins->sreg2, ins->sreg1);
				riscv_bne(code, RISCV_ZERO, RISCV_T1, loop_start - code);
				riscv_patch_rel(branch_label, code, MONO_R_RISCV_BNE);
				
				riscv_fence(code, RISCV_FENCE_MEM, RISCV_FENCE_MEM);
				riscv_addi(code, ins->dreg, RISCV_T0, 0);
				break;
			}

			/* Calls */
			case OP_VOIDCALL:
			case OP_CALL:
			case OP_LCALL:
			case OP_VCALL2: {
				call = (MonoCallInst*)ins;
				const MonoJumpInfoTarget patch = mono_call_to_patch (call);
				code = mono_riscv_emit_call (cfg, code, patch.type, patch.target);
				code = emit_move_return_value (cfg, code, ins);
				break;
			}
			case OP_VOIDCALL_REG:
				// use JALR x1, 0(src1)
				riscv_jalr(code, RISCV_RA, ins->sreg1, 0);
				code = emit_move_return_value(cfg, code, ins);
				break;
			case OP_CALL_MEMBASE:
			case OP_VCALL2_MEMBASE:
			case OP_VOIDCALL_MEMBASE:
				code = mono_riscv_emit_load(code, RISCV_T0, ins->inst_basereg, ins->inst_offset, 0);
				riscv_jalr(code, RISCV_RA, RISCV_T0, 0);
				code = emit_move_return_value(cfg, code, ins);
				break;

			/* Branch */
			case OP_RISCV_BNE:
				mono_add_patch_info_rel (cfg, (code - cfg->native_code), MONO_PATCH_INFO_BB, ins->inst_true_bb, MONO_R_RISCV_BNE);
				riscv_bne(code, ins->sreg1, ins->sreg2, 0);
				break;
			case OP_RISCV_BEQ:
				mono_add_patch_info_rel (cfg, (code - cfg->native_code), MONO_PATCH_INFO_BB, ins->inst_true_bb, MONO_R_RISCV_BEQ);
				riscv_beq(code, ins->sreg1, ins->sreg2, 0);
				break;
			case OP_RISCV_BGE:
				mono_add_patch_info_rel (cfg, (code - cfg->native_code), MONO_PATCH_INFO_BB, ins->inst_true_bb, MONO_R_RISCV_BGE);
				riscv_bge(code, ins->sreg1, ins->sreg2, 0);
				break;
			case OP_RISCV_BGEU:
				mono_add_patch_info_rel (cfg, (code - cfg->native_code), MONO_PATCH_INFO_BB, ins->inst_true_bb, MONO_R_RISCV_BGE);
				riscv_bgeu(code, ins->sreg1, ins->sreg2, 0);
				break;
			case OP_RISCV_BLT:
				mono_add_patch_info_rel (cfg, (code - cfg->native_code), MONO_PATCH_INFO_BB, ins->inst_true_bb, MONO_R_RISCV_BGE);
				riscv_blt(code, ins->sreg1, ins->sreg2, 0);
				break;
			case OP_RISCV_BLTU:
				mono_add_patch_info_rel (cfg, (code - cfg->native_code), MONO_PATCH_INFO_BB, ins->inst_true_bb, MONO_R_RISCV_BGE);
				riscv_bltu(code, ins->sreg1, ins->sreg2, 0);
				break;
			case OP_BR:
				mono_add_patch_info_rel (cfg, offset, MONO_PATCH_INFO_BB, ins->inst_target_bb, MONO_R_RISCV_JAL);
				riscv_jal (code, RISCV_ZERO, 0);
				break;
			case OP_RISCV_EXC_BNE:
			case OP_RISCV_EXC_BEQ:
			case OP_RISCV_EXC_BGEU:
			case OP_RISCV_EXC_BLTU:{
				code = mono_riscv_emit_branch_exc (cfg, code, *ins, (const char*)ins->inst_p1);
				break;
			}

			/* Throw */
			case OP_THROW:
				code = mono_riscv_emit_call (cfg, code, MONO_PATCH_INFO_JIT_ICALL_ID,
							  GUINT_TO_POINTER (MONO_JIT_ICALL_mono_arch_throw_exception));
				break;
			case OP_RETHROW:
				code = mono_riscv_emit_call (cfg, code, MONO_PATCH_INFO_JIT_ICALL_ID,
							  GUINT_TO_POINTER (MONO_JIT_ICALL_mono_arch_rethrow_exception));
				break;

			case OP_GC_SAFE_POINT:{
				guint8 *src_inst_pointer [1];

				riscv_ld (code, RISCV_T1, ins->sreg1, 0);
				/* Call it if it is non-null */
				src_inst_pointer [0] = code;
				riscv_beq (code, RISCV_ZERO, RISCV_T1, 0);
				code = mono_riscv_emit_call (cfg, code, MONO_PATCH_INFO_JIT_ICALL_ID, GUINT_TO_POINTER (MONO_JIT_ICALL_mono_threads_state_poll));
				mono_riscv_patch (src_inst_pointer [0], code, MONO_R_RISCV_BEQ);
				break;
			}
			case OP_START_HANDLER:{
				MonoInst *spvar = mono_find_spvar_for_region (cfg, bb->region);
				/* Save caller address */
				code = mono_riscv_emit_store(code, RISCV_RA, spvar->inst_basereg, spvar->inst_offset, 0);

				/*
				* Reserve a param area, see test_0_finally_param_area ().
				* This is needed because the param area is not set up when
				* we are called from EH code.
				*/
				if (cfg->param_area)
					riscv_addi(code, RISCV_SP, RISCV_SP, -cfg->param_area);
				break;
			}
			case OP_CALL_HANDLER:
				mono_add_patch_info_rel (cfg, offset, MONO_PATCH_INFO_BB, ins->inst_target_bb, MONO_R_RISCV_JAL);
				riscv_jal (code, RISCV_RA, 0);
				cfg->thunk_area += THUNK_SIZE;
				for (GList *tmp = ins->inst_eh_blocks; tmp != bb->clause_holes; tmp = tmp->prev)
					mono_cfg_add_try_hole (cfg, ((MonoLeaveClause *) tmp->data)->clause, code, bb);
				break;
			case OP_ENDFINALLY:{
				MonoInst *spvar = mono_find_spvar_for_region (cfg, bb->region);
				if (cfg->param_area)
					riscv_addi(code, RISCV_SP, RISCV_SP, -cfg->param_area);
				if (ins->opcode == OP_ENDFILTER && ins->sreg1 != RISCV_A0)
					riscv_addi(code, RISCV_A0, ins->sreg1, 0);
				
				/* Return to either after the branch in OP_CALL_HANDLER, or to the EH code */
				code = mono_riscv_emit_load(code, RISCV_RA, spvar->inst_basereg, spvar->inst_offset, 0);
				riscv_jalr(code, RISCV_ZERO, RISCV_RA, 0);
				break;
			}

			default:
				printf ("unable to output following IR:"); mono_print_ins (ins);
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
	MonoClass *exc_class;
	guint8 *code, *ip;
	guint8* exc_throw_pos [MONO_EXC_INTRINS_NUM] = {NULL};
	guint8 exc_throw_found [MONO_EXC_INTRINS_NUM] = {0};
	int exc_id, max_epilog_size = 0;

	for (ji = cfg->patch_info; ji; ji = ji->next) {
		if (ji->type == MONO_PATCH_INFO_EXC) {
			exc_id = mini_exception_id_by_name ((const char*)ji->data.target);
			g_assert (exc_id < MONO_EXC_INTRINS_NUM);
			if (!exc_throw_found [exc_id]) {
				max_epilog_size += 40; // 8 Inst for exception
				exc_throw_found [exc_id] = TRUE;
			}
		}
	}

	code = realloc_code (cfg, max_epilog_size);

	/* Emit code to raise corlib exceptions */
	for (ji = cfg->patch_info; ji; ji = ji->next) {
		if (ji->type != MONO_PATCH_INFO_EXC)
			continue;

		ip = cfg->native_code + ji->ip.i;

		exc_id = mini_exception_id_by_name ((const char*)ji->data.target);

		if (exc_throw_pos [exc_id]){
			/* ip should points to the branch inst in OP_COND_EXC_... */
			riscv_patch_rel (ip, exc_throw_pos [exc_id], ji->relocation);
			ji->type = MONO_PATCH_INFO_NONE;
			continue;
		}

		exc_throw_pos [exc_id] = code;
		riscv_patch_rel (ip, code, ji->relocation);

		/* A0 = type token */
		exc_class = mono_class_load_from_name (mono_defaults.corlib, "System", ji->data.name);
		code = mono_riscv_emit_imm (code, RISCV_A0, m_class_get_type_token (exc_class) - MONO_TOKEN_TYPE_DEF);
		/* A1 = throw ip */
		riscv_addi (code, RISCV_A1, RISCV_T0, 0);
		/* Branch to the corlib exception throwing trampoline */
		ji->ip.i = code - cfg->native_code;
		ji->type = MONO_PATCH_INFO_JIT_ICALL_ID;
		ji->data.jit_icall_id = MONO_JIT_ICALL_mono_arch_throw_corlib_exception;
		ji->relocation = MONO_R_RISCV_JAL;
		riscv_jal (code, RISCV_RA, 0);

		cfg->thunk_area += THUNK_SIZE;
		set_code_cursor (cfg, code);
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
	ss_trampoline = mini_get_single_step_trampoline ();
}

void
mono_arch_stop_single_stepping (void)
{
	ss_trampoline = NULL;
}

gboolean
mono_arch_is_single_step_event (void *info, void *sigctx)
{
	// NOT_IMPLEMENTED;
	/* No reference Information, Don't know how to implement */
	return FALSE;
}

gboolean
mono_arch_is_breakpoint_event (void *info, void *sigctx)
{
	// NOT_IMPLEMENTED;
	/* No reference Information, Don't know how to implement */
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
	SeqPointInfo *info;
	MonoJitInfo *ji;
	MonoJitMemoryManager *jit_mm;

	jit_mm = get_default_jit_mm ();

	// FIXME: Add a free function

	jit_mm_lock (jit_mm);
	info = (SeqPointInfo *)g_hash_table_lookup (jit_mm->arch_seq_points, code);
	jit_mm_unlock (jit_mm);

	if (!info) {
		ji = mini_jit_info_table_find (code);
		g_assert (ji);

		info = g_malloc0 (sizeof (SeqPointInfo) + (ji->code_size / 4) * sizeof(guint8*));

		info->ss_tramp_addr = &ss_trampoline;

		jit_mm_lock (jit_mm);
		g_hash_table_insert (jit_mm->arch_seq_points, code, info);
		jit_mm_unlock (jit_mm);
	}

	return info;
}
#endif /* MONO_ARCH_SOFT_DEBUG_SUPPORTED */

gpointer
mono_arch_load_function (MonoJitICallId jit_icall_id)
{
	return NULL;
}
