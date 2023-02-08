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
	MONO_ARCH_DUMP_CODE_DEBUG(code, 1);
	code = mono_riscv_emit_load(code, RISCV_S0, RISCV_SP, stack_offset - sizeof(host_mgreg_t)*2, 0);
	MONO_ARCH_DUMP_CODE_DEBUG(code, 1);
	riscv_addi(code, RISCV_SP, RISCV_SP, stack_offset);
	MONO_ARCH_DUMP_CODE_DEBUG(code, 1);

	return code;
}

static guint8*
emit_thunk (guint8 *code, gconstpointer target)
{
	g_print("emit thunk at 0x%lx\n",code);
	guint8 *p = code;
	code = mono_riscv_emit_imm(code, RISCV_T0, (gsize)target);
	riscv_jalr (code, RISCV_ZERO, RISCV_T0,0);
	g_print("jalr zero, 0(t0)<%lx>\n", target);

	// TODO: figure out whether thunk need return and deicde delet or not
	riscv_jalr (code, RISCV_ZERO, RISCV_RA, 0);
	g_print("ret\n");

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
			if(riscv_is_jal_disp(code,target)){
				riscv_jal(code, rd, riscv_get_jal_disp(code,target));
				g_print("jar %s, 0x%x <0x%lx> ",mono_arch_regname(rd), riscv_get_jal_disp(code,target), target);
				MONO_ARCH_DUMP_CODE_DEBUG(code, 1);
			}
			else{
				gpointer thunk;
				thunk = create_thunk (cfg, code, target);
				g_assert (riscv_is_jal_disp (code, thunk));
				riscv_jal (code, rd, riscv_get_jal_disp(code,thunk));
				g_print("jar %s, 0x%x <0x%lx> via thunk <%lx>",mono_arch_regname(rd), riscv_get_jal_disp(code,target), target, thunk);
				MONO_ARCH_DUMP_CODE_DEBUG(code, 1);
			}
			break;
		}
		case MONO_R_RISCV_BEQ:
		case MONO_R_RISCV_BNE:
		case MONO_R_RISCV_BGE:{
			int offset = target - code;
			g_assert (RISCV_VALID_B_IMM ((gint32) (gssize) (offset)));

			gint32 inst = *(gint32 *) code;
			gint32 rs1 = RISCV_BITS (inst, 15, 5);
			gint32 rs2 = RISCV_BITS (inst, 20, 5);
			if(relocation == MONO_R_RISCV_BEQ){
				riscv_beq (code, rs1, rs2, offset);
				g_print("BEQ %s, %s, 0x%x <0x%lx> ", mono_arch_regname(rs1), mono_arch_regname(rs2), (gint32)offset & 0xffe, offset);
			}
			else if(relocation == MONO_R_RISCV_BNE){
				riscv_bne (code, rs1, rs2, offset);
				g_print("BNE %s, %s, 0x%x <0x%lx> ", mono_arch_regname(rs1), mono_arch_regname(rs2), (gint32)offset & 0xffe, offset);
			}
			else if(relocation == MONO_R_RISCV_BGE){
				riscv_bge (code, rs1, rs2, offset);
				g_print("BGE %s, %s, 0x%x <0x%lx> ", mono_arch_regname(rs1), mono_arch_regname(rs2), (gint32)offset & 0xffe, offset);
			}
			MONO_ARCH_DUMP_CODE_DEBUG(code, 1);
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

	if( cinfo->next_areg < RISCV_A0 || cinfo->next_areg > RISCV_A7){
		ainfo->storage = ArgOnStack;
		if(cinfo->vararg){
#ifndef TARGET_RISCV64
			size = 8;
#else if TARGET_RISCV32
			size = 4;
#endif
		}
		cinfo->stack_usage = ALIGN_TO (cinfo->stack_usage, size);
		ainfo->offset = cinfo->stack_usage;
		ainfo->slot_size = size;
		ainfo->is_signed = sign;
		cinfo->stack_usage += size;
	}
	else{
		ainfo->storage = ArgInIReg;
		ainfo->reg = cinfo->next_areg;
		cinfo->next_areg ++;
	}
}

static void
add_valuetype (CallInfo *cinfo, ArgInfo *ainfo, MonoType *t){
	int i, size, align_size, nregs;
	guint32 align;

	size = mini_type_stack_size_full (t, &align, cinfo->pinvoke);
#ifndef TARGET_RISCV64
	align_size = ALIGN_TO (size, 8);
	nregs = align_size / 8;
#else if TARGET_RISCV32
	align_size = ALIGN_TO (size, 4);
	nregs = align_size / 4;
#endif
	
	if (align_size > 16) {
		ainfo->storage = ArgVtypeByRef;
		ainfo->size = size;
		return;
	}

	// save it on stack if don't have enough regs
	if (cinfo->next_areg + nregs > RISCV_A7){
		size = ALIGN_TO (size, 8);
		ainfo->storage = ArgVtypeOnStack;
		cinfo->stack_usage = ALIGN_TO (cinfo->stack_usage, align);
		ainfo->offset = cinfo->stack_usage;
		ainfo->size = size;
		cinfo->stack_usage += size;
		cinfo->next_areg = RISCV_A7 + 1;
	} else {
		ainfo->storage = ArgVtypeInIReg;
		ainfo->reg = cinfo->next_areg;
		ainfo->nregs = nregs;
		ainfo->size = size;
		cinfo->next_areg += nregs;
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
		case MONO_TYPE_I4:
			add_arg (cinfo, ainfo, 4, TRUE);
			break;
		case MONO_TYPE_U4:
			add_arg (cinfo, ainfo, 4, FALSE);
			break;
		case MONO_TYPE_I8:
#ifdef TARGET_RISCV64
		case MONO_TYPE_I:
#endif
			add_arg (cinfo, ainfo, 8, TRUE);
			break;
#ifdef TARGET_RISCV64
		case MONO_TYPE_U:
		case MONO_TYPE_OBJECT:
#endif
		case MONO_TYPE_U8:
			add_arg (cinfo, ainfo, 8, FALSE);
			break;
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
		default:
			g_print("Can't process storage type %d\n", ainfo->storage);
			NOT_IMPLEMENTED;
	}
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
			NOT_IMPLEMENTED;
		}
	}

	g_assert (!sig->hasthis);

	for (int i = 0; i < sig->param_count; i++){
		ainfo = &cinfo->args [i];

		if (ainfo->storage == ArgVtypeByRef)
			NOT_IMPLEMENTED;

		storage = arg_get_storage (ccontext, ainfo);

		interp_cb->frame_arg_to_data ((MonoInterpFrameHandle)frame, sig, i, storage);
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
		storage = arg_get_storage (ccontext, ainfo);
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

/* 
 * take the arguments and generate the arch-specific
 * instructions to properly call the function in call.
 * This includes pushing, moving arguments to the right register
 * etc.
 */
static void
emit_sig_cookie (MonoCompile *cfg, MonoCallInst *call, CallInfo *cinfo){
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
	MonoInst *in, *ins;
	MonoMethodSignature *sig;

	CallInfo *cinfo;
	int is_virtual = 0;

	sig = call->signature;
	int paramNum = sig->param_count + sig->hasthis;

	cinfo = get_call_info (cfg->mempool, sig);

	/* Emit the inst of return by return type */
	switch (cinfo->ret.storage){
		default:
			break;
		case ArgVtypeByRef:
		case ArgVtypeInIReg:
			g_print("unable process storage type 0x%x\n",cinfo->ret.storage); 
			NOT_IMPLEMENTED;
			break;
	}

	if (cinfo->struct_ret)
		// call->used_iregs |= 1 << cinfo->struct_ret;
		NOT_IMPLEMENTED;

	if (COMPILE_LLVM (cfg)) {
		/* We shouldn't be called in the llvm case */
		cfg->disable_llvm = TRUE;
		return;
	}

	for (int i = 0; i < paramNum; i++)
	{
		ArgInfo *ainfo = cinfo->args + i;
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

		if (is_virtual && i == 0) {
			NOT_IMPLEMENTED;
		}

		in = call->args [i];
		switch (ainfo->storage){
			case ArgInIReg:{
				if (!m_type_is_byref(t) && ((t->type == MONO_TYPE_I8) || (t->type == MONO_TYPE_U8))){
					NOT_IMPLEMENTED;
				}
				else{
					MONO_INST_NEW (cfg, ins, OP_MOVE);
					ins->dreg = mono_alloc_ireg (cfg);
					ins->sreg1 = in->dreg;
					MONO_ADD_INS (cfg->cbb, ins);

					mono_call_inst_add_outarg_reg (cfg, call, ins->dreg, ainfo->reg, FALSE);
				}
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
		case OP_LADD:
		case OP_LADD_IMM:
		case OP_LAND_IMM:
		case OP_LCONV_TO_I:
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

	if (cfg->method->save_lmf) {
		/* Save all callee-saved registers normally, and restore them when unwinding through an LMF */
		cfg->arch.saved_iregs |= MONO_ARCH_CALLEE_SAVED_REGS;
	}
	else {
		/* Callee saved regs */
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
		ins->inst_offset = -offset;
		offset += size;
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
		ins->inst_offset = -offset;
		offset += size;
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
		ins->inst_offset = -offset;
		offset += size;
		g_print("alloc bp_tramp_var to %ld(%s).\n", ins->inst_offset, mono_arch_regname (ins->inst_basereg));
	}

	/* Allocate locals */
	local_stack = mono_allocate_stack_slots (cfg, FALSE, &locals_stack_size, &locals_stack_align);
	if (locals_stack_align)
		offset = ALIGN_TO (offset, locals_stack_align);

	for (guint i = cfg->locals_start; i < cfg->num_varinfo; i++){
		if (local_stack [i] != -1) {
			ins = cfg->varinfo [i];
			ins->opcode = OP_REGOFFSET;
			ins->inst_basereg = cfg->frame_reg;
			ins->inst_offset = -(offset + local_stack [i]);
			g_print ("allocated local %d to %ld(s0/fp); ", i, ins->inst_offset); mono_print_ins (ins);
		}
	}
	offset += locals_stack_size;
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
	MonoInst *ins,*n, *temp;
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
			case OP_VOIDCALL:
			case OP_GET_EX_OBJ:
			case OP_I8CONST:
			case OP_ICONST:
			case OP_SHR_UN_IMM:
			case OP_MOVE:
			case OP_LADD:
			
			/* skip dummy IL */
			case OP_NOT_REACHED:
			case OP_NOT_NULL:
			case OP_DUMMY_USE:
			case OP_NOP:

			/* skip custom OP code*/
			case OP_RISCV_BEQ:
			case OP_RISCV_BNE:
			case OP_RISCV_BGE:
				break;
			
			/* Atomic Ext */
			case OP_MEMORY_BARRIER:
				break;
			case OP_ATOMIC_STORE_I4:
				if(ins->inst_offset){
					NOT_IMPLEMENTED;
					NEW_INS (cfg, ins, temp, OP_ADD_IMM);
					temp->dreg = ins->dreg;
					temp->sreg1 = ins->inst_destbasereg;
					temp->inst_imm = ins->inst_offset;
				}
				break;

			case OP_VOIDCALL_REG:
				// use JALR x1, 0(src1)
				ins->dreg = RISCV_X1;
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
			case OP_STORE_MEMBASE_IMM:
			case OP_STOREI1_MEMBASE_IMM:
			case OP_STOREI4_MEMBASE_IMM:{
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
					default:
						g_assert_not_reached();
						break;
				}
				goto loop_start; /* make it handle the possibly big ins->inst_offset */
			}
			// Inst S{B|H|W|D} use I-type Imm
			case OP_STORE_MEMBASE_REG:
			case OP_STOREI1_MEMBASE_REG:
			case OP_STOREI2_MEMBASE_REG:
			case OP_STOREI4_MEMBASE_REG:
			case OP_STOREI8_MEMBASE_REG:{
				// check if offset is valid I-type Imm
				if(! RISCV_VALID_I_IMM ((gint32) (gssize) (ins->inst_offset)))
					NOT_IMPLEMENTED;
				break;
			}
			// Inst L{B|H|W|D} use I-type Imm
			case OP_LOAD_MEMBASE:
			case OP_LOADI1_MEMBASE:
			case OP_LOADU1_MEMBASE:
			case OP_LOADI2_MEMBASE:
			case OP_LOADU2_MEMBASE:
			case OP_LOADI4_MEMBASE:
			case OP_LOADU4_MEMBASE:
			case OP_LOADI8_MEMBASE:
				if(! RISCV_VALID_I_IMM ((gint32) (gssize) (ins->inst_imm))){
					NEW_INS (cfg, ins, temp, OP_ICONST);
					temp->inst_c0 = (ins->inst_imm >> 12) << 12;
					temp->dreg = mono_alloc_ireg (cfg);
					ins->sreg1 = temp->dreg;
					ins->inst_imm = 0;
				}
				break;
			// Inst ADDI use I-type Imm
			case OP_ADD_IMM:
			case OP_LADD_IMM:
				if(! RISCV_VALID_I_IMM ((gint32) (gssize) (ins->inst_imm))){
					NEW_INS (cfg, ins, temp, OP_ICONST);
					temp->inst_c0 = (ins->inst_imm >> 12) << 12;
					temp->dreg = mono_alloc_ireg (cfg);
					ins->sreg2 = temp->dreg;
					ins->inst_imm = 0;
					// there is no OP_ADD opcode, use OP_LADD instead
					ins->opcode = OP_LADD;
				}
				break;
			case OP_LCOMPARE_IMM:
			case OP_ICOMPARE_IMM:{
				if(ins->inst_imm == 0){
					ins->sreg2 = RISCV_ZERO;
				}
				else{
					NOT_IMPLEMENTED;
				}

			case OP_LCOMPARE:				
				if (ins->next){
					if(ins->next->opcode == OP_LBEQ || ins->next->opcode == OP_IBEQ){
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
						ins->next->opcode = OP_RISCV_BGE;
						ins->next->sreg1 = ins->sreg1;
						ins->next->sreg2 = ins->sreg2;
						NULLIFY_INS (ins);
					}
					else {
						NOT_IMPLEMENTED;
					}
				}
				else{
					g_assert_not_reached();
				}
				break;
			}
			case OP_LAND_IMM:
				if(! RISCV_VALID_I_IMM ((gint32) (gssize) (ins->inst_imm))){
					NEW_INS (cfg, ins, temp, OP_ICONST);
					temp->inst_c0 = (ins->inst_imm >> 12) << 12;
					temp->dreg = mono_alloc_ireg (cfg);
					ins->sreg2 = temp->dreg;
					ins->inst_imm = 0;
					ins->opcode = OP_IAND;
				}
				break;
			default:
				printf ("unable to lowering following IR:"); mono_print_ins (ins);
				NOT_IMPLEMENTED;
				break;
		}
	}

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
		// g_print("addi %s, X0, %x\n",mono_arch_regname(rd), imm);
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
			// g_print("lui %s, %x\n",mono_arch_regname(rd), Hi);
		}
		riscv_addiw(code, rd, rd, Lo);
		// g_print("addiw %s, %s, %x\n",mono_arch_regname(rd),mono_arch_regname(rd), Lo);
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
	// g_print("load  %s, %s, 0x%lx\n",mono_arch_regname(rd), mono_arch_regname(rd), imm);
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
	g_print("Store%d %s, %d(%s)\n", length, mono_arch_regname(rs2), imm, mono_arch_regname(rs1));
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

/*
 * emit_load_regarray:
 *
 *   Emit code to load the registers in REGS from the appropriate elements of
 * a register array at BASEREG+OFFSET.
 */
guint8*
emit_load_regarray (guint8 *code, guint64 regs, int basereg, int offset, MonoBoolean isFloat){
	int i;

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

	/* pc */
	code = mono_riscv_emit_imm (code, RISCV_T6, (gsize)code);
	MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2)
	code = mono_riscv_emit_store (code, RISCV_T6, RISCV_FP, -lmf_offset + MONO_STRUCT_OFFSET (MonoLMF, pc), 0);
	MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2)
	/* gregs + fp + sp */
	// code = emit_store_regset_cfa (cfg, code, (MONO_ARCH_CALLEE_SAVED_REGS | (1 << RISCV_FP) | (1 << RISCV_SP)), RISCV_FP, lmf_offset + MONO_STRUCT_OFFSET (MonoLMF, gregs), cfa_offset, (1 << RISCV_FP) | (1 << RISCV_SP));
	code = emit_store_stack (code, (MONO_ARCH_CALLEE_SAVED_REGS | (1 << RISCV_FP) | (1 << RISCV_SP)), RISCV_FP, -lmf_offset + MONO_STRUCT_OFFSET (MonoLMF, gregs), FALSE);

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
					MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
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
					code = mono_riscv_emit_store(code, ainfo->reg, ins->inst_basereg, ins->inst_offset, 0);
					MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
					if (i == 0 && sig->hasthis) {
						mono_add_var_location (cfg, ins, TRUE, ainfo->reg, 0, 0, code - cfg->native_code);
						mono_add_var_location (cfg, ins, FALSE, ins->inst_basereg, ins->inst_offset, code - cfg->native_code, 0);
					}
					break;
				default:
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
				NOT_IMPLEMENTED;
			}
			break;
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
		MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);

		// save return value
		stack_size += sizeof(target_mgreg_t);
		code = mono_riscv_emit_store(code, RISCV_RA, RISCV_SP, cfg->stack_offset - stack_size, 0);
		MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);

		// save s0(fp) value
		stack_size += sizeof(target_mgreg_t);
		code = mono_riscv_emit_store(code, RISCV_FP, RISCV_SP, cfg->stack_offset - stack_size, 0);
		MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
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
	MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);

	// save other registers
	if (cfg->param_area) {
		/* The param area is below the frame pointer */
		riscv_addi(code, RISCV_SP, RISCV_SP, -cfg->param_area);
		MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
	}

	if (cfg->method->save_lmf){
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

	if (cfg->method->save_lmf) {
		code = emit_load_regarray (code, MONO_ARCH_CALLEE_SAVED_REGS & cfg->used_int_regs, RISCV_FP, -cfg->lmf_var->inst_offset + MONO_STRUCT_OFFSET (MonoLMF, gregs) + (MONO_ARCH_FIRST_LMF_REG * sizeof(host_mgreg_t)), FALSE);
	} else {
		/* Restore gregs */
		code = emit_load_stack (code, MONO_ARCH_CALLEE_SAVED_REGS & cfg->used_int_regs, RISCV_FP, -cfg->stack_offset + sizeof(host_mgreg_t), FALSE);
	}
	
	/* Load returned vtypes into registers if needed */
	cinfo = cfg->arch.cinfo;
	switch (cinfo->ret.storage) {
		case ArgNone:
		case ArgInIReg:
			break;
		default:
			g_print("Unable process returned storage %d(0x%x)\n",cinfo->ret.storage,cinfo->ret.storage);
			NOT_IMPLEMENTED;
	}

	/* Destroy frame */
	code = mono_riscv_emit_destroy_frame(code, cfg->stack_offset);

	if(cinfo->ret.storage == ArgNone || cinfo->ret.storage == ArgInIReg)
		riscv_jalr(code, RISCV_X0, RISCV_RA, 0);
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
		if (cfg->verbose_level > 2) {
			g_print ("    @ 0x%x\t", offset);
			mono_print_ins_index (ins_cnt++, ins);
		}

		/* Check for virtual regs that snuck by */
		g_assert ((ins->dreg >= -1) && (ins->dreg < 32));

		switch (ins->opcode) {
			case OP_NOT_REACHED:
			case OP_NOT_NULL:
			case OP_DUMMY_USE:
				break;
			case OP_GET_EX_OBJ:
				if (ins->dreg != RISCV_A0){
					// mv dreg, RISCV_A0
					riscv_addi(code, ins->dreg, RISCV_A0, 0);
					MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				}
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
			case OP_NOP:
				code = mono_riscv_emit_nop(code);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_MOVE:
				// mv ra, a1 -> addi ra, a1, 0
				riscv_addi(code, ins->dreg, ins->sreg1, 0);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_LOAD_MEMBASE:
				code = mono_riscv_emit_load(code, ins->dreg, ins->sreg1, ins->inst_offset, 0);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_LOADI1_MEMBASE:
			case OP_LOADU1_MEMBASE:
				code = mono_riscv_emit_load(code, ins->dreg, ins->sreg1, ins->inst_offset, 1);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_LOADI2_MEMBASE:
			case OP_LOADU2_MEMBASE:
				code = mono_riscv_emit_load(code, ins->dreg, ins->sreg1, ins->inst_offset, 2);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_LOADU4_MEMBASE:
			case OP_LOADI4_MEMBASE:
				code = mono_riscv_emit_load(code, ins->dreg, ins->sreg1, ins->inst_offset, 4);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_LOADI8_MEMBASE:
				code = mono_riscv_emit_load(code, ins->dreg, ins->sreg1, ins->inst_offset, 8);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_STORE_MEMBASE_REG:
				code = mono_riscv_emit_store(code, ins->sreg1, ins->dreg, ins->inst_offset, 0);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_STOREI1_MEMBASE_REG:
				code = mono_riscv_emit_store(code, ins->sreg1, ins->dreg, ins->inst_offset, 1);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_STOREI2_MEMBASE_REG:
				code = mono_riscv_emit_store(code, ins->sreg1, ins->dreg, ins->inst_offset, 2);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_STOREI4_MEMBASE_REG:
				code = mono_riscv_emit_store(code, ins->sreg1, ins->dreg, ins->inst_offset, 4);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_STOREI8_MEMBASE_REG:
				code = mono_riscv_emit_store(code, ins->sreg1, ins->dreg, ins->inst_offset, 8);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_ICONST:
			case OP_I8CONST:
				code = mono_riscv_emit_imm(code, ins->dreg, ins->inst_c0);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_LADD:
				riscv_add(code, ins->dreg, ins->sreg1, ins->sreg2);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_ADD_IMM:
			case OP_LADD_IMM:
				riscv_addi(code, ins->dreg, ins->sreg1, ins->inst_imm);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;

			/* Bit/logic */
			case OP_IAND:
				riscv_and(code, ins->dreg, ins->sreg1, ins->sreg2);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_LAND_IMM:
				riscv_andi(code, ins->dreg, ins->sreg1, ins->inst_imm);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_SHR_UN_IMM:
				riscv_srli(code, ins->dreg, ins->sreg1, ins->inst_imm);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;

			/* Atomic */
			case OP_MEMORY_BARRIER:
				riscv_fence(code, RISCV_FENCE_MEM, RISCV_FENCE_MEM);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_ATOMIC_STORE_I4:{
				gint ordering = mono_arch_get_memory_ordering(ins->backend.memory_barrier_kind);
#ifdef TARGET_RISCV64
				riscv_sc_w(code, ordering, RISCV_ZERO, ins->sreg1, ins->dreg);
#else
				riscv_sc_d(code, ordering, RISCV_ZERO, ins->sreg1, ins->dreg);
#endif
				break;
			}

			/* Calls */
			case OP_CALL:
			case OP_VOIDCALL:{
				call = (MonoCallInst*)ins;
				const MonoJumpInfoTarget patch = mono_call_to_patch (call);
				code = mono_riscv_emit_call (cfg, code, patch.type, patch.target);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				code = emit_move_return_value (cfg, code, ins);
				break;
			}
			case OP_VOIDCALL_REG:
				// use JALR x1, 0(src1)
				riscv_jalr(code, RISCV_RA, ins->sreg1, 0);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;

			/* Branch */
			case OP_RISCV_BNE:
				mono_add_patch_info_rel (cfg, (code - cfg->native_code), MONO_PATCH_INFO_BB, ins->inst_true_bb, MONO_R_RISCV_BNE);
				riscv_bne(code, ins->sreg1, ins->sreg2, 0);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_RISCV_BEQ:
				mono_add_patch_info_rel (cfg, (code - cfg->native_code), MONO_PATCH_INFO_BB, ins->inst_true_bb, MONO_R_RISCV_BEQ);
				riscv_beq(code, ins->sreg1, ins->sreg2, 0);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_RISCV_BGE:
				mono_add_patch_info_rel (cfg, (code - cfg->native_code), MONO_PATCH_INFO_BB, ins->inst_true_bb, MONO_R_RISCV_BGE);
				riscv_bge(code, ins->sreg1, ins->sreg2, 0);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
			case OP_BR:
				mono_add_patch_info_rel (cfg, offset, MONO_PATCH_INFO_BB, ins->inst_target_bb, MONO_R_RISCV_JAL);
				riscv_jal (code, RISCV_ZERO, 0);
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
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

			/* Throw */
			case OP_THROW:
				code = mono_riscv_emit_call (cfg, code, MONO_PATCH_INFO_JIT_ICALL_ID,
							  GUINT_TO_POINTER (MONO_JIT_ICALL_mono_arch_throw_exception));
				break;
			case OP_RETHROW:
				code = mono_riscv_emit_call (cfg, code, MONO_PATCH_INFO_JIT_ICALL_ID,
							  GUINT_TO_POINTER (MONO_JIT_ICALL_mono_arch_rethrow_exception));
				MONO_ARCH_DUMP_CODE_DEBUG(code, cfg->verbose_level > 2);
				break;
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
