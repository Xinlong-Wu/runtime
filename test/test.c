HANDLES(RTH_2, "GetAssembly", ves_icall_RuntimeTypeHandle_GetAssembly, void, 2, (MonoQCallTypeHandle, MonoObjectHandleOnStack))

MONO_HANDLE_DECLARE_RAW (RTH_2, "GetAssembly", ves_icall_RuntimeTypeHandle_GetAssembly, void, 2, (MonoQCallTypeHandle, MonoObjectHandleOnStack)); \

#define MONO_HANDLE_DECLARE_RAW(id, name, func, rettype, n, argtypes)	\
ICALL_EXPORT MONO_HANDLE_TYPE_RAWPOINTER_MONO_HANDLE_TYPE_WRAP_void(void)   \
ves_icall_RuntimeTypeHandle_GetAssembly_raw ( MONO_HANDLE_FOREACH_ARG_RAW_2 (MonoQCallTypeHandle, MonoObjectHandleOnStack))

MONO_HANDLE_DECLARE (RTH_2, "GetAssembly", ves_icall_RuntimeTypeHandle_GetAssembly, void, 2, (MonoQCallTypeHandle, MonoObjectHandleOnStack));
