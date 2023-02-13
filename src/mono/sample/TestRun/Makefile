TOP=../../../../
DOTNET:=$(TOP)dotnet.sh
DOTNET_Q_ARGS=--nologo -v:q -consoleloggerparameters:NoSummary

MONO_CONFIG?=Debug
MONO_ARCH=riscv64
TARGET_OS=Linux
AOT?=false

#NET_TRACE_PATH=<path-to-trace-of-sample>
#PGO_BINARY_PATH=<path-to-dotnet-pgo-executable>
#MIBC_PROFILE_PATH=<path-to-mibc-for-sample>

MONO_ENV_DEBUG_OPTIONS = "-v -v -v -v --compile-all --debug --debugger-agent=transport=dt_socket,address=127.0.0.1:1235,server=y,suspend=y,loglevel=10"
MONO_ENV_RUN_OPTIONS = "-v -v -v -v --compile-all"

build: TestRun.cs
	csc TestRun.cs

run-baseline: build
	COMPlus_DebugWriteToStdErr=1 \
	$(TOP)artifacts/bin/testhost/net8.0-$(TARGET_OS)-Debug-x64/dotnet TestRun.exe

run: build
	COMPlus_DebugWriteToStdErr=1 \
	MONO_ENV_OPTIONS=$(MONO_ENV_RUN_OPTIONS) \
	qemu-riscv64 -L $(TOP)/.tools/rootfs/riscv64/ $(TOP)artifacts/bin/testhost/net8.0-$(TARGET_OS)-Debug-$(MONO_ARCH)/dotnet TestRun.exe

debug: build
	COMPlus_DebugWriteToStdErr=1 \
	MONO_ENV_OPTIONS=$(MONO_ENV_RUN_OPTIONS) \
	qemu-riscv64 -L $(TOP)/.tools/rootfs/riscv64/ \
	-g 12345 \
	$(TOP)artifacts/bin/testhost/net8.0-$(TARGET_OS)-Debug-$(MONO_ARCH)/dotnet TestRun.exe &\
	riscv64-unknown-linux-gnu-gdb -ex 'target remote localhost:12345'  \
	-ex "set sysroot $(TOP).tools/rootfs/riscv64" -ex "set solib-search-path $(TOP)artifacts/bin/testhost/net8.0-Linux-Debug-riscv64/shared/Microsoft.NETCore.App/8.0.0" \
	-ex "b mono_arch_emit_outarg_vt" \
	$(TOP)artifacts/bin/testhost/net8.0-$(TARGET_OS)-Debug-$(MONO_ARCH)/dotnet

clean:
	rm -f *.exe *.exe.mdb
