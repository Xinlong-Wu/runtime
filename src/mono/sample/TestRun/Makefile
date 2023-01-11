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
MONO_ENV_RUN_OPTIONS = "-v -v -v -v --interp"

build: TestRun.cs
	mono-csc TestRun.cs --debug

run-baseline: build
	COMPlus_DebugWriteToStdErr=1 \
	$(TOP)artifacts/bin/testhost/net8.0-$(TARGET_OS)-Debug-x64/dotnet TestRun.exe

run: build
	COMPlus_DebugWriteToStdErr=1 \
	MONO_ENV_OPTIONS=$(MONO_ENV_RUN_OPTIONS) \
	qemu-riscv64-static -L $(TOP)/.tools/rootfs/riscv64/ $(TOP)artifacts/bin/testhost/net8.0-$(TARGET_OS)-Debug-$(MONO_ARCH)/dotnet TestRun.exe

debug: build
	COMPlus_DebugWriteToStdErr=1 \
	MONO_ENV_OPTIONS=$(MONO_ENV_DEBUG_OPTIONS) \
	qemu-riscv64-static -L $(TOP)/.tools/rootfs/riscv64/ $(TOP)artifacts/bin/testhost/net8.0-$(TARGET_OS)-Debug-$(MONO_ARCH)/dotnet TestRun.exe

clean:
	rm -f *.exe *.exe.mdb
