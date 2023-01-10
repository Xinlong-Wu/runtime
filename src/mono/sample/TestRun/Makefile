TOP=../../../../
DOTNET:=$(TOP)dotnet.sh
DOTNET_Q_ARGS=--nologo -v:q -consoleloggerparameters:NoSummary

MONO_CONFIG?=Debug
MONO_ARCH=riscv64
TARGET_OS?=$(shell . $(TOP)eng/native/init-os-and-arch.sh && echo $${os} | tr "[:upper:]" "[:lower:]")
AOT?=false

#NET_TRACE_PATH=<path-to-trace-of-sample>
#PGO_BINARY_PATH=<path-to-dotnet-pgo-executable>
#MIBC_PROFILE_PATH=<path-to-mibc-for-sample>

MONO_ENV_DEBUG_OPTIONS = "-v -v -v -v --compile-all --debug --debugger-agent=transport=dt_socket,address=127.0.0.1:1235,server=y,suspend=y,loglevel=10"
MONO_ENV_RUN_OPTIONS = "--compile-all"

publish:
	$(DOTNET) publish \
	-c $(MONO_CONFIG) \
	-r $(TARGET_OS)-$(MONO_ARCH) \
	/p:RunAOTCompilation=$(AOT) \
	'/p:NetTracePath="$(NET_TRACE_PATH)"' \
	'/p:PgoBinaryPath="$(PGO_BINARY_PATH)"' \
	'/p:MibcProfilePath="$(MIBC_PROFILE_PATH)"'

run: publish
	COMPlus_DebugWriteToStdErr=1 \
	MONO_ENV_OPTIONS=$(MONO_ENV_RUN_OPTIONS) \
	$(TOP)artifacts/bin/TestRun/$(MONO_ARCH)/$(MONO_CONFIG)/$(TARGET_OS)-$(MONO_ARCH)/publish/TestRun

debug: publish
	COMPlus_DebugWriteToStdErr=1 \
	MONO_ENV_OPTIONS=$(MONO_ENV_DEBUG_OPTIONS) \
	$(TOP)artifacts/bin/TestRun/$(MONO_ARCH)/$(MONO_CONFIG)/$(TARGET_OS)-$(MONO_ARCH)/publish/TestRun

clean:
	rm -rf $(TOP)artifacts/bin/TestRun/
