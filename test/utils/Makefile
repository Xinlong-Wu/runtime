
LLVM_PATH=/home/wuxinlong/opt/llvm/

C_FLAGS=$(shell llvm-config --cflags)
LD_FLAGS=$(shell llvm-config --ldflags --libs --system-libs)

native-dump: native-dump.cpp
	clang++ ${C_FLAGS} -o $@.o $^ ${LD_FLAGS}