# make -j 30;
MONO_PATH=/home/wuxinlong/workspace/runtime/artifacts/bin/mono/Linux.riscv64.Debug \
qemu-riscv64 -L ../.tools/rootfs/riscv64 mono -v -v -v -v $*

# ./test/run.sh --interp  --compile HelloWorld.Program:Main test/hello.exe

# llvm-mc test/riscv.s -triple=riscv64 -riscv-no-aliases -show-encoding