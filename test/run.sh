# make -j 30;
qemu-riscv64-static -L ../.tools/rootfs/riscv64 mono $*

# ./test/run.sh --interp  --compile HelloWorld.Program:Main test/hello.exe

# llvm-mc test/riscv.s -triple=riscv64 -riscv-no-aliases -show-encoding