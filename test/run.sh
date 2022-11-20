make -j 30;
qemu-riscv64 -L /home/wuxinlong/opt/riscv/sysroot/ /home/wuxinlong/workspace/mono/mono/mini/mono $*

# ./test/run.sh --interp  --compile HelloWorld.Program:Main test/hello.exe

# llvm-mc test/riscv.s -triple=riscv64 -riscv-no-aliases -show-encoding