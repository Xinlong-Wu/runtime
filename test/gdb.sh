
make -j 30;

MONO_PATH=/home/wuxinlong/workspace/runtime/artifacts/bin/mono/Linux.riscv64.Debug \
qemu-riscv64 -L ../.tools/rootfs/riscv64  -g 12345 \
mono -v -v -v -v $* & \
riscv64-unknown-linux-gnu-gdb -ex 'target remote localhost:12345' -ex 'b main' -ex 'c' mono
