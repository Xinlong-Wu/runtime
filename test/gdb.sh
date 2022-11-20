
make -j 30;

qemu-riscv64 -L /home/wuxinlong/opt/riscv/sysroot/  -g 12345 \
/home/wuxinlong/workspace/mono/mono/mini/mono $* & \
riscv64-unknown-linux-gnu-gdb -ex 'target remote localhost:12345' -ex 'b main' -ex 'c' /home/wuxinlong/workspace/mono/mono/mini/mono
