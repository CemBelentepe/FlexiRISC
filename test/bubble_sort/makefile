CC32=riscv32-unknown-elf
CC64=riscv64-unknown-elf
CCFLAGS=-march=rv32im -mabi=ilp32 -O3 -T ../linksc.ld -nostartfiles -ffunction-sections -fdata-sections -Wl,--gc-sections -o bubble_sort.elf
	
build:
	$(CC32)-gcc bubble_sort.c ../crt0.s $(CCFLAGS)
	$(CC32)-objcopy -O binary -j .init -j .text -j .rodata bubble_sort.elf bubble_sort.bin
	../rom_generator bubble_sort.bin
	cp bubble_sort.data ../memory_contents
	$(CC32)-objdump -D bubble_sort.elf > bubble_sort.s

multilib:
	$(CC64)-gcc bubble_sort.c ../crt0.s $(CCFLAGS)
	$(CC64)-objcopy -O binary -j .init -j .text -j .rodata bubble_sort.elf bubble_sort.bin
	../rom_generator bubble_sort.bin
	cp bubble_sort.data ../memory_contents
	$(CC64)-objdump -D bubble_sort.elf > bubble_sort.s