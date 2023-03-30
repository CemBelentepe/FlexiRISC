CC32=riscv32-unknown-elf
CC64=riscv64-unknown-elf
TEST_NAME=basic_test
CCFLAGS=-march=rv64i -mabi=lp64 -O3 -T ../linksc.ld -nostartfiles -ffunction-sections -fdata-sections -Wl,--gc-sections -o $(TEST_NAME).elf
	
build:
	$(CC32)-gcc *.c ../crt0.s $(CCFLAGS)
	$(CC32)-objcopy -O binary -j .init -j .text -j .rodata $(TEST_NAME).elf $(TEST_NAME).bin
	../rom_generator $(TEST_NAME).bin
	cp $(TEST_NAME).data ../memory_contents
	$(CC32)-objdump -D basic_test.elf > basic_test.s

multilib:
	$(CC64)-gcc *.c ../crt0.s $(CCFLAGS)
	$(CC64)-objcopy -O binary -j .init -j .text -j .rodata $(TEST_NAME).elf $(TEST_NAME).bin
	../rom_generator $(TEST_NAME).bin
	cp $(TEST_NAME).data ../memory_contents
	$(CC64)-objdump -D basic_test.elf > basic_test.s
