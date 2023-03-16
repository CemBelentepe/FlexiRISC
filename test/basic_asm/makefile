CC32=riscv32-unknown-elf
CC64=riscv64-unknown-elf
TEST_NAME=basic_asm
CCFLAGS=-march=rv32i -mabi=ilp32 -O3 -T ../linksc.ld -nostartfiles -ffunction-sections -fdata-sections -Wl,--gc-sections -o $(TEST_NAME).elf
	
build:
	$(CC32)-gcc $(TEST_NAME).asm $(CCFLAGS)
	$(CC32)-objcopy -O binary -j .init -j .text -j .rodata $(TEST_NAME).elf $(TEST_NAME).bin
	../rom_generator $(TEST_NAME).bin
	cp $(TEST_NAME).data ../memory_contents
	$(CC32)-objdump -D $(TEST_NAME).elf > $(TEST_NAME)_com.s

multilib:
	$(CC64)-gcc $(TEST_NAME).asm $(CCFLAGS)
	$(CC64)-objcopy -O binary -j .init -j .text -j .rodata $(TEST_NAME).elf $(TEST_NAME).bin
	../rom_generator $(TEST_NAME).bin
	cp $(TEST_NAME).data ../memory_contents
	$(CC64)-objdump -D $(TEST_NAME).elf > $(TEST_NAME)_com.s
