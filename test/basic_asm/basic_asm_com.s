
basic_asm.elf:     file format elf32-littleriscv


Disassembly of section .text:

00000000 <_start>:
   0:	00002097          	auipc	ra,0x2
   4:	ffc08093          	addi	ra,ra,-4 # 1ffc <__BSS_END__+0xfe4>
   8:	00a00113          	li	sp,10
   c:	0020a023          	sw	sp,0(ra)
  10:	0000a183          	lw	gp,0(ra)

00000014 <end>:
  14:	0000006f          	j	14 <end>
