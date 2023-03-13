
basic_test.elf:     file format elf32-littleriscv


Disassembly of section .init:

00000000 <_start>:
   0:	00002117          	auipc	sp,0x2
   4:	ffc10113          	addi	sp,sp,-4 # 1ffc <__stack_top>
   8:	00010433          	add	s0,sp,zero
   c:	0040006f          	j	10 <main>

Disassembly of section .text:

00000010 <main>:
  10:	ff010113          	addi	sp,sp,-16
  14:	00500793          	li	a5,5
  18:	00f12223          	sw	a5,4(sp)
  1c:	00a00793          	li	a5,10
  20:	00f12423          	sw	a5,8(sp)
  24:	00412783          	lw	a5,4(sp)
  28:	00812703          	lw	a4,8(sp)
  2c:	00e787b3          	add	a5,a5,a4
  30:	00f12623          	sw	a5,12(sp)
  34:	0000006f          	j	34 <main+0x24>

Disassembly of section .eh_frame:

00000038 <__BSS_END__-0x1028>:
  38:	0010                	0x10
  3a:	0000                	unimp
  3c:	0000                	unimp
  3e:	0000                	unimp
  40:	7a01                	lui	s4,0xfffe0
  42:	0052                	c.slli	zero,0x14
  44:	7c01                	lui	s8,0xfffe0
  46:	0101                	addi	sp,sp,0
  48:	00020d1b          	0x20d1b
  4c:	0010                	0x10
  4e:	0000                	unimp
  50:	0018                	0x18
  52:	0000                	unimp
  54:	ffac                	fsw	fa1,120(a5)
  56:	ffff                	0xffff
  58:	0010                	0x10
  5a:	0000                	unimp
  5c:	0000                	unimp
	...

Disassembly of section .comment:

00000000 <.comment>:
   0:	3a434347          	fmsub.d	ft6,ft6,ft4,ft7,rmm
   4:	2820                	fld	fs0,80(s0)
   6:	29554e47          	fmsub.s	ft8,fa0,fs5,ft5,rmm
   a:	3720                	fld	fs0,104(a4)
   c:	322e                	fld	ft4,232(sp)
   e:	302e                	fld	ft0,232(sp)
	...
