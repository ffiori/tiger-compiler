.data

.text
# PROCEDURE _tigermain
	.align 2
	.type _tigermain, @function
	.globl _tigermain
_tigermain:
	addi sp, sp, -32
	sd s0, 24(sp)
	sd a0, 16(sp)
	sd ra, 8(sp)
	addi s0, sp, 24
L1:
	ADD s1, x0, s1
	ADD s2, x0, s2
	ADD s3, x0, s3
	ADD s4, x0, s4
	ADD s5, x0, s5
	ADD s6, x0, s6
	ADD s7, x0, s7
	ADD s8, x0, s8
	SD a0, -8(s0)
	LI a1, 0
	ADD a1, x0, a1
	LI a1, 0
	ADD a1, x0, a1
	LI a0, 0
	ADD a0, x0, a0
	ADD a1, x0, a1
	ADD a1, x0, a0
	ADD a1, x0, a0
	LI a0, 0
	ADD a0, x0, a0
	ADD s1, x0, s1
	ADD s2, x0, s2
	ADD s3, x0, s3
	ADD s4, x0, s4
	ADD s5, x0, s5
	ADD s6, x0, s6
	ADD s7, x0, s7
	ADD s8, x0, s8
	J L0
L0:
	ld ra, 8(sp)
	ld s0, 24(sp)
	addi sp, sp, 32
	jr ra
# END _tigermain

