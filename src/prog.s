.data

.text
# PROCEDURE _tigermain
	.align 2
	.type _tigermain, @function
	.globl _tigermain
_tigermain:
	addi sp, sp, -136
	sd s0, 128(sp)
	sd a0, 120(sp)
	sd ra, 112(sp)
	addi s0, sp, 128
L6:
	LI s10, 9
	SD s10, -64(s0)
	LD s11, -64(s0)
	ADD s10, x0, s11
	SD s10, -48(s0)
	LI s10, 7
	SD s10, -72(s0)
	LD s11, -72(s0)
	ADD s10, x0, s11
	SD s10, -56(s0)
	LI s10, 0
	SD s10, -80(s0)
	LD s10, -80(s0)
	ADD s11, x0, s10
	SD s11, -120(s0)
	LD s10, -48(s0)
	LD s11, -56(s0)
	BLT s11, s10, L0
L1:
	LI s10, 0
	SD s10, -88(s0)
	LD s11, -120(s0)
	LD s10, -88(s0)
	BNE s11, s10, L2
L3:
	LI s10, 123
	SD s10, -96(s0)
	LD s10, -96(s0)
	ADD s11, x0, s10
	SD s11, -128(s0)
L4:
	LD s10, -128(s0)
	ADD a0, x0, s10
	J L5
L0:
	LI s10, 1
	SD s10, -104(s0)
	LD s10, -104(s0)
	ADD s11, x0, s10
	SD s11, -120(s0)
	J L1
L2:
	LI s10, 999
	SD s10, -112(s0)
	LD s10, -112(s0)
	ADD s11, x0, s10
	SD s11, -128(s0)
	J L4
L5:
	ld ra, 112(sp)
	ld s0, 128(sp)
	addi sp, sp, 136
	jr ra
# END _tigermain

