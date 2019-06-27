.data

.text
# PROCEDURE _tigermain
	.align 2
	.type _tigermain, @function
	.globl _tigermain
_tigermain:
	addi sp, sp, -176
	sd s0, 168(sp)
	sd a0, 160(sp)
	sd ra, 152(sp)
	addi s0, sp, 168
L6:
	SD s1, -128(s0)
	SD s2, -136(s0)
	SD s3, -144(s0)
	SD s4, -152(s0)
	SD s5, -160(s0)
	SD s6, -168(s0)
	SD s7, -40(s0)
	SD s8, -48(s0)
	SD a0, -8(s0)
	LI s10, 9
	SD s10, -56(s0)
	LD s9, -56(s0)
	SD s9, -24(s0)
	LI s10, 7
	SD s10, -64(s0)
	LD s9, -64(s0)
	SD s9, -32(s0)
	LI s10, 0
	SD s10, -72(s0)
	LD s9, -72(s0)
	SD s9, -112(s0)
	LD s10, -24(s0)
	LD s11, -32(s0)
	BLT s11, s10, L0
L1:
	LI s10, 0
	SD s10, -80(s0)
	LD s11, -112(s0)
	LD s10, -80(s0)
	BNE s11, s10, L2
L3:
	LI s10, 123
	SD s10, -88(s0)
	LD s9, -88(s0)
	SD s9, -120(s0)
L4:
	LD a0, -120(s0)
	LD s1, -128(s0)
	LD s2, -136(s0)
	LD s3, -144(s0)
	LD s4, -152(s0)
	LD s5, -160(s0)
	LD s6, -168(s0)
	LD s7, -40(s0)
	LD s8, -48(s0)
	J L5
L0:
	LI s10, 1
	SD s10, -96(s0)
	LD s9, -96(s0)
	SD s9, -112(s0)
	J L1
L2:
	LI s10, 99
	SD s10, -104(s0)
	LD s9, -104(s0)
	SD s9, -120(s0)
	J L4
L5:
	ld ra, 152(sp)
	ld s0, 168(sp)
	addi sp, sp, 176
	jr ra
# END _tigermain

