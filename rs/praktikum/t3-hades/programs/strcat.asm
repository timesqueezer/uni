Start:
	.org	0x8000
	.ascii	"TestStringhdt ?"
	.defw	0
	.org	0x0000
	movi	R10, 8
	lsli	R10, 12
	jsr	strlen
	halt
strlen:
	movi	R0, 0	; to compare
	movi	R11, 0	; length = 0
	movi	R1, 0	; current character
loop:
	ldw	R1, 0(R10)
	cmpe	R0, R1
	bt	strlenend
	addi	R11, 1
	addi	R10, 2
	br	loop
strlenend:
	jmp	r15