	.text
	.file	"ex.ll"
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
# BB#0:
	movl	$120, %eax
	xorl	%edx, %edx
	retl
.Ltmp0:
	.size	main, .Ltmp0-main


	.section	".note.GNU-stack","",@progbits
