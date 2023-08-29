			# This code was produced by the CERI Compiler
	.data
FormatString1:	.string "%llu\n"	# used by printf to display 64-bit unsigned integers
TrueString:	.string "TRUE\n"	# used by printf to display the boolean value TRUE
FalseString:	.string "FALSE\n"	# used by printf to display the boolean value FALSE
b:	.quad 0
c:	.quad 0
a:	.quad 0
d:	.quad 0
e:	.quad 0
f:	.quad 0
g:	.quad 0
	.text		# The following lines contain the program
	.globl main	# The main function must be visible from outside
main:			# The main function body :
	movq %rsp, %rbp	# Save the position of the stack's top
	push a
	push $1
	pop a
	push d
	push $2
	pop d
CASE0:
	push a
Switch0:
	push d
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	push %rbx
	je Statement1
	jne Switch1
Statement1:
	push a
	push $5
	pop a
	jmp EndCase0
Switch1:
	push $1
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	push %rbx
	je Statement2
	push $2
	pop %rax
	pop %rbx
	cmpq %rax, %rbx
	push %rbx
	je Statement2
	jne Switch2
Statement2:
	push a
	push a
	push $1
	pop %rbx
	pop %rax
	addq	%rbx, %rax	# ADD
	push %rax
	pop a
	jmp EndCase0
Switch2:
EndCase0:
	movq %rbp, %rsp		# Restore the position of the stack's top
	ret			# Return from main function
