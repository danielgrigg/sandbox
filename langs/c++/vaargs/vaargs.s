	.text
.globl _Foo
_Foo:
LFB2:
	pushq	%rbp
LCFI0:
	movq	%rsp, %rbp
LCFI1:
	subq	$120, %rsp
LCFI2:
	movq	%rsi, -168(%rbp)
	movq	%rdx, -160(%rbp)
	movq	%rcx, -152(%rbp)
	movq	%r8, -144(%rbp)
	movq	%r9, -136(%rbp)
	movzbl	%al, %eax
	movq	%rax, -232(%rbp)
	movq	-232(%rbp), %rdx
	leaq	0(,%rdx,4), %rax
	leaq	L2(%rip), %rdx
	movq	%rdx, -232(%rbp)
	subq	%rax, -232(%rbp)
	leaq	-1(%rbp), %rax
	movq	-232(%rbp), %rdx
	jmp	*%rdx
	movaps	%xmm7, -15(%rax)
	movaps	%xmm6, -31(%rax)
	movaps	%xmm5, -47(%rax)
	movaps	%xmm4, -63(%rax)
	movaps	%xmm3, -79(%rax)
	movaps	%xmm2, -95(%rax)
	movaps	%xmm1, -111(%rax)
	movaps	%xmm0, -127(%rax)
L2:
	movl	%edi, -212(%rbp)
	leaq	-208(%rbp), %rax
	movl	$8, (%rax)
	leaq	-208(%rbp), %rax
	movl	$48, 4(%rax)
	leaq	-208(%rbp), %rax
	leaq	16(%rbp), %rdx
	movq	%rdx, 8(%rax)
	leaq	-208(%rbp), %rax
	leaq	-176(%rbp), %rdx
	movq	%rdx, 16(%rax)
	movl	-208(%rbp), %eax
	cmpl	$48, %eax
	jae	L3
	movq	-192(%rbp), %rdx
	movl	-208(%rbp), %eax
	mov	%eax, %eax
	addq	%rax, %rdx
	movq	%rdx, -224(%rbp)
	movl	-208(%rbp), %eax
	addl	$8, %eax
	movl	%eax, -208(%rbp)
	jmp	L5
L3:
	movq	-200(%rbp), %rax
	movq	%rax, -224(%rbp)
	addq	$8, %rax
	movq	%rax, -200(%rbp)
L5:
	movq	-224(%rbp), %rax
	movl	(%rax), %eax
	movl	%eax, -180(%rbp)
	movl	-180(%rbp), %eax
	leave
	ret
LFE2:
.globl _main
_main:
LFB3:
	pushq	%rbp
LCFI3:
	movq	%rsp, %rbp
LCFI4:
	movl	$42, %esi
	movl	$1, %edi
	movl	$0, %eax
	call	_Foo
	leave
	ret
LFE3:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0x0
	.byte	0x1
	.ascii "zR\0"
	.byte	0x1
	.byte	0x78
	.byte	0x10
	.byte	0x1
	.byte	0x10
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.byte	0x90
	.byte	0x1
	.align 3
LECIE1:
.globl _Foo.eh
_Foo.eh:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB2-.
	.set L$set$2,LFE2-LFB2
	.quad L$set$2
	.byte	0x0
	.byte	0x4
	.set L$set$3,LCFI0-LFB2
	.long L$set$3
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xd
	.byte	0x6
	.align 3
LEFDE1:
.globl _main.eh
_main.eh:
LSFDE3:
	.set L$set$5,LEFDE3-LASFDE3
	.long L$set$5
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB3-.
	.set L$set$6,LFE3-LFB3
	.quad L$set$6
	.byte	0x0
	.byte	0x4
	.set L$set$7,LCFI3-LFB3
	.long L$set$7
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$8,LCFI4-LCFI3
	.long L$set$8
	.byte	0xd
	.byte	0x6
	.align 3
LEFDE3:
	.subsections_via_symbols
