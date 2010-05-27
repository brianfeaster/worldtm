#define DEBUG 0
#define VALIDATE 1
#define DB_MODULE "VM "
#include "debug.h"
#include <stdio.h>
#include <string.h>
#include <signal.h> /* for signal() */
#include <unistd.h> /* For ualarm() */
#include "vm.h"

#include <unistd.h> /* For write(). */

#define OPDB(s) DBE fprintf(stderr,"\n%08x:%04x " s, code, ((Obj*)ip-(Obj*)code))


/* Conditional Branch   - Immediate/Relative  near/far
   Unconditional Branch - Immediate/Relative  near/far
   Call                 - Immediate/Relative  near/far

 No need to have separate immediate/relative addresses.  Relative is always
 better.  Might want to merge blocks together?  Can then optimize far jump
 into a near jump

Conditional Branch    - near/far ; Can do either near/far as caller won't
Jump                  - near/far ; return.  Easy to optimize if blocks merged.
Call/Ret              - far ; Caller should always assume ret signifies end of
                            ; execution of block and return to calling block.

BRANCH block inst
BRANCH inst
CALL block inst
RET (saved block/inst)
*/

void memDebugDumpHeapHeaders (void);

fp vmCallerPreGarbageCollect = 0,
   vmCallerPostGarbageCollect = 0;

int vmGCCount=0;

void vmPreGarbageCollect (void) {
	DB("VM -->vmPreGarbageCollect");
	if (vmGCCount++ == 0) {
		if (vmCallerPreGarbageCollect) vmCallerPreGarbageCollect();
		/* Only convert ip to offset if it's a pointer (big number larger pointing
			into the code object).  Multiply by four to force opcode offset (not
			byte). */
		if (ip >= code) ip = (Obj)((ip - code)/4);
		DB("VM <--vmPreGarbageCollect");
	}
}
void vmPostGarbageCollect (void) {
	DB("VM -->vmPostGarbageCollect");
	if (vmGCCount-- == 1) {
		if (vmCallerPostGarbageCollect) vmCallerPostGarbageCollect();
		/* Only convert ip to pointer if it's an offset (low number below the
			code's address). */
		if (ip < code) ip = code + ((int)ip)*4;
		DB("VM <--vmPostGarbageCollect");
	}
}

/* Default Object serializer
 */
void vmObjectDumperDefault (Obj o) {
 static char buff[80];
 int len;
	len = sprintf (buff, "%08x", o);
	write (1, buff, len);
}

void (*vmObjectDumper)(Obj o) = vmObjectDumperDefault;



/* This causes the machine to make a call to the interrupt handler.
*/
int interrupt=0;

void vmSigAlarmHandler (int sig) {
	interrupt=1;
}

void vmSigAlarmReset (void) {
	ualarm(10*1000,0); /* 10 miliseconds (100 tics/sec)*/
}

fp vmCallerInterruptHandler = 0;

void vmInterruptHandler (void) {
	DB("VM -->vmInterruptHandler <= %08x %08x", code, ip);
	/* Calling these pre/post functions for now until I abstract better the
		concept of massaging register values into garbage-collector-friendly
		values. */
	vmPreGarbageCollect();
	if (vmCallerInterruptHandler) vmCallerInterruptHandler();
	vmPostGarbageCollect();
	interrupt=0;
	vmSigAlarmReset();
	DB("VM <--vmInterruptHandler => %08x %08x", code, ip);
}


/* Virtual Machine
*/

#define INIT 0
#define RUN  1
void vmVm (int cmd) {
	/* Initialize the opcodes.  Really we're just assigning a bunch of global
		label pointers the jump addresses of all the opcode implementations in
		this function. */
	if (cmd == INIT) {
		NOP=&&nop;

		MVI0=&&mvi0;   MVI1=&&mvi1;   MVI2=&&mvi2;  MVI3=&&mvi3; MVI4=&&mvi4;
		MVI5=&&mvi5;   MVI6=&&mvi6;   MVI7=&&mvi7;
		MV01=&&mv01;   MV02=&&mv02;   MV03=&&mv03;  MV04=&&mv04; MV07=&&mv07;
		MV016=&&mv016; MV01C=&&mv01c;
		MV10=&&mv10;   MV13=&&mv13;  MV116=&&mv116; MV20=&&mv20;   MV23=&&mv23;
		MV30=&&mv30;   MV416=&&mv416; MV50=&&mv50;
      MV160=&&mv160; MV162=&&mv162; MV164=&&mv164;

		LDI00=&&ldi00;   LDI02=&&ldi02;   LDI016=&&ldi016;   LDI11=&&ldi11;
      LDI116=&&ldi116; LDI20=&&ldi20;   LDI22=&&ldi22;     LDI40=&&ldi40;
      LDI50=&&ldi50;   LDI160=&&ldi160; LDI1616=&&ldi1616;

      LD012=&&ld012; STI01=&&sti01; STI016=&&sti016; STI20=&&sti20;
      STI21=&&sti21;
      STI30=&&sti30; STI40=&&sti40; STI50=&&sti50; ST012=&&st012;
      ST201=&&st201;

		PUSH0=&&push0; PUSH1=&&push1;   PUSH2=&&push2;   PUSH3=&&push3;
		PUSH4=&&push4; PUSH7=&&push7;   PUSH15=&&push15; PUSH16=&&push16;
      PUSH1D=&&push1d; PUSH1E=&&push1e;

		POP0=&&pop0;   POP1=&&pop1;     POP2=&&pop2;     POP3=&&pop3;
		POP4=&&pop4;   POP7=&&pop7; POP15=&&pop15;  POP1D=&&pop1d; POP1E=&&pop1e;

		ADDI0=&&addi0; ADDI1=&&addi1; ADD10=&&add10; MUL10=&&mul10;

		BLTI1=&&blti1;
		BEQI0=&&beqi0; BEQI1=&&beqi1; BEQI7=&&beqi7;
		BNEI0=&&bnei0; BNEI1=&&bnei1;
		BRTI0=&&brti0; BNTI0=&&bnti0;
		BRA=&&bra;

		J0=&&j0;       JAL0=&&jal0;   RET=&&ret;
		J2=&&j2;       JAL2=&&jal2;

		SYSI=&&sysi;   SYS0=&&sys0;   QUIT=&&quit;
		return;
	}

 /* pc, after a syscall, could have it's code object move.  Find
    all opcodes that could possibly cause a GC.  Better to NOT EVER USE
    LOCAL C VARS.  DUHHH. */
	/* Since registers are really void* and opcodes are u32 words, instruction
		addresses must be adjusted by 4 times.
		void **pc = (void**)code + (int)ip;
	*/

	/* Run machine code by jumping to first opcode (code=r13  ip=r1b). */
	ip = code + (int)ip*4;
	goto **(void**)ip;

	/* NOP */
	nop: OPDB("NOP");
	goto **(void**)(ip+=4);

	/* Load immediate value into register. */
	mvi0: OPDB("mvi0"); r0=*(Obj*)(ip+=4); goto **(void**)(ip+=4);
	mvi1: OPDB("mvi1"); r1=*(Obj*)(ip+=4); goto **(void**)(ip+=4);
	mvi2: OPDB("mvi2"); r2=*(Obj*)(ip+=4); goto **(void**)(ip+=4);
	mvi3: OPDB("mvi3"); r3=*(Obj*)(ip+=4); goto **(void**)(ip+=4);
	mvi4: OPDB("mvi4"); r4=*(Obj*)(ip+=4); goto **(void**)(ip+=4);
	mvi5: OPDB("mvi5"); r5=*(Obj*)(ip+=4); goto **(void**)(ip+=4);
	mvi6: OPDB("mvi6"); r6=*(Obj*)(ip+=4); goto **(void**)(ip+=4);
	mvi7: OPDB("mvi7"); r7=*(Obj*)(ip+=4); goto **(void**)(ip+=4);

	/* Copy regster to another. */
	mv01: OPDB("mv01"); r0=r1; goto **(void**)(ip+=4);
	mv02: OPDB("mv02"); r0=r2; goto **(void**)(ip+=4);
	mv03: OPDB("mv03"); r0=r3; goto **(void**)(ip+=4);
	mv04: OPDB("mv04"); r0=r4; goto **(void**)(ip+=4);
	mv07: OPDB("mv07"); r0=r7; goto **(void**)(ip+=4);
	mv016: OPDB("mv016"); r0=r16; goto **(void**)(ip+=4);
	mv01c: OPDB("mv01c"); r0=r1c; goto **(void**)(ip+=4);
	mv10: OPDB("mv10"); r1=r0; goto **(void**)(ip+=4);
	mv13: OPDB("mv13"); r1=r3; goto **(void**)(ip+=4);
	mv116: OPDB("mv116"); r1=r16; goto **(void**)(ip+=4);
	mv20: OPDB("mv20"); r2=r0; goto **(void**)(ip+=4);
	mv23: OPDB("mv23"); r2=r3; goto **(void**)(ip+=4);
	mv30: OPDB("mv30"); r3=r0; goto **(void**)(ip+=4);
	mv50: OPDB("mv50"); r5=r0; goto **(void**)(ip+=4);
	mv416: OPDB("mv416"); r4=r16; goto **(void**)(ip+=4);
	mv160: OPDB("mv160"); r16=r0; goto **(void**)(ip+=4);
	mv162: OPDB("mv162"); r16=r2; goto **(void**)(ip+=4);
	mv164: OPDB("mv164"); r16=r4; goto **(void**)(ip+=4);

	/* Load r2 <- *(r0 + immediate) */
	ldi00: OPDB("ldi00");
	r0=memVectorObject(r0, *(u32*)(ip+=4));//*((Obj*)r0 + *(u32*)(ip+=4));
	goto **(void**)(ip+=4);
	ldi02: OPDB("ldi02");
	r0=memVectorObject(r2, *(u32*)(ip+=4));//*((Obj*)r2 + *(u32*)(ip+=4));
	goto **(void**)(ip+=4);
	ldi016: OPDB("ldi016");
	r0=memVectorObject(r16, *(u32*)(ip+=4));//*((Obj*)r16 + *(u32*)(ip+=4));
	goto **(void**)(ip+=4);
	ldi11: OPDB("ldi11");
	r1=memVectorObject(r1, *(u32*)(ip+=4));//*((Obj*)r1 + *(u32*)(ip+=4));
	goto **(void**)(ip+=4);
	ldi116: OPDB("ldi116");
	r1=memVectorObject(r16, *(u32*)(ip+=4));//*((Obj*)r16 + *(u32*)(ip+=4));
	goto **(void**)(ip+=4);
	ldi20: OPDB("ldi20");
	r2=//memVectorObject(r0, *(u32*)(ip+=4));
		*((Obj*)r0 + *(u32*)(ip+=4));
	goto **(void**)(ip+=4);
	ldi22: OPDB("ldi22");
	r2=//memVectorObject(r2, *(u32*)(ip+=4));
		*((Obj*)r2 + *(u32*)(ip+=4));
	goto **(void**)(ip+=4);
	ldi40: OPDB("ldi40");
	r4=memVectorObject(r0, *(u32*)(ip+=4));//*((Obj*)r0 + *(u32*)(ip+=4));
	goto **(void**)(ip+=4);
	ldi50: OPDB("ldi50");
	r5=memVectorObject(r0, *(u32*)(ip+=4));//*((Obj*)r0 + *(u32*)(ip+=4));
	goto **(void**)(ip+=4);
	ldi160: OPDB("ldi160");
	r16=memVectorObject(r0, *(u32*)(ip+=4));//*((Obj*)r0 + *(u32*)(ip+=4));
	goto **(void**)(ip+=4);
	ldi1616: OPDB("ldi1616");
	r16=memVectorObject(r16, *(u32*)(ip+=4));//*((Obj*)r16 + *(u32*)(ip+=4));
	goto **(void**)(ip+=4);

	/* Load value in register's address plus register offset into register. */
	ld012: OPDB("ld012"); r0=*((Obj*)r1 + (u32)r2);  goto **(void**)(ip+=4);

	/* Store r0 -> *(r1 + immediate). */
	sti01:OPDB("sti01");
#if VALIDATE
		if (!(0 <= *(u32*)(ip+4) && *(u32*)(ip+4) < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode sti01 %d < %d", memObjectLength(r1), *(u32*)(ip+4));
#endif
		*((Obj*)r1 + *(u32*)(ip+=4))=r0; goto **(void**)(ip+=4);
	sti016:OPDB("sti016");
#if VALIDATE
		if (!(0 <= *(u32*)(ip+4) && *(u32*)(ip+4) < memObjectLength(r16))) fprintf (stderr, "[ERROR opcode sti016 %d < %d", memObjectLength(r16), *(u32*)(ip+4));
#endif
		*((Obj*)r16 + *(u32*)(ip+=4))=r0; goto **(void**)(ip+=4);
	sti20:OPDB("sti20");
#if VALIDATE
		if (!(0 <= *(u32*)(ip+4) && *(u32*)(ip+4) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti20 %d < %d", memObjectLength(r0), *(u32*)(ip+4));
#endif
		*((Obj*)r0 + *(u32*)(ip+=4))=r2; goto **(void**)(ip+=4);
	sti21:OPDB("sti21");
#if VALIDATE
		if (!(0 <= *(u32*)(ip+4) && *(u32*)(ip+4) < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode sti21 %d < %d", memObjectLength(r1), *(u32*)(ip+4));
#endif
		*((Obj*)r1 + *(u32*)(ip+=4))=r2; goto **(void**)(ip+=4);
	sti30:OPDB("sti30");
#if VALIDATE
		if (!(0 <= *(u32*)(ip+4) && *(u32*)(ip+4) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti30 %d < %d", memObjectLength(r0), *(u32*)(ip+4));
#endif
		*((Obj*)r0 + *(u32*)(ip+=4))=r3; goto **(void**)(ip+=4);
	sti40:OPDB("sti40");
#if VALIDATE
		if (!(0 <= *(u32*)(ip+4) && *(u32*)(ip+4) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti40 %d < %d", memObjectLength(r0), *(u32*)(ip+4));
#endif
		*((Obj*)r0 + *(u32*)(ip+=4))=r4; goto **(void**)(ip+=4);
	sti50:OPDB("sti50");
#if VALIDATE
		if (!((0 <= *(u32*)(ip+4)) && (*(u32*)(ip+4) < memObjectLength(r0)))) {
			fprintf (stderr, "[ERROR opcode sti50 %08x < %08x]\n", memObjectLength(r0), *(u32*)(ip+4));
			//vmPreGarbageCollect();
			//r0 = code;
			//vmDebugDump();
			//return;
			//vmPostGarbageCollect();
		}
#endif
		*((Obj*)r0 + *(u32*)(ip+=4))=r5; goto **(void**)(ip+=4);

	/* Store r0 -> *(r1 + r2). */
	st012: OPDB("st012");
#if VALIDATE
		if (!(0 <= r2 &&  (int)r2 < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode st012 %d < %d", memObjectLength(r1), r2);
#endif
		*((Obj*)r1 + (u32)r2) = r0;  goto **(void**)(ip+=4);
	st201: OPDB("st201"); *((Obj*)r0 + (u32)r1) = r2;  goto **(void**)(ip+=4);

	/* Push register using local stack pointer. */
	push0: OPDB("push0");  memStackPush(stack, r0);  goto **(void**)(ip+=4);
	push1: OPDB("push1");  memStackPush(stack, r1);  goto **(void**)(ip+=4);
	push2: OPDB("push2");  memStackPush(stack, r2);  goto **(void**)(ip+=4);
	push3: OPDB("push3");  memStackPush(stack, r3);  goto **(void**)(ip+=4);
	push4: OPDB("push4");  memStackPush(stack, r4);  goto **(void**)(ip+=4);
	push7: OPDB("push7");  memStackPush(stack, r7);  goto **(void**)(ip+=4);
	push15:OPDB("push15"); memStackPush(stack, r15); goto **(void**)(ip+=4);
	push16:OPDB("push16"); memStackPush(stack, r16); goto **(void**)(ip+=4);
	push1d:OPDB("push1d"); memStackPush(stack, r1d); goto **(void**)(ip+=4);
	push1e:OPDB("push1e"); memStackPush(stack, r1e); goto **(void**)(ip+=4);


	/* Pop into a register. */
	pop0: OPDB("pop0");   r0 = memStackPop(stack);  goto **(void**)(ip+=4);
	pop1: OPDB("pop1");   r1 = memStackPop(stack);  goto **(void**)(ip+=4);
	pop2: OPDB("pop2");   r2 = memStackPop(stack);  goto **(void**)(ip+=4);
	pop3: OPDB("pop3");   r3 = memStackPop(stack);  goto **(void**)(ip+=4);
	pop4: OPDB("pop4");   r4 = memStackPop(stack);  goto **(void**)(ip+=4);
	pop7: OPDB("pop7");   r7 = memStackPop(stack);  goto **(void**)(ip+=4);
	pop15:OPDB("pop15"); r15 = memStackPop(stack);  goto **(void**)(ip+=4);
	pop1d:OPDB("pop1d"); r1d = memStackPop(stack);  goto **(void**)(ip+=4);
	pop1e:OPDB("pop1e"); r1e = memStackPop(stack);  goto **(void**)(ip+=4);

	/* Add immediate to r0. */
	addi0: OPDB("addi0"); r0 += *(s32*)(ip+=4); goto **(void**)(ip+=4);
	addi1: OPDB("addi1"); r1 += *(s32*)(ip+=4); goto **(void**)(ip+=4);

	/* Mutate object r1 with (object r1 + object r0). */
	add10: OPDB("add10"); *(s32*)r1 += *(s32*)r0; goto **(void**)(ip+=4);
	mul10: OPDB("mul10"); *(s32*)r1 *= *(s32*)r0; goto **(void**)(ip+=4);

	blti1: OPDB("blti1");
	if (r1<*(void**)(ip+=4)) {
		ip+=4;
		ip += *(int*)ip;
		ip+=4;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip+=8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	/* Jump to immediate2 if r0 equal to immediate 1.  Will these ever be
		emitted?  (if (= x 0) blah foo) =>
		MVI $0 <BININD x>
		LDI $0 [$0 + 0]
		BNQI $0 0 blah:
		foo:
		BRA end:
		blah:
		end:
	*/
	beqi0: OPDB("beqi0");
	if (r0==*(void**)(ip+=4)) {
		ip+=4;
		ip += *(int*)ip;
		ip+=4;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip+=8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	beqi1: OPDB("beqi1");
	if (r1 == *(void**)(ip+=4)) {
		ip += 4;
		ip += *(int*)ip;
		ip+=4;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip+=8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	beqi7: OPDB("beqi7");
	if (r7 == *(void**)(ip+=4)) {
		ip += 4;
		ip += *(int*)ip;
		ip+=4;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip+=8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	/* Jump to immediate2 if r0 not equal to immediate 1. */
	bnei0: OPDB("bnei0");
	if (r0 != *(void**)(ip+=4)) {
		ip += 4;
		ip += *(int*)ip;
		ip+=4;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip+=8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	/* Jump to immediate2 if r1 not equal to immediate 1. */
	bnei1: OPDB("bnei0");
	if (r1!=*(void**)(ip+=4)) {
		ip += 4;
		ip += *(int*)ip;
		ip+=4;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip+=8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	/* Jump to immediate 2 if r0's type equals to immediate 1. */
	brti0: OPDB("brti0");
	if (((u32)r0>0xfffff) && (memObjectType(r0))==*(u32*)(ip+=4)) {
		ip += 4;
		ip += *(int*)ip;
		ip+=4;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip+=8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	/* Jump to immediate 2 if r0's type not equal to immediate 1. */
	bnti0: OPDB("bnti0");
	if (((u32)r0<0x100000) || (memObjectType(r0))!=*(u32*)(ip+=4)) {
		ip += 4;
		ip += *(int*)ip;
		ip+=4;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip+=8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	/* Branch always. */
	bra: OPDB("bra");
	ip += 4;
	ip += *(u32*)ip;
	ip+=4;
	if (interrupt) vmInterruptHandler();
	goto **(void**)(ip);

	/* Jump to first instruction in block in r0. */
	j0: OPDB("j0");
	ip = code = r0;
	if (interrupt) vmInterruptHandler();
	goto **(void**)ip;

	j2: OPDB("j2");
	ip = code = r2;
	if (interrupt) vmInterruptHandler();
	goto **(void**)ip;

	/* Link block/offset then jump to first instruction in block in acc. */
	jal0: OPDB("jal0");
	/* Save the pc and program. */
	retip = (Obj)(ip - code);
	retcode = code;
	retenv = env;
	ip = code = r0;
	if (interrupt) vmInterruptHandler();
	goto **(void**)ip;

	/* Link block/offset then jump to first instruction in block in acc. */
	jal2: OPDB("jal2");
	/* Save the pc and program. */
	retip = (Obj)(ip - code);
	retcode = code;
	retenv = env;
	ip = code = r2;
	if (interrupt) vmInterruptHandler();
	goto **(void**)ip;

	/* Ret to caller. */
	ret: OPDB("ret");
	env = retenv;
	code = retcode;
	ip = code + (int)retip;
	ip+=4;
	if (interrupt) vmInterruptHandler();
	goto **(void**)(ip);

	/* Immediate syscall.  Like 'sys' only C address is immediate value.  Set
		the ip to next instruction right first so that the syscall runs with
		the IP at the next instruction. */ 
	sysi: OPDB("sysi");
	ip+=8;
	vmPreGarbageCollect();
	(*(void(**)(void))((Obj*)code+(int)ip-1))();
	vmPostGarbageCollect();
	if (interrupt) vmInterruptHandler();
	goto **(void**)ip;

	/* System call.  Really just a C function call, address in accumulator. 
		imediate field is passed to C function.  See sysi comment for other
		information. */
	sys0: OPDB("sys0");
	ip+=4;
	vmPreGarbageCollect();
	(*(void(*)(void))r0)();
	vmPostGarbageCollect();
	if (interrupt) vmInterruptHandler();
	goto **(void**)ip;

	/* Halt virtual machine.  Return to OS?*/
	quit: OPDB("quit"); return;
}

/* Starts virtual machine using the program code (r1c) starting at instruction
   offset in immediate ip (r1b).
*/
void vmRun (void) {
	DB("VM -->vmRun <= %08x %04x", code, ip);
	vmVm (RUN);
	DB("VM <--vmRun");
}



void vmInitialize (fp intHandler, fp preGC, fp postGC, fp1 objDumper) {
 static int shouldInitialize=1;
	if (shouldInitialize) {
		shouldInitialize=0;
		vmVm(INIT); // Initialize the opcodes (C jump addresses).
		if (intHandler) vmCallerInterruptHandler = intHandler;
		if (preGC) vmCallerPreGarbageCollect = preGC;
		if (postGC) vmCallerPostGarbageCollect = postGC;
		if (objDumper) vmObjectDumper = objDumper;
		memInitialize(&vmPreGarbageCollect, &vmPostGarbageCollect);
		memNewStack(); stack = r0;
		memNewStack(); asmstack = r0;
		/* Start the interrupt schedule timer. */
		signal(SIGALRM, vmSigAlarmHandler);
		vmSigAlarmReset();
	}
}

void vmDebugDump (void) {
 Obj *ip=r0;
	memDebugDumpHeapHeaders();
	while (ip < ((Obj*)r0 + memObjectLength(r0))) {
		printf ("\n%04x ", ip-(Obj*)r0);
		if (*ip == NOP)      {printf("nop");}
		else if (*ip==MVI0)  {printf("mvi$0 "); vmObjectDumper(*++ip);}
		else if (*ip==MVI1)  {printf("mvi$1 "); vmObjectDumper(*++ip);}
		else if (*ip==MVI2)  {printf("mvi$2 "); vmObjectDumper(*++ip);}
		else if (*ip==MVI3)  {printf("mvi$3 "); vmObjectDumper(*++ip);}
		else if (*ip==MVI4)  {printf("mvi$4 "); vmObjectDumper(*++ip);}
		else if (*ip==MVI5)  {printf("mvi$5 "); vmObjectDumper(*++ip);}
		else if (*ip==MVI6)  {printf("mvi$6 "); vmObjectDumper(*++ip);}
		else if (*ip==MVI7)  {printf("mvi$7 "); vmObjectDumper(*++ip);}
		else if (*ip==MV01)  {printf("mv$0$1");}
		else if (*ip==MV02)  {printf("mv$0$2");}
		else if (*ip==MV03)  {printf("mv$0$3");}
		else if (*ip==MV04)  {printf("mv$0$4");}
		else if (*ip==MV07)  {printf("mv$0$7");}
		else if (*ip==MV016) {printf("mv$0$16");}
		else if (*ip==MV01C) {printf("mv$0$1c");}
		else if (*ip==MV10)  {printf("mv$1$0");}
		else if (*ip==MV13)  {printf("mv$1$3");}
		else if (*ip==MV116) {printf("mv$1$16");}
		else if (*ip==MV20)  {printf("mv$2$0");}
		else if (*ip==MV23)  {printf("mv$2$3");}
		else if (*ip==MV30)  {printf("mv$3$0");}
		else if (*ip==MV50)  {printf("mv$5$0");}
		else if (*ip==MV416) {printf("mv$4$16");}
		else if (*ip==MV160) {printf("mv$16$0");}
		else if (*ip==MV162) {printf("mv$16$2");}
		else if (*ip==MV164) {printf("mv$16$4");}
		else if (*ip==LDI00) {printf("ldi$0$0 "); vmObjectDumper(*++ip);}
		else if (*ip==LDI02) {printf("ldi$0$2 "); vmObjectDumper(*++ip);}
		else if (*ip==LDI016){printf("ldi$0$16 "); vmObjectDumper(*++ip);}
		else if (*ip==LDI11) {printf("ldi$1$1 "); vmObjectDumper(*++ip);}
		else if (*ip==LDI116){printf("ldi$1$16 "); vmObjectDumper(*++ip);}
		else if (*ip==LDI20) {printf("ldi$2$0 "); vmObjectDumper(*++ip);}
		else if (*ip==LDI22) {printf("ldi$2$2 "); vmObjectDumper(*++ip);}
		else if (*ip==LDI40) {printf("ldi$4$0 "); vmObjectDumper(*++ip);}
		else if (*ip==LDI50) {printf("ldi$5$0 "); vmObjectDumper(*++ip);}
		else if (*ip==LDI160){printf("ldi$16$0 "); vmObjectDumper(*++ip);}
		else if (*ip==LDI1616){printf("ldi$16$16 "); vmObjectDumper(*++ip);}
		else if (*ip==LD012) {printf("ld0$1$2");}
		else if (*ip==STI01) {printf("sti$0$1 "); vmObjectDumper(*++ip);}
		else if (*ip==STI016){printf("sti$0$16 "); vmObjectDumper(*++ip);}
		else if (*ip==STI20) {printf("sti$2$0 "); vmObjectDumper(*++ip);}
		else if (*ip==STI21) {printf("sti$2$1 "); vmObjectDumper(*++ip);}
		else if (*ip==STI30) {printf("sti$3$0 "); vmObjectDumper(*++ip);}
		else if (*ip==STI40) {printf("sti$4$0 "); vmObjectDumper(*++ip);}
		else if (*ip==STI50) {printf("sti$5$0 "); vmObjectDumper(*++ip);}
		else if (*ip==ST012) {printf("st$0$1$2");}
		else if (*ip==ST201) {printf("st$2$0$1");}
		else if (*ip==PUSH0) {printf("push$0");}
		else if (*ip==PUSH1) {printf("push$1");}
		else if (*ip==PUSH2) {printf("push$2");}
		else if (*ip==PUSH3) {printf("push$3");}
		else if (*ip==PUSH4) {printf("push$4");}
		else if (*ip==PUSH7) {printf("push$7");}
		else if (*ip==PUSH15){printf("push$15");}
		else if (*ip==PUSH16){printf("push$16");}
		else if (*ip==PUSH1D){printf("push$1d");}
		else if (*ip==PUSH1E){printf("push$1e");}
		else if (*ip==POP0)  {printf("pop$0");}
		else if (*ip==POP1)  {printf("pop$1");}
		else if (*ip==POP2)  {printf("pop$2");}
		else if (*ip==POP3)  {printf("pop$3");}
		else if (*ip==POP4)  {printf("pop$4");}
		else if (*ip==POP7)  {printf("pop$7");}
		else if (*ip==POP15) {printf("pop$15");}
		else if (*ip==POP1D) {printf("pop$1d");}
		else if (*ip==POP1E) {printf("pop$1e");}
		else if (*ip==ADDI0) {printf("addi$0 %d", *(ip+1)); ip++; }
		else if (*ip==ADDI1) {printf("addi$1 %d", *(ip+1)); ip++; }
		else if (*ip==ADD10) {printf("add$1$0"); }
		else if (*ip==MUL10) {printf("mul$1$0"); }
		else if (*ip==BLTI1) {printf("blti$1 %x %04x",
									 *(ip+1), 3+ip-(Obj*)r0+(int)*(ip+2)/4); ip+=2;}
		else if (*ip==BEQI0) {printf("beqi$0 %x %04x",
									 *(ip+1), 3+ip-(Obj*)r0+(int)*(ip+2)/4); ip+=2;}
		else if (*ip==BEQI1) {printf("beqi$1 %x %04x",
									 *(ip+1), 3+ip-(Obj*)r0+(int)*(ip+2)/4); ip+=2;}
		else if (*ip==BEQI7) {printf("beqi$7 %x %04x",
									 *(ip+1), 3+ip-(Obj*)r0+(int)*(ip+2)/4); ip+=2;}
		else if (*ip==BNEI0) {printf("bnei$0 %x %04x",
									 *(ip+1), 3+ip-(Obj*)r0+(int)*(ip+2)/4); ip+=2;}
		else if (*ip==BNEI1) {printf("bnei$1 %x %04x",
									 *(ip+1), 3+ip-(Obj*)r0+(int)*(ip+2)/4); ip+=2;}
		else if (*ip==BRTI0) {printf ("brti$0 %x %04x",
									 *(ip+1), 3+ip-(Obj*)r0+(int)*(ip+2)/4); ip+=2;}
		else if (*ip==BNTI0) {printf ("bnti$0 %08x %04x",
									 *(ip+1), 3+ip-(Obj*)r0+(int)*(ip+2)/4); ip+=2;}
		else if (*ip==BRA)   {printf ("bra %04x",
									 2+ip-(Obj*)r0+(int)*(ip+1)/4); ip++;}
		else if (*ip==J0)    {printf ("j$0");}
		else if (*ip==J2)    {printf ("j$2");}
		else if (*ip==JAL0)  {printf ("jal$0");}
		else if (*ip==JAL2)  {printf ("jal$2");}
		else if (*ip==RET)   {printf ("ret");}
		else if (*ip==SYSI)  {printf ("sysi "); vmObjectDumper(*++ip);}
		else if (*ip==SYS0)  {printf ("sys$0");}
		else if (*ip==QUIT)  {printf ("quit");}
		else {
			printf ("??? %08x ", *ip);
			vmObjectDumper(*ip);
		}
		ip++;
		fflush(stdout);
	}
}


/* Debugging: Syscalls.
*/
static void displayInteger (void) { printf ("%d", r1); }
static void displayString (void) { printf ("%s", r1); }

/* Debuggin: Registers eventually containing runable code.
*/
#define sub2 r10
#define sub r11

int vmmain (void) {
 int len;
	setbuf(stdout, NULL);
	vmInitialize(0, 0, 0, 0);

	Obj helloWorld[] = {
		PUSH1,
		MVI1, "Hello World!\n",
		MVI0, displayString,
		SYS0,
		POP1,
		RET};
   memNewVector(TCODE, (len=sizeof(helloWorld))/4);
	memcpy(r0, helloWorld, len);
	vmDebugDump();
	sub2 = r0;


	Obj printNumbers[] = {
		SYSI, displayInteger,
		BEQI1, (Obj)0, (Obj)(2*4),  /* Return if r1==0 */
		BRA, (Obj)(1*4),
		RET,
	
		PUSH1D,              /* Save return address */
		PUSH1E,
	
		MVI0, sub2,
		JAL0,
		MV01,               /* r1-- */
		ADDI0, (Obj)-1,
		MV10,
		MVI0, &sub,    /* Call another function. */
		LDI00, 0,
		JAL0,
	
		POP1E,               /* Restore return address */
		POP1D,
		RET};
   memNewVector(TCODE, (len=sizeof(printNumbers))/4);
	memcpy(r0, printNumbers, len);
	vmDebugDump();
	sub = r0;


	Obj mainCode[] = {
		MVI1, (Obj)11,
		MVI0, sub,
		JAL0,
	
		MVI2,  0,
		MV02,
		MV10,
		SYSI, displayInteger, /* print r2 */
		MVI1, "\r",
		SYSI, displayString,
		MV02,
		ADDI0,  (Obj)1,
		MV20,
		BNEI0,  (Obj)0x8000,  (Obj)(-15*4),
	
		MVI1, "\n",
		SYSI, displayString,
		BRA,  (Obj)(-28*4)};
   memNewVector(TCODE, (len=sizeof(mainCode))/4);
	memcpy(r0, mainCode, len);
	vmDebugDump();
	code=r0;

	memDebugDumpHeapStructures();
	memGarbageCollect();
	ip=0;
	vmRun();
	return 0;
}
