#define DEBUG 0
#define VALIDATE 1
#define DB_MODULE "VM "
#include "debug.h"
#include <stdio.h>
#include <string.h>
#include <signal.h> /* for signal() */
#include <unistd.h> /* For ualarm() */
#include <fcntl.h>
#include "vm.h"

#include <unistd.h> /* For write(). */

/* Debg dump the current opcode
*/
#define OPDB(s,...) DBE fprintf(stderr,"\n"OBJ":"HEX" " s, code, ((Obj*)ip-(Obj*)code), ##__VA_ARGS__)
//#define OPDB(s) DBE fprintf(stderr,"\n"OBJ":"HEX" " s, code+8*((Obj*)ip-(Obj*)code), ((Obj*)ip-(Obj*)code))


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

Func vmCallerPreGarbageCollect = 0,
     vmCallerPostGarbageCollect = 0;

int vmGCCount=0;

void vmPreGarbageCollect (void) {
	DB("::vmPreGarbageCollect  ip "OBJ"  code"OBJ, ip, code);
	if (vmGCCount++ == 0) {
		/* These are passed from obj.c */
		if (vmCallerPreGarbageCollect) vmCallerPreGarbageCollect();
		/* Only convert ip to offset if it's a pointer (big number larger pointing
			into the code object).  Multiply by eight to force opcode offset (not
			byte). */
		if (ip >= code) ip = (Obj)((ip - code)/8);
		DB("  --vmPreGarbageCollect  ip "OBJ"  code"OBJ, ip, code);
	}
}
void vmPostGarbageCollect (void) {
	DB("::vmPostGarbageCollect  ip "OBJ"  code"OBJ, ip, code);
	if (vmGCCount-- == 1) {
		if (vmCallerPostGarbageCollect) vmCallerPostGarbageCollect();
		/* Only convert ip to pointer if it's an offset (low number below the
			code's address). */
		if (ip < code) ip = code + ((Int)ip)*8;
		DB("  --vmPostGarbageCollect  ip "OBJ"  code"OBJ, ip, code);
	}
}


/* This causes the machine to make a call to the interrupt handler.
*/
Int interrupt=0;

void vmSigAlarmHandler (int sig) {
	interrupt=1;
}

void vmSigAlarmReset (void) {
	ualarm(10*1000,0); /* 10 miliseconds (100 tics/sec)*/
}

Func vmCallerScheduler = NULL;

void vmInterruptHandler (void) {
	DB("VM -->vmInterruptHandler <= %08x %08x", code, ip);
	/* Calling these pre/post functions for now until I abstract better the
		concept of massaging register values into garbage-collector-friendly
		values. */
	vmPreGarbageCollect();

	if (vmCallerScheduler) vmCallerScheduler();

	vmPostGarbageCollect();
	interrupt=0;
	vmSigAlarmReset();
	DB("VM <--vmInterruptHandler => %08x %08x", code, ip);
}


/* Virtual Machine
*/

#define INIT 0
#define RUN  1
void vmVm (Int cmd) {
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
		MV61=&&mv61;   MV72=&&mv72;

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

		POP0=&&pop0;    POP1=&&pop1;   POP2=&&pop2;   POP3=&&pop3;  POP4=&&pop4;  POP7=&&pop7;
		POP15=&&pop15;  POP1D=&&pop1d; POP1E=&&pop1e;

		ADDI0=&&addi0; ADDI1=&&addi1; ADD10=&&add10; MUL10=&&mul10;

		BLTI1=&&blti1;
		BEQI0=&&beqi0; BEQI1=&&beqi1; BEQI7=&&beqi7;
		BNEI0=&&bnei0; BNEI1=&&bnei1;
		BRTI0=&&brti0; BNTI0=&&bnti0;
		BRA=&&bra;

		J0=&&j0;      J2=&&j2;
		JAL0=&&jal0;  JAL2=&&jal2;
		RET=&&ret;

		SYSI=&&sysi;   SYS0=&&sys0;   QUIT=&&quit;

		//-----------
		memObjStringSet(NOP); memObjStringSet(MVI0); memObjStringSet(MVI1); memObjStringSet(MVI2); memObjStringSet(MVI3); memObjStringSet(MVI4); memObjStringSet(MVI5); memObjStringSet(MVI6); memObjStringSet(MVI7); memObjStringSet(MV01); memObjStringSet(MV02); memObjStringSet(MV03); memObjStringSet(MV04); memObjStringSet(MV07); memObjStringSet(MV016); memObjStringSet(MV01C); memObjStringSet(MV10); memObjStringSet(MV13); memObjStringSet(MV116); memObjStringSet(MV20); memObjStringSet(MV23); memObjStringSet(MV30); memObjStringSet(MV416); memObjStringSet(MV50); memObjStringSet(MV160); memObjStringSet(MV162); memObjStringSet(MV164); memObjStringSet(MV61); memObjStringSet(MV72); memObjStringSet(LDI00); memObjStringSet(LDI02); memObjStringSet(LDI016); memObjStringSet(LDI11); memObjStringSet(LDI116); memObjStringSet(LDI20); memObjStringSet(LDI22); memObjStringSet(LDI40); memObjStringSet(LDI50); memObjStringSet(LDI160); memObjStringSet(LDI1616); memObjStringSet(LD012); memObjStringSet(STI01); memObjStringSet(STI016); memObjStringSet(STI20); memObjStringSet(STI21); memObjStringSet(STI30); memObjStringSet(STI40); memObjStringSet(STI50); memObjStringSet(ST012); memObjStringSet(ST201); memObjStringSet(PUSH0); memObjStringSet(PUSH1); memObjStringSet(PUSH2); memObjStringSet(PUSH3); memObjStringSet(PUSH4); memObjStringSet(PUSH7); memObjStringSet(PUSH15); memObjStringSet(PUSH16); memObjStringSet(PUSH1D); memObjStringSet(PUSH1E); memObjStringSet(POP0); memObjStringSet(POP1); memObjStringSet(POP2); memObjStringSet(POP3); memObjStringSet(POP4); memObjStringSet(POP7); memObjStringSet(POP15); memObjStringSet(POP1D); memObjStringSet(POP1E); memObjStringSet(ADDI0); memObjStringSet(ADDI1); memObjStringSet(ADD10); memObjStringSet(MUL10); memObjStringSet(BLTI1); memObjStringSet(BEQI0); memObjStringSet(BEQI1); memObjStringSet(BEQI7); memObjStringSet(BNEI0); memObjStringSet(BNEI1); memObjStringSet(BRTI0); memObjStringSet(BNTI0); memObjStringSet(BRA); memObjStringSet(J0); memObjStringSet(J2); memObjStringSet(JAL0); memObjStringSet(JAL2); memObjStringSet(RET); memObjStringSet(SYSI); memObjStringSet(SYS0); memObjStringSet(QUIT);
		//-----------

		return;
	}

	/* pc, after a syscall, could have it's code object move.  Find
		all opcodes that could possibly cause a GC.  Better to NOT EVER USE
		LOCAL C VARS.  DUHHH.
	*/

	/* Since registers are really void* and opcodes are u64 words, instruction
		addresses must be adjusted by 8 times.
		void **pc = (void**)code + (int)ip;
	*/

	/* Run machine code by jumping to first opcode (code=r13  ip=r1b). */
	ip = code + (Int)ip*8;
	DB("Starting VM:  ip="HEX"  code="HEX"  **ip="HEX, ip, code, *(void**)ip);
	goto **(void**)ip;

	/* NOP */
	nop: OPDB("NOP");
	goto **(void**)(ip+=8);

	/* Load immediate value into register. */
	mvi0: OPDB("mvi0"); r0=*(Obj*)(ip+=8); goto **(void**)(ip+=8);
	mvi1: OPDB("mvi1"); r1=*(Obj*)(ip+=8); goto **(void**)(ip+=8);
	mvi2: OPDB("mvi2"); r2=*(Obj*)(ip+=8); goto **(void**)(ip+=8);
	mvi3: OPDB("mvi3"); r3=*(Obj*)(ip+=8); goto **(void**)(ip+=8);
	mvi4: OPDB("mvi4"); r4=*(Obj*)(ip+=8); goto **(void**)(ip+=8);
	mvi5: OPDB("mvi5"); r5=*(Obj*)(ip+=8); goto **(void**)(ip+=8);
	mvi6: OPDB("mvi6"); r6=*(Obj*)(ip+=8); goto **(void**)(ip+=8);
	mvi7: OPDB("mvi7"); r7=*(Obj*)(ip+=8); goto **(void**)(ip+=8);

	/* Copy regster to another. */
	mv01: OPDB("mv01"); r0=r1; goto **(void**)(ip+=8);
	mv02: OPDB("mv02"); r0=r2; goto **(void**)(ip+=8);
	mv03: OPDB("mv03"); r0=r3; goto **(void**)(ip+=8);
	mv04: OPDB("mv04"); r0=r4; goto **(void**)(ip+=8);
	mv07: OPDB("mv07"); r0=r7; goto **(void**)(ip+=8);
	mv016: OPDB("mv016"); r0=r16; goto **(void**)(ip+=8);
	mv01c: OPDB("mv01c"); r0=r1c; goto **(void**)(ip+=8);
	mv10: OPDB("mv10"); r1=r0; goto **(void**)(ip+=8);
	mv13: OPDB("mv13"); r1=r3; goto **(void**)(ip+=8);
	mv116: OPDB("mv116"); r1=r16; goto **(void**)(ip+=8);
	mv20: OPDB("mv20"); r2=r0; goto **(void**)(ip+=8);
	mv23: OPDB("mv23"); r2=r3; goto **(void**)(ip+=8);
	mv30: OPDB("mv30"); r3=r0; goto **(void**)(ip+=8);
	mv50: OPDB("mv50"); r5=r0; goto **(void**)(ip+=8);
	mv416: OPDB("mv416"); r4=r16; goto **(void**)(ip+=8);
	mv160: OPDB("mv160"); r16=r0; goto **(void**)(ip+=8);
	mv162: OPDB("mv162"); r16=r2; goto **(void**)(ip+=8);
	mv164: OPDB("mv164"); r16=r4; goto **(void**)(ip+=8);
	mv61: OPDB("mv61"); r6=r1; goto **(void**)(ip+=8);
	mv72: OPDB("mv72"); r7=r2; goto **(void**)(ip+=8);

	/* Load r2 <- *(r0 + immediate) */
	ldi00: OPDB("ldi00");
	r0=memVectorObject(r0, *(Num*)(ip+=8));//*((Obj*)r0 + *(Num*)(ip+=8));
	goto **(void**)(ip+=8);
	ldi02: OPDB("ldi02");
	r0=memVectorObject(r2, *(Num*)(ip+=8));//*((Obj*)r2 + *(Num*)(ip+=8));
	goto **(void**)(ip+=8);
	ldi016: OPDB("ldi016");
	r0=memVectorObject(r16, *(Num*)(ip+=8));//*((Obj*)r16 + *(Num*)(ip+=8));
	goto **(void**)(ip+=8);
	ldi11: OPDB("ldi11");
	r1=memVectorObject(r1, *(Num*)(ip+=8));//*((Obj*)r1 + *(Num*)(ip+=8));
	goto **(void**)(ip+=8);
	ldi116: OPDB("ldi116");
	r1=memVectorObject(r16, *(Num*)(ip+=8));//*((Obj*)r16 + *(Num*)(ip+=8));
	goto **(void**)(ip+=8);
	ldi20: OPDB("ldi20");
	r2=//memVectorObject(r0, *(Num*)(ip+=8));
		*((Obj*)r0 + *(Num*)(ip+=8));
	goto **(void**)(ip+=8);
	ldi22: OPDB("ldi22");
	r2=//memVectorObject(r2, *(Num*)(ip+=8));
		*((Obj*)r2 + *(Num*)(ip+=8));
	goto **(void**)(ip+=8);
	ldi40: OPDB("ldi40");
	r4=memVectorObject(r0, *(Num*)(ip+=8));//*((Obj*)r0 + *(Num*)(ip+=8));
	goto **(void**)(ip+=8);
	ldi50: OPDB("ldi50");
	r5=memVectorObject(r0, *(Num*)(ip+=8));//*((Obj*)r0 + *(Num*)(ip+=8));
	goto **(void**)(ip+=8);
	ldi160: OPDB("ldi160");
	r16=memVectorObject(r0, *(Num*)(ip+=8));//*((Obj*)r0 + *(Num*)(ip+=8));
	goto **(void**)(ip+=8);
	ldi1616: OPDB("ldi1616");
	r16=memVectorObject(r16, *(Num*)(ip+=8));//*((Obj*)r16 + *(Num*)(ip+=8));
	goto **(void**)(ip+=8);

	/* Load value in register's address plus register offset into register. */
	ld012: OPDB("ld012"); r0=*((Obj*)r1 + (Num)r2);  goto **(void**)(ip+=8);

	/* Store r0 -> *(r1 + immediate). */
	sti01:OPDB("sti01");
#if VALIDATE
		if (!(0 <= *(Num*)(ip+8) && *(Num*)(ip+8) < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode sti01 %d < %d", memObjectLength(r1), *(Num*)(ip+8));
#endif
		*((Obj*)r1 + *(Num*)(ip+=8))=r0; goto **(void**)(ip+=8);
	sti016:OPDB("sti016");
#if VALIDATE
		if (!(0 <= *(Num*)(ip+8) && *(Num*)(ip+8) < memObjectLength(r16))) fprintf (stderr, "[ERROR opcode sti016 %d < %d", memObjectLength(r16), *(Num*)(ip+8));
#endif
		*((Obj*)r16 + *(Num*)(ip+=8))=r0; goto **(void**)(ip+=8);
	sti20:OPDB("sti20");
#if VALIDATE
		if (!(0 <= *(Num*)(ip+8) && *(Num*)(ip+8) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti20 %d < %d", memObjectLength(r0), *(Num*)(ip+8));
#endif
		*((Obj*)r0 + *(Num*)(ip+=8))=r2; goto **(void**)(ip+=8);
	sti21:OPDB("sti21");
#if VALIDATE
		if (!(0 <= *(Num*)(ip+8) && *(Num*)(ip+8) < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode sti21 %d < %d", memObjectLength(r1), *(Num*)(ip+8));
#endif
		*((Obj*)r1 + *(Num*)(ip+=8))=r2; goto **(void**)(ip+=8);
	sti30:OPDB("sti30");
#if VALIDATE
		if (!(0 <= *(Num*)(ip+8) && *(Num*)(ip+8) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti30 %d < %d", memObjectLength(r0), *(Num*)(ip+8));
#endif
		*((Obj*)r0 + *(Num*)(ip+=8))=r3; goto **(void**)(ip+=8);
	sti40:OPDB("sti40");
#if VALIDATE
		if (!(0 <= *(Num*)(ip+8) && *(Num*)(ip+8) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti40 %d < %d", memObjectLength(r0), *(Num*)(ip+8));
#endif
		*((Obj*)r0 + *(Num*)(ip+=8))=r4; goto **(void**)(ip+=8);
	sti50:OPDB("sti50");
#if VALIDATE
		if (!((0 <= *(Num*)(ip+8)) && (*(Num*)(ip+8) < memObjectLength(r0)))) {
			fprintf (stderr, "[ERROR opcode sti50 %08x < %08x]\n", memObjectLength(r0), *(Num*)(ip+8));
			//vmPreGarbageCollect();
			//r0 = code;
			//vmDebugDump();
			//return;
			//vmPostGarbageCollect();
		}
#endif
		*((Obj*)r0 + *(Num*)(ip+=8))=r5; goto **(void**)(ip+=8);

	/* Store r0 -> *(r1 + r2). */
	st012: OPDB("st012");
#if VALIDATE
		if (!(0 <= r2 &&  (Int)r2 < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode st012 %d < %d", memObjectLength(r1), r2);
#endif
		*((Obj*)r1 + (Num)r2) = r0;  goto **(void**)(ip+=8);
	st201: OPDB("st201"); *((Obj*)r0 + (Num)r1) = r2;  goto **(void**)(ip+=8);

	/* Push register using local stack pointer. */
	push0: OPDB("push0");  memStackPush(stack, r0);  goto **(void**)(ip+=8);
	push1: OPDB("push1");  memStackPush(stack, r1);  goto **(void**)(ip+=8);
	push2: OPDB("push2");  memStackPush(stack, r2);  goto **(void**)(ip+=8);
	push3: OPDB("push3");  memStackPush(stack, r3);  goto **(void**)(ip+=8);
	push4: OPDB("push4");  memStackPush(stack, r4);  goto **(void**)(ip+=8);
	push7: OPDB("push7");  memStackPush(stack, r7);  goto **(void**)(ip+=8);
	push15:OPDB("push15"); memStackPush(stack, r15); goto **(void**)(ip+=8);
	push16:OPDB("push16"); memStackPush(stack, r16); goto **(void**)(ip+=8);
	push1d:OPDB("push1d"); memStackPush(stack, r1d); goto **(void**)(ip+=8);
	push1e:OPDB("push1e"); memStackPush(stack, r1e); goto **(void**)(ip+=8);


	/* Pop into a register. */
	pop0: OPDB("pop0");   r0 = memStackPop(stack);  goto **(void**)(ip+=8);
	pop1: OPDB("pop1");   r1 = memStackPop(stack);  goto **(void**)(ip+=8);
	pop2: OPDB("pop2");   r2 = memStackPop(stack);  goto **(void**)(ip+=8);
	pop3: OPDB("pop3");   r3 = memStackPop(stack);  goto **(void**)(ip+=8);
	pop4: OPDB("pop4");   r4 = memStackPop(stack);  goto **(void**)(ip+=8);
	pop7: OPDB("pop7");   r7 = memStackPop(stack);  goto **(void**)(ip+=8);
	pop15:OPDB("pop15"); r15 = memStackPop(stack);  goto **(void**)(ip+=8);
	pop1d:OPDB("pop1d"); r1d = memStackPop(stack);  goto **(void**)(ip+=8);
	pop1e:OPDB("pop1e"); r1e = memStackPop(stack);  goto **(void**)(ip+=8);

	/* Add immediate to r0. */
	addi0: OPDB("addi0"); r0 += *(Int*)(ip+=8); goto **(void**)(ip+=8);
	addi1: OPDB("addi1"); r1 += *(Int*)(ip+=8); goto **(void**)(ip+=8);

	/* Mutate object r1 with (object r1 + object r0). */
	add10: OPDB("add10"); *(Int*)r1 += *(Int*)r0; goto **(void**)(ip+=8);
	mul10: OPDB("mul10"); *(Int*)r1 *= *(Int*)r0; goto **(void**)(ip+=8);

	blti1: OPDB("blti1");
	if (r1<*(void**)(ip+=8)) {
		ip += 8;
		ip += *(Int*)ip;
		ip += 8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip += 16;
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
	if (r0==*(void**)(ip+=8)) {
		ip += 8;
		ip += *(Int*)ip;
		ip += 8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip += 2*8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	beqi1: OPDB("beqi1");
	if (r1 == *(Obj*)(ip+=8)) {
		ip += 8;
		ip += *(Int*)ip;
		ip += 8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip += 2*8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	beqi7: OPDB("beqi7");
	if (r7 == *(void**)(ip+=8)) {
		ip += 8;
		ip += *(Int*)ip;
		ip += 8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip += 2*8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	/* Jump to immediate2 if r0 not equal to immediate 1. */
	bnei0: OPDB("bnei0 r0="OBJ" type="HEX, r0, *(void**)(ip+8));
	if (r0 != *(void**)(ip+=8)) {
		ip += 8;
		ip += *(Int*)ip;
		ip += 8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip += 2*8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	/* Jump to immediate2 if r1 not equal to immediate 1. */
	bnei1: OPDB("bnei1");
	if (r1!=*(void**)(ip+=8)) {
		ip += 8;
		ip += *(Int*)ip;
		ip +=8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip += 2*8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	/* Jump to immediate 2 if r0's type equals to immediate 1. */
	brti0: OPDB("brti0");
	if (((Num)r0>0xfffff) && (memObjectType(r0))==*(Num*)(ip+=8)) {
		ip += 8;
		ip += *(Int*)ip;
		ip += 8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip += 2*8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	/* Jump to immediate 2 if r0's type not equal to immediate 1. */
	bnti0: OPDB("bnti0");
	if (((Num)r0<0x100000) || (memObjectType(r0))!=*(Num*)(ip+=8)) {
		ip += 8;
		ip += *(Int*)ip;
		ip += 8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	} else {
		ip += 2*8;
		if (interrupt) vmInterruptHandler();
		goto **(void**)(ip);
	}

	/* Branch always. */
	bra: OPDB("bra");
	ip += 8l;
	ip += *(Num*)ip;
	ip += 8l;
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
	ip = code + (Int)retip;
	ip += 8;
	if (interrupt) vmInterruptHandler();
	goto **(void**)(ip);

	/* Immediate syscall.  Like 'sys' only C address is immediate value.  Set
		the ip to next instruction right first so that the syscall runs with
		the IP at the next instruction. */ 
	sysi: OPDB("sysi");
	ip += (2*8);
	vmPreGarbageCollect();
	(*(void(**)(void))((Obj*)code+(Int)ip-1))();
	vmPostGarbageCollect();
	if (interrupt) vmInterruptHandler();
	goto **(void**)ip;

	/* System call.  Really just a C function call, address in accumulator. 
		imediate field is passed to C function.  See sysi comment for other
		information. */
	sys0: OPDB("sys0");
	ip += 8;
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
	DB(INDENT0"::"STR  " code:"OBJ  " ip:"INT, __func__, code, ip);
	vmVm (RUN);
	DB(INDENT1"--"STR, __func__);
}


/* Default Object serializer
 */
void vmObjectDumperDefault (Obj o, FILE *stream) {
 static char *p;

	fprintf (stream, HEX, o);

	if ((p=memObjString(o))) fprintf (stream, ":%s", p);
}

void (*vmObjectDumper)(Obj o, FILE *stream) = vmObjectDumperDefault;


/* Called by asm.c */
void vmInitialize (Func scheduler, Func preGC, Func postGC, void(*vmObjDumper)(Obj, FILE*)) {
 static int shouldInitialize=1;
	if (shouldInitialize) {
		shouldInitialize=0;
		vmVm(INIT); // Initialize the opcodes (C jump addresses).
		if (scheduler) vmCallerScheduler = scheduler;
		if (preGC) vmCallerPreGarbageCollect = preGC;
		if (postGC) vmCallerPostGarbageCollect = postGC;
		if (vmObjDumper) vmObjectDumper = vmObjDumper;
		memInitialize(&vmPreGarbageCollect, &vmPostGarbageCollect);
		memNewStack(); stack = r0;
		memNewStack(); asmstack = r0;
		/* Start the interrupt schedule timer. */
		signal(SIGALRM, vmSigAlarmHandler);
		vmSigAlarmReset();
	}
}


void vmDebugDumpCode (Obj c, FILE *stream) {
 int fdState;
 Obj *i=c;
 Num asmLineNumber;

	fcntl (0, F_SETFL, (fdState=fcntl(0, F_GETFL, 0))&~O_NONBLOCK);

	DB (INDENT1"::"STR " "OBJ"  code:"OBJ"  ip:"OBJ, __func__, c, code, ip);

	if (stream == NULL) stream=stderr;

	while (i < ((Obj*)c + memObjectLength(c))) { // Forcing pointer arithmetic.
		asmLineNumber = i-(Obj*)c;
		fprintf (stream, NL OBJ STR HEX04" ",
			i,
			(i==ip || asmLineNumber==(Num)ip)?"*":" ",
			asmLineNumber);
		if (*i == NOP)      {fprintf(stream, "nop");}
		else if (*i==MVI0)  {fprintf(stream, "mvi_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==MVI1)  {fprintf(stream, "mvi_1 "); vmObjectDumper(*++i, stream);}
		else if (*i==MVI2)  {fprintf(stream, "mvi_2 "); vmObjectDumper(*++i, stream);}
		else if (*i==MVI3)  {fprintf(stream, "mvi_3 "); vmObjectDumper(*++i, stream);}
		else if (*i==MVI4)  {fprintf(stream, "mvi_4 "); vmObjectDumper(*++i, stream);}
		else if (*i==MVI5)  {fprintf(stream, "mvi_5 "); vmObjectDumper(*++i, stream);}
		else if (*i==MVI6)  {fprintf(stream, "mvi_6 "); vmObjectDumper(*++i, stream);}
		else if (*i==MVI7)  {fprintf(stream, "mvi_7 "); vmObjectDumper(*++i, stream);}
		else if (*i==MV01)  {fprintf(stream, "mv_0_1 ");}
		else if (*i==MV02)  {fprintf(stream, "mv_0_2 ");}
		else if (*i==MV03)  {fprintf(stream, "mv_0_3 ");}
		else if (*i==MV04)  {fprintf(stream, "mv_0_4 ");}
		else if (*i==MV07)  {fprintf(stream, "mv_0_7 ");}
		else if (*i==MV016) {fprintf(stream, "mv_0_16 ");}
		else if (*i==MV01C) {fprintf(stream, "mv_0_1c ");}
		else if (*i==MV10)  {fprintf(stream, "mv_1_0 ");}
		else if (*i==MV13)  {fprintf(stream, "mv_1_3 ");}
		else if (*i==MV116) {fprintf(stream, "mv_1_16 ");}
		else if (*i==MV20)  {fprintf(stream, "mv_2_0 ");}
		else if (*i==MV23)  {fprintf(stream, "mv_2_3 ");}
		else if (*i==MV30)  {fprintf(stream, "mv_3_0 ");}
		else if (*i==MV50)  {fprintf(stream, "mv_5_0 ");}
		else if (*i==MV416) {fprintf(stream, "mv_4_16 ");}
		else if (*i==MV160) {fprintf(stream, "mv_16_0 ");}
		else if (*i==MV162) {fprintf(stream, "mv_16_2 ");}
		else if (*i==MV164) {fprintf(stream, "mv_16_4 ");}
		else if (*i==MV61) {fprintf(stream, "mv_6_1 ");}
		else if (*i==MV72) {fprintf(stream, "mv_7_2 ");}
		else if (*i==LDI00) {fprintf(stream, "ldi_0_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI02) {fprintf(stream, "ldi_0_2 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI016){fprintf(stream, "ldi_0_16 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI11) {fprintf(stream, "ldi_1_1 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI116){fprintf(stream, "ldi_1_16 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI20) {fprintf(stream, "ldi_2_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI22) {fprintf(stream, "ldi_2_2 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI40) {fprintf(stream, "ldi_4_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI50) {fprintf(stream, "ldi_5_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI160){fprintf(stream, "ldi_16_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI1616){fprintf(stream, "ldi_16_16 "); vmObjectDumper(*++i, stream);}
		else if (*i==LD012) {fprintf(stream, "ld0_1_2");}
		else if (*i==STI01) {fprintf(stream, "sti_0_1 "); vmObjectDumper(*++i, stream);}
		else if (*i==STI016){fprintf(stream, "sti_0_16 "); vmObjectDumper(*++i, stream);}
		else if (*i==STI20) {fprintf(stream, "sti_2_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==STI21) {fprintf(stream, "sti_2_1 "); vmObjectDumper(*++i, stream);}
		else if (*i==STI30) {fprintf(stream, "sti_3_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==STI40) {fprintf(stream, "sti_4_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==STI50) {fprintf(stream, "sti_5_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==ST012) {fprintf(stream, "st_0_1_2 ");}
		else if (*i==ST201) {fprintf(stream, "st_2_0_1 ");}
		else if (*i==PUSH0) {fprintf(stream, "push_0 ");}
		else if (*i==PUSH1) {fprintf(stream, "push_1 ");}
		else if (*i==PUSH2) {fprintf(stream, "push_2 ");}
		else if (*i==PUSH3) {fprintf(stream, "push_3 ");}
		else if (*i==PUSH4) {fprintf(stream, "push_4 ");}
		else if (*i==PUSH7) {fprintf(stream, "push_7 ");}
		else if (*i==PUSH15){fprintf(stream, "push_15 ");}
		else if (*i==PUSH16){fprintf(stream, "push_16 ");}
		else if (*i==PUSH1D){fprintf(stream, "push_1d ");}
		else if (*i==PUSH1E){fprintf(stream, "push_1e ");}
		else if (*i==POP0)  {fprintf(stream, "pop_0 ");}
		else if (*i==POP1)  {fprintf(stream, "pop_1 ");}
		else if (*i==POP2)  {fprintf(stream, "pop_2 ");}
		else if (*i==POP3)  {fprintf(stream, "pop_3 ");}
		else if (*i==POP4)  {fprintf(stream, "pop_4 ");}
		else if (*i==POP7)  {fprintf(stream, "pop_7 ");}
		else if (*i==POP15) {fprintf(stream, "pop_15 ");}
		else if (*i==POP1D) {fprintf(stream, "pop_1d ");}
		else if (*i==POP1E) {fprintf(stream, "pop_1e ");}
		else if (*i==ADDI0) {fprintf(stream, "addi_0 %d", *(i+1)); i++; }
		else if (*i==ADDI1) {fprintf(stream, "addi_1 %d", *(i+1)); i++; }
		else if (*i==ADD10) {fprintf(stream, "add_1_0 "); }
		else if (*i==MUL10) {fprintf(stream, "mul_1_0 "); }
		else if (*i==BLTI1) {fprintf(stream, "blti_1 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==BEQI0) {fprintf(stream, "beqi_0 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==BEQI1) {fprintf(stream, "beqi_1 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==BEQI7) {fprintf(stream, "beqi_7 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==BNEI0) {fprintf(stream, "bnei_0 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==BNEI1) {fprintf(stream, "bnei_1 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==BRTI0) {fprintf (stream, "brti_0 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==BNTI0) {fprintf (stream, "bnti_0 %08x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==BRA)   {fprintf (stream, "bra %04x",
									 2+i-(Obj*)c+(Int)*(i+1)/8); i++;}
		else if (*i==J0)    {fprintf (stream, "j_0 ");}
		else if (*i==J2)    {fprintf (stream, "j_2 ");}
		else if (*i==JAL0)  {fprintf (stream, "jal_0 ");}
		else if (*i==JAL2)  {fprintf (stream, "jal_2 ");}
		else if (*i==RET)   {fprintf (stream, "ret ");}
		else if (*i==SYSI)  {fprintf (stream, "sysi "); vmObjectDumper(*++i, stream);}
		else if (*i==SYS0)  {fprintf (stream, "sys_0 ");}
		else if (*i==QUIT)  {fprintf (stream, "quit ");}
		else {
			fprintf (stream, "???:"HEX" ", *i);
			vmObjectDumper(*i, stream);
		}
		i++;
		fflush(stdout);
	}
	printf (NL);
	fcntl (0, F_SETFL, fdState);
	DB (INDENT2"--"STR, __func__);
}
#undef DB_MODULE
