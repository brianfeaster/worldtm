#define DEBUG 0
#define DB_DESC "VM "
#define VALIDATE 1
#include "debug.h"
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h> /* For write(). */
#include <assert.h>
#include "vm.h"
#include "mem.h"
/* While the virtual machine is running, register rip/r1d, which is the opcode
   index, is transformed into a pointer into the rcode/r1e vector object.  This
   transformation is undone and rip reverted to an immediate index value when
   a SYS opcode is performed or the interrupt handler called.  */

/* Debg dump the current opcode
*/
#define OPDB(s,...) DBE fprintf(stderr,"\n"OBJ":"HEX" " s, rcode, ((Obj*)rip-(Obj*)rcode), ##__VA_ARGS__)
//#define OPDB(s) DBE fprintf(stderr,"\n"OBJ":"HEX" " s, rcode+8*((Obj*)rip-(Obj*)rcode), ((Obj*)rip-(Obj*)rcode))



/* Registers.  These make up the root set used by the garbage collector.
 */
Obj r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7,
    r8,  r9,  ra,  rb,  rc,  rd,  re,  rf,
    r10, r11, r12, r13, r14, r15, r16, r17,
    r18, r19, r1a, r1b, r1c, r1d, r1e, r1f;

int vmRunCount=0;


void vmPush (Obj o) {
	memStackPush(r1f, o);
}


Obj vmPop (void) {
	return memStackPop(r1f);
}


void vmRunRestore (void) {
	DBBEG("  ip:"OBJ"  code:"OBJ, rip, rcode);
	assert(1 == vmRunCount);
	--vmRunCount;
	/* Only convert ip to offset if it's a pointer (big number larger pointing into
		the code object).  Multiply by eight to force opcode offset (not byte). */
	if (rip >= rcode) rip = (Obj)((Obj*)rip - (Obj*)rcode);
	DBEND("  ip:"OBJ"  code:"OBJ, rip, rcode);
}

void vmRunSetup (void) {
	DBBEG("  ip:"OBJ"  code:"OBJ, rip, rcode);
	assert(0 == vmRunCount);
	++vmRunCount;
	/* Only convert ip to pointer if it's an offset (low number below the
		code's address). */
	if (rip < rcode) rip = (Obj)((Obj*)rcode + (Num)rip);
	DBEND("  ip:"OBJ"  code:"OBJ, rip, rcode);
}


/* This flag causes the virtual machine to make a call to the interrupt handler.
   It is set in this module by the timer signal handler.  It is also set
   in the sys module by catchSignal() the generic signaler handling mechanism.
*/
Int vmInterrupt=0;
Func vmInterruptHandler = NULL;

void vmProcessInterrupt (void) {
	DBBEG("  rcode:"OBJ" rip:"OBJ, rcode, rip);
	assert (vmInterruptHandler);
	vmInterrupt=0;
	vmRunRestore();
	vmInterruptHandler();
	vmRunSetup();
	DBEND("  rcode:"OBJ" rip:"OBJ, rcode, rip);
}


/* Virtual Machine
*/

void *NOP,
     *MVI0, *MVI1, *MVI2, *MVI3, *MVI4, *MVI5, *MVI6, *MVI7,
     *MV01, *MV02, *MV03, *MV04, *MV07, *MV01E, *MV10, *MV13,
     *MV20, *MV23, *MV30, *MV51C, *MV518,
     *MV50, *MV1C0, *MV1C18,
     *MV61, *MV72,
     *LDI00, *LDI02, *LDI05, *LDI01C, *LDI11, *LDI20, *LDI22, *LDI50, *LDI1C0, *LDI11C,
     *LDI40,
     *LD012,
     *STI01, *STI01C, *STI21, *STI20, *STI30, *STI40, *STI50,
     *ST012, *ST201,
     *PUSH0, *PUSH1, *PUSH2, *PUSH3, *PUSH4, *PUSH5, *PUSH7, *PUSH19,
     *PUSH1A, *PUSH1B,
     *POP0,  *POP1,  *POP2,  *POP3,  *POP4,  *POP7, *POP19, *POP1A,  *POP1B,
     *ADDI0, *ADDI1, *ADD10, *MUL10,
     *BLTI1,
     *BEQI0, *BEQI1, *BEQI7, *BNEI0, *BNEI1, *BNEI5, *BRTI0, *BNTI0, *BRA,
     *J0, *J2, *JAL0, *JAL2, *RET,
     *SYSI, *SYS0, *QUIT;

#define VM_INIT 0
#define VM_RUN  1
void vmVm (Int cmd) {
	/* Initialize the opcodes.  Really we're just assigning a bunch of global
		label pointers the jump addresses of all the opcode implementations in
		this function. */
	if (cmd == VM_INIT) {
		DBBEG("Initializing opcode values");
		NOP=&&nop;

		MVI0=&&mvi0;   MVI1=&&mvi1;   MVI2=&&mvi2;  MVI3=&&mvi3; MVI4=&&mvi4;
		MVI5=&&mvi5;   MVI6=&&mvi6;   MVI7=&&mvi7;
		MV01=&&mv01;   MV02=&&mv02;   MV03=&&mv03;  MV04=&&mv04; MV07=&&mv07;
		MV01E=&&mv01e;
		MV10=&&mv10;   MV13=&&mv13;  MV20=&&mv20;   MV23=&&mv23;
		MV30=&&mv30;   MV51C=&&mv51c; MV518=&&mv518; MV50=&&mv50;
		MV1C0=&&mv1c0; MV1C18=&&mv1c18;
		MV61=&&mv61;   MV72=&&mv72;

		LDI00=&&ldi00;   LDI02=&&ldi02;   LDI05=&&ldi05;  LDI01C=&&ldi01c; LDI11=&&ldi11;
      LDI11C=&&ldi11c; LDI20=&&ldi20;  LDI22=&&ldi22;   LDI40=&&ldi40;
      LDI50=&&ldi50;   LDI1C0=&&ldi1c0;

      LD012=&&ld012; STI01=&&sti01; STI01C=&&sti01c; STI20=&&sti20;
      STI21=&&sti21;
      STI30=&&sti30; STI40=&&sti40; STI50=&&sti50; ST012=&&st012;
      ST201=&&st201;

		PUSH0=&&push0; PUSH1=&&push1;   PUSH2=&&push2;   PUSH3=&&push3;
		PUSH4=&&push4; PUSH5=&&push5;   PUSH7=&&push7;   PUSH19=&&push19;
      PUSH1A=&&push1a; PUSH1B=&&push1b;

		POP0=&&pop0;    POP1=&&pop1;   POP2=&&pop2;   POP3=&&pop3;  POP4=&&pop4;  POP7=&&pop7;
		POP19=&&pop19;  POP1A=&&pop1a; POP1B=&&pop1b;

		ADDI0=&&addi0; ADDI1=&&addi1; ADD10=&&add10; MUL10=&&mul10;

		BLTI1=&&blti1;
		BEQI0=&&beqi0; BEQI1=&&beqi1; BEQI7=&&beqi7;
		BNEI0=&&bnei0; BNEI1=&&bnei1; BNEI5=&&bnei5;
		BRTI0=&&brti0; BNTI0=&&bnti0;
		BRA=&&bra;

		J0=&&j0;      J2=&&j2;
		JAL0=&&jal0;  JAL2=&&jal2;
		RET=&&ret;

		SYSI=&&sysi;   SYS0=&&sys0;   QUIT=&&quit;

		//TODO move this to the module that handles object dumps
		memObjStringSet(NOP);
		memObjStringSet(MVI0); memObjStringSet(MVI1); memObjStringSet(MVI2); memObjStringSet(MVI3);
		memObjStringSet(MVI4); memObjStringSet(MVI5); memObjStringSet(MVI6); memObjStringSet(MVI7);
		memObjStringSet(MV01); memObjStringSet(MV02); memObjStringSet(MV03); memObjStringSet(MV04);
		memObjStringSet(MV07); memObjStringSet(MV01E); memObjStringSet(MV10);
		memObjStringSet(MV13); memObjStringSet(MV20); memObjStringSet(MV23);
		memObjStringSet(MV30); memObjStringSet(MV51C); memObjStringSet(MV518);
		memObjStringSet(MV50);
		memObjStringSet(MV1C0); memObjStringSet(MV1C18); memObjStringSet(MV61); memObjStringSet(MV72);
		memObjStringSet(LDI00); memObjStringSet(LDI02); memObjStringSet(LDI05); memObjStringSet(LDI01C);
		memObjStringSet(LDI11); memObjStringSet(LDI11C); memObjStringSet(LDI20); memObjStringSet(LDI22);
		memObjStringSet(LDI40); memObjStringSet(LDI50); memObjStringSet(LDI1C0);
		memObjStringSet(LD012);
		memObjStringSet(STI01); memObjStringSet(STI01C); memObjStringSet(STI20); memObjStringSet(STI21);
		memObjStringSet(STI30); memObjStringSet(STI40); memObjStringSet(STI50); memObjStringSet(ST012);
		memObjStringSet(ST201);
		memObjStringSet(PUSH0); memObjStringSet(PUSH1); memObjStringSet(PUSH2); memObjStringSet(PUSH3);
		memObjStringSet(PUSH4); memObjStringSet(PUSH5); memObjStringSet(PUSH7); memObjStringSet(PUSH19);
		memObjStringSet(PUSH1A); memObjStringSet(PUSH1B);
		memObjStringSet(POP0); memObjStringSet(POP1); memObjStringSet(POP2); memObjStringSet(POP3);
		memObjStringSet(POP4); memObjStringSet(POP7); memObjStringSet(POP19);
		memObjStringSet(POP1A); memObjStringSet(POP1B);
		memObjStringSet(ADDI0); memObjStringSet(ADDI1); memObjStringSet(ADD10);
		memObjStringSet(MUL10);
		memObjStringSet(BLTI1);
		memObjStringSet(BEQI0); memObjStringSet(BEQI1); memObjStringSet(BEQI7);
		memObjStringSet(BNEI0); memObjStringSet(BNEI1); memObjStringSet(BNEI5);
		memObjStringSet(BRTI0);
		memObjStringSet(BNTI0);
		memObjStringSet(BRA);
		memObjStringSet(J0); memObjStringSet(J2);
		memObjStringSet(JAL0); memObjStringSet(JAL2);
		memObjStringSet(RET);
		memObjStringSet(SYSI); memObjStringSet(SYS0);
		memObjStringSet(QUIT);

		DBEND();
		return;
	}

	/* pc, after a syscall, could have it's code object move.  Find
		all opcodes that could possibly cause a GC.  Better to NOT EVER USE
		LOCAL C VARS.  DUHHH.
	*/

	/* Since registers are really void* and opcodes are u64 words, instruction
		addresses must be adjusted by 8 times.
		void **pc = (void**)rcode + (int)rip;
	*/

	/* Run machine code by jumping to first opcode (code=r13  rip=r1b). */
	vmRunSetup();
	DBBEG("  Starting VM:  ip="HEX"  code="HEX"  *ip="HEX, rip, rcode, *(void**)rip);
	goto **(void**)rip;

	/* NOP */
	nop: OPDB("NOP");
	goto **(void**)(rip+=8);

	/* Load immediate value into register. */
	mvi0: OPDB("mvi0"); r0=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	mvi1: OPDB("mvi1"); r1=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	mvi2: OPDB("mvi2"); r2=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	mvi3: OPDB("mvi3"); r3=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	mvi4: OPDB("mvi4"); r4=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	mvi5: OPDB("mvi5"); r5=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	mvi6: OPDB("mvi6"); r6=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	mvi7: OPDB("mvi7"); r7=*(Obj*)(rip+=8); goto **(void**)(rip+=8);

	/* Copy regster to another. */
	mv01: OPDB("mv01"); r0=r1; goto **(void**)(rip+=8);
	mv02: OPDB("mv02"); r0=r2; goto **(void**)(rip+=8);
	mv03: OPDB("mv03"); r0=r3; goto **(void**)(rip+=8);
	mv04: OPDB("mv04"); r0=r4; goto **(void**)(rip+=8);
	mv07: OPDB("mv07"); r0=r7; goto **(void**)(rip+=8);
	mv01e: OPDB("mv01e"); r0=r1e; goto **(void**)(rip+=8);
	mv10: OPDB("mv10"); r1=r0; goto **(void**)(rip+=8);
	mv13: OPDB("mv13"); r1=r3; goto **(void**)(rip+=8);
	mv20: OPDB("mv20"); r2=r0; goto **(void**)(rip+=8);
	mv23: OPDB("mv23"); r2=r3; goto **(void**)(rip+=8);
	mv30: OPDB("mv30"); r3=r0; goto **(void**)(rip+=8);
	mv50: OPDB("mv50"); r5=r0; goto **(void**)(rip+=8);
	mv51c: OPDB("mv51c"); r5=r1c; goto **(void**)(rip+=8);
	mv518: OPDB("mv518"); r5=r18; goto **(void**)(rip+=8);
	mv1c0: OPDB("mv1c0"); r1c=r0; goto **(void**)(rip+=8);
	mv1c18: OPDB("mv1c18"); r1c=r18; goto **(void**)(rip+=8);
	mv61: OPDB("mv61"); r6=r1; goto **(void**)(rip+=8);
	mv72: OPDB("mv72"); r7=r2; goto **(void**)(rip+=8);

	/* Load r2 <- *(r0 + immediate) */
	ldi00: OPDB("ldi00");
	r0=memVectorObject(r0, *(Num*)(rip+=8));//*((Obj*)r0 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	ldi02: OPDB("ldi02");
	r0=memVectorObject(r2, *(Num*)(rip+=8));//*((Obj*)r2 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	ldi05: OPDB("ldi05");
	r0=memVectorObject(r5, *(Num*)(rip+=8));//*((Obj*)r5 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	ldi01c: OPDB("ldi01c");
	r0=memVectorObject(r1c, *(Num*)(rip+=8));//*((Obj*)r1c + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	ldi11: OPDB("ldi11");
	r1=memVectorObject(r1, *(Num*)(rip+=8));//*((Obj*)r1 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	ldi11c: OPDB("ldi11c");
	r1=memVectorObject(r1c, *(Num*)(rip+=8));//*((Obj*)r1c + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	ldi20: OPDB("ldi20");
	r2=//memVectorObject(r0, *(Num*)(rip+=8));
		*((Obj*)r0 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	ldi22: OPDB("ldi22");
	r2=//memVectorObject(r2, *(Num*)(rip+=8));
		*((Obj*)r2 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	ldi40: OPDB("ldi40");
	r4=memVectorObject(r0, *(Num*)(rip+=8));//*((Obj*)r0 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	ldi50: OPDB("ldi50");
	r5=memVectorObject(r0, *(Num*)(rip+=8));//*((Obj*)r0 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	ldi1c0: OPDB("ldi1c0");
	r1c=memVectorObject(r0, *(Num*)(rip+=8));//*((Obj*)r0 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);

	/* Load value in register's address plus register offset into register. */
	ld012: OPDB("ld012"); r0=*((Obj*)r1 + (Num)r2);  goto **(void**)(rip+=8);

	/* Store r0 -> *(r1 + immediate). */
	sti01:OPDB("sti01");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode sti01 %d < %d", memObjectLength(r1), *(Num*)(rip+8));
#endif
		*((Obj*)r1 + *(Num*)(rip+=8))=r0; goto **(void**)(rip+=8);
	sti01c:OPDB("sti01c");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r1c))) fprintf (stderr, "[ERROR opcode sti01c %d < %d", memObjectLength(r1c), *(Num*)(rip+8));
#endif
		*((Obj*)r1c + *(Num*)(rip+=8))=r0; goto **(void**)(rip+=8);
	sti20:OPDB("sti20");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti20 %d < %d", memObjectLength(r0), *(Num*)(rip+8));
#endif
		*((Obj*)r0 + *(Num*)(rip+=8))=r2; goto **(void**)(rip+=8);
	sti21:OPDB("sti21");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode sti21 %d < %d", memObjectLength(r1), *(Num*)(rip+8));
#endif
		*((Obj*)r1 + *(Num*)(rip+=8))=r2; goto **(void**)(rip+=8);
	sti30:OPDB("sti30");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti30 %d < %d", memObjectLength(r0), *(Num*)(rip+8));
#endif
		*((Obj*)r0 + *(Num*)(rip+=8))=r3; goto **(void**)(rip+=8);
	sti40:OPDB("sti40");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti40 %d < %d", memObjectLength(r0), *(Num*)(rip+8));
#endif
		*((Obj*)r0 + *(Num*)(rip+=8))=r4; goto **(void**)(rip+=8);
	sti50:OPDB("sti50");
#if VALIDATE
		if (!((0 <= *(Num*)(rip+8)) && (*(Num*)(rip+8) < memObjectLength(r0)))) fprintf (stderr, "[ERROR opcode sti50 %08x < %08x]\n", memObjectLength(r0), *(Num*)(rip+8));
#endif
		*((Obj*)r0 + *(Num*)(rip+=8))=r5; goto **(void**)(rip+=8);

	/* Store r0 -> *(r1 + r2). */
	st012: OPDB("st012");
#if VALIDATE
		if (!(0 <= r2 &&  (Int)r2 < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode st012 %d < %d", memObjectLength(r1), r2);
#endif
		*((Obj*)r1 + (Num)r2) = r0;  goto **(void**)(rip+=8);
	st201: OPDB("st201"); *((Obj*)r0 + (Num)r1) = r2;  goto **(void**)(rip+=8);

	/* Push register using local stack pointer. */
	push0: OPDB("push0");  vmPush(r0);  goto **(void**)(rip+=8);
	push1: OPDB("push1");  vmPush(r1);  goto **(void**)(rip+=8);
	push2: OPDB("push2");  vmPush(r2);  goto **(void**)(rip+=8);
	push3: OPDB("push3");  vmPush(r3);  goto **(void**)(rip+=8);
	push4: OPDB("push4");  vmPush(r4);  goto **(void**)(rip+=8);
	push5: OPDB("push5");  vmPush(r5);  goto **(void**)(rip+=8);
	push7: OPDB("push7");  vmPush(r7);  goto **(void**)(rip+=8);
	push19:OPDB("push19"); vmPush(r19); goto **(void**)(rip+=8);
	push1a:OPDB("push1a"); vmPush(r1a); goto **(void**)(rip+=8);
	push1b:OPDB("push1b"); vmPush(r1b); goto **(void**)(rip+=8);


	/* Pop into a register. */
	pop0: OPDB("pop0");   r0 = vmPop();  goto **(void**)(rip+=8);
	pop1: OPDB("pop1");   r1 = vmPop();  goto **(void**)(rip+=8);
	pop2: OPDB("pop2");   r2 = vmPop();  goto **(void**)(rip+=8);
	pop3: OPDB("pop3");   r3 = vmPop();  goto **(void**)(rip+=8);
	pop4: OPDB("pop4");   r4 = vmPop();  goto **(void**)(rip+=8);
	pop7: OPDB("pop7");   r7 = vmPop();  goto **(void**)(rip+=8);
	pop19:OPDB("pop19"); r19 = vmPop();  goto **(void**)(rip+=8);
	pop1a:OPDB("pop1a"); r1a = vmPop();  goto **(void**)(rip+=8);
	pop1b:OPDB("pop1b"); r1b = vmPop();  goto **(void**)(rip+=8);

	/* Add immediate to r0. */
	addi0: OPDB("addi0"); r0 += *(Int*)(rip+=8); goto **(void**)(rip+=8);
	addi1: OPDB("addi1"); r1 += *(Int*)(rip+=8); goto **(void**)(rip+=8);

	/* Mutate object r1 with (object r1 + object r0). */
	add10: OPDB("add10"); *(Int*)r1 += *(Int*)r0; goto **(void**)(rip+=8);
	mul10: OPDB("mul10"); *(Int*)r1 *= *(Int*)r0; goto **(void**)(rip+=8);

	blti1: OPDB("blti1");
	if (r1<*(void**)(rip+=8)) {
		rip += 8;
		rip += *(Int*)rip;
		rip += 8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	} else {
		rip += 2*8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	}

	/* Jump to immediate2 if r0 equal to immediate 1
	*/
	beqi0: OPDB("beqi0");
	if (r0==*(void**)(rip+=8)) {
		rip += 8;
		rip += *(Int*)rip;
		rip += 8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	} else {
		rip += 2*8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	}

	beqi1: OPDB("beqi1");
	if (r1 == *(Obj*)(rip+=8)) {
		rip += 8;
		rip += *(Int*)rip;
		rip += 8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	} else {
		rip += 2*8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	}

	beqi7: OPDB("beqi7");
	if (r7 == *(void**)(rip+=8)) {
		rip += 8;
		rip += *(Int*)rip;
		rip += 8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	} else {
		rip += 2*8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	}

	/* Jump to immediate2 if r0 not equal to immediate 1. */
	bnei0: OPDB("bnei0 r0="OBJ" type="HEX, r0, *(void**)(rip+8));
	if (r0 != *(void**)(rip+=8)) {
		rip += 8;
		rip += *(Int*)rip;
		rip += 8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	} else {
		rip += 2*8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	}

	/* Jump to immediate2 if r1 not equal to immediate 1. */
	bnei1: OPDB("bnei1");
	if (r1!=*(void**)(rip+=8)) {
		rip += 8;
		rip += *(Int*)rip;
		rip +=8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	} else {
		rip += 2*8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	}

	bnei5: OPDB("bnei5");
	if (r5!=*(void**)(rip+=8)) {
		rip += 8;
		rip += *(Int*)rip;
		rip +=8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	} else {
		rip += 2*8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	}

	/* Jump to immediate 2 if r0's type equals to immediate 1. */
	brti0: OPDB("brti0");
	if (((Num)r0>0xfffff) && (memObjectType(r0))==*(Num*)(rip+=8)) {
		rip += 8;
		rip += *(Int*)rip;
		rip += 8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	} else {
		rip += 2*8;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	}

	/* Jump to immediate 2 if r0's type not equal to immediate 1. */
	bnti0: OPDB("bnti0");
	if (((Num)r0<0x100000) || (memObjectType(r0))!=*(Num*)(rip+=8)) {
		rip += 8l;
		rip += *(Int*)rip;
		rip += 8l;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	} else {
		rip += 2*8l;
		if (vmInterrupt) vmProcessInterrupt();
		goto **(void**)(rip);
	}

	/* Branch always. */
	bra: OPDB("bra");
	rip += 8l;
	rip += *(Num*)rip;
	rip += 8l;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)(rip);

	/* Jump to first instruction in block in r0. */
	j0: OPDB("j0");
	rip = rcode = r0;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	j2: OPDB("j2");
	rip = rcode = r2;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	/* Link block/offset then jump to first instruction in block in acc. */
	jal0: OPDB("jal0");
	/* Save the pc and program. */
	rretip = (Obj)(rip - rcode);
	rretcode = rcode;
	rretenv = renv;
	rip = rcode = r0;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	/* Link block/offset then jump to first instruction in block in acc. */
	jal2: OPDB("jal2");
	/* Save the pc and program. */
	rretip = (Obj)(rip - rcode);
	rretcode = rcode;
	rretenv = renv;
	rip = rcode = r2;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	/* Ret to caller. */
	ret: OPDB("ret");
	renv = rretenv;
	rcode = rretcode;
	rip = rcode + (Int)rretip;
	rip += 8;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)(rip);

	/* Immediate syscall.  Like 'sys' only C address is immediate value.  Set
		the rip to next instruction right first so that the syscall runs with
		the IP at the next instruction. */ 
	sysi: OPDB("sysi");
	rip += (2*8);
	vmRunRestore();
	(*(void(**)(void))((Obj*)rcode+(Int)rip-1))();
	vmRunSetup();
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	/* System call.  Really just a C function call, address in accumulator. 
		imediate field is passed to C function.  See sysi comment for other
		information. */
	sys0: OPDB("sys0");
	rip += 8;
	vmRunRestore();
	(*(void(*)(void))r0)();
	vmRunSetup();
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	/* Halt virtual machine.  Return to OS?*/
	quit: OPDB("quit");
	vmRunRestore();
	DBEND();
	return;
}

/* Starts virtual machine using the program code (r1c) starting at instruction
   offset in immediate rip (r1b).
*/
void vmRun (void) {
	DBBEG("  code:"OBJ  " ip:"INT, rcode, rip);
	vmVm (VM_RUN);
	DBEND();
}



/* Default Object serializer
 */
void vmObjectDumperDefault (Obj o, FILE *stream) {
 static Str p;
	fprintf (stream, HEX, o);
	if ((p = memObjString(o))) fprintf (stream, ":%s", p);
}

void (* vmObjectDumper)(Obj o, FILE *stream) = vmObjectDumperDefault;

void vmDebugDumpCode (Obj c, FILE *stream) {
 int fdState;
 Obj *i=c;
 Num lineNumber;

	/* Reenable i/o blocking */
	fcntl (0, F_SETFL, (fdState=fcntl(0, F_GETFL, 0))&~O_NONBLOCK);

	DBBEG ("  "OBJ"  rcode:"OBJ"  rip:"OBJ, c, rcode, rip);

	if (stream == NULL) stream=stderr;

	while (i < ((Obj*)c + memObjectLength(c))) { // Forcing pointer arithmetic.
		lineNumber = (Num)(i-(Obj*)c);
		fprintf (stream, NL OBJ STR HEX04" ",
			i,
			(i==rip || lineNumber==(Num)rip)?"*":" ",
			lineNumber);
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
		else if (*i==MV01E) {fprintf(stream, "mv_0_1e ");}
		else if (*i==MV10)  {fprintf(stream, "mv_1_0 ");}
		else if (*i==MV13)  {fprintf(stream, "mv_1_3 ");}
		else if (*i==MV20)  {fprintf(stream, "mv_2_0 ");}
		else if (*i==MV23)  {fprintf(stream, "mv_2_3 ");}
		else if (*i==MV30)  {fprintf(stream, "mv_3_0 ");}
		else if (*i==MV50)  {fprintf(stream, "mv_5_0 ");}
		else if (*i==MV51C) {fprintf(stream, "mv_5_1c ");}
		else if (*i==MV518) {fprintf(stream, "mv_5_18 ");}
		else if (*i==MV1C0) {fprintf(stream, "mv_1c_0 ");}
		else if (*i==MV1C18) {fprintf(stream, "mv_1c_18 ");}
		else if (*i==MV61) {fprintf(stream, "mv_6_1 ");}
		else if (*i==MV72) {fprintf(stream, "mv_7_2 ");}
		else if (*i==LDI00) {fprintf(stream, "ldi_0_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI02) {fprintf(stream, "ldi_0_2 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI05) {fprintf(stream, "ldi_0_5 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI01C){fprintf(stream, "ldi_0_1c "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI11) {fprintf(stream, "ldi_1_1 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI11C){fprintf(stream, "ldi_1_1c "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI20) {fprintf(stream, "ldi_2_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI22) {fprintf(stream, "ldi_2_2 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI40) {fprintf(stream, "ldi_4_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI50) {fprintf(stream, "ldi_5_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==LDI1C0){fprintf(stream, "ldi_1c_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==LD012) {fprintf(stream, "ld0_1_2");}
		else if (*i==STI01) {fprintf(stream, "sti_0_1 "); vmObjectDumper(*++i, stream);}
		else if (*i==STI01C){fprintf(stream, "sti_0_1c "); vmObjectDumper(*++i, stream);}
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
		else if (*i==PUSH5) {fprintf(stream, "push_5 ");}
		else if (*i==PUSH7) {fprintf(stream, "push_7 ");}
		else if (*i==PUSH19){fprintf(stream, "push_19 ");}
		else if (*i==PUSH1A){fprintf(stream, "push_1a ");}
		else if (*i==PUSH1B){fprintf(stream, "push_1b ");}
		else if (*i==POP0)  {fprintf(stream, "pop_0 ");}
		else if (*i==POP1)  {fprintf(stream, "pop_1 ");}
		else if (*i==POP2)  {fprintf(stream, "pop_2 ");}
		else if (*i==POP3)  {fprintf(stream, "pop_3 ");}
		else if (*i==POP4)  {fprintf(stream, "pop_4 ");}
		else if (*i==POP7)  {fprintf(stream, "pop_7 ");}
		else if (*i==POP19) {fprintf(stream, "pop_19 ");}
		else if (*i==POP1A) {fprintf(stream, "pop_1a ");}
		else if (*i==POP1B) {fprintf(stream, "pop_1b ");}
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
		else if (*i==BNEI5) {fprintf(stream, "bnei_5 ");
									vmObjectDumper(*(i+1), stream);
									fprintf(stream, " %04x", 3+i-(Obj*)c+(Int)*(i+2)/8);
									i+=2;}
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
			fprintf(stream, HEX" = ", *i);
			vmObjectDumper(*i, stream);
		}
		i++;
		fflush(stdout);
	}
	printf (NL);
	fcntl (0, F_SETFL, fdState);
	DBEND ();
}




void vmInitialize (Func interruptHandler, void(*vmObjDumper)(Obj, FILE*)) {
 static Num shouldInitialize=1;
	DBBEG();
	if (shouldInitialize) {
		DB("  Activating module...");
		shouldInitialize=0;
		memInitialize(0, 0);
		DB("Create 'registers'");
		memRegisterRoot(r0);  memRegisterRoot(r1);  memRegisterRoot(r2);  memRegisterRoot(r3);
		memRegisterRoot(r4);  memRegisterRoot(r5);  memRegisterRoot(r6);  memRegisterRoot(r7);
		memRegisterRoot(r8);  memRegisterRoot(r9);  memRegisterRoot(ra);  memRegisterRoot(rb);
		memRegisterRoot(rc);  memRegisterRoot(rd);  memRegisterRoot(re);  memRegisterRoot(rf);
		memRegisterRoot(r10); memRegisterRoot(r11); memRegisterRoot(r12); memRegisterRoot(r13);
		memRegisterRoot(r14); memRegisterRoot(r15); memRegisterRoot(r16); memRegisterRoot(r17);
		memRegisterRoot(r18); memRegisterRoot(r19); memRegisterRoot(r1a); memRegisterRoot(r1b);
		memRegisterRoot(r1c); memRegisterRoot(r1d); memRegisterRoot(r1e); memRegisterRoot(r1f);
		DB("Create the stack");
		r1f = memNewStack();
		DB("Register the internal object types");
		memRegisterType (TCODE, "code");
		DB("Initialize opcode values");
		vmVm(VM_INIT); /* Opcode values are really C goto addresses */
		//DB("Activate periodic timer and its signal handler");
		//signal(SIGALRM, vmSigAlarmHandler); /* Start the interrupt schedule timer. */
		//vmSigAlarmReset();
	} else {
		DB("  Module already activated");
	}

	if (interruptHandler) {
		DB("  Setting interruptHandler callback function");
		assert(!vmInterruptHandler);
		vmInterruptHandler = interruptHandler; /* sys.c:sysSchedule()  */
	}
	if (vmObjDumper) {
		DB("  Setting vmObjDumper callback function");
		assert(vmObjectDumperDefault == vmObjectDumper); /* Verify the object dump callback is changed once */
		vmObjectDumper = vmObjDumper;
	}
	DBEND();
}


#undef DB_DESC
#undef DEBUG
