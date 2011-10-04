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

/* Debug dump the current opcode
*/
#define OPDB(s,...) DBE fprintf(stderr,"\n"OBJ":"HEX" " s, rcode, ((Obj*)rip-(Obj*)rcode), ##__VA_ARGS__)



/* Registers.  These make up the root set used by the garbage collector.
 */
Obj r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7,
    r8,  r9,  ra,  rb,  rc,  rd,  re,  rf,
    r10, r11, r12, r13, r14, r15, r16, r17,
    r18, r19, r1a, r1b, r1c, r1d, r1e, r1f;

void vmPush (Obj o) {
	memStackPush(r1f, o);
}


Obj vmPop (void) {
	return memStackPop(r1f);
}


int vmRunCount=0;

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
	assert (vmInterrupt);
	assert (vmInterruptHandler);
	vmInterrupt=0;
	vmRunRestore();
	vmInterruptHandler();
	vmRunSetup();
	DBEND("  rcode:"OBJ" rip:"OBJ, rcode, rip);
}


/* Virtual Machine
*/

void *vmNOP,
     *vmMVI0, *vmMVI1, *vmMVI2, *vmMVI3, *vmMVI4, *vmMVI5, *vmMVI6, *vmMVI7,
     *vmMV01, *vmMV02, *vmMV03, *vmMV04, *vmMV07, *vmMV01E, *vmMV10, *vmMV13,
     *vmMV20, *vmMV23, *vmMV30, *vmMV51C, *vmMV518,
     *vmMV50, *vmMV1C0, *vmMV1C18,
     *vmMV61, *vmMV72,
     *vmLDI00, *vmLDI02, *vmLDI01C, *vmLDI11, *vmLDI20, *vmLDI22, *vmLDI50, *vmLDI1C0, *vmLDI11C,
     *vmLD012,
     *vmSTI01, *vmSTI01C, *vmSTI21, *vmSTI20, *vmSTI30, *vmSTI40, *vmSTI50,
     *vmST012, *vmST201,
     *vmPUSH0, *vmPUSH1, *vmPUSH2, *vmPUSH3, *vmPUSH4, *vmPUSH5, *vmPUSH7, *vmPUSH19,
     *vmPUSH1A, *vmPUSH1B,
     *vmPOP0,  *vmPOP1,  *vmPOP2,  *vmPOP3,  *vmPOP4,  *vmPOP7, *vmPOP19, *vmPOP1A,  *vmPOP1B,
     *vmADDI0, *vmADDI1, *vmADDI2, *vmADD10, *vmMUL10,
     *vmBLTI1,
     *vmBEQI0, *vmBEQI1, *vmBEQI7, *vmBNEI0, *vmBNEI1, *vmBNEI2, *vmBNEI5, *vmBRTI0, *vmBNTI0, *vmBRA,
     *vmJ0, *vmJ2, *vmJAL0, *vmJAL2, *vmRET,
     *vmSYSI, *vmSYS0, *vmQUIT;


/* Macro which associates the opcode symbol with (1) a goto address (for the virtual machine)
   and (2) a string (for debug dumps):
    vmOPCODE = &&gOPCODE;
    memPointerRegister(vmOPCODE);
*/
#define memRegisterOpcode(OP) vm##OP=&&g##OP; memPointerRegister(vm##OP);

void vmVm (void) {
 static Num NeedToInitialized = 1;
	if (NeedToInitialized) {
		NeedToInitialized = 0;
		DBBEG("  Initializing opcodes' addresses and strings");
		/* Assign C jump label addresses of each opcode implementation to each opcode object symbol */
		memRegisterOpcode(NOP);

		memRegisterOpcode(MVI0); memRegisterOpcode(MVI1); memRegisterOpcode(MVI2); memRegisterOpcode(MVI3);
		memRegisterOpcode(MVI4); memRegisterOpcode(MVI5); memRegisterOpcode(MVI6); memRegisterOpcode(MVI7);

		memRegisterOpcode(MV01); memRegisterOpcode(MV02); memRegisterOpcode(MV03); memRegisterOpcode(MV04);
		memRegisterOpcode(MV07); memRegisterOpcode(MV01E);
		memRegisterOpcode(MV10); memRegisterOpcode(MV13); memRegisterOpcode(MV20);
		memRegisterOpcode(MV23); memRegisterOpcode(MV30);
		memRegisterOpcode(MV50); memRegisterOpcode(MV51C); memRegisterOpcode(MV518);
		memRegisterOpcode(MV61);
		memRegisterOpcode(MV72);
		memRegisterOpcode(MV1C0); memRegisterOpcode(MV1C18);

		memRegisterOpcode(LDI00); memRegisterOpcode(LDI02); memRegisterOpcode(LDI01C);
		memRegisterOpcode(LDI11); memRegisterOpcode(LDI11C);
		memRegisterOpcode(LDI20); memRegisterOpcode(LDI22);
		memRegisterOpcode(LDI50);
		memRegisterOpcode(LDI1C0);

		memRegisterOpcode(LD012);

		memRegisterOpcode(STI01); memRegisterOpcode(STI01C);
		memRegisterOpcode(STI20); memRegisterOpcode(STI21);
		memRegisterOpcode(STI30);
		memRegisterOpcode(STI40);
		memRegisterOpcode(STI50);

		memRegisterOpcode(ST012);
		memRegisterOpcode(ST201);

		memRegisterOpcode(PUSH0); memRegisterOpcode(PUSH1); memRegisterOpcode(PUSH2); memRegisterOpcode(PUSH3);
		memRegisterOpcode(PUSH4); memRegisterOpcode(PUSH5); memRegisterOpcode(PUSH7);
		memRegisterOpcode(PUSH19); memRegisterOpcode(PUSH1A); memRegisterOpcode(PUSH1B);

		memRegisterOpcode(POP0); memRegisterOpcode(POP1); memRegisterOpcode(POP2); memRegisterOpcode(POP3);
		memRegisterOpcode(POP4); memRegisterOpcode(POP7);
		memRegisterOpcode(POP19); memRegisterOpcode(POP1A); memRegisterOpcode(POP1B);

		memRegisterOpcode(ADDI0); memRegisterOpcode(ADDI1); memRegisterOpcode(ADDI2);

		memRegisterOpcode(ADD10);

		memRegisterOpcode(MUL10);

		memRegisterOpcode(BLTI1);

		memRegisterOpcode(BEQI0); memRegisterOpcode(BEQI1); memRegisterOpcode(BEQI7);

		memRegisterOpcode(BNEI0); memRegisterOpcode(BNEI1); memRegisterOpcode(BNEI2); memRegisterOpcode(BNEI5);

		memRegisterOpcode(BRTI0);

		memRegisterOpcode(BNTI0);

		memRegisterOpcode(BRA);

		memRegisterOpcode(J0); memRegisterOpcode(J2);

		memRegisterOpcode(JAL0); memRegisterOpcode(JAL2);

		memRegisterOpcode(RET);

		memRegisterOpcode(SYSI); memRegisterOpcode(SYS0);

		memRegisterOpcode(QUIT);

		DBEND();

		return;
	}

	/* Since registers are really void* and opcodes are u64 words, instruction
		addresses must be adjusted by 8 times.
		void **pc = (void**)rcode + (int)rip;
	*/

	vmRunSetup();

	DBBEG("  Starting VM:  ip="HEX"  code="HEX"  *ip="HEX, rip, rcode, *(void**)rip);
	goto **(void**)rip; /* Run machine code execution by "gotoing" the first opcode (code=r13  rip=r1b). */

	/* NOP */
	gNOP: OPDB("NOP");  goto **(void**)(rip+=8);

	/* Load immediate value into register. */
	gMVI0: OPDB("mvi0"); r0=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	gMVI1: OPDB("mvi1"); r1=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	gMVI2: OPDB("mvi2"); r2=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	gMVI3: OPDB("mvi3"); r3=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	gMVI4: OPDB("mvi4"); r4=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	gMVI5: OPDB("mvi5"); r5=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	gMVI6: OPDB("mvi6"); r6=*(Obj*)(rip+=8); goto **(void**)(rip+=8);
	gMVI7: OPDB("mvi7"); r7=*(Obj*)(rip+=8); goto **(void**)(rip+=8);

	/* Copy regster to another. */
	gMV01: OPDB("mv01"); r0=r1; goto **(void**)(rip+=8);
	gMV02: OPDB("mv02"); r0=r2; goto **(void**)(rip+=8);
	gMV03: OPDB("mv03"); r0=r3; goto **(void**)(rip+=8);
	gMV04: OPDB("mv04"); r0=r4; goto **(void**)(rip+=8);
	gMV07: OPDB("mv07"); r0=r7; goto **(void**)(rip+=8);
	gMV01E: OPDB("mv01e"); r0=r1e; goto **(void**)(rip+=8);
	gMV10: OPDB("mv10"); r1=r0; goto **(void**)(rip+=8);
	gMV13: OPDB("mv13"); r1=r3; goto **(void**)(rip+=8);
	gMV20: OPDB("mv20"); r2=r0; goto **(void**)(rip+=8);
	gMV23: OPDB("mv23"); r2=r3; goto **(void**)(rip+=8);
	gMV30: OPDB("mv30"); r3=r0; goto **(void**)(rip+=8);
	gMV50: OPDB("mv50"); r5=r0; goto **(void**)(rip+=8);
	gMV51C: OPDB("mv51c"); r5=r1c; goto **(void**)(rip+=8);
	gMV518: OPDB("mv518"); r5=r18; goto **(void**)(rip+=8);
	gMV61: OPDB("mv61"); r6=r1; goto **(void**)(rip+=8);
	gMV72: OPDB("mv72"); r7=r2; goto **(void**)(rip+=8);
	gMV1C0: OPDB("mv1c0"); r1c=r0; goto **(void**)(rip+=8);
	gMV1C18: OPDB("mv1c18"); r1c=r18; goto **(void**)(rip+=8);

	/* Load r2 <- *(r0 + immediate) */
	gLDI00: OPDB("ldi00");
	r0=memVectorObject(r0, *(Num*)(rip+=8));//*((Obj*)r0 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	gLDI02: OPDB("ldi02");
	r0=memVectorObject(r2, *(Num*)(rip+=8));//*((Obj*)r2 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	gLDI01C: OPDB("ldi01c");
	r0=memVectorObject(r1c, *(Num*)(rip+=8));//*((Obj*)r1c + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	gLDI11: OPDB("ldi11");
	r1=memVectorObject(r1, *(Num*)(rip+=8));//*((Obj*)r1 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	gLDI11C: OPDB("ldi11c");
	r1=memVectorObject(r1c, *(Num*)(rip+=8));//*((Obj*)r1c + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	gLDI20: OPDB("ldi20");
	r2=//memVectorObject(r0, *(Num*)(rip+=8));
		*((Obj*)r0 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	gLDI22: OPDB("ldi22");
	r2=//memVectorObject(r2, *(Num*)(rip+=8));
		*((Obj*)r2 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	gLDI50: OPDB("ldi50");
	r5=memVectorObject(r0, *(Num*)(rip+=8));//*((Obj*)r0 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);
	gLDI1C0: OPDB("ldi1c0");
	r1c=memVectorObject(r0, *(Num*)(rip+=8));//*((Obj*)r0 + *(Num*)(rip+=8));
	goto **(void**)(rip+=8);

	/* Load value in register's address plus register offset into register. */
	gLD012: OPDB("ld012"); r0=*((Obj*)r1 + (Num)r2);  goto **(void**)(rip+=8);

	/* Store r0 -> *(r1 + immediate). */
	gSTI01:OPDB("sti01");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode sti01 %d < %d", memObjectLength(r1), *(Num*)(rip+8));
#endif
		*((Obj*)r1 + *(Num*)(rip+=8))=r0; goto **(void**)(rip+=8);
	gSTI01C:OPDB("sti01c");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r1c))) fprintf (stderr, "[ERROR opcode sti01c %d < %d", memObjectLength(r1c), *(Num*)(rip+8));
#endif
		*((Obj*)r1c + *(Num*)(rip+=8))=r0; goto **(void**)(rip+=8);
	gSTI20:OPDB("sti20");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti20 %d < %d", memObjectLength(r0), *(Num*)(rip+8));
#endif
		*((Obj*)r0 + *(Num*)(rip+=8))=r2; goto **(void**)(rip+=8);
	gSTI21:OPDB("sti21");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode sti21 %d < %d", memObjectLength(r1), *(Num*)(rip+8));
#endif
		*((Obj*)r1 + *(Num*)(rip+=8))=r2; goto **(void**)(rip+=8);
	gSTI30:OPDB("sti30");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti30 %d < %d", memObjectLength(r0), *(Num*)(rip+8));
#endif
		*((Obj*)r0 + *(Num*)(rip+=8))=r3; goto **(void**)(rip+=8);
	gSTI40:OPDB("sti40");
#if VALIDATE
		if (!(0 <= *(Num*)(rip+8) && *(Num*)(rip+8) < memObjectLength(r0))) fprintf (stderr, "[ERROR opcode sti40 %d < %d", memObjectLength(r0), *(Num*)(rip+8));
#endif
		*((Obj*)r0 + *(Num*)(rip+=8))=r4; goto **(void**)(rip+=8);
	gSTI50:OPDB("sti50");
#if VALIDATE
		if (!((0 <= *(Num*)(rip+8)) && (*(Num*)(rip+8) < memObjectLength(r0)))) fprintf (stderr, "[ERROR opcode sti50 %08x < %08x]\n", memObjectLength(r0), *(Num*)(rip+8));
#endif
		*((Obj*)r0 + *(Num*)(rip+=8))=r5; goto **(void**)(rip+=8);

	/* Store r0 -> *(r1 + r2). */
	gST012: OPDB("st012");
#if VALIDATE
		if (!(0 <= r2 &&  (Int)r2 < memObjectLength(r1))) fprintf (stderr, "[ERROR opcode st012 %d < %d", memObjectLength(r1), r2);
#endif
		*((Obj*)r1 + (Num)r2) = r0;  goto **(void**)(rip+=8);
	gST201: OPDB("st201"); *((Obj*)r0 + (Num)r1) = r2;  goto **(void**)(rip+=8);

	/* Push register using local stack pointer. */
	gPUSH0: OPDB("push0");  vmPush(r0);  goto **(void**)(rip+=8);
	gPUSH1: OPDB("push1");  vmPush(r1);  goto **(void**)(rip+=8);
	gPUSH2: OPDB("push2");  vmPush(r2);  goto **(void**)(rip+=8);
	gPUSH3: OPDB("push3");  vmPush(r3);  goto **(void**)(rip+=8);
	gPUSH4: OPDB("push4");  vmPush(r4);  goto **(void**)(rip+=8);
	gPUSH5: OPDB("push5");  vmPush(r5);  goto **(void**)(rip+=8);
	gPUSH7: OPDB("push7");  vmPush(r7);  goto **(void**)(rip+=8);
	gPUSH19:OPDB("push19"); vmPush(r19); goto **(void**)(rip+=8);
	gPUSH1A:OPDB("push1a"); vmPush(r1a); goto **(void**)(rip+=8);
	gPUSH1B:OPDB("push1b"); vmPush(r1b); goto **(void**)(rip+=8);


	/* Pop into a register. */
	gPOP0: OPDB("pop0");   r0 = vmPop();  goto **(void**)(rip+=8);
	gPOP1: OPDB("pop1");   r1 = vmPop();  goto **(void**)(rip+=8);
	gPOP2: OPDB("pop2");   r2 = vmPop();  goto **(void**)(rip+=8);
	gPOP3: OPDB("pop3");   r3 = vmPop();  goto **(void**)(rip+=8);
	gPOP4: OPDB("pop4");   r4 = vmPop();  goto **(void**)(rip+=8);
	gPOP7: OPDB("pop7");   r7 = vmPop();  goto **(void**)(rip+=8);
	gPOP19:OPDB("POP19"); r19 = vmPop();  goto **(void**)(rip+=8);
	gPOP1A:OPDB("pop1a"); r1a = vmPop();  goto **(void**)(rip+=8);
	gPOP1B:OPDB("pop1b"); r1b = vmPop();  goto **(void**)(rip+=8);

	/* Add immediate to r0. */
	gADDI0: OPDB("addi0"); r0 += *(Int*)(rip+=8); goto **(void**)(rip+=8);
	gADDI1: OPDB("addi1"); r1 += *(Int*)(rip+=8); goto **(void**)(rip+=8);
	gADDI2: OPDB("addi2"); r2 += *(Int*)(rip+=8); goto **(void**)(rip+=8);

	/* Mutate object r1 with (object r1 + object r0). */
	gADD10: OPDB("add10"); *(Int*)r1 += *(Int*)r0; goto **(void**)(rip+=8);
	gMUL10: OPDB("mul10"); *(Int*)r1 *= *(Int*)r0; goto **(void**)(rip+=8);

	gBLTI1: OPDB("blti1");
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
	gBEQI0: OPDB("beqi0");
	if (r0 == *(void**)(rip+=8)) {
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

	gBEQI1: OPDB("beqi1");
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

	gBEQI7: OPDB("beqi7");
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
	gBNEI0: OPDB("bnei0 r0="OBJ" type="HEX, r0, *(void**)(rip+8));
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
	gBNEI1: OPDB("bnei1");
	if (r1 != *(void**)(rip+=8)) {
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

	/* Jump to immediate2 if r1 not equal to immediate 1. */
	gBNEI2: OPDB("bnei2");
	if (r2 != *(void**)(rip+=8)) {
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

	gBNEI5: OPDB("bnei5");
	if (r5 != *(void**)(rip+=8)) {
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
	gBRTI0: OPDB("brti0");
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
	gBNTI0: OPDB("bnti0");
	if (((Num)r0<0x430000) || (memObjectType(r0))!=*(Num*)(rip+=8)) {
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
	gBRA: OPDB("bra");
	rip += 8l;
	rip += *(Num*)rip;
	rip += 8l;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)(rip);

	/* Jump to first instruction in block in r0. */
	gJ0: OPDB("j0");
	rip = rcode = r0;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	gJ2: OPDB("j2");
	rip = rcode = r2;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	/* Link block/offset then jump to first instruction in block in acc. */
	gJAL0: OPDB("jal0");
	/* Save the pc and program. */
	rretip = (Obj)(rip - rcode);
	rretcode = rcode;
	rretenv = renv;
	rip = rcode = r0;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	/* Link block/offset then jump to first instruction in block in acc. */
	gJAL2: OPDB("jal2");
	/* Save the pc and program. */
	rretip = (Obj)(rip - rcode);
	rretcode = rcode;
	rretenv = renv;
	rip = rcode = r2;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	/* Ret to caller. */
	gRET: OPDB("ret");
	renv = rretenv;
	rcode = rretcode;
	rip = rcode + (Int)rretip;
	rip += 8;
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)(rip);

	/* Immediate syscall.  Like 'sys' only C address is immediate value.  Set
		the rip to next instruction right first so that the syscall runs with
		the IP at the next instruction. */ 
	gSYSI: OPDB("sysi");
	rip += (2*8);
	vmRunRestore();
	(*(void(**)(void))((Obj*)rcode+(Int)rip-1))();
	vmRunSetup();
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	/* System call.  Really just a C function call, address in accumulator. 
		imediate field is passed to C function.  See sysi comment for other
		information. */
	gSYS0: OPDB("sys0");
	rip += 8;
	vmRunRestore();
	(*(void(*)(void))r0)();
	vmRunSetup();
	if (vmInterrupt) vmProcessInterrupt();
	goto **(void**)rip;

	/* Halt virtual machine.  Return to OS?*/
	gQUIT: OPDB("quit");
	vmRunRestore();
	DBEND();
	return;
}

/* Starts virtual machine using the program code (r1c) starting at instruction
   offset in immediate rip (r1b).
*/
void vmRun (void) {
	DBBEG("  code:"OBJ  " ip:"INT, rcode, rip);
	vmVm();
	DBEND();
}



/* Default Object serializer
 */
void vmObjectDumperDefault (Obj o, FILE *stream) {
 static Str p;
	fprintf (stream, "#<"HEX, o);
	if ((p = memPointerString(o))) fprintf (stream, ":%s", p);
	fprintf (stream, ">");
}

void (* vmObjectDumper)(Obj o, FILE *stream) = vmObjectDumperDefault;

Int vmOffsetToPosition (Obj codeBlock, Obj *instPtr) {
 Int pos = 3 + instPtr - (Obj*)codeBlock + (Int)*(instPtr+2) / 8;
	return (memObjectLength(codeBlock) < pos) ? (Int)*(instPtr+2) : pos;
}
Int vmBraOffsetToPosition (Obj codeBlock, Obj *instPtr) {
 Int pos = 2 + instPtr - (Obj*)codeBlock + (Int)*(instPtr+1) / 8;
	return (memObjectLength(codeBlock) < pos) ? (Int)*(instPtr+1) : pos;
}

void vmDebugDumpCode (Obj c, FILE *stream) {
 int fdState;
 Obj *i = c;
 Num lineNumber;

	DBBEG ("  "OBJ"  rcode:"OBJ"  rip:"OBJ, c, rcode, rip);

	/* Reenable i/o blocking */
	fcntl (0, F_SETFL, (fdState=fcntl(0, F_GETFL, 0))&~O_NONBLOCK);

	if (stream == NULL) stream=stderr;

	while (i < ((Obj*)c + memObjectLength(c))) { // Forcing pointer arithmetic.
		lineNumber = (Num)(i-(Obj*)c);
		fprintf (stream, NL OBJ STR HEX04" ",
			i,
			(i==rip || lineNumber==(Num)rip)?"*":" ",
			lineNumber);
		if      (*i==vmNOP)   {fprintf(stream, "nop");}
		else if (*i==vmMVI0)  {fprintf(stream, "mvi  $0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI1)  {fprintf(stream, "mvi  $1 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI2)  {fprintf(stream, "mvi  $2 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI3)  {fprintf(stream, "mvi  $3 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI4)  {fprintf(stream, "mvi  $4 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI5)  {fprintf(stream, "mvi  $5 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI6)  {fprintf(stream, "mvi  $6 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI7)  {fprintf(stream, "mvi  $7 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMV01)  {fprintf(stream, "mv   $0 $1 ");}
		else if (*i==vmMV02)  {fprintf(stream, "mv   $0 $2 ");}
		else if (*i==vmMV03)  {fprintf(stream, "mv   $0 $3 ");}
		else if (*i==vmMV04)  {fprintf(stream, "mv   $0 $4 ");}
		else if (*i==vmMV07)  {fprintf(stream, "mv   $0 $7 ");}
		else if (*i==vmMV01E) {fprintf(stream, "mv   $0 $1e ");}
		else if (*i==vmMV10)  {fprintf(stream, "mv   $1 $0 ");}
		else if (*i==vmMV13)  {fprintf(stream, "mv   $1 $3 ");}
		else if (*i==vmMV20)  {fprintf(stream, "mv   $2 $0 ");}
		else if (*i==vmMV23)  {fprintf(stream, "mv   $2 $3 ");}
		else if (*i==vmMV30)  {fprintf(stream, "mv   $3 $0 ");}
		else if (*i==vmMV50)  {fprintf(stream, "mv   $5 $0 ");}
		else if (*i==vmMV51C) {fprintf(stream, "mv   $5 $1c ");}
		else if (*i==vmMV518) {fprintf(stream, "mv   $5 $18 ");}
		else if (*i==vmMV1C0) {fprintf(stream, "mv   $1c $0 ");}
		else if (*i==vmMV1C18){fprintf(stream, "mv   $1c $18 ");}
		else if (*i==vmMV61)  {fprintf(stream, "mv   $6 $1 ");}
		else if (*i==vmMV72)  {fprintf(stream, "mv   $7 $2 ");}
		else if (*i==vmLDI00) {fprintf(stream, "ldi  $0 $0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI02) {fprintf(stream, "ldi  $0 $2 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI01C){fprintf(stream, "ldi  $0 $1c "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI11) {fprintf(stream, "ldi  $1 $1 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI11C){fprintf(stream, "ldi  $1 $1c "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI20) {fprintf(stream, "ldi  $2 $0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI22) {fprintf(stream, "ldi  $2 $2 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI50) {fprintf(stream, "ldi  $5 $0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI1C0){fprintf(stream, "ldi  $1c $0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLD012) {fprintf(stream, "ld0  $1 $2");}
		else if (*i==vmSTI01) {fprintf(stream, "sti  $0 $1 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI01C){fprintf(stream, "sti  $0 $1c "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI20) {fprintf(stream, "sti  $2 $0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI21) {fprintf(stream, "sti  $2 $1 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI30) {fprintf(stream, "sti  $3 $0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI40) {fprintf(stream, "sti  $4 $0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI50) {fprintf(stream, "sti  $5 $0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmST012) {fprintf(stream, "st   $0 $1 $2 ");}
		else if (*i==vmST201) {fprintf(stream, "st   $2 $0 $1 ");}
		else if (*i==vmPUSH0) {fprintf(stream, "push $0 ");}
		else if (*i==vmPUSH1) {fprintf(stream, "push $1 ");}
		else if (*i==vmPUSH2) {fprintf(stream, "push $2 ");}
		else if (*i==vmPUSH3) {fprintf(stream, "push $3 ");}
		else if (*i==vmPUSH4) {fprintf(stream, "push $4 ");}
		else if (*i==vmPUSH5) {fprintf(stream, "push $5 ");}
		else if (*i==vmPUSH7) {fprintf(stream, "push $7 ");}
		else if (*i==vmPUSH19){fprintf(stream, "push $19 ");}
		else if (*i==vmPUSH1A){fprintf(stream, "push $1a ");}
		else if (*i==vmPUSH1B){fprintf(stream, "push $1b ");}
		else if (*i==vmPOP0)  {fprintf(stream, "pop  $0 ");}
		else if (*i==vmPOP1)  {fprintf(stream, "pop  $1 ");}
		else if (*i==vmPOP2)  {fprintf(stream, "pop  $2 ");}
		else if (*i==vmPOP3)  {fprintf(stream, "pop  $3 ");}
		else if (*i==vmPOP4)  {fprintf(stream, "pop  $4 ");}
		else if (*i==vmPOP7)  {fprintf(stream, "pop  $7 ");}
		else if (*i==vmPOP19) {fprintf(stream, "pop  $19 ");}
		else if (*i==vmPOP1A) {fprintf(stream, "pop  $1a ");}
		else if (*i==vmPOP1B) {fprintf(stream, "pop  $1b ");}
		else if (*i==vmADDI0) {fprintf(stream, "addi $0 %ld", *(i+1)); i++; }
		else if (*i==vmADDI1) {fprintf(stream, "addi $1 %ld", *(i+1)); i++; }
		else if (*i==vmADDI2) {fprintf(stream, "addi $2 %ld", *(i+1)); i++; }
		else if (*i==vmADD10) {fprintf(stream, "add  $1 $0 "); }
		else if (*i==vmMUL10) {fprintf(stream, "mul  $1 $0 "); }

		else if (*i==vmBLTI1) {fprintf(stream, "blti $1 "HEX" "HEX04, *(i+1), vmOffsetToPosition(c, i)); i+=2;}
		else if (*i==vmBEQI0) {fprintf(stream, "beqi $0 "HEX" "HEX04, *(i+1), vmOffsetToPosition(c, i)); i+=2;}
		else if (*i==vmBEQI1) {fprintf(stream, "beqi $1 "HEX" "HEX04, *(i+1), vmOffsetToPosition(c, i)); i+=2;}
		else if (*i==vmBEQI7) {fprintf(stream, "beqi $7 "HEX" "HEX04, *(i+1), vmOffsetToPosition(c, i)); i+=2;}
		else if (*i==vmBNEI0) {fprintf(stream, "bnei $0 "HEX" "HEX04, *(i+1), vmOffsetToPosition(c, i)); i+=2;}
		else if (*i==vmBNEI1) {fprintf(stream, "bnei $1 "HEX" "HEX04, *(i+1), vmOffsetToPosition(c, i)); i+=2;}
		else if (*i==vmBNEI2) {fprintf(stream, "bnei $2 "HEX" "HEX04, *(i+1), vmOffsetToPosition(c, i)); i+=2;}
		else if (*i==vmBNEI5) {fprintf(stream, "bnei $5 "HEX" "HEX04, *(i+1), vmOffsetToPosition(c, i)); i+=2;}
		else if (*i==vmBRTI0) {fprintf(stream, "brti $0 "HEX" "HEX04, *(i+1), vmOffsetToPosition(c, i)); i+=2;}
		else if (*i==vmBNTI0) {fprintf(stream, "bnti $0 "HEX" "HEX04, *(i+1), vmOffsetToPosition(c, i)); i+=2;}
		else if (*i==vmBRA)   {fprintf(stream, "bra "HEX04, vmBraOffsetToPosition(c, i)); i++;}

		else if (*i==vmJ0)    {fprintf(stream, "j    $0 ");}
		else if (*i==vmJ2)    {fprintf(stream, "j    $2 ");}
		else if (*i==vmJAL0)  {fprintf(stream, "jal  $0 ");}
		else if (*i==vmJAL2)  {fprintf(stream, "jal  $2 ");}
		else if (*i==vmRET)   {fprintf(stream, "ret");}
		else if (*i==vmSYSI)  {fprintf(stream, "sysi "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSYS0)  {fprintf(stream, "sys  $0 ");}
		else if (*i==vmQUIT)  {fprintf(stream, "quit");}
		else {
			//fprintf(stream, HEX" = ", *i);
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
		DB("Activating module...");
		shouldInitialize=0;
		memInitialize(0, 0);
		DB("Create 'registers'");
		memRootSetRegister(r0);  memRootSetRegister(r1);  memRootSetRegister(r2);  memRootSetRegister(r3);
		memRootSetRegister(r4);  memRootSetRegister(r5);  memRootSetRegister(r6);  memRootSetRegister(r7);
		memRootSetRegister(r8);  memRootSetRegister(r9);  memRootSetRegister(ra);  memRootSetRegister(rb);
		memRootSetRegister(rc);  memRootSetRegister(rd);  memRootSetRegister(re);  memRootSetRegister(rf);
		memRootSetRegister(r10); memRootSetRegister(r11); memRootSetRegister(r12); memRootSetRegister(r13);
		memRootSetRegister(r14); memRootSetRegister(r15); memRootSetRegister(r16); memRootSetRegister(r17);
		memRootSetRegister(r18); memRootSetRegister(r19); memRootSetRegister(r1a); memRootSetRegister(r1b);
		memRootSetRegister(r1c); memRootSetRegister(r1d); memRootSetRegister(r1e); memRootSetRegister(r1f);
		DB("Create the stack");
		r1f = memNewStack();
		DB("Register the internal object types");
		memTypeRegisterString (TCODE, "code");
		DB("Initialize opcode values");
		vmVm(); /* The first call to vmVm() initializes opcode values */
	} else {
		DB("Module already activated");
	}

	if (interruptHandler) {
		DB("Setting interrupt handler callback function");
		assert(!vmInterruptHandler);
		vmInterruptHandler = interruptHandler; /* sys.c:sysSchedule()  */
	}
	if (vmObjDumper) {
		DB("Setting vmObjDumper callback function");
		assert(vmObjectDumperDefault == vmObjectDumper); /* Verify the object dump callback is changed once */
		vmObjectDumper = vmObjDumper;
	}
	DBEND();
}


#undef DB_DESC
#undef DEBUG
