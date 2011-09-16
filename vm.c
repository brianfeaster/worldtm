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
     *vmADDI0, *vmADDI1, *vmADD10, *vmMUL10,
     *vmBLTI1,
     *vmBEQI0, *vmBEQI1, *vmBEQI7, *vmBNEI0, *vmBNEI1, *vmBNEI5, *vmBRTI0, *vmBNTI0, *vmBRA,
     *vmJ0, *vmJ2, *vmJAL0, *vmJAL2, *vmRET,
     *vmSYSI, *vmSYS0, *vmQUIT;

#define VM_INIT 0
#define VM_RUN  1
void vmVm (Int cmd) {
	/* Initialize the opcodes.  Really we're just assigning a bunch of global
		label pointers the jump addresses of all the opcode implementations in
		this function. */
	if (cmd == VM_INIT) {
		DBBEG("Initializing opcode values");
		vmNOP=&&nop;

		vmMVI0=&&mvi0;   vmMVI1=&&mvi1;   vmMVI2=&&mvi2;  vmMVI3=&&mvi3; vmMVI4=&&mvi4;
		vmMVI5=&&mvi5;   vmMVI6=&&mvi6;   vmMVI7=&&mvi7;
		vmMV01=&&mv01;   vmMV02=&&mv02;   vmMV03=&&mv03;  vmMV04=&&mv04; vmMV07=&&mv07;
		vmMV01E=&&mv01e;
		vmMV10=&&mv10;   vmMV13=&&mv13;  vmMV20=&&mv20;   vmMV23=&&mv23;
		vmMV30=&&mv30;   vmMV51C=&&mv51c; vmMV518=&&mv518; vmMV50=&&mv50;
		vmMV1C0=&&mv1c0; vmMV1C18=&&mv1c18;
		vmMV61=&&mv61;   vmMV72=&&mv72;

		vmLDI00=&&ldi00;   vmLDI02=&&ldi02;   vmLDI01C=&&ldi01c; vmLDI11=&&ldi11;
      vmLDI11C=&&ldi11c; vmLDI20=&&ldi20;  vmLDI22=&&ldi22;
      vmLDI50=&&ldi50;   vmLDI1C0=&&ldi1c0;

      vmLD012=&&ld012; vmSTI01=&&sti01; vmSTI01C=&&sti01c; vmSTI20=&&sti20;
      vmSTI21=&&sti21;
      vmSTI30=&&sti30; vmSTI40=&&sti40; vmSTI50=&&sti50; vmST012=&&st012;
      vmST201=&&st201;

		vmPUSH0=&&push0; vmPUSH1=&&push1;   vmPUSH2=&&push2;   vmPUSH3=&&push3;
		vmPUSH4=&&push4; vmPUSH5=&&push5;   vmPUSH7=&&push7;   vmPUSH19=&&push19;
      vmPUSH1A=&&push1a; vmPUSH1B=&&push1b;

		vmPOP0=&&pop0;    vmPOP1=&&pop1;   vmPOP2=&&pop2;   vmPOP3=&&pop3;  vmPOP4=&&pop4;  vmPOP7=&&pop7;
		vmPOP19=&&pop19;  vmPOP1A=&&pop1a; vmPOP1B=&&pop1b;

		vmADDI0=&&addi0; vmADDI1=&&addi1; vmADD10=&&add10; vmMUL10=&&mul10;

		vmBLTI1=&&blti1;
		vmBEQI0=&&beqi0; vmBEQI1=&&beqi1; vmBEQI7=&&beqi7;
		vmBNEI0=&&bnei0; vmBNEI1=&&bnei1; vmBNEI5=&&bnei5;
		vmBRTI0=&&brti0; vmBNTI0=&&bnti0;
		vmBRA=&&bra;

		vmJ0=&&j0;      vmJ2=&&j2;
		vmJAL0=&&jal0;  vmJAL2=&&jal2;
		vmRET=&&ret;

		vmSYSI=&&sysi;   vmSYS0=&&sys0;   vmQUIT=&&quit;

		//TODO move this to the module that handles object dumps
		memPointerRegister(vmNOP);
		memPointerRegister(vmMVI0); memPointerRegister(vmMVI1); memPointerRegister(vmMVI2); memPointerRegister(vmMVI3);
		memPointerRegister(vmMVI4); memPointerRegister(vmMVI5); memPointerRegister(vmMVI6); memPointerRegister(vmMVI7);
		memPointerRegister(vmMV01); memPointerRegister(vmMV02); memPointerRegister(vmMV03); memPointerRegister(vmMV04);
		memPointerRegister(vmMV07); memPointerRegister(vmMV01E); memPointerRegister(vmMV10);
		memPointerRegister(vmMV13); memPointerRegister(vmMV20); memPointerRegister(vmMV23);
		memPointerRegister(vmMV30); memPointerRegister(vmMV51C); memPointerRegister(vmMV518);
		memPointerRegister(vmMV50);
		memPointerRegister(vmMV1C0); memPointerRegister(vmMV1C18); memPointerRegister(vmMV61); memPointerRegister(vmMV72);
		memPointerRegister(vmLDI00); memPointerRegister(vmLDI02); memPointerRegister(vmLDI01C);
		memPointerRegister(vmLDI11); memPointerRegister(vmLDI11C); memPointerRegister(vmLDI20); memPointerRegister(vmLDI22);
		memPointerRegister(vmLDI50); memPointerRegister(vmLDI1C0);
		memPointerRegister(vmLD012);
		memPointerRegister(vmSTI01); memPointerRegister(vmSTI01C); memPointerRegister(vmSTI20); memPointerRegister(vmSTI21);
		memPointerRegister(vmSTI30); memPointerRegister(vmSTI40); memPointerRegister(vmSTI50); memPointerRegister(vmST012);
		memPointerRegister(vmST201);
		memPointerRegister(vmPUSH0); memPointerRegister(vmPUSH1); memPointerRegister(vmPUSH2); memPointerRegister(vmPUSH3);
		memPointerRegister(vmPUSH4); memPointerRegister(vmPUSH5); memPointerRegister(vmPUSH7); memPointerRegister(vmPUSH19);
		memPointerRegister(vmPUSH1A); memPointerRegister(vmPUSH1B);
		memPointerRegister(vmPOP0); memPointerRegister(vmPOP1); memPointerRegister(vmPOP2); memPointerRegister(vmPOP3);
		memPointerRegister(vmPOP4); memPointerRegister(vmPOP7); memPointerRegister(vmPOP19);
		memPointerRegister(vmPOP1A); memPointerRegister(vmPOP1B);
		memPointerRegister(vmADDI0); memPointerRegister(vmADDI1); memPointerRegister(vmADD10);
		memPointerRegister(vmMUL10);
		memPointerRegister(vmBLTI1);
		memPointerRegister(vmBEQI0); memPointerRegister(vmBEQI1); memPointerRegister(vmBEQI7);
		memPointerRegister(vmBNEI0); memPointerRegister(vmBNEI1); memPointerRegister(vmBNEI5);
		memPointerRegister(vmBRTI0);
		memPointerRegister(vmBNTI0);
		memPointerRegister(vmBRA);
		memPointerRegister(vmJ0); memPointerRegister(vmJ2);
		memPointerRegister(vmJAL0); memPointerRegister(vmJAL2);
		memPointerRegister(vmRET);
		memPointerRegister(vmSYSI); memPointerRegister(vmSYS0);
		memPointerRegister(vmQUIT);

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
	fprintf (stream, "#<"HEX, o);
	if ((p = memPointerString(o))) fprintf (stream, ":%s", p);
	fprintf (stream, ">");
}

void (* vmObjectDumper)(Obj o, FILE *stream) = vmObjectDumperDefault;

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
		else if (*i==vmMVI0)  {fprintf(stream, "mvi_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI1)  {fprintf(stream, "mvi_1 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI2)  {fprintf(stream, "mvi_2 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI3)  {fprintf(stream, "mvi_3 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI4)  {fprintf(stream, "mvi_4 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI5)  {fprintf(stream, "mvi_5 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI6)  {fprintf(stream, "mvi_6 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMVI7)  {fprintf(stream, "mvi_7 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmMV01)  {fprintf(stream, "mv_0_1 ");}
		else if (*i==vmMV02)  {fprintf(stream, "mv_0_2 ");}
		else if (*i==vmMV03)  {fprintf(stream, "mv_0_3 ");}
		else if (*i==vmMV04)  {fprintf(stream, "mv_0_4 ");}
		else if (*i==vmMV07)  {fprintf(stream, "mv_0_7 ");}
		else if (*i==vmMV01E) {fprintf(stream, "mv_0_1e ");}
		else if (*i==vmMV10)  {fprintf(stream, "mv_1_0 ");}
		else if (*i==vmMV13)  {fprintf(stream, "mv_1_3 ");}
		else if (*i==vmMV20)  {fprintf(stream, "mv_2_0 ");}
		else if (*i==vmMV23)  {fprintf(stream, "mv_2_3 ");}
		else if (*i==vmMV30)  {fprintf(stream, "mv_3_0 ");}
		else if (*i==vmMV50)  {fprintf(stream, "mv_5_0 ");}
		else if (*i==vmMV51C) {fprintf(stream, "mv_5_1c ");}
		else if (*i==vmMV518) {fprintf(stream, "mv_5_18 ");}
		else if (*i==vmMV1C0) {fprintf(stream, "mv_1c_0 ");}
		else if (*i==vmMV1C18) {fprintf(stream, "mv_1c_18 ");}
		else if (*i==vmMV61) {fprintf(stream, "mv_6_1 ");}
		else if (*i==vmMV72) {fprintf(stream, "mv_7_2 ");}
		else if (*i==vmLDI00) {fprintf(stream, "ldi_0_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI02) {fprintf(stream, "ldi_0_2 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI01C){fprintf(stream, "ldi_0_1c "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI11) {fprintf(stream, "ldi_1_1 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI11C){fprintf(stream, "ldi_1_1c "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI20) {fprintf(stream, "ldi_2_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI22) {fprintf(stream, "ldi_2_2 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI50) {fprintf(stream, "ldi_5_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLDI1C0){fprintf(stream, "ldi_1c_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmLD012) {fprintf(stream, "ld0_1_2");}
		else if (*i==vmSTI01) {fprintf(stream, "sti_0_1 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI01C){fprintf(stream, "sti_0_1c "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI20) {fprintf(stream, "sti_2_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI21) {fprintf(stream, "sti_2_1 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI30) {fprintf(stream, "sti_3_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI40) {fprintf(stream, "sti_4_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSTI50) {fprintf(stream, "sti_5_0 "); vmObjectDumper(*++i, stream);}
		else if (*i==vmST012) {fprintf(stream, "st_0_1_2 ");}
		else if (*i==vmST201) {fprintf(stream, "st_2_0_1 ");}
		else if (*i==vmPUSH0) {fprintf(stream, "push_0 ");}
		else if (*i==vmPUSH1) {fprintf(stream, "push_1 ");}
		else if (*i==vmPUSH2) {fprintf(stream, "push_2 ");}
		else if (*i==vmPUSH3) {fprintf(stream, "push_3 ");}
		else if (*i==vmPUSH4) {fprintf(stream, "push_4 ");}
		else if (*i==vmPUSH5) {fprintf(stream, "push_5 ");}
		else if (*i==vmPUSH7) {fprintf(stream, "push_7 ");}
		else if (*i==vmPUSH19){fprintf(stream, "push_19 ");}
		else if (*i==vmPUSH1A){fprintf(stream, "push_1a ");}
		else if (*i==vmPUSH1B){fprintf(stream, "push_1b ");}
		else if (*i==vmPOP0)  {fprintf(stream, "pop_0 ");}
		else if (*i==vmPOP1)  {fprintf(stream, "pop_1 ");}
		else if (*i==vmPOP2)  {fprintf(stream, "pop_2 ");}
		else if (*i==vmPOP3)  {fprintf(stream, "pop_3 ");}
		else if (*i==vmPOP4)  {fprintf(stream, "pop_4 ");}
		else if (*i==vmPOP7)  {fprintf(stream, "pop_7 ");}
		else if (*i==vmPOP19) {fprintf(stream, "pop_19 ");}
		else if (*i==vmPOP1A) {fprintf(stream, "pop_1a ");}
		else if (*i==vmPOP1B) {fprintf(stream, "pop_1b ");}
		else if (*i==vmADDI0) {fprintf(stream, "addi_0 %d", *(i+1)); i++; }
		else if (*i==vmADDI1) {fprintf(stream, "addi_1 %d", *(i+1)); i++; }
		else if (*i==vmADD10) {fprintf(stream, "add_1_0 "); }
		else if (*i==vmMUL10) {fprintf(stream, "mul_1_0 "); }
		else if (*i==vmBLTI1) {fprintf(stream, "blti_1 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==vmBEQI0) {fprintf(stream, "beqi_0 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==vmBEQI1) {fprintf(stream, "beqi_1 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==vmBEQI7) {fprintf(stream, "beqi_7 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==vmBNEI0) {fprintf(stream, "bnei_0 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==vmBNEI1) {fprintf(stream, "bnei_1 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==vmBNEI5) {fprintf(stream, "bnei_5 ");
									vmObjectDumper(*(i+1), stream);
									fprintf(stream, " %04x", 3+i-(Obj*)c+(Int)*(i+2)/8);
									i+=2;}
		else if (*i==vmBRTI0) {fprintf (stream, "brti_0 %x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==vmBNTI0) {fprintf (stream, "bnti_0 %08x %04x",
									 *(i+1), 3+i-(Obj*)c+(Int)*(i+2)/8); i+=2;}
		else if (*i==vmBRA)   {fprintf (stream, "bra %04x",
									 2+i-(Obj*)c+(Int)*(i+1)/8); i++;}
		else if (*i==vmJ0)    {fprintf (stream, "j_0 ");}
		else if (*i==vmJ2)    {fprintf (stream, "j_2 ");}
		else if (*i==vmJAL0)  {fprintf (stream, "jal_0 ");}
		else if (*i==vmJAL2)  {fprintf (stream, "jal_2 ");}
		else if (*i==vmRET)   {fprintf (stream, "ret ");}
		else if (*i==vmSYSI)  {fprintf (stream, "sysi "); vmObjectDumper(*++i, stream);}
		else if (*i==vmSYS0)  {fprintf (stream, "sys_0 ");}
		else if (*i==vmQUIT)  {fprintf (stream, "quit ");}
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
		DB("  Activating module...");
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
		vmVm(VM_INIT); /* Opcode values are really C goto addresses */
	} else {
		DB("  Module already activated");
	}

	if (interruptHandler) {
		DB("  Setting interrupt handler callback function");
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
