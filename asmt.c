#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include "asm.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"
#include "test.h"


extern void asmIBlockDefaultTagSet (Obj ib, Obj tag);
extern void asmIBlockConditionalTagSet (Obj ib, Obj tag);
extern Num asmGenerateNewIBlock (Num icodeSize);

extern void asmICodePushNewMV (Obj rega, Obj regb);
extern void asmICodePushNewMVI (Obj r, Obj o);
extern void asmICodePushNewLDI (Obj rega, Obj regb, Obj o);
extern void asmICodePushNewPUSH (Obj o);
extern void asmICodePushNewPOP (Obj r);
extern void asmICodePushNewADDI (Obj r, Obj o);
extern void asmICodePushNewBEQI (Obj rega, Obj imm, Obj o);
extern void asmICodePushNewBNEI (Obj r, Obj i, Obj o);
extern void asmICodePushNewBRTI (Obj rega, Obj imm, Obj o);
extern void asmICodePushNewBRA (Obj o);
extern void asmICodePushNewSYSI (Obj o);
extern void asmICodePushNewNOP (void);
extern void asmICodePushNewQUIT (void);

void asmGenerateIBlockWithPushedIcodes ();


/*******************************************************************************
 TESTS
*******************************************************************************/
void cctDumpSpace (void) { fprintf(FB, " "); }
void cctDumpNewline (void) { fprintf(FB, "\n"); }
void cctDumpIntegerR0 (void) { fprintf(FB, INT, r0); }
void cctDumpIntegerR0stderr (void) { fprintf(stderr, INT, r0); }
void cctDumpObjR0 (void) { objDump(r0, FB); }

/* Create simple icode program of one iblock then compile the
   iblock and run the code block in the VM
*/
void test1 (void) {
	asmInit();
	FBInit();

	/* Head iblock of the igraph */
	asmICodePushNewNOP();
	asmGenerateIBlockWithPushedIcodes();

	asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */

	/* Create a new iblock as the default block to the iblock specified by
	   the first parameter.
	   Need to know beforehand how many instructions will be emitted.  Pass in
	   parent iblock ID (initially 0) and number of icode objects to emit.
	 */
	/* Emit icode objects by combining instruction's fields */
	asmICodePushNewMVI(R0, (Obj)69);
	asmICodePushNewSYSI(cctDumpIntegerR0);
	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();

	asmAssemble();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	FBFinalize("69");
}


/* Creates three linked iblocks with the 2nd containing a conditional
   branch to itself.  The program counts from 10 to 0.
*/
void test2 (void) {
	asmInit();
	FBInit();

	/* Head iblock of the igraph */
	asmICodePushNewNOP();
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */

	asmICodePushNewMVI(R0, (Obj)10);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */

	asmICodePushNewSYSI((Obj)cctDumpIntegerR0);
	asmICodePushNewSYSI((Obj)cctDumpSpace);
	asmICodePushNewADDI(R0, (Obj)-1);
	asmICodePushNewBNEI(R0, 0, ofalse);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */
	asmIBlockConditionalTagSet(riblock, riblock); /* Set conditional block to self */

	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();

	asmAssemble(); /* rcode/r1e */
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	FBFinalize("10 9 8 7 6 5 4 3 2 1 ");
}


/* Compile a mutli iblock program with an overlapping branch
   to a prior iblock.  Counts from 5 to 1, 4 to 1, ... 1 to 1
*/
void test3 (void) {
	asmInit();
	FBInit();

	/* Head iblock of the igraph */
	asmICodePushNewNOP();
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */

	asmICodePushNewMVI(R1, (Obj)5);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);

	asmICodePushNewSYSI((Obj)cctDumpSpace);
	asmICodePushNewMV(R0, R1);
	asmGenerateIBlockWithPushedIcodes();
	r3 = riblock; /* Keep track of this iblock so we can link to it from an iblock below */
	asmIBlockDefaultTagSet(riblock, otrue);

	asmICodePushNewSYSI((Obj)cctDumpIntegerR0);
	asmICodePushNewSYSI((Obj)cctDumpSpace);
	asmICodePushNewADDI(R0, (Obj)-1);
	asmICodePushNewBNEI(R0, 0, ofalse);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);
	asmIBlockConditionalTagSet(riblock, riblock); /* Set this iblock's conditional link to self */

	asmICodePushNewADDI(R1, (Obj)-1);
	asmICodePushNewBNEI(R1, 0, ofalse); /* Jump back to another iblock */
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);
	asmIBlockConditionalTagSet(riblock, r3); /* Set this iblock's conditional link to self */

	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();

	asmAssemble(); /* rcode/r1e */
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	FBFinalize(" 5 4 3 2 1  4 3 2 1  3 2 1  2 1  1 ");
}


/* Compile a mutli iblock program with a branch to a forward
   iblock.
*/
void test4 (void) {
	asmInit();
	FBInit();

	asmICodePushNewNOP();
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);

	// 4
	asmICodePushNewMVI(R1, (Obj)9);
	asmICodePushNewMV(R0, R1);
	asmICodePushNewBNEI(R0, 0, ofalse); /* Branch forward to 7 for fun which just branches back to 5 */
	asmGenerateIBlockWithPushedIcodes();
	r4 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);

	// 5
	asmICodePushNewSYSI((Obj)cctDumpSpace);
	asmGenerateIBlockWithPushedIcodes();
	r5 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);

	asmICodePushNewSYSI((Obj)cctDumpIntegerR0);
	asmICodePushNewSYSI((Obj)cctDumpSpace);
	asmICodePushNewADDI(R0, (Obj)-1);
	asmICodePushNewBNEI(R0, 0, ofalse); /* Loop to self */
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);
	asmIBlockConditionalTagSet(riblock, riblock);

	// 6
	asmICodePushNewBNEI(R0, 0, ofalse); /* Branch to 8 and quit */
	asmGenerateIBlockWithPushedIcodes();
	r6 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);

	// 7
	asmICodePushNewBNEI(R0, 0, ofalse); /* Branch back to 5 */
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);
	asmIBlockConditionalTagSet(r4, riblock);
	asmIBlockConditionalTagSet(riblock, r5);

	// 8
	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockConditionalTagSet(r6, riblock);

	asmAssemble();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	FBFinalize(" 9 8 7 6 5 4 3 2 1 ");
}


/* Igraph representing two conditional's sharing the same target
*/
void test5 (void) {
	asmInit();
	FBInit();

	asmICodePushNewMVI(R0, (Obj)9);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);

	// id2
	asmICodePushNewBEQI(R0, (Obj)1, ofalse); /* Branch to id4  */
	asmGenerateIBlockWithPushedIcodes();
	r4 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);

	// id3
	asmICodePushNewBEQI(R0, (Obj)2, ofalse);
	asmGenerateIBlockWithPushedIcodes();
	r5 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);

	// id5
	asmICodePushNewSYSI((Obj)cctDumpIntegerR0);
	asmICodePushNewADDI(R0, (Obj)-1);
	asmICodePushNewBNEI(R0, 0, ofalse); /* Branch back to id2 */
	asmGenerateIBlockWithPushedIcodes();
	r6 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);
	asmIBlockConditionalTagSet(riblock, r4);

	// id6
	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);

	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();

	// id4
	asmICodePushNewSYSI((Obj)cctDumpIntegerR0);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockConditionalTagSet(r4, riblock);
	asmIBlockConditionalTagSet(r5, riblock);
	asmIBlockDefaultTagSet(riblock, r6);

	asmAssemble();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	FBFinalize("98765432211");
}


/* How I'd like to write an assembly program.  asmAsm will generate
   a new igraph attached to the last specified (or generated) iblock
   in the global igraph.
*/
void cctAsm() {
 Obj L0, L1, L2, L3, L4, L5, L6;

	asmInit();
	FBInit();

	L0 = asmNewLabel();
	L1 = asmNewLabel();
	L2 = asmNewLabel();
	L3 = asmNewLabel();

	asmAsm (
		MVI, R2, (Obj)3,
		MVI, R1, (Obj)3,
	LABEL, L1,
		MVI, R0, (Obj)8,
	LABEL, L2,
		SYSI, cctDumpIntegerR0,
		ADDI, R0, -1l,
		BEQI, R0, 0l, L3,
		BRA, L2,
	LABEL, L3,
		SYSI, cctDumpNewline,
		ADDI, R1, -1l,
		BNEI, R1, 0l, L1
	);

	L4 = asmNewLabel();
	L5 = asmNewLabel();
	L6 = asmNewLabel();

	asmAsm (
	LABEL, L4,
		MVI, R0, ofalse,
		BNEI, R0, otrue, L5,
		MVI, R0, ofalse,
		BRA, L6,
	LABEL, L5,
		MVI, R0, otrue,
	LABEL, L6,
		SYSI, cctDumpObjR0,
		MVI, R1, (Obj)2,
		ADDI, R2, -1l,
		BNEI, R2, 0l, L1,
		QUIT);
	
	asmAssemble();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	FBFinalize("87654321\n"
	           "87654321\n"
	           "87654321\n"
	           "#t87654321\n"
	           "87654321\n"
	           "#t87654321\n"
	           "87654321\n#t");
}


void cctAsmNested() {
 Obj L0, L1;

	FBInit();

	asmInit();
	L0 = asmNewLabel();
	asmAsm (
		MVI, R0, (Obj)9,
	LABEL, L0,
		SYSI, cctDumpIntegerR0,
		ADDI, R0, -1l,
		BNEI, R0, 0l, L0,
		SYSI, cctDumpNewline,
		QUIT
	);

		/* New ASM context */
		asmStart();
		L1 = asmNewLabel();
		asmAsm (
			MVI, R0, (Obj)10,
		LABEL, L1,
			SYSI, cctDumpIntegerR0,
			ADDI, R0, -2l,
			BNEI, R0, 0l, L1,
			SYSI, cctDumpNewline,
			QUIT
		);
		asmAssemble();
		rcode = r0;
		rip = 0;
		vmRun();

	/* Previous ASM context restored by asmAssemble() */
	asmAssemble();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	FBFinalize("108642\n987654321\n");

//asmDumpIBlocks();
//vmDebugDumpCode(rcode, stderr);
}


void syscallAdd (void) {
 Int sum=0;
	while (r1--) sum += *(Int*)vmPop();
	objNewInt(sum);
}

void syscallMul (void) {
 Int product=1;
	while (r1--) product *= *(Int*)vmPop();
	objNewInt(product);
}

void syscallNewIntImm (void) {
	objNewInt((Int)r0);
}

void cctSysCall (void) {

	FBInit();

	asmInit();
	asmAsm (
		MVI, R0, (Obj)6,
		SYSI, syscallNewIntImm,
		PUSH, R0,
		MVI, R0, (Obj)9,
		SYSI, syscallNewIntImm,
		PUSH, R0,
		MVI, R1, (Obj)2,
		SYSI, syscallAdd,
		SYSI, cctDumpObjR0,
		QUIT
	);
	asmAssemble();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	FBFinalize("15");
}

void cctJumpAndLink (void) {
	FBInit();
	asmInit();

	asmAsm (
		SYSI, cctDumpIntegerR0,
		RET
	);
	asmAssemble();
	rcode = r0;
	r7 = rcode; /* Keep track of this block in r7 */

	asmInit();
	asmAsm (
		MVI, R2, r7,
		MVI, R0, 0,
		JAL, R2,
		ADDI, R0, 1l,
		JAL, R2,
		QUIT
	);

	asmAssemble();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
//vmDebugDumpCode(rcode, stderr);
//fflush(FB);
//fprintf(stderr, "[%s]", fpBuff);
	FBFinalize("01");
}


/* Verify opcodes and their specific fields can be assembled
*/
void opcodes (void) {
 Obj L;

	/* MV */
	asmInit(); asmAsm(MV, R0, R1); asmAssemble(); assert(vmMV01 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R0, R3); asmAssemble(); assert(vmMV03 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R0, R4); asmAssemble(); assert(vmMV04 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R0, RE); asmAssemble(); assert(vmMV0E == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R1, R0); asmAssemble(); assert(vmMV10 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R1, R3); asmAssemble(); assert(vmMV13 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R2, R0); asmAssemble(); assert(vmMV20 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R3, R0); asmAssemble(); assert(vmMV30 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R5, R0); asmAssemble(); assert(vmMV50 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R5, R8); asmAssemble(); assert(vmMV58 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R5, RC); asmAssemble(); assert(vmMV5C == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, RC, R0); asmAssemble(); assert(vmMVC0 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, RC, R5); asmAssemble(); assert(vmMVC5 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, RC, R8); asmAssemble(); assert(vmMVC8 == memVectorObject(r0, 0));

	/* MVI */
	asmInit(); asmAsm(MVI, R0, 99); asmAssemble(); assert(vmMVI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(MVI, R1, 98); asmAssemble(); assert(vmMVI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(MVI, R2, 97); asmAssemble(); assert(vmMVI2 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(MVI, R3, 96); asmAssemble(); assert(vmMVI3 == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(MVI, R4, 95); asmAssemble(); assert(vmMVI4 == memVectorObject(r0, 0)); assert(95 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(MVI, R5, 95); asmAssemble(); assert(vmMVI5 == memVectorObject(r0, 0)); assert(95 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(MVI, R6, 94); asmAssemble(); assert(vmMVI6 == memVectorObject(r0, 0)); assert(94 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(MVI, R7, 93); asmAssemble(); assert(vmMVI7 == memVectorObject(r0, 0)); assert(93 == (Num)memVectorObject(r0, 1));

	/* LDI */
	asmInit(); asmAsm(LDI, R0, R0, 99); asmAssemble(); assert(vmLDI00 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R0, R2, 99); asmAssemble(); assert(vmLDI02 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R0, RC, 98); asmAssemble(); assert(vmLDI0C == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R1, R1, 97); asmAssemble(); assert(vmLDI11 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R1, RC, 96); asmAssemble(); assert(vmLDI1C == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R2, R0, 95); asmAssemble(); assert(vmLDI20 == memVectorObject(r0, 0)); assert(95 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R2, R2, 94); asmAssemble(); assert(vmLDI22 == memVectorObject(r0, 0)); assert(94 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R5, R0, 93); asmAssemble(); assert(vmLDI50 == memVectorObject(r0, 0)); assert(93 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, RC, R0, 92); asmAssemble(); assert(vmLDIC0 == memVectorObject(r0, 0)); assert(92 == (Num)memVectorObject(r0, 1)); 

	/* LD */
	asmInit(); asmAsm(LD, R0, R1, R2); asmAssemble(); assert(vmLD012 == memVectorObject(r0, 0));

	/* STI */
	asmInit(); asmAsm(STI, R0, R1, 99); asmAssemble(); assert(vmSTI01 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(STI, R0, R5, 99); asmAssemble(); assert(vmSTI05 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(STI, R0, RC, 98); asmAssemble(); assert(vmSTI0C == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(STI, R2, R0, 97); asmAssemble(); assert(vmSTI20 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(STI, R2, R1, 96); asmAssemble(); assert(vmSTI21 == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(STI, R3, R0, 95); asmAssemble(); assert(vmSTI30 == memVectorObject(r0, 0)); assert(95 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(STI, R5, R0, 94); asmAssemble(); assert(vmSTI50 == memVectorObject(r0, 0)); assert(94 == (Num)memVectorObject(r0, 1));

	/* ST */
	asmInit(); asmAsm(ST, R0, R1, R2); asmAssemble(); assert(vmST012 == memVectorObject(r0, 0));

	/* PUSH */
	asmInit(); asmAsm(PUSH, R0); asmAssemble(); assert(vmPUSH0  == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, R1); asmAssemble(); assert(vmPUSH1  == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, R2); asmAssemble(); assert(vmPUSH2  == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, R4); asmAssemble(); assert(vmPUSH4  == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, R5); asmAssemble(); assert(vmPUSH5  == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, R7); asmAssemble(); assert(vmPUSH7  == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, R9); asmAssemble(); assert(vmPUSH9 == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, RA); asmAssemble(); assert(vmPUSHA == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, RB); asmAssemble(); assert(vmPUSHB == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, RC); asmAssemble(); assert(vmPUSHC == memVectorObject(r0, 0));

	/* POP */
	asmInit(); asmAsm(POP, R0);  asmAssemble(); assert(vmPOP0  == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, R1);  asmAssemble(); assert(vmPOP1  == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, R2);  asmAssemble(); assert(vmPOP2  == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, R3);  asmAssemble(); assert(vmPOP3  == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, R4);  asmAssemble(); assert(vmPOP4  == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, R7);  asmAssemble(); assert(vmPOP7  == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, R9); asmAssemble(); assert(vmPOP9 == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, RA); asmAssemble(); assert(vmPOPA == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, RB); asmAssemble(); assert(vmPOPB == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, RC); asmAssemble(); assert(vmPOPC == memVectorObject(r0, 0));

	/* ADDI */
	asmInit(); asmAsm(ADDI, R0, 99);  asmAssemble(); assert(vmADDI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(ADDI, R1, 98);  asmAssemble(); assert(vmADDI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(ADDI, R2, 97);  asmAssemble(); assert(vmADDI2 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));

	/* BLTI */
	asmInit(); L=asmNewLabel(); asmAsm(BLTI, R1, 98, L, LABEL, L, NOP); asmAssemble(); assert(vmBLTI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BEQI */
	asmInit(); L=asmNewLabel(); asmAsm(BEQI, R0, 99, L, LABEL, L, NOP); asmAssemble(); assert(vmBEQI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BEQI, R1, 98, L, LABEL, L, NOP); asmAssemble(); assert(vmBEQI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BEQI, R7, 97, L, LABEL, L, NOP); asmAssemble(); assert(vmBEQI7 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BNEI */
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R0, 99, L, LABEL, L, NOP); asmAssemble(); assert(vmBNEI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R1, 98, L, LABEL, L, NOP); asmAssemble(); assert(vmBNEI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R2, 97, L, LABEL, L, NOP); asmAssemble(); assert(vmBNEI2 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R5, 96, L, LABEL, L, NOP); asmAssemble(); assert(vmBNEI5 == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BRTI */
	asmInit(); L=asmNewLabel(); asmAsm(BRTI, R0, 99, L, LABEL, L, NOP); asmAssemble(); assert(vmBRTI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BNTI */
	asmInit(); L=asmNewLabel(); asmAsm(BNTI, R0, 99, L, LABEL, L, NOP); asmAssemble(); assert(vmBNTI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BRA */
	asmInit(); L=asmNewLabel(); asmAsm(LABEL, L, BRA, L); asmAssemble(); assert(vmBRA == memVectorObject(r0, 0)); assert(-2*8 == (Int)memVectorObject(r0, 1));

	/* JMP */
	asmInit(); L=asmNewLabel(); asmAsm(JMP, R0); asmAssemble(); assert(vmJMP0 == memVectorObject(r0, 0));
	asmInit(); L=asmNewLabel(); asmAsm(JMP, R2); asmAssemble(); assert(vmJMP2 == memVectorObject(r0, 0));

	/* JAL */
	asmInit(); L=asmNewLabel(); asmAsm(JAL, R0, NOP); asmAssemble(); assert(vmJAL0 == memVectorObject(r0, 0));
	asmInit(); L=asmNewLabel(); asmAsm(JAL, R2, NOP); asmAssemble(); assert(vmJAL2 == memVectorObject(r0, 0));

	/* RET */
	asmInit(); asmAsm(RET, NOP); asmAssemble(); assert(vmRET == memVectorObject(r0, 0));

	/* SYS */
	asmInit(); asmAsm(SYS, R0, NOP); asmAssemble(); assert(vmSYS0 == memVectorObject(r0, 0));

	/* SYSI */
	asmInit(); asmAsm(SYSI, 99, NOP); asmAssemble(); assert(vmSYSI == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));

	/* SYSI */
	asmInit(); asmAsm(QUIT); asmAssemble(); assert(vmQUIT == memVectorObject(r0, 0));

	/* NOP isn't emitted */
	asmInit(); asmAsm(NOP); asmAssemble(); assert(vmNOP == memVectorObject(r0, 0));

//asmDumpIBlocks();
//vmDebugDumpCode(r0, stderr);
}




static void asmtDisplayInteger (void) { fprintf (FB, INT, r1); }
static void asmtDisplayString (void) { fprintf (FB, "%s", r1); }
static void asmtDisplayNewline (void) { fprintf (FB, "\n"); }
static void asmtVmDebugDumpCode (void) {
	if (0) {
		memDebugDumpAll(stdout);
		vmDebugDumpCode(rcode, stdout);
	}
}

int myTest (void) {
 Obj Lmain, Lloop, Lloopdone;
 char *welcomemsg="Hello, unit test!!!";

	FBInit();

	memPointerRegister (asmtDisplayString);
	memPointerRegister (asmtDisplayInteger);
	memPointerRegister (asmtDisplayNewline);
	memPointerRegister (asmtVmDebugDumpCode);
	memPointerRegister (welcomemsg);

	asmInit();
	Lmain = asmNewLabel();
	Lloop = asmNewLabel();
	Lloopdone = asmNewLabel();
	/* Create the assembly.
	*/
	asmAsm(
		SYSI, asmtDisplayNewline,
		MVI, R1, welcomemsg,
		SYSI, asmtDisplayString,
		SYSI, asmtDisplayNewline,
		MVI, R0, 0l,
	 LABEL, Lmain,
		MV, R1, R0,
	 LABEL, Lloop,
		SYSI, asmtDisplayInteger,
		PUSH, R1,
			MVI, R1, " ",
			SYSI, asmtDisplayString, /* Space */
		POP, R1,
		ADDI, R1, 1l,
		BEQI, R1, 10l, Lloopdone,
		BRA, Lloop,
	 LABEL, Lloopdone,
		MVI, R1, "\r\n",
		SYSI, asmtDisplayString,
		ADDI, R0, 1l,
		BNEI, R0, 10l, Lmain,
		SYSI, asmtVmDebugDumpCode,
		QUIT
	);

	/* Compile the assembly program then run the program */
	asmAssemble();
	rcode = r0;
	rip = 0;
//vmDebugDumpCode(rcode, stderr);
	vmRun();

	/* Run the program again after a garbage collection */
	memGarbageCollect();
	rip = 0;
	vmRun();

	FBFinalize("\nHello, unit test!!!\n"
	              "0 1 2 3 4 5 6 7 8 9 \r\n"
	              "1 2 3 4 5 6 7 8 9 \r\n"
	              "2 3 4 5 6 7 8 9 \r\n"
	              "3 4 5 6 7 8 9 \r\n"
	              "4 5 6 7 8 9 \r\n"
	              "5 6 7 8 9 \r\n"
	              "6 7 8 9 \r\n"
	              "7 8 9 \r\n"
	              "8 9 \r\n"
	              "9 \r\n"
	              "\nHello, unit test!!!\n"
	              "0 1 2 3 4 5 6 7 8 9 \r\n"
	              "1 2 3 4 5 6 7 8 9 \r\n"
	              "2 3 4 5 6 7 8 9 \r\n"
	              "3 4 5 6 7 8 9 \r\n"
	              "4 5 6 7 8 9 \r\n"
	              "5 6 7 8 9 \r\n"
	              "6 7 8 9 \r\n"
	              "7 8 9 \r\n"
	              "8 9 \r\n"
	              "9 \r\n");

	return 0;
}

void optimizePopPush (void) {
	asmInit();
	asmAsm(
		PUSH, R0,
		MVI, R1, 3, /* Create a vector */
		SYSI, objNewVector1,
		PUSH, R3,
		POP, R4,
			LDI, R2, R0, 0l, /* Consider 1st element, inc and store back */
			ADDI, R2, (Obj)1,
			STI, R2, R0, 0l,
			MV, R1, R0,
			//MV, R0, R4, /* This would prevent the above pop and below push from being optimized out */
			//SYSI, cctDumpObjR0, /* So should this */
			MV, R0, R1,
			LDI, R2, R0, 1l, /* Consider 2nd element, inc and store back */
			ADDI, R2, (Obj)2,
			STI, R2, R0, 1l,
		PUSH, R4,
		POP, R3,
		POP, R0,
		QUIT
	);
//asmDumpIBlocks();
	asmAssemble();
	assert(21==memObjectLength(r0)); /* Verify 21 opcodes emitted */
//vmDebugDumpCode(r0, stderr);
	rcode = r0;  rip = 0;  vmRun();
}


void optimizeEmptyIBlock (void) {
 Obj L, LL, LLL;
	asmInit();
	L = asmNewLabel();
	LL = asmNewLabel();
	LLL = asmNewLabel();
	asmAsm(
		BEQI, R0, 0, L,
	 LABEL, L,
		BRA, LL,
	 LABEL, LL,
		MV, R0, R1,
	 LABEL, LLL,
		BRA, LLL,
		NOP
	);
	/* Call asmPrepareIGraph() directly and verify igraph connectivity */
	asmAssemble();
	/* Expect the following code block:
	   2AE69E4E7220*0000 beqi $0 0 0003
	   2AE69E4E7238 0003 mv   $0 $1
	   2AE69E4E7240 0004 bra 0004
	*/
	assert(vmBEQI0 == memVectorObject(r0, 0));
	assert(vmMV01 == memVectorObject(r0, 3));
	assert(vmBRA == memVectorObject(r0, 4));
	assert(-2*8 == (Num)memVectorObject(r0, 5));
}


int main (int argc, char *argv[]) {
	asmInitialize();
	vmInitialize(NULL, objDump); /* Register display with the VM code dumper */
	testInitialize();

	memPointerRegister(syscallAdd);
	memPointerRegister(syscallMul);
	memPointerRegister(cctDumpSpace);
	memPointerRegister(cctDumpNewline);
	memPointerRegister(cctDumpIntegerR0);
	memPointerRegister(cctDumpObjR0);

	TEST(test1);
	TEST(test2);
	TEST(test3);
	TEST(test4);
	TEST(test5);
	TEST(cctAsm);
	TEST(cctAsmNested);
	TEST(cctSysCall);
	TEST(cctJumpAndLink);
	TEST(opcodes);
	TEST(myTest);
	TEST(optimizePopPush);
	TEST(optimizeEmptyIBlock);

	return 0;
}
