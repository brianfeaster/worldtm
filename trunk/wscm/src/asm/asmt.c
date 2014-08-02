#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include "mem.h"
#include "vm.h"
#include "obj.h"
#include "asm.h"
#include "test.h"

#define RSTK R1F

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

/* Debugging - Uncomment this and make the two calls below to enable VM stepping.
static void vmtStepHandler (void) {
	memPrintStructures(stdout);
	memPrintRootSet(stdout);
	vmDisplayTypeCode(rcode, stdout);
	getchar();
	vmInterrupt = 1; // Force interrupt after next instruction to this stepHandler
}
vmInitialize(vmtStepHandler, NULL);
vmInterrupt = 1;
*/



/*******************************************************************************
 TESTS
*******************************************************************************/
void cctDumpSpace      (void) { fprintf(FB, " "); }
void cctDumpNewline    (void) { fprintf(FB, "\n"); }
void cctPrintIntr00    (void) { fprintf(FB, INT, r00); }
void cctDumpHexR00     (void) { fprintf(FB, HEX, r00); }
void cctDumpObjR00     (void) { objDisplay(r00, FB); }

void cctDumpSpacestderr     (void) { fprintf(stderr, " "); }
void cctDumpReturnstderr     (void) { fprintf(stderr, "\r"); }
void cctDumpNewlienstderr     (void) { fprintf(stderr, "\n"); }
void cctDumpIntegerR00stderr (void) { fprintf(stderr, INT, r00); }
void cctDumpHEX016R00stderr  (void) { fprintf(stderr, HEX016, r00); }


void cctDebugDumpAll (void) {
	memPrintStructures(stdout);
	memPrintObject(rstack, stdout);
}

/* Create a NOP iblock followed by a simple icode program of one iblock then compile the
   iblock and run the code block in the VM
*/
void TestmviRegister (void) {
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
	asmICodePushNewMVI(R00, (Obj)69);
	asmICodePushNewSYSI(cctPrintIntr00);
	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();

	asmAssemble();
	rcode = r00;
	rip = 0;
	vmRun();

//memPrintAll(stdout);
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

	asmICodePushNewMVI(R00, (Obj)10);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */

	asmICodePushNewSYSI((Obj)cctPrintIntr00);
	asmICodePushNewSYSI((Obj)cctDumpSpace);
	asmICodePushNewADDI(R00, (Obj)-1);
	asmICodePushNewBNEI(R00, 0, ofalse);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */
	asmIBlockConditionalTagSet(riblock, riblock); /* Set conditional block to self */

	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();

	asmAssemble(); /* rcode/r1e */
	rcode = r00;
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

	asmICodePushNewMVI(R01, (Obj)5);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);

	asmICodePushNewSYSI((Obj)cctDumpSpace);
	asmICodePushNewMV(R00, R01);
	asmGenerateIBlockWithPushedIcodes();
	r03 = riblock; /* Keep track of this iblock so we can link to it from an iblock below */
	asmIBlockDefaultTagSet(riblock, otrue);

	asmICodePushNewSYSI((Obj)cctPrintIntr00);
	asmICodePushNewSYSI((Obj)cctDumpSpace);
	asmICodePushNewADDI(R00, (Obj)-1);
	asmICodePushNewBNEI(R00, 0, ofalse);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);
	asmIBlockConditionalTagSet(riblock, riblock); /* Set this iblock's conditional link to self */

	asmICodePushNewADDI(R01, (Obj)-1);
	asmICodePushNewBNEI(R01, 0, ofalse); /* Jump back to another iblock */
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);
	asmIBlockConditionalTagSet(riblock, r03); /* Set this iblock's conditional link to self */

	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();

	asmAssemble(); /* rcode/r1e */
	rcode = r00;
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
	asmICodePushNewMVI(R01, (Obj)9);
	asmICodePushNewMV(R00, R01);
	asmICodePushNewBNEI(R00, 0, ofalse); /* Branch forward to 7 for fun which just branches back to 5 */
	asmGenerateIBlockWithPushedIcodes();
	r04 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);

	// 5
	asmICodePushNewSYSI((Obj)cctDumpSpace);
	asmGenerateIBlockWithPushedIcodes();
	r05 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);

	asmICodePushNewSYSI((Obj)cctPrintIntr00);
	asmICodePushNewSYSI((Obj)cctDumpSpace);
	asmICodePushNewADDI(R00, (Obj)-1);
	asmICodePushNewBNEI(R00, 0, ofalse); /* Loop to self */
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);
	asmIBlockConditionalTagSet(riblock, riblock);

	// 6
	asmICodePushNewBNEI(R00, 0, ofalse); /* Branch to 8 and quit */
	asmGenerateIBlockWithPushedIcodes();
	r06 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);

	// 7
	asmICodePushNewBNEI(R00, 0, ofalse); /* Branch back to 5 */
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);
	asmIBlockConditionalTagSet(r04, riblock);
	asmIBlockConditionalTagSet(riblock, r05);

	// 8
	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockConditionalTagSet(r06, riblock);

	asmAssemble();
	rcode = r00;
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

	asmICodePushNewMVI(R00, (Obj)9);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);

	// id2
	asmICodePushNewBEQI(R00, (Obj)1, ofalse); /* Branch to id4  */
	asmGenerateIBlockWithPushedIcodes();
	r04 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);

	// id3
	asmICodePushNewBEQI(R00, (Obj)2, ofalse);
	asmGenerateIBlockWithPushedIcodes();
	r05 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);

	// id5
	asmICodePushNewSYSI((Obj)cctPrintIntr00);
	asmICodePushNewADDI(R00, (Obj)-1);
	asmICodePushNewBNEI(R00, 0, ofalse); /* Branch back to id2 */
	asmGenerateIBlockWithPushedIcodes();
	r06 = riblock;
	asmIBlockDefaultTagSet(riblock, otrue);
	asmIBlockConditionalTagSet(riblock, r04);

	// id6
	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockDefaultTagSet(riblock, otrue);

	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();

	// id4
	asmICodePushNewSYSI((Obj)cctPrintIntr00);
	asmGenerateIBlockWithPushedIcodes();
	asmIBlockConditionalTagSet(r04, riblock);
	asmIBlockConditionalTagSet(r05, riblock);
	asmIBlockDefaultTagSet(riblock, r06);

	asmAssemble();
	rcode = r00;
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
 Obj L1, L2, L3, L4, L5, L6;

	asmInit();
	FBInit();

	L1 = asmNewLabel();
	L2 = asmNewLabel();
	L3 = asmNewLabel();

	asmAsm (
		MVI, R02, (Obj)3,
		MVI, R01, (Obj)3,
	LABEL, L1,
		MVI, R00, (Obj)8,
	LABEL, L2,
		SYSI, cctPrintIntr00,
		ADDI, R00, -1l,
		BEQI, R00, 0l, L3,
		BRA, L2,
	LABEL, L3,
		SYSI, cctDumpNewline,
		ADDI, R01, -1l,
		BNEI, R01, 0l, L1
	);

	L4 = asmNewLabel();
	L5 = asmNewLabel();
	L6 = asmNewLabel();

	asmAsm (
	LABEL, L4,
		MVI, R00, ofalse,
		BNEI, R00, otrue, L5,
		MVI, R00, ofalse,
		BRA, L6,
	LABEL, L5,
		MVI, R00, otrue,
	LABEL, L6,
		SYSI, cctDumpObjR00,
		MVI, R01, (Obj)2,
		ADDI, R02, -1l,
		BNEI, R02, 0l, L1,
		QUIT);
	
	asmAssemble();
	rcode = r00;
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
		MVI, R00, (Obj)9,
	LABEL, L0,
		SYSI, cctPrintIntr00,
		ADDI, R00, -1l,
		BNEI, R00, 0l, L0,
		SYSI, cctDumpNewline,
		QUIT
	);

		/* New ASM context */
		asmStart();
		L1 = asmNewLabel();
		asmAsm (
			MVI, R00, (Obj)10,
		LABEL, L1,
			SYSI, cctPrintIntr00,
			ADDI, R00, -2l,
			BNEI, R00, 0l, L1,
			SYSI, cctDumpNewline,
			QUIT
		);
		asmAssemble();
		rcode = r00;
		rip = 0;
		vmRun();

	/* Previous ASM context restored by asmAssemble() */
	asmAssemble();
	rcode = r00;
	rip = 0;
	vmRun();

	/* Verify output */
	FBFinalize("108642\n987654321\n");

//asmDumpIBlocks();
//objDisplay(rcode, stderr);
}


void debugDumpAll (void) {
	//memDebugDumpHeapHeaders(stdout);
	fprintf(stdout, "\n");
	memPrintObject (rstack, stdout);
}

void syscallAdd (void) {
 Int sum=0;
	while (r01--)
	{
		sum += *(Int*)vmPop();
	}
	objNewInt(sum);
}

void syscallMul (void) {
 Int product=1;
	while (r01--) product *= *(Int*)vmPop();
	objNewInt(product);
}

void syscallNewIntImm (void) {
	objNewInt((Int)r00);
}

void cctSysCall (void) {

	FBInit();

	asmInit();
	asmAsm (
		MVI, R00, (Obj)6,
		SYSI, syscallNewIntImm,
		PUSH, R00,
		MVI, R00, (Obj)9,
		SYSI, syscallNewIntImm,
		PUSH, R00,
		MVI, R01, (Obj)2,
		SYSI, syscallAdd,
		SYSI, cctDumpObjR00,
		QUIT
	);
	asmAssemble();
	rcode = r00;
	rip = 0;
	vmRun();

	/* Verify output */
	FBFinalize("15");
}

void cctJumpAndLink (void) {
	FBInit();
	asmInit();

	asmAsm (
		SYSI, cctPrintIntr00,
		RET
	);
	asmAssemble();
	rcode = r00;
	r07 = rcode; /* Keep track of this block in r7 */

	asmInit();
	asmAsm (
		MVI, R02, r07,
		MVI, R00, 0,
		JAL, R02,
		ADDI, R00, 1l,
		JAL, R02,
		QUIT
	);

	asmAssemble();
	rcode = r00;
	rip = 0;
	vmRun();

	/* Verify output */
//objDisplay(rcode, stderr);
//fflush(FB);
//fprintf(stderr, "[%s]", fpBuff);
	FBFinalize("01");
}


/* Verify opcodes and their specific fields can be assembled
*/
void opcodes (void) {
 Obj L;

	/* MV */
	asmInit(); asmAsm(MV, R00, R01); asmAssemble(); assert(vmMV_R00_R01 == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R00, R03); asmAssemble(); assert(vmMV_R00_R03 == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R00, R04); asmAssemble(); assert(vmMV_R00_R04 == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R00, R0E); asmAssemble(); assert(vmMV_R00_R0E == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R01, R00); asmAssemble(); assert(vmMV_R01_R00 == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R01, R03); asmAssemble(); assert(vmMV_R01_R03 == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R02, R00); asmAssemble(); assert(vmMV_R02_R00 == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R03, R00); asmAssemble(); assert(vmMV_R03_R00 == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R05, R00); asmAssemble(); assert(vmMV_R05_R00 == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R05, R08); asmAssemble(); assert(vmMV_R05_R08 == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R05, R0C); asmAssemble(); assert(vmMV_R05_R0C == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R0C, R00); asmAssemble(); assert(vmMV_R0C_R00 == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R0C, R05); asmAssemble(); assert(vmMV_R0C_R05 == memVectorObject(r00, 0));
	asmInit(); asmAsm(MV, R0C, R08); asmAssemble(); assert(vmMV_R0C_R08 == memVectorObject(r00, 0));

	/* MVI */
	asmInit(); asmAsm(MVI, R00, 99); asmAssemble(); assert(vmMV_R00_I == memVectorObject(r00, 0)); assert(99 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(MVI, R01, 98); asmAssemble(); assert(vmMV_R01_I == memVectorObject(r00, 0)); assert(98 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(MVI, R02, 97); asmAssemble(); assert(vmMV_R02_I == memVectorObject(r00, 0)); assert(97 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(MVI, R03, 96); asmAssemble(); assert(vmMV_R03_I == memVectorObject(r00, 0)); assert(96 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(MVI, R04, 95); asmAssemble(); assert(vmMV_R04_I == memVectorObject(r00, 0)); assert(95 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(MVI, R05, 95); asmAssemble(); assert(vmMV_R05_I == memVectorObject(r00, 0)); assert(95 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(MVI, R06, 94); asmAssemble(); assert(vmMV_R06_I == memVectorObject(r00, 0)); assert(94 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(MVI, R07, 93); asmAssemble(); assert(vmMV_R07_I == memVectorObject(r00, 0)); assert(93 == (Num)memVectorObject(r00, 1));

	/* LDI */
	asmInit(); asmAsm(LDI, R00, R00, 99); asmAssemble(); assert(vmLD_R00_R00_I == memVectorObject(r00, 0)); assert(99 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(LDI, R00, R02, 99); asmAssemble(); assert(vmLD_R00_R02_I == memVectorObject(r00, 0)); assert(99 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(LDI, R00, R0C, 98); asmAssemble(); assert(vmLD_R00_R0C_I == memVectorObject(r00, 0)); assert(98 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(LDI, R01, R01, 97); asmAssemble(); assert(vmLD_R01_R01_I == memVectorObject(r00, 0)); assert(97 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(LDI, R01, R0C, 96); asmAssemble(); assert(vmLD_R01_R0C_I == memVectorObject(r00, 0)); assert(96 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(LDI, R02, R00, 95); asmAssemble(); assert(vmLD_R02_R00_I == memVectorObject(r00, 0)); assert(95 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(LDI, R02, R02, 94); asmAssemble(); assert(vmLD_R02_R02_I == memVectorObject(r00, 0)); assert(94 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(LDI, R05, R00, 93); asmAssemble(); assert(vmLD_R05_R00_I == memVectorObject(r00, 0)); assert(93 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(LDI, R0C, R00, 92); asmAssemble(); assert(vmLD_R0C_R00_I == memVectorObject(r00, 0)); assert(92 == (Num)memVectorObject(r00, 1)); 

	/* LD */
	asmInit(); asmAsm(LD, R00, R01, R02); asmAssemble(); assert(vmLD_R00_R01_R02 == memVectorObject(r00, 0));

	/* STI */
	asmInit(); asmAsm(STI, R00, R01, 99); asmAssemble(); assert(vmST_R00_R01_I == memVectorObject(r00, 0)); assert(99 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(STI, R00, R05, 99); asmAssemble(); assert(vmST_R00_R05_I == memVectorObject(r00, 0)); assert(99 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(STI, R00, R0C, 98); asmAssemble(); assert(vmST_R00_R0C_I == memVectorObject(r00, 0)); assert(98 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(STI, R02, R00, 97); asmAssemble(); assert(vmST_R02_R00_I == memVectorObject(r00, 0)); assert(97 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(STI, R02, R01, 96); asmAssemble(); assert(vmST_R02_R01_I == memVectorObject(r00, 0)); assert(96 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(STI, R03, R00, 95); asmAssemble(); assert(vmST_R03_R00_I == memVectorObject(r00, 0)); assert(95 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(STI, R05, R00, 94); asmAssemble(); assert(vmST_R05_R00_I == memVectorObject(r00, 0)); assert(94 == (Num)memVectorObject(r00, 1));

	/* ST */
	asmInit(); asmAsm(ST, R00, R01, R02); asmAssemble(); assert(vmST_R00_R01_R02 == memVectorObject(r00, 0));

	/* PUSH */
	asmInit(); asmAsm(PUSH, R00); asmAssemble(); assert(vmPUSH_R00  == memVectorObject(r00, 0));
	asmInit(); asmAsm(PUSH, R01); asmAssemble(); assert(vmPUSH_R01  == memVectorObject(r00, 0));
	asmInit(); asmAsm(PUSH, R02); asmAssemble(); assert(vmPUSH_R02  == memVectorObject(r00, 0));
	asmInit(); asmAsm(PUSH, R04); asmAssemble(); assert(vmPUSH_R04  == memVectorObject(r00, 0));
	asmInit(); asmAsm(PUSH, R05); asmAssemble(); assert(vmPUSH_R05  == memVectorObject(r00, 0));
	asmInit(); asmAsm(PUSH, R07); asmAssemble(); assert(vmPUSH_R07  == memVectorObject(r00, 0));
	asmInit(); asmAsm(PUSH, R09); asmAssemble(); assert(vmPUSH_R09 == memVectorObject(r00, 0));
	asmInit(); asmAsm(PUSH, R0A); asmAssemble(); assert(vmPUSH_R0A == memVectorObject(r00, 0));
	asmInit(); asmAsm(PUSH, R0B); asmAssemble(); assert(vmPUSH_R0B == memVectorObject(r00, 0));
	asmInit(); asmAsm(PUSH, R0C); asmAssemble(); assert(vmPUSH_R0C == memVectorObject(r00, 0));

	/* POP */
	asmInit(); asmAsm(POP, R00); asmAssemble(); assert(vmPOP_R00 == memVectorObject(r00, 0));
	asmInit(); asmAsm(POP, R01); asmAssemble(); assert(vmPOP_R01 == memVectorObject(r00, 0));
	asmInit(); asmAsm(POP, R02); asmAssemble(); assert(vmPOP_R02 == memVectorObject(r00, 0));
	asmInit(); asmAsm(POP, R03); asmAssemble(); assert(vmPOP_R03 == memVectorObject(r00, 0));
	asmInit(); asmAsm(POP, R04); asmAssemble(); assert(vmPOP_R04 == memVectorObject(r00, 0));
	asmInit(); asmAsm(POP, R07); asmAssemble(); assert(vmPOP_R07 == memVectorObject(r00, 0));
	asmInit(); asmAsm(POP, R09); asmAssemble(); assert(vmPOP_R09 == memVectorObject(r00, 0));
	asmInit(); asmAsm(POP, R0A); asmAssemble(); assert(vmPOP_R0A == memVectorObject(r00, 0));
	asmInit(); asmAsm(POP, R0B); asmAssemble(); assert(vmPOP_R0B == memVectorObject(r00, 0));
	asmInit(); asmAsm(POP, R0C); asmAssemble(); assert(vmPOP_R0C == memVectorObject(r00, 0));

	/* LSLI */
	asmInit(); asmAsm(LSLI, R10, 16);  asmAssemble(); assert(vmLSL_R10_I == memVectorObject(r00, 0)); assert(16 == (Num)memVectorObject(r00, 1));

	/* LSRI */
	asmInit(); asmAsm(LSRI, R10, 16);  asmAssemble(); assert(vmLSR_R10_I == memVectorObject(r00, 0)); assert(16 == (Num)memVectorObject(r00, 1));

	/* ADDI */
	asmInit(); asmAsm(ADDI, R00, 99);  asmAssemble(); assert(vmADD_R00_I == memVectorObject(r00, 0)); assert(99 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(ADDI, R01, 98);  asmAssemble(); assert(vmADD_R01_I == memVectorObject(r00, 0)); assert(98 == (Num)memVectorObject(r00, 1));
	asmInit(); asmAsm(ADDI, R02, 97);  asmAssemble(); assert(vmADD_R02_I == memVectorObject(r00, 0)); assert(97 == (Num)memVectorObject(r00, 1));

	/* BLTI */
	asmInit(); L=asmNewLabel(); asmAsm(BLTI, R01, 98, L, LABEL, L, NOP); asmAssemble(); assert(vmBLT_R01_I == memVectorObject(r00, 0)); assert(98 == (Num)memVectorObject(r00, 1)); assert(3*ObjSize == (Int)memVectorObject(r00, 2));

	/* BEQI */
	asmInit(); L=asmNewLabel(); asmAsm(BEQI, R00, 99, L, LABEL, L, NOP); asmAssemble(); assert(vmBEQ_R00_I == memVectorObject(r00, 0)); assert(99 == (Num)memVectorObject(r00, 1)); assert(3*ObjSize == (Int)memVectorObject(r00, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BEQI, R01, 98, L, LABEL, L, NOP); asmAssemble(); assert(vmBEQ_R01_I == memVectorObject(r00, 0)); assert(98 == (Num)memVectorObject(r00, 1)); assert(3*ObjSize == (Int)memVectorObject(r00, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BEQI, R07, 97, L, LABEL, L, NOP); asmAssemble(); assert(vmBEQ_R07_I == memVectorObject(r00, 0)); assert(97 == (Num)memVectorObject(r00, 1)); assert(3*ObjSize == (Int)memVectorObject(r00, 2));

	/* BNEI */
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R00, 99, L, LABEL, L, NOP); asmAssemble(); assert(vmBNE_R00_I == memVectorObject(r00, 0)); assert(99 == (Num)memVectorObject(r00, 1)); assert(3*ObjSize == (Int)memVectorObject(r00, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R01, 98, L, LABEL, L, NOP); asmAssemble(); assert(vmBNE_R01_I == memVectorObject(r00, 0)); assert(98 == (Num)memVectorObject(r00, 1)); assert(3*ObjSize == (Int)memVectorObject(r00, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R02, 97, L, LABEL, L, NOP); asmAssemble(); assert(vmBNE_R02_I == memVectorObject(r00, 0)); assert(97 == (Num)memVectorObject(r00, 1)); assert(3*ObjSize == (Int)memVectorObject(r00, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R05, 96, L, LABEL, L, NOP); asmAssemble(); assert(vmBNE_R05_I == memVectorObject(r00, 0)); assert(96 == (Num)memVectorObject(r00, 1)); assert(3*ObjSize == (Int)memVectorObject(r00, 2));

	/* BRA */
	asmInit(); L=asmNewLabel(); asmAsm(LABEL, L, BRA, L); asmAssemble(); assert(vmBRA == memVectorObject(r00, 0)); assert(0 == (Int)memVectorObject(r00, 1));

	/* JMP */
	asmInit(); L=asmNewLabel(); asmAsm(JMP, R00); asmAssemble(); assert(vmJMP_R00 == memVectorObject(r00, 0));
	asmInit(); L=asmNewLabel(); asmAsm(JMP, R02); asmAssemble(); assert(vmJMP_R02 == memVectorObject(r00, 0));

	/* JAL */
	asmInit(); L=asmNewLabel(); asmAsm(JAL, R00, NOP); asmAssemble(); assert(vmJAL_R00 == memVectorObject(r00, 0));
	asmInit(); L=asmNewLabel(); asmAsm(JAL, R02, NOP); asmAssemble(); assert(vmJAL_R02 == memVectorObject(r00, 0));

	/* RET */
	asmInit(); asmAsm(RET, NOP); asmAssemble(); assert(vmRET == memVectorObject(r00, 0));

	/* SYS */
	asmInit(); asmAsm(SYS, R00, NOP); asmAssemble(); assert(vmSYS_R00 == memVectorObject(r00, 0));

	/* SYSI */
	asmInit(); asmAsm(SYSI, 99, NOP); asmAssemble(); assert(vmSYS_I == memVectorObject(r00, 0)); assert(99 == (Num)memVectorObject(r00, 1));

	/* SYSI */
	asmInit(); asmAsm(QUIT); asmAssemble(); assert(vmQUIT == memVectorObject(r00, 0));

	/* NOP isn't emitted */
	asmInit(); asmAsm(NOP); asmAssemble(); assert(vmNOP == memVectorObject(r00, 0));

//asmDumpIBlocks();
//objDisplay(r00, stderr);
}




static void asmtDisplayInteger (void) { fprintf (FB, INT, r01); }
static void asmtDisplayString (void) { fprintf (FB, "%s", r01); }
static void asmtDisplayNewline (void) { fprintf (FB, "\n"); }
static void asmtVmDebugDumpCode (void) {
	if (0) {
		memPrintAll(stdout);
		objDisplay(rcode, stdout);
	}
}

int myTest (void) {
 Obj Lmain, Lloop, Lloopdone;
 char *welcomemsg="Hello, unit test!!!";

	FBInit();

	MEM_ADDRESS_REGISTER(asmtDisplayString);
	MEM_ADDRESS_REGISTER(asmtDisplayInteger);
	MEM_ADDRESS_REGISTER(asmtDisplayNewline);
	MEM_ADDRESS_REGISTER(asmtVmDebugDumpCode);
	MEM_ADDRESS_REGISTER(welcomemsg);

	asmInit();
	Lmain = asmNewLabel();
	Lloop = asmNewLabel();
	Lloopdone = asmNewLabel();
	/* Create the assembly.
	*/
	asmAsm(
		SYSI, asmtDisplayNewline,
		MVI, R01, welcomemsg,
		SYSI, asmtDisplayString,
		SYSI, asmtDisplayNewline,
		MVI, R00, 0l,
	 LABEL, Lmain,
		MV, R01, R00,
	 LABEL, Lloop,
		SYSI, asmtDisplayInteger,
		PUSH, R01,
			MVI, R01, " ",
			SYSI, asmtDisplayString, /* Space */
		POP, R01,
		ADDI, R01, 1l,
		BEQI, R01, 10l, Lloopdone,
		BRA, Lloop,
	 LABEL, Lloopdone,
		MVI, R01, "\r\n",
		SYSI, asmtDisplayString,
		ADDI, R00, 1l,
		BNEI, R00, 10l, Lmain,
		SYSI, asmtVmDebugDumpCode,
		QUIT
	);

	/* Compile the assembly program then run the program */
	asmAssemble();
	rcode = r00;
	rip = 0;
//objDisplay(rcode, stderr);
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
		PUSH, R00,
		MVI, R01, 3, /* Create a vector */
		SYSI, objNewVector01,
		PUSH, R03,
		POP, R04,
			LDI, R02, R00, 0l, /* Consider 1st element, inc and store back */
			ADDI, R02, (Obj)1,
			STI, R02, R00, 0l,
			MV, R01, R00,
			//MV, R00, R4, /* This would prevent the above pop and below push from being optimized out */
			//SYSI, cctDumpObjR00, /* So should this */
			MV, R00, R01,
			LDI, R02, R00, 1l, /* Consider 2nd element, inc and store back */
			ADDI, R02, (Obj)2,
			STI, R02, R00, 1l,
		PUSH, R04,
		POP, R03,
		POP, R00,
		QUIT
	);
//asmDumpIBlocks();
	asmAssemble();
	assert(21==memObjectLength(r00)); /* Verify 21 opcodes emitted */
//objDisplay(r00, stderr);
	rcode = r00;  rip = 0;  vmRun();
}


void optimizeEmptyIBlock (void) {
 Obj L, LL, LLL;
	asmInit();
	L = asmNewLabel();
	LL = asmNewLabel();
	LLL = asmNewLabel();
	asmAsm(
		BEQI, R00, 0, L,
	 LABEL, L,
		BRA, LL,
	 LABEL, LL,
		MV, R00, R01,
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
	assert(vmBEQ_R00_I == memVectorObject(r00, 0));
	assert(vmMV_R00_R01 == memVectorObject(r00, 3));
	assert(vmBRA == memVectorObject(r00, 4));
	assert(0*8 == (Num)memVectorObject(r00, 5));
}


/* Create a simple iblock with an opcode using an intermediate register (ireg)
*/
void TestMVIiReg (void) {
	asmInit();
	FBInit();

	/* Create a new iblock as the default block to the iblock specified by
	   the first parameter.
	 */

	/* Emit icode objects by combining instruction's fields */
	asmICodePushNewMVI(R00, (Obj)69);
	asmICodePushNewSYSI(cctPrintIntr00);
	asmICodePushNewQUIT();

	asmAssemble();
	rcode = r00;
	rip = 0;
	vmRun();

//memPrintAll(stdout);
	/* Verify output */
	FBFinalize("69");
}

void TestSpillover (void) {
	asmInit();
	FBInit();

	asmAsm(
		ADDI, RSTK, 2*ObjSize,                // Create two local vars
		MVI,  R00,  (Obj)0x69,                      // Save 69 to local var
		STI,  R00,  RSTK, (Obj)(-1*ObjSize),
		MVI,  R00,  (Obj)0x42,                      // Load 42
		STI,  R00,  RSTK, (Obj)(-2*ObjSize),    // Save 42 to local var
		SYSI, cctDumpHexR00,                 // Display 42
		LDI,  R00,  RSTK, (Obj)(-1*ObjSize),    // Load 69 from local var and display
		SYSI, cctDumpHexR00,
		ADDI, RSTK, (Obj)(-2*ObjSize),        // Unallocate two local vars (free stack space)
		QUIT
	);

	asmAssemble();
	rcode = r00;
	rip = 0;
	vmRun();

	FBFinalize("4269");
}

/* Compute and print sum of 0 to n from 10 down to 1 
*/
void TestIRegistersSumtorial (void) {
 Num A, B, C; // Intermediate registers
 Obj L, M;    // Labels
	FBInit();
	asmInit();
	A = asmNewOregister();
	B = asmNewOregister();
	C = asmNewOregister();
	L = asmNewLabel();
	M = asmNewLabel();
	asmAsm(
		MVI,  A, (Obj)10,     // A = 10
	LABEL, M,
		MV,   B, A,             // B = A
		MVI,  C, (Obj)0,        // C = 0
	LABEL, L,
		ADD, C, B,                // C += B
		ADDI, B, (Obj)-1,         // --B
		BNEI, B, (Obj)0, L,
		MV, R00, C,
		SYSI, cctPrintIntr00,
		SYSI, cctDumpSpace,
		ADDI, A, (Obj)-1,     // --A
		BNEI, A, (Obj)0, M,
		QUIT
	);

	asmAssemble();

	rcode = r00;
	rip = 0;
	vmRun();

	//objDisplay(rcode, stdout);
	FBFinalize("55 45 36 28 21 15 10 6 3 1 ");
}

int main (void) {
	asmInitialize();
	vmInitialize(NULL, objDisplay); /* Register display with the VM code dumper */
	testInitialize();

	MEM_ADDRESS_REGISTER(syscallAdd);
	MEM_ADDRESS_REGISTER(syscallMul);
	MEM_ADDRESS_REGISTER(cctDumpSpace);
	MEM_ADDRESS_REGISTER(cctDumpNewline);
	MEM_ADDRESS_REGISTER(cctPrintIntr00);
	MEM_ADDRESS_REGISTER(cctDumpObjR00);
	MEM_ADDRESS_REGISTER(syscallNewIntImm);
	MEM_ADDRESS_REGISTER(cctDumpHexR00);
	TEST(TestmviRegister);
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
	TEST(TestMVIiReg);
	TEST(TestSpillover);
	TEST(TestIRegistersSumtorial);
	return 0;
}
