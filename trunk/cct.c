#define DEBUG 0
#define DB_DESC "CC"
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "cc.h"
#include "sys.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"

/*
 TABLE OF CONTENTS
 REPL
*/
#define riblock rb /* The current iblock where new icode is emitted and new iblocks are attached to */
#define rexpr   rf /* Expression being compiled.  See vm.h */


#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))



/* A character file buffer and the functions that print to it
*/
FILE *FB;
char *fpBuff=NULL;

/* Initialize character file buffer */
void cctIOInit (void) {
 static Num fpBuffStrSize;
	FB = open_memstream(&fpBuff, &fpBuffStrSize);
	assert(NULL != FB);
}

/* Compare character file buffer's contents with string argument */
void cctIOFinalize (char *baseString) {
	fflush(FB);
	assert(0 == strcmp(fpBuff, baseString));
	fclose(FB);
	free(fpBuff);
}


void cctDumpSpace (void) { fprintf(FB, " "); }
void cctDumpNewline (void) { fprintf(FB, "\n"); }
void cctDumpIntegerR0 (void) { fprintf(FB, INT, r0); }
void cctDumpIntegerR0stderr (void) { fprintf(stderr, INT, r0); }
void cctDumpObjR0 (void) { sysDisplay(r0, FB); }



void ccIBlockSetDefaultTag (Obj ib, Obj tag);
void ccIBlockSetConditionalTag (Obj ib, Obj tag);
extern Num ccGenerateNewIBlock (Num icodeSize);
extern void ccAsmInit (void);
extern void ccAsmIGraph (void);


/* Emit a new icode object to the current iblock.  Not called by the
   following opcode calls.
*/
extern void ccICodePushNewMV (Obj rega, Obj regb);
extern void ccICodePushNewMVI (Obj r, Obj o);
extern void ccICodePushNewLDI (Obj rega, Obj regb, Obj o);
extern void ccICodePushNewPUSH (Obj o);
extern void ccICodePushNewPOP (Obj r);
extern void ccICodePushNewADDI (Obj r, Obj o);
extern void ccICodePushNewBEQI (Obj rega, Obj imm, Obj o);
extern void ccICodePushNewBNEI (Obj r, Obj i, Obj o);
extern void ccICodePushNewBRTI (Obj rega, Obj imm, Obj o);
extern void ccICodePushNewBRA (Obj o);
extern void ccICodePushNewSYSI (Obj o);
extern void ccICodePushNewNOP (void);
extern void ccICodePushNewQUIT (void);

extern void ccGenerateIBlockWithPushedIcodes ();



/* Create simple icode program of one iblock then compile the
   iblock and run the code block in the VM
*/
void test1 (void) {
	ccAsmInit();
	cctIOInit();

	/* Head iblock of the igraph */
	ccICodePushNewNOP();
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */

	/* Create a new iblock as the default block to the iblock specified by
	   the first parameter.
	   Need to know beforehand how many instructions will be emitted.  Pass in
	   parent iblock ID (initially 0) and number of icode objects to emit.
	 */
	/* Emit icode objects by combining instruction's fields */
	ccICodePushNewMVI(R0, (Obj)69);
	ccICodePushNewSYSI(cctDumpIntegerR0);
	ccICodePushNewQUIT();
	ccGenerateIBlockWithPushedIcodes();

	ccAsmIGraph();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	cctIOFinalize("69");
}


/* Creates three linked iblocks with the 2nd containing a conditional
   branch to itself.  The program counts from 10 to 0.
*/
void test2 (void) {
	ccAsmInit();
	cctIOInit();

	/* Head iblock of the igraph */
	ccICodePushNewNOP();
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */

	ccICodePushNewMVI(R0, (Obj)10);
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */

	ccICodePushNewSYSI((Obj)cctDumpIntegerR0);
	ccICodePushNewSYSI((Obj)cctDumpSpace);
	ccICodePushNewADDI(R0, (Obj)-1);
	ccICodePushNewBNEI(R0, 0, false);
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */
	ccIBlockSetConditionalTag(riblock, riblock); /* Set conditional block to self */

	ccICodePushNewQUIT();
	ccGenerateIBlockWithPushedIcodes();

	ccAsmIGraph(); /* rcode/r1e */
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	cctIOFinalize("10 9 8 7 6 5 4 3 2 1 ");
}


/* Compile a mutli iblock program with an overlapping branch
   to a prior iblock.  Counts from 5 to 1, 4 to 1, ... 1 to 1
*/
void test3 (void) {
	ccAsmInit();
	cctIOInit();

	/* Head iblock of the igraph */
	ccICodePushNewNOP();
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */

	ccICodePushNewMVI(R1, (Obj)5);
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true);

	ccICodePushNewSYSI((Obj)cctDumpSpace);
	ccICodePushNewMV(R0, R1);
	ccGenerateIBlockWithPushedIcodes();
	r3 = riblock; /* Keep track of this iblock so we can link to it from an iblock below */
	ccIBlockSetDefaultTag(riblock, true);

	ccICodePushNewSYSI((Obj)cctDumpIntegerR0);
	ccICodePushNewSYSI((Obj)cctDumpSpace);
	ccICodePushNewADDI(R0, (Obj)-1);
	ccICodePushNewBNEI(R0, 0, false);
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true);
	ccIBlockSetConditionalTag(riblock, riblock); /* Set this iblock's conditional link to self */

	ccICodePushNewADDI(R1, (Obj)-1);
	ccICodePushNewBNEI(R1, 0, false); /* Jump back to another iblock */
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true);
	ccIBlockSetConditionalTag(riblock, r3); /* Set this iblock's conditional link to self */

	ccICodePushNewQUIT();
	ccGenerateIBlockWithPushedIcodes();

	ccAsmIGraph(); /* rcode/r1e */
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	cctIOFinalize(" 5 4 3 2 1  4 3 2 1  3 2 1  2 1  1 ");
}


/* Compile a mutli iblock program with a branch to a forward
   iblock.
*/
void test4 (void) {
	ccAsmInit();
	cctIOInit();

	ccICodePushNewNOP();
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true);

	// 4
	ccICodePushNewMVI(R1, (Obj)9);
	ccICodePushNewMV(R0, R1);
	ccICodePushNewBNEI(R0, 0, false); /* Branch forward to 7 for fun which just branches back to 5 */
	ccGenerateIBlockWithPushedIcodes();
	r4 = riblock;
	ccIBlockSetDefaultTag(riblock, true);

	// 5
	ccICodePushNewSYSI((Obj)cctDumpSpace);
	ccGenerateIBlockWithPushedIcodes();
	r5 = riblock;
	ccIBlockSetDefaultTag(riblock, true);

	ccICodePushNewSYSI((Obj)cctDumpIntegerR0);
	ccICodePushNewSYSI((Obj)cctDumpSpace);
	ccICodePushNewADDI(R0, (Obj)-1);
	ccICodePushNewBNEI(R0, 0, false); /* Loop to self */
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true);
	ccIBlockSetConditionalTag(riblock, riblock);

	// 6
	ccICodePushNewBNEI(R0, 0, false); /* Branch to 8 and quit */
	ccGenerateIBlockWithPushedIcodes();
	r6 = riblock;
	ccIBlockSetDefaultTag(riblock, true);

	// 7
	ccICodePushNewBNEI(R0, 0, false); /* Branch back to 5 */
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true);
	ccIBlockSetConditionalTag(r4, riblock);
	ccIBlockSetConditionalTag(riblock, r5);

	// 8
	ccICodePushNewQUIT();
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetConditionalTag(r6, riblock);

	ccAsmIGraph();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	cctIOFinalize(" 9 8 7 6 5 4 3 2 1 ");
}


/* Igraph representing two conditional's sharing the same target
*/
void test5 (void) {
	ccAsmInit();
	cctIOInit();

	ccICodePushNewMVI(R0, (Obj)9);
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true);

	// id2
	ccICodePushNewBEQI(R0, (Obj)1, false); /* Branch to id4  */
	ccGenerateIBlockWithPushedIcodes();
	r4 = riblock;
	ccIBlockSetDefaultTag(riblock, true);

	// id3
	ccICodePushNewBEQI(R0, (Obj)2, false);
	ccGenerateIBlockWithPushedIcodes();
	r5 = riblock;
	ccIBlockSetDefaultTag(riblock, true);

	// id5
	ccICodePushNewSYSI((Obj)cctDumpIntegerR0);
	ccICodePushNewADDI(R0, (Obj)-1);
	ccICodePushNewBNEI(R0, 0, false); /* Branch back to id2 */
	ccGenerateIBlockWithPushedIcodes();
	r6 = riblock;
	ccIBlockSetDefaultTag(riblock, true);
	ccIBlockSetConditionalTag(riblock, r4);

	// id6
	ccICodePushNewQUIT();
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetDefaultTag(riblock, true);

	ccICodePushNewQUIT();
	ccGenerateIBlockWithPushedIcodes();

	// id4
	ccICodePushNewSYSI((Obj)cctDumpIntegerR0);
	ccGenerateIBlockWithPushedIcodes();
	ccIBlockSetConditionalTag(r4, riblock);
	ccIBlockSetConditionalTag(r5, riblock);
	ccIBlockSetDefaultTag(riblock, r6);

	ccAsmIGraph();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	cctIOFinalize("98765432211");
}


/* How I'd like to write an assembly program.  ccAsm will generate
   a new igraph attached to the last specified (or generated) iblock
   in the global igraph.
*/
void cctAsm() {
 Obj L0, L1, L2, L3, L4, L5, L6;

	ccAsmInit();
	cctIOInit();

	L0 = ccNewLabel();
	L1 = ccNewLabel();
	L2 = ccNewLabel();
	L3 = ccNewLabel();

	ccAsm (
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

	L4 = ccNewLabel();
	L5 = ccNewLabel();
	L6 = ccNewLabel();

	ccAsm (
	LABEL, L4,
		MVI, R0, false,
		BNEI, R0, true, L5,
		MVI, R0, false,
		BRA, L6,
	LABEL, L5,
		MVI, R0, true,
	LABEL, L6,
		SYSI, cctDumpObjR0,
		MVI, R1, (Obj)2,
		ADDI, R2, -1l,
		BNEI, R2, 0l, L1,
		QUIT);
	
	ccAsmIGraph();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	cctIOFinalize("87654321\n"
	             "87654321\n"
	             "87654321\n"
	             "#t87654321\n"
	             "87654321\n"
	             "#t87654321\n"
	             "87654321\n#t");
}


void cctAsmNested() {
 Obj L0, L1;

	cctIOInit();

	ccAsmInit();
	L0 = ccNewLabel();
	ccAsm (
		MVI, R0, (Obj)9,
	LABEL, L0,
		SYSI, cctDumpIntegerR0,
		ADDI, R0, -1l,
		BNEI, R0, 0l, L0,
		SYSI, cctDumpNewline,
		QUIT
	);

		/* New ASM context */
		ccStart();
		L1 = ccNewLabel();
		ccAsm (
			MVI, R0, (Obj)10,
		LABEL, L1,
			SYSI, cctDumpIntegerR0,
			ADDI, R0, -2l,
			BNEI, R0, 0l, L1,
			SYSI, cctDumpNewline,
			QUIT
		);
		ccAsmIGraph();
		rcode = r0;
		rip = 0;
		vmRun();

	/* Previous ASM context restored by ccAsmIGraph() */
	ccAsmIGraph();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	cctIOFinalize("108642\n987654321\n");

//ccDumpIBlocks();
//vmDebugDumpCode(rcode, stderr);
}


void syscallAdd (void) {
 Int sum=0;
	DBBEG();
	while (r1--) sum += *(Int*)vmPop();
	objNewInt(sum);
	DBEND();
}

void syscallMul (void) {
 Int product=1;
	DBBEG();
	while (r1--) product *= *(Int*)vmPop();
	objNewInt(product);
	DBEND();
}

void syscallNewIntImm (void) {
	DBBEG();
	objNewInt((Int)r0);
	DBEND();
}

void cctSysCall (void) {

	cctIOInit();

	ccAsmInit();
	ccAsm (
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
	ccAsmIGraph();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	cctIOFinalize("15");

//ccDumpIBlocks();
//vmDebugDumpCode(rcode, stderr);
}

void cctJumpAndLink (void) {
	cctIOInit();
	ccAsmInit();

	ccAsm (
		SYSI, cctDumpIntegerR0,
		RET
	);
	ccAsmIGraph();
	rcode = r0;
	r7 = rcode; /* Keep track of this block in r7 */

	ccAsmInit();
	ccAsm (
		MVI, R2, r7,
		MVI, R0, 0,
		JAL, R2,
		ADDI, R0, 1l,
		JAL, R2,
		QUIT
	);

	ccAsmIGraph();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
//vmDebugDumpCode(rcode, stderr);
//fflush(FB);
//fprintf(stderr, "[%s]", fpBuff);
	cctIOFinalize("01");
}


/* Verify opcodes and their specific fields can be assembled
*/
void opcodes (void) {
 Obj L;

	/* MV */
	ccAsmInit(); ccAsm(MV, R0, R1); ccAsmIGraph(); assert(vmMV01 == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(MV, R0, R3); ccAsmIGraph(); assert(vmMV03 == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(MV, R1, R0); ccAsmIGraph(); assert(vmMV10 == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(MV, R2, R0); ccAsmIGraph(); assert(vmMV20 == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(MV, R3, R0); ccAsmIGraph(); assert(vmMV30 == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(MV, R5, R18); ccAsmIGraph(); assert(vmMV518 == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(MV, R5, R1C); ccAsmIGraph(); assert(vmMV51C == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(MV, R1C, R0); ccAsmIGraph(); assert(vmMV1C0 == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(MV, R1C, R18); ccAsmIGraph(); assert(vmMV1C18 == memVectorObject(r0, 0));

	/* MVI */
	ccAsmInit(); ccAsm(MVI, R0, 99); ccAsmIGraph(); assert(vmMVI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(MVI, R1, 98); ccAsmIGraph(); assert(vmMVI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(MVI, R2, 97); ccAsmIGraph(); assert(vmMVI2 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(MVI, R3, 96); ccAsmIGraph(); assert(vmMVI3 == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1));

	/* LDI */
	ccAsmInit(); ccAsm(LDI, R0, R0,  99); ccAsmIGraph(); assert(vmLDI00  == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(LDI, R0, R1C, 98); ccAsmIGraph(); assert(vmLDI01C == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(LDI, R1, R1,  97); ccAsmIGraph(); assert(vmLDI11  == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(LDI, R1, R1C, 96); ccAsmIGraph(); assert(vmLDI11C == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(LDI, R2, R0,  95); ccAsmIGraph(); assert(vmLDI20  == memVectorObject(r0, 0)); assert(95 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(LDI, R2, R2,  94); ccAsmIGraph(); assert(vmLDI22  == memVectorObject(r0, 0)); assert(94 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(LDI, R5, R0,  93); ccAsmIGraph(); assert(vmLDI50  == memVectorObject(r0, 0)); assert(93 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(LDI, R1C, R0, 92); ccAsmIGraph(); assert(vmLDI1C0 == memVectorObject(r0, 0)); assert(92 == (Num)memVectorObject(r0, 1)); 

	/* LD */
	ccAsmInit(); ccAsm(LD, R0, R1, R2); ccAsmIGraph(); assert(vmLD012 == memVectorObject(r0, 0));

	/* STI */
	ccAsmInit(); ccAsm(STI, R0, R1,  99); ccAsmIGraph(); assert(vmSTI01  == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(STI, R0, R1C, 98); ccAsmIGraph(); assert(vmSTI01C == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(STI, R2, R0,  97); ccAsmIGraph(); assert(vmSTI20  == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(STI, R2, R1,  96); ccAsmIGraph(); assert(vmSTI21  == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(STI, R3, R0,  95); ccAsmIGraph(); assert(vmSTI30  == memVectorObject(r0, 0)); assert(95 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(STI, R5, R0,  94); ccAsmIGraph(); assert(vmSTI50  == memVectorObject(r0, 0)); assert(94 == (Num)memVectorObject(r0, 1));

	/* ST */
	ccAsmInit(); ccAsm(ST, R0, R1, R2); ccAsmIGraph(); assert(vmST012 == memVectorObject(r0, 0));

	/* PUSH */
	ccAsmInit(); ccAsm(PUSH, R0);  ccAsmIGraph(); assert(vmPUSH0  == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(PUSH, R1);  ccAsmIGraph(); assert(vmPUSH1  == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(PUSH, R2);  ccAsmIGraph(); assert(vmPUSH2  == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(PUSH, R19); ccAsmIGraph(); assert(vmPUSH19 == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(PUSH, R1A); ccAsmIGraph(); assert(vmPUSH1A == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(PUSH, R1B); ccAsmIGraph(); assert(vmPUSH1B == memVectorObject(r0, 0));

	/* POP */
	ccAsmInit(); ccAsm(POP, R0);  ccAsmIGraph(); assert(vmPOP0  == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(POP, R1);  ccAsmIGraph(); assert(vmPOP1  == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(POP, R2);  ccAsmIGraph(); assert(vmPOP2  == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(POP, R19); ccAsmIGraph(); assert(vmPOP19 == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(POP, R1A); ccAsmIGraph(); assert(vmPOP1A == memVectorObject(r0, 0));
	ccAsmInit(); ccAsm(POP, R1B); ccAsmIGraph(); assert(vmPOP1B == memVectorObject(r0, 0));

	/* ADDI */
	ccAsmInit(); ccAsm(ADDI, R0, 99);  ccAsmIGraph(); assert(vmADDI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(ADDI, R1, 98);  ccAsmIGraph(); assert(vmADDI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	ccAsmInit(); ccAsm(ADDI, R2, 97);  ccAsmIGraph(); assert(vmADDI2 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));

	/* BLTI */
	ccAsmInit(); L=ccNewLabel(); ccAsm(BLTI, R1, 98, L, LABEL, L, NOP); ccAsmIGraph(); assert(vmBLTI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BEQI */
	ccAsmInit(); L=ccNewLabel(); ccAsm(BEQI, R0, 99, L, LABEL, L, NOP); ccAsmIGraph(); assert(vmBEQI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	ccAsmInit(); L=ccNewLabel(); ccAsm(BEQI, R1, 98, L, LABEL, L, NOP); ccAsmIGraph(); assert(vmBEQI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BNEI */
	ccAsmInit(); L=ccNewLabel(); ccAsm(BNEI, R0, 99, L, LABEL, L, NOP); ccAsmIGraph(); assert(vmBNEI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	ccAsmInit(); L=ccNewLabel(); ccAsm(BNEI, R1, 98, L, LABEL, L, NOP); ccAsmIGraph(); assert(vmBNEI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	ccAsmInit(); L=ccNewLabel(); ccAsm(BNEI, R2, 97, L, LABEL, L, NOP); ccAsmIGraph(); assert(vmBNEI2 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	ccAsmInit(); L=ccNewLabel(); ccAsm(BNEI, R5, 96, L, LABEL, L, NOP); ccAsmIGraph(); assert(vmBNEI5 == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BRTI */
	ccAsmInit(); L=ccNewLabel(); ccAsm(BRTI, R0, 99, L, LABEL, L, NOP); ccAsmIGraph(); assert(vmBRTI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BNTI */
	ccAsmInit(); L=ccNewLabel(); ccAsm(BNTI, R0, 99, L, LABEL, L, NOP); ccAsmIGraph(); assert(vmBNTI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BRA */
	ccAsmInit(); L=ccNewLabel(); ccAsm(LABEL, L, BRA, L); ccAsmIGraph(); assert(vmBRA == memVectorObject(r0, 0)); assert(-2*8 == (Int)memVectorObject(r0, 1));

	/* JMP */
	ccAsmInit(); L=ccNewLabel(); ccAsm(JMP, R0); ccAsmIGraph(); assert(vmJ0 == memVectorObject(r0, 0));
	ccAsmInit(); L=ccNewLabel(); ccAsm(JMP, R2); ccAsmIGraph(); assert(vmJ2 == memVectorObject(r0, 0));

	/* JAL */
	ccAsmInit(); L=ccNewLabel(); ccAsm(JAL, R0, NOP); ccAsmIGraph(); assert(vmJAL0 == memVectorObject(r0, 0));
	ccAsmInit(); L=ccNewLabel(); ccAsm(JAL, R2, NOP); ccAsmIGraph(); assert(vmJAL2 == memVectorObject(r0, 0));

	/* RET */
	ccAsmInit(); L=ccNewLabel(); ccAsm(RET); ccAsmIGraph(); assert(vmRET == memVectorObject(r0, 0));

	/* SYS */
	ccAsmInit(); L=ccNewLabel(); ccAsm(SYS, R0); ccAsmIGraph(); assert(vmSYS0 == memVectorObject(r0, 0));

	/* SYSI */
	ccAsmInit(); L=ccNewLabel(); ccAsm(SYSI, 99); ccAsmIGraph(); assert(vmSYSI == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));

	/* SYSI */
	ccAsmInit(); L=ccNewLabel(); ccAsm(QUIT); ccAsmIGraph(); assert(vmQUIT == memVectorObject(r0, 0));

	/* NOP This shouldn't be emitted*/
	ccAsmInit(); L=ccNewLabel(); ccAsm(NOP); ccAsmIGraph(); assert(0 == memVectorObject(r0, 0));

//ccDumpIBlocks();
//vmDebugDumpCode(r0, stderr);
}


/*
 Read a scheme expression, evaluate and print the results
*/
void repl (void) {
	DBBEG();
	yyrestart(0);   /* Tell scanner to use stdin/0 as input. */
	while (1) {
		//renv = rtge; /* Evaluate in TGE */
		fprintf(stderr, "\n== Read and parse ===============\nWSCM>");
		yyparse();/* Expr read into r0. */

		if (eof == r0) break;

		fprintf(stderr, "\n== Compile ======================\n");
		sysWrite(r0, stderr);
		ccCompile(); /* Compile expression in r0 into a code block in r0 */
		vmDebugDumpCode(r0, stderr);

		fprintf(stderr, "\n== Execute and return value =====\n");
		rcode = r0;
		rip = 0;
		vmRun();
		sysDisplay(r0, stderr);
		//DBE fprintf(stderr, "== Debug =======================");
		//DBE memDebugDumpHeapHeaders(stderr);
		//DBE sysWrite(rstack, stderr);
		//DBE for (i=memStackLength(rstack); 0<i; --i) sysWrite(memStackObject(rstack, i-1), stdout);
	}
	fprintf (stderr, "WEL loop done\n");
	DBEND();
}




int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	ccInitialize();

	/* Register display with the VM code dumper */
	vmInitialize(NULL, sysDisplay);

	sysDefineSyscall(syscallAdd, "+");
	sysDefineSyscall(syscallMul, "*");
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
	//TEST(repl);
	return 0;
}
