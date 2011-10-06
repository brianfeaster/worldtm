#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include "asm.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"

void ccIBlockSetDefaultTag (Obj ib, Obj tag);
void ccIBlockSetConditionalTag (Obj ib, Obj tag);
extern Num ccGenerateNewIBlock (Num icodeSize);
#define riblock rb /* The current iblock where new icode is emitted and new iblocks are attached to */
#define rexpr   rf /* Expression being compiled.  See vm.h */

#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))



/* A character file buffer and the functions that print to it
*/
FILE *FB;
char *FBBuff=NULL;

/* Initialize character file buffer */
void FBInit (void) {
 static Num size;
	FB = open_memstream(&FBBuff, &size);
	assert(NULL != FB);
}

/* Compare character file buffer's contents with string argument */
void FBFinalize (char *goldenString) {
	fflush(FB);
	assert(0 == strcmp(FBBuff, goldenString));
	fclose(FB);
	free(FBBuff);
}




void cctDumpSpace (void) { fprintf(FB, " "); }
void cctDumpNewline (void) { fprintf(FB, "\n"); }
void cctDumpIntegerR0 (void) { fprintf(FB, INT, r0); }
void cctDumpIntegerR0stderr (void) { fprintf(stderr, INT, r0); }
void cctDumpObjR0 (void) { objDump(r0, FB); }




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
	asmInit();
	FBInit();

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

	asmAsmIGraph();
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

	asmAsmIGraph(); /* rcode/r1e */
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

	asmAsmIGraph(); /* rcode/r1e */
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

	asmAsmIGraph();
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

	asmAsmIGraph();
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
	
	asmAsmIGraph();
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
		asmAsmIGraph();
		rcode = r0;
		rip = 0;
		vmRun();

	/* Previous ASM context restored by asmAsmIGraph() */
	asmAsmIGraph();
	rcode = r0;
	rip = 0;
	vmRun();

	/* Verify output */
	FBFinalize("108642\n987654321\n");

//ccDumpIBlocks();
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
	asmAsmIGraph();
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
	asmAsmIGraph();
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

	asmAsmIGraph();
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
	asmInit(); asmAsm(MV, R0, R1); asmAsmIGraph(); assert(vmMV01 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R0, R3); asmAsmIGraph(); assert(vmMV03 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R1, R0); asmAsmIGraph(); assert(vmMV10 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R2, R0); asmAsmIGraph(); assert(vmMV20 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R3, R0); asmAsmIGraph(); assert(vmMV30 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R5, R18); asmAsmIGraph(); assert(vmMV518 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R5, R1C); asmAsmIGraph(); assert(vmMV51C == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R1C, R0); asmAsmIGraph(); assert(vmMV1C0 == memVectorObject(r0, 0));
	asmInit(); asmAsm(MV, R1C, R18); asmAsmIGraph(); assert(vmMV1C18 == memVectorObject(r0, 0));

	/* MVI */
	asmInit(); asmAsm(MVI, R0, 99); asmAsmIGraph(); assert(vmMVI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(MVI, R1, 98); asmAsmIGraph(); assert(vmMVI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(MVI, R2, 97); asmAsmIGraph(); assert(vmMVI2 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(MVI, R3, 96); asmAsmIGraph(); assert(vmMVI3 == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1));

	/* LDI */
	asmInit(); asmAsm(LDI, R0, R0,  99); asmAsmIGraph(); assert(vmLDI00  == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R0, R1C, 98); asmAsmIGraph(); assert(vmLDI01C == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R1, R1,  97); asmAsmIGraph(); assert(vmLDI11  == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R1, R1C, 96); asmAsmIGraph(); assert(vmLDI11C == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R2, R0,  95); asmAsmIGraph(); assert(vmLDI20  == memVectorObject(r0, 0)); assert(95 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R2, R2,  94); asmAsmIGraph(); assert(vmLDI22  == memVectorObject(r0, 0)); assert(94 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R5, R0,  93); asmAsmIGraph(); assert(vmLDI50  == memVectorObject(r0, 0)); assert(93 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(LDI, R1C, R0, 92); asmAsmIGraph(); assert(vmLDI1C0 == memVectorObject(r0, 0)); assert(92 == (Num)memVectorObject(r0, 1)); 

	/* LD */
	asmInit(); asmAsm(LD, R0, R1, R2); asmAsmIGraph(); assert(vmLD012 == memVectorObject(r0, 0));

	/* STI */
	asmInit(); asmAsm(STI, R0, R1,  99); asmAsmIGraph(); assert(vmSTI01  == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(STI, R0, R1C, 98); asmAsmIGraph(); assert(vmSTI01C == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(STI, R2, R0,  97); asmAsmIGraph(); assert(vmSTI20  == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(STI, R2, R1,  96); asmAsmIGraph(); assert(vmSTI21  == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(STI, R3, R0,  95); asmAsmIGraph(); assert(vmSTI30  == memVectorObject(r0, 0)); assert(95 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(STI, R5, R0,  94); asmAsmIGraph(); assert(vmSTI50  == memVectorObject(r0, 0)); assert(94 == (Num)memVectorObject(r0, 1));

	/* ST */
	asmInit(); asmAsm(ST, R0, R1, R2); asmAsmIGraph(); assert(vmST012 == memVectorObject(r0, 0));

	/* PUSH */
	asmInit(); asmAsm(PUSH, R0);  asmAsmIGraph(); assert(vmPUSH0  == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, R1);  asmAsmIGraph(); assert(vmPUSH1  == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, R2);  asmAsmIGraph(); assert(vmPUSH2  == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, R19); asmAsmIGraph(); assert(vmPUSH19 == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, R1A); asmAsmIGraph(); assert(vmPUSH1A == memVectorObject(r0, 0));
	asmInit(); asmAsm(PUSH, R1B); asmAsmIGraph(); assert(vmPUSH1B == memVectorObject(r0, 0));

	/* POP */
	asmInit(); asmAsm(POP, R0);  asmAsmIGraph(); assert(vmPOP0  == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, R1);  asmAsmIGraph(); assert(vmPOP1  == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, R2);  asmAsmIGraph(); assert(vmPOP2  == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, R19); asmAsmIGraph(); assert(vmPOP19 == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, R1A); asmAsmIGraph(); assert(vmPOP1A == memVectorObject(r0, 0));
	asmInit(); asmAsm(POP, R1B); asmAsmIGraph(); assert(vmPOP1B == memVectorObject(r0, 0));

	/* ADDI */
	asmInit(); asmAsm(ADDI, R0, 99);  asmAsmIGraph(); assert(vmADDI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(ADDI, R1, 98);  asmAsmIGraph(); assert(vmADDI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1));
	asmInit(); asmAsm(ADDI, R2, 97);  asmAsmIGraph(); assert(vmADDI2 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1));

	/* BLTI */
	asmInit(); L=asmNewLabel(); asmAsm(BLTI, R1, 98, L, LABEL, L, NOP); asmAsmIGraph(); assert(vmBLTI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BEQI */
	asmInit(); L=asmNewLabel(); asmAsm(BEQI, R0, 99, L, LABEL, L, NOP); asmAsmIGraph(); assert(vmBEQI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BEQI, R1, 98, L, LABEL, L, NOP); asmAsmIGraph(); assert(vmBEQI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BNEI */
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R0, 99, L, LABEL, L, NOP); asmAsmIGraph(); assert(vmBNEI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R1, 98, L, LABEL, L, NOP); asmAsmIGraph(); assert(vmBNEI1 == memVectorObject(r0, 0)); assert(98 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R2, 97, L, LABEL, L, NOP); asmAsmIGraph(); assert(vmBNEI2 == memVectorObject(r0, 0)); assert(97 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));
	asmInit(); L=asmNewLabel(); asmAsm(BNEI, R5, 96, L, LABEL, L, NOP); asmAsmIGraph(); assert(vmBNEI5 == memVectorObject(r0, 0)); assert(96 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BRTI */
	asmInit(); L=asmNewLabel(); asmAsm(BRTI, R0, 99, L, LABEL, L, NOP); asmAsmIGraph(); assert(vmBRTI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BNTI */
	asmInit(); L=asmNewLabel(); asmAsm(BNTI, R0, 99, L, LABEL, L, NOP); asmAsmIGraph(); assert(vmBNTI0 == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1)); assert(0 == (Int)memVectorObject(r0, 2));

	/* BRA */
	asmInit(); L=asmNewLabel(); asmAsm(LABEL, L, BRA, L); asmAsmIGraph(); assert(vmBRA == memVectorObject(r0, 0)); assert(-2*8 == (Int)memVectorObject(r0, 1));

	/* JMP */
	asmInit(); L=asmNewLabel(); asmAsm(JMP, R0); asmAsmIGraph(); assert(vmJ0 == memVectorObject(r0, 0));
	asmInit(); L=asmNewLabel(); asmAsm(JMP, R2); asmAsmIGraph(); assert(vmJ2 == memVectorObject(r0, 0));

	/* JAL */
	asmInit(); L=asmNewLabel(); asmAsm(JAL, R0, NOP); asmAsmIGraph(); assert(vmJAL0 == memVectorObject(r0, 0));
	asmInit(); L=asmNewLabel(); asmAsm(JAL, R2, NOP); asmAsmIGraph(); assert(vmJAL2 == memVectorObject(r0, 0));

	/* RET */
	asmInit(); L=asmNewLabel(); asmAsm(RET); asmAsmIGraph(); assert(vmRET == memVectorObject(r0, 0));

	/* SYS */
	asmInit(); L=asmNewLabel(); asmAsm(SYS, R0); asmAsmIGraph(); assert(vmSYS0 == memVectorObject(r0, 0));

	/* SYSI */
	asmInit(); L=asmNewLabel(); asmAsm(SYSI, 99); asmAsmIGraph(); assert(vmSYSI == memVectorObject(r0, 0)); assert(99 == (Num)memVectorObject(r0, 1));

	/* SYSI */
	asmInit(); L=asmNewLabel(); asmAsm(QUIT); asmAsmIGraph(); assert(vmQUIT == memVectorObject(r0, 0));

	/* NOP This shouldn't be emitted*/
	asmInit(); L=asmNewLabel(); asmAsm(NOP); asmAsmIGraph(); assert(0 == memVectorObject(r0, 0));

//ccDumpIBlocks();
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
	asmAsmIGraph();
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


int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	asmInitialize();

	/* Register display with the VM code dumper */
	vmInitialize(NULL, objDump);

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

	return 0;
}
