#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cc.h"
#include "sys.h"
#include "obj.h"
#include "asm.h"
#include "vm.h"
#include "mem.h"

#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))



void ccDebugDumpIntegerR0 (void) { fprintf(stdout, INT, r0); }
void ccDebugDumpSpace (void) { fprintf(stdout, " "); }
void ccDebugDumpNewline (void) { fprintf(stdout, "\n"); }


/* Create simple icode program of one iblock then compile the
   iblock and run the code block in the VM
*/
void test1 (void) {
 Num id;
	ccResetIGraph();

	/* Head iblock of the igraph */
	ccGenerateNewIBlock(1);
	ccNOP();

	/* Create a new iblock as the default block to the iblock specified by
	   the first parameter.
	   Need to know beforehand how many instructions will be emitted.  Pass in
	   parent iblock ID (initially 0) and number of icode objects to emit.
	 */
	id = ccNewDefaultIBlock(0, 3);
	/* Emit icode objects by combining instruction's fields */
	ccMVI(R0, (Obj)69);
	ccSYSI(ccDebugDumpIntegerR0);
	ccQUIT();

	ccAssembleIGraph();
	rip = 0;

	vmRun();
}


/* Creates three linked iblocks with the 2nd containing a conditional
   branch to itself.  The program counts from 10 to 0.
*/
void test2 (void) {
 Num id1, id2, id3;
	ccResetIGraph();

	/* Head iblock of the igraph */
	ccGenerateNewIBlock(1);
	ccNOP();

	id1 = ccNewDefaultIBlock(0, 1);
	ccMVI(R0, (Obj)10);

	id2 = ccNewDefaultIBlock(id1, 4);
	ccSYSI((Obj)ccDebugDumpIntegerR0);
	ccSYSI((Obj)ccDebugDumpSpace);
	ccADDI(R0, (Obj)-1);
	ccBNEI(R0, 0, false); /* Jump back to this iblock */
	ccIBlockSetConditional(id2, id2); /* Set conditional block to self */

	id3 = ccNewDefaultIBlock (id2, 1);
	ccQUIT();

	ccAssembleIGraph(); /* rcode/r1e */
	rip = 0;

	vmRun();
}


/* Compile a mutli iblock program with an overlapping branch
   to a prior iblock.  Counts from 5 to 1, 4 to 1, ... 1 to 1
*/
void test3 (void) {
 Num id1, id2, id3, id4, id5;
	ccResetIGraph();

	/* Head iblock of the igraph */
	ccGenerateNewIBlock(1);
	ccNOP();

	id1 = ccNewDefaultIBlock(0, 1);
	ccMVI(R1, (Obj)5);

	id2 = ccNewDefaultIBlock(id1, 2);
	ccSYSI((Obj)ccDebugDumpSpace);
	ccMV(R0, R1);

	id3 = ccNewDefaultIBlock(id2, 4);
	ccSYSI((Obj)ccDebugDumpIntegerR0);
	ccSYSI((Obj)ccDebugDumpSpace);
	ccADDI(R0, (Obj)-1);
	ccBNEI(R0, 0, true); /* Jump back to this iblock */
	ccIBlockSetConditional(id3, id3); /* Set this iblock's conditional link to self */

	id4 = ccNewDefaultIBlock(id3, 2);
	ccADDI(R1, (Obj)-1);
	ccBNEI(R1, 0, true); /* Jump back to another iblock */
	ccIBlockSetConditional(id4, id2); /* Set this iblock's conditional link to another iblock */

	id5 = ccNewDefaultIBlock(id4, 1);
	ccQUIT();

	ccAssembleIGraph(); /* rcode/r1e */
	rip = 0;

	vmRun();
}


/* Compile a mutli iblock program with a branch to a forward
   iblock.
*/
void test4 (void) {
 Num id1, id2, id3, id4, id5, id6;

	ccResetIGraph();
	/* Head iblock of the igraph */
	ccGenerateNewIBlock(1);
	ccNOP();


	id1 = ccNewDefaultIBlock(0, 3);
	ccMVI(R1, (Obj)9);
	ccMV(R0, R1);
	ccBNEI(R0, 0, false); /* Jump forward to iblock 5 */

	id2 = ccNewDefaultIBlock(id1, 1);
	ccSYSI((Obj)ccDebugDumpSpace);

	id3 = ccNewDefaultIBlock(id2, 4);
	ccSYSI((Obj)ccDebugDumpIntegerR0);
	ccSYSI((Obj)ccDebugDumpSpace);
	ccADDI(R0, (Obj)-1);
	ccBNEI(R0, 0, false); /* Jump back to this iblock */
	ccIBlockSetConditional(id3, id3); /* Set this iblock's conditional link to self */

	id4 = ccNewDefaultIBlock(id3, 1);
	ccBNEI(R0, 0, false); /* This will jump to 6 */

	id5 = ccNewDefaultIBlock(id4, 1);
	ccBNEI(R0, 0, 0); /* Jump back to 1 */
	ccIBlockSetConditional(id1, id5); /* Set 1's conditional link this iblock */
	ccIBlockSetConditional(id5, id2); /* Set my conditional link to iblock 2 */

	id6 = ccNewDefaultIBlock(id5, 1);
	ccQUIT();
	ccIBlockSetConditional(id4, id6); /* Set 4's conditional branch to this iblock*/

	ccAssembleIGraph(); /* rcode/r1e */
	rip = 0;

//ccDumpIBlocks();
//vmDebugDumpCode(rcode, stderr);
	vmRun();
}


/* Igraph representing two conditional's sharing the same target
*/
void test5 (void) {
 Num id1, id2, id3, id4, id5, id6;

	ccResetIGraph();

	/* Head iblock of the igraph */
	ccGenerateNewIBlock(1);
	ccNOP();


	id1 = ccNewDefaultIBlock(0, 1);
	ccMVI(R0, (Obj)9);

	id2 = ccNewDefaultIBlock(id1, 1);
	ccBEQI(R0, (Obj)1, false);

	id3 = ccNewDefaultIBlock(id2, 1);
	ccBEQI(R0, (Obj)2, false);


	id5 = ccNewDefaultIBlock(id3, 3);
	ccSYSI((Obj)ccDebugDumpIntegerR0);
	ccADDI(R0, (Obj)-1);
	ccBNEI(R0, 0, false);
	ccIBlockSetConditional(id5, id2);

	id6 = ccNewDefaultIBlock(id5, 1);
	ccQUIT();

	ccNewDefaultIBlock(id6, 1);
	ccQUIT();


	id4 = ccNewConditionalIBlock(id2, 1);
	ccSYSI((Obj)ccDebugDumpIntegerR0);
	ccIBlockSetConditional(id3, id4);
	ccIBlockSetDefault(id4, id5);

	//ccDumpIBlocks();

	ccAssembleIGraph(); /* rcode/r1e */
	rip = 0;

	//vmDebugDumpCode(rcode, stderr);
	vmRun();
}

/* How I'd like to write an assembly program.  ccAsm will generate
   a new igraph attached to the last specified (or generated) iblock
   in the global igraph.
*/
void ccDumpStringR0 (void) { printf(NUM"  ", (Num)r0); }
void ccDumpObjR0 (void) { sysDisplay(r0, stdout); }
void ccDumpNewline (void) { printf("\n"); }

void cctTestAsm() {
 Num L1, L2, L3, L4, L5, L6;

	//ccResetIGraph();
	//ccGenerateNewIBlock(1);
	//ccNOP();
	//ccAssembleIGraph(); /* rcode/r1e */

	memPointerRegister(ccDumpStringR0);
	memPointerRegister(ccDumpObjR0);
	memPointerRegister(ccDumpNewline);

	/* For now labels must be created before initializing the asm module */
	L1 = ccAsmLabelNew();
	L2 = ccAsmLabelNew();
	L3 = ccAsmLabelNew();
	L4 = ccAsmLabelNew();
	L5 = ccAsmLabelNew();
	L6 = ccAsmLabelNew();
	ccAsmInit ();

	ccAsm (
		MVI, R2, (Obj)5,
		MVI, R1, (Obj)8,
	LABEL, L1,
		MVI, R0, (Obj)8,
	LABEL, L2,
		SYSI, ccDumpStringR0,
		ADDI, R0, -1l,
		BEQI, R0, 0l, L3,
		BRA, L2,
	LABEL, L3,
		SYSI, ccDumpNewline,
		ADDI, R1, -1l,
		BNEI, R1, 0l, L1
	);

	//vmPush(r3);
	//ccAssembleIGraph();
	//rip = 0;
	//vmRun();
	//r3 = vmPop();

	//ccAsmInit ();

	ccAsm (
	LABEL, L4,
		MVI, R0, false,
		BNEI, R0, true, L5,
		MVI, R0, false,
		BRA, L6,
	LABEL, L5,
		MVI, R0, true,
	LABEL, L6,
		SYSI, ccDumpObjR0,
		MVI, R1, (Obj)2,
		ADDI, R2, -1l,
		BNEI, R2, 0l, L1,
		QUIT);
	
	ccAssembleIGraph();

	rip = 0;
	vmRun();
}


int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	ccInitialize();
	memPointerRegister(ccDebugDumpIntegerR0);
	memPointerRegister(ccDebugDumpSpace);
	memPointerRegister(ccDebugDumpNewline);

	TEST(test1);
	TEST(test2);
	TEST(test3);
	TEST(test4);
	TEST(test5);
	TEST(cctTestAsm);
	return 0;
}
