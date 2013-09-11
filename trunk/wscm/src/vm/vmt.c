#include <stdio.h>
#include <stdlib.h>
#include <signal.h> /* for signal() */
#include <unistd.h> /* For ualarm() */
#include <string.h>
#include <assert.h>
#include "vm.h"
#include "mem.h"
#include "test.h"


/*******************************************************************************
 TESTS
*******************************************************************************/
void displayHexD1 (void) {
	fprintf(FB, "%x", d1);
}

void displayHexR1 (void) {
	fprintf(FB, "%x", *(Int*)r1);
}

void displayIntegerR1 (void) {
	fprintf(FB, "%d", r1);
}

void displayStringR1 (void) {
	fprintf(FB, "%s", r1);
}


/* A machine language program that outputs to a buffer some strings and numbers.
*/
void TESTfancyHelloWorld (void) {
 Length codeSize;
 /* Registers holding runable code objects */
 #define printNumbersSub r7
 #define helloWorldSub r6

	FBInit();

	/* Dump "hello world" to stdout */
	Obj helloWorld[] = {
		vmPUSH1,
		vmMVI0, displayStringR1,
		vmMVI1, "Hello,",
		vmSYS0,
		vmMVI1, "World[tm]!",
		vmSYS0,
		vmPOP1,
		vmRET };

	codeSize = sizeof(helloWorld);
   r0 = memNewVector(TCODE, codeSize/8);
	helloWorldSub = r0;
	memcpy(helloWorldSub, helloWorld, codeSize);

	Obj printNumbers[] = {
		vmSYSI, displayIntegerR1,
		vmBEQI1, (Obj)0, (Obj)(2*8),  /* Return if r1==0 */
		vmBRA, (Obj)(1*8),
		vmRET,

		vmPUSHA,              /* Save return address */
		vmPUSHB,

		vmMVI0, helloWorldSub,
		vmJAL0,
		vmMV01,               /* r1-- */
		vmADDI0, (Obj)-1,
		vmMV10,
		vmMV0E, // Or load address of printNumbersSub (r9) then index ptr:  MVI0, &printNumbersSub, LDI00, 0,
		vmJAL0,

		vmPOPB,               /* Restore return address */
		vmPOPA,
		vmRET };

	codeSize = sizeof(printNumbers);
   r0 = memNewVector(TCODE, codeSize/8);
	printNumbersSub = r0;
	memcpy(printNumbersSub, printNumbers, codeSize);

	Obj mainCode[] = {
		vmMVI3,  0, /* Outer loop counter */
		vmMVI1, (Obj)3,
		vmMVI0, printNumbersSub,
		vmJAL0,

		vmMVI2, (Obj)0, /* Inner loop counter */
		vmMV02,        /* loop0 */
		vmMV10,
		vmSYSI, displayIntegerR1, /* print r2 */
		vmMV02, /* Add 1 to r2 */
		vmADDI0,  (Obj)1,
		vmMV20,
		vmBNEI0,  (Obj)0x5,  (Obj)(-11*8),  /* BNEI r0 0x8000 loop0 */

		vmMVI1, "\n",
		vmSYSI, displayStringR1,
		vmMV03, /* Add 1 to r3 */
		vmADDI0, (Obj)1,
		vmMV30,
		vmBNEI0,  (Obj)3, (Obj)(-29*8), /* BRA to instruction 0 in this code block. */
		vmQUIT };

	codeSize = sizeof(mainCode);
   r0 = memNewVector(TCODE, codeSize/8);
	rcode = r0;
	memcpy(rcode, mainCode, codeSize);

	/* Set instruction pointer and start  the machine language program running */
	dip=0;
	vmRun();

	/* Verify the machine language program's output against a magic string */
	FBFinalize("3Hello,World[tm]!2Hello,World[tm]!1Hello,World[tm]!001234\n"
	           "3Hello,World[tm]!2Hello,World[tm]!1Hello,World[tm]!001234\n"
	           "3Hello,World[tm]!2Hello,World[tm]!1Hello,World[tm]!001234\n");
}



/* Verify the interrupt/scheduler handler mechanism.  The handler will
   increment r5 every time slice.  The machine language program will
   halt after four r5 incrementeds.  */
void vmtSigAlarmReset (void) { ualarm(10*1000,0); }
void vmtSchedulerHandler (void) { ++r5; vmtSigAlarmReset(); } 
void vmtSigAlarmHandler (int sig) { vmInterrupt=1;}

void TESTScheduler (void) {
	vmInitialize(vmtSchedulerHandler, NULL);
	signal(SIGALRM, vmtSigAlarmHandler); /* Start the interrupt schedule timer. */
	vmtSigAlarmReset();

	Obj prog[] = {
		vmMVI5, 0l, /* Clear r5 for incrementing by the interrupt/scheduler handler */
		/* Wait for r5 to increment to 1 */
		vmMVI1, 0l,
		vmADDI1, (Obj)1,
		vmBNEI5, (Obj)1, (Obj)(-5*8),
		/* Wait for r5 to increment to 2 */
		vmMVI1, 0l,
		vmADDI1, (Obj)1,
		vmBNEI5, (Obj)2, (Obj)(-5*8),
		/* Wait for r5 to increment to 3 */
		vmMVI1, 0l,
		vmADDI1, (Obj)1,
		vmBNEI5, (Obj)3, (Obj)(-5*8),
		/* Wait for r5 to increment to 4 */
		vmMVI1, 0l,
		vmADDI1, (Obj)1,
		vmBNEI5, (Obj)4, (Obj)(-5*8),
		vmQUIT };

	r0 = memNewVector(TCODE, sizeof(prog)/8);
	memcpy(r0, prog, sizeof(prog));
	rcode = r0;
	dip = 0;
	vmRun();

	assert((Obj)4==r5);
}


/* Verify opcode LD_D1_R1
*/
void TESTMvLdSt (void) {
	FBInit();

	vmInitialize(NULL, NULL);

	// Register a new array type
	memTypeRegisterString(0, (Str)"Int");

	// Register external function names
	memPointerRegister(displayHexD1);
	memPointerRegister(displayHexR1);

	// Create a number object in r0
	r2 = memNewArray(0, sizeof(Int));
	*(Num*)r2 = 0x69;

	// Assembly program
	Obj prog[] = {
		vmMVI1, r2,
		LD_D1_R1,
		vmSYSI, displayHexD1,
      ADD_D1_I, (Obj)1,
		vmSYSI, displayHexD1,
		BEQ_D1_I, (Obj)0x6a, (Obj)(-7*8),
		ST_D1_R1,
		vmSYSI, displayHexR1,
		vmQUIT };

	// Copy the program into a code object
	r0 = memNewVector(TCODE, sizeof(prog)/8);
	memcpy(r0, prog, sizeof(prog));
	rcode = r0;
	dip = 0;
	vmRun();

	// Look at the VM details
	//memDebugDumpAll(stdout);

	// Disassemble the code block
	//vmDisplayTypeCode(rcode, stdout);

	//FBDump();
	FBFinalize("696a6b6b");
}

int main (int argc, char *argv[]) {
	vmInitialize(0, 0);
	testInitialize();
	TEST(TESTfancyHelloWorld);
	TEST(TESTScheduler);
	TEST(TESTMvLdSt);
	return 0;
}
