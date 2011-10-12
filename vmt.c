#include <stdio.h>
#include <stdlib.h>
#include <signal.h> /* for signal() */
#include <unistd.h> /* For ualarm() */
#include <string.h>
#include <assert.h>
#include "vm.h"
#include "mem.h"

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




void displayIntegerR1 (void) {
	fprintf(FB, "%d", r1);
}

void displayStringR1 (void) {
	fprintf(FB, "%s", r1);
}



/* A machine language program that outputs to a buffer some strings and numbers.
*/
void fancyHelloWorld (void) {
 Length codeSize;
 /* Registers holding runable code objects */
 #define printNumbersSub r9
 #define helloWorldSub r10

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

		vmPUSH1A,              /* Save return address */
		vmPUSH1B,

		vmMVI0, helloWorldSub,
		vmJAL0,
		vmMV01,               /* r1-- */
		vmADDI0, (Obj)-1,
		vmMV10,
		vmMV01E, // Or load address of printNumbersSub (r9) then index ptr:  MVI0, &printNumbersSub, LDI00, 0,
		vmJAL0,

		vmPOP1B,               /* Restore return address */
		vmPOP1A,
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
	rip=0;
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

void testScheduler (void) {
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
	rip = 0;
	vmRun();

	assert((Obj)4==r5);
}



int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	vmInitialize(0, 0);

	//memDebugDumpAll(stdout);
	TEST(fancyHelloWorld);
	TEST(testScheduler);

	return 0;
}
