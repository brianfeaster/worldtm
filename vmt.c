#include <stdio.h>
#include <stdlib.h>
#include <signal.h> /* for signal() */
#include <unistd.h> /* For ualarm() */
#include <string.h>
#include <assert.h>
#include "vm.h"
#include "mem.h"

#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))



/* A character buffer and the functions that print to it
*/
FILE *fp;
char *buff, *baseString;
Num buffLength=0;

void vmtIOInit (void) {
	fp = open_memstream(&buff, &buffLength);
	assert(NULL != fp);
}

void vmtIOFlush (void) {
	fflush(fp);
}

void vmtIOFinalize (void) {
	fclose(fp);
	free(buff);
}

void displayIntegerR1 (void) {
	fprintf (fp, "%d", r1);
}

void displayStringR1 (void) {
	fprintf (fp, "%s", r1);
}

/* A machine language program that outputs to a buffer some strings and numbers.
*/
void fancyHelloWorld (void) {
 LengthType codeSize;
 /* Registers holding runable code objects */
 #define helloWorldSub r10
 #define printNumbersSub r11

	vmtIOInit();

	/* Dump "hello world" to stdout */
	Obj helloWorld[] = {
		PUSH1,
		MVI0, displayStringR1,
		MVI1, "Hello,",
		SYS0,
		MVI1, "World[tm]!",
		SYS0,
		POP1,
		RET };

	codeSize = sizeof(helloWorld);
   memNewVector(TCODE, codeSize/8);
	helloWorldSub = r0;
	memcpy(helloWorldSub, helloWorld, codeSize);

	Obj printNumbers[] = {
		SYSI, displayIntegerR1,
		BEQI1, (Obj)0, (Obj)(2*8),  /* Return if r1==0 */
		BRA, (Obj)(1*8),
		RET,

		PUSH1A,              /* Save return address */
		PUSH1B,

		MVI0, helloWorldSub,
		JAL0,
		MV01,               /* r1-- */
		ADDI0, (Obj)-1,
		MV10,
		MV01E, // Or load address of printNumbersSub (r11) then index ptr:  MVI0, &printNumbersSub, LDI00, 0,
		JAL0,

		POP1B,               /* Restore return address */
		POP1A,
		RET };

	codeSize = sizeof(printNumbers);
   memNewVector(TCODE, codeSize/8);
	printNumbersSub = r0;
	memcpy(printNumbersSub, printNumbers, codeSize);

	Obj mainCode[] = {
		MVI3,  0, /* Outer loop counter */
		MVI1, (Obj)3,
		MVI0, printNumbersSub,
		JAL0,

		MVI2, (Obj)0, /* Inner loop counter */
		MV02,        /* loop0 */
		MV10,
		SYSI, displayIntegerR1, /* print r2 */
		MV02, /* Add 1 to r2 */
		ADDI0,  (Obj)1,
		MV20,
		BNEI0,  (Obj)0x5,  (Obj)(-11*8),  /* BNEI r0 0x8000 loop0 */

		MVI1, "\n",
		SYSI, displayStringR1,
		MV03, /* Add 1 to r3 */
		ADDI0, (Obj)1,
		MV30,
		BNEI0,  (Obj)3, (Obj)(-29*8), /* BRA to instruction 0 in this code block. */
		QUIT };

	codeSize = sizeof(mainCode);
   memNewVector(TCODE, codeSize/8);
	rcode = r0;
	memcpy(rcode, mainCode, codeSize);

	/* Set instruction pointer and start  the machine language program running */
	rip=0;
	vmRun();


	/* Verify the machine language program's output against a magic string */
	vmtIOFlush();
	baseString = "3Hello,World[tm]!2Hello,World[tm]!1Hello,World[tm]!001234\n"
	             "3Hello,World[tm]!2Hello,World[tm]!1Hello,World[tm]!001234\n"
	             "3Hello,World[tm]!2Hello,World[tm]!1Hello,World[tm]!001234\n";
	assert(buffLength == strlen(baseString));
	assert(0 == strcmp(buff, baseString));

	vmtIOFinalize();
}



/* Verify the interrupt/scheduler handler mechanism.  The handler will
   increment r5 every time slice.  The machine language program will
   halt after four r5 incrementeds.  */
void vmtSchedulerHandler (void) { ++r5; vmtSigAlarmReset(); } 
void vmtSigAlarmHandler (int sig) { vmInterrupt=1;}
void vmtSigAlarmReset (void) { ualarm(10*1000,0); }

void testScheduler (void) {
	vmInitialize(vmtSchedulerHandler, NULL);
	signal(SIGALRM, vmtSigAlarmHandler); /* Start the interrupt schedule timer. */
	vmtSigAlarmReset();

	Obj prog[] = {
		MVI5, 0l, /* Clear r5 for incrementing by the interrupt/scheduler handler */
		/* Wait for r5 to increment to 1 */
		MVI1, 0l,
		ADDI1, (Obj)1,
		BNEI5, (Obj)1, (Obj)(-5*8),
		/* Wait for r5 to increment to 2 */
		MVI1, 0l,
		ADDI1, (Obj)1,
		BNEI5, (Obj)2, (Obj)(-5*8),
		/* Wait for r5 to increment to 3 */
		MVI1, 0l,
		ADDI1, (Obj)1,
		BNEI5, (Obj)3, (Obj)(-5*8),
		/* Wait for r5 to increment to 4 */
		MVI1, 0l,
		ADDI1, (Obj)1,
		BNEI5, (Obj)4, (Obj)(-5*8),
		QUIT };

	memNewVector(TCODE, sizeof(prog)/8);
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

	TEST(fancyHelloWorld);
	TEST(testScheduler);

	return 0;
}
