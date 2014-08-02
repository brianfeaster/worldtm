#include <stdio.h>
#include <stdlib.h>
#include <signal.h> /* for signal() */
#include <unistd.h> /* For ualarm() */
#include <string.h>
#include <assert.h>
#include "vm.h"
#include "mem.h"
#include "test.h"

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
void displayChar10 (void) {
	fprintf(FB, "%c", r10);
}
void displayHex00 (void) {
	fprintf(FB, "%llx", r00);
}

void displayHex10016 (void) {
	fprintf(FB, " %llx", r10);
}

void displayHex11016 (void) {
	fprintf(FB, "%016llx", r11);
}

void displayHex11 (void) {
	fprintf(FB, "%llx", r11);
}

void displayHexR01 (void) {
	fprintf(FB, "%llx", *(Int*)r01);
}

void displayHex00R01 (void) {
	fprintf(FB, "%016llx", *(Int*)r01);
}

void displayIntegerR11 (void) {
	fprintf(FB, "%lld", r11);
}

void displayStringR01 (void) {
	fprintf(FB, "%s", r01);
}

void debugDumpAll (void) {
	memPrintAll(stdout);
}

void keyboardPause (void) {
	getchar();
}

#define TINT           0x01l
#define TREAL          0x02l
#define TCHAR          0x03l
#define TSTR           0x04l
#define TVEC           0x80l
#define TTUPLE         0x81l
#define TPAIR          0x82l

// Copy the opcodes into a code object.
#define CREATE_CODE_BLOCK(codeblock) {r00 = memNewVector(TCODE, sizeof(codeblock)/ObjSize); memcpy(r00, codeblock, sizeof(codeblock)); }


/* A verify the vector stack and array stack
*/
void TESTStacks (void) {
	FBInit();

	CREATE_CODE_BLOCK(((Obj[]){
		vmNOP,
		vmPUSH_R10, vmPUSH_R11, vmPUSH_R12, vmPUSH_R13,
		vmPUSH_R00, vmPUSH_R01, vmPUSH_R02, vmPUSH_R03,
		//SYS_I, debugDumpAll,
		vmPOP_R00, vmPOP_R01, vmPOP_R02, vmPOP_R03,
		vmPOP_R10, vmPOP_R11, vmPOP_R12, vmPOP_R13,
		vmQUIT}));

	rcode = r00;
	rip = (Obj)(1*ObjSize);

	r00 = (Obj)1; r01 = (Obj)2; r02 = (Obj)3; r03 = (Obj)4;
	r10 = (Obj)1; r11 = (Obj)2; r12 = (Obj)3; r13 = (Obj)4;

	vmRun();

	assert((Obj)4 == r00); assert((Obj)3 == r01); assert((Obj)2 == r02); assert((Obj)1 == r03);
	assert((Obj)4 == r10); assert((Obj)3 == r11); assert((Obj)2 == r12); assert((Obj)1 == r13);

	// Disassemble the code block
	//vmDisplayTypeCode(rcode, stdout);
	//memGarbageCollect();
	//memDebugDumpAll(stdout);
	//FBDump();
}


/* Verify opcode LD_R11_R01
*/
void TESTMvLdSt (void) {
	FBInit();

	// Create a number object in r02
	r02 = memNewArray(TINT, sizeof(Int));
	*(Num*)r02 = 0x69;

	CREATE_CODE_BLOCK(((Obj[]){
		vmMV_R01_I,  r02,            // $r01 <- #<0x69>
		vmLD_R11_R01,                // $r11 <- [$r01]
		vmSYS_I,    displayHex11,
      vmADD_R11_I, (Obj)1,
		vmSYS_I,    displayHex11,
		vmNOP,
		vmBEQ_R11_I, (Obj)0x6a, (Obj)(-5 * ObjSize),
		vmST_R11_R01,
		vmSYS_I,    displayHexR01,
		vmQUIT}));

	rcode = r00;
	rip = 0;
	vmRun();

	FBFinalize("696a6b6b");
}

void TESTLdStImm (void) {
	FBInit();

	// Create a number object in r02
	r02 = memNewVector(TVEC, 3);
	((Obj*)r02)[0] = (Obj)0x69;
	((Obj*)r02)[1] = (Obj)0x42;
	((Obj*)r02)[2] = (Obj)0x18;

	CREATE_CODE_BLOCK(((Obj[]){
		vmMV_R01_I,     r02,
		vmLD_R00_R01,
		vmSYS_I,        displayHex00,
		vmLD_R00_R01_I, (Obj)(1*ObjSize),
		vmSYS_I,        displayHex00,
		vmLD_R00_R01_I, (Obj)(2*ObjSize),
		vmSYS_I,        displayHex00,
		vmQUIT}));
	//vmDisplayTypeCode(rcode, stdout);
	//memPrintObject(r02, stdout);
	rcode = r00;
	rip = (Obj)0;
	vmRun();
	//memPrintHeapYoung(stdout);
	//memPrintRootSet(stdout);
	//vmDisplayTypeCode(rcode, stdout);
	//memGarbageCollect();
	//memDebugDumpAll(stdout);

	FBFinalize("694218");
}

/* display digit 1 and 4 directly.
*/
void TESTJal (void) {
	FBInit();

	CREATE_CODE_BLOCK(((Obj[]){
		vmSYS_I, displayHex11,
		vmRET}));

	r01 = r00;

	CREATE_CODE_BLOCK(((Obj[]){
      vmMV_R11_I, (Obj)0,
      vmADD_R11_I, (Obj)1,
		vmMV_R00_R01,
		vmJAL_R00,
		vmADD_R11_I, (Obj)3,
		vmJAL_R00,
		vmQUIT}));

	rcode = r00;
	rip = 0;

	vmRun();

	FBFinalize("14");
}

/* Display hex digits 0 through f by looping.
*/
void TESTBlt (void) {
	FBInit();

	CREATE_CODE_BLOCK(((Obj[]){
		vmMV_R01_I, (Obj)0,
		vmADD_R01_I, (Obj)1,
		vmMV_R00_R01,
		vmSYS_I, displayHex00,
		vmBLT_R01_I, (Obj)0x0f, (Obj)(-5*ObjSize),
		vmQUIT}));

	rcode = r00;
	rip = 0;
	vmRun();

	FBFinalize("123456789abcdef");
}


/* A machine language program that outputs to a buffer some strings and numbers.
*/
void TESTfancyHelloWorld (void) {
 /* Registers holding runable code objects */
 #define printNumbersSub r07
 #define helloWorldSub r06

	FBInit();

	/* Dump "hello world" to stdout */
	CREATE_CODE_BLOCK(((Obj[]){
		vmPUSH_R01,
			vmMV_R00_I, displayStringR01,
			vmMV_R01_I, "Hello,",
			vmSYS_R00,
			vmMV_R01_I, "World[tm]!",
			vmSYS_R00,
		vmPOP_R01,
		vmRET}));

	helloWorldSub = r00;

	CREATE_CODE_BLOCK(((Obj[]){
		vmSYS_I, displayIntegerR11,
		vmBEQ_R11_I, (Obj)0, (Obj)(5*ObjSize),  /* Return if r1==0 */
		vmBRA, (Obj)(3*ObjSize),
		vmRET,
		vmPUSH_R1C,              /* Save return address */
		vmPUSH_R0C,

		vmMV_R00_I, helloWorldSub,          /* move $00 $code then call myself */
		vmJAL_R00,

		vmADD_R11_I, (Obj)-1,     // r1--

		vmMV_R00_R0D, 
		vmJAL_R00,

		vmPOP_R0C,               /* Restore return address */
		vmPOP_R1C,
		vmRET}));

	printNumbersSub = r00;

	// Main
	CREATE_CODE_BLOCK(((Obj[]){
		vmMV_R03_I,  0, /* Outer loop counter */
		vmMV_R11_I, (Obj)3,

		vmMV_R00_I, printNumbersSub,
		vmJAL_R00,

		vmMV_R02_I, (Obj)0, /* Inner loop counter */
		vmMV_R00_R02,        /* loop0 */
		vmMV_R11_R00,
		vmSYS_I, displayIntegerR11, /* print r2 */
		vmADD_R11_I,  (Obj)1,
		vmBEQ_R11_I,  (Obj)0x5,  (Obj)(5*ObjSize),  /* BNEI r00 0x8000 loop0 */
		vmBRA, (Obj)(-7*ObjSize),

		vmMV_R01_I, "\n",
		vmSYS_I, displayStringR01,

		vmMV_R00_R03, /* Add 1 to r3 */
		vmMV_R11_R00,
		vmADD_R11_I, (Obj)1,
		vmMV_R03_R11,
		vmBEQ_R11_I,  (Obj)3, (Obj)(5*ObjSize),
		vmBRA, (Obj)(-30*ObjSize), /* BRA to instruction 1 in this code block. */
		vmQUIT}));

	rcode = r00;

	rip=0; // Set instruction pointer and start the machine language program running
	vmRun();

	/* Verify the machine language program's output against a magic string */
	FBFinalize("3Hello,World[tm]!2Hello,World[tm]!1Hello,World[tm]!001234\n"
	           "3Hello,World[tm]!2Hello,World[tm]!1Hello,World[tm]!001234\n"
	           "3Hello,World[tm]!2Hello,World[tm]!1Hello,World[tm]!001234\n");
}


/* Verify the interrupt/scheduler handler mechanism.  The handler will
   increment r05 every time slice.  The machine language program will
   halt after four r05 incrementeds.  */
void vmtSigAlarmReset (void) { ualarm(10*1000,0); }
void vmtSchedulerHandler (void) { r05 += 1; vmtSigAlarmReset(); } 
void vmtSigAlarmHandler (int sig) { vmInterrupt=1;}

void TESTScheduler (void) {
	vmInitialize(vmtSchedulerHandler, NULL);
	signal(SIGALRM, vmtSigAlarmHandler); /* Register alarm signal handler */
	vmtSigAlarmReset(); // Enable alarm interrupt.

	CREATE_CODE_BLOCK(((Obj[]){
		vmMV_R05_I, 0l, /* Clear r05 for incrementing by the interrupt/scheduler handler */
		/* Wait for r05 to increment to 1 */
		vmMV_R01_I, 0l,
		vmADD_R00_I, (Obj)1,
		vmBNE_R05_I, (Obj)1, (Obj)(-2*ObjSize),
		/* Wait for r05 to increment to 2 */
		vmMV_R01_I, 0l,
		vmADD_R01_I, (Obj)1,
		vmBNE_R05_I, (Obj)2, (Obj)(-2*ObjSize),
		/* Wait for r05 to increment to 3 */
		vmMV_R01_I, 0l,
		vmADD_R01_I, (Obj)1,
		vmBNE_R05_I, (Obj)3, (Obj)(-2*ObjSize),
		/* Wait for r05 to increment to 4 */
		vmMV_R01_I, 0l,
		vmADD_R01_I, (Obj)1,
		vmBNE_R05_I, (Obj)4, (Obj)(-2*ObjSize),
		vmQUIT}));

	rcode = r00;
	rip = 0;
	vmRun();

	ualarm(0,0); // Disable alarm interrupt

	assert((Obj)4==r05);
}

/* Display the descriptor of two array objects and one vector object.
*/
void TESTObjectDescriptor (void) {
	FBInit();

	r02 = memNewArray(TINT,  sizeof(Int)); *(Int*)r02 = (Int)0xb00b1355;
	r03 = memNewArray(TREAL, sizeof(Real)); *(Real*)r03 = (Real)1.80081355;
	r04 = memNewVector(TVEC, 3);
	memVectorSet(r04, 0, r02);
	memVectorSet(r04, 1, r03);
	memVectorSet(r04, 2, r04);
	
	CREATE_CODE_BLOCK(((Obj[]){
		vmMV_R01_R02,
		vmLD_R11_R01_I, (Obj)(-1*ObjSize),
		vmSYS_I,        displayHex11016,
		vmMV_R10_I,     (Obj)' ',
		vmSYS_I,        displayChar10,

		vmMV_R01_R03,
		vmLD_R11_R01_I, (Obj)(-1*ObjSize),
		vmSYS_I,        displayHex11016,
		vmMV_R10_I,     (Obj)' ',
		vmSYS_I,        displayChar10,

		vmMV_R01_R04,
		vmLD_R11_R01_I, (Obj)(-1*ObjSize),
		vmSYS_I,        displayHex11016,

		vmQUIT}));

	rcode = r00;
	rip = 0;
	vmRun();

	FBFinalize("0100000000000008 0200000000000008 8000000000000003");
}

/* Display the descriptor of two array objects and one vector object.
*/
void TESTTypes (void) {
	FBInit();

	r02 = memNewArray(TINT,  sizeof(Int)); *(Int*)r02 = (Int)0xb00b1355;
	r03 = memNewArray(TREAL, sizeof(Real)); *(Real*)r03 = (Real)1.80081355;
	r04 = memNewArray(TCHAR, sizeof(Chr)); *(Real*)r03 = 'A';
	r05 = memNewVector(TVEC, 3);
	r06 = memNewVector(TTUPLE, 2);
	r07 = memNewVector(TPAIR, 2);

	CREATE_CODE_BLOCK(((Obj[]){
		vmMV_R10_I,     (Obj)' ',

		vmLD_R11_R02_I, (Obj)(-1*ObjSize),
		vmSYS_I,        displayHex11016,
		vmSYS_I,        displayChar10,

		vmLD_R11_R03_I, (Obj)(-1*ObjSize),
		vmSYS_I,        displayHex11016,
		vmSYS_I,        displayChar10,
		vmADD_R11_I, (Obj)1,
		vmBLT_R11_I, (Obj)0x0200000000000002, (Obj)(-6*8),

		vmQUIT}));

	//memDebugDumpAll(stdout);
	//vmDisplayTypeCode(r00, stdout);

	rcode = r00;
	rip = 0;
	vmRun();
	FBFinalize("0100000000000008 0200000000000008 ");
}



/* Display the descriptor of two array objects and one vector object.
*/
void TESTLslLsr (void) {
	FBInit();

	r02 = memNewArray(TINT,  sizeof(Int)); *(Int*)r02 = (Int)0xb00b1355;
	r03 = memNewArray(TREAL, sizeof(Real)); *(Real*)r03 = (Real)1.80081355;
	r04 = memNewArray(TCHAR, sizeof(Chr)); *(Real*)r03 = 'A';
	r05 = memNewVector(TVEC, 3);
	r06 = memNewVector(TTUPLE, 2);
	r07 = memNewVector(TPAIR, 2);

	CREATE_CODE_BLOCK(((Obj[]){
		vmMV_R10_I,  (Obj)0xfedcba98,
		vmLSL_R10_I, (Obj)32,
		vmMV_R00_R10,
		vmSYS_I,     displayHex00,

		vmMV_R10_I, (Obj)' ',
		vmSYS_I,     displayChar10,

		vmMV_R00_I,  (Obj)0xf2345678,
		vmSYS_I,     displayHex00,

		vmMV_R10_I, (Obj)' ',
		vmSYS_I,     displayChar10,

		vmQUIT}));

	rcode = r00;
	rip = 0;
	vmRun();
	FBFinalize("fedcba9800000000 f2345678 ");
}

/* Recursively count and display 0 to 4
*/
void TESTRecursiveLoop (void) {
	FBInit();

	CREATE_CODE_BLOCK(((Obj[]){
		vmSYS_I, displayHex10016,
		vmBEQ_R10_I, (Obj)4, (Obj)(12*ObjSize), // end
		vmPUSH_R1C,
		vmPUSH_R0C,
		vmPUSH_R0B,
		vmADD_R10_I, (Obj)1,
		vmJAL_R02, 
		vmPOP_R0B,
		vmPOP_R0C,
		vmPOP_R1C,
		vmRET})); // :end

	r02 = r00; // Recursive function in $02

	CREATE_CODE_BLOCK(((Obj[]){
		vmMV_R10_I, 0,
		vmJAL_R02, // Start recursive call
		vmQUIT}));

	rcode = r00;
	rip = 0;
	vmRun();
	FBFinalize(" 0 1 2 3 4");
}

/* Create a "string" object, fill it and print it.
*/
void TESTByteLd (void) {
	FBInit();

	r01 = memNewArray(TSTR, 16);

	CREATE_CODE_BLOCK(((Obj[]){
		vmMV_R01_I, r01, // $01 gets the uninitialized object string
		vmMV_R00_I, (Obj)0xdeadbeef12345678,

		vmMV_R10_I,     (Obj)' ',
		vmSYS_I,        displayChar10,

		vmST_R00_R01_I, 0,
		vmST_R00_R01_I, (Obj)(1*ObjSize),

		vmLD_R11_R01_I, (Obj)(0*ObjSize),
		vmSYS_I, displayHex11016,

		vmMV_R10_I,     (Obj)' ',
		vmSYS_I,        displayChar10,

		vmLD_R11_R01_I, (Obj)(1*ObjSize),
		vmSYS_I, displayHex11016,

		vmQUIT}));

	rcode = r00;
	rip = 0;
	vmRun();
	FBFinalize(" deadbeef12345678 deadbeef12345678");
}


/* An endless loop.  Don't call unless you can interactively interrup it.
*/
void TESTEndlessLoop (void) {
	CREATE_CODE_BLOCK(((Obj[]){
		vmBRA, (Obj)(2*ObjSize),
		vmBRA, (Obj)(-2*ObjSize),
		vmQUIT}));
	rcode = r00;
	rip = 0;
	vmRun();
}

int main (int argc, char *argv[]) {
	testInitialize();
	vmInitialize(0, 0);
	// Register external function names
	memTypeStringRegister(TINT,    (Str)"TINT");
	memTypeStringRegister(TREAL,   (Str)"TREAL");
	memTypeStringRegister(TCHAR,   (Str)"TCHAR");
	memTypeStringRegister(TSTR,    (Str)"TSTR");
	memTypeStringRegister(TVEC,    (Str)"TVEC");
	memTypeStringRegister(TTUPLE,  (Str)"TTUPLE");
	memTypeStringRegister(TPAIR,   (Str)"TPAIR");
	MEM_ADDRESS_REGISTER(debugDumpAll);
	MEM_ADDRESS_REGISTER(displayHex11);
	MEM_ADDRESS_REGISTER(displayHexR01);
	MEM_ADDRESS_REGISTER(rcode);
	MEM_ADDRESS_REGISTER(displayHex11016);
	MEM_ADDRESS_REGISTER(displayChar10);

	TEST(TESTStacks);
	TEST(TESTMvLdSt);
	TEST(TESTLdStImm);
	TEST(TESTJal);
	TEST(TESTBlt);
	TEST(TESTfancyHelloWorld);
	TEST(TESTScheduler);
	TEST(TESTObjectDescriptor);
	TEST(TESTTypes);
	TEST(TESTLslLsr);
	TEST(TESTRecursiveLoop);
	TEST(TESTByteLd);
	//TEST(TESTEndlessLoop);
	//memPrintAll(0);
	return 0;
}
