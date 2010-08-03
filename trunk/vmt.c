#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "vm.h"


extern void vmInitialize (Func intHandler, Func preGC, Func postGC, void(*vmObjDumper)(Obj, FILE*));

/* Debugging: Syscalls.
*/
static void displayInteger (void) { printf ("%d", r1); }
static void displayString (void) { printf ("%s", r1); }

/* Debuggin: Registers eventually containing runable code.
*/
#define sub2 r10
#define sub r11

int main (void) {
 int len;
	setbuf(stdout,0);
	vmInitialize(0, 0, 0, 0);

	Obj helloWorld[] = {
		PUSH1,
		MVI1, " ABC",
		MVI0, displayString,
		SYS0,
		MVI1, "  XYZ\n",
		MVI0, displayString,
		SYS0,
		POP1,
		RET};
   memNewVector(TCODE, (len=sizeof(helloWorld))/8); code=r0;
	memcpy(code, helloWorld, len);
	sub2 = r0;


	Obj printNumbers[] = {
		SYSI, displayInteger,
		BEQI1, (Obj)0, (Obj)(2*8),  /* Return if r1==0 */
		BRA, (Obj)(1*8),
		RET,
	
		PUSH1D,              /* Save return address */
		PUSH1E,
	
		MVI0, sub2,
		JAL0,
		MV01,               /* r1-- */
		ADDI0, (Obj)-1,
		MV10,
		MV01C, // Or load address of sub (r11) then index ptr:  MVI0, &sub, LDI00, 0,
		JAL0,
	
		POP1E,               /* Restore return address */
		POP1D,
		RET};
   memNewVector(TCODE, (len=sizeof(printNumbers))/8);
	memcpy(r0, printNumbers, len);
	//vmDebugDump();
	sub = r0;


	Obj mainCode[] = {
		MVI3,  0, /* Outer loop counter */
		MVI1, (Obj)3,
		MVI0, sub,
		JAL0,
	
		MVI2,  (Obj)0, /* Inner loop counter */
		MV02,        /* loop0 */
		MV10,
		SYSI, displayInteger, /* print r2 */
		MVI1, "\n",
		SYSI, displayString,
		MV02, /* Add 1 to r2 */
		ADDI0,  (Obj)1,
		MV20,
		BNEI0,  (Obj)0xa,  (Obj)(-15*8),  /* BNEI r0 0x8000 loop0 */
	
		MVI1, "\n",
		SYSI, displayString,
		MV03, /* Add 1 to r3 */
		ADDI0, (Obj)1,
		MV30,
		BNEI0,  (Obj)3, (Obj)(-33*8), /* BRA to instruction 0 in this code block. */
		QUIT};
   memNewVector(TCODE, (len=sizeof(mainCode))/8);
	memcpy(r0, mainCode, len);
	//vmDebugDump();
	code=r0;

	//memDebugDumpHeapStructures();
	//memGarbageCollect();
	ip=0;
	vmRun();

	return 0;
}

#undef DB_MODULE
