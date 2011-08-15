#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "comp.h"
#include "asm.h"
#include "sys.h"

#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))



int maint (void) {
	return 0;
}


int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	compInitialize();

	TEST(maint);

#if 0
void helloWorld (void) {printf ("\nHello world!\n");}
void displayInteger$0 (void) {printf ("%08x", *(s32*)r0); }
void displayInteger$1 (void) {printf ("%08x", *(s32*)r1); }
void displayString$0  (void) {write (1, r0, memObjectLength(r0)); }
void displayString$1  (void) {write (1, r1, memObjectLength(r1)); }
void displayCString   (void) {printf (r0); }


	/* Assemble a new program. */
	asmAsm(
		SYSI, helloWorld,
		MVI1, r1,
		SYSI, objCopyInteger,
		SYSI, displayInteger$0,
		MVI0, "\n+",
		SYSI, displayCString,
		MVI0, r2,
		SYSI, displayInteger$0,
		ADD10,
		MVI0, "\n=",
		SYSI, displayCString,
		SYSI, displayInteger$1,
		LABEL, "top",
		MVI0, r2,
		ADD10,
		MVI0, "\r",
		SYSI, displayCString,
		SYSI, displayInteger$1,
		BRA, ADDR, "top",
		END
	);
	asmCompileAsmstack(0);
	asmNewCode();
	vmDebugDumpCode(r0,stdout);
	code=r0;  ip=0;  vmRun();
	memGarbageCollect();
	memDebugDumpHeapHeaders(stdout);
	goto done;
	memStackPush(stack, r0);
	memGarbageCollect();
	memStackPop(stack);
	memStackPop(stack);
	memStackPop(stack);
	memGarbageCollect();
	memGarbageCollect();
	memGarbageCollect();
	memGarbageCollect();
#endif

	return 0;
}
