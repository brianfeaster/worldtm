#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include "asm.h"
#include "vm.h"
#include "mem.h"

#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))




Int bcount;
char buff[4096];
static void asmtDisplayInteger (void) { bcount += sprintf (buff+bcount, INT, r1); }
static void asmtDisplayString (void) { bcount += sprintf (buff+bcount, "%s", r1); }
static void asmtDisplayNewline (void) { bcount += sprintf (buff+bcount, "\n"); }
static void asmtVmDebugDumpCode (void) {
	if (0) {
		memDebugDumpAll(stdout);
		vmDebugDumpCode(rcode, stdout);
	}
}



int myTest (void) {
 char *welcomemsg="Hello, unit test!!!";
 Num programOutputStringCompare;
	setbuf(stdout, NULL);

	memObjStringSet (asmtDisplayString);
	memObjStringSet (asmtDisplayInteger);
	memObjStringSet (asmtDisplayNewline);
	memObjStringSet (asmtVmDebugDumpCode);
	memObjStringSet (welcomemsg);

	/* Create the assembly.
	*/
	asmAsm(
		SYSI, asmtDisplayNewline,
		MVI1, welcomemsg, SYSI, asmtDisplayString,
		SYSI, asmtDisplayNewline,
		MVI0, 0l,
	LABEL, "main",
		MV10,
	LABEL, "loop",
		SYSI, asmtDisplayInteger,
		PUSH1,
			MVI1, " ", SYSI, asmtDisplayString, /* Space */
		POP1,
		ADDI1, 1l,
		BEQI1, 10l, ADDR, "loopdone",
		BRA, ADDR, "loop",
	LABEL, "loopdone",
		MVI1, "\r\n", SYSI, asmtDisplayString,
		ADDI0, 1l,
		BNEI0, 10l, ADDR, "main",
	LABEL, "done",
		SYSI, asmtVmDebugDumpCode,
		QUIT,
		END
	);

	/* Compile the assembly */
	asmCompileAsmstack(0);

	/* Create new code object from asmstack into r0 */
	asmNewCode();

	/* Run the code twice. */
	rcode = r0;
	rip=0; vmRun();

	memGarbageCollect();
	rip=0; vmRun();

	programOutputStringCompare = strcmp(buff,
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

	assert(!programOutputStringCompare);
	return 0;
}


int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	asmInitialize();

	TEST(myTest);

	return 0;
}
