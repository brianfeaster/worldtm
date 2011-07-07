#include <stdarg.h>
#include <string.h>
#include "asm.h"

/* Debugging: Syscalls.
*/
static void asmtDisplayInteger (void) { printf (INT, r1); }
static void asmtDisplayString (void) { printf ("%s", r1); }
static void asmtDisplayNewline (void) { printf ("\n"); }
static void asmtVmDebugDumpCode (void) { memDebugDumpAll(stdout); vmDebugDumpCode(code, stdout); }

int main (void) {
 char *welcomemsg="asmt.c  Unit test for asm module.";
	setbuf(stdout, NULL);
	asmInitialize(NULL, NULL, NULL, NULL); /* intHandler preGC postGC vmObjDumper */

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
		MVI0, (Obj)0,
	LABEL, "main",
		MV10,
	LABEL, "loop",
		SYSI, asmtDisplayInteger,
		PUSH1, MVI1, " ", SYSI, asmtDisplayString, POP1, /* Space */
		ADDI1, (Obj)1,
		BEQI1, (Obj)10, ADDR, "loopdone",
		BRA, ADDR, "loop",
	LABEL, "loopdone",
		MVI1, "\r\n", SYSI, asmtDisplayString,
		ADDI0, (Obj)1,
		BNEI0, (Obj)10, ADDR, "main",
	LABEL, "done",
		SYSI, asmtVmDebugDumpCode,
		QUIT,
		END
	);

	/* Compile the assembly.  Dump asmstack before and after. */
	memDebugDumpObject(asmstack, stdout);
	asmCompileAsmstack(0);
	memDebugDumpObject(asmstack, stdout);

	/* Create new code object from asmstack into r0. */
	asmNewCode();

	/* Dump assembled code object. */
	memDebugDumpObject(r0, stdout);

	/* Run the code twice. */
	code = r0;
	ip=0; vmRun();
	memGarbageCollect();
	ip=0; vmRun();

	//memDebugDumpAll();
	//vmDebugDumpCode(code);

	return 0;
}
