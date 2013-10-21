#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include "mem.h"
#include "vm.h"
#include "asm.h"
#include "obj.h"
#include "sys.h"
#include "os.h"
#include "comp.h"
#include "test.h"


/*******************************************************************************
 TESTS
*******************************************************************************/
/* Verify goto to an address pointer works.
*/
void testGoto() {
	void **lab=0;
	lab = &&skip;
	goto *lab;
	exit(-1);
	skip:
	return;
}


/* step hander.  Perform the following at the start of a test

vmInitialize(wscmStepHandler, NULL);
vmInterrupt = 1;
static void wscmStepHandler (void) {
	memDebugDumpHeapHeaders(stdout);
	vmDisplayTypeCode(rcode, stdout);
	getchar();
	vmInterrupt = 1; // Force interrupt after next instruction to this stepHandler
}
*/

/* Verify a semaphore can be created, decremented and incremented
   with expected return values in r00 signalling that the semaphore
   was successful or has passed its threshold.
*/
void objVerifySemaphore (void) {
	/* Create the semaphore in r01 with initial counter at 1 */
	objNewInt(1);
	vmPush(r00);
	osOpenSemaphore();

	r01 = r00;
	assert(1 == (Int)car(r01));

	sysSemaphoreDown();
	assert(r00 = otrue);
	assert(0 == (Int)car(r01));

	sysSemaphoreDown();
	assert(r00 = r01);
	assert(-1 == (Int)car(r01));

	sysSemaphoreUp();
	assert(r00 = r01);
	assert(0 == (Int)car(r01));

	sysSemaphoreUp();
	assert(r00 = otrue);
	assert(1 == (Int)car(r01));
}


/* Numerical equivalence. */
void sysEquals (void) {
	r01=vmPop();  r00=vmPop();
	r00 = TINTEGER == memObjectType(r00)
	     && TINTEGER == memObjectType(r01)
	     && *(Int*)r00 == *(Int*)r01
	     ? otrue : ofalse;
}

void sysAdd (void) {
	r01=vmPop();
	r00=vmPop();
	objNewInt(*(Int*)r00 + *(Int*)r01);
}

void wscmtDisplay (void) {
 Int fd=1;
 FILE *stream=NULL;

	if ((Int)r01==2) fd=*(Int*)vmPop(); /* Descriptor. */

	if (fd==1) stream = stdout;
	if (fd==2) stream = stderr;
	assert(NULL != stream);

	objDisplay(r00=vmPop(), stream);

	return;
}


void stuff (void) {
	sysDefineSyscall (wscmtDisplay, "display");
	sysDefineSyscall (sysEquals, "=");
	sysDefineSyscall (sysAdd, "+");

	/* Create a code block that will be returned to after a 'ret' call */
	asmInit();
	asmAsm(QUIT);
	asmAssemble();
	rcodelink = r00;
	riplink = (Obj)(0 * ObjSize);
	renvlink = renv;

	yy_scan_string((Str)"(let ((v (eval '(+ 42 69)))) v)");
	yyparse(); compCompile(); rcode=r00; rip=0; vmRun();
	assert(*(Num*)r00 == 111);

	yy_scan_string((Str)"(let ~ ((i 0)(e 9000)) (display \"\\e7\") (display i) (display \"\\e8\") (if (= i e) e (~ (+ i 1) e)))");
	yyparse(); compCompile(); rcode=r00; rip=0; vmRun();
	assert(*(Num*)r00 == 9000);

	memGarbageCollect();
}


int main (void) {
	compInitialize(); /* asm vm mem os sys obj */
	testInitialize();
	TEST(testGoto);
	TEST(objVerifySemaphore);
	TEST(stuff);
	return 0;
}
