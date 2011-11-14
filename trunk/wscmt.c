#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include "comp.h"
#include "os.h"
#include "sys.h"
#include "obj.h"
#include "asm.h"
#include "vm.h"
#include "mem.h"
#include "test.h"


/*******************************************************************************
 TESTS
*******************************************************************************/
/* Verify goto to an address pointer works.
*/
void testGoto() {
	void **label = &&skip;
	goto **label;
	exit(-1);
	skip:
	return;
}


/* Verify a semaphore can be created, decremented and incremented
   with expected return values in r0 signalling that the semaphore
   was successful or has passed its threshold.
*/
void objVerifySemaphore (void) {
	/* Create the semaphore in r1 with initial counter at 1 */
	objNewInt(1);
	vmPush(r0);
	osOpenSemaphore();

	r1 = r0;
	assert(1 == (Int)car(r1));

	sysSemaphoreDown();
	assert(r0 = otrue);
	assert(0 == (Int)car(r1));

	sysSemaphoreDown();
	assert(r0 = r1);
	assert(-1 == (Int)car(r1));

	sysSemaphoreUp();
	assert(r0 = r1);
	assert(0 == (Int)car(r1));

	sysSemaphoreUp();
	assert(r0 = otrue);
	assert(1 == (Int)car(r1));
}


/* Numerical equivalence. */
void sysEquals (void) {
	r1=vmPop();  r0=vmPop();
	r0 = TINTEGER == memObjectType(r0)
	     && TINTEGER == memObjectType(r1)
	     && *(Int*)r0 == *(Int*)r1
	     ? otrue : ofalse;
}

void sysAdd (void) {
	r1=vmPop();
	r0=vmPop();
	objNewInt(*(Int*)r0 + *(Int*)r1);
}

void wscmtDisplay (void) {
 Int fd=1;
 FILE *stream=NULL;

	if ((Int)r1==2) fd=*(Int*)vmPop(); /* Descriptor. */

	if (fd==1) stream = stdout;
	if (fd==2) stream = stderr;
	assert(NULL != stream);

	objDisplay(r0=vmPop(), stream);

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
	rretcode = r0;
	rretip = (Obj)(-1 * 8);
	rretenv = renv;

	yy_scan_string((Str)"(let ((v (eval '(+ 42 69)))) v)");
	yyparse(); compCompile(); rcode=r0; rip=0; vmRun();
	assert(*(Num*)r0 == 111);

	yy_scan_string((Str)"(let ~ ((i 0)(e 9000)) (display \"\\e7\") (display i) (display \"\\e8\") (if (= i e) e (~ (+ i 1) e)))");
	yyparse(); compCompile(); rcode=r0; rip=0; vmRun();
	assert(*(Num*)r0 == 9000);

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
