#define DEBUG 0
#define DB_DESC "WSCMTEST "
#include "debug.h"
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
	assert(r0 = true);
	assert(0 == (Int)car(r1));

	sysSemaphoreDown();
	assert(r0 = r1);
	assert(-1 == (Int)car(r1));

	sysSemaphoreUp();
	assert(r0 = r1);
	assert(0 == (Int)car(r1));

	sysSemaphoreUp();
	assert(r0 = true);
	assert(1 == (Int)car(r1));
}

/* Numerical equivalence. */
void sysEquals (void) {
	r1=vmPop();  r0=vmPop();
	r0 = TINTEGER == memObjectType(r0)
	     && TINTEGER == memObjectType(r1)
	     && *(Int*)r0 == *(Int*)r1
	     ? true : false;
}

void wscmtDisplay (void) {
 Int fd=1;
 FILE *stream=NULL;

	if ((Int)r1==2) fd=*(Int*)vmPop(); /* Descriptor. */

	if (fd==1) stream = stdout;
	if (fd==2) stream = stderr;
	assert(NULL != stream);

	sysDisplay(r0=vmPop(), stream);

	return;
}


int main (void) {
	setbuf(stdout, NULL);
	testGoto();

	compInitialize();

	/* Create empty global environment list. */
	objNewSymbol((Str)"TGE", 3);
	r1=r0;  r2=null;  objCons12();  renv=rtge=r0;

	sysDefineSyscall (wscmtDisplay, "display");
	sysDefineSyscall (sysEquals, "=");

	//yy_scan_string((Str)"(display (eval '(+ 1 2)))"); eval is a good one to test
	yy_scan_string((Str)"(let ~ ((i 0)(e 9000)) (display i) (display \"\\r\")(if (= i e) (display \"\n\") (~ (+ i 1) e)))");
	
	yyparse(); //wscmWrite(r0, 0, 1);
	rexpr=r0; compCompile();


	// Fire up VM.
	sleep(1); vmInterrupt=0; /* Give the interrupt handler time to trigger so forcing it to 0 actually stays 0. */
	rcode=r0; rip=0; vmRun();
	memGarbageCollect();

	DBE memDebugDumpAll(stdout);
	DBE vmDebugDumpCode(rcode, stdout);

	objVerifySemaphore();

	return 0;
}

#undef DB_DESC
#undef DEBUG