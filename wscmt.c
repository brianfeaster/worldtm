#define DEBUG 0
#define DB_DESC "WSCMTEST "
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include "sys.h"
#include "comp.h"
#include "debug.h"

extern void sysOpenSemaphore (void);
extern void sysIllegalOperator();
extern void sysTGEMutate();
extern void wscmInitialize();
extern void vmVm(Int cmd);


/* Verify goto to an address pointer works.
*/
void testGoto() {
	void **label = &&skip;
	goto **label;
	exit(-1);
	skip:
	return;
}

extern int GarbageCollectionMode;

void objVerifySemaphore (void) {
	objNewInt(7);
	push(r0);
	sysOpenSemaphore();
	//memDebugDumpAll(stdout);
	sysWrite(r0, stdout);
	r0=r1=null;
	GarbageCollectionMode = 1;
   memGarbageCollect();
	//memDebugDumpAll(stdout);
}

/* Numerical equivalence. */
void sysEquals (void) {
	r1=pop();  r0=pop();
	r0 = TINTEGER == memObjectType(r0)
	     && TINTEGER == memObjectType(r1)
	     && *(Int*)r0 == *(Int*)r1
	     ? true : false;
}

void wscmtDisplay (void) {
 Int fd=1;
 FILE *stream=NULL;

	if ((Int)r1==2) fd=*(Int*)pop(); /* Descriptor. */

	if (fd==1) stream = stdout;
	if (fd==2) stream = stderr;
	assert(NULL != stream);

	sysDisplay(r0=pop(), stream);

	return;
}


int main (void) {
	DB ("::%s", __func__);
	setbuf(stdout, NULL);
	printf("\e[25?l");
	testGoto();

	objInitialize();
	compInitialize();

	/* Create empty global environment list. */
	objNewSymbol((Str)"TGE", 3);
	r1=r0;  r2=null;  objCons12();  env=tge=r0;

	wscmDefineSyscall (wscmtDisplay, "display");
	wscmDefineSyscall (sysEquals, "=");

	// Expression to evaluate.
	//yy_scan_string((Str)"(begin (display \"\n\") (fun '*) (display \"The end.\n\"))");
	//yy_scan_string((Str)"(let ~ () (~))");
	//yy_scan_string((Str)"(begin (display stdin) (display (cons '(you typed) (read stdin))))");
	yy_scan_string((Str)"(let ~ ((i 0)(e 9000)) (display i) (display \"\\r\")(if (= i e) (display \"\n\") (~ (+ i 1) e)))");
	//yy_scan_string((Str)"(display (eval '(+ 1 2)))");
	
	yyparse(); //wscmWrite(r0, 0, 1);
	expr=r0; compCompile();

	// Fire up VM.
	sleep(1); vmInterrupt=0; /* Give the interrupt handler time to trigger so forcing it to 0 actually stays 0. */
	code=r0; ip=0; vmRun();
	memGarbageCollect();

	DBE memDebugDumpAll(stdout);
	DBE vmDebugDumpCode(code, stdout);
	objVerifySemaphore();
	DB ("  --%s", __func__);
	return 0;
}

#undef DB_DESC
#undef DEBUG
