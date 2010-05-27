#define DEBUG 0
#define DB_MODULE "WSCMTEST "
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include "scanner.h"
#include "obj.h"
#include "comp.h"


extern int interrupt;
extern void sysIllegalOperator();
extern void sysTGELookup();
extern void sysTGEMutate();
extern void wscmError();
extern void wscmInitialize();
extern void objNewClosure1Env();
extern void vmVm(Int cmd);
extern void wscmWrite (Obj a, long islist, Int fd);


void wscmTestInitialize() {
	/* Initialize obj, asm, and mem modules. */
	objInitialize(NULL); /* Pass a NULL interrupt handler.
	/* Create empty global environment list. */
	objNewSymbol("TGE", 3);
	r1=r0;  r2=null;  objCons12();  env=tge=r0;
}


/* Verify goto to an address pointer works.
*/
void testGoto() {
	void **label = &&skip;
	goto **label;
	exit(-1);
	skip:
	return;
}


int main (void) {
	//wscmTestInitialize();
	wscmInitialize();
	vmVm(0);
	setbuf(stdout, NULL);
	yy_scan_string("(begin (fun '*) (display \"The end.\n\"))");
	/* Infinite loop.
	yy_scan_string("(let ~ () (~))");
	*/
	yyparse();
	wscmWrite(r0, 0, 1);
	compCompile();

	testGoto();

	DB("Address: sysIllegalOperator = %p", sysIllegalOperator);
	DB("Address: sysTGELookup = %p", sysTGELookup);
	DB("Address: sysTGEMutate = %p", sysTGEMutate);
	DB("Address: wscmError = %p", wscmError);
	DB("Address: objNewClosure1Env = %p", objNewClosure1Env);

	// Fire up VM.
	interrupt=0;
	code=r0; ip=0; vmRun();

	memDebugDumpAll();
	return 0;
}
