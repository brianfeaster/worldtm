#define DEBUG 0
#define DB_MODULE "WSCMTEST "
#include "debug.h"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include "sys.h"


extern void sysIllegalOperator();
extern void sysTGELookup();
extern void sysTGEMutate();
extern void wscmInitialize();
extern void objNewClosure1Env();
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


int main (void) {
	DB ("::%s", __func__);
	setbuf(stdout, NULL);
	printf("\e[25?l");
	testGoto();

	wscmInitialize();

	// Expression to evaluate.
	//yy_scan_string((Str)"(begin (display \"\n\") (fun '*) (display \"The end.\n\"))");
	//yy_scan_string((Str)"(let ~ () (~))");
	//yy_scan_string((Str)"(begin (display stdin) (display (cons '(you typed) (read stdin))))");
	yy_scan_string((Str)"(let ~ ((i 0)(e 9000)) (display i) (display \"\\r\")(if (= i e) (display \"\n\") (~ (+ i 1) e)))");
	//yy_scan_string((Str)"(display (eval '(+ 1 2)))");
	
	yyparse(); //wscmWrite(r0, 0, 1);
	compCompile();

	// Fire up VM.
	sleep(1); interrupt=0; /* Give the interrupt handler time to trigger so forcing it to 0 actually stays 0. */
	code=r0; ip=0; vmRun();
	memGarbageCollect();

	DBE memDebugDumpAll(stdout);
	DBE vmDebugDumpCode(code, stdout);
	DB ("  --%s", __func__);
	return 0;
}

#undef DB_MODULE
