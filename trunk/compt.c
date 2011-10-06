#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "comp.h"
#include "os.h"
#include "sys.h"
#include "asm.h"
#include "vm.h"


#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))

#define rexpr       rf /* Expression being compiled.  See vm.h */


/* Verify a simple lambda expression compiles and evaluates
*/
void simpleLambda (void) {

	asmInit();
	ccICodePushNewQUIT();
	ccICodePushNewQUIT();
	ccGenerateIBlockWithPushedIcodes();
	asmAsmIGraph();
	rretcode = r0;
	rretip = 0;

	yy_scan_string ((Str)"((lambda () 99))");
	yyparse(); /* Use the internal parser */
	ccCompile();
	rcode=r0;
//vmDebugDumpCode(rcode, stderr);
//memDebugDumpAll(stderr);
	rip=0;
	vmRun();
	assert(99 == *(Int*)r0); /* The expression returns the number 99 */
}


int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	ccInitialize();

	TEST(simpleLambda);

	return 0;
}
