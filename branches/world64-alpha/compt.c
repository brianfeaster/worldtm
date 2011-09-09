#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "comp.h"
#include "os.h"
#include "sys.h"
#include "asm.h"
#include "vm.h"


#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))



/* Verify a simple lambda expression compiles and evaluates
*/
void simpleLambda (void) {
	yy_scan_string ((Str)"((lambda () 99))");
	yyparse(); /* Use the internal parser */
	rexpr=r0; compCompile();
	rcode=r0;
	rip=0;
	vmRun();
	assert(99 == *(Int*)r0); /* The expression returns the number 99 */
}


int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	compInitialize();

	TEST(simpleLambda);

	return 0;
}
