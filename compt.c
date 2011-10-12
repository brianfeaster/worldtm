#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "comp.h"
#include "obj.h"
#include "sys.h"
#include "asm.h"
#include "vm.h"


extern void asmICodePushNewQUIT (void);
extern void asmGenerateIBlockWithPushedIcodes ();
#define rexpr       rf /* Expression being compiled.  See vm.h */


#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))


/* Verify a simple lambda expression compiles and evaluates
*/
void simpleLambda (void) {

	asmInit();
	asmICodePushNewQUIT();
	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();
	asmAsmIGraph();
	rretcode = r0;
	rretip = 0;

	yy_scan_string ((Str)"((lambda () 99))");
	yyparse(); /* Use the internal parser */
	compCompile();
	rcode=r0;
	rip=0;
	vmRun();
	assert(99 == *(Int*)r0); /* The expression returns the number 99 */
}


void aif (void) {
	asmInit();
	asmICodePushNewQUIT();
	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();
	asmAsmIGraph();
	rretcode = r0;
	rretip = 0;

	yy_scan_string ((Str)"(let ~ () (=> 9 (lambda (x) x) 8))");
	yyparse(); /* Use the internal parser */
	compCompile();
	rcode=r0;
	rip=0;
	vmRun();
	assert(9 == *(Int*)r0); /* The expression returns the number 99 */
}

void testif (void) {
	asmInit();
	asmICodePushNewQUIT();
	asmICodePushNewQUIT();
	asmGenerateIBlockWithPushedIcodes();
	asmAsmIGraph();
	rretcode = r0;
	rretip = 0;

	yy_scan_string ((Str)"(if #t (not #t) 2)");
	yyparse(); /* Use the internal parser */
	compCompile();
	rcode=r0;
	rip=0;
	vmRun();
	sysDisplay(r0, stderr);
	assert(false == r0); /* The expression returns the number 99 */
}


int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	compInitialize();

	TEST(simpleLambda);
	TEST(aif);
	TEST(testif);

	return 0;
}
