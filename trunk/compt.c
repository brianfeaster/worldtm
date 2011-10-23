#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "comp.h"
#include "asm.h"
#include "os.h"
#include "sys.h"
#include "obj.h"
#include "vm.h"


extern void asmICodePushNewQUIT (void);


#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))

/* A character file buffer and the functions that print to it
*/
FILE *FB;
char *FBBuff=NULL;

/* Initialize character file buffer */
void FBInit (void) {
 static Num size;
	FB = open_memstream(&FBBuff, &size);
	assert(NULL != FB);
}

/* Dump character file buffer's contents.  Finalize related objects. */
void FBDump () {
	fflush(FB);
	fclose(FB);
	fprintf(stderr, FBBuff);
	free(FBBuff);
}

/* Compare character file buffer's contents with string argument. Finalize related objects. */
void FBFinalize (char *goldenString) {
	fflush(FB);
	assert(0 == strcmp(FBBuff, goldenString));
	fclose(FB);
	free(FBBuff);
}



/* A C function to verify and parse an s-expression operands
*/
extern Obj rexpr, rcomperror;
extern Num compParseOperands (Num count);

void parseOperands (void) {
	/* Parse a 0 operand expression */
	yy_scan_string ((Str)"(fun0)"); yyparse();
	rexpr = r0;
	assert(0 == compParseOperands(0));
	assert((Obj)0 == r0);

	/* Parse a 1 operand expression */
	yy_scan_string ((Str)"(fun1 a)"); yyparse();
	rexpr = r0;
	assert(0 == compParseOperands(1));
	assert((Obj)1 == r0);
	objNewSymbol((Str)"a", 1);
	assert(r0 == r1);

	/* Parse a 2 operand expression */
	yy_scan_string ((Str)"(fun1 a b)"); yyparse();
	rexpr = r0;
	assert(0 == compParseOperands(2));
	assert((Obj)2 == r0);
	objNewSymbol((Str)"a", 1); assert(r0 == r1);
	objNewSymbol((Str)"b", 1); assert(r0 == r2);

	/* Parse a 3 operand expression */
	yy_scan_string ((Str)"(fun1 a b c)");
	yyparse();
	rexpr = r0;
	assert(0 == compParseOperands(3));
	assert((Obj)3 == r0);
	objNewSymbol((Str)"a", 1); assert(r0 == r1);
	objNewSymbol((Str)"b", 1); assert(r0 == r2);
	objNewSymbol((Str)"c", 1); assert(r0 == r3);

	/* Parse a 4 operand expression */
	yy_scan_string ((Str)"(fun1 a b c d)");
	yyparse();
	rexpr = r0;
	assert(0 == compParseOperands(4));
	assert((Obj)4 == r0);
	objNewSymbol((Str)"a", 1); assert(r0 == r1);
	objNewSymbol((Str)"b", 1); assert(r0 == r2);
	objNewSymbol((Str)"c", 1); assert(r0 == r3);
	objNewSymbol((Str)"d", 1); assert(r0 == r4);

	/* Verify malformed expressions are caught
	*/
	yy_scan_string ((Str)"(fun0 . error)"); yyparse();
	rexpr = r0;
	assert(compParseOperands(0));

	yy_scan_string ((Str)"(fun1 a . error)"); yyparse();
	rexpr = r0;
	assert(compParseOperands(1));

	yy_scan_string ((Str)"(fun2 a b . error)"); yyparse();
	rexpr = r0;
	assert(compParseOperands(2));

	yy_scan_string ((Str)"(fun3 a b c . error)"); yyparse();
	rexpr = r0;
	assert(compParseOperands(3));

	r0=r1=r2=r3=r4=r5=0;
	yy_scan_string ((Str)"(fun4 a b c d . error)"); yyparse();
	rexpr = r0;
	assert(compParseOperands(4));
}


/* Verify a simple lambda expression compiles and evaluates
*/
void simpleLambda (void) {

	asmInit();
	asmICodePushNewQUIT();
	asmICodePushNewQUIT();
	asmAssemble();
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
	asmAssemble();
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
	asmAssemble();
	rretcode = r0;
	rretip = 0;

	yy_scan_string ((Str)"(if #t (not #t) 2)");
	yyparse(); /* Use the internal parser */
	compCompile();
	rcode=r0;
	rip=0;
	vmRun();
	//sysDisplay(r0, stderr);
	assert(false == r0);
}


/* This is registered with the os module's exception handler.  It will be
   called if a compiler error occurs with a list in r0 continaing the
   error message and relevant s-expressions from the compiled program.

   It expect FB to be initialized then finalized.  See FB's character file
   buffer code above.
*/
void exceptionHandler (void) {
	sysDisplay(r0, FB);
}

void errorcar (void) {
	FBInit();
	yy_scan_string ((Str)"(car)");
	yyparse();
	compCompile();
	assert(false == r0);
	FBFinalize("(Syntax error 'car' (car))");
}

void errorbegincar (void) {
	FBInit();
	yy_scan_string ((Str)"(begin 1 (car))");
	yyparse();
	compCompile();
	assert(false == r0);
	FBFinalize("(Syntax error 'car' (car) (begin 1 (car)))");


	FBInit();
	yy_scan_string ((Str)"(begin (car) 2)");
	yyparse();
	compCompile();
	assert(false == r0);
	FBFinalize("(Syntax error 'car' (car) (begin (car) 2))");
}

void errorconscarcarifcar (void) {
	FBInit();
	yy_scan_string ((Str)"(cons 1 (car (car (if 1 (car)))))");
	yyparse();
	compCompile();
	assert(false == r0);
	FBFinalize("(Syntax error 'car' (car) (if 1 (car)) (car (if 1 (car))) (car (car (if 1 (car)))) (cons 1 (car (car (if 1 (car))))))");

}

void runtimeerrorcarcar (void) {
	FBInit();
	yy_scan_string ((Str)"(cons 1 (car (car (if 1 (car 9)))))"); /* This leaves object integer 1 on the stack */
	yyparse();
	compCompile();
	rcode = r0;
	rip = 0;
	vmRun();
	FBFinalize("(car expects pair for target 9 (car 9) (if 1 (car 9)) (car (if 1 (car 9))) (car (car (if 1 (car 9)))) (cons 1 (car (car (if 1 (car 9))))))");

	FBInit();
	yy_scan_string ((Str)"(car (car 9))");
	yyparse();
	compCompile();
	rcode = r0;
	rip = 0;
	vmRun();
	//FBDump();
	FBFinalize("(car expects pair for target 9 (car 9) (car (car 9)))");
}


int main (int argc, char *argv[]) {
	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	compInitialize();
	osInitialize(exceptionHandler);

	TEST(parseOperands);
	TEST(simpleLambda);
	TEST(aif);
	TEST(testif);
	TEST(errorcar);
	TEST(errorbegincar);
	TEST(errorconscarcarifcar);
	TEST(runtimeerrorcarcar);
	return 0;
}
