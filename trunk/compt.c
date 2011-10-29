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
#include "mem.h"


extern Obj rexpr;
extern Num compParseOperands (Num count);
extern void asmICodePushNewQUIT (void);
extern Num matchArgumentList (void); 
extern Num compParseTransformProcedure (void);
extern Num compParseTransformDefine (void);

Num compIsError (void);

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
 Num res;
	fflush(FB);
	res = (Num)strcmp(FBBuff, goldenString);
	if (res) fprintf(stderr, "\nReceived [%s]\nExpected [%s] ", FBBuff, goldenString);
	assert(0 == res);
	fclose(FB);
	free(FBBuff);
}



/* Verify the argument list can be parsed and errors detected
*/

void matchargs (void) {
	yy_scan_string ((Str)"()"); yyparse();
	assert(!matchArgumentList());
	assert(null == car(r1));
	assert(null == r2);

	yy_scan_string ((Str)"r"); yyparse();
	assert(!matchArgumentList());
	objNewSymbol((Str)"r", 1);
	assert(r0 == car(r1));
	assert(r0 == r2);

	yy_scan_string ((Str)"(x)"); yyparse();
	assert(!matchArgumentList());
	objNewSymbol((Str)"x", 1);
	assert(r0 == car(r1));
	assert(null == r2);

	yy_scan_string ((Str)"(x . y)"); yyparse();
	assert(!matchArgumentList());
	objNewSymbol((Str)"x", 1);
	assert(r0 == car(r1));
	objNewSymbol((Str)"y", 1);
	assert(r0 == r2);

	yy_scan_string ((Str)"(x y . r)"); yyparse();
	assert(!matchArgumentList());
	objNewSymbol((Str)"x", 1);
	assert(r0 == car(r1));
	objNewSymbol((Str)"y", 1);
	assert(r0 == cadr(r1));
	objNewSymbol((Str)"r", 1);
	assert(r0 == r2);

	yy_scan_string ((Str)"(())"); yyparse();
	assert(matchArgumentList());

	yy_scan_string ((Str)"1"); yyparse();
	assert(matchArgumentList());

	yy_scan_string ((Str)"(1)"); yyparse();
	assert(matchArgumentList());

	yy_scan_string ((Str)"(a 1)"); yyparse();
	assert(matchArgumentList());

	yy_scan_string ((Str)"(1 a)"); yyparse();
	assert(matchArgumentList());

	yy_scan_string ((Str)"(a b 1)"); yyparse();
	assert(matchArgumentList());

	yy_scan_string ((Str)"(a . 1)"); yyparse();
	assert(matchArgumentList());

	yy_scan_string ((Str)"(a b . 1)"); yyparse();
	assert(matchArgumentList());
}

/* Verify a lambda expressions can be parsed and errors detected
*/
void parselambda (void) {
	yy_scan_string ((Str)"(())"); yyparse(); rexpr=r0;

	assert(!compParseTransformProcedure()); // r1=(()) r2=() r3=() r4=()
	assert(null == car(r1));
	assert(null == r2);
	assert(null == r3);
	assert(null == r4);

	yy_scan_string ((Str)"(r b)"); yyparse(); rexpr=r0;
	assert(!compParseTransformProcedure()); // r1=(r) r2=r r3=() r4=b
	objNewSymbol((Str)"r", 1); assert(r0 == car(r1));
	                           assert(r0 == r2);
	                           assert(null == r3);
	objNewSymbol((Str)"b", 1); assert(r0 == r4);

	yy_scan_string ((Str)"((x) a b)"); yyparse(); rexpr=r0;
	assert(!compParseTransformProcedure()); // r1=(x) r2=() r3=(a) r4=b
	objNewSymbol((Str)"x", 1); assert(r0 == car(r1));
	                           assert(null == r2);
	objNewSymbol((Str)"a", 1); assert(r0 == car(r3));
	objNewSymbol((Str)"b", 1); assert(r0 == r4);

	/* Malformed lambda expression body */
	compErrorReset();
	yy_scan_string ((Str)"((x) . b)"); yyparse(); rexpr=r0;
	assert(compParseTransformProcedure());

	/* Malformed lambda expression formals and body */
	compErrorReset();
	yy_scan_string ((Str)"((1) . 2)"); yyparse(); rexpr=r0;
	assert(compParseTransformProcedure());
}

/* A C function to verify and parse an s-expression operands
*/
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


/* Verify a malformed lambda expression can be identified
*/
void lambdamalformed (void) {
	FBInit();
	yy_scan_string ((Str)"(lambda () . 1)"); yyparse(); compCompile();
	FBFinalize("(Syntax error procedure body 1 (lambda () . 1))");

	FBInit();
	yy_scan_string ((Str)"(lambda () 1 . 2)"); yyparse(); compCompile();
	FBFinalize("(Syntax error procedure body 2 (lambda () 1 . 2))");

	FBInit();
	yy_scan_string ((Str)"(lambda r . 1)"); yyparse(); compCompile();
	FBFinalize("(Syntax error procedure body 1 (lambda r . 1))");

	FBInit();
	yy_scan_string ((Str)"(lambda r 1 . 2)"); yyparse(); compCompile();
	FBFinalize("(Syntax error procedure body 2 (lambda r 1 . 2))");
                 
	FBInit();
	yy_scan_string ((Str)"(lambda (x) . 1)"); yyparse(); compCompile();
	FBFinalize("(Syntax error procedure body 1 (lambda (x) . 1))");

	FBInit();
	yy_scan_string ((Str)"(lambda (x) 1 . 2)"); yyparse(); compCompile();
	FBFinalize("(Syntax error procedure body 2 (lambda (x) 1 . 2))");

}


void parseDefine (void) {
	yy_scan_string ((Str)"(x 9)"); yyparse();  rexpr=r0;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"x", 1); assert(r0 == r1);
	objNewInt(9); assert(r0 == r2); /* Works because integers are cached between +/- 1024 */

	FBInit();
	yy_scan_string ((Str)"((f) 9)"); yyparse();  rexpr=r0;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"f", 1); assert(r0 == r1);
	sysDisplay(r2, FB);  FBFinalize("(lambda () 9)");

	FBInit();
	yy_scan_string ((Str)"((f g) 9 8)"); yyparse();  rexpr=r0;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"f", 1); assert(r0 == r1);
	sysDisplay(r2, FB);  FBFinalize("(lambda (g) 9 8)");

	FBInit();
	yy_scan_string ((Str)"((f . a) 9)"); yyparse();  rexpr=r0;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"f", 1); assert(r0 == r1);
	sysDisplay(r2, FB);  FBFinalize("(lambda a 9)");

	FBInit();
	yy_scan_string ((Str)"((f . a) 9)"); yyparse();  rexpr=r0;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"f", 1); assert(r0 == r1);
	sysDisplay(r2, FB);  FBFinalize("(lambda a 9)");

	FBInit();
	yy_scan_string ((Str)"((f . a))"); yyparse();  rexpr=r0;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"f", 1); assert(r0 == r1);
	sysDisplay(r2, FB);  FBFinalize("(lambda a)");
}


/* Verify a simple lambda expression compiles and evaluates
*/
void compilerunsimpleLambda (void) {

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


void compilerunaif (void) {
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
	assert(9 == *(Int*)r0); /* The expression returns the number 9 */
}


void compilerunif (void) {
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


/* 4 unit tests that verify runtime car argument checks occur in
   various locations in an expression.
*/
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
	yy_scan_string ((Str)"(cons 1 (car (car (if 2 (car 9)))))"); /* This leaves object integer 1 on the stack */
	yyparse();
	compCompile();
	rcode = r0;
	rip = 0;
	vmRun();
	FBFinalize("(car expects pair for target 9 (car 9) (if 2 (car 9)) (car (if 2 (car 9))) (car (car (if 2 (car 9)))) (cons 1 (car (car (if 2 (car 9))))))");
	vmPop();

	FBInit();
	yy_scan_string ((Str)"(car (car 9))");
	yyparse();
	compCompile();
	rcode = r0;
	rip = 0;
	vmRun();
	FBFinalize("(car expects pair for target 9 (car 9) (car (car 9)))");
}

extern Obj rcomperrortrace;
extern Obj rcomperrormessage;
void parsepushoperands (void) {
 Num operandCount;

	FBInit();
	yy_scan_string ((Str)"((lambda 1))");  yyparse();  rexpr = r0;
	compCompile();
	assert(compIsError());
	FBFinalize("(Syntax error procedure args 1 ((lambda 1)))");

	renv = rtge;
	FBInit();
	yy_scan_string ((Str)"((lambda (x) x)z)");  yyparse();  rexpr = r0;
	compCompile();
	assert(!compIsError());
	rcode = r0;
	rip = 0;
	vmRun();
	FBFinalize("(Unbound symbol z)");

	yy_scan_string ((Str)"((lambda (x) x)99)");  yyparse();  rexpr = r0;
	compCompile();
	assert(!compIsError());
	rcode = r0;
	rip = 0;
	vmRun();
	assert(*(Num*)r0 == 99l);
}


int main (int argc, char *argv[]) {
	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	compInitialize();
	osInitialize(exceptionHandler);

	assert(0 == memStackLength(rstack));

	TEST(matchargs);
	TEST(parselambda);
	TEST(parseOperands);
	TEST(lambdamalformed);
	TEST(parseDefine);
	TEST(compilerunsimpleLambda);
	TEST(compilerunaif);
	TEST(compilerunif);
	TEST(errorcar);
	TEST(errorbegincar);
	TEST(errorconscarcarifcar);
	TEST(runtimeerrorcarcar);
	TEST(parsepushoperands);

	assert(0 == memStackLength(rstack));
	return 0;
}
