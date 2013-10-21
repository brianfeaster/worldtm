#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "mem.h"
#include "vm.h"
#include "obj.h"
#include "os.h"
#include "asm.h"
#include "comp.h"
#include "sys.h"
#include "test.h"


extern Obj rexpr;
extern Num compParseOperands (Num count);
extern void asmICodePushNewQUIT (void);
extern Num matchArgumentList (void); 
extern Num compParseTransformProcedure (void);
extern Num compParseTransformDefine (void);
extern void compErrorReset (void);

extern Num compIsError (void);

/* VM step hander.  Perform the following at the start of a test

vmInitialize(comptStepHandler, NULL);
vmInterrupt = 1;
static void comptStepHandler (void) {
	memDebugDumpHeapHeaders(stdout);
	vmDisplayTypeCode(rcode, stdout);
	getchar();
	vmInterrupt = 1; // Force interrupt after next instruction to this stepHandler
}
*/

/*******************************************************************************
 TESTS
*******************************************************************************/
/* Verify the argument list can be parsed and errors detected
*/

void matchargs (void) {
	yy_scan_string ((Str)"()"); yyparse();
	assert(!matchArgumentList());
	assert(onull == car(r01));
	assert(onull == r02);

	yy_scan_string ((Str)"r"); yyparse();
	assert(!matchArgumentList());
	objNewSymbol((Str)"r", 1);
	assert(r00 == car(r01));
	assert(r00 == r02);

	yy_scan_string ((Str)"(x)"); yyparse();
	assert(!matchArgumentList());
	objNewSymbol((Str)"x", 1);
	assert(r00 == car(r01));
	assert(onull == r02);

	yy_scan_string ((Str)"(x . y)"); yyparse();
	assert(!matchArgumentList());
	objNewSymbol((Str)"x", 1);
	assert(r00 == car(r01));
	objNewSymbol((Str)"y", 1);
	assert(r00 == r02);

	yy_scan_string ((Str)"(x y . r)"); yyparse();
	assert(!matchArgumentList());
	objNewSymbol((Str)"x", 1);
	assert(r00 == car(r01));
	objNewSymbol((Str)"y", 1);
	assert(r00 == cadr(r01));
	objNewSymbol((Str)"r", 1);
	assert(r00 == r02);

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
	yy_scan_string ((Str)"(())"); yyparse(); rexpr=r00;

	assert(!compParseTransformProcedure()); // r01=(()) r02=() r03=() r04=()
	assert(onull == car(r01));
	assert(onull == r02);
	assert(onull == r03);
	assert(onull == r04);

	yy_scan_string ((Str)"(r b)"); yyparse(); rexpr=r00;
	assert(!compParseTransformProcedure()); // r01=(r) r02=r r03=() r04=b
	objNewSymbol((Str)"r", 1); assert(r00 == car(r01));
	                           assert(r00 == r02);
	                           assert(onull == r03);
	objNewSymbol((Str)"b", 1); assert(r00 == r04);

	yy_scan_string ((Str)"((x) a b)"); yyparse(); rexpr=r00;
	assert(!compParseTransformProcedure()); // r01=(x) r02=() r03=(a) r04=b
	objNewSymbol((Str)"x", 1); assert(r00 == car(r01));
	                           assert(onull == r02);
	objNewSymbol((Str)"a", 1); assert(r00 == car(r03));
	objNewSymbol((Str)"b", 1); assert(r00 == r04);

	/* Malformed lambda expression body */
	compErrorReset();
	yy_scan_string ((Str)"((x) . b)"); yyparse(); rexpr=r00;
	assert(compParseTransformProcedure());

	/* Malformed lambda expression formals and body */
	compErrorReset();
	yy_scan_string ((Str)"((1) . 2)"); yyparse(); rexpr=r00;
	assert(compParseTransformProcedure());
}

/* A C function to verify and parse an s-expression operands
*/
void parseOperands (void) {
	/* Parse a 0 operand expression */
	yy_scan_string ((Str)"(fun0)"); yyparse();
	rexpr = r00;
	assert(0 == compParseOperands(0));
	assert((Obj)0 == r00);

	/* Parse a 1 operand expression */
	yy_scan_string ((Str)"(fun1 a)"); yyparse();
	rexpr = r00;
	assert(0 == compParseOperands(1));
	assert((Obj)1 == r00);
	objNewSymbol((Str)"a", 1);
	assert(r00 == r01);

	/* Parse a 2 operand expression */
	yy_scan_string ((Str)"(fun1 a b)"); yyparse();
	rexpr = r00;
	assert(0 == compParseOperands(2));
	assert((Obj)2 == r00);
	objNewSymbol((Str)"a", 1); assert(r00 == r01);
	objNewSymbol((Str)"b", 1); assert(r00 == r02);

	/* Parse a 3 operand expression */
	yy_scan_string ((Str)"(fun1 a b c)");
	yyparse();
	rexpr = r00;
	assert(0 == compParseOperands(3));
	assert((Obj)3 == r00);
	objNewSymbol((Str)"a", 1); assert(r00 == r01);
	objNewSymbol((Str)"b", 1); assert(r00 == r02);
	objNewSymbol((Str)"c", 1); assert(r00 == r03);

	/* Parse a 4 operand expression */
	yy_scan_string ((Str)"(fun1 a b c d)");
	yyparse();
	rexpr = r00;
	assert(0 == compParseOperands(4));
	assert((Obj)4 == r00);
	objNewSymbol((Str)"a", 1); assert(r00 == r01);
	objNewSymbol((Str)"b", 1); assert(r00 == r02);
	objNewSymbol((Str)"c", 1); assert(r00 == r03);
	objNewSymbol((Str)"d", 1); assert(r00 == r04);

	/* Verify malformed expressions are caught
	*/
	yy_scan_string ((Str)"(fun0 . error)"); yyparse();
	rexpr = r00;
	assert(compParseOperands(0));

	yy_scan_string ((Str)"(fun1 a . error)"); yyparse();
	rexpr = r00;
	assert(compParseOperands(1));

	yy_scan_string ((Str)"(fun2 a b . error)"); yyparse();
	rexpr = r00;
	assert(compParseOperands(2));

	yy_scan_string ((Str)"(fun3 a b c . error)"); yyparse();
	rexpr = r00;
	assert(compParseOperands(3));

	r00=r01=r02=r03=r04=r05=0;
	yy_scan_string ((Str)"(fun4 a b c d . error)"); yyparse();
	rexpr = r00;
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
	yy_scan_string ((Str)"(x 9)"); yyparse();  rexpr=r00;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"x", 1); assert(r00 == r01);
	objNewInt(9); assert(r00 == r02); /* Works because integers are cached between +/- 1024 */

	FBInit();
	yy_scan_string ((Str)"((f) 9)"); yyparse();  rexpr=r00;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"f", 1); assert(r00 == r01);
	objDisplay(r02, FB);  FBFinalize("(lambda () 9)");

	FBInit();
	yy_scan_string ((Str)"((f g) 9 8)"); yyparse();  rexpr=r00;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"f", 1); assert(r00 == r01);
	objDisplay(r02, FB);  FBFinalize("(lambda (g) 9 8)");

	FBInit();
	yy_scan_string ((Str)"((f . a) 9)"); yyparse();  rexpr=r00;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"f", 1); assert(r00 == r01);
	objDisplay(r02, FB);  FBFinalize("(lambda a 9)");

	FBInit();
	yy_scan_string ((Str)"((f . a) 9)"); yyparse();  rexpr=r00;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"f", 1); assert(r00 == r01);
	objDisplay(r02, FB);  FBFinalize("(lambda a 9)");

	FBInit();
	yy_scan_string ((Str)"((f . a))"); yyparse();  rexpr=r00;
	assert(!compParseTransformDefine());
	objNewSymbol((Str)"f", 1); assert(r00 == r01);
	objDisplay(r02, FB);  FBFinalize("(lambda a)");
}


/* Verify a simple lambda expression compiles and evaluates
*/
void compilerunsimpleLambda (void) {

	asmInit();
	asmICodePushNewQUIT();
	asmICodePushNewQUIT();
	asmAssemble();
	rcodelink = r00;
	riplink = 0;

	yy_scan_string ((Str)"((lambda () 99))");
	yyparse(); /* Use the internal parser */
	compCompile();
	rcode=r00;
	rip=0;
	vmRun();
	assert(99 == *(Int*)r00); /* The expression returns the number 99 */
}


void compilerunaif (void) { // Enable stepping TODO DEBUGGING

	asmInit();
	asmICodePushNewQUIT();
	asmICodePushNewQUIT();
	asmAssemble();
	rcodelink = r00;
	riplink = 0;

	yy_scan_string ((Str)"(let ~ () (=> 9 (lambda (x) x) 8))");
	yyparse(); /* Use the internal parser */
	compCompile();
	rcode=r00;
	rip=0;
	vmRun();
	assert(9 == *(Int*)r00); /* The expression returns the number 9 */
}


void compilerunif (void) {
	asmInit();
	asmICodePushNewQUIT();
	asmICodePushNewQUIT();
	asmAssemble();
	rcodelink = r00;
	riplink = 0;

	yy_scan_string ((Str)"(if #t (not #t) 2)");
	yyparse(); /* Use the internal parser */
	compCompile();
	rcode=r00;
	rip=0;
	vmRun();
	//objDisplay(r00, stderr);
	assert(ofalse == r00);
}


/* This is registered with the os module's exception handler.  It will be
   called if a compiler error occurs with a list in r00 continaing the
   error message and relevant s-expressions from the compiled program.

   It expect FB to be initialized then finalized.  See FB's character file
   buffer code above.
*/
void exceptionHandler (void) {
	objDisplay(r00, FB);
}


/* 4 unit tests that verify runtime car argument checks occur in
   various locations in an expression.
*/
void errorcar (void) {
	FBInit();
	yy_scan_string ((Str)"(car)");
	yyparse();
	compCompile();
	assert(ofalse == r00);
	FBFinalize("(Syntax error 'car' (car))");
}

void errorbegincar (void) {
	FBInit();
	yy_scan_string ((Str)"(begin 1 (car))");
	yyparse();
	compCompile();
	assert(ofalse == r00);
	FBFinalize("(Syntax error 'car' (car) (begin 1 (car)))");


	FBInit();
	yy_scan_string ((Str)"(begin (car) 2)");
	yyparse();
	compCompile();
	assert(ofalse == r00);
	FBFinalize("(Syntax error 'car' (car) (begin (car) 2))");
}

void errorconscarcarifcar (void) {
	FBInit();
	yy_scan_string ((Str)"(cons 1 (car (car (if 1 (car)))))");
	yyparse();
	compCompile();
	assert(ofalse == r00);
	FBFinalize("(Syntax error 'car' (car) (if 1 (car)) (car (if 1 (car))) (car (car (if 1 (car)))) (cons 1 (car (car (if 1 (car))))))");

}

void runtimeerrorcarcar (void) {
	FBInit();
	yy_scan_string ((Str)"(cons 1 (car (car (if 2 (car 9)))))"); /* This leaves object integer 1 on the stack */
	yyparse();
	compCompile();
	rcode = r00;
	rip = 0;
	vmRun();
	FBFinalize("(car expects pair for target 9 (car 9) (if 2 (car 9)) (car (if 2 (car 9))) (car (car (if 2 (car 9)))) (cons 1 (car (car (if 2 (car 9))))))");
	vmPop();

	FBInit();
	yy_scan_string ((Str)"(car (car 9))");
	yyparse();
	compCompile();
	rcode = r00;
	rip = 0;
	vmRun();
	FBFinalize("(car expects pair for target 9 (car 9) (car (car 9)))");
}

extern Obj rcomperrortrace;
extern Obj rcomperrormessage;
void parsepushoperands (void) {

	FBInit();
	yy_scan_string ((Str)"((lambda 1))");  yyparse();  rexpr = r00;
	compCompile();
	assert(compIsError());
	FBFinalize("(Syntax error procedure args 1 ((lambda 1)))");

	renv = rtge;
	FBInit();
	yy_scan_string ((Str)"((lambda (x) x)z)");  yyparse();  rexpr = r00;
	compCompile();
	assert(!compIsError());
	rcode = r00;
	rip = 0;
	vmRun();
	FBFinalize("(Unbound symbol: z)");

	yy_scan_string ((Str)"((lambda (x) x)99)");  yyparse();  rexpr = r00;
	compCompile();
	assert(!compIsError());
	rcode = r00;
	rip = 0;
	vmRun();
	assert(*(Num*)r00 == 99l);
}


int main (void) {
	compInitialize();
	osInitialize(exceptionHandler);
	testInitialize();

	assert(0 == memVecStackLength(rstack));

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

	assert(0 == memVecStackLength(rstack));
	return 0;
}
