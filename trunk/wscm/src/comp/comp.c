#define DEBUG 0
#define DB_DESC "COMP "
#include "debug.h"
#include <stdlib.h> /* exit() */
#include <string.h> /* memcpy() */
#include <assert.h>
#include "mem.h"
#include "vm.h"
#include "obj.h"
#include "sys.h"
#include "os.h"
#include "asm.h"
#include "comp.h"

/* TABLE OF CONTENTS
 Errors
 Syscalls
 Helpers
 Compilers
 Init

 Compile a scheme expression into a VM code block.  Calls ASM and Assemble
 functions in this module.

DESIGN
   Expression to compile assigned to local rexpr
   Flow keeps track of pseudo environment in renv/rc and used registers in flags
   Compiler error/exception handling handled by each sub-compiler.
*/


/* Compiler flags
*/
typedef Num CompState;

static const Num IREGISTERMASK = (Num)0x0000ffffl;
static const Num CCTAILCALL    = (Num)0x00010000l;
static const Num CCNODEFINES   = (Num)0x00020000l;


CompState  compCompileExpr (CompState state);
void compCombination (Num flags);


/* Rootset objects
*/
Obj rexpr, rcomperror, rcomperrormessage, rcomperrortrace, rsubexpr;



/*******************************************************************************
 Errors
*******************************************************************************/
void compErrorReset (void) {
	rcomperror = ofalse;
	rcomperrormessage = 0;
	rcomperrortrace = onull;
}

Num compIsError (void) {
	return ofalse != rcomperror;
}

/* Raise a compiler error.  Sets the error message and initializes
   the offending s-expression trace list with rexpr.
*/
void compErrorRaise (Str msg) {
	DBBEG();
	assert(ofalse == rcomperror); /* Make sure no overlapping error/exception calls */
	rcomperror = otrue;
	rcomperrormessage = msg;
	/* Keep track of the current sub expression stack for the error message */
	rcomperrortrace = rsubexpr;
	DBEND();
}

/* Handle runtime compiler error/exceptions.  Packages up local objects
   for the os exception handler.
*/
void compThrowCompilerError (void) {
	r00 = rcomperrortrace;
	sysListToStack();

	r01 = (Obj)objListLength(rcomperrortrace);
	osException(rcomperrormessage);
}


/* Keep track of current sub-expression compilation during compiler flow
*/
void compPushSubExpr (Obj exp) {
	rsubexpr = objCons(exp, rsubexpr);
}

void compPopSubExpr (void) {
	assert(onull != rsubexpr);
	rsubexpr = cdr(rsubexpr);
}



/*******************************************************************************
 Syscalls
*******************************************************************************/
/* Runtime error handler.  Interface to the os module's exception handler.
      r00 <= error message C string
      r01 <= stack expression count
   stack <= expressions to dump
*/
void compSyscallError (void) {
	osException(r00);
}

/* Runtime symbol lookup syscall.  If a symbol in r01 found in TGE mutate code
   to just reference the binding's value rather than make this syscall.
*/
void compSyscallTGELookup (void) {
	DBBEG(" ");
	DBE objDisplay(r01, stderr);
	sysTGEFind();
	if (r00 == onull) {
		vmPush(r01);
		r01 = (Obj)1;
		r00 = (Obj)"Unbound symbol:";
		compSyscallError();
	} else {
		DB("found in tge @ opcode "HEX, (Num)rip-4);
		/* Specialization optimization.  Mutate code that originally called
		   this function into a code that references the binding's value. */
		memVectorSet(rcode, (Num)rip/ObjSize-4, vmMV_R00_I);     memVectorSet(rcode, (Num)rip/ObjSize-3, r00);
		memVectorSet(rcode, (Num)rip/ObjSize-2, vmLD_R00_R00_I); memVectorSet(rcode, (Num)rip/ObjSize-1, 0);
		memVectorSet(rcode, (Num)rip/ObjSize, vmNOP);
		/* Force virtual machine to run this code. */
		rip -= 4*ObjSize;
	}
	DBEND();
}

/* Run time symbol mutate syscall.  If a symbol in r01 found in TGE mutate code
   to just reference the binding's and mutate binding's value with r00.
*/
void compSyscallTGEMutate (void) {
	DBBEG();
	r02=r00; /* Since a syscall, save value we're trying to set!. */
	sysTGEFind();
	if (r00 == onull) {
		printf ("Unbound symbol \"");
		objDisplay(r01, stdout);
		printf ("\"\n");
		r00 = r02; /* TODO  runtime error.  call thread's exception handler continuation */
	} else {
		DB("found in tge at opcode %0x", (Int)rip-4);
		/* Specialization optimization.  Muate code that originally called
		   this function into a code that references the binding's value. */
		memVectorSet(rcode, (Num)rip-4, vmMV_R01_I);  memVectorSet(rcode, (Num)rip-3, r00);
		memVectorSet(rcode, (Num)rip-2, vmST_R00_R01_I); memVectorSet(rcode, (Num)rip-1, 0);
		r00 = r02; /* Restore value we're trying to set!. */
		/* Force virtual machine to run this code. */
		rip -= 4;
	}
	DBEND();
}

void compSyscallVerifyVectorRef (void) {
	if (!memIsObjectType(r01, TVECTOR)) {
		vmPush(r01); /* Push the vector */
		r00 = r03;/* Push sub expression stack */
		sysListToStack();
		r01 = (Obj)1 + objListLength(r03); /* Number of items on stack to dump */
		r00 = "vector-ref target not a vector";
		compSyscallError();
	} else if (memObjectLength(r01) <= (Num)r02) {
		objNewInt((Int)r02);  vmPush(r00); /* Push the index as a new object */
		vmPush(r01); /* Push the vector */
		r00 = r03; /* Push sub expression stack */
		sysListToStack();
		r01 = (Obj)2 + objListLength(r03); /* Number of items on stack to dump */
		r00 = "vector-ref index out of the vector bounds";
		compSyscallError();
	}
}

void compSyscallVerifyVectorSetB (void) {
	if (!memIsObjectType(r01, TVECTOR)) {
		vmPush(r00);                      /* Push new value */
		objNewInt((Int)r02);  vmPush(r00); /* Push the index as a new object */
		vmPush(r01);                      /* Push vector */
		r00 = r03;                         /* Push sub expression stack */
		sysListToStack();
		r01 = (Obj)3 + objListLength(r03); /* Number of items on stack to dump */
		r00 = "vector-set! target not a vector";
		compSyscallError();
	} else if (memObjectLength(r01) <= (Num)r02) {
		vmPush(r00);                      /* Push new value */
		objNewInt((Int)r02);  vmPush(r00); /* Push the index as a new object */
		vmPush(r01);                      /* Push vector */
		r00 = r03;                         /* Push sub expression stack */
		sysListToStack();
		r01 = (Obj)3 + objListLength(r03); /* Number of items on stack to dump */
		r00 = "vector-set! index out of bounds";
		compSyscallError();
	}
}

/* Compiles s-expression in r00 into code block in r00.
   Called during runtime via eval and macro statements.
*/
void compSyscallCompile (void) {
 CompState retFlags;
 Num destinationReg;
	DBBEG();
	if (otrue == odebug) { sysDumpEnv(renv); }
	compErrorReset();
	asmInit();

	rexpr = r00;
	retFlags = compCompileExpr(CCTAILCALL);

	if (compIsError()) {
		asmReset();
		compThrowCompilerError();
	} else {
		/* Finalize the assembled code by emitting code that moves
		   the value of the last compild expression into R00 and a
		   with a 'ret' opcode */
		destinationReg = retFlags & IREGISTERMASK;
		asmAsm(
			MV, R00, destinationReg,
			RET);
		asmAssemble();
	}
   // Dump the compiled code object as machine language
	
	if (ofalse != odebug) { objDisplay(r00, stderr); }

	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compDebug (void) {
	objDisplay(rcode, stdout);
	objDisplay(r02, stdout);
	memPrintStructures(stdout);
}

/*******************************************************************************
 Helpers
*******************************************************************************/
/* Parse (operator operand...) placing up to 4 operands into r01..r04
   with operand count in r00

   rexpr <= expression to parse
   count <= number of operands to match
      r00  => operand count
     r01.. => operands
   return => 1 if error, 0 otherwise
*/
Num compParseOperands (Num count) {
 Num ret=0;
	DBBEG();
	r00 = cdr(rexpr);
	if (onull == r00) { /* Matched 0 operands */
		r00 = (Obj)0;
		if (0 == count) goto ret;
		else goto reterror;
	}

	if (!objIsPair(r00)) { r00 = (Obj)0; goto reterror; } /* Malformed length 0 */

	r01 = car(r00);
	r00 = cdr(r00);
	if (onull == r00) { /* Matched 1 operand */
		r00 = (Obj)1;
		if (1 == count) goto ret;
		else goto reterror;
	}

	if (!objIsPair(r00)) { r00 = (Obj)1; goto reterror; } /* Malformed length 1 */

	r02 = car(r00);
	r00 = cdr(r00);
	if (onull == r00) { /* Matched 2 operands */
		r00 = (Obj)2;
		if (2 == count) goto ret;
		else goto reterror;
	}

	if (!objIsPair(r00)) { r00 = (Obj)2; goto reterror; } /* Malformed length 2 */
	r03 = car(r00);
	r00 = cdr(r00);
	if (onull == r00) { /* Matched 3 operands */
		r00 = (Obj)3;
		if (3 == count) goto ret;
		else goto reterror;
	}

	if (!objIsPair(r00)) { r00 = (Obj)3; goto reterror; } /* Malformed length 3 */

	r04 = car(r00);
	r00 = cdr(r00);
	if (onull == r00) { /* Matched 4 operands */
		r00 = (Obj)4;
		if (4 == count) goto ret;
		else goto reterror;
	}

	/* Malformed length 4 or don't expect more than 4 operands */
	r00 = (Obj)4;
reterror:
	ret = 1;
ret:
	DBEND(STR, ret?" *ERROR*":"");
	return ret;
}


/* Verify rexpr contains a (lambda ... ...) s-expression
*/
Num compMatchLambda (void) {
	if (!objIsPair(rexpr)) return 0;
	if (slambda != car(rexpr)) return 0;
	return 1;
}


/* Transform expr:((var formals) body) into the form
   r00:(var (lambda formals body)).  No syntic error checking is performed
   yet.  Would rather implement a macro transformation facility.
*/
void compTransformDefineFunction (void) {
	DBBEG();
	r05 = cdr(rexpr);  /* Function's body. */
	rexpr = car(rexpr);
	r03 = car(rexpr); /* Function's name. */
	r04 = cdr(rexpr); /* Function's formal parameters. */

	r01=r04;      r02=r05;   objCons012(); /* (formals body) */
	r01=slambda; r02=r00;   objCons012(); /* (lambda formals body) */
	r01=r00;      r02=onull; objCons012(); /* ((lambda formals body)) */
	r01=r03;      r02=r00;   objCons012(); /* (fn (lambda formals body)) */
	
	DBEND("  =>  ");
	DBE objDisplay(rexpr, stderr);
}


/* Parse and transform define expression
  rexpr <= define expression's body (var expr) or ((var . formals) exp)
         => r01 formal argument symbol
         => r02 expression
  return => 0 success, 1 empty syntax error, 2 illegal formal, 3 illegal expression
*/
Num compParseTransformDefine (void) {
	r00 = rexpr;

	/* Empty */
	if (!objIsPair(r00)) return 1;

	/* If a formals list, then the expression is of the form ((...) body), so transform */
	if (objIsPair(car(r00))) compTransformDefineFunction();

	r01 = car(r00); /* Consider variable */

	/* Empty */
	if (!objIsSymbol(r01)) return 2;

	r00 = cdr(r00);
	if (objIsPair(r00)) r02 = car(r00);
	else if (onull == r00) r02 = onull;
	else return 3;

	return 0;
}


/* Transform internal definitions
    expr <=  ((a b) (define x q) (define y r) body)
       r00 => ((a b) ((lambda (x y) (set! x q) (set! y r) body) () ()))
             or bad define statement if an error occured
   return => 1 if syntax error, 0 success
*/
Num compTransformInternalDefinitions (void) {
 Num ret=0, definitionsCount=0;
	DBBEG();

	vmPush(car(rexpr)); /* Push arg-list */
	rexpr = cdr(rexpr); /* Consider lambda'a body */

	/* Save lambda body. */
	while (objIsPair(rexpr) && objIsPair(car(rexpr)) && sdefine == caar(rexpr)) {
		definitionsCount++;
		vmPush(cdr(rexpr)); /* Push rest */

		vmPush(car(rexpr)); /* Push NEXT (in case of error messages) */

		rexpr = cdar(rexpr); /* Consider next, skipping 'define' symbol */
		ret = compParseTransformDefine(); /* Returns r01=variable  r02=expression */

		if (ret) {
			r00 = vmPop(); /* Return NEXT, the offending define statement */
			while (definitionsCount--) vmPop(); /* Pop pushed definitions */
			vmPop(); /* Pop arg-list */
			goto ret;
		} else vmPop(); /* Pop NEXT */

		rexpr = vmPop(); /* Restore rest (not needed) */

		vmPush(objCons(r01, objCons(r02, onull))); /* Push reparsed definition */
	}

	/* rexpr now the rest of the non internal definition statements */

	/* expr is now pointing at body of function.  If there were any internal
	   definitions, form an equivalent letrec expression. */
	if (definitionsCount) {
		r04=onull; /* Local variable list.  Start out empty. */
		r05=rexpr; /* Set! expressions and body list. Start out with body. */
		r06=onull; /* Null arguments list. */
		while (definitionsCount--) {
			r03=vmPop();/* Considered saved transformed define expression. */
			/* Prepend formal argument to list. */
			r01=car(r03); r02=r04; objCons012(); r04=r00;
			/* Prepend set! expression to list. */
			r01=ssetb;   r02=r03; objCons012();  /* Create (set! var ...) */
			r01=r00;      r02=r05; objCons012(); r05=r00;
			/* Prepend another onull to argument list. */
			r01=onull;    r02=r06; objCons012(); r06=r00;
		}
		r01=r04;      r02=r05;  objCons012();
		r01=slambda; r02=r00;  objCons012();
		r01=r00;      r02=r06;  objCons012();
		/* Create list consisting of this new expression. */
		r01=r00;      r02=onull; objCons012();
		rexpr = r00;
	}
	
	r01=vmPop(); r02=rexpr;   objCons012(); /* Re-attach arg list */

ret:
	DBEND("  =>  ");
	DBE objDisplay(rexpr, stdout);
	return ret;
}

void compTransformLet (void) {
 Num bindingLen, i;
	DBBEG();
	r04=car(rexpr);     /* Consider the let bindings. */
	r05 = cdr(rexpr);   /* Consider the let body. */

	/* Create (val ...) */
	r06=r04;
	bindingLen=objListLength(r04);
	for (i=0; i<bindingLen; i++) {
		vmPush(car(cdar(r06)));
		r06=cdr(r06);
	}
	r02=onull;
	for (i=0; i<bindingLen; i++) {
		r01=vmPop();
		objCons012();
		r02=r00;
	}
	vmPush(r02);

	/* Create (var...) */
	r06=r04;
	bindingLen=objListLength(r04);
	for (i=0; i<bindingLen; i++) {
		vmPush(caar(r06));
		r06=cdr(r06);
	}
	r02=onull;
	for (i=0; i<bindingLen; i++) {
		r01=vmPop();
		objCons012();
		r02=r00;
	}

	/* Create ((var...)body) */
	r01=r02;  r02=r05;  objCons012();

	/* Create (lambda (var...)body) */
	r01=slambda;r02=r00;  objCons012();

	/* Create ((lambda (var...) body) val...) */
	r01=r00;  r02=vmPop();  objCons012();

	/* Return transformed expression. */
	rexpr=r00;

	DBEND("  =>  ");
	DBE objDisplay(rexpr, stdout);
}


/* Parse an argument list
     r00 <= argument list (), r, (x), (x y . r)
      r01 => normalized args list: (args rest), (args ()), (rest), (())
      r02 => dotted arg, (), the bad argument
  return => 0 success, 1 fail

   Normalize a scheme formals list into an internal normalized formals
   environment list.  A proper list with a symbol or null as the "rest"
   formal.

   (x)       ->  (x ())
   (x y)     ->  (x y ())
   (x . r)   ->  (x r)
   (x y . r) ->  (x y r)
   r         ->  (r)
   ()        ->  (())

*/
Num matchArgumentList (void) {
 Num count=0, err=0;

	/* Push all args except dotted */
	while (objIsPair(r00)) {
		++count;
		vmPush(car(r00));
		r00 = cdr(r00);
	}

	/* r02 gets the dotted formal.  Error if not a symbol nor null */
	r02 = r00;
	err = (!objIsSymbol(r02) && onull != r02);

	/* Include the dotted arg in the args list */
	r01 = objCons(r02, onull);

	/* r01 gets a new arg list */
	while (count--) {
		r00 = vmPop();
		/* Replace r02 with last invalid variable as an error */
		if (!objIsSymbol(r00)) { r02 = r00;  err=1; }
		objCons101(); /* r01 <= (cons r00 r01) */
	}

	return err;
}


/* Parse a bock's body
      r00 <= lambda expression's (body)
       r03 => body but last or ()
       r04 => body last, (),  illegal dotted tail
   return => 0 success, 1 fail
*/
Num matchBody (void) {
 Num count=0, err=0;

	r03 = onull;
	r04 = onull;

	/* Push all expressoins */
	while (objIsPair(r00)) {
		++count;
		vmPush(car(r00));
		r00 = cdr(r00);
	}

	if (onull != r00) { err=1;  r04 = r00; } /* Malformed list flag */

	if (count--) {
		/* r04 gets the last expression or the malformed tail as an error */
		r04 = vmPop();
		if (err) r04 = r00;
		/* r03 gets all but the last expressions */
		while (count--) { r00=vmPop();  objCons303(); }
	}

	return err;
}


/* Parse and transform (internal definitions) lambda expressioin.
   rexpr <= lambda expression's list (arg-list body)
       r01 => args, ()
       r02 => dotted arg, (), invalid arg
       r03 => body butlast, ()
       r04 => body last, (), invalid dotted tail
   return => 0 success, 1 arg list syntax fail, 2 body syntax fail
*/
Num compParseTransformProcedure (void) {
 Num ret=0;
	/* Empty */
	if (!objIsPair(rexpr)) {
		ret = 1;
		goto reterror;
	}

	if (compTransformInternalDefinitions()) {
		r04 = r00; /* r00 has malformed internal define statement */
		ret = 3; /* Internal definitions malformed */
		goto reterror;
	}

	/* r00 has new internal-defines-transformed lambda */

	vmPush(cdr(r00)); /* Push body */

	/* Consider arg-list and parse into r01 and r02 */
	r00 = car(r00);
	if (matchArgumentList()) {
		vmPop(); /* Pop arg-list */
		ret = 1; /* r02 contains bad argument for error reporting */
		goto reterror;
	}

	r00 = vmPop(); /* Restore body */

	/* Consider and parse body into r03 and r04 */
	if (matchBody()) {
		ret = 2; /* r04 contains bad tail for error reporting */
		goto reterror;
	}

reterror:
	if (ret) {
		if (ret == 1) {
			compPushSubExpr(r02);
			compErrorRaise((Str)"Syntax error procedure args");
		} else if (ret == 2) {
			compPushSubExpr(r04);
			compErrorRaise((Str)"Syntax error procedure body");
		} else if (ret == 3) {
			compPushSubExpr(r04);
			compErrorRaise((Str)"Syntax error procedure internal definition");
		} else assert(!"Unexpected parse procedure return value");
		compPopSubExpr();
	};

	return ret;
}

void compTransformNamedLet (void) {
 Num bindingLen, i;
	DBBEG();
	r03=car(rexpr);   /* Consider the named-let name symbol. */
	rexpr = cdr(rexpr);
	r04=car(rexpr);   /* Consider the named-let bindings. */
	r05=cdr(rexpr);   /* Consider the named-let body. */

	/* Create ((name val ...)) */
	r06=r04;
	bindingLen=objListLength(r04);
	for (i=0; i<bindingLen; i++) {
		vmPush(car(cdar(r06)));
		r06=cdr(r06);
	}
	r02=onull;
	for (i=0; i<bindingLen; i++) {
		r01=vmPop();
		objCons012();
		r02=r00;
	}
	r01=r03;  objCons012();
	r01=r00;  r02=onull;  objCons012();
	vmPush(r00);

	/* Create (set! name (lambda (var...) body)). */
	r06=r04;
	bindingLen=objListLength(r04);
	for (i=0; i<bindingLen; i++) {
		vmPush(caar(r06));
		r06=cdr(r06);
	}
	r02=onull;
	for (i=0; i<bindingLen; i++) {
		r01=vmPop();
		objCons012();
		r02=r00;
	}
	r01=r02;     r02=r05;  objCons012();
	r01=slambda;r02=r00;  objCons012();
	r01=r00;     r02=onull;objCons012();
	r01=r03;     r02=r00;  objCons012();
	r01=ssetb;  r02=r00;  objCons012();

	/* Merge them into new-body. */
	r01=r00;  r02=vmPop();  objCons012();
	vmPush(r00);

	/* Create (lambda name new-body) */
	r01=r03;  r02=vmPop();  objCons012();
	r01=slambda; r02=r00;  objCons012();
	vmPush(r00);

	/* Create ((lambda name newbody)) and we're done. */
	r01=vmPop();  r02=onull;  objCons012();

	/* Return transformed expression. */
	rexpr=r00;

	DBEND("  =>  ");
	DBE objDisplay(rexpr, stdout);
}

/* Transform:
   (letrec ((v exp)...) body)  =>  (let ((v ())...) (set! v exp)... body)
   Why not:  ((lambda (v ...) (set! v exp) ... body) () ...)
*/
void compTransformLetrec (void) {
 Num len;
	DBBEG();
	rexpr=cdr(rexpr); /* Skip letrec. */

   if (!objIsPair(car(rexpr))) {
		fprintf (stderr, "letrec malformed: ");
		objDisplay(rexpr, stderr);
	}

	/* Push and count letrec binding expressions. */
	for (r03=car(rexpr), len=0;  r03!=onull; r03=cdr(r03), len++) vmPush(car(r03));

	/* Create (()) in r04. */
	r01=onull;  r02=onull;  objCons012();
	r04=r00;
	/* Create ((x ())...) in r03 from bindings on stack so start it with null. */
	r03=onull;
	while(len--) {
		r01=car(vmPop());  r02=r04;  objCons012(); /* Form (x ()). */
		r01=r00;          r02=r03;  objCons012(); /* Form ((x ()) ...). */
		r03=r00;
	}
	vmPush(r03); /* Save transformed bindings to stack. */

	/* Push and count letrec binding expressions (again). */
	for (r03=car(rexpr), len=0;  r03!=onull; r03=cdr(r03), len++) vmPush(car(r03));
	/* Create (((x ())...) (set! x rexpr) ... body). */
	r03=cdr(rexpr); /* Consider (body). */
	while(len--) {
		r01=ssetb;   r02=vmPop();  objCons012();
		r01=r00;      r02=r03;     objCons012();
		r03=r00;
	}

	/* Create (bindings (set! ...) body). */
	r01=vmPop();  r02=r03;  objCons012();

	/* Create (let ...). */
	r01=slet; r02=r00;  objCons012();

	DBEND("  =>  ");
	DBE objDisplay(r00, stdout);
}

/* Given <qq template> in rexpr, create cons tree in r00.
*/
void compTransformQuasiquote (int depth) {
 int isUnquote, isQuasiquote;
	DBBEG();
	if (objIsPair(rexpr)) { /* Is this (unquote ...) */
		isUnquote    = (car(rexpr)==sunquote);
		isQuasiquote = (car(rexpr)==squasiquote);
		if (isUnquote && depth==0) {
			/* (unquote atom) => atom */
			r00 = cadr(rexpr);
		} else if (objIsPair(car(rexpr))
		           && caar(rexpr) == sunquotesplicing
		           && depth==0) {
			/* ((unquote-splicing template) . b) */
			vmPush(car(cdar(rexpr))); /* Save template */
			rexpr=cdr(rexpr);  /* Consider b */
			compTransformQuasiquote(depth); /* => b' */
			/* (append template b') */
			r01=r00;     r02=onull;  objCons012(); /* => (b') */
			r01=vmPop();  r02=r00;    objCons012(); /* => (template b') */
			r01=sappend;  r02=r00;    objCons012(); /* => (append template b') */
		} else { /* Transform (a . b) => (cons a' b') */
			vmPush(cdr(rexpr)); /* Save b */
			rexpr=car(rexpr);  /* Consider a */
			compTransformQuasiquote(depth); /* => a' */
			rexpr=vmPop();      /* Restore b */
			vmPush(r00);        /* Save a' */
			compTransformQuasiquote(depth - isUnquote + isQuasiquote); /* => b' */
			r01=r00;     r02=onull;  objCons012(); /* => (b') */
			r01=vmPop();  r02=r00;    objCons012(); /* => (a' b') */
			r01=scons;  r02=r00;    objCons012(); /* => (cons a' b') */
		}
	/* Transform atom into (quote atom) */
	} else {
		r01=rexpr;   r02=onull;  objCons012(); // atom   => (atom)
		r01=squote; r02=r00;    objCons012(); // (atom) => (quote atom)
	}
	DBEND();
}



/*******************************************************************************
 Compilers
*******************************************************************************/

/* Generate assembly which looks up value of symbol in a local or
   global environment and assigns to r00.  A symbol lookup will be:
   (1) Compiled either as direct reference to a global environment binding
   (2) Compiled into a series of parent environment references and one
       local environment reference.
   (3) A syscall that attempts to locate the named binding which will then
       code modify itslef into case (1).
*/
CompState compSymbol (CompState flags) {
 Num d, ret, depth, offset;
	DBBEG();
	DBE objDisplay(rexpr, stderr);

	Num destIReg = asmNewOregister();

	/* Scan local environments.  Returned is a 16 bit number, the high 8 bits
	   is the environment chain depth, the low 8 bits the binding offset. The
	   offset will be 2 or greater if a variable is found in any environment
	   excluding the global environment. */
	r01 = rexpr;
	ret = sysEnvFind();

	if (ret) {
		depth = ret >> 8;
		offset = ret & 0xff;
		DB("Found in a local environment depth:"NUM" offset:"NUM, depth, offset);
		/* Emit code that traverses the environment chain and references the proper binding. */
		if (depth == 0) {
			asmAsm(LDI, destIReg, RENV, (Obj)(offset*ObjSize));
		} else {
			asmAsm(LDI, destIReg, RENV, 0l); /* Parent env */
			for (d=1; d < depth; d++) asmAsm(LDI, destIReg, destIReg, 0l); /* It's parent env */
			asmAsm(LDI, destIReg, destIReg, (Obj)(offset*ObjSize)); /* Local symbol offset */
		}
		asmAsm(MV, R00, destIReg);
	} else {
		/* Scan tge... */
		sysTGEFind(); /* R00 gets the symbol/value pair */
		if (onull == r00) {
			DB("Can't find in TGE...maybe at runtime");
			asmAsm(
				MVI, R01, rexpr,
				SYSI, compSyscallTGELookup);
		} else {
			DB("Found in TGE");
			asmAsm(
				MVI, destIReg, r00, /* the static symbol/value pair */
				LDI, destIReg, destIReg, 0l);
		}
		asmAsm(MV, R00, destIReg);
	}
	flags = (flags & ~IREGISTERMASK) | destIReg; // Return the iregister containing the symbol value
	DBEND();
	return flags;
}


void compSetB (Num flags) {
 Num ret, d,depth, offset;
	DBBEG();

	if (compParseOperands(2)) {
		compErrorRaise((Str)"Syntax error 'set!'");
		goto ret;
	}

	if (!memIsObjectType(r01, TSYMBOL)) {
		compErrorRaise((Str)"Syntax error 'set!' expects a variable as first operand");
		goto ret;
	}

	vmPush(r01); /* Save VAR */
	/* Emit code that evaluates EXPR */
	rexpr = r02;
	compCompileExpr(flags & ~CCTAILCALL);
	r01 = vmPop(); /* Restore VAR */

	if (compIsError()) goto ret;

	/* Scan local environments.  Returned is a 16 bit number, the high 8 bits
	   is the environment chain depth, the low 8 bits the binding offset. The
	   offset will be 1 or greater if a variable is found in any environment
	   excluding the global environment. */
	ret = sysEnvFind();

	if (ret) {
		depth = ret >> 8;
		offset = ret & 0xff;
		DB("found in a local environment depth:"NUM" offset:"NUM, depth, offset);
		/* Emit code that traverses the environment chain and references the proper binding. */
		if (depth == 0) {
			asmAsm(STI, R00, RENV, (Obj)(offset*ObjSize));
		} else {
			asmAsm(LDI, R01, RENV, 0l); /* Parent env */
			for (d=1; d < depth; d++) asmAsm(LDI, R01, R01, 0l); /* It's parent env */
			asmAsm(STI, R00, R01, (Obj)(offset*ObjSize)); /* Local symbol offset */
		}
	} else {
		/* Scan tge... */
		sysTGEFind();
		if (r00 == onull) {
			DB("can't find in TGE...maybe at runtime");
			asmAsm(
				MVI, R01, r01,
				SYSI, compSyscallTGEMutate);
		} else {
			DB("found in TGE");
			r03 = r00; /* Keep track of the symbol */
			asmAsm(
				MVI, R01, r03,
				STI, R00, R01, 0l);
		}
	}
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compIf (Num flags) {
 Num hasAlternate;
 Obj L1, L2;
	DBBEG();

	/* Parse the operands r01 = TEST, r02 = CONSEQUENT, and r03 = ALTERNATE */
	if (!compParseOperands(2)) hasAlternate = 0;
	else if (!compParseOperands(3)) hasAlternate = 1;
	else {
		compErrorRaise((Str)"Syntax error 'if'");
		goto ret;
	}

	if (hasAlternate) vmPush(r03); /* Save ALTERNATE */
	vmPush(r02); /* Save CONSEQUENT */

	/* [TEST] */
	rexpr = r01; /* Consider TEST */
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) {
		vmPop(); /* Pop CONSEQUENT */
		if (hasAlternate) vmPop(); /* Pop ALTERNATE */
		goto ret;
	}

	/* [TEST]---[BRANCH] */
 	L1 = asmNewLabel();
	asmAsm(BEQI, R00, ofalse, L1);

	/* [TEST]---[BRANCH]---[CONSEQUENT] */
	rexpr = vmPop(); /* Compile CONSEQUENT expression */
	compCompileExpr(flags);

	if (compIsError()) {
		if (hasAlternate) vmPop(); /* Pop ALTERNATE */
		goto ret;
	}

	if (!hasAlternate) {
		/* [TEST]---[BRANCH]---[CONSEQUENT]--[END] */
		asmAsm(LABEL, L1);
	} else {
 		L2 = asmNewLabel();
		/* [TEST]---[BRANCH]---[CONSEQUENT]--[JUMP]--[ALTERNATE]--[END] */
		asmAsm(
			BRA, L2,
			LABEL, L1
		);
		rexpr = vmPop(); /* Consider and compile ALTERNATE */
		compCompileExpr(flags);

		if (compIsError()) goto ret;

		asmAsm(LABEL, L2);
	}
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


/* Compile (cons A B)
*/
void compCons (Num flags) {
	DBBEG();

	if (compParseOperands(2)) {
		compErrorRaise((Str)"Syntax error 'cons'");
		goto ret;
	}

	vmPush(r02); /* Save B */

	rexpr = r01; /* Consider and compile A */
	compCompileExpr(flags & ~CCTAILCALL);
	r02 = vmPop(); /* Restore B */

	if (compIsError()) goto ret;

	asmAsm(PUSH, R00);

	rexpr = r02; /* Consider and compile B */
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) goto ret;

	asmAsm(
		POP, R01,
		SYSI, objCons010
	);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


/* Compile (car PAIR) or (cdr PAIR)
*/
void compCxr (Num flags) {
 Num carorcdr;
 Obj Lok;
	DBBEG();

	/* Initialize to 1 for 'cdr' expression, 0 for 'car' expression */
	carorcdr = (scdr == car(rexpr));

	if (compParseOperands(1)) {
		compErrorRaise(carorcdr?(Str)"Syntax error 'cdr'":(Str)"Syntax error 'car'");
		goto ret;
	}

	rexpr = r01;  /* Consider and compile PAIR */
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) goto ret;

	Lok = asmNewLabel();
	asmAsm(
		/* Compare object type */
		LDI,  R10, R00, -1*ObjSize,
		LSRI, R10, (Obj)DescLengthBitCount,
		BEQI, R10, TPAIR, Lok,
			PUSH, R00, /* Add value of PAIR to stack */
			MVI, R00, rsubexpr, /* Error situation. Consider expression. */
			SYSI, sysListToStack,
			MVI, R01, 1 + objListLength(rsubexpr), /* Number of items on stack to dump */
			MVI, R00, carorcdr?"cdr expects pair for target":"car expects pair for target",
			SYSI,  compSyscallError, /* Error correction */
			RET, /* TODO Required for unit test since no exception handler exists in that simple environment so control returns from the SYSI instruction above.  Will get rid of eventually */
	LABEL, Lok,
		LDI, R00, R00, carorcdr*ObjSize /* Perform car */
	);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compSetCxrB (Num flags) {
 Num carorcdr;
 Obj Lispair;
	DBBEG();

	/* Initialize to 1 for 'set-cdr!' expression, 0 for 'set-car!' expression */
	carorcdr = (ssetcdrb == car(rexpr));

	if (compParseOperands(2)) {
		compErrorRaise(carorcdr?(Str)"Syntax error 'set-cdr!'":(Str)"Syntax error 'set-car!'");
		goto ret;
	}

	vmPush(r01); /* Save PAIR */
	rexpr = r02;/* Consider and compile EXPR */
	compCompileExpr(flags & ~CCTAILCALL);
	asmAsm(PUSH, R00);
	r01 = vmPop(); /* Restore PAIR */

	if (compIsError()) goto ret;

	rexpr = r01; /* Consider and compile PAIR */
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) goto ret;

	Lispair = asmNewLabel();
	asmAsm(
		LDI, R10, R00, (Obj)(-1*ObjSize),
		LSRI, R10, (Obj)DescLengthBitCount,
		BEQI, R10, TPAIR, Lispair,
		PUSH, R00, /* Add value of PAIR to stack */
		MVI, R00, rsubexpr, /* Error situation.  Add sub s-expressions to stack. */
		SYSI, sysListToStack,
		MVI, R01, 1l + objListLength(rsubexpr), /* Number of items on stack to dump */
		MVI, R00, carorcdr?"set-cdr! expects pair for target":"set-car! expects pair for target",
		SYSI,  compSyscallError, /* Error correction */
		RET, /* TODO Required for unit test since no exception handler exists in that simple environment so control returns from the SYSI instruction above.  Will get rid of eventually */
	 LABEL, Lispair,
		POP, R02,
		STI, R02, R00, carorcdr*ObjSize
	);

ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


/* Compile syntatx expression (vector-ref VECTOR INDEX)
*/
void compVectorRef (Num flags) {
	DBBEG();

	if (compParseOperands(2)) {
		compErrorRaise((Str)"Syntax error 'vector-ref'");
		goto ret;
	}

	vmPush(r02); /* Save INDEX */
	rexpr = r01; /* Consider and compile VECTOR */
	compCompileExpr(flags & ~CCTAILCALL);
	r02 = vmPop(); /* Restore INDEX */

	if (compIsError()) goto ret;

	rexpr = r02; /* Consider and compile INDEX expression */
	if (memIsObjectType(rexpr, TINTEGER)) {
		/* Load static integer value into register */
		asmAsm(
			MV, R01, R00, /* Move vector to r01 */
			MVI, R02, *(Num*)rexpr,
			MVI, R03, rsubexpr, /* Pass the sub expression list */
			SYSI, compSyscallVerifyVectorRef,
			LSLI, R02, (Obj)3,
			LD, R00, R01, R02);
	} else {
		asmAsm(PUSH, R00); /* Push VECTOR */
		compCompileExpr(flags & ~CCTAILCALL);

		if (compIsError()) goto ret;

		asmAsm(
			POP, R01, /* Restore VECTOR */
			/* Load object's integer value into register. */
			LDI, R02, R00, 0l, /* This fails runtime type check */
			MVI, R03, rsubexpr, /* Pass the sub expression list */
			SYSI, compSyscallVerifyVectorRef,
			LSLI, R02, (Obj)3,
			LD, R00, R01, R02);
	}

ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

/* Compile (vector-set! VECTOR INDEX EXPR)
   TODO optimize constant INDEX values like I do in compVectorRef or just optimize in assembler
*/
void compVectorSetB (Num flags) {
	DBBEG();

	if (compParseOperands(3)) {
		compErrorRaise((Str)"Syntax error 'vector-set!'");
		goto ret;
	}

	vmPush(r03); /* Save EXPR */
	vmPush(r02); /* Save INDEX */

	/* Consider and compile Vector expression. */
	rexpr = r01;
	compCompileExpr(flags & ~CCTAILCALL);
	asmAsm(PUSH, R00); /* Save evaluated VECTOR object */

	r02 = vmPop(); /* Restore INDEX */
	r03 = vmPop(); /* Restore EXPR */

	if (compIsError()) goto ret;

	/* Consider and compile INDEX */
	vmPush(r03); /* Save EXPR */
	rexpr = r02;
	compCompileExpr(flags & ~CCTAILCALL);
	asmAsm(PUSH, R00); /* Save evaluated INDEX object */
	r03 = vmPop(); /* Restore EXPR */

	if (compIsError()) goto ret;

	/* Consider and compile EXPR */
	rexpr = r03;
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) goto ret;

	asmAsm (
		POP, R02,        /* Pop INDEX object */
		LDI, R02, R02, 0l,/* Load INDEX object's integer value into register */
		POP, R01,        /* Pop VECTOR object */
		MVI, R03, rsubexpr, /* Pass the sub expression list */
		SYSI, compSyscallVerifyVectorSetB, /* TODO verify vector and index objects */
		LSLI, R02, (Obj)3,
		ST, R00, R01, R02  /* Store new-value object in vector */
	);

ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


/* Create a new code block that handles a call to a closures function

    r01 <= normalized args
    r02 <= dotted arg
    r03 <= body butlast
    r04 <= body last
  renv <= pseudo env
     r00 => code object

   Emitted code assumes stack contain its arguments and the count in R01.
   The count includes dotted arguments that need grouping for the
   dotted formal variable.  The code also assumes its containing closure
   in r00 #(code lexical-environment) which it extends the environment with.

    #(PARENT-ENV (a ()) 42)
            |
            #(PARENT-ENV (x r) 1 (2 3 4 5))
                    |
                    (TGE (square . #<closure>) (x . 5) (y . 9))
*/
void compLambdaBody (Num flags) {
 Num nonDottedArgCount;
 Obj Lexpectednoargs, LnotEnoughArguments, LnormalFormals, LbuildRestList;
	DBBEG();

	asmStart(); /* Creating a new code object so start a new sub-ASM context */

	if (onull == car(r01)) {
		/* Since a lambda with empty formals list, emit code which doesn't extend
		   the environment but instead sets env to the containing closure's env
		   or TGE if this is a top level definition. */
		if (renv == rtge) asmAsm(MV, RENV, RTGE); /* env = tge */
		else asmAsm(LDI, RENV, R00, 1l*ObjSize); /* Since macro is always compiled before evaluation, this will be the dynamic environment */

		Lexpectednoargs = asmNewLabel();
		asmAsm (
			BEQI, R01, 0, Lexpectednoargs,
			MVI, R00, rsubexpr, /* Error situation.  Add sub s-expressions to stack S*/
			SYSI, sysListToStack,
			ADDI, R01, objListLength(rsubexpr), /* Add the number of sub-expressions just pushed */
			MVI, R00, "Too many arguments to closure",
			SYSI,  compSyscallError,
			LABEL, Lexpectednoargs
		);
	} else {
		/* Emit code that extends the environment.  Pops the top most arguments
		   into a list for the 'rest' formal parameter  (lambda (a b . rest)...).
		   R03 contains the non-dotted formal parameter length (via the Normalize
		   function above). (TODO Free variables can be statically compiled?) */

		nonDottedArgCount = objListLength(r01) - 1;

		/* Temporarily save lexical environment, from closure in r00, or tge, to r05.
		   Use TGE when a top level definition.  See also the similar situation in
		   this if block's true clause with the empty formals case. */
		if (car(renv) == rtge) asmAsm(MV, R05, RTGE);
		else asmAsm(LDI, R05, R00, 1*ObjSize);/* Since macro is always compiled before evaluation, this will be the dynamic environment */

		LnotEnoughArguments = asmNewLabel();
		LnormalFormals = asmNewLabel();
		asmAsm (
			MVI, R00, onull, /* Initial formal argument 'rest' value (empty list). */
			/* nonDottedArgCount is non-dotted formal argument length. */
			BLTI, R01, nonDottedArgCount, LnotEnoughArguments,
			BEQI, R01, nonDottedArgCount, LnormalFormals
		);

		/* Emit code for functions lacking a dotted formal argument.  This code
		   will be reached if there are more values passed to the function than
		   there are formal arguments.  Otherwise it will just continue to build
		   the dotted formal list. */
		if (r02 == onull) {
			asmAsm (
				MVI, R00, rsubexpr, /* Error situation.  Add sub s-expressions to stack */
				SYSI, sysListToStack,
				ADDI, R01, objListLength(rsubexpr), /* Add the number of sub-expressions just pushed */
				MVI, R00, "Too many arguments to function",
				SYSI, compSyscallError /* Error correction */
			);
		}

		LbuildRestList = asmNewLabel();
		asmAsm (
		LABEL, LbuildRestList,
			MV, R03, R00,
			POP, R02,
			SYSI, objCons023,
			ADDI, R01, -1l,
			BNEI, R01, nonDottedArgCount, LbuildRestList,
			BRA, LnormalFormals,
		LABEL, LnotEnoughArguments,
			MVI, R00, rsubexpr, /* Error situation.  Add sub s-expressions to stack Was MVI, R01, rsubexpr, PUSH, R01 */
			SYSI, sysListToStack,
			ADDI, R01, objListLength(rsubexpr), /* Add the number of sub-expressions just pushed */
			MVI, R00, "Not enough arguments to closure",
			SYSI, compSyscallError, /* Error correction */
			PUSH, R00,
			ADDI, R01, 1l,
			BNEI, R01, nonDottedArgCount, LnotEnoughArguments,
		LABEL, LnormalFormals,
			PUSH, R00,
			/* Create the local environment. R01 is the length of the vector.
			   3 is added to account for the parent env, formal argument list
			   and rest formal argument. */
			ADDI, R01, 3l,
			SYSI,  objNewVector01, /* New vector in r00 of size imm:R01. */
			STI, R05, R00, 0l, /* Set parent link. */
			/* Set the environment's normalized formal argument list which was
			   created before the call to this C function. */
			MVI, R03, cdr(renv),
			STI, R03, R00, 1l*ObjSize
		);

		/* Emit code that pops arguments off stack and stores into proper
		   local environment locations.  */
		nonDottedArgCount++;
		while (nonDottedArgCount--) {
			asmAsm (
				POP, R02,
				STI, R02, R00, (nonDottedArgCount+2l)*ObjSize
			);
		}
		/* Set env register to the newly extended environment. */
		asmAsm(MV, RENV, R00);
	}

	/* Compile lambda statements body contained in r03/buttail and r04/tail */

	if (r04 == onull) { /* TAIL expression */
		/* An empty lambda body will return null.  Not to r05rs spec which requires
			one or more expressions */
		DB("Empty function body.");
		asmAsm(MVI, R00, onull);
	} else {
		vmPush(r04); /* Save TAIL since compcompileexpr is called again */

		while (objIsPair(r03)) { /* Loop over body expressoins */
			DB("Lambda body non-tail expression");
			vmPush(cdr(r03)); /* Push REST */
			rexpr = car(r03); /* Consider expression and compile */
			compCompileExpr((flags & ~CCTAILCALL) | CCNODEFINES);
			r03 = vmPop(); /* Restore REST */
			if (compIsError()) {
				vmPop(); /* Restore TAIL */
				goto end;
			}
		}
		DB("Lambda body tail expression");
		rexpr = vmPop(); /* Restore TAIL expression */
		compCompileExpr(flags | CCTAILCALL | CCNODEFINES);
	}

end:
	if (compIsError()) {
		/* An error occured while compiling the lambda's body so the
		   current assemblyer context must be explicitly abandoned */
		asmReset();
	} else {
		/* Successfull compilation of lambda body so continue to assemble
		   igraph which also restores previous assembler context */
		asmAsm(RET);
		asmAssemble();
	}

	DB ("Code block => ");
	DBE objDisplay(r00, stderr);
	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compLambda (Num flags) {
	DBBEG(" <= ");
	DBE objDisplay(rexpr, stdout);

	rexpr = cdr(rexpr); /* Skip 'lambda' */

	/* Parse lambda expression and check for syntax errors
	   Gives:     r01=normalized args   r03=body butlast
	              r02=dotted arg        r04=body last
	              renv=pseudo env  */
	if (compParseTransformProcedure()) goto ret;

	vmPush(renv); /* Save env since a temporary pseudo env might be created */

	/* Create a temporary extended pseudo environment (parent . formals-list) only
	   if the parsed formals list in r01 contains a parameter */
	if (onull != car(r01)) renv = objCons(renv, r01);

	compLambdaBody(flags);

	renv = vmPop(); /* Restore env */

	if (compIsError()) goto ret;

	/* Generate code that generates a closure.  Closure returned in r00 created
   	from r01 (code) and r01c (current environment). */
	asmAsm(
		MVI, R01, r00, /* Load r01 with code just generated */
		SYSI, sysNewClosure1Env /* Create closure from r01 and rc/renv */
	);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


/* Expects the lambda expression operator and list of operands
	 rexpr <= (lambda-expression  args  ...)
*/
void compLambdaInline (Num flags) {
 Num operandCount=0, formalsCount, index=0, dottedLength, hasDotted=0;
 Num isTailCall;
	DBBEG();

	isTailCall = (flags & CCTAILCALL);
//fprintf(stderr, "[LambdaInline"NUM"]", isTailCall);

	vmPush(renv); /* Save environment */
	vmPush(rexpr); /* Save combination */

	/* Verify operand list syntax */
	r00 = cdr(rexpr); /* Skip operator, consider operands list */
	while (objIsPair(r00)) { ++operandCount;  r00 = cdr(r00); }
	if (onull != r00) {
		vmPop(); /* Pop combination */
		goto ret;
	}

	rexpr = vmPop(); /* restore combination */

	vmPush(cdr(rexpr)); /* Save operands */

	rexpr = cdar(rexpr); /* Consider lambda's formals/body */
	if (compParseTransformProcedure()) {
		vmPop(); /* Pop operands */
		goto ret;
	}

	rexpr = vmPop(); /* Pop operands */

	/* rexpr = operands list
	   r01 => args, ()
      r02 => dotted arg, (), invalid arg
      r03 => body butlast, ()
      r04 => body last, (), invalid dotted tail */

	/* Verify operand and formals counts */
	formalsCount = objListLength(r01) - 1; /* Don't count dotted formal */
	hasDotted = onull != r02;
	dottedLength = operandCount - formalsCount;

	if (operandCount < formalsCount) {
		compErrorRaise((Str)"Combination requires more operands");
		goto ret;
	}

	if (formalsCount < operandCount && !hasDotted) {
		compErrorRaise((Str)"Combination given excess operands");
		goto ret;
	}

	/* Push body and tail */
	vmPush(r04);
	vmPush(r03);

	/* Create extended environment and keep track of it while operands are evaluated
		Emit code to set the environment's formal values as I compile */

	if (formalsCount || hasDotted) {

		/* Create and push a temporary extended pseudo environment (parent . formals-list)
		   for compile time only there is an environment to extend */
		vmPush(objCons(renv, r01)); /* Save environment */

		asmAsm(
			/* Create the local environment. R01 is the length of the vector.
			   2 is added to account for the parent env and formals list */
			MVI, R01, (Obj)((Num)formalsCount + (Num)hasDotted + 2l),
			SYSI, objNewVector01, /* New vector in r00 of size imm:R01. */
			MV, R05, RENV,
			STI, R05, R00, 0l, /* Set current env as parent link */
			MVI, R05, r01, /* Set normalized formals list */
			STI, R05, R00, 1l*ObjSize,
			MV, R05, R00,
			PUSH, R05
		);

		/* Compile each operand */
		while (objIsPair(rexpr)) {
			vmPush(cdr(rexpr)); /* Push rest */
			rexpr = car(rexpr); /* Consider next operand */
			compCompileExpr(flags & ~CCTAILCALL);
			rexpr = vmPop(); /* Pop rest */

			if (compIsError()) {
				vmPop(); /* Pop body and tail */
				vmPop();
				goto ret;
			}

			if (index < formalsCount) {
				asmAsm(
					/* Set the formal's value in the new non-live extended environment */
					POP, R05,
					STI, R00, R05, (Obj)((index++ + 2l)*ObjSize),
					PUSH, R05
				);
			} else {
				/* Save the operand's value for the dotted formal's value */
				asmAsm(PUSH, R00);
			}
		}

		/* Create dotted formal's value from pushed values */
		if (hasDotted) {
			asmAsm(
				MVI, R00, onull); /* Dotted formal will be null if no extra arguments are passed */
			while (dottedLength--)
				asmAsm(
					POP, R01,
					SYSI, objCons010
				);
			asmAsm(
				POP, R05,
				/* Set the dotted formal's value in the new non-live extended environment */
				STI, R00, R05, (Obj)((index++ + 2l)*ObjSize)
			);
		} else asmAsm(POP, R05);

		if (!isTailCall) asmAsm(PUSH, RENV);
		asmAsm(MV, RENV, R05); /* Set register to the newly created one */

		renv = vmPop(); /* Restore pseudo extended environment */
	}

	/* Compile lambda statements body contained in r03/buttail and r04/tail */
	r03 = vmPop();
	r04 = vmPop();

	if (r04 == onull) { /* TAIL expression */
		/* An empty lambda body will return null.  Not to r05rs spec which requires
			one or more expressions */
		DB("Empty function body.");
		asmAsm(MVI, R00, onull);
	} else {
		vmPush(r04); /* Save TAIL since compcompileexpr is called again */

		while (objIsPair(r03)) { /* Loop over body expressoins */
			DB("Lambda body non-tail expression");
			vmPush(cdr(r03)); /* Push REST */
			rexpr = car(r03); /* Consider expression and compile */
			compCompileExpr((flags & ~CCTAILCALL) | CCNODEFINES);
			r03 = vmPop(); /* Restore REST */
			if (compIsError()) {
				vmPop(); /* Restore TAIL */
				goto ret;
			}
		}
		DB("Lambda body tail expression");
		rexpr = vmPop(); /* Restore TAIL expression */
		compCompileExpr(flags | CCNODEFINES);
	}

	if (!isTailCall && (formalsCount || hasDotted)) {
		asmAsm(POP, RENV); /* Restore environment if one was extended */
	}

ret:
	renv = vmPop(); /* Restore environment */
	DBEND();
	return;
}


void compBegin (Num flags) {
	DBBEG();

	rexpr = cdr(rexpr); /* Skip symbol 'begin. */

	if (rexpr == onull) {
		asmAsm(MVI, R00, onull);
	} else {
		while (cdr(rexpr) != onull) {
			DB("begin block's non-tail expression");

			vmPush(cdr(rexpr)); /* Push rest of operands */
				rexpr = car(rexpr); /* Consider next operand */
				compCompileExpr(flags & ~CCTAILCALL);
			rexpr = vmPop(); /* Pop rest of expression */

			if (compIsError()) goto ret;
		}

		DB("begin block's tail expression");
		rexpr = car(rexpr);
		compCompileExpr(flags);

		if (compIsError()) goto ret;
	}

ret:
	DBEND(" ");
	DBE objDisplay(rcomperror, stderr);
}


/* Define can be (define var expr) or (define (var args) expr)
   the latter is transformed to the former
*/
void compDefine (Num flags) {
 Num ret;
	DBBEG();

	if (flags & CCNODEFINES) {
		compErrorRaise((Str)"Illegally placed 'define' statement");
		goto ret;
	}

	rexpr = cdr(rexpr); /* Skip 'define' symbol */

	ret = compParseTransformDefine();
	if (2 == ret) {
		compPushSubExpr(r01);
		compErrorRaise((Str)"Syntax error 'define' invalid variable");
		compPopSubExpr();
		goto ret;
	} else if (ret) {
		compErrorRaise((Str)"Syntax error 'define'");
		goto ret;
	}
	/* r01 has VARIABLE  r02 has transformed EXPRESSION */

	vmPush(renv); /* Save current env */
	renv = rtge; /* Define is always "evaluated" in the top level environment */

	vmPush(r02); /* Save EXPRESSION */

	/* Bind (if not already bound) the symbol and get its binding. */
	sysTGEBind(); /* Return TGE binding in r00 */

	/* Emit code to set the binding's value. */

	rexpr = vmPop(); /* Restore EXPRESSION */
	vmPush(r00); /* Save binding for inclusion in emitted code */
	/* Compile EXPRESSION.  Call complambda directly if possible.  This This avoids tainting the
	   sub-expression stack with the transformed s-expression. */
	if (compMatchLambda()) compLambda(flags & ~CCTAILCALL);
	else compCompileExpr(flags & ~CCTAILCALL);

	asmAsm(
		MVI, R01, vmPop(), /* Load r01 with saved binding. */
		STI, R00, R01, 0L   /* Set binding's value. */
	);

	renv = vmPop(); /* Restore original env */

ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compNot (Num flags) {
 Obj L1, L2;
	DBBEG();

	if (compParseOperands(1)) {
		compErrorRaise((Str)"Syntax error 'not'");
		goto ret;
	}

	rexpr = r01; /* Consider and compile parsed operand */
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) goto ret;

 	L1 = asmNewLabel();
 	L2 = asmNewLabel();
	asmAsm (
		BEQI, R00, ofalse, L1,
		MVI, R00, ofalse,
		BRA, L2,
	 LABEL, L1,
		MVI, R00, otrue,
	 LABEL, L2
	);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

/* Compiles expressions of the form (or ...) or (and ...)
    orand <=  flag signaling 0/or 1/and expression
*/
void compOrAnd (Num flags) {
 Num orand;
 Obj Lend;
	DBBEG();

	/* Initialize to 1 for 'and' expression, 0 for 'or' expression */
	orand = (sand == car(rexpr));

	if (onull == cdr(rexpr)) {
		if (orand) asmAsm (MVI, R00, otrue);  /* Empty 'and' expression returns #t */
		else       asmAsm (MVI, R00, ofalse); /* Empty 'or'  expression returns #f */
	} else {
		Lend = asmNewLabel();
		r02 = cdr(rexpr); /* Consider operand list */
		while (onull != r02) {
			/* At this point operand list is a pair or non-null on-pair */
			if (!objIsPair(r02)) { // TODO Need a general parsing stage instead of this inflow check.  It should also identify the last expression separately from the butlast
				compErrorRaise(orand?(Str)"Syntax error 'and'":(Str)"Syntax error 'or'");
				goto ret;
			}
			/* At this point operand list valid so far */
			rexpr = car(r02); /* Consider next expression */
			vmPush(r02 = cdr(r02)); /* Consider rest operand list and save */
			if (onull == r02) {
				compCompileExpr(flags); /* Tail call */
			} else {
				compCompileExpr(flags & ~CCTAILCALL); /* Not-tail call */
				if (orand) asmAsm(BEQI, R00, ofalse, Lend); /* Emit short circuit 'and' instruction */
				else       asmAsm(BNEI, R00, ofalse, Lend); /* Emit short circuit 'or' instruction */
			}
			r02 = vmPop(); /* Restore operand list.  Can't keep in r02 as it might get used. */
			if (compIsError()) goto ret;
		}
		asmAsm (LABEL, Lend); /* Target for short circuit instructions */
	}
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compAsmCombination (Num flags) {
 Num IsTailCall = flags & CCTAILCALL;
 Obj Lsyscall, Lclosure, Lpopargs, Lpopargsdone, Lend=0;
	DBBEG("  IsTailCall="NUM, IsTailCall);
	Lsyscall = asmNewLabel();
	Lclosure = asmNewLabel();
	Lpopargs = asmNewLabel();
	Lpopargsdone = asmNewLabel();

	asmAsm (
		/* Compare operator's object type */
		LDI,  R10, R00, (Obj)(-1*ObjSize),
		LSRI, R10, (Obj)DescLengthBitCount,
		BEQI, R10, TSYSCALL, Lsyscall,
		BEQI, R10, TCLOSURE, Lclosure,

	 LABEL, Lpopargs,
		BEQI, R01, (Obj)0, Lpopargsdone,
		POP, R02,
		ADDI, R01, (Obj)-1,
		BRA, Lpopargs,
	 LABEL, Lpopargsdone,
		PUSH, R00, /* Push the bad operator */
		MVI, R00, rsubexpr, /* Error situation.  Add sub s-expressions to stack */
		SYSI, sysListToStack,
		MVI, R01, 1l + objListLength(rsubexpr), /* Number of items on stack to dump */
		MVI, R00, "Illegal operator type", /* Illegal operator section.  For now just dump the arguments.  Doesn't return.*/
		SYSI, compSyscallError
	);

	/* Syscall operator section.  Reference the syscall address, set the
  	operand count then make the system call.  */
	asmAsm (
	 LABEL, Lsyscall,
		LDI, R00, R00, 0l, /*  Reference the syscall address then make the system call.  */
		SYS, R00
	);
	if (IsTailCall)
		asmAsm (RET);
	else {
 		Lend = asmNewLabel();
		asmAsm (BRA, Lend);
	}

	/* Closure operator section.  Load jump address into r02.  R01 is
	   argument count and r00 is the closure (which is needed as it
	   holds the lexical environment).
	*/
	asmAsm (
	 LABEL, Lclosure,
		LDI, R02, R00, 0l
	);
	if (IsTailCall)
		asmAsm(JMP, R02);
	else asmAsm(
		JAL, R02,
	 LABEL, Lend,
		POP, RENVLINK, /* Restores previous environment, ip and code registers. */
		POP, RCODELINK,
		POP, RIPLINK
	);

	DBEND(STR, compIsError()?" *ERROR*":"");
}


/* Compiles expression of the form (if testExpr (consequentExpr {value of testExpr}) alternateExpr)
*/
void compAIf (Num flags) {
 Num hasAlternate;
 Obj LfalseBraAddr, Lend;
	DBBEG();

	/* Parse the operands r01 = TEST, r02 = CONSEQUENT, and r03 = ALTERNATE */
	if (!compParseOperands(2)) hasAlternate = 0;
	else if (!compParseOperands(3)) hasAlternate = 1;
	else {
		compErrorRaise((Str)"Syntax error '=>'");
		goto ret;
	}

	if (hasAlternate) vmPush(r03); /* Save ALTERNATE */
	vmPush(r02); /* Save CONSEQUENT */

	DB("compiling test");
	rexpr = r01;
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) {
		vmPop(); /* Pop CONSEQUENT */
		if (hasAlternate) vmPop(); /* Pop ALTERNATE */
		goto ret;
	}

	DB("compiling test logic");
	LfalseBraAddr = asmNewLabel();
	asmAsm(BEQI, R00, ofalse, LfalseBraAddr);

	DB("compiling consequent");
	/* Save execution state, possibly, since the following is the equivalent of compcombination */
	if (!((Num)flags & CCTAILCALL)) {
		asmAsm (
			PUSH, RIPLINK,
			PUSH, RCODELINK,
			PUSH, RENVLINK);
	}

	asmAsm(
		PUSH, R00 /* Push result of test expression on the stack.  Becomes argument to consequent. */
	);

	/* Compile consequent. */
	rexpr = vmPop();
	compCompileExpr(flags & ~CCTAILCALL);
	asmAsm(MVI, R01, 1l);  /* Set the argument count to 1.  Argument already on the stack. */
	compAsmCombination(flags);

	if (compIsError()) {
		if (hasAlternate) vmPop(); /* Pop ALTERNATE */
		goto ret;
	}

	DB("compiling end of consequent and beginning of alternate");
	Lend = asmNewLabel();
	asmAsm(
		BRA, Lend,
	 LABEL, LfalseBraAddr
	);

	/* Compile alternate expression.  If mising, #f will be returned left over from test condition. */
	if (hasAlternate) {
		DB("compiling alternate");
		rexpr = vmPop();
		compCompileExpr(flags);
	}

	asmAsm(LABEL, Lend);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

/* Transforms then compiles the cond special form
   (cond <clause> ...)
     clause := (<test>    <expr> ...)
               (<test> => <expr>)
               (<test>)
               (else      <expr> ...)
*/
void compCond (Num flags) {
 Num clauses=0;
	DBBEG();

	/* Push clauses, checking for non-lists and verifying the else clause is last */
	r02 = cdr(rexpr); /* Skip symbol 'cond */
	while (objIsPair(r02)) { /* Over all clauses  expr = (<clause> ....) */
		r01 = car(r02); /* Consider next clause  r01 = <clause>  */
		/* Error if clause is not a list */
		if (!objIsPair(r01)) {
			rexpr = r01;
			compErrorRaise((Str)"Syntax error 'cond' clause");
			while(clauses--) vmPop(); /* Pop pushed clauses since not continuing */
			goto ret;
		} else {
			DB("Pushing clause");
			DBE objDisplay(r01, stderr);
			clauses++;
			vmPush(r01);
			r02 = cdr(r02); /* Consider next clause for this loop */
			if (selse == car(r01)) {
				/* Else clause matched, so stop pushing clauses and give warning if more clauses follow */
				if (r02 != onull) {
					fprintf (stderr, "\nWARNING: compCond: cond's else clause followed by more clauses ");
					objDisplay(r02, stderr);
				}
				r02 = onull;
			}
		}
	}

	/* Pop clauses building the if/or/begin tree bottom-up into r00 */
	DB (" Creating nested if/or/begin expression");
	r00 = onull;
	while (clauses--) {
		r05 = vmPop(); /* Consider clause r05 = <clause> = (r04 . r03) */
		r04 = car(r05); /* First expr */
		r03 = cdr(r05) ; /* Rest expr */
		if (selse == r04) {
			assert(onull == r00); /* This better be the first clause popped or not at all */
			r01=sbegin; r02=r03; objCons012();          /* (begin <expr> ...) */
		} else if (!objIsPair(r03)) {
			r01=r00;  r02=onull; objCons012();           /* (translated) */
			r01=r04;  r02=r00; objCons012();             /* (<test> (translated)) */
			r01=sor; r02=r00; objCons012();             /* (or <test> (translated)) */
		} else if (saif == car(r03)) {
			r03 = cdr(r03); /* Consider (r04 => . r03 */
			r01=r00;  r02=onull; objCons012();           /* (translated) */
			if (objIsPair(cdr(r03))) { /* Give warning if => clause followed by more clauses */
				fprintf (stderr, "\nWARNING: compCond: cond's => expr not a single expression ");
				objDisplay(r05, stderr);
			}
			r01=car(r03); r02=r00; objCons012();         /* (<expr> translated) */
			r01=r04;  r02=r00; objCons012();             /* (<test> <expr> translated) */
			r01=saif; r02=r00; objCons012();            /* (if <test> <expr> translated) */
		} else {
			r01=r00;  r02=onull; objCons012(); vmPush(r00); /* (translated) */
			r01=sbegin; r02=r03; objCons012();          /* (begin <expr> ...) */
			r01=r00; r02=vmPop(); objCons012();           /* ((begin <expr> ...) translated) */
			r01=r04;  r02=r00; objCons012();             /* (<test> (begin <expr> ...) translated) */
			r01=sif; r02=r00; objCons012();             /* (if <test> (begin <expr> ...) translated) */
		}
	}
	DB ("compCond translated ");
	DBE objDisplay(r00, stdout);
	rexpr = r00;
	compCompileExpr(flags);

ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


/* Translate (case exp
					(lst expresions...)
					...)
	into      (let ((case exp))
					 (cond ((assv case 'lst) expresions...)
							 ...))
*/
void compCase (Num flags) {
 Num clauses=0;
	DBBEG();

	/* Push clauses, checking for non-lists and verifying the else clause is last */
	r02 = cdr(rexpr); /* Skip symbol 'case and expression */
	if (!objIsPair(r02)) {
		compErrorRaise((Str)"Syntax error in case statement.  Missing key expression.");
		goto ret;
	}

	vmPush(car(r02)); /* Push key expression */
	r02 = cdr(r02); /* Consider all clauses */

	while (objIsPair(r02)) { /* Over all clauses  expr = (<clause> ....) */
		r01 = car(r02); /* Consider next clause  r01 = <clause>  */
		/* Error if clause is not a list */
		if (!objIsPair(r01)) {
			rexpr = r01;
			DB("Syntax error 'case' clause");
			compErrorRaise((Str)"Syntax error 'case' clause");
			while(clauses--) vmPop(); /* Pop pushed clauses since not continuing */
			goto ret;
		} else if (!objIsPair(car(r01)) && selse != car(r01)) {
			rexpr = r01;
			DB("Syntax error 'case' clause's datum field not a list");
			compErrorRaise((Str)"Syntax error 'case' clause's datum field not a list");
			while(clauses--) vmPop(); /* Pop pushed clauses since not continuing */
			goto ret;
		} else {
			DB("Pushing clause");
			DBE objDisplay(r01, stderr);
			clauses++;
			vmPush(r01);
			r02 = cdr(r02); /* Consider next clause for this loop */
			if (selse == car(r01)) {
				/* Else clause matched, so stop pushing clauses and give warning if more clauses follow */
				if (r02 != onull) {
					fprintf (stderr, "\nWARNING: compCase: case's else clause followed by more clauses ");
					objDisplay(r02, stderr);
				}
				r02 = onull;
			}
		}
	}

	/* Pop clauses building the if/or/begin tree bottom-up into r00 */
	DB (" Creating nested if/or/begin expression");
	r00 = onull;
	while (clauses--) {
		r05 = vmPop(); /* Consider clause r05 = <clause> = <datum expr ...> = (r04 . r03) */
		r04 = car(r05); /* Datum list or 'else symbol */
		r03 = cdr(r05) ; /* Rest expr */
		if (selse == r04) {
			assert(onull == r00); /* This better be the first clause popped or not at all */
			r01=r05; r02=onull; objCons012();
		} else {
			//if (r00 != onull) { r01=r00;  r02=onull; objCons012(); vmPush(r00); }
			vmPush(r00);                                /* Push (translated) or () if first clause */
			vmPush(r03);                                /* Push expressions */
			r01=r04; r02=onull; objCons012();             /* (datum) */
			r01=squote; r02=r00; objCons012();             /* '(datum) */
			r01=r00; r02=onull; objCons012();             /* ('(datum)) */
			r01=scase;  r02=r00; objCons012();             /* (case '(datum)) */
			r01=smemv;  r02=r00; objCons012();             /* (memv case '(datum)) */
			r01=r00;  r02=onull; objCons012();             /* ((memv case '(datum))) */
			r01=spairp;  r02=r00; objCons012();             /* (pair? (memv case '(datum))) */
			r01=r00;  r02=vmPop(); objCons012();           /* ((memv case '(datum)) expressions) */
			r01=r00; r02=vmPop(); objCons012();            /* (((memv case '(datum)) expressions) translated) */
		}
	}

	r01=scond; r02=r00; objCons012();    /* (cond ...) */

	r01=r00; r02=onull; objCons012();    /* ((cond ...)) */

	r01=vmPop();                   /* Consider key */
	vmPush(r00);                   /* Save ((cond ..)) */

	r02=onull; objCons012();       /* (key) */
	r01=scase; r02=r00; objCons012();  /* (case key) */
	r01=r00; r02=onull; objCons012();     /* ((case key)) */
	r01=r00; r02=vmPop(); objCons012();     /* (((case key)) (cond ...)) */
	r01=slet; r02=r00; objCons012();     /* (let ((case key)) (cond ...)) */

	DB ("compCase translated ");
	DBE objDisplay(r00, stdout);
	rexpr = r00;
	compCompileExpr(flags);

ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compProcedureP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (compParseOperands(1)) {
		compErrorRaise((Str)"Syntax error 'procedure?'");
		goto ret;
	}

	rexpr = r01; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		/* Compare object type */
		LDI,  R10, R00, (Obj)(-1*ObjSize),
		LSRI, R10, (Obj)DescLengthBitCount,
		BEQI, R10, TCLOSURE, Ltrue,
		MVI, R00, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R00, otrue,
	 LABEL, Lend);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compNullP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (compParseOperands(1)) {
		compErrorRaise((Str)"Syntax error 'null?'");
		goto ret;
	}

	rexpr = r01; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BEQI, R00, onull, Ltrue,
		MVI, R00, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R00, otrue,
	 LABEL, Lend);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compPairP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (compParseOperands(1)) {
		compErrorRaise((Str)"Syntax error 'pair?'");
		goto ret;
	}

	rexpr = r01; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		/* Compare object type */
		LDI,  R10, R00, (Obj)(-1*ObjSize),
		LSRI, R10, (Obj)DescLengthBitCount,
		BEQI, R10, TPAIR, Ltrue,
		MVI, R00, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R00, otrue,
	 LABEL, Lend);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compVectorP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (compParseOperands(1)) {
		compErrorRaise((Str)"Syntax error 'vector?'");
		goto ret;
	}

	rexpr = r01; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		/* Compare object type */
		LDI,  R10, R00, (Obj)(-1*ObjSize),
		LSRI, R10, (Obj)DescLengthBitCount,
		BEQI, R10, TVECTOR, Ltrue,
		MVI, R00, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R00, otrue,
	 LABEL, Lend);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compCharP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (compParseOperands(1)) {
		compErrorRaise((Str)"Syntax error 'char?'");
		goto ret;
	}

	rexpr = r01; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		/* Compare object type */
		LDI,  R10, R00, (Obj)(-1*ObjSize),
		LSRI, R10, (Obj)DescLengthBitCount,
		BEQI, R10, TCHAR, Ltrue,
		MVI, R00, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R00, otrue,
	 LABEL, Lend);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compStringP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (compParseOperands(1)) {
		compErrorRaise((Str)"Syntax error 'string?'");
		goto ret;
	}

	rexpr = r01; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		/* Compare object type */
		LDI,  R10, R00, (Obj)(-1*ObjSize),
		LSRI, R10, (Obj)DescLengthBitCount,
		BEQI, R10, TSTRING, Ltrue,
		MVI, R00, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R00, otrue,
	 LABEL, Lend);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compIntegerP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (compParseOperands(1)) {
		compErrorRaise((Str)"Syntax error 'integer?'");
		goto ret;
	}

	rexpr = r01; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		/* Compare object type */
		LDI,  R10, R00, (Obj)(-1*ObjSize),
		LSRI, R10, (Obj)DescLengthBitCount,
		BEQI, R10, TINTEGER, Ltrue,
		MVI, R00, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R00, otrue,
	 LABEL, Lend);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compSymbolP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (compParseOperands(1)) {
		compErrorRaise((Str)"Syntax error 'symbol?'");
		goto ret;
	}

	rexpr = r01; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		/* Compare object type */
		LDI,  R10, R00, (Obj)(-1*ObjSize),
		LSRI, R10, (Obj)DescLengthBitCount,
		BEQI, R10, TSYMBOL, Ltrue,
		MVI, R00, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R00, otrue,
	 LABEL, Lend);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compPortP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (compParseOperands(1)) {
		compErrorRaise((Str)"Syntax error 'port?'");
		goto ret;
	}

	rexpr = r01; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		/* Compare object type */
		LDI,  R10, R00, (Obj)(-1*ObjSize),
		LSRI, R10, (Obj)DescLengthBitCount,
		BEQI, R10, TPORT, Ltrue,
		MVI, R00, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R00, otrue,
	 LABEL, Lend);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compEOFObjectP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (compParseOperands(1)) {
		compErrorRaise((Str)"Syntax error 'eof-object?'");
		goto ret;
	}

	rexpr = r01; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BEQI, R00, oeof, Ltrue,
		MVI, R00, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R00, otrue,
	 LABEL, Lend);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compLet (Num flags) {
	DBBEG();
	rexpr=cdr(rexpr); /* Skip 'let. */

	if (memIsObjectType(car(rexpr), TSYMBOL))
		compTransformNamedLet(); /* Transform named-let form (let symbol ...) */
	else
		compTransformLet();      /* Transform let form (let (...) ...). */

	/* Compile the transformed form by calling compcombination directly since
	   it will alwys be a closure combination. This avoids tainting the sub
	   expression stack with the transformed s-expression. */
	compCombination(flags);

	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compLetrec (Num flags) {
	DBBEG();
	compTransformLetrec();
	rexpr = r00;
	compCompileExpr(flags);
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compQuasiquote (Num flags) {
	DBBEG();
	rexpr = cadr(rexpr); // Given (quasiquote <qq template>) pass <qq template>
	compTransformQuasiquote(0);
	rexpr = r00;
	DB("quasiquote transformation => ");
	DBE objDisplay(rexpr, stderr);
	compCompileExpr(flags);
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compQuote (Num flags) {
	DBBEG();
	asmAsm (
		MVI, R00, cadr(rexpr)
	);
	DBEND(STR, compIsError()?" *ERROR*":"");
}


/* Compile the form (apply fn argument-list).  This should be similar to
   a combination expression. */
void compApply (Num flags) {
 Num operandCount=0;
 Obj Largcount, Largcountdone;
	DBBEG();

	rexpr = cdr(rexpr); /* Skip over 'apply symbol */

	/* Is this a tail call?  if not save state. */
	if (!((Num)flags & CCTAILCALL)) {
		asmAsm (
			PUSH, RIPLINK,
			PUSH, RCODELINK,
			PUSH, RENVLINK);
	}

	vmPush(car(rexpr)); /* Save operator parameter. */

	/* Compile operand expressions the last of which hopefully evaluates to a list of args.
	   The resulting arguments will be pushed onto the stack and passed to the function.  */
	rexpr = cdr(rexpr);
	while (objIsPair(rexpr)) {
		vmPush (cdr(rexpr)); /* Push rest */
		rexpr = car(rexpr); /* Consider expression  */
		compCompileExpr(flags & ~CCTAILCALL);
		asmAsm(PUSH, R00);
		operandCount++;
		rexpr = vmPop();

		if (compIsError()) {
			vmPop(); /* Pop operator */
			goto ret;
		}
	}

	/* Restore and compile operator expression. */
	rexpr=vmPop();
	compCompileExpr(flags & ~CCTAILCALL);
	asmAsm(MV, R03, R00); /* Save operator in r03 */

	if (compIsError()) goto ret;

	/* At this point stack has the arguments, the argument-list and r03 has function.
	   Want to transfers the argument-list items from list to the stack with r01 ending up
	   with the argument count.  Initially the argument count is the number of initial
	   non-list arguments to apply.
	*/
	Largcount = asmNewLabel();
	Largcountdone = asmNewLabel();
	asmAsm (
		MVI, R01, (Obj)(operandCount-1), /* Initialize operand count in r01 to number of initial arguments to apply. */
		POP, R00,    /* Pop argument-list. */
	 LABEL, Largcount,
		BEQI, R00, onull, Largcountdone,
		ADDI, R01, 1l, /* Inc argument count in r01. */
		LDI, R02, R00, 0l, /* Push the car. */
		PUSH, R02,
		LDI, R00, R00, 1l*ObjSize, /* Consider cdr. */
		BRA, Largcount,
	 LABEL, Largcountdone,
		MV, R00, R03     /* Operator back to r00 */
	);

	/* Emit code to that applys args to function/code tail optimized or not. */
	compAsmCombination(flags);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compEval (Num flags) {
	DBBEG();

	/* Skip symbol 'eval' and compile the operand */
	rexpr = cadr(rexpr);
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) goto ret;

	/* Syscall to compile the argument at runtime */
	asmAsm(SYSI, (Obj)compSyscallCompile);

	if (flags & CCTAILCALL) {
		asmAsm(JMP, R00);
	} else {
		asmAsm(
			PUSH, RIPLINK,
         PUSH, RCODELINK,
         PUSH, RENVLINK,
			JAL, R00,
			POP, RENVLINK,
         POP, RCODELINK,
         POP, RIPLINK
		);
	}
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

// Show when a macro is called
//void myfun  (void) { fprintf(stderr, "."); }

void compMacro (Num flags) {
	DBBEG();

	/* Parse lambda expression and check for syntax errors */
	rexpr = cdr(rexpr); /* Skip 'macro' */

	vmPush(rexpr); /* Save original macro expression */

	if (compParseTransformProcedure()) {
		vmPop(); /* Pop original macro expression */
		goto ret;
	}

	/* r01 contains normalized formals and (r03 . (r04))  the body.  Hmmm */
	/* Transform (macro ... ...) => (lambda . transformed-macro-body) assigned to r00 */

	rexpr = vmPop(); /* Restore original macro expression...ignore all the parsing just done (for now) TODO */
	r01=slambda;  r02 = rexpr;  objCons012();

	asmStart();
	asmAsm(
		//SYSI, myfun,
		PUSH, R01, /* Save the argument count in R01 */
		MVI, R00, r00, /* The transformed (lambda ... ...) expression */
		SYSI, compSyscallCompile,
		PUSH, RIPLINK,
		PUSH, RCODELINK,
		PUSH, RENVLINK,
		JAL, R00,
		POP, RENVLINK,
		POP, RCODELINK,
		POP, RIPLINK,
		POP, R01, /* Restore argument count for the just compiled closure */
		LDI, R02, R00, 0l, /* load r02 with code block and call it.  it will return a closure.  */
		JMP, R02);
	asmAssemble();
	/* Sub ASM context ends */

	/* Generate code that generates a closure.  Closure returned in r00 created
	   from r01 (code) and rc (current/dynamic environment). */
	asmAsm(
		MVI, R01, r00, /* Load r01 with code block just compiled. */
		SYSI, sysNewClosure1Env, /* Create closure from r01 and env (rc, which isn't used by the code block just generated) */
		STI, R02, R00, 1l*ObjSize);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

/* Stored stack expected in r03.
*/
void compSysReinstateContinuation (void) {
 Num length;
	DBBEG();

	if ((Int)r01==1) r00=vmPop();
	else {
		fprintf (stderr, "ERROR: compReinstateContinuation() bad argument count %d.\n", (Int)r01);
		exit (-1);
	}

	/* Reinstate stack and registers.
	*/
	length = memObjectLength(r03); /* The stored stack is in r03. */
	memcpy(rstack+ObjSize, r03, length*ObjSize); /* Copy objects into stack vector. */
	*(Num*)rstack = (length+1)*ObjSize; /* Set the stack object pointer. */
	rcode = vmPop();
	rcodelink = vmPop();
	rip = vmPop();
	riplink = vmPop();
	renv = vmPop();
	renvlink = vmPop();
	r01 = (Obj)1l; /* Let contnuation code know this is a call to the continuation */

	DBEND();
}

void compSyscallCreateContinuation (void) {
 Num length;
	DBBEG();
	vmPush(renvlink);
	vmPush(renv);
	vmPush(riplink);
	vmPush(rip);
	vmPush(rcodelink);
	vmPush(rcode);
	length = memVecStackLength(rstack);
	objNewVector(length);
	memcpy(r00, rstack+1*ObjSize, length*ObjSize);
	vmPop(); vmPop(); vmPop(); vmPop(); vmPop(); vmPop();

	asmStart();
	asmAsm(
		MVI, R03, r00,  /* Copy of stack moved to r03 at runtime */
		SYSI, (Obj)compSysReinstateContinuation, /* This never returns */
		RET
	);
	asmAssemble();
	r01 = r00; /* Move new code block to r00 */

	sysNewClosure1Env();
	memVectorSet(r00, 1, rtge); /* Set to TGE just in case. */

	r01 = 0l; /* Let continuation code know this is a call to capture the continuation and to pass it to fn argument  */

	DBEND();
}

/* At this point evaluating (fn).  Want to pass it code that when
   called will revert to this continuation point passing back
   whatever parameter it was given.

   Emit code that stores continuation.  Tricky.  What should the IP
   register value be?  The code register is obvious.  It'll probably
   be a static offset.

   Emit code that calls the function with the continuation.  This might
   be in a tail context.
*/
void compCallCC (Num flags) {
 Obj Lcontinuationcall;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip over 'call/cc symbol in (call/cc fn)*/

	Lcontinuationcall = asmNewLabel();

	asmAsm(
		SYSI, compSyscallCreateContinuation,
		BEQI, R01, 1l, Lcontinuationcall
	);

	/* Is this a tail call?  if not save state. */
	if (!((Num)flags & CCTAILCALL))
		asmAsm (
			PUSH, RIPLINK,
			PUSH, RCODELINK,
			PUSH, RENVLINK);

	asmAsm(
		/* Push the continuation just create via compSyscallCreateContinuation.  This is the argument to the function */
		PUSH, R00
	);

	rexpr = car(rexpr); /* Consider and compile fn. */
	compCompileExpr(flags & ~CCTAILCALL);

	/* Setup application to fn */
	asmAsm(
		MVI, R01, 1l
	);

	compAsmCombination(flags);

	asmAsm(
		LABEL, Lcontinuationcall
	);

	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compThread (Num flags) {
	DBBEG();

	/* Start a new assembly context, compiling parameters passed to thread as a
	   begin block emitting the unthread syscall as the last opcode. */
	asmStart();
	compBegin(0);
	if (compIsError()) {
		asmReset();
	} else {
		asmAsm(SYSI, osUnthread, RET); /* Need a RET so assembler creates a default iblock for the SYSI's iblock */
		asmAssemble(); /* End this sub assembly context, with the new assembled code block in r00 */

		asmAsm(
			MVI, R00, r00,
			SYSI, osNewThread /* the osNewThread syscall returns thread ID integer object in r00 at runtime or #f on failure */
		);
	}

	DBEND(STR, compIsError()?" *ERROR*":"");
}


/* Compile (OPERATOR OPERAND ...)
   expr <= combination expression
*/
void compCombination (Num flags) {
 Num operandCount=0;
	DBBEG();

	vmPush(rexpr); /* Save expression*/
	rexpr = car(rexpr);
	if (compMatchLambda()) {
		/* This combination's operator is a lambda expression so compile the procedure's
		   body inline with the current assembly context */
		rexpr = vmPop(); /* Restore expression */
		compLambdaInline(flags);
	} else {
		rexpr = vmPop(); /* Restore expression */
		if (!(flags & CCTAILCALL)) {
			asmAsm (
				PUSH, RIPLINK,
				PUSH, RCODELINK,
				PUSH, RENVLINK
			);
		}

		vmPush(car(rexpr)); /* Save OPERATOR */

		/* Compile (OPERAND . REST)*/
		rexpr = cdr(rexpr);
		while (objIsPair(rexpr)) {
			operandCount++;
			vmPush(cdr(rexpr)); /* Save REST */
			rexpr = car(rexpr); /* Consider and compile OPERAND */

			compCompileExpr(flags & ~CCTAILCALL);

			asmAsm(PUSH, R00);
			rexpr = vmPop(); /* Restore REST */

			if (compIsError()) { vmPop(); goto ret; } /* Pop operator */
		}

		r01 = vmPop(); /* Restore OPERATOR */

		/* Check combination is syntatically correct (not a malformed dotted list) */
		if (onull != rexpr) {
			compErrorRaise((Str)"Syntax error combination");
			goto ret;
		}

		/* Consider and compile OPERATOR.  Call complambda directly if possible.This
	   	This avoids tainting the sub expression stack with the transformed
	   	s-expression. */
		rexpr = r01; /* Consider OPERATOR and compile */
		compCompileExpr(flags & ~CCTAILCALL);

		if (compIsError()) goto ret;

		/* Emit code that applys args to syscall/closure (hopefully) operator */
		asmAsm (MVI, R01, operandCount); /* At runtime, r01 contains the stack argument count */
		compAsmCombination(flags);
	}

ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compIntrinsic (Num flags) {
	DBBEG();
	asmAsm(MVI, R00, rexpr);
	DBEND();
}


/* Emit icode that evaluates and adds the values.
*/

void compSyscallNewInt (void) {
	objNewInt((Int)r10);
}
/* Syscall to create a new integer object in r00
    r00 <=  Immediate value to initialize integer object with
    r00  => new integer object
*/
void compPrimitiveAdd (Num flags) {
 Int constantSum = 0; // Initial constant sum
 Num acc, emittedAcc=0;
	DBBEG();
	rexpr = cdr(rexpr); // consider list of arguments

	// Gather constants into one constant
	vmPush(rexpr);
	while (objIsPair(rexpr)) {
		if (memIsObjectType(car(rexpr), TINTEGER)) {
			constantSum += *(Int*)car(rexpr);
		}
		rexpr = cdr(rexpr);
	}
	rexpr = vmPop();

	while (objIsPair(rexpr)) {
		if (!memIsObjectType(car(rexpr), TINTEGER)) { // Skip over already optimized integer constants
			vmPush(rexpr); // Save rest of arguments
			rexpr = car(rexpr); // Consider next argument and compiole
			compCompileExpr(flags & ~CCTAILCALL);
			rexpr = vmPop(); // restore rest of arguments
			// Emit code that initializes the accumulating sum register
			if (!emittedAcc) {
				acc = asmNewIregister();
				asmAsm(
					MVI, acc, (Obj)constantSum
				);
				emittedAcc = 1;
			}
			// Emit code which accumulates the next argument expression
			asmAsm(
				LDI, R10, R00, 0, // Consider value of evaluated argument
				ADD, acc, R10
			);
		}
		rexpr = cdr(rexpr);
	}

	// Emit code that initializes the accumulating sum register
	if (!emittedAcc) {
		acc = asmNewIregister();
		asmAsm(
			MVI, acc, (Obj)constantSum
		);
		emittedAcc = 1;
	}

	asmAsm(
		MV, R10, acc,
		SYSI, compSyscallNewInt
	);
	DBEND();
}
void compPrimitiveMul (Num flags) {
 Int constantSum = 1; // Initial constant sum
 Num acc, emittedAcc=0;
	DBBEG();
	rexpr = cdr(rexpr); // consider list of arguments

	// Gather constants into one constant
	vmPush(rexpr);
	while (objIsPair(rexpr)) {
		if (memIsObjectType(car(rexpr), TINTEGER)) {
			constantSum *= *(Int*)car(rexpr);
		}
		rexpr = cdr(rexpr);
	}
	rexpr = vmPop();

	while (objIsPair(rexpr)) {
		if (!memIsObjectType(car(rexpr), TINTEGER)) { // Skip over already optimized integer constants
			vmPush(rexpr); // Save rest of arguments
			rexpr = car(rexpr); // Consider next argument and compiole
			compCompileExpr(flags & ~CCTAILCALL);
			rexpr = vmPop(); // restore rest of arguments
			// Emit code that initializes the accumulating sum register
			if (!emittedAcc) {
				acc = asmNewIregister();
				asmAsm(
					MVI, acc, (Obj)constantSum
				);
				emittedAcc = 1;
			}
			// Emit code which accumulates the next argument expression
			asmAsm(
				LDI, R10, R00, 0, // Consider value of evaluated argument
				MUL, acc, R10
			);
		}
		rexpr = cdr(rexpr);
	}

	// Emit code that initializes the accumulating sum register
	if (!emittedAcc) {
		acc = asmNewIregister();
		asmAsm(
			MVI, acc, (Obj)constantSum
		);
		emittedAcc = 1;
	}

	asmAsm(
		MV, R10, acc,
		SYSI, compSyscallNewInt
	);
	DBEND();
}


/* Recursive scheme expression compiler.  Translates an expression in
   rexpr into opcodes which are sent to the assembler.
    rexpr <= S-expression to compile
    return => flags
*/
CompState compCompileExpr (CompState flags) {
 Obj op;
 CompState retFlags=(CompState)R00;
	DBBEG(" <= ");
	DBE objDisplay(rexpr, stderr);

//fprintf(stderr, "[CompileExpr:"NUM"]", flags & CCTAILCALL);
	compPushSubExpr(rexpr);

	switch (memObjectType(rexpr)) {
		case TSYMBOL: retFlags = compSymbol(flags); break;
		case TPAIR  : op = car(rexpr);
			           if      (ssetb      == op) compSetB(flags);
			           else if (sif        == op) compIf(flags);
			           else if (scons      == op) compCons(flags);
			           else if (scar       == op) compCxr(flags);
			           else if (scdr       == op) compCxr(flags);
			           else if (ssetcarb   == op) compSetCxrB(flags);
			           else if (ssetcdrb   == op) compSetCxrB(flags);
			           else if (svectorref == op) compVectorRef(flags);
			           else if (svectorsetb== op) compVectorSetB(flags);
			           else if (slambda    == op) compLambda(flags);
			           else if (sbegin     == op) compBegin(flags);
			           else if (sdefine    == op) compDefine(flags);
			           else if (snot       == op) compNot(flags);
			           else if (sor        == op) compOrAnd(flags);
			           else if (sand       == op) compOrAnd(flags);
			           else if (saif       == op) compAIf(flags);
			           else if (scond      == op) compCond(flags);
						  else if (scase      == op) compCase(flags);
			           else if (sprocedurep== op) compProcedureP(flags);
			           else if (snullp     == op) compNullP(flags);
			           else if (spairp     == op) compPairP(flags);
			           else if (svectorp   == op) compVectorP(flags);
			           else if (scharp     == op) compCharP(flags);
			           else if (sstringp   == op) compStringP(flags);
			           else if (sintegerp  == op) compIntegerP(flags);
			           else if (ssymbolp   == op) compSymbolP(flags);
			           else if (sportp     == op) compPortP(flags);
			           else if (seofobjectp== op) compEOFObjectP(flags);
			           else if (slet       == op) compLet(flags);
						  else if (sletstar   == op) compLetrec(flags); /* TODO HACK */
			           else if (sletrec    == op) compLetrec(flags);
			           else if (squasiquote== op) compQuasiquote(flags);
			           else if (squote     == op) compQuote(flags);
			           else if (sapply     == op) compApply(flags);
			           else if (seval      == op) compEval(flags);
			           else if (smacro     == op) compMacro(flags);
			           else if (scallcc    == op) compCallCC(flags);
			           else if (sthread    == op) compThread(flags);
			           else if (sthread    == op) compThread(flags);
			           else if (srem       == op); /* The comment operator */
                    else if (objIsSymbol(op) && (r01=op, sysTGEFind(), r00 != onull) && memIsObjectType(car(r00), TPRIMITIVE)) { /* Syntactic form */
                       (*(void(**)(Num))car(r00))(flags); 
                    }
			           else compCombination(flags);
			           break;
		default     : compIntrinsic(flags);
	}

	compPopSubExpr();

	DBEND(STR, compIsError()?" *ERROR*":"");

	return retFlags;
}



/* Compiles the expression in r00 into a code object runable by the virtual machine.
      r00 <= scheme expression to compile
    rexpr = temp
       r00 => VM code block or #f if an exception/error occured while compiling
*/
void compCompile (void) {
 CompState retFlags;
 Num destinationReg;
	DBBEG();
	if (otrue == odebug) { sysDumpEnv(renv); }
	compErrorReset();
	asmInit();

	rexpr = r00;
	retFlags = compCompileExpr(CCTAILCALL);

	if (compIsError()) {
		asmReset();
		compThrowCompilerError();
		r00 = ofalse;
	} else {
		/* Finalize the assembled code by emitting code that moves
		   the value of the last compild expression into R00 and a
		   with a 'ret' opcode */
		destinationReg = retFlags & IREGISTERMASK;
		asmAsm(
			MV, R00, destinationReg,
			RET);
		asmAssemble();
	}

	DBEND(STR, compIsError()?"  *ERROR*":"");
}



/*******************************************************************************
 Init
*******************************************************************************/
void compInitialize (void) {
 static Num shouldInitialize=1;
	DBBEG();
	if (shouldInitialize) {
		DB("Activating module");
		shouldInitialize=0;

		DB("Initializing submodules");
		asmInitialize(); /*     obj vm mem */
		osInitialize(0);  /* sys obj vm mem */

		DB("Registering rootset objects");
		memRootSetAddressRegister(&rexpr);            MEM_ADDRESS_REGISTER(&rexpr);
		memRootSetAddressRegister(&rcomperror);       MEM_ADDRESS_REGISTER(&rcomperror);
		memRootSetAddressRegister(&rcomperrormessage);MEM_ADDRESS_REGISTER(&rcomperrormessage);
		memRootSetAddressRegister(&rcomperrortrace);  MEM_ADDRESS_REGISTER(&rcomperrortrace);
		memRootSetAddressRegister(&rsubexpr);         MEM_ADDRESS_REGISTER(&rsubexpr);

		rsubexpr = onull;

		DB("Registering primitive operators");
      sysDefinePrimitive((Func)compPrimitiveAdd, "+");
      //sysDefinePrimitive((Func)compPrimitiveSub, "-");
      sysDefinePrimitive((Func)compPrimitiveMul, "*");

		DB("Registering static pointer description strings");
		MEM_ADDRESS_REGISTER(compSyscallCompile); 
		MEM_ADDRESS_REGISTER(compSysReinstateContinuation); 
		MEM_ADDRESS_REGISTER(compSyscallCreateContinuation); 
		MEM_ADDRESS_REGISTER(compSyscallTGELookup);
		MEM_ADDRESS_REGISTER("Too many arguments to closure"); 
		MEM_ADDRESS_REGISTER("Illegal operator type");
		MEM_ADDRESS_REGISTER(compSyscallError);
		MEM_ADDRESS_REGISTER("runtime error");
		MEM_ADDRESS_REGISTER("Compiler error");
		MEM_ADDRESS_REGISTER("Too many arguments to function");
		MEM_ADDRESS_REGISTER("Not enough arguments to closure");
		MEM_ADDRESS_REGISTER(compSyscallVerifyVectorSetB);
		MEM_ADDRESS_REGISTER(compSyscallVerifyVectorRef);
		MEM_ADDRESS_REGISTER(compDebug);
		MEM_ADDRESS_REGISTER(compSyscallTGEMutate);
		MEM_ADDRESS_REGISTER(compSyscallNewInt);
	} else {
		DB("Module already activated");
	}
	DBEND();
}



#undef DB_DESC
#undef DEBUG
