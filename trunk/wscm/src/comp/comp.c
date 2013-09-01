#define DEBUG 0
#define DB_DESC "COMP "
#include "debug.h"
#include <stdlib.h> /* exit() */
#include <string.h> /* memcpy() */
#include <assert.h>
#include "comp.h"
#include "asm.h"
#include "os.h"
#include "sys.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"

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
static const Num CCTAILCALL  = (Num)0x00010000l;
static const Num CCNODEFINES = (Num)0x00020000l;


void compCompileExpr (Num flags);
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
	r0 = rcomperrortrace;
	sysListToStack();

	r1 = (Obj)objListLength(rcomperrortrace);
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
      r0 <= error message C string
      r1 <= stack expression count
   stack <= expressions to dump
*/
void compSyscallError (void) {
	osException(r0);
}

/* Runtime symbol lookup syscall.  If a symbol in r1 found in TGE mutate code
   to just reference the binding's value rather than make this syscall.
*/
void compSyscallTGELookup (void) {
	DBBEG(" ");
	DBE objDisplay(r1, stderr);
	sysTGEFind();
	if (r0 == onull) {
		vmPush(r1);
		r1 = (Obj)1;
		r0 = (Obj)"Unbound symbol:";
		compSyscallError();
	} else {
		DB("found in tge @ opcode "HEX, (Num)rip-4);
		/* Specialization optimization.  Mutate code that originally called
		   this function into a code that references the binding's value. */
		memVectorSet(rcode, (Num)rip-4, vmMVI0);  memVectorSet(rcode, (Num)rip-3, r0);
		memVectorSet(rcode, (Num)rip-2, vmLDI00); memVectorSet(rcode, (Num)rip-1, 0);
		/* Force virtual machine to run this code. */
		rip -= 4;
	}
	DBEND();
}

/* Run time symbol mutate syscall.  If a symbol in r1 found in TGE mutate code
   to just reference the binding's and mutate binding's value with r0.
*/
void compSyscallTGEMutate (void) {
	DBBEG();
	r2=r0; /* Since a syscall, save value we're trying to set!. */
	sysTGEFind();
	if (r0 == onull) {
		printf ("Unbound symbol \"");
		objDisplay(r1, stdout);
		printf ("\"\n");
		r0 = r2; /* TODO  runtime error.  call thread's exception handler continuation */
	} else {
		DB("found in tge at opcode %0x", (Int)rip-4);
		/* Specialization optimization.  Muate code that originally called
		   this function into a code that references the binding's value. */
		memVectorSet(rcode, (Num)rip-4, vmMVI1);  memVectorSet(rcode, (Num)rip-3, r0);
		memVectorSet(rcode, (Num)rip-2, vmSTI01); memVectorSet(rcode, (Num)rip-1, 0);
		r0 = r2; /* Restore value we're trying to set!. */
		/* Force virtual machine to run this code. */
		rip -= 4;
	}
	DBEND();
}

void compSyscallVerifyVectorRef (void) {
	if (!memIsObjectType(r1, TVECTOR)) {
		vmPush(r1); /* Push the vector */
		r0 = r3;/* Push sub expression stack */
		sysListToStack();
		r1 = (Obj)1 + objListLength(r3); /* Number of items on stack to dump */
		r0 = "vector-ref target not a vector";
		compSyscallError();
	} else if (memObjectLength(r1) <= (Num)r2) {
		objNewInt((Int)r2);  vmPush(r0); /* Push the index as a new object */
		vmPush(r1); /* Push the vector */
		r0 = r3; /* Push sub expression stack */
		sysListToStack();
		r1 = (Obj)2 + objListLength(r3); /* Number of items on stack to dump */
		r0 = "vector-ref index out of the vector bounds";
		compSyscallError();
	}
}

void compSyscallVerifyVectorSetB (void) {
	if (!memIsObjectType(r1, TVECTOR)) {
		vmPush(r0);                      /* Push new value */
		objNewInt((Int)r2);  vmPush(r0); /* Push the index as a new object */
		vmPush(r1);                      /* Push vector */
		r0 = r3;                         /* Push sub expression stack */
		sysListToStack();
		r1 = (Obj)3 + objListLength(r3); /* Number of items on stack to dump */
		r0 = "vector-set! target not a vector";
		compSyscallError();
	} else if (memObjectLength(r1) <= (Num)r2) {
		vmPush(r0);                      /* Push new value */
		objNewInt((Int)r2);  vmPush(r0); /* Push the index as a new object */
		vmPush(r1);                      /* Push vector */
		r0 = r3;                         /* Push sub expression stack */
		sysListToStack();
		r1 = (Obj)3 + objListLength(r3); /* Number of items on stack to dump */
		r0 = "vector-set! index out of bounds";
		compSyscallError();
	}
}

/* Compiles s-expression in r0 into code block in r0.
   Called during runtime via eval and macro statements.
*/
void compSyscallCompile (void) {
	DBBEG();
	if (otrue == odebug) { sysDumpEnv(renv); }
	compErrorReset();
	asmInit();
	rexpr = r0;
	//compCompileExpr(0); /* Pass empty flags */
	compCompileExpr(CCTAILCALL);
	if (compIsError()) {
		asmReset();
		compThrowCompilerError();
	} else {
		/* Finalize the assembled code with a 'ret' opcode */
		asmAsm(RET);
		asmAssemble();
	}
   // Dump the compiled code object as machine language
	//objDisplay(r0, stderr);
	DBEND(STR, compIsError()?" *ERROR*":"");
}



/*******************************************************************************
 Helpers
*******************************************************************************/
/* Parse (operator operand...) placing up to 4 operands into r1..r4
   with operand count in r0

   rexpr <= expression to parse
   count <= number of operands to match
      r0  => operand count
     r1.. => operands
   return => 1 if error, 0 otherwise
*/
Num compParseOperands (Num count) {
 Num ret=0;
	DBBEG();
	r0 = cdr(rexpr);
	if (onull == r0) { /* Matched 0 operands */
		r0 = (Obj)0;
		if (0 == count) goto ret;
		else goto reterror;
	}

	if (!objIsPair(r0)) { r0 = (Obj)0; goto reterror; } /* Malformed length 0 */

	r1 = car(r0);
	r0 = cdr(r0);
	if (onull == r0) { /* Matched 1 operand */
		r0 = (Obj)1;
		if (1 == count) goto ret;
		else goto reterror;
	}

	if (!objIsPair(r0)) { r0 = (Obj)1; goto reterror; } /* Malformed length 1 */

	r2 = car(r0);
	r0 = cdr(r0);
	if (onull == r0) { /* Matched 2 operands */
		r0 = (Obj)2;
		if (2 == count) goto ret;
		else goto reterror;
	}

	if (!objIsPair(r0)) { r0 = (Obj)2; goto reterror; } /* Malformed length 2 */
	r3 = car(r0);
	r0 = cdr(r0);
	if (onull == r0) { /* Matched 3 operands */
		r0 = (Obj)3;
		if (3 == count) goto ret;
		else goto reterror;
	}

	if (!objIsPair(r0)) { r0 = (Obj)3; goto reterror; } /* Malformed length 3 */

	r4 = car(r0);
	r0 = cdr(r0);
	if (onull == r0) { /* Matched 4 operands */
		r0 = (Obj)4;
		if (4 == count) goto ret;
		else goto reterror;
	}

	/* Malformed length 4 or don't expect more than 4 operands */
	r0 = (Obj)4;
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
   r0:(var (lambda formals body)).  No syntic error checking is performed
   yet.  Would rather implement a macro transformation facility.
*/
void compTransformDefineFunction (void) {
	DBBEG();
	r5 = cdr(rexpr);  /* Function's body. */
	rexpr = car(rexpr);
	r3 = car(rexpr); /* Function's name. */
	r4 = cdr(rexpr); /* Function's formal parameters. */

	r1=r4;      r2=r5;   objCons12(); /* (formals body) */
	r1=slambda; r2=r0;   objCons12(); /* (lambda formals body) */
	r1=r0;      r2=onull; objCons12(); /* ((lambda formals body)) */
	r1=r3;      r2=r0;   objCons12(); /* (fn (lambda formals body)) */
	
	DBEND("  =>  ");
	DBE objDisplay(rexpr, stderr);
}


/* Parse and transform define expression
  rexpr <= define expression's body (var expr) or ((var . formals) exp)
         => r1 formal argument symbol
         => r2 expression
  return => 0 success, 1 empty syntax error, 2 illegal formal, 3 illegal expression
*/
Num compParseTransformDefine (void) {
	r0 = rexpr;

	/* Empty */
	if (!objIsPair(r0)) return 1;

	/* If a formals list, then the expression is of the form ((...) body), so transform */
	if (objIsPair(car(r0))) compTransformDefineFunction();

	r1 = car(r0); /* Consider variable */

	/* Empty */
	if (!objIsSymbol(r1)) return 2;

	r0 = cdr(r0);
	if (objIsPair(r0)) r2 = car(r0);
	else if (onull == r0) r2 = onull;
	else return 3;

	return 0;
}


/* Transform internal definitions
    expr <=  ((a b) (define x q) (define y r) body)
       r0 => ((a b) ((lambda (x y) (set! x q) (set! y r) body) () ()))
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
		ret = compParseTransformDefine(); /* Returns r1=variable  r2=expression */

		if (ret) {
			r0 = vmPop(); /* Return NEXT, the offending define statement */
			while (definitionsCount--) vmPop(); /* Pop pushed definitions */
			vmPop(); /* Pop arg-list */
			goto ret;
		} else vmPop(); /* Pop NEXT */

		rexpr = vmPop(); /* Restore rest (not needed) */

		vmPush(objCons(r1, objCons(r2, onull))); /* Push reparsed definition */
	}

	/* rexpr now the rest of the non internal definition statements */

	/* expr is now pointing at body of function.  If there were any internal
	   definitions, form an equivalent letrec expression. */
	if (definitionsCount) {
		r4=onull; /* Local variable list.  Start out empty. */
		r5=rexpr; /* Set! expressions and body list. Start out with body. */
		r6=onull; /* Null arguments list. */
		while (definitionsCount--) {
			r3=vmPop();/* Considered saved transformed define expression. */
			/* Prepend formal argument to list. */
			r1=car(r3); r2=r4; objCons12(); r4=r0;
			/* Prepend set! expression to list. */
			r1=ssetb;   r2=r3; objCons12();  /* Create (set! var ...) */
			r1=r0;      r2=r5; objCons12(); r5=r0;
			/* Prepend another onull to argument list. */
			r1=onull;    r2=r6; objCons12(); r6=r0;
		}
		r1=r4;      r2=r5;  objCons12();
		r1=slambda; r2=r0;  objCons12();
		r1=r0;      r2=r6;  objCons12();
		/* Create list consisting of this new expression. */
		r1=r0;      r2=onull; objCons12();
		rexpr = r0;
	}
	
	r1=vmPop(); r2=rexpr;   objCons12(); /* Re-attach arg list */

	DBEND("  =>  ");
	DBE objDisplay(rexpr, stdout);
ret:
	return ret;
}

void compTransformLet (void) {
 Num bindingLen, i;
	DBBEG();
	r4=car(rexpr);     /* Consider the let bindings. */
	r5 = cdr(rexpr);   /* Consider the let body. */

	/* Create (val ...) */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		vmPush(car(cdar(r6)));
		r6=cdr(r6);
	}
	r2=onull;
	for (i=0; i<bindingLen; i++) {
		r1=vmPop();
		objCons12();
		r2=r0;
	}
	vmPush(r2);

	/* Create (var...) */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		vmPush(caar(r6));
		r6=cdr(r6);
	}
	r2=onull;
	for (i=0; i<bindingLen; i++) {
		r1=vmPop();
		objCons12();
		r2=r0;
	}

	/* Create ((var...)body) */
	r1=r2;  r2=r5;  objCons12();

	/* Create (lambda (var...)body) */
	r1=slambda;r2=r0;  objCons12();

	/* Create ((lambda (var...) body) val...) */
	r1=r0;  r2=vmPop();  objCons12();

	/* Return transformed expression. */
	rexpr=r0;

	DBEND("  =>  ");
	DBE objDisplay(rexpr, stdout);
}


/* Parse an argument list
     r0 <= argument list (), r, (x), (x y . r)
      r1 => normalized args list: (args rest), (args ()), (rest), (())
      r2 => dotted arg, (), the bad argument
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
	while (objIsPair(r0)) {
		++count;
		vmPush(car(r0));
		r0 = cdr(r0);
	}

	/* r2 gets the dotted formal.  Error if not a symbol nor null */
	r2 = r0;
	err = (!objIsSymbol(r2) && onull != r2);

	/* Include the dotted arg in the args list */
	r1 = objCons(r2, onull);

	/* r1 gets a new arg list */
	while (count--) {
		r0 = vmPop();
		/* Replace r2 with last invalid variable as an error */
		if (!objIsSymbol(r0)) { r2 = r0;  err=1; }
		objCons101(); /* r1 <= (cons r0 r1) */
	}

	return err;
}


/* Parse a bock's body
      r0 <= lambda expression's (body)
       r3 => body but last or ()
       r4 => body last, (),  illegal dotted tail
   return => 0 success, 1 fail
*/
Num matchBody (void) {
 Num count=0, err=0;

	r3 = onull;
	r4 = onull;

	/* Push all expressoins */
	while (objIsPair(r0)) {
		++count;
		vmPush(car(r0));
		r0 = cdr(r0);
	}

	if (onull != r0) { err=1;  r4 = r0; } /* Malformed list flag */

	if (count--) {
		/* r4 gets the last expression or the malformed tail as an error */
		r4 = vmPop();
		if (err) r4 = r0;
		/* r3 gets all but the last expressions */
		while (count--) { r0=vmPop();  objCons303(); }
	}

	return err;
}


/* Parse and transform (internal definitions) lambda expressioin.
   rexpr <= lambda expression's list (arg-list body)
       r1 => args, ()
       r2 => dotted arg, (), invalid arg
       r3 => body butlast, ()
       r4 => body last, (), invalid dotted tail
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
		r4 = r0; /* r0 has malformed internal define statement */
		ret = 3; /* Internal definitions malformed */
		goto reterror;
	}

	/* r0 has new internal-defines-transformed lambda */

	vmPush(cdr(r0)); /* Push body */

	/* Consider arg-list and parse into r1 and r2 */
	r0 = car(r0);
	if (matchArgumentList()) {
		vmPop(); /* Pop arg-list */
		ret = 1; /* r2 contains bad argument for error reporting */
		goto reterror;
	}

	r0 = vmPop(); /* Restore body */

	/* Consider and parse body into r3 and r4 */
	if (matchBody()) {
		ret = 2; /* r4 contains bad tail for error reporting */
		goto reterror;
	}

reterror:
	if (ret) {
		if (ret == 1) {
			compPushSubExpr(r2);
			compErrorRaise((Str)"Syntax error procedure args");
		} else if (ret == 2) {
			compPushSubExpr(r4);
			compErrorRaise((Str)"Syntax error procedure body");
		} else if (ret == 3) {
			compPushSubExpr(r4);
			compErrorRaise((Str)"Syntax error procedure internal definition");
		} else assert(!"Unexpected parse procedure return value");
		compPopSubExpr();
	};

	return ret;
}

void compTransformNamedLet (void) {
 Num bindingLen, i;
	DBBEG();
	r3=car(rexpr);   /* Consider the named-let name symbol. */
	rexpr = cdr(rexpr);
	r4=car(rexpr);   /* Consider the named-let bindings. */
	r5=cdr(rexpr);   /* Consider the named-let body. */

	/* Create ((name val ...)) */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		vmPush(car(cdar(r6)));
		r6=cdr(r6);
	}
	r2=onull;
	for (i=0; i<bindingLen; i++) {
		r1=vmPop();
		objCons12();
		r2=r0;
	}
	r1=r3;  objCons12();
	r1=r0;  r2=onull;  objCons12();
	vmPush(r0);

	/* Create (set! name (lambda (var...) body)). */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		vmPush(caar(r6));
		r6=cdr(r6);
	}
	r2=onull;
	for (i=0; i<bindingLen; i++) {
		r1=vmPop();
		objCons12();
		r2=r0;
	}
	r1=r2;     r2=r5;  objCons12();
	r1=slambda;r2=r0;  objCons12();
	r1=r0;     r2=onull;objCons12();
	r1=r3;     r2=r0;  objCons12();
	r1=ssetb;  r2=r0;  objCons12();

	/* Merge them into new-body. */
	r1=r0;  r2=vmPop();  objCons12();
	vmPush(r0);

	/* Create (lambda name new-body) */
	r1=r3;  r2=vmPop();  objCons12();
	r1=slambda; r2=r0;  objCons12();
	vmPush(r0);

	/* Create ((lambda name newbody)) and we're done. */
	r1=vmPop();  r2=onull;  objCons12();

	/* Return transformed expression. */
	rexpr=r0;

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
	for (r3=car(rexpr), len=0;  r3!=onull; r3=cdr(r3), len++) vmPush(car(r3));

	/* Create (()) in r4. */
	r1=onull;  r2=onull;  objCons12();
	r4=r0;
	/* Create ((x ())...) in r3 from bindings on stack so start it with null. */
	r3=onull;
	while(len--) {
		r1=car(vmPop());  r2=r4;  objCons12(); /* Form (x ()). */
		r1=r0;          r2=r3;  objCons12(); /* Form ((x ()) ...). */
		r3=r0;
	}
	vmPush(r3); /* Save transformed bindings to stack. */

	/* Push and count letrec binding expressions (again). */
	for (r3=car(rexpr), len=0;  r3!=onull; r3=cdr(r3), len++) vmPush(car(r3));
	/* Create (((x ())...) (set! x rexpr) ... body). */
	r3=cdr(rexpr); /* Consider (body). */
	while(len--) {
		r1=ssetb;   r2=vmPop();  objCons12();
		r1=r0;      r2=r3;     objCons12();
		r3=r0;
	}

	/* Create (bindings (set! ...) body). */
	r1=vmPop();  r2=r3;  objCons12();

	/* Create (let ...). */
	r1=slet; r2=r0;  objCons12();

	DBEND("  =>  ");
	DBE objDisplay(r0, stdout);
}

/* Given <qq template> in rexpr, create cons tree in r0.
*/
void compTransformQuasiquote (int depth) {
 int isUnquote, isQuasiquote;
	DBBEG();
	if (objIsPair(rexpr)) { /* Is this (unquote ...) */
		isUnquote    = (car(rexpr)==sunquote);
		isQuasiquote = (car(rexpr)==squasiquote);
		if (isUnquote && depth==0) {
			/* (unquote atom) => atom */
			r0 = cadr(rexpr);
		} else if (objIsPair(car(rexpr))
		           && caar(rexpr) == sunquotesplicing
		           && depth==0) {
			/* ((unquote-splicing template) . b) */
			vmPush(car(cdar(rexpr))); /* Save template */
			rexpr=cdr(rexpr);  /* Consider b */
			compTransformQuasiquote(depth); /* => b' */
			/* (append template b') */
			r1=r0;     r2=onull;  objCons12(); /* => (b') */
			r1=vmPop();  r2=r0;    objCons12(); /* => (template b') */
			r1=sappend;  r2=r0;    objCons12(); /* => (append template b') */
		} else { /* Transform (a . b) => (cons a' b') */
			vmPush(cdr(rexpr)); /* Save b */
			rexpr=car(rexpr);  /* Consider a */
			compTransformQuasiquote(depth); /* => a' */
			rexpr=vmPop();      /* Restore b */
			vmPush(r0);        /* Save a' */
			compTransformQuasiquote(depth - isUnquote + isQuasiquote); /* => b' */
			r1=r0;     r2=onull;  objCons12(); /* => (b') */
			r1=vmPop();  r2=r0;    objCons12(); /* => (a' b') */
			r1=scons;  r2=r0;    objCons12(); /* => (cons a' b') */
		}
	/* Transform atom into (quote atom) */
	} else {
		r1=rexpr;   r2=onull;  objCons12(); // atom   => (atom)
		r1=squote; r2=r0;    objCons12(); // (atom) => (quote atom)
	}
	DBEND();
}



/*******************************************************************************
 Compilers
*******************************************************************************/

/* Generate assembly which looks up value of symbol in a local or
   global environment and assigns to r0.  A symbol lookup will be:
   (1) Compiled either as direct reference to a global environment binding
   (2) Compiled into a series of parent environment references and one
       local environment reference.
   (3) A syscall that attempts to locate the named binding which will then
       code modify itslef into case (1).
*/
void compSymbol (Num flags) {
 Num d, ret, depth, offset;
	DBBEG();
	DBE objDisplay(rexpr, stderr);

	/* Scan local environments.  Returned is a 16 bit number, the high 8 bits
	   is the environment chain depth, the low 8 bits the binding offset. The
	   offset will be 2 or greater if a variable is found in any environment
	   excluding the global environment. */
	r1 = rexpr;
	ret = sysEnvFind();

	if (ret) {
		depth = ret >> 8;
		offset = ret & 0xff;
		DB("Found in a local environment depth:"NUM" offset:"NUM, depth, offset);
		/* Emit code that traverses the environment chain and references the proper binding. */
		if (depth == 0) {
			asmAsm(LDI, R0, RC, (Obj)offset);
		} else {
			asmAsm(LDI, R0, RC, 0l); /* Parent env */
			for (d=1; d < depth; d++) asmAsm(LDI, R0, R0, 0l); /* It's parent env */
			asmAsm(LDI, R0, R0, (Obj)offset); /* Local symbol offset */
		}
	} else {
		/* Scan tge... */
		sysTGEFind(); /* r0 gets the symbol/value pair */
		if (onull == r0) {
			DB("Can't find in TGE...maybe at runtime");
			asmAsm(
				MVI, R1, rexpr,
				SYSI, compSyscallTGELookup);
		} else {
			DB("Found in TGE");
			asmAsm(
				MVI, R0, r0, /* the static symbol/value pair */
				LDI, R0, R0, 0l);
		}
	}
	DBEND();
}


void compSetB (Num flags) {
 Num ret, d,depth, offset;
	DBBEG();

	if (compParseOperands(2)) {
		compErrorRaise((Str)"Syntax error 'set!'");
		goto ret;
	}

	if (!memIsObjectType(r1, TSYMBOL)) {
		compErrorRaise((Str)"Syntax error 'set!' expects a variable as first operand");
		goto ret;
	}

	vmPush(r1); /* Save VAR */
	/* Emit code that evaluates EXPR */
	rexpr = r2;
	compCompileExpr(flags & ~CCTAILCALL);
	r1 = vmPop(); /* Restore VAR */

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
			asmAsm(STI, R0, RC, (Obj)offset);
		} else {
			asmAsm(LDI, R1, RC, 0l); /* Parent env */
			for (d=1; d < depth; d++) asmAsm(LDI, R1, R1, 0l); /* It's parent env */
			asmAsm(STI, R0, R1, (Obj)offset); /* Local symbol offset */
		}
	} else {
		/* Scan tge... */
		sysTGEFind();
		if (r0 == onull) {
			DB("can't find in TGE...maybe at runtime");
			asmAsm(
				MVI, R1, r1,
				SYSI, compSyscallTGEMutate);
		} else {
			DB("found in TGE");
			r3 = r0; /* Keep track of the symbol */
			asmAsm(
				MVI, R1, r3,
				STI, R0, R1, 0l);
		}
	}
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compIf (Num flags) {
 Num hasAlternate;
 Obj L1, L2;
	DBBEG();

	/* Parse the operands r1 = TEST, r2 = CONSEQUENT, and r3 = ALTERNATE */
	if (!compParseOperands(2)) hasAlternate = 0;
	else if (!compParseOperands(3)) hasAlternate = 1;
	else {
		compErrorRaise((Str)"Syntax error 'if'");
		goto ret;
	}

	if (hasAlternate) vmPush(r3); /* Save ALTERNATE */
	vmPush(r2); /* Save CONSEQUENT */

	/* [TEST] */
	rexpr = r1; /* Consider TEST */
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) {
		vmPop(); /* Pop CONSEQUENT */
		if (hasAlternate) vmPop(); /* Pop ALTERNATE */
		goto ret;
	}

	/* [TEST]---[BRANCH] */
 	L1 = asmNewLabel();
	asmAsm(BEQI, R0, ofalse, L1);

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

	vmPush(r2); /* Save B */
	rexpr = r1; /* Consider and compile A */
	compCompileExpr(flags & ~CCTAILCALL);
	r2 = vmPop(); /* Restore B */

	if (compIsError()) goto ret;

	asmAsm(PUSH, R0);

	rexpr = r2; /* Consider and compile B */
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) goto ret;

	asmAsm(
		POP, R1,
		SYSI, objCons10
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

	rexpr = r1;  /* Consider and compile PAIR */
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) goto ret;

	Lok = asmNewLabel();
	asmAsm(
		BRTI, R0, TPAIR, Lok,
		PUSH, R0, /* Add value of PAIR to stack */
		MVI, R0, rsubexpr, /* Error situation.  Add sub s-expressions to stack Was MVI, R1, rsubexpr, PUSH, R1 */
		SYSI, sysListToStack,
		MVI, R1, 1l + objListLength(rsubexpr), /* Number of items on stack to dump */
		MVI, R0, carorcdr?"cdr expects pair for target":"car expects pair for target",
		SYSI,  compSyscallError, /* Error correction */
		RET, /* TODO Required for unit test since no exception handler exists in that simple environment so control returns from the SYSI instruction above.  Will get rid of eventually */
	 LABEL, Lok,
		LDI, R0, R0, carorcdr /* Perform car */
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

	vmPush(r1); /* Save PAIR */
	rexpr = r2;/* Consider and compile EXPR */
	compCompileExpr(flags & ~CCTAILCALL);
	asmAsm(PUSH, R0);
	r1 = vmPop(); /* Restore PAIR */

	if (compIsError()) goto ret;

	rexpr = r1; /* Consider and compile PAIR */
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) goto ret;

	Lispair = asmNewLabel();
	asmAsm(
		BRTI, R0, TPAIR, Lispair,
		PUSH, R0, /* Add value of PAIR to stack */
		MVI, R0, rsubexpr, /* Error situation.  Add sub s-expressions to stack Was MVI, R1, rsubexpr, PUSH, R1 */
		SYSI, sysListToStack,
		MVI, R1, 1l + objListLength(rsubexpr), /* Number of items on stack to dump */
		MVI, R1, 2l, /* Number of items on stack to dump */
		MVI, R0, carorcdr?"set-cdr! expects pair for target":"set-car! expects pair for target",
		SYSI,  compSyscallError, /* Error correction */
		RET, /* TODO Required for unit test since no exception handler exists in that simple environment so control returns from the SYSI instruction above.  Will get rid of eventually */
	 LABEL, Lispair,
		POP, R2,
		STI, R2, R0, carorcdr
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

	vmPush(r2); /* Save INDEX */
	rexpr = r1; /* Consider and compile VECTOR */
	compCompileExpr(flags & ~CCTAILCALL);
	r2 = vmPop(); /* Restore INDEX */

	if (compIsError()) goto ret;

	rexpr = r2; /* Consider and compile INDEX expression */
	if (memIsObjectType(rexpr, TINTEGER)) {
		/* Load static integer value into register */
		asmAsm(
			MV, R1, R0, /* Move vector to r1 */
			MVI, R2, *(Num*)rexpr,
			MVI, R3, rsubexpr, /* Pass the sub expression list */
			SYSI, compSyscallVerifyVectorRef,
			LD, R0, R1, R2);
	} else {
		asmAsm(PUSH, R0); /* Push VECTOR */
		compCompileExpr(flags & ~CCTAILCALL);

		if (compIsError()) goto ret;

		asmAsm(
			POP, R1, /* Restore VECTOR */
			/* Load object's integer value into register. */
			LDI, R2, R0, 0l, /* This fails runtime type check */
			MVI, R3, rsubexpr, /* Pass the sub expression list */
			SYSI, compSyscallVerifyVectorRef,
			LD, R0, R1, R2);
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

	vmPush(r3); /* Save EXPR */
	vmPush(r2); /* Save INDEX */

	/* Consider and compile Vector expression. */
	rexpr = r1;
	compCompileExpr(flags & ~CCTAILCALL);
	asmAsm(PUSH, R0); /* Save evaluated VECTOR object */

	r2 = vmPop(); /* Restore INDEX */
	r3 = vmPop(); /* Restore EXPR */

	if (compIsError()) goto ret;

	/* Consider and compile INDEX */
	vmPush(r3); /* Save EXPR */
	rexpr = r2;
	compCompileExpr(flags & ~CCTAILCALL);
	asmAsm(PUSH, R0); /* Save evaluated INDEX object */
	r3 = vmPop(); /* Restore EXPR */

	if (compIsError()) goto ret;

	/* Consider and compile EXPR */
	rexpr = r3;
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) goto ret;

	asmAsm (
		POP, R2,        /* Pop INDEX object */
		LDI, R2, R2, 0l,/* Load INDEX object's integer value into register */
		POP, R1,        /* Pop VECTOR object */
		MVI, R3, rsubexpr, /* Pass the sub expression list */
		SYSI, compSyscallVerifyVectorSetB, /* TODO verify vector and index objects */
		ST, R0, R1, R2  /* Store new-value object in vector */
	);

ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


/* Create a new code block that handles a call to a closures function

    r1 <= normalized args
    r2 <= dotted arg
    r3 <= body butlast
    r4 <= body last
  renv <= pseudo env
     r0 => code object

   Emitted code assumes stack contain its arguments and the count in R1.
   The count includes dotted arguments that need grouping for the
   dotted formal variable.  The code also assumes its containing closure
   in r0 #(code lexical-environment) which it extends the environment with.

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

	if (onull == car(r1)) {
		/* Since a lambda with empty formals list, emit code which doesn't extend
		   the environment but instead sets env to the containing closure's env
		   or TGE if this is a top level definition. */
		if (renv == rtge) asmAsm(MV, RC, R8); /* env = tge */
		else asmAsm(LDI, RC, R0, 1l); /* Since macro is always compiled before evaluation, this will be the dynamic environment */

		Lexpectednoargs = asmNewLabel();
		asmAsm (
			BEQI, R1, 0, Lexpectednoargs,
			MVI, R0, rsubexpr, /* Error situation.  Add sub s-expressions to stack S*/
			SYSI, sysListToStack,
			ADDI, R1, objListLength(rsubexpr), /* Add the number of sub-expressions just pushed */
			MVI, R0, "Too many arguments to closure",
			SYSI,  compSyscallError,
			LABEL, Lexpectednoargs
		);
	} else {
		/* Emit code that extends the environment.  Pops the top most arguments
		   into a list for the 'rest' formal parameter  (lambda (a b . rest)...).
		   R3 contains the non-dotted formal parameter length (via the Normalize
		   function above). (TODO Free variables can be statically compiled?) */

		nonDottedArgCount = objListLength(r1) - 1;

		/* Temporarily save lexical environment, from closure in r0, or tge, to r5.
		   Use TGE when a top level definition.  See also the similar situation in
		   this if block's true clause with the empty formals case. */
		if (car(renv) == rtge) asmAsm(MV, R5, R8);
		else asmAsm(LDI, R5, R0, 1l);/* Since macro is always compiled before evaluation, this will be the dynamic environment */

		LnotEnoughArguments = asmNewLabel();
		LnormalFormals = asmNewLabel();
		asmAsm (
			MVI, R0, onull, /* Initial formal argument 'rest' value (empty list). */
			/* nonDottedArgCount is non-dotted formal argument length. */
			BLTI, R1, nonDottedArgCount, LnotEnoughArguments,
			BEQI, R1, nonDottedArgCount, LnormalFormals
		);

		/* Emit code for functions lacking a dotted formal argument.  This code
		   will be reached if there are more values passed to the function than
		   there are formal arguments.  Otherwise it will just continue to build
		   the dotted formal list. */
		if (r2 == onull) {
			asmAsm (
				MVI, R0, rsubexpr, /* Error situation.  Add sub s-expressions to stack */
				SYSI, sysListToStack,
				ADDI, R1, objListLength(rsubexpr), /* Add the number of sub-expressions just pushed */
				MVI, R0, "Too many arguments to function",
				SYSI, compSyscallError /* Error correction */
			);
		}

		LbuildRestList = asmNewLabel();
		asmAsm (
		LABEL, LbuildRestList,
			MV, R3, R0,
			POP, R2,
			SYSI, objCons23,
			ADDI, R1, -1l,
			BNEI, R1, nonDottedArgCount, LbuildRestList,
			BRA, LnormalFormals,
		LABEL, LnotEnoughArguments,
			MVI, R0, rsubexpr, /* Error situation.  Add sub s-expressions to stack Was MVI, R1, rsubexpr, PUSH, R1 */
			SYSI, sysListToStack,
			ADDI, R1, objListLength(rsubexpr), /* Add the number of sub-expressions just pushed */
			MVI, R0, "Not enough arguments to closure",
			SYSI, compSyscallError, /* Error correction */
			PUSH, R0,
			ADDI, R1, 1l,
			BNEI, R1, nonDottedArgCount, LnotEnoughArguments,
		LABEL, LnormalFormals,
			PUSH, R0,
			/* Create the local environment. R1 is the length of the vector.
			   3 is added to account for the parent env, formal argument list
			   and rest formal argument. */
			ADDI, R1, 3l,
			SYSI,  objNewVector1, /* New vector in r0 of size imm:R1. */
			STI, R5, R0, 0l, /* Set parent link. */
			/* Set the environment's normalized formal argument list which was
			   created before the call to this C function. */
			MVI, R3, cdr(renv),
			STI, R3, R0, 1l
		);

		/* Emit code that pops arguments off stack and stores into proper
		   local environment locations.  */
		nonDottedArgCount++;
		while (nonDottedArgCount--) {
			asmAsm (
				POP, R2,
				STI, R2, R0, nonDottedArgCount+2l
			);
		}
		/* Set env register to the newly extended environment. */
		asmAsm(MV, RC, R0);
	}

	/* Compile lambda statements body contained in r3/buttail and r4/tail */

	if (r4 == onull) { /* TAIL expression */
		/* An empty lambda body will return null.  Not to r5rs spec which requires
			one or more expressions */
		DB("Empty function body.");
		asmAsm(MVI, R0, onull);
	} else {
		vmPush(r4); /* Save TAIL since compcompileexpr is called again */

		while (objIsPair(r3)) { /* Loop over body expressoins */
			DB("Lambda body non-tail expression");
			vmPush(cdr(r3)); /* Push REST */
			rexpr = car(r3); /* Consider expression and compile */
			compCompileExpr((flags & ~CCTAILCALL) | CCNODEFINES);
			r3 = vmPop(); /* Restore REST */
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
	DBE objDisplay(r0, stderr);
	DBEND(STR, compIsError()?" *ERROR*":"");
}

void compLambda (Num flags) {
	DBBEG(" <= ");
	DBE objDisplay(rexpr, stdout);

	rexpr = cdr(rexpr); /* Skip 'lambda' */

	/* Parse lambda expression and check for syntax errors
	   Gives:     r1=normalized args   r3=body butlast
	              r2=dotted arg        r4=body last
	              renv=pseudo env  */
	if (compParseTransformProcedure()) goto ret;

	vmPush(renv); /* Save env since a temporary pseudo env might be created */

	/* Create a temporary extended pseudo environment (parent . formals-list) only
	   if the parsed formals list in r1 contains a parameter */
	if (onull != car(r1)) renv = objCons(renv, r1);

	compLambdaBody(flags);

	renv = vmPop(); /* Restore env */

	if (compIsError()) goto ret;

	/* Generate code that generates a closure.  Closure returned in r0 created
   	from r1 (code) and r1c (current environment). */
	asmAsm(
		MVI, R1, r0, /* Load r1 with code just generated */
		SYSI, sysNewClosure1Env /* Create closure from r1 and rc/renv */
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
	r0 = cdr(rexpr); /* Skip operator, consider operands list */
	while (objIsPair(r0)) { ++operandCount;  r0 = cdr(r0); }
	if (onull != r0) {
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
	   r1 => args, ()
      r2 => dotted arg, (), invalid arg
      r3 => body butlast, ()
      r4 => body last, (), invalid dotted tail */

	/* Verify operand and formals counts */
	formalsCount = objListLength(r1) - 1; /* Don't count dotted formal */
	hasDotted = onull != r2;
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
	vmPush(r4);
	vmPush(r3);

	/* Create extended environment and keep track of it while operands are evaluated
		Emit code to set the environment's formal values as I compile */

	if (formalsCount || hasDotted) {

		/* Create and push a temporary extended pseudo environment (parent . formals-list)
		   for compile time only there is an environment to extend */
		vmPush(objCons(renv, r1)); /* Save environment */

		asmAsm(
			/* Create the local environment. R1 is the length of the vector.
			   2 is added to account for the parent env and formals list */
			MVI, R1, (Obj)((Num)formalsCount + (Num)hasDotted + 2l),
			SYSI, objNewVector1, /* New vector in r0 of size imm:R1. */
			MV, R5, RC,
			STI, R5, R0, 0l, /* Set current env as parent link */
			MVI, R5, r1, /* Set normalized formals list */
			STI, R5, R0, 1l,
			MV, R5, R0,
			PUSH, R5
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
					POP, R5,
					STI, R0, R5, (Obj)(index++ + 2l),
					PUSH, R5
				);
			} else {
				/* Save the operand's value for the dotted formal's value */
				asmAsm(PUSH, R0);
			}
		}

		/* Create dotted formal's value from pushed values */
		if (hasDotted) {
			asmAsm(
				MVI, R0, onull); /* Dotted formal will be null if no extra arguments are passed */
			while (dottedLength--)
				asmAsm(
					POP, R1,
					SYSI, objCons10
				);
			asmAsm(
				POP, R5,
				/* Set the dotted formal's value in the new non-live extended environment */
				STI, R0, R5, (Obj)(index++ + 2l)
			);
		} else asmAsm(POP, R5);

		if (!isTailCall) asmAsm(PUSH, RC);
		asmAsm(MV, RC, R5); /* Set register to the newly created one */

		renv = vmPop(); /* Restore pseudo extended environment */
	}

	/* Compile lambda statements body contained in r3/buttail and r4/tail */
	r3 = vmPop();
	r4 = vmPop();

	if (r4 == onull) { /* TAIL expression */
		/* An empty lambda body will return null.  Not to r5rs spec which requires
			one or more expressions */
		DB("Empty function body.");
		asmAsm(MVI, R0, onull);
	} else {
		vmPush(r4); /* Save TAIL since compcompileexpr is called again */

		while (objIsPair(r3)) { /* Loop over body expressoins */
			DB("Lambda body non-tail expression");
			vmPush(cdr(r3)); /* Push REST */
			rexpr = car(r3); /* Consider expression and compile */
			compCompileExpr((flags & ~CCTAILCALL) | CCNODEFINES);
			r3 = vmPop(); /* Restore REST */
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
		asmAsm(POP, RC); /* Restore environment if one was extended */
	}

ret:
	renv = vmPop(); /* Restore environment */
	return;
}


void compBegin (Num flags) {
	DBBEG();

	rexpr = cdr(rexpr); /* Skip symbol 'begin. */

	if (rexpr == onull) {
		asmAsm(MVI, R0, onull);
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
		compPushSubExpr(r1);
		compErrorRaise((Str)"Syntax error 'define' invalid variable");
		compPopSubExpr();
		goto ret;
	} else if (ret) {
		compErrorRaise((Str)"Syntax error 'define'");
		goto ret;
	}
	/* r1 has VARIABLE  r2 has transformed EXPRESSION */

	vmPush(renv); /* Save current env */
	renv = rtge; /* Define is always "evaluated" in the top level environment */

	vmPush(r2); /* Save EXPRESSION */

	/* Bind (if not already bound) the symbol and get its binding. */
	sysTGEBind(); /* Return TGE binding in r0 */

	/* Emit code to set the binding's value. */

	rexpr = vmPop(); /* Restore EXPRESSION */
	vmPush(r0); /* Save binding for inclusion in emitted code */
	/* Compile EXPRESSION.  Call complambda directly if possible.  This This avoids tainting the
	   sub-expression stack with the transformed s-expression. */
	if (compMatchLambda()) compLambda(flags & ~CCTAILCALL);
	else compCompileExpr(flags & ~CCTAILCALL);

	asmAsm(
		MVI, R1, vmPop(), /* Load r1 with saved binding. */
		STI, R0, R1, 0L   /* Set binding's value. */
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

	rexpr = r1; /* Consider and compile parsed operand */
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) goto ret;

 	L1 = asmNewLabel();
 	L2 = asmNewLabel();
	asmAsm (
		BEQI, R0, ofalse, L1,
		MVI, R0, ofalse,
		BRA, L2,
	 LABEL, L1,
		MVI, R0, otrue,
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
		if (orand) asmAsm (MVI, R0, otrue);  /* Empty 'and' expression returns #t */
		else       asmAsm (MVI, R0, ofalse); /* Empty 'or'  expression returns #f */
	} else {
		Lend = asmNewLabel();
		r2 = cdr(rexpr); /* Consider operand list */
		while (onull != r2) {
			/* At this point operand list is a pair or non-null on-pair */
			if (!objIsPair(r2)) { // TODO Need a general parsing stage instead of this inflow check.  It should also identify the last expression separately from the butlast
				compErrorRaise(orand?(Str)"Syntax error 'and'":(Str)"Syntax error 'or'");
				goto ret;
			}
			/* At this point operand list valid so far */
			rexpr = car(r2); /* Consider next expression */
			vmPush(r2 = cdr(r2)); /* Consider rest operand list and save */
			if (onull == r2) {
				compCompileExpr(flags); /* Tail call */
			} else {
				compCompileExpr(flags & ~CCTAILCALL); /* Not-tail call */
				if (orand) asmAsm(BEQI, R0, ofalse, Lend); /* Emit short circuit 'and' instruction */
				else       asmAsm(BNEI, R0, ofalse, Lend); /* Emit short circuit 'or' instruction */
			}
			r2 = vmPop(); /* Restore operand list.  Can't keep in r2 as it might get used. */
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
		BRTI, R0, TSYSCALL, Lsyscall,
		BRTI, R0, TCLOSURE, Lclosure,

	 LABEL, Lpopargs,
		BEQI, R1, (Obj)0, Lpopargsdone,
		POP, R2,
		ADDI, R1, (Obj)-1,
		BRA, Lpopargs,
	 LABEL, Lpopargsdone,
		PUSH, R0, /* Push the bad operator */
		MVI, R0, rsubexpr, /* Error situation.  Add sub s-expressions to stack Was MVI, R1, rsubexpr, PUSH, R1 */
		SYSI, sysListToStack,
		MVI, R1, 1l + objListLength(rsubexpr), /* Number of items on stack to dump */
		MVI, R0, "Illegal operator type", /* Illegal operator section.  For now just dump the arguments.  Doesn't return.*/
		SYSI, compSyscallError
	);

	/* Syscall operator section.  Reference the syscall address, set the
  	operand count then make the system call.  */
	asmAsm (
	 LABEL, Lsyscall,
		LDI, R0, R0, 0l, /*  Reference the syscall address then make the system call.  */
		SYS, R0
	);
	if (IsTailCall)
		asmAsm (RET);
	else {
 		Lend = asmNewLabel();
		asmAsm (BRA, Lend);
	}

	/* Closure operator section.  Load jump address into r2.  R1 is
	   argument count and r0 is the closure (which is needed as it
	   holds the lexical environment).
	*/
	asmAsm (
	 LABEL, Lclosure,
		LDI, R2, R0, 0l
	);
	if (IsTailCall)
		asmAsm(JMP, R2);
	else asmAsm(
		JAL, R2,
	 LABEL, Lend,
		POP, R9, /* Restores previous environment, ip and code registers. */
		POP, RB,
		POP, RA
	);

	DBEND(STR, compIsError()?" *ERROR*":"");
}


/* Compiles expression of the form (if testExpr (consequentExpr {value of testExpr}) alternateExpr)
*/
void compAIf (Num flags) {
 Num hasAlternate;
 Obj LfalseBraAddr, Lend;
	DBBEG();

	/* Parse the operands r1 = TEST, r2 = CONSEQUENT, and r3 = ALTERNATE */
	if (!compParseOperands(2)) hasAlternate = 0;
	else if (!compParseOperands(3)) hasAlternate = 1;
	else {
		compErrorRaise((Str)"Syntax error '=>'");
		goto ret;
	}

	if (hasAlternate) vmPush(r3); /* Save ALTERNATE */
	vmPush(r2); /* Save CONSEQUENT */

	DB("compiling test");
	rexpr = r1;
	compCompileExpr(flags & ~CCTAILCALL);

	if (compIsError()) {
		vmPop(); /* Pop CONSEQUENT */
		if (hasAlternate) vmPop(); /* Pop ALTERNATE */
		goto ret;
	}

	DB("compiling test logic");
	LfalseBraAddr = asmNewLabel();
	asmAsm(BEQI, R0, ofalse, LfalseBraAddr);

	DB("compiling consequent");
	/* Save execution state, possibly, since the following is the equivalent of compcombination */
	if (!((Num)flags & CCTAILCALL)) {
		asmAsm (
			PUSH, RA,
			PUSH, RB,
			PUSH, R9);
	}

	asmAsm(
		PUSH, R0 /* Push result of test expression on the stack.  Becomes argument to consequent. */
	);

	/* Compile consequent. */
	rexpr = vmPop();
	compCompileExpr(flags & ~CCTAILCALL);
	asmAsm(MVI, R1, 1l);  /* Set the argument count to 1.  Argument already on the stack. */
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
	r2 = cdr(rexpr); /* Skip symbol 'cond */
	while (objIsPair(r2)) { /* Over all clauses  expr = (<clause> ....) */
		r1 = car(r2); /* Consider next clause  r1 = <clause>  */
		/* Error if clause is not a list */
		if (!objIsPair(r1)) {
			rexpr = r1;
			compErrorRaise((Str)"Syntax error 'cond' clause");
			while(clauses--) vmPop(); /* Pop pushed clauses since not continuing */
			goto ret;
		} else {
			DB("Pushing clause");
			DBE objDisplay(r1, stderr);
			clauses++;
			vmPush(r1);
			r2 = cdr(r2); /* Consider next clause for this loop */
			if (selse == car(r1)) {
				/* Else clause matched, so stop pushing clauses and give warning if more clauses follow */
				if (r2 != onull) {
					fprintf (stderr, "\nWARNING: compCond: cond's else clause followed by more clauses ");
					objDisplay(r2, stderr);
				}
				r2 = onull;
			}
		}
	}

	/* Pop clauses building the if/or/begin tree bottom-up into r0 */
	DB (" Creating nested if/or/begin expression");
	r0 = onull;
	while (clauses--) {
		r5 = vmPop(); /* Consider clause r5 = <clause> = (r4 . r3) */
		r4 = car(r5); /* First expr */
		r3 = cdr(r5) ; /* Rest expr */
		if (selse == r4) {
			assert(onull == r0); /* This better be the first clause popped or not at all */
			r1=sbegin; r2=r3; objCons12();          /* (begin <expr> ...) */
		} else if (!objIsPair(r3)) {
			r1=r0;  r2=onull; objCons12();           /* (translated) */
			r1=r4;  r2=r0; objCons12();             /* (<test> (translated)) */
			r1=sor; r2=r0; objCons12();             /* (or <test> (translated)) */
		} else if (saif == car(r3)) {
			r3 = cdr(r3); /* Consider (r4 => . r3 */
			r1=r0;  r2=onull; objCons12();           /* (translated) */
			if (objIsPair(cdr(r3))) { /* Give warning if => clause followed by more clauses */
				fprintf (stderr, "\nWARNING: compCond: cond's => expr not a single expression ");
				objDisplay(r5, stderr);
			}
			r1=car(r3); r2=r0; objCons12();         /* (<expr> translated) */
			r1=r4;  r2=r0; objCons12();             /* (<test> <expr> translated) */
			r1=saif; r2=r0; objCons12();            /* (if <test> <expr> translated) */
		} else {
			r1=r0;  r2=onull; objCons12(); vmPush(r0); /* (translated) */
			r1=sbegin; r2=r3; objCons12();          /* (begin <expr> ...) */
			r1=r0; r2=vmPop(); objCons12();           /* ((begin <expr> ...) translated) */
			r1=r4;  r2=r0; objCons12();             /* (<test> (begin <expr> ...) translated) */
			r1=sif; r2=r0; objCons12();             /* (if <test> (begin <expr> ...) translated) */
		}
	}
	DB ("compCond translated ");
	DBE objDisplay(r0, stdout);
	rexpr = r0;
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
	r2 = cdr(rexpr); /* Skip symbol 'case and expression */
	if (!objIsPair(r2)) {
		compErrorRaise((Str)"Syntax error in case statement.  Missing key expression.");
		goto ret;
	}

	vmPush(car(r2)); /* Push key expression */
	r2 = cdr(r2); /* Consider all clauses */

	while (objIsPair(r2)) { /* Over all clauses  expr = (<clause> ....) */
		r1 = car(r2); /* Consider next clause  r1 = <clause>  */
		/* Error if clause is not a list */
		if (!objIsPair(r1)) {
			rexpr = r1;
			DB("Syntax error 'case' clause");
			compErrorRaise((Str)"Syntax error 'case' clause");
			while(clauses--) vmPop(); /* Pop pushed clauses since not continuing */
			goto ret;
		} else if (!objIsPair(car(r1)) && selse != car(r1)) {
			rexpr = r1;
			DB("Syntax error 'case' clause's datum field not a list");
			compErrorRaise((Str)"Syntax error 'case' clause's datum field not a list");
			while(clauses--) vmPop(); /* Pop pushed clauses since not continuing */
			goto ret;
		} else {
			DB("Pushing clause");
			DBE objDisplay(r1, stderr);
			clauses++;
			vmPush(r1);
			r2 = cdr(r2); /* Consider next clause for this loop */
			if (selse == car(r1)) {
				/* Else clause matched, so stop pushing clauses and give warning if more clauses follow */
				if (r2 != onull) {
					fprintf (stderr, "\nWARNING: compCase: case's else clause followed by more clauses ");
					objDisplay(r2, stderr);
				}
				r2 = onull;
			}
		}
	}

	/* Pop clauses building the if/or/begin tree bottom-up into r0 */
	DB (" Creating nested if/or/begin expression");
	r0 = onull;
	while (clauses--) {
		r5 = vmPop(); /* Consider clause r5 = <clause> = <datum expr ...> = (r4 . r3) */
		r4 = car(r5); /* Datum list or 'else symbol */
		r3 = cdr(r5) ; /* Rest expr */
		if (selse == r4) {
			assert(onull == r0); /* This better be the first clause popped or not at all */
			r1=r5; r2=onull; objCons12();
		} else {
			//if (r0 != onull) { r1=r0;  r2=onull; objCons12(); vmPush(r0); }
			vmPush(r0);                                /* Push (translated) or () if first clause */
			vmPush(r3);                                /* Push expressions */
			r1=r4; r2=onull; objCons12();             /* (datum) */
			r1=squote; r2=r0; objCons12();             /* '(datum) */
			r1=r0; r2=onull; objCons12();             /* ('(datum)) */
			r1=scase;  r2=r0; objCons12();             /* (case '(datum)) */
			r1=smemv;  r2=r0; objCons12();             /* (memv case '(datum)) */
			r1=r0;  r2=onull; objCons12();             /* ((memv case '(datum))) */
			r1=spairp;  r2=r0; objCons12();             /* (pair? (memv case '(datum))) */
			r1=r0;  r2=vmPop(); objCons12();           /* ((memv case '(datum)) expressions) */
			r1=r0; r2=vmPop(); objCons12();            /* (((memv case '(datum)) expressions) translated) */
		}
	}

	r1=scond; r2=r0; objCons12();    /* (cond ...) */

	r1=r0; r2=onull; objCons12();    /* ((cond ...)) */

	r1=vmPop();                   /* Consider key */
	vmPush(r0);                   /* Save ((cond ..)) */

	r2=onull; objCons12();       /* (key) */
	r1=scase; r2=r0; objCons12();  /* (case key) */
	r1=r0; r2=onull; objCons12();     /* ((case key)) */
	r1=r0; r2=vmPop(); objCons12();     /* (((case key)) (cond ...)) */
	r1=slet; r2=r0; objCons12();     /* (let ((case key)) (cond ...)) */

	DB ("compCase translated ");
	DBE objDisplay(r0, stdout);
	rexpr = r0;
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

	rexpr = r1; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TCLOSURE, Ltrue,
		MVI, R0, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, otrue,
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

	rexpr = r1; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BEQI, R0, onull, Ltrue,
		MVI, R0, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, otrue,
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

	rexpr = r1; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TPAIR, Ltrue,
		MVI, R0, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, otrue,
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

	rexpr = r1; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TVECTOR, Ltrue,
		MVI, R0, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, otrue,
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

	rexpr = r1; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TCHAR, Ltrue,
		MVI, R0, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, otrue,
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

	rexpr = r1; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TSTRING, Ltrue,
		MVI, R0, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, otrue,
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

	rexpr = r1; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TINTEGER, Ltrue,
		MVI, R0, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, otrue,
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

	rexpr = r1; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TSYMBOL, Ltrue,
		MVI, R0, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, otrue,
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

	rexpr = r1; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TPORT, Ltrue,
		MVI, R0, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, otrue,
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

	rexpr = r1; /* Consider and compile expression. */
	compCompileExpr(flags & ~CCTAILCALL);
	if (compIsError()) goto ret;

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BEQI, R0, oeof, Ltrue,
		MVI, R0, ofalse,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, otrue,
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
	rexpr = r0;
	compCompileExpr(flags);
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compQuasiquote (Num flags) {
	DBBEG();
	rexpr = cadr(rexpr); // Given (quasiquote <qq template>) pass <qq template>
	compTransformQuasiquote(0);
	rexpr = r0;
	DB("quasiquote transformation => ");
	DBE objDisplay(rexpr, stderr);
	compCompileExpr(flags);
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compQuote (Num flags) {
	DBBEG();
	asmAsm (
		MVI, R0, cadr(rexpr)
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
			PUSH, RA,
			PUSH, RB,
			PUSH, R9);
	}

	vmPush(car(rexpr)); /* Save operator parameter. */

	/* Compile operand expressions the last of which hopefully evaluates to a list of args.
	   The resulting arguments will be pushed onto the stack and passed to the function.  */
	rexpr = cdr(rexpr);
	while (objIsPair(rexpr)) {
		vmPush (cdr(rexpr)); /* Push rest */
		rexpr = car(rexpr); /* Consider expression  */
		compCompileExpr(flags & ~CCTAILCALL);
		asmAsm(PUSH, R0);
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
	asmAsm(MV, R3, R0); /* Save operator in r3 */

	if (compIsError()) goto ret;

	/* At this point stack has the arguments, the argument-list and r3 has function.
	   Want to transfers the argument-list items from list to the stack with r1 ending up
	   with the argument count.  Initially the argument count is the number of initial
	   non-list arguments to apply.
	*/
	Largcount = asmNewLabel();
	Largcountdone = asmNewLabel();
	asmAsm (
		MVI, R1, (Obj)(operandCount-1), /* Initialize operand count in r1 to number of initial arguments to apply. */
		POP, R0,    /* Pop argument-list. */
	 LABEL, Largcount,
		BEQI, R0, onull, Largcountdone,
		ADDI, R1, 1l, /* Inc argument count in r1. */
		LDI, R2, R0, 0l, /* Push the car. */
		PUSH, R2,
		LDI, R0, R0, 1l, /* Consider cdr. */
		BRA, Largcount,
	 LABEL, Largcountdone,
		MV, R0, R3     /* Operator back to r0 */
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
		asmAsm(JMP, R0);
	} else {
		asmAsm(
			PUSH, RA,  PUSH, RB,  PUSH, R9,
			JAL, R0,
			POP, R9,  POP, RB,  POP, RA
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

	/* r1 contains normalized formals and (r3 . (r4))  the body.  Hmmm */
	/* Transform (macro ... ...) => (lambda . transformed-macro-body) assigned to r0 */

	rexpr = vmPop(); /* Restore original macro expression...ignore all the parsing just done (for now) TODO */
	r1=slambda;  r2 = rexpr;  objCons12();

	asmStart();
	asmAsm(
		//SYSI, myfun,
		PUSH, R1, /* Save the argument count in R1 */
		MVI, R0, r0, /* The transformed (lambda ... ...) expression */
		SYSI, compSyscallCompile,
		PUSH, RA, PUSH, RB, PUSH, R9,
		JAL, R0,
		POP, R9, POP, RB, POP, RA,
		POP, R1, /* Restore argument count for the just compiled closure */
		LDI, R2, R0, 0l, /* load r2 with code block and call it.  it will return a closure.  */
		JMP, R2);
	asmAssemble();
	/* Sub ASM context ends */

	/* Generate code that generates a closure.  Closure returned in r0 created
	   from r1 (code) and rc (current/dynamic environment). */
	asmAsm(
		MVI, R1, r0, /* Load r1 with code block just compiled. */
		SYSI, sysNewClosure1Env, /* Create closure from r1 and env (rc, which isn't used by the code block just generated) */
		STI, R2, R0, 1l);
ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}

/* Stored stack expected in r3.
*/
void compSysReinstateContinuation (void) {
 Num length;
	DBBEG();

	if ((Int)r1==1) r0=vmPop();
	else {
		fprintf (stderr, "ERROR: compReinstateContinuation() bad argument count %d.\n", (Int)r1);
		exit (-1);
	}

	/* Reinstate stack and registers.
	*/
	length = memObjectLength(r3); /* The stored stack is in r3. */
	memcpy(rstack+8, r3, length*8); /* Copy objects into stack vector. */
	*(Obj*)rstack = rstack+length*8; /* Set the stack object pointer. */
	rcode = vmPop();
	rretcode = vmPop();
	rip = vmPop();
	rretip = vmPop();
	renv = vmPop();
	rretenv = vmPop();
	r1 = (Obj)1l; /* Let contnuation code know this is a call to the continuation */

	DBEND();
}

void compSyscallCreateContinuation (void) {
 Num length;
	DBBEG();
	vmPush(rretenv);
	vmPush(renv);
	vmPush(rretip);
	vmPush(rip);
	vmPush(rretcode);
	vmPush(rcode);
	length = memStackLength(rstack);
	objNewVector(length);
	memcpy(r0, rstack+ObjSize, length*ObjSize);
	vmPop(); vmPop(); vmPop(); vmPop(); vmPop(); vmPop();

	asmStart();
	asmAsm(
		MVI, R3, r0,  /* Copy of stack moved to r3 at runtime */
		SYSI, (Obj)compSysReinstateContinuation, /* This never returns */
		RET
	);
	asmAssemble();
	r1 = r0; /* Move new code block to r0 */

	sysNewClosure1Env();
	memVectorSet(r0, 1, rtge); /* Set to TGE just in case. */

	r1 = 0l; /* Let continuation code know this is a call to capture the continuation and to pass it to fn argument  */

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
		BEQI, R1, 1l, Lcontinuationcall
	);

	/* Is this a tail call?  if not save state. */
	if (!((Num)flags & CCTAILCALL))
		asmAsm (
			PUSH, RA,
			PUSH, RB,
			PUSH, R9);

	asmAsm(
		/* Push the continuation just create via compSyscallCreateContinuation.  This is the argument to the function */
		PUSH, R0
	);

	rexpr = car(rexpr); /* Consider and compile fn. */
	compCompileExpr(flags & ~CCTAILCALL);

	/* Setup application to fn */
	asmAsm(
		MVI, R1, 1l
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
		asmAssemble(); /* End this sub assembly context, with the new assembled code block in r0 */

		asmAsm(
			MVI, R0, r0,
			SYSI, osNewThread /* the osNewThread syscall returns thread ID integer object in r0 at runtime or #f on failure */
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
				PUSH, RA, // ip
				PUSH, RB, // code
				PUSH, R9  // env
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

			asmAsm(PUSH, R0);
			rexpr = vmPop(); /* Restore REST */

			if (compIsError()) { vmPop(); goto ret; } /* Pop operator */
		}

		r1 = vmPop(); /* Restore OPERATOR */

		/* Check combination is syntatically correct (not a malformed dotted list) */
		if (onull != rexpr) {
			compErrorRaise((Str)"Syntax error combination");
			goto ret;
		}

		/* Consider and compile OPERATOR.  Call complambda directly if possible.This
	   	This avoids tainting the sub expression stack with the transformed
	   	s-expression. */
		rexpr = r1; /* Consider OPERATOR and compile */
		compCompileExpr(flags & ~CCTAILCALL);

		if (compIsError()) goto ret;

		/* Emit code that applys args to syscall/closure (hopefully) operator */
		asmAsm (MVI, R1, operandCount); /* At runtime, r1 contains the stack argument count */
		compAsmCombination(flags);
	}

ret:
	DBEND(STR, compIsError()?" *ERROR*":"");
}


void compIntrinsic (Num flags) {
	DBBEG();
	asmAsm(MVI, R0, rexpr);
	DBEND();
}

void compPrimitiveAdd (Num flags) {
   DBBEG();
	asmAsm(MVI, R0, otrue);
	DBEND();
}


/* Recursive scheme expression compiler.  Translates an expression in
   rexpr into opcodes which are sent to the assembler.
    rexpr <= S-expression to compile
    return => flags
*/
void compCompileExpr (Num flags) {
 Obj op;
	DBBEG(" <= ");
	DBE objDisplay(rexpr, stderr);

//fprintf(stderr, "[CompileExpr:"NUM"]", flags & CCTAILCALL);
	compPushSubExpr(rexpr);

	switch (memObjectType(rexpr)) {
		case TSYMBOL: compSymbol(flags); break;
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
                    else if (objIsSymbol(op) && (r1=op, sysTGEFind(), r0 != onull) && memIsObjectType(car(r0), TPRIMITIVE)) {
                       objWrite(car(r0), stderr);
                       objWrite(cdr(r0), stderr);
                       (*(void(**)(Num))car(r0))(flags); 
                    }
			           else compCombination(flags);
			           break;
		default     : compIntrinsic(flags);
	}

	compPopSubExpr();

	DBEND(STR, compIsError()?" *ERROR*":"");
}



/* Compiles the expression in r0 into a code object runable by the virtual machine.
      r0 <= scheme expression to compile
    rexpr = temp
       r0 => VM code block or #f if an exception/error occured while compiling
*/
void compCompile (void) {
	DBBEG();
	if (otrue == odebug) { sysDumpEnv(renv); }
	compErrorReset();
	asmInit();
	rexpr = r0;
	compCompileExpr(CCTAILCALL);

	if (compIsError()) {
		asmReset();
		compThrowCompilerError();
		r0 = ofalse;
	} else {
		/* Finalize the assembled code with a 'ret' opcode */
		asmAsm(RET);
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
		memRootSetRegister(rexpr);
		memRootSetRegister(rcomperror);
		memRootSetRegister(rcomperrormessage);
		memRootSetRegister(rcomperrortrace);
		memRootSetRegister(rsubexpr);

		rsubexpr = onull;

		DB("Registering primitive operators");
      sysDefinePrimitive((Func)compPrimitiveAdd, ":+:");

		DB("Registering static pointer description strings");
		memPointerRegister(sysNewClosure1Env); 
		memPointerRegister(compSyscallCompile); 
		memPointerRegister(compSysReinstateContinuation); 
		memPointerRegister(compSyscallCreateContinuation); 
		memPointerRegister(osNewThread); 
		memPointerRegister("Too many arguments to closure"); 
		memPointerRegister("Illegal operator type");
		memPointerRegister(compSyscallError);
		memPointerRegister("runtime error");
		memPointerRegister("Compiler error");
		memPointerRegister("Too many arguments to function");
		memPointerRegister("Not enough arguments to closure");
		memPointerRegister(sysListToStack);
		memPointerRegister(compSyscallVerifyVectorSetB);
		memPointerRegister(compSyscallVerifyVectorRef);
	} else {
		DB("Module already activated");
	}
	DBEND();
}



#undef DB_DESC
#undef DEBUG
