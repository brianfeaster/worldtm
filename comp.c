#define DEBUG 0
#define DB_DESC "COMP "
#include "debug.h"
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h> /* exit() */
#include <string.h> /* memcpy() */
#include "comp.h"
#include "os.h"
#include "sys.h"
#include "obj.h"
#include "asm.h"
#include "vm.h"
#include "mem.h"
/* Functions related to compiling scheme expressions into
   virtual machine code blocks

   TODO This compiler module is not reentrant.
*/


Num wscmDebug=0;
Num compExpression (Num flags);


/* Compiler flags passed around by comp functions.
*/
static const Num TAILCALL  = 0x00010000;
static const Num NODEFINES = 0x00020000;
/*
static const Num R8 =        0x00000100;
static const Num R7 =        0x00000080;
static const Num R6 =        0x00000040;
static const Num R5 =        0x00000020;
static const Num R4 =        0x00000010;
static const Num R3 =        0x00000008;
static const Num R2 =        0x00000004;
static const Num R1 =        0x00000002;
static const Num R0 =        0x00000001;
*/



void compWrite (void) {
	objDump(r0, stdout);
}


/* Has compiler encountered an error flag?
*/
Num CompError;

void compError (void) {
	fprintf(stderr, "compError: "STR, r0);
	while (r1--) {
		sysDisplay(vmPop(), stderr);
	}
	exit(-1);
}


/* Dump the illegal operator error message including the offending expression
*/
void compIllegalOperator (void) {
	fprintf (stderr, "ERROR: Illegal operator (");
	objDump (r0, stderr);
	while ((Int)r1--) {
		fprintf (stderr, " ");
		objDump (vmPop(), stderr);
	}
	fprintf(stderr, ")");
	r0 = false; /* TODO return false for now.  Add a call to error continuation. */
}

/* Remember BASIC?  This is a 'REMark' or comment syntatic operator.
*/
void compRem () {
	DBBEG();
	DBEND();
}

/* Compiles s-expression in r0 into code block in r0.  Probably messes up
   a bunch of registers.
	TODO: does this mangle r19/1a/1b retenv/retip/retcode?
*/
void compSysCompile (void) {
	DBBEG();
	rexpr=r0;
	vmPush(renv);
	CompError=0;
	asmAsm ( /* Keep track of original expression for debugging. */
		BRA, 8,
		rexpr,
		END
	);
	if (compExpression(0))
	{
		r0 = "compSysCompile: compExpression failed";
		compError();
		goto ret;
	}
	asmAsm(
		RET,
		END
	);
	asmNewCode();
	if (wscmDebug) vmDebugDumpCode(r0, stderr); // Dump the code block after compiling code during runtime.
	renv=vmPop();
ret:
	DBEND("  =>  ");
	DBE objDump (r0, stderr);
}

void compEval (Num flags) {
	DBBEG();
	rexpr = cadr(rexpr);
	compExpression(flags & ~TAILCALL);
	asmAsm(
		SYSI, compSysCompile,
	END);
	if (flags & TAILCALL) {
		asm(J0);
	} else {
		asmAsm(
			PUSH1A, PUSH1B, PUSH19,
			JAL0,
			POP19, POP1B, POP1A,
		END);
	}
	DBEND();
}

/* Doesn't need compiling so just return it.
*/
void compSelfEvaluating (void) {
	DBBEG();
	asm(MVI0); asm(rexpr);
	DBEND();
}

/* Run time symbol lookup syscall.  If a symbol in r1 found in TGE mutate code
   to just reference the binding's value rather than make this syscall.
*/
void compTGELookup (void) {
	DBBEG();
	sysTGEFind();
	if (r0 == null) {
		printf ("ERROR: Unbound symbol:");
		objDump(r1, stdout);
		r0 = r1;
		// TODO  Kill thread, stop machine, return to monitor/shell?
	} else {
		DB("SYS    found in tge @ opcode %x", (Num)rip-4);
		/* Specialization optimization.  Muate code that originally called
		   this function into a code that references the binding's value. */
		memVectorSet(rcode, (Num)rip-4, MVI0);  memVectorSet(rcode, (Num)rip-3, r0);
		memVectorSet(rcode, (Num)rip-2, LDI00); memVectorSet(rcode, (Num)rip-1, 0);
		/* Force virtual machine to run this code. */
		rip -= 4;
	}
	DBEND();
}

void compVariableReference (Num flags) {
 Int ret, depth;
	DBBEG();
	DBE objDump(rexpr, stderr);
	r1 = rexpr;

	/* Scan local environments.  Returned is a 16 bit number, the high 8 bits
	   is the environment chain depth, the low 8 bits the binding offset. The
	   offset will be 2 or greater if a variable is found in any environment
	   excluding the global environment. */
	ret = sysEnvFind();
	if (ret) {
	DB("   found in a local environment %02x", ret);
		/* Emit code that traverses the environment chain and references the
		   proper binding. */
		if ((ret>>8) == 0) {
			asm(LDI01C); asm(ret & 0xff);
		} else {
			asm(LDI01C); asm(0l);
			for (depth=1; depth < (ret>>8); depth++) {
				asm(LDI00); asm(0l);
			}
			asm(LDI00); asm(ret & 0xff); /* Mask the offset value. */
		}
	} else {
		/* Scan tge... */
		sysTGEFind();
		if (r0 == null) {
			DB("   can't find in TGE...maybe at runtime");
			asm(MVI1); asm(rexpr);
			asm(SYSI); asm(compTGELookup);
		} else {
			DB("   found in TGE");
			asm(MVI0); asm(r0);
			asm(LDI00); asm(0l);
		}
	}

	DBEND();
}

/* Transform expr:((fn formals) body) into the form
   r0:(fn (lambda formals body)).  No syntic error checking is performed
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
	r1=r0;      r2=null; objCons12(); /* ((lambda formals body)) */
	r1=r3;      r2=r0;   objCons12(); /* (fn (lambda formals body)) */
	
	DBEND("  =>  ");
	DBE objDump(rexpr, stderr);
}


/* Define by itself only makes sense when evaluated in the top level
   environment.  Any other use of it is just syntatic sugar for the various
   let expressions.  For now define will always work and assumes TGE as the
   current working environment.
*/
void compDefine (Num flags) {
	DBBEG();

	if (flags & NODEFINES) {
		//CompError = 1;
		fprintf(stderr, "ERROR: compDefine(): Define not allowed here");
		objDump(rexpr, stderr);
	} else {
		rexpr = cdr(rexpr); /* Skip 'define symbol. */

		vmPush(renv);
		renv = rtge;

		/* If the expression is of the form ((...) body) transform. */
		if (objIsPair(car(rexpr))) {
			compTransformDefineFunction();
			rexpr = r0;
		}

		if (TSYMBOL == memObjectType(r1=car(rexpr))) {
			/* Bind (if not already bound) the symbol and get its binding. */
			sysTGEBind();
			/* Emit code to set the binding's value. */
			rexpr = cdr(rexpr);
			if (objIsPair(rexpr)) {
				vmPush(r0); /* Save binding. */
				rexpr = car(rexpr); /* Consider this definition's expression and compile. */
				compExpression((Num)flags & ~TAILCALL);
				asm(MVI1); asm(vmPop()); /* Load r1 with saved binding. */
				asm(STI01); asm(0l);    /* Set binding's value. */
			} else {
				write (2, "ERROR: compDefine(): Missing expression.", 40);
			}
		} else  {
			write (2, "ERROR: compDefine(): Not a symbol:", 34); objDump(r1, stderr);
		}

		renv = vmPop();
	}

	DBEND();
	return;
}

/* Run time symbol mutate syscall.  If a symbol in r1 found in TGE mutate code
   to just reference the binding's and mutate binding's value with r0.
*/
void compTGEMutate (void) {
	DBBEG();
	r2=r0; /* Since a syscall, save value we're trying to set!. */
	sysTGEFind();
	if (r0 == null) {
		printf ("ERROR: Unbound symbol '");
		objDump(r1, stdout);
		printf ("'\n");
		r0 = r2; /* Return value. TODO  Kill thread, stop machine, return to monitor/shell? */
	} else {
		DB("SYS    found in tge at opcode %0x", (Int)rip-4);
		/* Specialization optimization.  Muate code that originally called
		   this function into a code that references the binding's value. */
		memVectorSet(rcode, (Num)rip-4, MVI1);  memVectorSet(rcode, (Num)rip-3, r0);
		memVectorSet(rcode, (Num)rip-2, STI01); memVectorSet(rcode, (Num)rip-1, 0);
		r0 = r2; /* Restore value we're trying to set!. */
		/* Force virtual machine to run this code. */
		rip -= 4;
	}
	DBEND();
}


void compSetb (Num flags) {
 Int ret, depth;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'set! symbol. */
	vmPush(car(rexpr)); /* Save symbol. */
	/* Emit code that evaluates the expression. */
	rexpr = cadr(rexpr);
	compExpression(flags & ~TAILCALL);

	r1 = vmPop(); /* Restore symbol. */
	/* Scan local environments.  Returned is a 16 bit number, the high 8 bits
	   is the environment chain depth, the low 8 bits the binding offset. The
	   offset will be 1 or greater if a variable is found in any environment
	   excluding the global environment. */
	ret = sysEnvFind();
	if (ret) {
	DB("   found in a local environment %02x", ret);
		/* Emit code that traverses the environment chain and references the
		   proper binding. */
		if (ret>>8 == 0) {
			asm(STI01C); asm(ret & 0xff);
		} else {
			asm(LDI11C); asm(0l);
			for (depth=1; depth < (ret>>8); depth++) {
				asm(LDI11); asm(0l);
			}
			asm(STI01); asm(ret & 0xff); /* Mask the offset value. */
		}
	} else {
		/* Scan tge... */
		sysTGEFind();
		if (r0 == null) {
			DB("   can't find in TGE...maybe at runtime");
			asm(MVI1); asm(r1);
			asm(SYSI); asm(compTGEMutate);
		} else {
			DB("   found in TGE");
			asm(MVI1); asm(r0);
			asm(STI01); asm(0l);
		}
	}

	DBEND();
}

/* Transform expr:((define x q) (define y r) body)
       => r0:((lambda (x y) (set! x q) (set! y r) body) () ())
*/
void compTransformInternalDefinitions(void) {
 Int definitionsCount=0;
	DBBEG();

	/* Save lambda body. */
	while (objIsPair(rexpr) && objIsPair(car(rexpr)) && sdefine == caar(rexpr)) {
		vmPush(cdr(rexpr));
		rexpr = cdar(rexpr); // Consider next expression and skip 'define.
		if (objIsPair(car(rexpr))) {
			compTransformDefineFunction(); // Returns (fn (lambda formals body))
		} else {
			r0=rexpr;
		}
		rexpr = vmPop();
		vmPush(r0);
		definitionsCount++;
	}

	/* expr is now pointing at body of function.  If there were any internal
	   definitions, form an equivalent letrec expression. */
	if (definitionsCount) {
		r4=null; /* Local variable list.  Start out empty. */
		r5=rexpr; /* Set! expressions and body list. Start out with body. */
		r6=null; /* Null arguments list. */
		while (definitionsCount--) {
			r3=vmPop();/* Considered saved transformed define expression. */
			/* Prepend formal argument to list. */
			r1=car(r3); r2=r4; objCons12(); r4=r0;
			/* Prepend set! expression to list. */
			r1=ssetb;   r2=r3; objCons12();  /* Create (set! var ...) */
			r1=r0;      r2=r5; objCons12(); r5=r0;
			/* Prepend another null to argument list. */
			r1=null;    r2=r6; objCons12(); r6=r0;
		}
		r1=r4;      r2=r5;  objCons12();
		r1=slambda; r2=r0;  objCons12();
		r1=r0;      r2=r6;  objCons12();
		/* Create list consisting of this new expression. */
		r1=r0;      r2=null; objCons12();
		rexpr=r0;
	}
	
	DBEND("  =>  ");
	DBE objDump(rexpr, stdout);
}

/*
   Given (args body) in expr (r18) create a new code block that basically
   handles a call to a closures function.  The code assumes the closure is
   in r0.  A closure is a pair (code . environment) containing the code itself
   and the closures instantiated environment.

   Expr assumed to be of the form (args body) where args is currently of the
   form: (sym+).  Emit code that keeps track of the current environment

	Emitted code assumes the caller's code sets up the stack with all evaluated
   arguments pushed with r1 containing the arg count.  The count includes
   arguments to be grouped into the dotted formal argument list.

   Create an extended environment given:
   wscmExtendEnvironment moved from system call to inlined assembly.
    r1   - arg count
    r2   - lexical environment
    r3   - symbol list
    r1f (stack) - arguments on the stack.

  IE: ==> #( #<PARENT-ENV> (x y z rest) 1 2 3 (4 5 6))

 A local environment is of the form #(parent-env (x y . z) 1 2 (3 4 5))

    #( * (a . ()))
        \
         #( * (x y . z))
             \
              (TGE (square . #<closure>) (x . 5) (y . 9))

 A symbol lookup will be:
  (1) Compiled either as direct reference to a global environment binding
  (2) Compiled into a series of parent environment references and one
      local environment reference.
  (3) A syscall that attempts to locate the named binding which will then
      code modify itslef into case (1).
*/
void compLambdaBody (Num flags) {
 Num opcodeStart;
	DBBEG();
	DBE objDump(rexpr, stdout);

	/* Since we're creating a new code object, save the current asmstack and
	   create a new one to start emitting opcodes to. */
	vmPush(rasmstack);
	r0=memNewStack();
	rasmstack=r0;

	/* The first opcode emitted is a branch past a pointer to the original
	   expression being compiled.  This is a quick and dirty debugging aid. */
	asmAsm(
		BRA, 8l,
		rexpr,
		END
	);

	/* Emit code that extends stack-stored arguments (Free variables can be
	   statically compiled?  r2 is assumed to hold the environment to be
	   extended, r1 the argument count, r3 the formal arguments. */
	if (null == car(rexpr)) {
		/* Since a lambda with empty formals list, emit code which doesn't extend
		   the environment but instead sets env to the containing closure's env
		   or TGE if this is a top level definition (which will always be for 'lambda
		   and not 'macro) */
		if (renv==rtge) asm(MV1C18);
		else { asm(LDI1C0); asm(1l); }

		opcodeStart = memStackLength(rasmstack);
		asmAsm (
			BEQI1, 0, ADDR, "expectedNoArgs",
			MVI0, rexpr, /* Error situation.  Add expression to stack */
			PUSH0,
			ADDI1, 1l,
			MVI0, "Too many arguments to closure",
			SYSI, compError, /* Error correction */
			LABEL, "expectedNoArgs",
			END);
		asmCompileAsmstack(opcodeStart);
	} else {
		/* Emit code that extends the environment.  Pops the top most arguments
		   into a list for the 'rest' formal parameter  (lambda (a b . rest)...).
		   R3 contains the non-dotted formal parameter length (via the Normalize
		   function above). */
		opcodeStart = memStackLength(rasmstack);

		/* Temporarily save lexical environment, from closure in r0 or tge, to r5.
		   The stored environment might be null in which case keep track of
		   the current/dynamic environment instead of the stored lexical.  Use
		   TGE when a top level definition.  See also the similar situation in this
		   if blocks true clause with the empty formals case. */
		if (car(renv)==rtge) asm(MV518);
		else { asm(LDI50); asm(1l); }

		asmAsm (
			BNEI5, null, ADDR, "keepLexicalEnvironment",
			MV51C,
		LABEL, "keepLexicalEnvironment",
			MVI0, null, /* Initial formal argument 'rest' value (empty list). */
			/* r3 is non-dotted formal argument length. */
			BLTI1, r3, ADDR, "notEnoughArguments",
			BEQI1, r3, ADDR, "normalFormals",
			END);

		/* Emit code for functions lacking a dotted formal argument.  This code
		   will be reached if there are more values passed to the function than
		   there are formal arguments.  Otherwise it will just continue to build
		   the dotted formal list. */
		if (r4==0) {
			asmAsm (
				MVI0, rexpr, /* Add expression to stack */
				PUSH0,
				ADDI1, 1l,
				MVI0, "Too many arguments to function",
				SYSI, compError, /* Error correction */
				END);
		}

		asmAsm (
		LABEL, "buildRestList",
			MV30,
			POP2,
			SYSI, objCons23,
			ADDI1, -1l,
			BNEI1, r3, ADDR, "buildRestList",
			BRA, ADDR, "normalFormals",
		LABEL, "notEnoughArguments",
			MVI0, rexpr, /* Add expression to stack */
			PUSH0,
			ADDI1, 1l,
			MVI0, "Not enough arguments to closure",
			SYSI, compError, /* Error correction */
			PUSH0,
			ADDI1, 1l,
			BNEI1, r3, ADDR, "notEnoughArguments",
		LABEL, "normalFormals",
			PUSH0,
			/* Create the local environment. r1 is the length of the vector.
			   3 is added to account for the parent env, formal argument list
			   and rest formal argument. */
			ADDI1, 3l,
			SYSI,  objNewVector1, /* New vector in r0 of size imm:r1. */
			STI50, 0l, /* Set parent link. */
			/* Set the environment's normalized formal argument list which was
			   created before the call to this C function. */
			MVI3,  cdr(renv),
			STI30, 1l,
			END
		);
		asmCompileAsmstack(opcodeStart);

		/* Emit code that pops arguments off stack and stores into proper
		   local environment locations.  */
		r3++;
		while (r3--) {
			asmAsm (
				POP2,
				STI20, r3+2l,
				END
			);
		}
		/* Set env register to the newly extended environment. */
		asm(MV1C0);
	}

	/* Skip lambda's formal argument list. */
	rexpr = cdr(rexpr);

	/* Compile expressions in lambda block (all but the last).  If the lambda
	   body is empty, emit code that returns null.  This is not to r5rs
	   specification which requires body contain a sequence of one or more
	   expressions. */
	if (rexpr == null) {
		asm(MVI0); asm(null);
		DB("   Empty function body.");
	} else {
		/* Transform internal definitions, if any, and body into equivalent
		   expanded letrec and body, ie:(((lambda (f) (set! f ...) body) () () ...)).*/
		compTransformInternalDefinitions();
		while (cdr(rexpr) != null) {
			DB("   Lambda non-tail optimization");
			vmPush(cdr(rexpr)); /* Push next expr. */
			rexpr = car(rexpr);
			compExpression((flags & ~TAILCALL) | NODEFINES);
			rexpr = vmPop();
		}
		DB("   Lambda tail optimization");
		rexpr = car(rexpr);
		compExpression(flags | TAILCALL | NODEFINES);
	}

	asm(RET);
	asmNewCode(); /* Transfer code stack to fixed size code vector. */

	/* Revert back to code block we're generating. */
	rasmstack=vmPop();

	DBEND("  =>  ");
	DBE objDump (r0, stderr);
}



/* Normalize a scheme formals list into an internal normalized formals
   environment list.  A proper list with a symbol or null as the "rest"
   formal.

   (x)       ->  (x ())
   (x y)     ->  (x y ())
   (x . r)   ->  (x r)
   (x y . r) ->  (x y r)
   r         ->  (r)
   ()        ->  (())

   Given   r0   A lambda expression's formals list

   Uses    r1   List creation

   Return  r0   Normalized formal parameter list
           r3   Number of non-dotted formal parameters
           r4   0 or 1 dotted formals
*/
void compNormalizeFormals(void) {
 Num i;
	r3=0; /* Keep track of non-dotted formal count. */

	/* Push formals onto stack. */
	while (objIsPair(r0)) {
		r3++;
		vmPush(car(r0));
		r0=cdr(r0);
	}

	/* Keep track of the existence of a dotter formal */
	r4 = (r0==null) ? (Obj)0 : (Obj)1;

	/* Pop formals from stack creating list of args starting
      with (()) or (dotted-formal) */
	r1=r0;  r2=null;  objCons12();
	i=(Num)r3;
	while (i--) { r2=r0;  r1=vmPop();  objCons12(); }
}



/* Uses  r0 r1 r2 r3 r4
*/
void compLambda (Num flags) {
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'lambda. */

	vmPush(renv); /* Save env. */

	/* Extend pseudo environment only if the formals list is not empty to
	   mimic the runtime optimized environment chain.   A pseudo environment
	   is just the pair (parent-environment . formals-list)*/
	if (car(rexpr) != null) {
		r0=car(rexpr);
		compNormalizeFormals(); /* Create normalized list in r0, length in r3, dotted-formal bool in r4. */
		r1=renv;  r2=r0;  objCons12();  renv=r0;
	}

	/* Create closures code block in r0. */
	compLambdaBody(flags);

	renv = vmPop(); /* Restore env. */

	/* Generate code that generates a closure.  Closure returned in r0 created
	   from r1 (code) and r1c (current environment). */
	asmAsm(
		MVI1, r0, /* Load r1 with code. */
		SYSI, sysNewClosure1Env, /* Create closure from r1 and env (r1c) */
		END);

	DBEND();
}



void compMacro (Num flags) {
	DBBEG();

	vmPush(rasmstack);
	r0=memNewStack();
	rasmstack=r0;

	/* Transform (macro ... ...) => (lambda .. ...) assigned to r0 */
	r1=slambda;  r2 = cdr(rexpr);  objCons12();

	asmAsm(
		PUSH1,
		MVI0, r0,
		SYSI, compSysCompile,
		PUSH1A, PUSH1B, PUSH19,
		JAL0,
		POP19, POP1B, POP1A,
		LDI20, 0l, /* load r2 with code and jump. */
		POP1,
		J2,
	END);

	asmNewCode(); /* Transfer code stack to fixed size code vector into r0. */
	rasmstack=vmPop();

	/* Generate code that generates a closure.  Closure returned in r0 created
	   from r1 (code) and r1c (current environment). */
	asmAsm(
		MVI1, r0, /* Load r1 with code block just compiled. */
		SYSI, sysNewClosure1Env, /* Create closure from r1 and env (r1c) */
		MVI2, null, /* Replace stored lexical environment with null so dynamic environment is used when applied */
		STI20, 1l,
	END);

	DBEND();
}



void compVerifyVectorRef (void) {
	if (*(Int*)r0 < 0 || memObjectLength(r1) <= *(Int*)r0) {
		fprintf (stderr, "\nERROR::out of bounds:  (vector-ref ");
		objDump(r1, stderr); fprintf (stderr, " ");
		objDump(r0, stderr); fprintf (stderr, ")");
		compError();
	}
}

void compVerifyVectorSetB (void) {
	if (*(Int*)r2 < 0 || memObjectLength(r1) <= *(Int*)r2) {
		fprintf (stderr, "\nERROR::out of bounds:  (vector-set! ");
		objDump(r1, stderr); fprintf (stderr, " ");
		objDump(r2, stderr); fprintf (stderr, " ");
		objDump(r0, stderr); fprintf (stderr, ")");
		compError();
	}
}

void compVectorRef (Num flags) {
	DBBEG();
	vmPush(car(cddr(rexpr))); /* Save index expression. */
	rexpr = cadr(rexpr);       /* Compile Vector expression. */
	compExpression(flags & ~TAILCALL);
	rexpr = vmPop();            /* Compile index expression. */
	if (TINTEGER == memObjectType(rexpr)) {
		/* Load static integer value into register. */
		asm(LDI00); asm(*(Int*)rexpr);
	} else {
		asm(PUSH0);
		compExpression(flags & ~TAILCALL);
		asm(POP1);
		asm(SYSI); asm(compVerifyVectorRef);
		/* Load object's integer value into register. */
		asm(LDI20); asm(0l); /* This fails runtime type check */
		asm(LD012);
	}
	DBEND();
}

void compVectorVectorRef (Num flags) {
	DBBEG();
	rexpr=cdr(rexpr);    /* Skip 'vector-vector-ref. */
	vmPush(cadr(rexpr));  /* Save 1st index expressions. */
	vmPush(car(rexpr));   /* Save vector expressions. */

	rexpr=car(cddr(rexpr)); /* Compile 2nd expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);

	rexpr = vmPop();     /* Restore and compile vector expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);

	rexpr = vmPop();     /* Restore and compile 1st index expression. */
	compExpression(flags & ~TAILCALL);

	asmAsm (
		POP1,     /* Restore vector. */
		SYSI, compVerifyVectorRef,
		LDI20, 0l, /* Load 1st index object integer value into register. */
		LD012,    /* Index the vector. */
		MV10,     /* Move the sub-vector into r1. */
		POP0,     /* Restore 2nd index object. */
		SYSI, compVerifyVectorRef,
		LDI20, 0l, /* Load 2nd index object integer value into register. */
		LD012,    /* Index the sub-vector. */
		END
	);
	DBEND();
}

void compVectorSetb (Num flags) {
	DBBEG();
	rexpr=cdr(rexpr); /* Skip 'vector-set!. */
	vmPush(car(cddr(rexpr))); /* Save new-value expression. */
	vmPush(cadr(rexpr));      /* Save index expression. */
	/* Consider and compile Vector expression. */
	rexpr = car(rexpr);
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);           /* Save vector object. */
	/* Pop and compile index expression. */
	rexpr=vmPop();
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);           /* Save offset object. */
	/* Pop and compile new-value expression. */
	rexpr=vmPop();
	compExpression(flags & ~TAILCALL);
	asmAsm (
		POP2,       /* Pop offset object. */
		POP1,       /* Pop vector object. */
		SYSI, compVerifyVectorSetB,
		LDI22, 0,   /* Load offset object's integer value into register. */
		ST012,      /* Store new-value object in vector. */
		END
	);
	DBEND();
}

void compVectorVectorSetb (Num flags) {
	DBBEG();
	rexpr=cdr(rexpr);        /* Skip 'vector-vector-set!. */
	vmPush(cadr(cddr(rexpr)));/* Save new-value expression. */
	vmPush(cadr(rexpr));      /* Save 1st index expression. */
	vmPush(car(rexpr));       /* Save vector expression. */

	rexpr = car(cddr(rexpr)); /* Consider and compile 2nd index expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);           /* Save vector object. */

	/* Pop and compile vector expression. */
	rexpr=vmPop();
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);           /* Save offset object. */

	/* Pop and compile 1st index expression. */
	rexpr=vmPop();
	compExpression(flags & ~TAILCALL);

	asmAsm (
		POP1,     /* Restore vector. */
		SYSI, compVerifyVectorRef,
		LDI20, 0l, /* Load 1st index object integer value into register. */
		LD012,    /* Index the vetor. */
		PUSH0,    /* Save sub-vector. */
		END
	);

	/* Pop and compile new-value expression. */
	rexpr=vmPop();
	compExpression(flags & ~TAILCALL);
	asmAsm (
		POP1,       /* Pop vector object. */
		POP2,       /* Pop offset object. */
		SYSI, compVerifyVectorSetB,
		LDI22, 0l,   /* Load offset object's integer value into register. */
		ST012,      /* Store new-value object in vector. */
		END
	);
	DBEND();
}

void compCons (Num flags) {
	DBBEG();
	rexpr = cdr(rexpr);      /* skip 'cons. */
	vmPush(cadr(rexpr));      /* Save cdr expression. */
	rexpr = car(rexpr);      /* Compile car expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);
	rexpr = vmPop();          /* Restore and compile cdr expression. */
	compExpression(flags & ~TAILCALL);
	asmAsm (
		POP1,
		MV20,
		SYSI, objCons12,
		END
	);
	DBEND();
}

/* Parse the form (? *) placing * into r1
   Return: 0 success  -1 error
*/
Int parseUnary (void) {
	r0 = cdr(rexpr);
	if (!objIsPair(r0)) return -1;
	r1 = car(r0);
	if (cdr(r0) != null) return -1;
	return 0;
}

void compCar (Num flags) {
 Num opcodeStart;
	DBBEG();
	if (parseUnary()) {
		CompError = 1;
		objNewString((u8*)"ERROR: syntax error:", 20);  vmPush(r0);
		vmPush(rexpr);
		r1=(Obj)2;
		goto ret;
	}
	vmPush(rexpr); /* Save expression. */
	rexpr = r1;  /* Consider and compile expression parsed. */
	compExpression(flags & ~TAILCALL);
	rexpr = vmPop(); /* Restore expression. */
	objNewString((Str)"RUNTIME ERROR:", 14);
	opcodeStart = memStackLength(rasmstack);
	asmAsm(
		BRTI0, TPAIR, ADDR, "car",
		MVI0, r0,
		PUSH0,
		MVI0, rexpr,
		PUSH0,
		MVI1, 2l,
		SYSI, compError,
		LABEL, "car",
		LDI00, 0l, /* Perform car. */
		END
	);
	asmCompileAsmstack(opcodeStart);
ret:
	DBEND();
}

void compCdr (Num flags) {
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		r0 = "ERROR: cdr illegal operand count: ";
		CompError = 1;
		goto ret;
	}
	rexpr = cadr(rexpr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);
	asm(LDI00); asm(1);
ret:
	DBEND();
}

void compSetCarB (Num flags) {
	DB("-->compSetCarB");
	rexpr = cdr(rexpr); /* Skip set-car! symbol. */
	if (rexpr == null) {
		printf ("ERROR: set-car! illegal pair expression: ");
		objDump (rexpr, stdout);
		goto ret;
	}
	vmPush(car(rexpr)); /* Save pair expression. */
	rexpr = cdr(rexpr);
	if (rexpr == null) {
		printf ("ERROR: set-car! illegal object expression: ");
		objDump (rexpr, stdout);
		goto ret;
	}
	rexpr = car(rexpr);/* Consider and compile object expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);
	rexpr = vmPop();
	compExpression(flags & ~TAILCALL);
	asm(POP2);
	asm(STI20); asm(0l);
ret:
	DB("<--compSetCarB");
}

void compSetCdrB (Num flags) {
	DB("-->compSetCdrB");
	rexpr = cdr(rexpr); /* Skip set-cdr! symbol. */
	if (rexpr == null) {
		printf ("ERROR: set-cdr! illegal pair expression: ");
		objDump (rexpr, stdout);
		goto ret;
	}
	vmPush(car(rexpr)); /* Save pair expression. */
	rexpr = cdr(rexpr);
	if (rexpr == null) {
		printf ("ERROR: set-cdr! illegal object expression: ");
		objDump (rexpr, stdout);
		goto ret;
	}
	rexpr = car(rexpr);/* Consider and compile object expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);
	rexpr = vmPop();
	compExpression(flags & ~TAILCALL);
	asm(POP2);
	asm(STI20); asm(1l);
ret:
	DB("<--compSetCdrB");
}

void compProcedureP (Num flags) {
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		write (1, "ERROR: null? illegal operand count: ", 36);
		objDump (rexpr, stdout);
	}
	rexpr = cadr(rexpr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);
	asm(BRTI0); asm(TCLOSURE); asm(4*8l);
	asm(MVI0); asm(false);
	asm(BRA); asm(2*8l);
	asm(MVI0); asm(true);
	DBEND();
}

void compNullP (Num flags) {
	DB("-->compNullP");
	if (!objIsPair(cdr(rexpr))) {
		write (1, "ERROR: null? illegal operand count: ", 36);
		objDump (rexpr, stdout);
	}
	rexpr = cadr(rexpr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);
	asm(BRTI0); asm(TNULL); asm(4*8l);
	asm(MVI0); asm(false);
	asm(BRA); asm(2*8l);
	asm(MVI0); asm(true);
	DB("<--compNullP");
}

void compPairP (Num flags) {
	DB("-->compPairP");
	if (!objIsPair(cdr(rexpr))) {
		write (1, "ERROR: pair? illegal operand count: ", 36);
		objDump (rexpr, stdout);
		return;
	}
	rexpr = cadr(rexpr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);

	asmAsm(
		BRTI0, TPAIR, 4*8l,
		MVI0, false,
		BRA, 2*8l,
		MVI0, true,
		END
	);

	DB("<--compPairP");
}

void compVectorP (Num flags) {
 Num opcodeStart;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		write (1, "ERROR: vector? illegal operand count: ", 38);
		objDump (rexpr, stdout);
		return;
	}
	rexpr = cadr(rexpr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);

	opcodeStart = memStackLength(rasmstack);
	asmAsm (
		BRTI0, TNULLVEC, ADDR, "yes",
		BRTI0, TVECTOR,  ADDR, "yes",
		MVI0, false,
		BRA, ADDR, "done",
		LABEL, "yes", MVI0, true,
		LABEL, "done", 
		END
	);
	asmCompileAsmstack(opcodeStart);

	DBEND();
}

void compStringP (Num flags) {
 Num opcodeStart;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		write (1, "ERROR: string? illegal operand count: ", 38);
		objDump (rexpr, stdout);
		return;
	}
	rexpr = cadr(rexpr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);

	opcodeStart = memStackLength(rasmstack);
	asmAsm (
		BRTI0, TSTRING, ADDR, "yes",
		BRTI0, TNULLSTR, ADDR, "yes",
		MVI0, false,
		BRA, ADDR, "done",
		LABEL, "yes", MVI0, true,
		LABEL, "done", 
		END
	);
	asmCompileAsmstack(opcodeStart);

	DBEND();
}

void compIntegerP (Num flags) {
 Num opcodeStart;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		write (1, "ERROR: integer? illegal operand count: ", 38);
		objDump (rexpr, stdout);
		return;
	}
	rexpr = cadr(rexpr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);

	opcodeStart = memStackLength(rasmstack);
	asmAsm (
		BRTI0, TINTEGER, ADDR, "yes",
		MVI0, false,
		BRA, ADDR, "done",
	LABEL, "yes",
		MVI0, true,
	LABEL, "done", 
		END
	);
	asmCompileAsmstack(opcodeStart);

	DBEND();
}

void compSymbolP (Num flags) {
 Num opcodeStart;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: symbol? illegal operand count:");
		objDump (rexpr, stderr);
		return;
	}
	rexpr = cadr(rexpr); /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);

	opcodeStart = memStackLength(rasmstack);
	asmAsm (
		BRTI0, TSYMBOL, ADDR, "yes",
		MVI0, false,
		BRA, ADDR, "done",
		LABEL, "yes", MVI0, true,
		LABEL, "done", 
		END
	);
	asmCompileAsmstack(opcodeStart);

	DBEND();
}

void compPortP (Num flags) {
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		write (1, "ERROR: vector? illegal operand count: ", 38);
		objDump (rexpr, stdout);
		return;
	}
	rexpr = cadr(rexpr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);
	asmAsm(
		BRTI0, TPORT, 4*8l,
		MVI0, false,
		BRA, 2*8l,
		MVI0, true,
		END
	);
	DBEND();
}


void compEOFObjectP (Num flags) {
 Num opcodeStart;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		printf ("ERROR: eof-object? illegal operand count: ");
		objDump (rexpr, stdout);
		return;
	}
	rexpr = cadr(rexpr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);
	opcodeStart = memStackLength(rasmstack);
	asmAsm (
		BRTI0, TEOF, ADDR, "iseof",
		MVI0, false,
		BRA, ADDR, "end",
		LABEL, "iseof",
		MVI0, true,
		LABEL, "end",
		END
	);
	asmCompileAsmstack(opcodeStart);
	DBEND();
}

void compBegin (Num flags) {
	DBBEG();
	rexpr = cdr(rexpr); /* Skip symbol 'begin. */

	if (rexpr == null) {
		asmAsm(
			MVI0, null,
			END
		);
	} else {
		/* Compile non-tail expression. */
		while (cdr(rexpr) != null) {
			DB("   compBegin begin block non-tail expression.");
			vmPush(cdr(rexpr)); /* Push reset of expression. */
			rexpr = car(rexpr);
			compExpression(flags & ~TAILCALL);
			rexpr = vmPop();
		}
		/* Compile the tail expression. */
		DB("   compBegin begin block tail expression.");
		rexpr = car(rexpr);
		compExpression(flags);
	}
	DBEND();
}

void compQuote (void) {
	DBBEG();
	asmAsm (
		MVI0, cadr(rexpr),
		END
	);
	DBEND();
}


void compAsmTailCall () {
	/* Keep track of this opcode position for the compiling of the
	   labels and branches. */
 Num opcodeStart = memStackLength(rasmstack);
	DBBEG();
	asmAsm (
		BRTI0,  TSYSCALL, ADDR, "syscall",
		BRTI0,  TCLOSURE, ADDR, "code",
		/* Illegal operator section.  For now just dump the arguments.
		*/
		SYSI, compIllegalOperator,
		RET, /* Since tail call, return. */
		/*  Reference the syscall address then make the system call.
		*/
		LABEL, "syscall",
		LDI00, 0l,
		SYS0,
		RET, /* Since a tail call, return. */
		/* Closure operator section.  Load jump address into r2.  R1 is
		   argument count and r0 is the closure (which is needed as it
		   holds the lexical environment).
		*/
		LABEL, "code",
		LDI20, 0l,
		J2,
		END
	);
	asmCompileAsmstack(opcodeStart);
	DBEND();
}

void compAsmNonTailCall () {
	/* Keep track of this opcode position for the compiling of the
	   labels and branches. */
 Num opcodeStart = memStackLength(rasmstack);
	DBBEG();
	asmAsm (
		BRTI0,  TSYSCALL, ADDR, "syscall",
		BRTI0,  TCLOSURE, ADDR, "closure",
		/* Illegal operator section.  For now just dump the arguments. */
		SYSI, compIllegalOperator,
		BRA,  ADDR, "end",
		/* Syscall operator section.  Reference the syscall address, set the
	   	operand count then make the system call.  */
	LABEL, "syscall",
		LDI00, 0l,
		SYS0,
		BRA,  ADDR, "end",
		/* Closure operator section.
		*/
	LABEL, "closure",
		LDI20, 0l, /* load r2 with code and jump. */
		JAL2,
		/* End of block.
		 */
	LABEL, "end",
		POP19, /* Restores previous environment, ip and code registers. */
		POP1B,
		POP1A,
		END
	);
	asmCompileAsmstack(opcodeStart);
	DBEND();
}


/* Compiles expressions of the form (if test consequent alternate).
*/
void compIf (Num flags) {
 Num falseBraAddr, trueContAddr;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'if symbol. */
	vmPush (cddr(rexpr)); /* Push alternate expressions list.  Will be NULL or a list containing the alternate expression. */
	vmPush (cadr(rexpr));  /* Push consequent expressions. */

	/* Compile 'test' expression. */
	DB("compiling test");
	rexpr = car(rexpr);
	compExpression(flags & ~TAILCALL);

	/* The "branch on type" opcode.  Its immediate branch address field
	   is kept track of and will be set with the proper offset below.  */
	asm(BRTI0); asm(TFALSE); asm(0l);
	falseBraAddr = memStackLength(rasmstack);

	DB("compiling consequent");
	rexpr = vmPop(); /* Compile consequent. */
	compExpression(flags);

	/* The "branch after true block" opcode.  Its immediate branch address field
	   is kept track of and will be set with the proper offset below.  */
	asm(BRA); asm(0l);
	trueContAddr = memStackLength(rasmstack);

	/* Fill in the "branch on false" field. */
	DB("setting branch on false:%03x brt TFALSE %02x", falseBraAddr, (8*(trueContAddr-falseBraAddr)));
	memVectorSet(rasmstack, falseBraAddr, (Obj)(8*(trueContAddr-falseBraAddr)));

	/* Compile alternate.  Might not be specified in expression so just return (). */
	DB("compiling alternate");
	rexpr = vmPop();
	if (rexpr == null) {
		asm(MVI0); asm(null);
	}
	else {
		rexpr = car(rexpr); /* Consider alternate expression. */
		compExpression(flags);
	}

	/* Fill in the "branch after true block" field. */
	DBBEG("setting branch after true:%03x bra %02x", trueContAddr, (8*(memStackLength(rasmstack)-trueContAddr)));
	memVectorSet(rasmstack, trueContAddr,
	                (Obj)(8 * (memStackLength(rasmstack) - trueContAddr) ));
	DBEND();
}

/* Compiles expression of the form (if testExpr (consequentExpr {value of testExpr}) alternateExpr)
*/
void compAIf (Num flags) {
 Num falseBraAddr, trueContAddr;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'aif symbol. */
	vmPush (cddr(rexpr)); /* Push alternate expressions list.  Will be NULL or a list containing the alternate expression. */
	vmPush (cadr(rexpr));  /* Push consequent expressions. */

	/* Compile 'test' expression. */
	DB("compiling test");
	rexpr = car(rexpr);
	compExpression(flags & ~TAILCALL);

	/* The "branch on type" opcode.  Its immediate branch address field
	   is kept track of and will be set with the proper offset below.  */
	asm(BRTI0); asm(TFALSE); asm(0l);
	falseBraAddr = memStackLength(rasmstack);

	/* Save execution state, possibly, since the following is the equivalent
	   of compCombination */
	if (!((Num)flags & TAILCALL)) {
		asmAsm (
			PUSH1A,
			PUSH1B,
			PUSH19,
			END
		);
	}

	DB("compiling consequent");
	asm(PUSH0); /* Push result of test expression on the stack.  Becomes argument to consequent. */
	rexpr = vmPop(); /* Compile consequent. */
	compExpression(flags & ~TAILCALL);

	asm(MVI1); asm(1l); /* Set the argument count to 1.  Argument already on the stack. */
	
	if ((Num)flags & TAILCALL) compAsmTailCall();
	else compAsmNonTailCall();

	asm(BRA); asm(0l);
	trueContAddr = memStackLength(rasmstack);

	/* Fill in the "branch on false" field. */
	DB("setting branch on false:%03x brt TFALSE %02x", falseBraAddr, (8*(trueContAddr-falseBraAddr)));
	memVectorSet(rasmstack, falseBraAddr, (Obj)(8*(trueContAddr-falseBraAddr)));

	/* Compile alternate.  Might not be specified in expression so just return #f. */
	DB("compiling alternate");
	rexpr = vmPop();
	if (objIsPair(rexpr)) {
		 /* Compile alternate expression. */
		rexpr = car(rexpr);
		compExpression(flags);
	} else {
		/* reg 0 already #f from test condition */
	}

	/* Fill in the "branch after true block" field. */
	DB("setting branch after true:%03x bra %02x", trueContAddr, (8*(memStackLength(rasmstack)-trueContAddr)));
	memVectorSet(rasmstack, trueContAddr,
	                (Obj)(8 * (memStackLength(rasmstack) - trueContAddr) ));
	DBEND();
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
	rexpr = cdr(rexpr); /* Skip symbol 'cond */

	/* Push clauses, checking for non-lists and verifying the else clause is last */
	while (objIsPair(rexpr)) { /* Over all clauses  expr = (<clause> ....) */
		r5 = car(rexpr); /* Consider next clause  r5 = <clause>  */
		if (!objIsPair(r5)) { /* Error if clause is not a list */
			fprintf (stderr, "\nWARNING: compCond: Malformed cond clause ");
			objDump(r5, stderr);
		} else {
			DB("Pushing clause");
			DBE objDump(r5, stderr);
			clauses++;
			vmPush(r5);
			rexpr = cdr(rexpr); /* Consider next clause for this loop */
			if (selse == car(r5)) {
				/* Else clause matched, so stop pushing clauses and give warning if more clauses follow */
				if (rexpr != null) {
					fprintf (stderr, "\nWARNING: compCond: cond's else clause followed by more clauses ");
					objDump(rexpr, stderr);
				}
				rexpr = null;
			}
		}
	}

	/* Pop clauses building the if/or/begin tree bottom-up into r0 */
	DB (" Creating nested if/or/begin expression");
	r0 = null;
	while (clauses--) {
		r5 = vmPop(); /* Consider clause r5 = <clause> = (r4 . r3) */
		r4 = car(r5); /* First expr */
		r3 = cdr(r5) ; /* Rest expr */
		if (selse == r4) {
			assert(null == r0); /* This better be the first clause popped or not at all */
			r1=sbegin; r2=r3; objCons12();          /* (begin <expr> ...) */
		} else if (!objIsPair(r3)) {
			r1=r0;  r2=null; objCons12();           /* (translated) */
			r1=r4;  r2=r0; objCons12();             /* (<test> (translated)) */
			r1=sor; r2=r0; objCons12();             /* (or <test> (translated)) */
		} else if (saif == car(r3)) {
			r3 = cdr(r3); /* Consider (r4 => . r3 */
			r1=r0;  r2=null; objCons12();           /* (translated) */
			if (objIsPair(cdr(r3))) { /* Give warning if else clause followed by more clauses */
				fprintf (stderr, "\nWARNING: compCond: cond's => expr not a single expression ");
				objDump(r5, stderr);
			}
			r1=car(r3); r2=r0; objCons12();         /* (<expr> translated) */
			r1=r4;  r2=r0; objCons12();             /* (<test> <expr> translated) */
			r1=saif; r2=r0; objCons12();            /* (if <test> <expr> translated) */
		} else {
			r1=r0;  r2=null; objCons12(); vmPush(r0); /* (translated) */
			r1=sbegin; r2=r3; objCons12();          /* (begin <expr> ...) */
			r1=r0; r2=vmPop(); objCons12();           /* ((begin <expr> ...) translated) */
			r1=r4;  r2=r0; objCons12();             /* (<test> (begin <expr> ...) translated) */
			r1=sif; r2=r0; objCons12();             /* (if <test> (begin <expr> ...) translated) */
		}
	}
	DB ("compCond translated ");
	DBE objDump(r0, stdout);
	rexpr = r0;
	compExpression(flags);
	DBEND();
}

/* Compiles expressions of the form (or exp ...) into:
		exp
		branch if not false to end
*/
void compOr (Num flags) {
 Num opcodeStart;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'or. */

	/* Empty or expression returns #f. */
	if (null == rexpr) {
		asm (MVI0); asm(false);
	} else {
		opcodeStart = memStackLength(rasmstack);
		while (objIsPair(rexpr)) {
			vmPush (cdr(rexpr)); /* Push rest. */
			/* Is this the last expression?  If so it's tail optimized. */
			if (!objIsPair(cdr(rexpr))) {
				rexpr = car(rexpr); /* Consider next expression. */
				compExpression(flags);
			} else {
				rexpr = car(rexpr); /* Consider next expression. */
				compExpression(flags & ~TAILCALL);
				asmAsm (
					BNTI0, TFALSE, ADDR, "end",
					END
				);
			}
			rexpr = vmPop();
		}
		asm (LABEL); asm ("end");
		asmCompileAsmstack(opcodeStart);
	}
	DBEND();
}

/* Compiles expressions of the form (and exp ...) into:
		exp
		branch if false to end
*/
void compAnd (Num flags) {
 Num opcodeStart;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'and. */

	/* Empty or expression returns #t. */
	if (null == rexpr) {
		asm (MVI0); asm(true);
	} else {
		opcodeStart = memStackLength(rasmstack);
		while (objIsPair(rexpr)) {
			vmPush (cdr(rexpr)); /* Push rest. */
			/* Is this the last expression?  If so it's tail optimized. */
			if (!objIsPair(cdr(rexpr))) {
				rexpr = car(rexpr); /* Consider next expression. */
				compExpression(flags);
			} else {
				rexpr = car(rexpr); /* Consider next expression. */
				compExpression(flags & ~TAILCALL);
				asmAsm (
					BRTI0, TFALSE, ADDR, "end",
					END
				);
			}
			rexpr = vmPop();
		}
		asm (LABEL); asm ("end");
		asmCompileAsmstack(opcodeStart);
	}
	DBEND();
}

void compThread (void) {
	DBBEG();

	vmPush(rasmstack); /* Save code stack. */

	/* Create new emit-code stack. */
	r0=memNewStack();
	rasmstack=r0;

	/* Compile parameter passed to thread emitting the unthread syscall
	   as the last opcode. */
	compBegin(0);
	asm(SYSI); asm(osUnthread);
	asmNewCode();

	rasmstack=vmPop(); /* Restore code stack. */

	asm(MVI0); asm(r0);
	asm(SYSI); asm(osNewThread);

	DBEND("  => ");
	DBE objDump(r0, stderr);
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
	r2=null;
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
	r2=null;
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
	DBE objDump(rexpr, stdout);
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
	r2=null;
	for (i=0; i<bindingLen; i++) {
		r1=vmPop();
		objCons12();
		r2=r0;
	}
	r1=r3;  objCons12();
	r1=r0;  r2=null;  objCons12();
	vmPush(r0);

	/* Create (set! name (lambda (var...) body)). */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		vmPush(caar(r6));
		r6=cdr(r6);
	}
	r2=null;
	for (i=0; i<bindingLen; i++) {
		r1=vmPop();
		objCons12();
		r2=r0;
	}
	r1=r2;     r2=r5;  objCons12();
	r1=slambda;r2=r0;  objCons12();
	r1=r0;     r2=null;objCons12();
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
	r1=vmPop();  r2=null;  objCons12();

	/* Return transformed expression. */
	rexpr=r0;

	DBEND("  =>  ");
	DBE objDump(rexpr, stdout);
}

void compLet (Num flags) {
	DBBEG();
	rexpr=cdr(rexpr); /* Skip 'let. */

	/* Transform named-let form (let symbol ...). */
	if (memObjectType(car(rexpr)) == TSYMBOL)
		compTransformNamedLet();
	/* Transform let form (let (...) ...). */
	else
		compTransformLet();

	/* Now compile the transformed form. */
	compExpression(flags);

	DBEND();
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
		objDump(rexpr, stderr);
	}

	/* Push and count letrec binding expressions. */
	for (r3=car(rexpr), len=0;  r3!=null; r3=cdr(r3), len++) vmPush(car(r3));

	/* Create (()) in r4. */
	r1=null;  r2=null;  objCons12();
	r4=r0;
	/* Create ((x ())...) in r3 from bindings on stack so start it with null. */
	r3=null;
	while(len--) {
		r1=car(vmPop());  r2=r4;  objCons12(); /* Form (x ()). */
		r1=r0;          r2=r3;  objCons12(); /* Form ((x ()) ...). */
		r3=r0;
	}
	vmPush(r3); /* Save transformed bindings to stack. */

	/* Push and count letrec binding expressions (again). */
	for (r3=car(rexpr), len=0;  r3!=null; r3=cdr(r3), len++) vmPush(car(r3));
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
	DBE objDump(r0, stdout);
}

void compLetrec (Num flags) {
	DBBEG();
	compTransformLetrec();
	rexpr = r0;
	compExpression(flags);
	DBEND();
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
			r1=r0;     r2=null;  objCons12(); /* => (b') */
			r1=vmPop();  r2=r0;    objCons12(); /* => (template b') */
			r1=sappend;  r2=r0;    objCons12(); /* => (append template b') */
		} else { /* Transform (a . b) => (cons a' b') */
			vmPush(cdr(rexpr)); /* Save b */
			rexpr=car(rexpr);  /* Consider a */
			compTransformQuasiquote(depth); /* => a' */
			rexpr=vmPop();      /* Restore b */
			vmPush(r0);        /* Save a' */
			compTransformQuasiquote(depth - isUnquote + isQuasiquote); /* => b' */
			r1=r0;     r2=null;  objCons12(); /* => (b') */
			r1=vmPop();  r2=r0;    objCons12(); /* => (a' b') */
			r1=scons;  r2=r0;    objCons12(); /* => (cons a' b') */
		}
	/* Transform atom into (quote atom) */
	} else {
		r1=rexpr;   r2=null;  objCons12(); // atom   => (atom)
		r1=squote; r2=r0;    objCons12(); // (atom) => (quote atom)
	}
	DBEND();
}

void compQuasiquote (Num flags) {
	DBBEG();
	rexpr = cadr(rexpr); // Given (quasiquote <qq template>) pass <qq template>
	compTransformQuasiquote(0);
	rexpr = r0;
	DB("quasiquote transformation => ");
	DBE objDump (rexpr, stderr);
	compExpression(flags);
	DBEND();
}

/* Test code that for now emits a code block that peforms a compiled tree    
   structure walk.  It'll print the compiled tree nodes along with the 
   corresponding passed tree nodes.  Not sure where I was going with this.
   IE ((syntax-rules (x y z)) '(1 (2 3) 4)) => x1y(2 3)z4
*/
void compSyntaxRulesHelper (void) {
	DBBEG();
	if (objIsPair(rexpr)) {
		/* R2 contains the expression to be transformed.  */
		DB("   Considering:");
		DBE objDump(rexpr, stderr);
		vmPush(cdr(rexpr));
		asm(LDI02); asm(1l);
		asm(PUSH0);
		rexpr = car(rexpr);
		asm(LDI22); asm(0l);
		compSyntaxRulesHelper();
		rexpr=vmPop();
		asm(POP2);
		compSyntaxRulesHelper();
	} else if (rexpr == null) {
	} else {
		asm(MVI0); asm(rexpr);
		asm(PUSH0);
		asm(MVI1); asm(1l);
		asm(SYSI); asm(compWrite);
		asm(PUSH2);
		asm(MVI1); asm(1l);
		asm(SYSI); asm(compWrite);
	}
	DBEND();
}

/* Experimenal code.  Not sure if is very useful now.
*/ 
void compSyntaxRules (void) {
	DBBEG();
	DBE objDump (rexpr, stderr);
	rexpr = cadr(rexpr);
	/* Create new code block. */
	vmPush(rasmstack);
	r0=memNewStack();
	rasmstack=r0;
	asm(POP2);
	compSyntaxRulesHelper();
	asm(RET);
	asmNewCode(); /* Transfer code stack to fixed size code vector. */
	DBE objDump(r0, stdout);
	/* Restore code block. */
	rasmstack=vmPop();
	asm(MVI1); asm(r0); /* Load r1 with code. */
	asm(SYSI); asm(sysNewClosure1Env); /* Create closure from r1 and env (r1c) */
	DBEND();
}

void compNot (Num flags) {
 Num opcodeStart;
	DBBEG();
	rexpr = cadr(rexpr);           /* Compile this expression */
	compExpression(flags & ~TAILCALL);
	opcodeStart = memStackLength(rasmstack);
	asmAsm (
		BRTI0, TFALSE, ADDR, "false",
		MVI0, false,
		BRA, ADDR, "done",
		LABEL, "false", MVI0, true,
		LABEL, "done",
		END
	);
	asmCompileAsmstack(opcodeStart);
	DBEND();
}

/* R1A/asmstack is the stack opcodes ae pushed onto.
  R18/expr is the expression list who's element values needs to be added together.
*/
void compAdd (Num flags) {
 Int sum=0;
	DBBEG();
	rexpr=cdr(rexpr); /* Skip '+. */
	vmPush(rexpr); /* Save parameter list. */
	/* Constant folding:  Scan parameter list for constants and asm a single
	   opcode that stores their sum. */
	while (objIsPair(rexpr)) {
		if (TINTEGER == memObjectType(car(rexpr)))
			sum+=*(Int*)car(rexpr);
		rexpr=cdr(rexpr);
	}
DB("   compAdd constant folding:%d", sum);
	objNewInt(sum);
	asmAsm (
		MVI1, r0,
		SYSI, objCopyInteger, /* A copy because atom is mutated. */
		MV10,
		END
	);
	rexpr=vmPop(); /* Restore parameter list. */
	/* Scan parameter list for non-constant expressions to compile. */
	while (objIsPair(rexpr)) {
		if (TINTEGER != memObjectType(car(rexpr))) {
			asm(PUSH1); /* Save accumulating sum. */
			vmPush(rexpr);                /* Save parameter list */
			rexpr = car(rexpr);           /* Compile this expression */
			compExpression(flags & ~TAILCALL);
			rexpr = vmPop();              /* Restore parameter list */
			asm(POP1); /* Restore accumulating sum. */
			asm(ADD10); /* Add result of last expression to sum. */
		}
		rexpr = cdr(rexpr);
	}
	asm(MV01);
	DBEND();
}


void compCombination (Num flags) {
 Int operandCount=0;
	DBBEG();
	DBE memDebugDumpObject(rexpr, stdout);

	/* Make sure we push/pop the jump and linked code/ip registers at the start
	   and just before the last expression.  This must be done before arguments
	   are pushed onto the stack.  Bummer.
	BF This might actually not work.  Branches to deleted opcodes are occuring.
	if (!((unsigned)flags & TAILCALL)) {
		if (memStackObject(rasmstack, 0) == POP1A) {
			memStackPop(rasmstack); memStackPop(rasmstack); memStackPop(rasmstack);
		} else {
			asm(PUSH1A); asm(PUSH1B); asm(PUSH4);
		}
	}
	*/
	if (!((Num)flags & TAILCALL)) {
		asmAsm (
			PUSH1A,
			PUSH1B,
			PUSH19,
			END
		);
	}

	vmPush(car(rexpr)); /* Save operator parameter. */

	/* Compile operand expressions. */
	rexpr = cdr(rexpr);
	while (objIsPair(rexpr)) {
		operandCount++;
		vmPush(cdr(rexpr));
		rexpr = car(rexpr);
		compExpression(flags & ~TAILCALL);
		if (CompError) goto ret;
		asm(PUSH0);
		rexpr = vmPop();
	}

	/* Restore and compile operator expression. */
	rexpr=vmPop();
	compExpression(flags & ~TAILCALL);
	if (CompError) goto ret;

	/* Need to asm code that handles operators of type syscall, closure,
	   continuation and the like.  For now it just assumes a syscall.  Perhaps
	   a special syscall that handles this all in C for now?
	   Emit code to check the object type and either SYS the TSYSCALL type
	   or JAL the TCODE type.  */
	/* Emit code to that applys args to function/code tail optimized or not. */
	asm (MVI1);  asm(operandCount);
	if ((unsigned)flags & TAILCALL) compAsmTailCall();
	else compAsmNonTailCall();
	
ret:
	DBEND();
}



/* Compile the form (apply fn argument-list).  This should be similar to
   a combination expression. */
void compApply (Num flags) {
 Num opcodeStart, operandCount=0;
	DBBEG();

	rexpr = cdr(rexpr); /* Skip over 'apply symbol */

	/* Is this a tail call?  if not save state. */
	if (!((Num)flags & TAILCALL)) {
		asmAsm (
			PUSH1A,
			PUSH1B,
			PUSH19,
			END
		);
	}

	vmPush(car(rexpr)); /* Save operator parameter. */

	/* Compile operand expressions the last of which hopefully evaluates to a list of args.
	   The resulting arguments will be pushed onto the stack and passed to the function.  */
	rexpr = cdr(rexpr);
	while (objIsPair(rexpr)) {
		vmPush (cdr(rexpr)); /* Push rest */
		rexpr = car(rexpr); /* Consider expression  */
		compExpression(flags & ~TAILCALL);
		asm(PUSH0);
		operandCount++;
		rexpr = vmPop();
	}

	/* Restore and compile operator expression. */
	rexpr=vmPop();
	compExpression(flags & ~TAILCALL);
	asm(MV30); /* Save operator in r3 */

	/* At this point stack has the arguments, the argument-list and r3 has function.
	   Want to transfers the argument-list items from list to the stack with r1 ending up
	   with the argument count.  Initially the argument count is the number of initial
	   non-list arguments to apply.
	*/
	opcodeStart = memStackLength(rasmstack);
	asmAsm (
		MVI1, operandCount-1, /* Initialize operand count in r1 to number of initial arguments to apply. */
		POP0,    /* Pop argument-list. */
		LABEL, "argcount",
		BRTI0, TNULL, ADDR, "argcountdone",
		ADDI1, 1l, /* Inc argument count in r1. */
		LDI20, 0l, /* Push the car. */
		PUSH2,
		LDI00, 1l, /* Consider cdr. */
		BRA, ADDR, "argcount",
		LABEL, "argcountdone",
		MV03,     /* Operator back to r0 */
		END
	);
	asmCompileAsmstack(opcodeStart);

	/* Need to asm code that handles operators of type syscall, closure,
	   continuation and the like.  For now it just assumes a syscall.  Perhaps
	   a special syscall that handles this all in C for now?
	   Emit code to check the object type and either SYS the TSYSCALL type
	   or JAL the TCODE type.  */
	/* Emit code to that applys args to function/code tail optimized or not. */

	if ((unsigned)flags & TAILCALL) compAsmTailCall();
	else compAsmNonTailCall();

	DBEND();
}

/* Stored stack expected in r1.
*/
void compReinstateContinuation (void) {
 Num length;

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
}

void compCreateContinuation (void) {
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
	r1=r0;
	/* r1 now has a copy of the stack */

	/* Create a function that will reinstate this stack at runtime. */
	vmPush(rasmstack);
	r0=memNewStack();
	rasmstack=r0;
	asmAsm(
		MVI3, r1,  /* Stored copy of stack in r3. */
		SYSI, compReinstateContinuation,
		END
	);
	asmNewCode();  r1=r0;
	sysNewClosure1Env();
	memVectorSet(r0, 1, rtge); /* Set to TGE just in case. */
	rasmstack = vmPop();

	/* Skip the "continuation" jump in the code just after this syscall
	   in the compiled code.  See compCallcc() */
	rip += 2;
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
void compCallcc (Num flags) {
 Num opcodeStart;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip over 'call/cc symbol in (call/cc fn)*/

	opcodeStart = memStackLength(rasmstack);
	asmAsm(
		SYSI, compCreateContinuation,
		BRA, ADDR, "continuationcall",
		END
	);

	/* Is this a tail call?  if not save state. */
	if (!((Num)flags & TAILCALL))
		asmAsm (
			PUSH1A,
			PUSH1B,
			PUSH19,
			END
		);

	asm(PUSH0); /* Push the continuation just create. */

	rexpr = car(rexpr); /* Consider and compile fn. */
	compExpression(flags & ~TAILCALL);

	asm (MVI1);  asm(1);
	if ((unsigned)flags & TAILCALL) compAsmTailCall();
	else compAsmNonTailCall();

	asmAsm(LABEL, "continuationcall", END);
	asmCompileAsmstack(opcodeStart);

	DBEND();
}



/* Compile expression.
   expr (r18) -> Expression to compile.
   asmstack (r1a) -> Stack the emitted opcodes are pushed onto.
   env (rc6) -> Pseudo environment
	An expression is either a symbol, syntax, combination or self evaluating.
*/
Num compExpression (Num flags) {
	DBBEG();
	DBE memDebugDumpObject(rexpr, stdout);
	switch (memObjectType(rexpr)) {
		case TSYMBOL :
			compVariableReference(flags);
			break;
		case TPAIR :
			if      (srem       == car(rexpr)) compRem(flags);
			else if (sdefine    == car(rexpr)) compDefine(flags);
			else if (ssetb      == car(rexpr)) compSetb(flags);
			else if (slambda    == car(rexpr)) compLambda(flags);
			else if (smacro     == car(rexpr)) compMacro(flags);
			else if (snot       == car(rexpr)) compNot(flags);
			else if (sadd       == car(rexpr)) compAdd(flags);
			else if (svectorref == car(rexpr)) compVectorRef(flags);
			else if (svectorvectorref == car(rexpr)) compVectorVectorRef(flags);
			else if (svectorsetb== car(rexpr)) compVectorSetb(flags);
			else if (svectorvectorsetb== car(rexpr)) compVectorVectorSetb(flags);
			else if (scons      == car(rexpr)) compCons(flags);
			else if (scar       == car(rexpr)) compCar(flags);
			else if (scdr       == car(rexpr)) compCdr(flags);
			else if (ssetcarb   == car(rexpr)) compSetCarB(flags);
			else if (ssetcdrb   == car(rexpr)) compSetCdrB(flags);
			else if (sprocedurep== car(rexpr)) compProcedureP(flags);
			else if (snullp     == car(rexpr)) compNullP(flags);
			else if (spairp     == car(rexpr)) compPairP(flags);
			else if (svectorp   == car(rexpr)) compVectorP(flags);
			else if (sstringp   == car(rexpr)) compStringP(flags);
			else if (sintegerp  == car(rexpr)) compIntegerP(flags);
			else if (ssymbolp   == car(rexpr)) compSymbolP(flags);
			else if (sportp     == car(rexpr)) compPortP(flags);
			else if (seofobjectp== car(rexpr)) compEOFObjectP(flags);
			else if (sbegin     == car(rexpr)) compBegin(flags);
			else if (squote     == car(rexpr)) compQuote();
			else if (sif        == car(rexpr)) compIf(flags);
			else if (saif       == car(rexpr)) compAIf(flags);
			else if (scond      == car(rexpr)) compCond(flags);
			else if (sor        == car(rexpr)) compOr(flags);
			else if (sand       == car(rexpr)) compAnd(flags);
			else if (sthread    == car(rexpr)) compThread();
			else if (slet       == car(rexpr)) compLet(flags);
			else if (sletrec    == car(rexpr)) compLetrec(flags);
			else if (squasiquote== car(rexpr)) compQuasiquote(flags);
			else if (ssyntaxrules== car(rexpr)) compSyntaxRules();
			else if (seval      == car(rexpr)) compEval(flags);
			else if (sapply     == car(rexpr)) compApply(flags);
			else if (scallcc    == car(rexpr)) compCallcc(flags);
			else compCombination(flags);
			break;
		default:
			compSelfEvaluating();
			break;
	}
	DBEND();
	return CompError;
}


/* Compile expression.
   r18 -> Expression we're compiling.
   r0  <- Resuling code object (vector of VM opcodes).
*/
Num compCompile (void) {
 Num ret;
	DBBEG();

	CompError = 0; /* Clear error flag. */
	//renv = rtge;   /* Force evaluation in the global environment */

   /* Start emitting code.  Keep track of original expression for debugging. */
	asmAsm (
		BRA, 8l,
		rexpr,
		END
	);

	ret = compExpression(0);  /* No compiler flags */

   /* Emit the QUIT opcode which exits the VM. */
	asm(QUIT);
	asmNewCode();

	DBEND();
	return ret;
}

void compInitialize (void) {
 static Num shouldInitialize=1;
	DBBEG();
	if (shouldInitialize) {
		DB("  Activating module...");
		shouldInitialize=0;
		sysInitialize ();
		asmInitialize ();
		memObjStringSet(compIllegalOperator);
		memObjStringSet(compSysCompile);
		memObjStringSet(compTGELookup);
		memObjStringSet(compTGEMutate);
		memObjStringSet(compVerifyVectorRef);
		memObjStringSet(compVerifyVectorSetB);
		memObjStringSet(compReinstateContinuation);
		memObjStringSet(compCreateContinuation);
		memObjStringSet("Not enough arguments to closure");
		memObjStringSet("Too many arguments to function");
	} else {
		DB("  Module already activated");
	}
	DBEND();
}

#undef DB_DESC
#undef DEBUG
