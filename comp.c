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



/*
TABLE OF CONTENTS
 Compiler
 Init

TERMS
  I-Graph   Intermediate graph composed of I-blocks
  I-Block   I-graph node composed of a list of incoming iblocks, outgoing iblocks and icode statements
  I-Code    I-block statement composed of multiple code fields

DESIGN
   Expression to compile assigned to rexpr/rf
   Flow keeps track of pseudo environment in renv/r1c and used registers in flags
*/

void ccCompileExpr (Num flags);
void ccInitialize (void);

/* compiler flags
*/
static const Num CCTAILCALL  = (Num)0x00010000;
static const Num CCNODEFINES = (Num)0x00020000;



/*******************************************************************************
 Compiler

 Compile a scheme expression into a VM code block.  Calls ASM and Assemble
 functions in this module.
*******************************************************************************/

void ccError (void) {
	fprintf(stderr, "ccError: "STR, r0);
	while (r1--) {
		fprintf (stderr, " ");
		sysDisplay(vmPop(), stderr);
		*(int*)0=0;
	}
	exit(-1);
}


/* Run time symbol lookup syscall.  If a symbol in r1 found in TGE mutate code
   to just reference the binding's value rather than make this syscall.
*/
void ccTGELookup (void) {
	DBBEG();
	sysTGEFind();
	if (r0 == null) {
		printf ("ERROR: Unbound symbol:");
		objDump(r1, stdout);
		r0 = r1;
		/* TODO  Kill thread, stop machine, return to monitor/shell? */
	} else {
		DB("SYS    found in tge @ opcode %x", (Num)rip-4);
		/* Specialization optimization.  Muate code that originally called
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
void ccTGEMutate (void) {
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
		memVectorSet(rcode, (Num)rip-4, vmMVI1);  memVectorSet(rcode, (Num)rip-3, r0);
		memVectorSet(rcode, (Num)rip-2, vmSTI01); memVectorSet(rcode, (Num)rip-1, 0);
		r0 = r2; /* Restore value we're trying to set!. */
		/* Force virtual machine to run this code. */
		rip -= 4;
	}
	DBEND();
}

/* Generate assembly which looks up value of symbol in a local or
   global environment and assigns to r0
*/
void ccSymbol (Num flags) {
 Num d, ret, depth, offset;
	DBBEG();
	DBE objDump(rexpr, stderr);

	/* Scan local environments.  Returned is a 16 bit number, the high 8 bits
	   is the environment chain depth, the low 8 bits the binding offset. The
	   offset will be 2 or greater if a variable is found in any environment
	   excluding the global environment. */
	r1 = rexpr;
	ret = sysEnvFind();

	if (ret) {
		depth = ret >> 8;
		offset = ret & 0xff;
		DB("   found in a local environment depth:"NUM" offset:"NUM, depth, offset);
		/* Emit code that traverses the environment chain and references the proper binding. */
		if (depth == 0) {
			asmAsm(LDI, R0, R1C, (Obj)offset);
		} else {
			asmAsm(LDI, R0, R1C, 0l); /* Parent env */
			for (d=1; d < depth; d++) asmAsm(LDI, R0, R0, 0l); /* It's parent env */
			asmAsm(LDI, R0, R0, (Obj)offset); /* Local symbol offset */
		}
	} else {
		/* Scan tge... */
		sysTGEFind();
		if (null == r0) {
			DB("   can't find in TGE...maybe at runtime");
			asmAsm(
				MVI, R1, rexpr,
				SYSI, ccTGELookup);
		} else {
			DB("   found in TGE");
			r3 = r0; /* Keep track of the symbol */
			asmAsm(
				MVI, R0, r3,
				LDI, R0, R0, 0l);
		}
	}
	DBEND();
}


void ccSetB (Num flags) {
 Num ret, d,depth, offset;
	DBBEG();
	rexpr = cdr(rexpr); /* Consider set! parameter list (S E) */
	vmPush(car(rexpr)); /* Save S */
	/* Emit code that evaluates the expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	/* Scan local environments.  Returned is a 16 bit number, the high 8 bits
	   is the environment chain depth, the low 8 bits the binding offset. The
	   offset will be 1 or greater if a variable is found in any environment
	   excluding the global environment. */
	r1 = vmPop(); /* Restore S */
	ret = sysEnvFind();

	if (ret) {
		depth = ret >> 8;
		offset = ret & 0xff;
		DB("   found in a local environment depth:"NUM" offset:"NUM, depth, offset);
		/* Emit code that traverses the environment chain and references the proper binding. */
		if (depth == 0) {
			asmAsm(STI, R0, R1C, (Obj)offset);
		} else {
			asmAsm(LDI, R1, R1C, 0l); /* Parent env */
			for (d=1; d < depth; d++) asmAsm(LDI, R1, R1, 0l); /* It's parent env */
			asmAsm(STI, R0, R1, (Obj)offset); /* Local symbol offset */
		}
	} else {
		/* Scan tge... */
		sysTGEFind();
		if (r0 == null) {
			DB("   can't find in TGE...maybe at runtime");
			asmAsm(
				MVI, R1, rexpr,
				SYSI, ccTGEMutate);
		} else {
			DB("   found in TGE");
			r3 = r0; /* Keep track of the symbol */
			asmAsm(
				MVI, R1, r3,
				STI, R0, R1, 0l);
		}
	}

	DBEND();
}

void compIf (Num flags) {
 Obj L1, L2;
	DBBEG();

	rexpr = cdr(rexpr); /* Consider if expression's parameter list (TEST CONSEQUENT ALTERNATE)*/
	assert("If expression missing TEST operand" && memIsObjectType(rexpr, TPAIR));

	/* [TEST] */
	r0 = cdr(rexpr); /* Make sure at least the CONSEQUENT operand exists */
	assert("If expression missing consequent" && memIsObjectType(r0, TPAIR));
	vmPush(r0); /* Push (CONSEQUENT ALTERNATE) */

	rexpr = car(rexpr); /* Compile TEST expression */
	ccCompileExpr(flags & ~CCTAILCALL);
	rexpr = vmPop(); /* Restore (CONSEQUENT ALTERNATE) */

	/* [TEST]---[BRANCH] */
 	L1 = asmNewLabel();
	asmAsm(BEQI, R0, false, L1);

	/* [TEST]---[BRANCH]---[CONSEQUENT] */
	vmPush(cdr(rexpr)); /* Push (ALTERNATE) */
	rexpr = car(rexpr); /* Compile CONSEQUENT expression */
	ccCompileExpr(flags);
	rexpr = vmPop(); /* Restore (ALTERNATE) */

	if (null == rexpr) {
		/* [TEST]---[BRANCH]---[CONSEQUENT]--[END] */
		asmAsm(LABEL, L1);
	} else {
 		L2 = asmNewLabel();
		/* [TEST]---[BRANCH]---[CONSEQUENT]--[JUMP]--[ALTERNATE]--[END] */
		asmAsm(BRA, L2);
		asmAsm(LABEL, L1);
		rexpr = car(rexpr); /* Consider alternate */
		ccCompileExpr(flags); /* Compile ALTERNATE expression.  It becomes the leading alternate block's default */
		asmAsm(LABEL, L2);
	}

	DBEND();
}

void ccCons (Num flags) {
	DBBEG();

	rexpr = cdr(rexpr); /* Consider cons's parameter list (A B)*/
	assert(memIsObjectType(rexpr, TPAIR)); /* TODO Ugly output Assertion memIsObjectType(r15, 0x80l) */

	vmPush(cdr(rexpr)); /* Save (B) */

	rexpr = car(rexpr); /* Consider and compile A */
	ccCompileExpr(flags & ~CCTAILCALL);

	asmAsm(
		PUSH, R0
	);

	rexpr = vmPop(); /* Restore (B) */
	assert(memIsObjectType(rexpr, TPAIR));
	assert(memIsObjectType(cdr(rexpr), TNULL));

	rexpr = car(rexpr); /* Consider and compile B */
	ccCompileExpr(flags & ~CCTAILCALL);

	asmAsm(
		POP, R1,
		SYSI, objCons10
	);

	DBEND();
}


/* Parse the form (? *) placing * into r1
   Return: 0 success  -1 error
*/
Int ccParseUnary (void) {
	r0 = cdr(rexpr); /* Consider arguments */
	if (!objIsPair(r0)) return -1; /* No arguments */
	r1 = car(r0);
	if (cdr(r0) != null) return -1; /* More than one argument */
	return 0;
}

void ccCar (Num flags) {
 Obj Lok;
	DBBEG();

	vmPush(rexpr); /* Save expression. */

	if (ccParseUnary()) {
		r0 = "Compiler error";
		r1 = (Obj)1;
		ccError();
	}

	rexpr = r1;  /* Consider and compile expression parsed */

	ccCompileExpr(flags & ~CCTAILCALL);
	rexpr = vmPop(); /* Restore expression and use in runtime error message */
	Lok = asmNewLabel();
	asmAsm(
		BRTI, R0, TPAIR, Lok,
		MVI, R0, rexpr,
		PUSH, R0,
		MVI, R1, 1l,
		MVI, R0, "runtime error",
		SYSI, ccError,
	 LABEL, Lok,
		LDI, R0, R0, 0l /* Perform car */
	);
	DBEND();
}

void ccCdr (Num flags) {
 Obj Lok;
	DBBEG();

	vmPush(rexpr); /* Save expression. */

	if (ccParseUnary()) {
		r0 = "Compiler error";
		r1 = (Obj)1;
		ccError();
	}

	rexpr = r1;  /* Consider and compile expression parsed */

	ccCompileExpr(flags & ~CCTAILCALL);
	rexpr = vmPop(); /* Restore expression and use in runtime error message */
	Lok = asmNewLabel();
	asmAsm(
		BRTI, R0, TPAIR, Lok,
		MVI, R0, rexpr,
		PUSH, R0,
		MVI, R1, 1l,
		MVI, R0, "runtime error",
		SYSI, ccError,
	 LABEL, Lok,
		LDI, R0, R0, 1l /* Perform cdr */
	);
	DBEND();
}

void ccSetCarB (Num flags) {
	DBBEG();
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
	ccCompileExpr(flags & ~CCTAILCALL);
	asmAsm(PUSH, R0);
	rexpr = vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	asmAsm(
		POP, R2,
		STI, R2, R0, 0l);
ret:
	DBEND();
}

void ccSetCdrB (Num flags) {
	DBBEG();
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
	ccCompileExpr(flags & ~CCTAILCALL);
	asmAsm(PUSH, R0);
	rexpr = vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	asmAsm(
		POP, R2,
		STI, R2, R0, 1l);
ret:
	DBEND();
}

void ccVerifyVectorRef (void) {
	if (memObjectLength(r1) <= (Num)r2) {
		vmPush(r2);
		vmPush(r1);
		r0 = "vector-ref out of bounds";
		r1 = (Obj)2;
		ccError();
	}
}

void ccVerifyVectorSetB (void) {
	if (memObjectLength(r1) <= (Num)r2) {
		vmPush(r0);
		vmPush(r2);
		vmPush(r1);
		r0 = "vector-set! out of bounds";
		r1 = (Obj)3;
		ccError();
	}
}

void ccVectorRef (Num flags) {
	DBBEG();
	vmPush(car(cddr(rexpr))); /* Save index expression. */
	rexpr = cadr(rexpr);       /* Compile Vector expression. */
	ccCompileExpr(flags & ~CCTAILCALL);
	rexpr = vmPop();            /* Compile index expression. */
	if (TINTEGER == memObjectType(rexpr)) {
		/* Load static integer value into register */
		asmAsm(
			MV, R1, R0, /* Move object to r1 */
			MVI, R2, *(Num*)rexpr,
			SYSI, ccVerifyVectorRef,
			LD, R0, R1, R2);
	} else {
		asmAsm(PUSH, R0);
		ccCompileExpr(flags & ~CCTAILCALL);
		asmAsm(
			POP, R1,
			/* Load object's integer value into register. */
			LDI, R2, R0, 0l, /* This fails runtime type check */
			SYSI, ccVerifyVectorRef,
			LD, R0, R1, R2);
	}
	DBEND();
}

void ccVectorSetB (Num flags) {
	DBBEG();
	rexpr=cdr(rexpr); /* Skip 'vector-set!. */
	vmPush(car(cddr(rexpr))); /* Save new-value expression. */
	vmPush(cadr(rexpr));      /* Save index expression. */
	/* Consider and compile Vector expression. */
	rexpr = car(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);
	asmAsm(PUSH, R0);           /* Save vector object. */
	/* Pop and compile index expression. */
	rexpr=vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	asmAsm(PUSH, R0);           /* Save offset object. */
	/* Pop and compile new-value expression. */
	rexpr=vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	asmAsm (
		POP, R2,       /* Pop offset object. */
		LDI, R2, R2, 0l,   /* Load offset object's integer value into register. */
		POP, R1,       /* Pop vector object. */
		SYSI, ccVerifyVectorSetB,
		ST, R0, R1, R2       /* Store new-value object in vector. */
	);
	DBEND();
}


/* Transform expr:((fn formals) body) into the form
   r0:(fn (lambda formals body)).  No syntic error checking is performed
   yet.  Would rather implement a macro transformation facility.
*/
void ccTransformDefineFunction (void) {
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


/* Transform expr:((define x q) (define y r) body)
       => r0:((lambda (x y) (set! x q) (set! y r) body) () ())
*/
void ccTransformInternalDefinitions(void) {
 Int definitionsCount=0;
	DBBEG();

	/* Save lambda body. */
	while (objIsPair(rexpr) && objIsPair(car(rexpr)) && sdefine == caar(rexpr)) {
		vmPush(cdr(rexpr));
		rexpr = cdar(rexpr); // Consider next expression and skip 'define.
		if (objIsPair(car(rexpr))) {
			ccTransformDefineFunction(); // Returns (fn (lambda formals body))
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


/* Given (args body) in expr (r18) create a new code block that basically
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
void ccLambdaBody (Num flags) {
 Obj Lcomment, Lexpectednoargs, LkeepLexicalEnvironment, LnotEnoughArguments, LnormalFormals, LbuildRestList;
	DBBEG();
	DBE objDump(rexpr, stdout);

	/* Since we're creating a new code object, create a new ASM context */
	asmStart();

	/* The first opcode emitted is a branch past a pointer to the original
	   expression being compiled.  This is a quick and dirty debugging aid. */
	Lcomment = asmNewLabel();
	//asmAsm(
	//	BRA, Lcomment,
	//	rexpr,
	//	LABEL, Lcomment
	//);

	/* Emit code that extends stack-stored arguments (Free variables can be
	   statically compiled?  r2 is assumed to hold the environment to be
	   extended, r1 the argument count, r3 the formal arguments.) */
	if (null == car(rexpr)) {
		/* Since a lambda with empty formals list, emit code which doesn't extend
		   the environment but instead sets env to the containing closure's env
		   or TGE if this is a top level definition (which will always be for 'lambda
		   and not 'macro) */
		if (renv == rtge) asmAsm(MV, R1C, R18); /* env = tge */
		else asmAsm(LDI, R1C, R0, 1l);

		Lexpectednoargs = asmNewLabel();
		asmAsm (
			BEQI, R1, 0, Lexpectednoargs,
			MVI, R0, rexpr, /* Error situation.  Add expression to stack */
			PUSH, R0,
			ADDI, R1, 1l,
			MVI, R0, "Too many arguments to closure",
			SYSI, ccError, /* Error correction */
			LABEL, Lexpectednoargs
		);
	} else {
		/* Emit code that extends the environment.  Pops the top most arguments
		   into a list for the 'rest' formal parameter  (lambda (a b . rest)...).
		   R3 contains the non-dotted formal parameter length (via the Normalize
		   function above). */

		/* Temporarily save lexical environment, from closure in r0, or tge, to r5.
		   The stored environment might be null in which case keep track of
		   the current/dynamic environment instead of the stored lexical.  Use
		   TGE when a top level definition.  See also the similar situation in this
		   if block's true clause with the empty formals case. */
		if (car(renv) == rtge) asmAsm(MV, R5, R18);
		else asmAsm(LDI, R5, R0, 1l);

		LkeepLexicalEnvironment = asmNewLabel();
		LnotEnoughArguments = asmNewLabel();
		LnormalFormals = asmNewLabel();
		asmAsm (
			BNEI, R5, null, LkeepLexicalEnvironment,
			MV, R5, R1C,
		 LABEL, LkeepLexicalEnvironment,
			MVI, R0, null, /* Initial formal argument 'rest' value (empty list). */
			/* r3 is non-dotted formal argument length. */
			BLTI, R1, r3, LnotEnoughArguments,
			BEQI, R1, r3, LnormalFormals
		);

		/* Emit code for functions lacking a dotted formal argument.  This code
		   will be reached if there are more values passed to the function than
		   there are formal arguments.  Otherwise it will just continue to build
		   the dotted formal list. */
		if (r4 == 0) {
			asmAsm (
				MVI, R0, rexpr, /* Add expression to stack */
				PUSH, R0,
				ADDI, R1, 1l,
				MVI, R0, "Too many arguments to function",
				SYSI, ccError /* Error correction */
			);
		}

		LbuildRestList = asmNewLabel();
		asmAsm (
		LABEL, LbuildRestList,
			MV, R3, R0,
			POP, R2,
			SYSI, objCons23,
			ADDI, R1, -1l,
			BNEI, R1, r3, LbuildRestList,
			BRA, LnormalFormals,
		LABEL, LnotEnoughArguments,
			MVI, R0, rexpr, /* Add expression to stack */
			PUSH, R0,
			ADDI, R1, 1l,
			MVI, R0, "Not enough arguments to closure",
			SYSI, ccError, /* Error correction */
			PUSH, R0,
			ADDI, R1, 1l,
			BNEI, R1, r3, LnotEnoughArguments,
		LABEL, LnormalFormals,
			PUSH, R0,
			/* Create the local environment. r1 is the length of the vector.
			   3 is added to account for the parent env, formal argument list
			   and rest formal argument. */
			ADDI, R1, 3l,
			SYSI,  objNewVector1, /* New vector in r0 of size imm:r1. */
			STI, R5, R0, 0l, /* Set parent link. */
			/* Set the environment's normalized formal argument list which was
			   created before the call to this C function. */
			MVI, R3, cdr(renv),
			STI, R3, R0, 1l
		);

		/* Emit code that pops arguments off stack and stores into proper
		   local environment locations.  */
		r3++;
		while (r3--) {
			asmAsm (
				POP, R2,
				STI, R2, R0, r3+2l
			);
		}
		/* Set env register to the newly extended environment. */
		asmAsm(MV, R1C, R0);
	}

	/* Skip lambda's formal argument list. */
	rexpr = cdr(rexpr);

	/* Compile expressions in lambda block (all but the last).  If the lambda
	   body is empty, emit code that returns null.  This is not to r5rs
	   specification which requires body contain a sequence of one or more
	   expressions. */
	if (rexpr == null) {
		asmAsm(MVI, R0, null);
		DB("   Empty function body.");
	} else {
		/* Transform internal definitions, if any, and body into equivalent
		   expanded letrec and body, ie:(((lambda (f) (set! f ...) body) () () ...)).*/
		ccTransformInternalDefinitions();
		while (cdr(rexpr) != null) {
			DB("   Lambda non-tail optimization");
			vmPush(cdr(rexpr)); /* Push next expr. */
			rexpr = car(rexpr);
			ccCompileExpr((flags & ~CCTAILCALL) | CCNODEFINES);
			rexpr = vmPop();
		}
		DB("   Lambda tail optimization");
		rexpr = car(rexpr);
		ccCompileExpr(flags | CCTAILCALL | CCNODEFINES);
	}

	asmAsm(RET);

	/* Assemble igraph and restore previous ASM context */
//ccDumpIBlocks();
	asmAsmIGraph();

	DBE vmDebugDumpCode(r0, stderr);
	DBEND();
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
void ccNormalizeFormals(void) {
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

void ccLambda (Num flags) {
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'lambda. */

	vmPush(renv); /* Save env. */

	/* Extend pseudo environment only if the formals list is not empty to
	   mimic the runtime optimized environment chain.   A pseudo environment
	   is just the pair (parent-environment . formals-list)*/
	if (car(rexpr) != null) {
		r0=car(rexpr);
		ccNormalizeFormals(); /* Create normalized list in r0, length in r3, dotted-formal bool in r4. */
		r1=renv;  r2=r0;  objCons12();  renv=r0;
	}

	/* Create closures code block in r0. */
	ccLambdaBody(flags);

	renv = vmPop(); /* Restore env. */

	/* Generate code that generates a closure.  Closure returned in r0 created
	   from r1 (code) and r1c (current environment). */
	asmAsm(
		MVI, R1, r0, /* Load r1 with code just generated */
		SYSI, sysNewClosure1Env /* Create closure from r1 and env (r1c) */
	);

	DBEND();
}


void ccBegin (Num flags) {
	DBBEG();
	rexpr = cdr(rexpr); /* Skip symbol 'begin. */

	if (rexpr == null) {
		asmAsm(MVI, R0, null);
	} else {
		while (cdr(rexpr) != null) {
			DB("begin block's non-tail expression");
			vmPush(cdr(rexpr)); /* Push rest of operands */
			rexpr = car(rexpr); /* Consider next operand */
			ccCompileExpr(flags & ~CCTAILCALL);
			rexpr = vmPop(); /* Pop rest of expression */
		}
		DB("begin block's tail expression");
		rexpr = car(rexpr);
		ccCompileExpr(flags);
	}
	DBEND();
}


void ccDefine (Num flags) {
	DBBEG();
	if (flags & CCNODEFINES) {
		//CompError = 1;
		fprintf(stderr, "ERROR: compDefine(): Define not allowed here");
		objDump(rexpr, stderr);
	} else {
		rexpr = cdr(rexpr); /* Skip 'define symbol. */

		vmPush(renv);
		renv = rtge;

		/* If the expression is of the form ((...) body) transform. */
		if (objIsPair(car(rexpr))) {
			ccTransformDefineFunction();
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
				ccCompileExpr((Num)flags & ~CCTAILCALL);
				asmAsm(
					MVI, R1, vmPop(), /* Load r1 with saved binding. */
					STI, R0, R1, 0L);    /* Set binding's value. */
			} else {
				fprintf(stderr, "ERROR: compDefine(): Missing expression.");
			}
		} else  {
			fprintf(stderr, "ERROR: compDefine(): Not a symbol:"); objDump(r1, stderr);
		}

		renv = vmPop();
	}
	DBEND();
}


void compNot (Num flags) {
 Obj L1, L2;
	DBBEG();
	rexpr = cadr(rexpr);           /* Compile this expression */
	ccCompileExpr(flags & ~CCTAILCALL);
 	L1 = asmNewLabel();
 	L2 = asmNewLabel();
	asmAsm (
		BEQI, R0, false, L1,
		MVI, R0, false,
		BRA, L2,
	 LABEL, L1,
		MVI, R0, true,
	 LABEL, L2
	);
	DBEND();
}

/* Compiles expressions of the form (or exp ...) into:
		exp
		branch if not false to end
*/
void ccOr (Num flags) {
 Obj Lend;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'or. */

	/* Empty or expression returns #f. */
	if (null == rexpr) {
		asmAsm (MVI, R0, false);
	} else {
		Lend = asmNewLabel();
		while (objIsPair(rexpr)) {
			vmPush (cdr(rexpr)); /* Push rest. */
			/* Is this the last expression?  If so it's tail optimized. */
			if (!objIsPair(cdr(rexpr))) {
				rexpr = car(rexpr); /* Consider next expression. */
				ccCompileExpr(flags);
			} else {
				rexpr = car(rexpr); /* Consider next expression. */
				ccCompileExpr(flags & ~CCTAILCALL);
				asmAsm(BNTI, R0, TFALSE, Lend);
			}
			rexpr = vmPop();
		}
		asmAsm (LABEL, Lend);
	}
	DBEND();
}

/* Compiles expressions of the form (and exp ...) into:
		exp
		branch if false to end
*/
void ccAnd (Num flags) {
 Obj Lend;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'and. */

	/* Empty or expression returns #t. */
	if (null == rexpr) {
		asmAsm (MVI, R0, true);
	} else {
		Lend = asmNewLabel();
		while (objIsPair(rexpr)) {
			vmPush (cdr(rexpr)); /* Push rest. */
			/* Is this the last expression?  If so it's tail optimized. */
			if (!objIsPair(cdr(rexpr))) {
				rexpr = car(rexpr); /* Consider next expression. */
				ccCompileExpr(flags);
			} else {
				rexpr = car(rexpr); /* Consider next expression. */
				ccCompileExpr(flags & ~CCTAILCALL);
				asmAsm(BRTI, R0, TFALSE, Lend);
			}
			rexpr = vmPop();
		}
		asmAsm (LABEL, Lend);
	}
	DBEND();
}


void ccAsmCombination (Num flags) {
 Num IsTailCall = flags & CCTAILCALL;
 Obj Lsyscall, Lclosure, Lend;
	DBBEG("  IsTailCall="NUM, IsTailCall);
	Lsyscall = asmNewLabel();
	Lclosure = asmNewLabel();

	asmAsm (
		BRTI, R0, TSYSCALL, Lsyscall,
		BRTI, R0, TCLOSURE, Lclosure,
		MVI, R0, "Illegal Operator Type", /* Illegal operator section.  For now just dump the arguments.  Doesn't return.*/
		SYSI, ccError
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
		POP, R19, /* Restores previous environment, ip and code registers. */
		POP, R1B,
		POP, R1A
	);
	DBEND();
}


/* Compiles expression of the form (if testExpr (consequentExpr {value of testExpr}) alternateExpr)
*/
void compAIf (Num flags) {
 Obj LfalseBraAddr, Lend;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'aif symbol. */
	vmPush (cddr(rexpr)); /* Push alternate expressions list.  Will be NULL or a list containing the alternate expression. */
	vmPush (cadr(rexpr));  /* Push consequent expressions. */

	DB("compiling test");
	rexpr = car(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	DB("compiling test logic");
	LfalseBraAddr = asmNewLabel();
	asmAsm (
		BEQI, R0, false, LfalseBraAddr
	);

	DB("compiling consequent");
	/* Save execution state, possibly, since the following is the equivalent of ccCombination */
	if (!((Num)flags & CCTAILCALL)) {
		asmAsm (
			PUSH, R1A,
			PUSH, R1B,
			PUSH, R19);
	}

	asmAsm(
		PUSH, R0 /* Push result of test expression on the stack.  Becomes argument to consequent. */
	);

	/* Compile consequent. */
	rexpr = vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	asmAsm(MVI, R1, 1l);  /* Set the argument count to 1.  Argument already on the stack. */
	ccAsmCombination(flags);

	DB("compiling end of consequent and beginning of alternate");
	Lend = asmNewLabel();
	asmAsm(
		BRA, Lend,
	 LABEL, LfalseBraAddr
	);

	/* Compile alternate expression.  If mising, #f will be returned left over from test condition. */
	DB("compiling alternate");
	rexpr = vmPop();
	if (objIsPair(rexpr)) {
		rexpr = car(rexpr);
		ccCompileExpr(flags);
	}

	asmAsm(LABEL, Lend);

	DBEND();
}

/* Transforms then compiles the cond special form
   (cond <clause> ...)
     clause := (<test>    <expr> ...)
               (<test> => <expr>)
               (<test>)
               (else      <expr> ...)
*/
void ccCond (Num flags) {
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
	ccCompileExpr(flags);
	DBEND();
}

void ccProcedureP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (procedure?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TCLOSURE, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}


void ccNullP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (null?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BEQI, R0, null, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccPairP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (pair?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TPAIR, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccVectorP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (vector?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TVECTOR, Ltrue,
		BRTI, R0, TNULLVEC, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccStringP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (string?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TSTRING, Ltrue,
		BRTI, R0, TNULLSTR, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccIntegerP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (integer?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TINTEGER, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccSymbolP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (integer?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TSYMBOL, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccPortP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (integer?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TPORT, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccEOFObjectP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (integer?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = asmNewLabel();
	Ltrue = asmNewLabel();

	asmAsm(
		BRTI, R0, TEOF, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}


void ccTransformLet (void) {
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

void ccTransformNamedLet (void) {
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

void ccLet (Num flags) {
	DBBEG();
	rexpr=cdr(rexpr); /* Skip 'let. */

	/* Transform named-let form (let symbol ...) */
	if (memObjectType(car(rexpr)) == TSYMBOL)
		ccTransformNamedLet();
	/* Transform let form (let (...) ...). */
	else
		ccTransformLet();

	/* Now compile the transformed form. */
	ccCompileExpr(flags);

	DBEND();
}

/* Transform:
   (letrec ((v exp)...) body)  =>  (let ((v ())...) (set! v exp)... body)
   Why not:  ((lambda (v ...) (set! v exp) ... body) () ...)
*/
void ccTransformLetrec (void) {
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

void ccLetrec (Num flags) {
	DBBEG();
	ccTransformLetrec();
	rexpr = r0;
	ccCompileExpr(flags);
	DBEND();
}

/* Given <qq template> in rexpr, create cons tree in r0.
*/
void ccTransformQuasiquote (int depth) {
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
			ccTransformQuasiquote(depth); /* => b' */
			/* (append template b') */
			r1=r0;     r2=null;  objCons12(); /* => (b') */
			r1=vmPop();  r2=r0;    objCons12(); /* => (template b') */
			r1=sappend;  r2=r0;    objCons12(); /* => (append template b') */
		} else { /* Transform (a . b) => (cons a' b') */
			vmPush(cdr(rexpr)); /* Save b */
			rexpr=car(rexpr);  /* Consider a */
			ccTransformQuasiquote(depth); /* => a' */
			rexpr=vmPop();      /* Restore b */
			vmPush(r0);        /* Save a' */
			ccTransformQuasiquote(depth - isUnquote + isQuasiquote); /* => b' */
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

void ccQuasiquote (Num flags) {
	DBBEG();
	rexpr = cadr(rexpr); // Given (quasiquote <qq template>) pass <qq template>
	ccTransformQuasiquote(0);
	rexpr = r0;
	DB("quasiquote transformation => ");
	DBE objDump (rexpr, stderr);
	ccCompileExpr(flags);
	DBEND();
}


void ccQuote (void) {
	DBBEG();
	asmAsm (
		MVI, R0, cadr(rexpr));
	DBEND();
}


/* Compile the form (apply fn argument-list).  This should be similar to
   a combination expression. */
void ccApply (Num flags) {
 Num operandCount=0;
 Obj Largcount, Largcountdone;
	DBBEG();

	rexpr = cdr(rexpr); /* Skip over 'apply symbol */

	/* Is this a tail call?  if not save state. */
	if (!((Num)flags & CCTAILCALL)) {
		asmAsm (
			PUSH, R1A,
			PUSH, R1B,
			PUSH, R19);
	}

	vmPush(car(rexpr)); /* Save operator parameter. */

	/* Compile operand expressions the last of which hopefully evaluates to a list of args.
	   The resulting arguments will be pushed onto the stack and passed to the function.  */
	rexpr = cdr(rexpr);
	while (objIsPair(rexpr)) {
		vmPush (cdr(rexpr)); /* Push rest */
		rexpr = car(rexpr); /* Consider expression  */
		ccCompileExpr(flags & ~CCTAILCALL);
		asmAsm(PUSH, R0);
		operandCount++;
		rexpr = vmPop();
	}

	/* Restore and compile operator expression. */
	rexpr=vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	asmAsm(MV, R3, R0); /* Save operator in r3 */

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
		BRTI, R0, TNULL, Largcountdone,
		ADDI, R1, 1l, /* Inc argument count in r1. */
		LDI, R2, R0, 0l, /* Push the car. */
		PUSH, R2,
		LDI, R0, R0, 1l, /* Consider cdr. */
		BRA, Largcount,
	 LABEL, Largcountdone,
		MV, R0, R3     /* Operator back to r0 */
	);

	/* Emit code to that applys args to function/code tail optimized or not. */
	ccAsmCombination(flags);

	DBEND();
}

/* Compiles s-expression in r0 into code block in r0.  Probably messes up
   a bunch of registers.
	TODO: does this mangle r19/1a/1b retenv/retip/retcode?
*/
void ccSysCompile (void) {
	DBBEG();

	vmPush(renv); /* Save env vars */
	vmPush(rcode);

	//CompError = 0;
	//asmAsm (BRA, 8, rexpr); /* Keep track of original expression for debugging. */
	rexpr = r0;
	asmInit();
	ccCompileExpr(0);
	//if (CompError)
	//{
	//	r0 = "compSysCompile: ccCompileExpr failed";
	//	compError();
	//	goto ret;
	//}
	asmAsm(RET);

	asmAsmIGraph();
	//r0 = rcode; /* Move new code block to r0 */

	rcode = vmPop(); /* Restore env vars */
	renv = vmPop();

	DBE vmDebugDumpCode(r0, stderr); // Dump the code block after compiling code during runtime.
	DBEND("  =>  ");
	DBE objDump (r0, stderr);
}

void ccEval (Num flags) {
	DBBEG();
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);
	asmAsm(SYSI, (Obj)ccSysCompile);
	if (flags & CCTAILCALL) {
		asmAsm(JMP, R0);
	} else {
		asmAsm(
			PUSH, R1A,
			PUSH, R1B,
			PUSH, R19,
			JAL, R0,
			POP, R19,
			POP, R1B,
			POP, R1A);
	}
	DBEND();
}

void ccMacro (Num flags) {
	DBBEG();

	asmStart();

	/* Transform (macro ... ...) => (lambda .. ...) assigned to r0 */
	r1=slambda;  r2 = cdr(rexpr);  objCons12();

	asmAsm(
		PUSH, R1,
		MVI, R0, r0,
		SYSI, ccSysCompile,
		PUSH, R1A, PUSH, R1B, PUSH, R19,
		JAL, R0,
		POP, R19, POP, R1B, POP, R1A,
		LDI, R2, R0, 0l, /* load r2 with code block and call it.  it will return a closure.  */
		POP, R1,
		JMP, R2);

	asmAsmIGraph();
	//r0 = rcode; /* Move new code block to r0 */

	/* Generate code that generates a closure.  Closure returned in r0 created
	   from r1 (code) and r1c (current environment). */
	asmAsm(
		MVI, R1, r0, /* Load r1 with code block just compiled. */
		SYSI, sysNewClosure1Env, /* Create closure from r1 and env (r1c) */
		MVI, R2, null, /* Replace stored lexical environment with null so dynamic environment is used when applied */
		STI, R2, R0, 1l);

	DBEND();
}

/* Stored stack expected in r3.
*/
void ccSysReinstateContinuation (void) {
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

void ccSysCreateContinuation (void) {
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

	/* Need to protect register rcode/r1e since we are creating
	   a new code block at runtime. [TODO asmAsmIGraph should not clobber rcode] */
	vmPush(rcode);

	asmStart();
	asmAsm(
		MVI, R3, r0,  /* Copy of stack moved to r3 at runtime */
		SYSI, (Obj)ccSysReinstateContinuation);
	asmAsmIGraph();
	r1 = r0; /* Move new code block to r0 */

	rcode = vmPop();

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
void ccDumpHeapHeaders (void) {
	memDebugDumpHeapHeaders(stderr);
}
void ccCallcc (Num flags) {
 Obj Lcontinuationcall;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip over 'call/cc symbol in (call/cc fn)*/

	Lcontinuationcall = asmNewLabel();

	asmAsm(
		SYSI, ccSysCreateContinuation,
		BEQI, R1, 1l, Lcontinuationcall
	);

	/* Is this a tail call?  if not save state. */
	if (!((Num)flags & CCTAILCALL))
		asmAsm (
			PUSH, R1A,
			PUSH, R1B,
			PUSH, R19);

	asmAsm(
		/* Push the continuation just create via ccSysCreateContinuation.  This is the argument to the function */
		PUSH, R0
	);

	rexpr = car(rexpr); /* Consider and compile fn. */
	ccCompileExpr(flags & ~CCTAILCALL);

	/* Setup application to fn */
	asmAsm(
		MVI, R1, 1l
	);

	ccAsmCombination(flags);

	asmAsm(
		LABEL, Lcontinuationcall
	);

	DBEND();
}


void osUnthread (void); /* Refactor this call or ccThread */
void osNewThread (void); /* Refactor this call or ccThread */

void ccThread (void) {
	DBBEG();

	asmStart(); /* Enter a new assembly context */

	/* Compile parameters passed to thread as a begin block emitting the unthread syscall as the last opcode. */
	ccBegin(0);

	asmAsm(
		SYSI, osUnthread
	);
	asmAsmIGraph();

	asmAsm(
		MVI, R0, r0,
		SYSI, osNewThread /* the osNewThread syscall returns thread ID integer object */
	);

	DBEND("  => ");
	DBE objDump(r0, stderr);
}



void ccCombination (Num flags) {
 Int operandCount=0;
	DBBEG();

	if (!((Num)flags & CCTAILCALL)) {
		asmAsm (
			PUSH, R1A, // ip
			PUSH, R1B, // code
			PUSH, R19  // env
		);
	}

	vmPush(car(rexpr)); /* Save expression's operator parameter */

	/* Compile operand expressions. */
	rexpr = cdr(rexpr);
	while (objIsPair(rexpr)) {
		operandCount++;
		vmPush(cdr(rexpr));
		rexpr = car(rexpr);
		ccCompileExpr(flags & ~CCTAILCALL);
		//if (CompError) goto ret;
		asmAsm(PUSH, R0);
		rexpr = vmPop();
	}

	/* Restore and compile operator expression. */
	rexpr = vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	//if (CompError) goto ret;

	/* Emit code that applys args to function/code (hopefully) */
	asmAsm (MVI, R1, operandCount);

	ccAsmCombination(flags);

	DBEND();
}


void ccSelfEvaluating (Num flags) {
	DBBEG();
	asmAsm(MVI, R0, rexpr);
	DBEND();
}


/* Recursive scheme expression compiler.  Translates an expression in
   expr/rf into iblocks in riblocks/rc and code blocks.
*/
void ccCompileExpr (Num flags) {
	DBBEG();
	DBE sysDisplay(rexpr, stderr);

	switch (memObjectType(rexpr)) {
		case TSYMBOL: ccSymbol(flags); break;
		case TPAIR  : if      (ssetb      == car(rexpr)) ccSetB(flags);
			           else if (sif        == car(rexpr)) compIf(flags);
			           else if (scons      == car(rexpr)) ccCons(flags);
			           else if (scar       == car(rexpr)) ccCar(flags);
			           else if (scdr       == car(rexpr)) ccCdr(flags);
			           else if (ssetcarb   == car(rexpr)) ccSetCarB(flags);
			           else if (ssetcdrb   == car(rexpr)) ccSetCdrB(flags);
			           else if (svectorref == car(rexpr)) ccVectorRef(flags);
			           else if (svectorsetb== car(rexpr)) ccVectorSetB(flags);
			           else if (slambda    == car(rexpr)) ccLambda(flags);
			           else if (sbegin     == car(rexpr)) ccBegin(flags);
			           else if (sdefine    == car(rexpr)) ccDefine(flags);
			           else if (snot       == car(rexpr)) compNot(flags);
			           else if (sor        == car(rexpr)) ccOr(flags);
			           else if (sand       == car(rexpr)) ccAnd(flags);
			           else if (saif       == car(rexpr)) compAIf(flags);
			           else if (scond      == car(rexpr)) ccCond(flags);
			           else if (sprocedurep== car(rexpr)) ccProcedureP(flags);
			           else if (snullp     == car(rexpr)) ccNullP(flags);
			           else if (spairp     == car(rexpr)) ccPairP(flags);
			           else if (svectorp   == car(rexpr)) ccVectorP(flags);
			           else if (sstringp   == car(rexpr)) ccStringP(flags);
			           else if (sintegerp  == car(rexpr)) ccIntegerP(flags);
			           else if (ssymbolp   == car(rexpr)) ccSymbolP(flags);
			           else if (sportp     == car(rexpr)) ccPortP(flags);
			           else if (seofobjectp== car(rexpr)) ccEOFObjectP(flags);
			           else if (slet       == car(rexpr)) ccLet(flags);
			           else if (sletrec    == car(rexpr)) ccLetrec(flags);
			           else if (squasiquote== car(rexpr)) ccQuasiquote(flags);
			           else if (squote     == car(rexpr)) ccQuote();
			           else if (sapply     == car(rexpr)) ccApply(flags);
			           else if (seval      == car(rexpr)) ccEval(flags);
			           else if (smacro     == car(rexpr)) ccMacro(flags);
			           else if (scallcc    == car(rexpr)) ccCallcc(flags);
			           else if (sthread    == car(rexpr)) ccThread();
			           else if (srem       == car(rexpr)) ;
			           else ccCombination(flags);
			           break;
		default     : ccSelfEvaluating(flags);
	}

	DBEND();
}



/* Compiles the expression in r0 expr/r15 into an intermediate graph then
   asembles the igraph into a VM code object.
           r0 <= scheme expression to compile
      rexpr/rf = temp
  rcodenew/r11 = temp
            r0 => VM code block
*/
void ccCompile (void) {
	DBBEG();

	rexpr = r0;

	asmInit();
	ccCompileExpr(0);
	/* Finalize the last iblock with the 'ret' opcode */
	asmAsm(RET);
	asmAsmIGraph();
//ccDumpIBlocks();

	DBE vmDebugDumpCode(r0, stderr);
	DBEND();
}



/*******************************************************************************
 Init
*******************************************************************************/
void ccInitialize (void) {
 static Num shouldInitialize=1;
	DBBEG();
	if (shouldInitialize) {
		DB("Activating module");
		shouldInitialize=0;

		DB("Initializing submodules");
		asmInitialize(); /* objInitialize -> vmInitialize -> memInitialize */
		sysInitialize(); /* objInitialize -> vmInitialize -> memInitialize */

		DB("Registering static pointer description strings");
		memPointerRegister(objCons10);
		memPointerRegister(objCons23);
		memPointerRegister(sysNewClosure1Env); 
		memPointerRegister(ccSysCompile); 
		memPointerRegister(ccSysReinstateContinuation); 
		memPointerRegister(ccSysCreateContinuation); 
		memPointerRegister(osNewThread); 
		memPointerRegister("Too many arguments to closure"); 
		memPointerRegister("Illegal Operator Type");
		memPointerRegister(ccError);
		memPointerRegister("runtime error");
		memPointerRegister("Compiler error");
		memPointerRegister("Too many arguments to function");
		memPointerRegister("Not enough arguments to closure");
		memPointerRegister(objNewVector1);
	} else {
		DB("Module already activated");
	}
	DBEND();
}



#undef DB_DESC
#undef DEBUG
