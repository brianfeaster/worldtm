#define DEBUG 0
#define DB_MODULE "COMP "
#include "debug.h"

#include <stdio.h>
#include <unistd.h>
#include "obj.h"
#include "comp.h"

void sysCompile (void);
void wscmWrite (Obj a, long islist, Int fd);
Int  wscmEnvFind (void);
void wscmTGEFind (void);
void wscmError (void);
void sysTGELookup (void);
void wscmTGEBind (void);
void sysTGEMutate (void);
void sysUnthread (void);
void wscmNewThread (void);
void sysWrite (void);
void sysIllegalOperator (void);
void sysCreateContinuation (void);
void wscmDumpEnv (Obj o);

/* Has compiler encountered an error?
*/
Num CompError;

/* Remember BASIC?  This is a 'REMark' or comment syntatic operator.
*/
void compRem () {
	DB("-->%s", __func__);
	DB("<--%s", __func__);
}

void compEval (Num flags) {
	DB("::%s", __func__);
	expr = cadr(expr);
	compExpression(flags & ~TAILCALL);
	asm(SYSI); asm(sysCompile);
	if (flags & TAILCALL) {
		asm(J0);
	} else {
		asm(PUSH1D); asm(PUSH1E); asm(PUSH15);
		asm(JAL0);
		asm(POP15); asm(POP1E); asm(POP1D);
	}
	DB("  --%s", __func__);
}

void compSelfEvaluating (void) {
	DB("-->compSelfEvaluating ");
	asm(MVI0); asm(expr);
	DB("<--compSelfEvaluating ");
}

void compVariableReference (void) {
 Int ret, depth;
	DB("-->compVariableReference: ");
	DBE wscmWrite(expr, 0, 2);
	r1 = expr;
	/* Scan local environments.  Returned is a 16 bit number, the high 8 bits
	   is the environment chain depth, the low 8 bits the binding offset. The
	   offset will be 2 or greater if a variable is found in any environment
	   excluding the global environment. */
	ret = wscmEnvFind();
	if (ret) {
	DB("   found in a local environment %02x", ret);
		/* Emit code that traverses the environment chain and references the
		   proper binding. */
		if ((ret>>8) == 0) {
			asm(LDI016); asm(ret & 0xff);
		} else {
			asm(LDI016); asm(0l);
			for (depth=1; depth < (ret>>8); depth++) {
				asm(LDI00); asm(0l);
			}
			asm(LDI00); asm(ret & 0xff); /* Mask the offset value. */
		}
	} else {
		/* Scan tge... */
		wscmTGEFind();
		if (r0 == null) {
			DB("   can't find in TGE...maybe at runtime");
			asm(MVI1); asm(expr);
			asm(SYSI); asm(sysTGELookup);
		} else {
			DB("   found in TGE");
			asm(MVI0); asm(r0);
			asm(LDI00); asm(0l);
		}
	}

	//memDebugDumpObject(asmstack);

	DB("<--compVariableReference");
}

/* Transform expr:((fn formals) body) into the form
   r0:(fn (lambda formals body)).  No syntic error checking is performed
   yet.  Would rather implement a macro transformation facility.
*/
void compTransformDefineFunction (void) {
	DB("-->%s <= ", __func__);
	r5 = cdr(expr);  /* Function's body. */
	expr = car(expr);
	r3 = car(expr); /* Function's name. */
	r4 = cdr(expr); /* Function's formal parameters. */

	r1=r4;      r2=r5;   objCons12(); /* (formals body) */
	r1=slambda; r2=r0;   objCons12(); /* (lambda formals body) */
	r1=r0;      r2=null; objCons12(); /* ((lambda formals body)) */
	r1=r3;      r2=r0;   objCons12(); /* (fn (lambda formals body)) */
	
	DB("<--%s <= ", __func__);
	DBE wscmWrite(expr, 0, 1);
}


/* Define by itself only makes sense when evaluated in the top level
   environment.  Any other use of it is just syntatic sugar for the various
   let expressions.  For now define will always work and assumes TGE as the
   current working environment.
*/
void compDefine (Num flags) {
	DB("-->compDefine");
	expr = cdr(expr); /* Skip 'define symbol. */

	/* If the expression is of the form ((...) body) transform. */
	if (memObjectType(car(expr)) == TPAIR) {
		compTransformDefineFunction();
		expr = r0;
	}

	if (TSYMBOL == memObjectType(r1=car(expr))) {
		/* Bind (if not already bound) the symbol and get its binding. */
		wscmTGEBind();
		/* Emit code to set the binding's value. */
		expr = cdr(expr);
		if (TPAIR == memObjectType(expr)) {
			push(r0); /* Save binding. */
			expr = car(expr); /* Consider definition expression and compile. */
			compExpression((Num)flags & ~TAILCALL);
			asm(MVI1); asm(pop()); /* Load r1 with saved binding. */
			asm(STI01); asm(0l);    /* Set binding's value. */
		} else {
			write (2, "ERROR: compDefine(): Missing expression.", 34);
		}
	} else  {
		write (2, "ERROR: compDefine(): Not a symbol:", 28); wscmWrite(r1, 0,2);
	}
	DB("<--compDefine");
}


void compSetb (Num flags) {
 Int ret, depth;
	DB("-->compSetb");
	expr = cdr(expr); /* Skip 'set! symbol. */
	push(car(expr)); /* Save symbol. */
	/* Emit code that evaluates the expression. */
	expr = cadr(expr);
	compExpression(flags & ~TAILCALL);

	r1 = pop(); /* Restore symbol. */
	/* Scan local environments.  Returned is a 16 bit number, the high 8 bits
	   is the environment chain depth, the low 8 bits the binding offset. The
	   offset will be 1 or greater if a variable is found in any environment
	   excluding the global environment. */
	ret = wscmEnvFind();
	if (ret) {
	DB("   found in a local environment %02x", ret);
		/* Emit code that traverses the environment chain and references the
		   proper binding. */
		if (ret>>8 == 0) {
			asm(STI016); asm(ret & 0xff);
		} else {
			asm(LDI116); asm(0l);
			for (depth=1; depth < (ret>>8); depth++) {
				asm(LDI11); asm(0l);
			}
			asm(STI01); asm(ret & 0xff); /* Mask the offset value. */
		}
	} else {
		/* Scan tge... */
		wscmTGEFind();
		if (r0 == null) {
			DB("   can't find in TGE...maybe at runtime");
			asm(MVI1); asm(r1);
			asm(SYSI); asm(sysTGEMutate);
		} else {
			DB("   found in TGE");
			asm(MVI1); asm(r0);
			asm(STI01); asm(0l);
		}
	}

	DB("<--compSetb");
}

/* Transform expr:((define x q) (define y r) body)
       => r0:((lambda (x y) (set! x q) (set! y r) body) () ())
*/
void compTransformInternalDefinitions(void) {
 Int definitionsCount=0;
	DB("-->%s", __func__);

	/* Save lambda body. */
	while (memObjectType(car(expr)) == TPAIR && sdefine == caar(expr)) {
			push(cdr(expr));
			expr = cdar(expr); // Consider next expression and skip 'define.
			if (memObjectType(car(expr)) == TPAIR) {
				compTransformDefineFunction(); // Returns (fn (lambda formals body))
			} else {
				r0=expr;
			}
			expr = pop();
			push(r0);
			definitionsCount++;
	}

	/* expr is now pointing at body of function.  If there were any internal
	   definitions, form an equivalent letrec expression. */
	if (definitionsCount) {
		r4=null; /* Local variable list.  Start out empty. */
		r5=expr; /* Set! expressions and body list. Start out with body. */
		r6=null; /* Null arguments list. */
		while (definitionsCount--) {
			r3=pop();/* Considered saved transformed define expression. */
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
		expr=r0;
	}
	
	DB("<--%s => ", __func__);
	DBE wscmWrite(expr, 0, 1);
}

/*        ( (square.#<closure>) (x.5) (y.9) )
          ^
       #( | (y . 9) )
       ^
    #( | (x . ?) )

   (define makeAdder (lambda (y)  (lambda (x) (+ x y)))    )
   (define y*y+y*y   (lambda (y) ((lambda (sqr) (+ sqr sqr))(* y y))) )

   applying function: stack[ argn ... arg2 arg1
     * bind args to bound formals parameters.
     * left over assembled into a list.
*/

/* Given (args body) in expr (r18) create a new code block that basically
   handles a call to a closures function.  The code assumes the closure is
   in r0.  A closure is a pair (code . environment) containing the code itself
   and the closures instantiated environment.
	Expr better be of the form (args body) where args is currently of the
   form: (sym+).  Emit code that keeps track of the current environment

   Create an extended environment given:
   wscmExtendEnvironment moved from system call to inlined assembly.
    r1   - arg count
    r2   - lexical environment
    r3   - symbol list
    r1f (stack) - arguments on the stack.
  IE: ==> #( #<PARENT-ENV> (x y z rest) 1 2 3 (4 5 6))
 So a local environment is of the form #(parent-env (1 . x) (2 . y) ...)
 Symbol lookup will be (1) compiled either as direct reference or a sysCall
 that mutates itself into a direct reference or (2) traversal up the
 environment stack then reference to the local environment's binding then a
 reference to the bindings value.  Guaranteed to never be called with no
 formals to extend as that case is optimized in CompileLambdaBody.
 TODO: 'specialize' this with emitted VM code
*/

/* OUTDATED: If not enough arguments passed, stuff stack with nulls. */

void compLambdaBody (Num flags) {
 Int opcodeStart;
	DB("-->%s", __func__);
	DBE wscmWrite(expr, 0, 1);

	/* Since we're creating a new code object, save the current asmstack and
	   create a new one to start emitting opcodes to. */
	push(asmstack);
	memNewStack(); asmstack=r0;

	asmAsm(
		BRA, 8l,
		expr,
		END
	);

	/* Emit code that extends stack-stored arguments (Free variables can be
	   statically compiled?  r2 is assumed to hold the environment to be
	   extended, r1 the argument count, r3 the formal arguments. */
	if (null == car(expr)) {
		/* Since empty formals function, just set env to closure's env. */
		asm(LDI160); asm(1l);
	} else {
		/* Emit code that extends the environment.  Pops the top most arguments
		   into a list for the 'rest' formal parameter  (lambda (a b . rest)...).
		   R3 contains the non-dotted formal parameter length (via the Normalize
		   function above). */
		opcodeStart = memStackLength(asmstack);
		asmAsm (
			LDI50, 1l,   /* Temporarily save lexical environment to r5. */
			MVI0, null, /* Initial formal argument 'rest' value (empty list). */
			/* r3 is non-dotted formal argument length. */
			BLTI1, r3, ADDR, "notEnoughArguments",
			BEQI1, r3, ADDR, "normalFormals",
		LABEL, "buildRestList",
			MV30,
			POP2,
			SYSI, objCons23,
			ADDI1, -1l,
			BNEI1, r3, ADDR, "buildRestList",
			BRA, ADDR, "normalFormals",
		LABEL, "notEnoughArguments",
			MVI0, "Not enough arguments to closure",
			PUSH0,
			MVI1, 1l,
			SYSI, wscmError, // TODO BF First attempt at error correction.
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
			MVI3,  cdr(env),
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
		asm(MV160);
	}

	/* Skip lambda's formal argument list. */
	expr = cdr(expr);

	/* Compile expressions in lambda block (all but the last).  If the lambda
	   body is empty, emit code that returns null.  This is not to r5rs
	   specification which requires body contain a sequence of one or more
	   expressions. */
	if (expr == null) {
		asm(MVI0); asm(null);
		DB("   Empty function body.");
	} else {
		/* Transform internal definitions, if any, and body into equivalent
		   expanded letrec and body, ie:(((lambda (f) (set! f ...) body)()...)).*/
		compTransformInternalDefinitions();
		while (cdr(expr) != null) {
			DB("   Lambda non-tail optimization");
			push(cdr(expr)); /* Push next expr. */
			expr = car(expr);
			compExpression(flags & ~TAILCALL);
			expr = pop();
		}
		DB("   Lambda tail optimization");
		expr = car(expr);
		compExpression(flags | TAILCALL);
	}

	asm(RET);
	asmNewCode(); /* Transfer code stack to fixed size code vector. */
	//vmDebugDump();

	/* Revert back to code block we're generating. */
	asmstack=pop();

	DB("<--%s => ", __func__);
	DBE wscmWrite (r0, 0, 2);
}



/* Given a 'formals' list in r0 (from a lambda expression) normalize it.  IE:
   (x)->(x())   (x y)->(x y ())   (x . r)->(x r)   (x y . r)->(x y r)   r->(r)
   () (())
	Return:  r0 : normalized formal list
            r3 : non-dotted formal parameter length
*/
void wscmNormalizeFormals(void) {
 Int i;
	r3=0; /* Keep track of non-dotted formal count. */
	/* Push formals onto stack. */
	while (memObjectType(r0) == TPAIR) {
		r3++;
		push(car(r0));
		r0=cdr(r0);
	}
	r1=r0;  r2=null;  objCons12(); /* Create (()) or (rest-arg) */
	/* Pop formals from stack creating list of args. */
	i=(Int)r3;
	while (i--) {
		r2=r0;  r1=pop();  objCons12();
	}
}



void compLambda (Num flags) {
	DB("-->%s", __func__);
	expr = cdr(expr); /* Skip 'lambda. */

	/* Save env. */
	push(env);

	/* Extend pseudo environment only if the formals list is not empty to
	   mimic the runtime optimized environment chain.   A pseudo environment
	   is just the pair (parent-environment . formals-list)*/
	if (car(expr) != null) {
		r0=car(expr);
		wscmNormalizeFormals(); /* Get normalized list in r0, length in r3. */
		r1=env;  r2=r0;  objCons12();  env=r0;
	}

	/* Create closures code block in r0. */
	compLambdaBody(flags);

	/* Restore env. */
	env = pop();

	/* Generate code that generates a closure.  Closure returned in r0 created
	   from r1 (code) and r16 (current environment). */
	asm(MVI1); asm(r0); /* Load r1 with code. */
	asm(SYSI); asm(objNewClosure1Env); /* Create closure from r1 and env (r16) */

	DB("<--%s", __func__);
}

void compVerifyVectorRef (void) {
	if (*(Int*)r0 < 0 || memObjectLength(r1) <= *(Int*)r0) {
		fprintf (stderr, "ERROR::out of bounds:  (vector-ref ");
		wscmWrite(r1, 0, 2);
		fprintf (stderr, " ");
		wscmWrite(r0, 0, 2);
		fprintf (stderr, ")");
		/* Dump the current code block */
		vmDebugDumpCode(code);
		wscmDumpEnv(env);
		fflush(stdout);
		*(Int*)0 = 0;
	}
}

void compVerifyVectorSetB (void) {
	if (*(Int*)r2 < 0 || memObjectLength(r1) <= *(Int*)r2) {
		fprintf (stderr, "ERROR::out of bounds:  (vector-set! ");
		wscmWrite(r1, 0, 2); fprintf (stderr, "  ");
		wscmWrite(r2, 0, 2); fprintf (stderr, "  ");
		wscmWrite(r0, 0, 2); fprintf (stderr, ")");
		*(Int*)0 = 0;
	}
}

void compVectorRef (Num flags) {
	DB("-->%s", __func__);
	push(car(cddr(expr))); /* Save index expression. */
	expr = cadr(expr);       /* Compile Vector expression. */
	compExpression(flags & ~TAILCALL);
	expr = pop();            /* Compile index expression. */
	if (TINTEGER == memObjectType(expr)) {
		/* Load static integer value into register. */
		asm(LDI00); asm(*(Int*)expr);
	} else {
		asm(PUSH0);
		compExpression(flags & ~TAILCALL);
		asm(POP1);
		asm(SYSI); asm(compVerifyVectorRef);
		/* Load object's integer value into register. */
		asm(LDI20); asm(0l); // This fails runtime type check.
		asm(LD012);
	}
	DB("<--%s", __func__);
}

void compVectorVectorRef (Num flags) {
	DB("-->%s", __func__);
	expr=cdr(expr);    /* Skip 'vector-vector-ref. */
	push(cadr(expr));  /* Save 1st index expressions. */
	push(car(expr));   /* Save vector expressions. */

	expr=car(cddr(expr)); /* Compile 2nd expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);

	expr = pop();     /* Restore and compile vector expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);

	expr = pop();     /* Restore and compile 1st index expression. */
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
	DB("<--%s", __func__);
}

void compVectorSetb (Num flags) {
	DB("-->%s", __func__);
	expr=cdr(expr); /* Skip 'vector-set!. */
	push(car(cddr(expr))); /* Save new-value expression. */
	push(cadr(expr));      /* Save index expression. */
	/* Consider and compile Vector expression. */
	expr = car(expr);
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);           /* Save vector object. */
	/* Pop and compile index expression. */
	expr=pop();
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);           /* Save offset object. */
	/* Pop and compile new-value expression. */
	expr=pop();
	compExpression(flags & ~TAILCALL);
	asmAsm (
		POP2,       /* Pop offset object. */
		POP1,       /* Pop vector object. */
		SYSI, compVerifyVectorSetB,
		LDI22, 0,   /* Load offset object's integer value into register. */
		ST012,      /* Store new-value object in vector. */
		END
	);
	DB("<--%s", __func__);
}

void compVectorVectorSetb (Num flags) {
	DB("-->%s", __func__);
	expr=cdr(expr);        /* Skip 'vector-vector-set!. */
	push(cadr(cddr(expr)));/* Save new-value expression. */
	push(cadr(expr));      /* Save 1st index expression. */
	push(car(expr));       /* Save vector expression. */

	expr = car(cddr(expr)); /* Consider and compile 2nd index expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);           /* Save vector object. */

	/* Pop and compile vector expression. */
	expr=pop();
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);           /* Save offset object. */

	/* Pop and compile 1st index expression. */
	expr=pop();
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
	expr=pop();
	compExpression(flags & ~TAILCALL);
	asmAsm (
		POP1,       /* Pop vector object. */
		POP2,       /* Pop offset object. */
		SYSI, compVerifyVectorSetB,
		LDI22, 0l,   /* Load offset object's integer value into register. */
		ST012,      /* Store new-value object in vector. */
		END
	);
	DB("<--%s", __func__);
}

void compCons (Num flags) {
	DB("-->%s", __func__);
	expr = cdr(expr);      /* skip 'cons. */
	push(cadr(expr));      /* Save cdr expression. */
	expr = car(expr);      /* Compile car expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);
	expr = pop();          /* Restore and compile cdr expression. */
	compExpression(flags & ~TAILCALL);
	asmAsm (
		POP1,
		MV20,
		SYSI, objCons12,
		END
	);
	DB("<--%s", __func__);
}

/* Parse the form (? *) placing * into r1
   Return: 0 success  -1 error
*/
Int parseUnary (void) {
	r0 = cdr(expr);
	if (memObjectType(r0) != TPAIR) return -1;
	r1 = car(r0);
	if (cdr(r0) != null) return -1;
	return 0;
}

void compCar (Num flags) {
 Int opcodeStart;
	DB("-->%s", __func__);
	if (parseUnary()) {
		CompError = 1;
		objNewString((u8*)"ERROR: syntax error:", 20);  push(r0);
		push(expr);
		r1=(Obj)2;
		goto ret;
	}
	push(expr); /* Save expression. */
	expr = r1;  /* Consider and compile expression parsed. */
	compExpression(flags & ~TAILCALL);
	expr = pop(); /* Restore expression. */
	objNewString((Str)"RUNTIME ERROR:", 14);
	opcodeStart = memStackLength(asmstack);
	asmAsm(
		BRTI0, TPAIR, ADDR, "car",
		MVI0, r0,
		PUSH0,
		MVI0, expr,
		PUSH0,
		MVI1, 2l,
		SYSI, wscmError,
		LABEL, "car",
		LDI00, 0l, /* Perform car. */
		END
	);
	asmCompileAsmstack(opcodeStart);
ret:
	DB("<--%s", __func__);
}

void compCdr (Num flags) {
	DB("-->%s", __func__);
	if (memObjectType(cdr(expr)) != TPAIR) {
		r0 = "ERROR: cdr illegal operand count: ";
		CompError = 1;
		goto ret;
	}
	expr = cadr(expr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);
	asm(LDI00); asm(1);
ret:
	DB("<--%s", __func__);
}

void compSetCarB (Num flags) {
	DB("-->compSetCarB");
	expr = cdr(expr); /* Skip set-car! symbol. */
	if (expr == null) {
		printf ("ERROR: set-car! illegal pair expression: ");
		wscmWrite (expr, 0, 1);
		goto ret;
	}
	push(car(expr)); /* Save pair expression. */
	expr = cdr(expr);
	if (expr == null) {
		printf ("ERROR: set-car! illegal object expression: ");
		wscmWrite (expr, 0, 1);
		goto ret;
	}
	expr = car(expr);/* Consider and compile object expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);
	expr = pop();
	compExpression(flags & ~TAILCALL);
	asm(POP2);
	asm(STI20); asm(0l);
ret:
	DB("<--compSetCarB");
}

void compSetCdrB (Num flags) {
	DB("-->compSetCdrB");
	expr = cdr(expr); /* Skip set-cdr! symbol. */
	if (expr == null) {
		printf ("ERROR: set-cdr! illegal pair expression: ");
		wscmWrite (expr, 0, 1);
		goto ret;
	}
	push(car(expr)); /* Save pair expression. */
	expr = cdr(expr);
	if (expr == null) {
		printf ("ERROR: set-cdr! illegal object expression: ");
		wscmWrite (expr, 0, 1);
		goto ret;
	}
	expr = car(expr);/* Consider and compile object expression. */
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);
	expr = pop();
	compExpression(flags & ~TAILCALL);
	asm(POP2);
	asm(STI20); asm(1l);
ret:
	DB("<--compSetCdrB");
}

void compNullP (Num flags) {
	DB("-->compNullP");
	if (memObjectType(cdr(expr)) != TPAIR) {
		write (1, "ERROR: null? illegal operand count: ", 36);
		wscmWrite (expr, 0, 1);
	}
	expr = cadr(expr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);
	asm(BRTI0); asm(TNULL); asm(4*8l);
	asm(MVI0); asm(false);
	asm(BRA); asm(2*8l);
	asm(MVI0); asm(true);
	DB("<--compNullP");
}

void compPairP (Num flags) {
	DB("-->compPairP");
	if (memObjectType(cdr(expr)) != TPAIR) {
		write (1, "ERROR: pair? illegal operand count: ", 36);
		wscmWrite (expr, 0, 1);
		return;
	}
	expr = cadr(expr);      /* Consider and compile expression. */
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
 Int opcodeStart;
	DB("-->%s", __func__);
	if (memObjectType(cdr(expr)) != TPAIR) {
		write (1, "ERROR: vector? illegal operand count: ", 38);
		wscmWrite (expr, 0, 1);
		return;
	}
	expr = cadr(expr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);

	opcodeStart = memStackLength(asmstack);
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

	DB("<--%s", __func__);
}

void compStringP (Num flags) {
 Int opcodeStart;
	DB("-->%s", __func__);
	if (memObjectType(cdr(expr)) != TPAIR) {
		write (1, "ERROR: string? illegal operand count: ", 38);
		wscmWrite (expr, 0, 1);
		return;
	}
	expr = cadr(expr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);

	opcodeStart = memStackLength(asmstack);
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

	DB("<--%s", __func__);
}

void compPortP (Num flags) {
	DB("-->%s", __func__);
	if (memObjectType(cdr(expr)) != TPAIR) {
		write (1, "ERROR: vector? illegal operand count: ", 38);
		wscmWrite (expr, 0, 1);
		return;
	}
	expr = cadr(expr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);
	asmAsm(
		BRTI0, TPORT, 4*8l,
		MVI0, false,
		BRA, 2*8l,
		MVI0, true,
		END
	);
	DB("<--%s", __func__);
}


void compEOFObjectP (Num flags) {
 Int opcodeStart;
	DB("-->%s", __func__);
	if (memObjectType(cdr(expr)) != TPAIR) {
		printf ("ERROR: eof-object? illegal operand count: ");
		wscmWrite (expr, 0, 1);
		return;
	}
	expr = cadr(expr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);
	opcodeStart = memStackLength(asmstack);
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
	DB("<--%s", __func__);
}

void compBegin (Num flags) {
	DB("-->compBegin");
	expr = cdr(expr); /* Skip symbol 'begin. */

	if (expr == null) {
		asmAsm(
			MVI0, null,
			END
		);
	} else {
		/* Compile non-tail expression. */
		while (cdr(expr) != null) {
			DB("   compBegin begin block non-tail expression.");
			push(cdr(expr)); /* Push reset of expression. */
			expr = car(expr);
			compExpression(flags & ~TAILCALL);
			expr = pop();
		}
		/* Compile the tail expression. */
		DB("   compBegin begin block tail expression.");
		expr = car(expr);
		compExpression(flags);
	}
	DB("<--compBegin");
}

void compQuote (void) {
	DB("-->compQuote");
	asmAsm (
		MVI0, cadr(expr),
		END
	);
	DB("<--compQuote");
}



/* Compiles expressions of the form (if test consequent alternate).
*/
void compIf (Num flags) {
 Int falseBraAddr, trueContAddr;
	DB("-->%s", __func__);
	expr = cdr(expr); /* Skip 'if symbol. */
	push (cddr(expr)); /* Push alternate expressions list.  Could be NULL. */
	push (cadr(expr));  /* Push consequent expressions. */

	/* Compile 'test' expression. */
	DB("   compiling test");
	expr = car(expr);
	compExpression(flags & ~TAILCALL);

	/* The "branch on type" opcode.  Its immediate branch address field
	   is kept track of and will be set with the proper offset below.  */
	asm(BRTI0); asm(TFALSE); asm(0l);
	falseBraAddr = memStackLength(asmstack);

	DB("   compiling consequent");
	expr = pop(); /* Compile consequent. */
	compExpression(flags);

	/* The "branch after true block" opcode.  Its immediate branch address field
	   is kept track of and will be set with the proper offset below.  */
	asm(BRA); asm(0l);
	trueContAddr = memStackLength(asmstack);

	/* Fill in the "branch on false" field. */
	DB("   setting branch on false:%03x brt TFALSE %02x", falseBraAddr, (8*(trueContAddr-falseBraAddr)));
	memVectorSet(asmstack, falseBraAddr, (Obj)(8*(trueContAddr-falseBraAddr)));

	/* Compile alternate.  Might not be specified in expression so just return (). */
	DB("   compiling alternate");
	expr = pop();
	if (expr == null) {
		asm(MVI0); asm(null);
	}
	else {
		expr = car(expr); /* Consider alternate expression. */
		compExpression(flags);
	}

	/* Fill in the "branch after true block" field. */
	DB("   setting branch after true:%03x bra %02x", trueContAddr, (8*(memStackLength(asmstack)-trueContAddr)));
	memVectorSet(asmstack, trueContAddr,
	                (Obj)(8 * (memStackLength(asmstack) - trueContAddr) ));
	DB("<--%s", __func__);
}

/* Compiles expressions of the form (or exp ...) into:
		exp
		branch if not false to end
*/
void compOr (Num flags) {
 Int opcodeStart;
	DB("-->%s", __func__);
	expr = cdr(expr); /* Skip 'or. */

	/* Empty or expression returns #f. */
	if (null == expr) {
		asm (MVI0); asm(false);
	} else {
		opcodeStart = memStackLength(asmstack);
		while (memObjectType(expr) == TPAIR) {
			push (cdr(expr)); /* Push rest. */
			/* Is this the last expression?  If so it's tail optimized. */
			if (memObjectType(cdr(expr)) != TPAIR) {
				expr = car(expr); /* Consider next expression. */
				compExpression(flags);
			} else {
				expr = car(expr); /* Consider next expression. */
				compExpression(flags & ~TAILCALL);
				asmAsm (
					BNTI0, TFALSE, ADDR, "end",
					END
				);
			}
			expr = pop();
		}
		asm (LABEL); asm ("end");
		asmCompileAsmstack(opcodeStart);
	}
	DB("<-=%s", __func__);
}

/* Compiles expressions of the form (and exp ...) into:
		exp
		branch if false to end
*/
void compAnd (Num flags) {
 Int opcodeStart;
	DB("-->%s", __func__);
	expr = cdr(expr); /* Skip 'and. */

	/* Empty or expression returns #t. */
	if (null == expr) {
		asm (MVI0); asm(true);
	} else {
		opcodeStart = memStackLength(asmstack);
		while (memObjectType(expr) == TPAIR) {
			push (cdr(expr)); /* Push rest. */
			/* Is this the last expression?  If so it's tail optimized. */
			if (memObjectType(cdr(expr)) != TPAIR) {
				expr = car(expr); /* Consider next expression. */
				compExpression(flags);
			} else {
				expr = car(expr); /* Consider next expression. */
				compExpression(flags & ~TAILCALL);
				asmAsm (
					BRTI0, TFALSE, ADDR, "end",
					END
				);
			}
			expr = pop();
		}
		asm (LABEL); asm ("end");
		asmCompileAsmstack(opcodeStart);
	}
	DB("<-=%s", __func__);
}

void compThread (void) {
	DB("-->%s", __func__);

	push(asmstack); /* Save code stack. */

	/* Create new emit-code stack. */
	memNewStack(); asmstack=r0;

	/* Compile parameter passed to thread emitting the unthread syscall
	   as the last opcode. */
	compBegin(0);
	asm(SYSI); asm(sysUnthread);
	asmNewCode();

	asmstack=pop(); /* Restore code stack. */

	asm(MVI0); asm(r0);
	asm(SYSI); asm(wscmNewThread);

	DB("<--%s => ", __func__);
	DBE wscmWrite(r0, 0, 2);
}

void compTransformLet (void) {
 Int bindingLen, i;
	DB("-->%s", __func__);
	r4=car(expr);     /* Consider the let bindings. */
	r5 = cdr(expr);   /* Consider the let body. */

	/* Create (val ...) */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		push(car(cdar(r6)));
		r6=cdr(r6);
	}
	r2=null;
	for (i=0; i<bindingLen; i++) {
		r1=pop();
		objCons12();
		r2=r0;
	}
	push(r2);

	/* Create (var...) */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		push(caar(r6));
		r6=cdr(r6);
	}
	r2=null;
	for (i=0; i<bindingLen; i++) {
		r1=pop();
		objCons12();
		r2=r0;
	}

	/* Create ((var...)body) */
	r1=r2;  r2=r5;  objCons12();

	/* Create (lambda (var...)body) */
	r1=slambda;r2=r0;  objCons12();

	/* Create ((lambda (var...)body) val...) */
	r1=r0;  r2=pop();  objCons12();

	/* Return transormed expression. */
	expr=r0;

	DB("<--%s => ", __func__);
	DBE wscmWrite(expr, 0, 1);
}

void compTransformNamedLet (void) {
 Int bindingLen, i;
	DB("-->%s", __func__);
	r3=car(expr);   /* Consider the named-let name symbol. */
	expr = cdr(expr);
	r4=car(expr);   /* Consider the named-let bindings. */
	r5=cdr(expr);   /* Consider the named-let body. */

	/* Create ((name val ...)) */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		push(car(cdar(r6)));
		r6=cdr(r6);
	}
	r2=null;
	for (i=0; i<bindingLen; i++) {
		r1=pop();
		objCons12();
		r2=r0;
	}
	r1=r3;  objCons12();
	r1=r0;  r2=null;  objCons12();
	push(r0);

	/* Create (set! name (lambda (var...) body)). */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		push(caar(r6));
		r6=cdr(r6);
	}
	r2=null;
	for (i=0; i<bindingLen; i++) {
		r1=pop();
		objCons12();
		r2=r0;
	}
	r1=r2;     r2=r5;  objCons12();
	r1=slambda;r2=r0;  objCons12();
	r1=r0;     r2=null;objCons12();
	r1=r3;     r2=r0;  objCons12();
	r1=ssetb;  r2=r0;  objCons12();

	/* Merge them into new-body. */
	r1=r0;  r2=pop();  objCons12();
	push(r0);

	/* Create (lambda name new-body) */
	r1=r3;  r2=pop();  objCons12();
	r1=slambda; r2=r0;  objCons12();
	push(r0);

	/* Create ((lambda name newbody)) and we're done. */
	r1=pop();  r2=null;  objCons12();

	/* Return transformed expression. */
	expr=r0;

	DB("<--%s => ", __func__);
	DBE wscmWrite(expr, 0, 1);
}

void compLet (Num flags) {
	DB("-->%s", __func__);
	expr=cdr(expr); /* Skip 'let. */

	/* Transform named-let form (let symbol ...). */
	if (memObjectType(car(expr)) == TSYMBOL)
		compTransformNamedLet();
	/* Transform let form (let (...) ...). */
	else
		compTransformLet();

	/* Now compile the transformed form. */
	compExpression(flags);

	DB("<--%s", __func__);
}

/* Transform:
   (letrec ((v exp)...) body)  =>  (let ((v ())...) (set! v exp)... body)
   Why not:  ((lambda (v ...) (set! v exp) ... body) () ...)
*/
void compTransformLetrec (void) {
 Int len;
	DB("-->%s", __func__);
	expr=cdr(expr); /* Skip letrec. */

	/* Push and count letrec binding expressions. */
	for (r3=car(expr), len=0;  r3!=null; r3=cdr(r3), len++) push(car(r3));

	/* Create (()) in r4. */
	r1=null;  r2=null;  objCons12();
	r4=r0;
	/* Create ((x ())...) in r3 from bindings on stack so start it with null. */
	r3=null;
	while(len--) {
		r1=car(pop());  r2=r4;  objCons12(); /* Form (x ()). */
		r1=r0;          r2=r3;  objCons12(); /* Form ((x ()) ...). */
		r3=r0;
	}
	push(r3); /* Save transformed bindings to stack. */

	/* Push and count letrec binding expressions (again). */
	for (r3=car(expr), len=0;  r3!=null; r3=cdr(r3), len++) push(car(r3));
	/* Create (((x ())...) (set! x expr) ... body). */
	r3=cdr(expr); /* Consider (body). */
	while(len--) {
		r1=ssetb;   r2=pop();  objCons12();
		r1=r0;      r2=r3;     objCons12();
		r3=r0;
	}

	/* Create (bindings (set! ...) body). */
	r1=pop();  r2=r3;  objCons12();

	/* Create (let ...). */
	r1=slet; r2=r0;  objCons12();

	DB("<--%s => ", __func__);
	DBE wscmWrite(r0, 0, 1);
}

void compLetrec (Num flags) {
	DB("-->%s", __func__);
	compTransformLetrec();
	expr = r0;
	compExpression(flags);
	DB("<--%s", __func__);
}

/* Given (quasiquote <qq template>) in expr, create cons tree in r0.  Does
   not handle properly (currently) nested unquotes within nested quasiquotes.
*/
void compTransformQuasiquote (void) {
	DB("-->%s", __func__);
	if (memObjectType(expr) == TPAIR) {
		if (car(expr) == sunquote) {
			/* (unquote atom) => atom */
			r0 = cadr(expr);
		} else if (TPAIR == memObjectType(car(expr))
		           && caar(expr) == sunquotesplicing) {
			/* ((unquote-splicing template) . b) */
			push(car(cdar(expr))); /* Save template */
			expr=cdr(expr);  /* Consider b */
			compTransformQuasiquote(); /* => b' */
			/* (append template b') */
			r1=r0;     r2=null;  objCons12(); /* => (b') */
			r1=pop();  r2=r0;    objCons12(); /* => (template b') */
			r1=sappend;  r2=r0;    objCons12(); /* => (append template b') */
		} else { /* Transform (a . b) => (cons a' b') */
			push(cdr(expr)); /* Save b */
			expr=car(expr);  /* Consider a */
			compTransformQuasiquote(); /* => a' */
			expr=pop();      /* Restore b */
			push(r0);        /* Save a' */
			compTransformQuasiquote(); /* => b' */
			r1=r0;     r2=null;  objCons12(); /* => (b') */
			r1=pop();  r2=r0;    objCons12(); /* => (a' b') */
			r1=scons;  r2=r0;    objCons12(); /* => (cons a' b') */
		}
	/* Transform atom into (quote atom) */
	} else {
		r1=expr;   r2=null;  objCons12(); // => (atom)
		r1=squote; r2=r0;    objCons12(); // => (quote atom)
	}
	DB("<--%s", __func__);
}

void compQuasiquote (Num flags) {
	DB("-->%s", __func__);
	expr = cadr(expr); // Consider <qq template>
	compTransformQuasiquote();
	expr = r0;
	DB("   %s quasiquote transformation => ", __func__);
	DBE wscmWrite (expr, 0, 2);
	compExpression(flags);
	DB("<--%s", __func__);
}

/* Test code that for now emits a code block that peforms a compiled tree    
   structure walk.  It'll print the compiled tree nodes along with the 
   corresponding passed tree nodes.  Not sure where I was going with this.
   IE ((syntax-rules (x y z)) '(1 (2 3) 4)) => x1y(2 3)z4
*/
void compSyntaxRulesHelper (void) {
	DB("   -->wscmSyntaxRulesHelper");
	if (TPAIR == memObjectType(expr)) {
		/* R2 contains the expression to be transformed.  */
		DB("   Considering:");
		DBE wscmWrite(expr, 0, 2);
		push(cdr(expr));
		asm(LDI02); asm(1l);
		asm(PUSH0);
		expr = car(expr);
		asm(LDI22); asm(0l);
		compSyntaxRulesHelper();
		expr=pop();
		asm(POP2);
		compSyntaxRulesHelper();
	} else if (expr == null) {
	} else {
		asm(MVI0); asm(expr);
		asm(PUSH0);
		asm(MVI1); asm(1l);
		asm(SYSI); asm(sysWrite);
		asm(PUSH2);
		asm(MVI1); asm(1l);
		asm(SYSI); asm(sysWrite);
	}
	DB("   <--wscmSyntaxRulesHelper");
}

/* Experimenal code.  Not sure if is very useful now.
*/ 
void compSyntaxRules (void) {
	DB("-->wscmSyntaxRules <= ");
	DBE wscmWrite (expr, 0, 2);
	expr = cadr(expr);
	/* Create new code block. */
	push(asmstack);
	memNewStack(); asmstack=r0;
	asm(POP2);
	compSyntaxRulesHelper();
	asm(RET);
	asmNewCode(); /* Transfer code stack to fixed size code vector. */
	DBE wscmWrite(r0, 0, 1);
	/* Restore code block. */
	asmstack=pop();
	asm(MVI1); asm(r0); /* Load r1 with code. */
	asm(SYSI); asm(objNewClosure1Env); /* Create closure from r1 and env (r16) */
	DB("<--wscmSyntaxRules");
}

void compNot (Num flags) {
 Int opcodeStart;
	DB("-->%s", __func__);
	expr = cadr(expr);           /* Compile this expression */
	compExpression(flags);
	opcodeStart = memStackLength(asmstack);
	asmAsm (
		BRTI0, TFALSE, ADDR, "false",
		MVI0, false,
		BRA, ADDR, "done",
		LABEL, "false", MVI0, true,
		LABEL, "done",
		END
	);
	asmCompileAsmstack(opcodeStart);
	DB("<--%s", __func__);
}

/* R1A is vm asmstack stack (opcodes pushed onto stack).  R18 contains list
   expressiosn that need to be added.
*/
void compAdd (Num flags) {
 Int sum=0;
	DB("-->%s", __func__);
	expr=cdr(expr); /* Skip '+. */
	push(expr); /* Save parameter list. */
	/* Constant folding:  Scan parameter list for constants and asm a single
	   opcode that stores their sum. */
	while (TPAIR == memObjectType(expr)) {
		if (TINTEGER == memObjectType(car(expr)))
			sum+=*(Int*)car(expr);
		expr=cdr(expr);
	}
DB("   compAdd constant folding:%d", sum);
	objNewInt(sum);
	asmAsm (
		MVI1, r0,
		SYSI, objCopyInteger, /* A copy because atom is mutated. */
		MV10,
		END
	);
	expr=pop(); /* Restore parameter list. */
	/* Scan parameter list for non-constant expressions to compile. */
	while (TPAIR == memObjectType(expr)) {
		if (TINTEGER != memObjectType(car(expr))) {
			asm(PUSH1); /* Save accumulating sum. */
			push(expr);                /* Save parameter list */
			expr = car(expr);           /* Compile this expression */
			compExpression(flags & ~TAILCALL);
			expr = pop();              /* Restore parameter list */
			asm(POP1); /* Restore accumulating sum. */
			asm(ADD10); /* Add result of last expression to sum. */
		}
		expr = cdr(expr);
	}
	asm(MV01);
	DB("<--%s", __func__);
}


void compAsmTailCall () {
	/* Keep track of this opcode position for the compiling of the
	   labels and branches. */
 Int opcodeStart = memStackLength(asmstack);
	DB("::%s", __func__);
	asmAsm (
		BRTI0,  TSYSCALL, ADDR, "syscall",
		BRTI0,  TCLOSURE, ADDR, "code",
		/* Illegal operator section.  For now just dump the arguments.
		*/
		SYSI, sysIllegalOperator,
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
	DB("  --%s", __func__);
}

void compAsmNonTailCall () {
	/* Keep track of this opcode position for the compiling of the
	   labels and branches. */
 Int opcodeStart = memStackLength(asmstack);
	DB("::%s", __func__);
	asmAsm (
		BRTI0,  TSYSCALL, ADDR, "syscall",
		BRTI0,  TCLOSURE, ADDR, "closure",
		/* Illegal operator section.  For now just dump the arguments. */
		SYSI, sysIllegalOperator,
		BRA,  ADDR, "end",
		/* Syscall operator section.  Reference the syscall address, set the
	   	operand count then make the system call.
		*/
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
		POP15, /* Restores previous environment, ip and code registers. */
		POP1E,
		POP1D,
		END
	);
	asmCompileAsmstack(opcodeStart);
	DB("  --%s", __func__);
}

void compCombination (Num flags) {
 Int operandCount=0;
	DB("-->%s", __func__);

	/* Make sure we push/pop the jump and linked code/ip registers at the start
	   and just before the last expression.  This must be done before arguments
	   are pushed onto the stack.  Bummer.
	BF This might actually not work.  Branches to deleted opcodes are occuring.
	if (!((unsigned)flags & TAILCALL)) {
		if (memStackObject(asmstack, 0) == POP1D) {
			memStackPop(asmstack); memStackPop(asmstack); memStackPop(asmstack);
		} else {
			asm(PUSH1D); asm(PUSH1E); asm(PUSH4);
		}
	}
	*/
	if (!((Num)flags & TAILCALL)) {
		asmAsm (
			PUSH1D,
			PUSH1E,
			PUSH15,
			END
		);
	}

	push(car(expr)); /* Save operator parameter. */

	/* Compile operand expressions. */
	expr = cdr(expr);
	while (TPAIR==memObjectType(expr)) {
		operandCount++;
		push(cdr(expr));
		expr = car(expr);
		compExpression(flags & ~TAILCALL);
		if (CompError) goto ret;
		asm(PUSH0);
		expr = pop();
	}

	/* Restore and compile operator expression. */
	expr=pop();
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
	DB("<--%s", __func__);
}



/* Compile the form (apply fn argument-list).  This should be similar to
   a combination expression. */
void compApply (Num flags) {
 Int opcodeStart;
	DB("-->%s", __func__);

	expr = cdr(expr); /* Skip over 'apply symbol */

	/* Is this a tail call?  if not save state. */
	if (!((Num)flags & TAILCALL)) {
		asmAsm (
			PUSH1D,
			PUSH1E,
			PUSH15,
			END
		);
	}

	push(car(expr)); /* Save operator parameter. */

	/* Compile operand expression which hopefully evaluates to a list of args. */
	expr = cadr(expr);
	compExpression(flags & ~TAILCALL);
	asm(PUSH0);

	/* Restore and compile operator expression. */
	expr=pop();
	compExpression(flags & ~TAILCALL);
	asm(MV30); /* Save operator in r3 */

	/* At this point stack has argument-list and r3 has function.  Want
	   to transfers arguments from list to the stack with r1 ending up
	   with the argument count.
	*/
	opcodeStart = memStackLength(asmstack);
	asmAsm (
		MVI1, 0l, /* Initialize operand count in r1 to 0. */
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

	DB("<--%s", __func__);
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
 Int opcodeStart;
	DB("-->%s", __func__);
	expr = cdr(expr); /* Skip over 'call/cc symbol in (call/cc fn)*/

	opcodeStart = memStackLength(asmstack);
	asmAsm(
		SYSI, sysCreateContinuation,
		BRA, ADDR, "continuationcall",
		END
	);

	/* Is this a tail call?  if not save state. */
	if (!((Num)flags & TAILCALL))
		asmAsm (
			PUSH1D,
			PUSH1E,
			PUSH15,
			END
		);

	asm(PUSH0); /* Push the continuation just create. */

	expr = car(expr); /* Consider and compile fn. */
	compExpression(flags & ~TAILCALL);

	asm (MVI1);  asm(1);
	if ((unsigned)flags & TAILCALL) compAsmTailCall();
	else compAsmNonTailCall();

	asmAsm(LABEL, "continuationcall", END);
	asmCompileAsmstack(opcodeStart);

	DB("<--%s", __func__);
}



/* Compile expression.
   expr (r18) -> Expression to compile.
   asmstack (r1a) -> Stack the emitted opcodes are pushed onto.
   env (r16) -> Pseudo environment
	An expression is either a symbol, syntax, combination or self evaluating.
*/
Int compExpression (Num flags) {
	DB("-->%s", __func__);
	switch (memObjectType(expr)) {
		case TSYMBOL :
			compVariableReference();
			break;
		case TPAIR :
			if      (srem       == car(expr)) compRem(flags);
			else if (sdefine    == car(expr)) compDefine(flags);
			else if (ssetb      == car(expr)) compSetb(flags);
			else if (slambda    == car(expr)) compLambda(flags);
			else if (snot       == car(expr)) compNot(flags);
			else if (sadd       == car(expr)) compAdd(flags);
			else if (svectorref == car(expr)) compVectorRef(flags);
			else if (svectorvectorref == car(expr)) compVectorVectorRef(flags);
			else if (svectorsetb== car(expr)) compVectorSetb(flags);
			else if (svectorvectorsetb== car(expr)) compVectorVectorSetb(flags);
			else if (scons      == car(expr)) compCons(flags);
			else if (scar       == car(expr)) compCar(flags);
			else if (scdr       == car(expr)) compCdr(flags);
			else if (ssetcarb   == car(expr)) compSetCarB(flags);
			else if (ssetcdrb   == car(expr)) compSetCdrB(flags);
			else if (snullp     == car(expr)) compNullP(flags);
			else if (spairp     == car(expr)) compPairP(flags);
			else if (svectorp   == car(expr)) compVectorP(flags);
			else if (sstringp   == car(expr)) compStringP(flags);
			else if (sportp     == car(expr)) compPortP(flags);
			else if (seofobjectp== car(expr)) compEOFObjectP(flags);
			else if (sbegin     == car(expr)) compBegin(flags);
			else if (squote     == car(expr)) compQuote();
			else if (sif        == car(expr)) compIf(flags);
			else if (sor        == car(expr)) compOr(flags);
			else if (sand       == car(expr)) compAnd(flags);
			else if (sthread    == car(expr)) compThread();
			else if (slet       == car(expr)) compLet(flags);
			else if (sletrec    == car(expr)) compLetrec(flags);
			else if (squasiquote== car(expr)) compQuasiquote(flags);
			else if (ssyntaxrules== car(expr)) compSyntaxRules();
			else if (seval      == car(expr)) compEval(flags);
			else if (sapply     == car(expr)) compApply(flags);
			else if (scallcc    == car(expr)) compCallcc(flags);
			else compCombination(flags);
			break;
		default:
			compSelfEvaluating();
			break;
	}
	DB("<--%s => %d", __func__, CompError);
	return CompError;
}


/* Compile expression.
   r0 -> Expression we're compiling.
   r0 <- Resuling code object (vector of VM opcodes).
*/
Int compCompile (void) {
 Int ret;
	DB ("::%s", __func__);
	//env = tge;               /* We'll be using a pseudo env (r16=r17). */
	expr = r0;                 /* Move expression to expr (r18). */
	push(expr);						/* Keep track of expression (for debugging). */
	asmAsm ( /* Keep track of original expression for debugging. */
		BRA, 8,
		expr,
		END
	);
	CompError = 0;             /* Clear error flag. */
	/* START the compilation process with empty flags. */
	ret = compExpression(0);
	asm(QUIT); /* Emit the QUIT opcode which exits the VM. */
	asmNewCode();
	DB("  --%s", __func__);
	return ret;
}

#undef DB_MODULE
