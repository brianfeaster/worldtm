#define DEBUG 0
#define DB_MODULE "COMP "
#include "debug.h"

#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include "obj.h"
#include "comp.h"

/* TODO This compiler module is not reentrant.
*/

void sysCompile (void);
Num wscmWrite (Obj a, FILE *stream);
Int  wscmEnvFind (void);
void wscmTGEFind (void);
void wscmEnvGet (void);
void sysError (void);
void sysDebugger (void);
void sysTGELookup (void);
void wscmTGEBind (void);
void sysTGEMutate (void);
void sysUnthread (void);
void wscmNewThread (void);
void sysWrite (void);
void sysIllegalOperator (void);
void sysCreateContinuation (void);
void sysDumpCallStackCode (void);

/* Has compiler encountered an error flag?
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
	asmAsm(
		SYSI, sysCompile,
	END);
	if (flags & TAILCALL) {
		asm(J0);
	} else {
		asmAsm(
			PUSH1D, PUSH1E, PUSH15,
			JAL0,
			POP15, POP1E, POP1D,
		END);
	}
	DB("  --%s", __func__);
}

/* Doesn't need compiling so just return it.
*/
void compSelfEvaluating (void) {
	DB("-->compSelfEvaluating ");
	asm(MVI0); asm(expr);
	DB("<--compSelfEvaluating ");
}

void compVariableReference (Num flags) {
 Int ret, depth;
	DB("-->compVariableReference: ");
	DBE wscmWrite(expr, stderr);
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
	DBE wscmWrite(expr, stdout);
}


/* Define by itself only makes sense when evaluated in the top level
   environment.  Any other use of it is just syntatic sugar for the various
   let expressions.  For now define will always work and assumes TGE as the
   current working environment.
*/
void compDefine (Num flags) {
	DB("-->compDefine");
	expr = cdr(expr); /* Skip 'define symbol. */

	push(env);
	env = tge;

	/* If the expression is of the form ((...) body) transform. */
	if (objIsPair(car(expr))) {
		compTransformDefineFunction();
		expr = r0;
	}

	if (TSYMBOL == memObjectType(r1=car(expr))) {
		/* Bind (if not already bound) the symbol and get its binding. */
		wscmTGEBind();
		/* Emit code to set the binding's value. */
		expr = cdr(expr);
		if (objIsPair(expr)) {
			push(r0); /* Save binding. */
			expr = car(expr); /* Consider this definition's expression and compile. */
			compExpression((Num)flags & ~TAILCALL);
			asm(MVI1); asm(pop()); /* Load r1 with saved binding. */
			asm(STI01); asm(0l);    /* Set binding's value. */
		} else {
			write (2, "ERROR: compDefine(): Missing expression.", 40);
		}
	} else  {
		write (2, "ERROR: compDefine(): Not a symbol:", 34); wscmWrite(r1, stderr);
	}

	env = pop();

	DB("<--compDefine");
	return;
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
	while (objIsPair(car(expr)) && sdefine == caar(expr)) {
			push(cdr(expr));
			expr = cdar(expr); // Consider next expression and skip 'define.
			if (objIsPair(car(expr))) {
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
	DBE wscmWrite(expr, stdout);
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
	DB("-->%s", __func__);
	DBE wscmWrite(expr, stdout);

	/* Since we're creating a new code object, save the current asmstack and
	   create a new one to start emitting opcodes to. */
	push(asmstack);
	memNewStack(); asmstack=r0;

	/* The first opcode emitted is a branch past a pointer to the original
	   expression being compiled.  This is a quick and dirty debugging aid. */
	asmAsm(
		BRA, 8l,
		expr,
		END
	);

	/* Emit code that extends stack-stored arguments (Free variables can be
	   statically compiled?  r2 is assumed to hold the environment to be
	   extended, r1 the argument count, r3 the formal arguments. */
	if (null == car(expr)) {
		/* Since a lambda with empty formals list, emit code which doesn't extend
		   the environment but instead sets env to the containing closure's env
		   or TGE if this is a top level definition (which will always be for 'lambda
		   and not 'macro) */
		if (env==tge) asm(MV1617);
		else { asm(LDI160); asm(1l); }

		opcodeStart = memStackLength(asmstack);
		asmAsm (
			BEQI1, 0, ADDR, "expectedNoArgs",
			MVI0, expr, /* Error situation.  Add expression to stack */
			PUSH0,
			ADDI1, 1l,
			MVI0, "Too many arguments to function",
			SYSI, sysError, /* Error correction */
			LABEL, "expectedNoArgs",
			END);
		asmCompileAsmstack(opcodeStart);
	} else {
		/* Emit code that extends the environment.  Pops the top most arguments
		   into a list for the 'rest' formal parameter  (lambda (a b . rest)...).
		   R3 contains the non-dotted formal parameter length (via the Normalize
		   function above). */
		opcodeStart = memStackLength(asmstack);

		/* Temporarily save lexical environment, from closure in r0 or tge, to r5.
		   The stored environment might be null in which case keep track of
		   the current/dynamic environment instead of the stored lexical.  Use
		   TGE when a top level definition.  See also the similar situation in this
		   if blocks true clause with the empty formals case. */
		if (car(env)==tge) asm(MV517);
		else { asm(LDI50); asm(1l); }

		asmAsm (
			BNEI5, null, ADDR, "keepLexicalEnvironment",
			MV516,
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
				MVI0, expr, /* Add expression to stack */
				PUSH0,
				ADDI1, 1l,
				MVI0, "Too many arguments to function",
				SYSI, sysError, /* Error correction */
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
			MVI0, expr, /* Add expression to stack */
			PUSH0,
			ADDI1, 1l,
			MVI0, "Not enough arguments to closure",
			SYSI, sysError, /* Error correction */
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
		   expanded letrec and body, ie:(((lambda (f) (set! f ...) body) () () ...)).*/
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
	DBE wscmWrite (r0, stderr);
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
void wscmNormalizeFormals(void) {
 Num i;
	r3=0; /* Keep track of non-dotted formal count. */

	/* Push formals onto stack. */
	while (objIsPair(r0)) {
		r3++;
		push(car(r0));
		r0=cdr(r0);
	}

	/* Keep track of the existence of a dotter formal */
	r4 = (r0==null) ? (Obj)0 : (Obj)1;

	/* Pop formals from stack creating list of args starting
      with (()) or (dotted-formal) */
	r1=r0;  r2=null;  objCons12();
	i=(Num)r3;
	while (i--) { r2=r0;  r1=pop();  objCons12(); }
}



/* Uses  r0 r1 r2 r3 r4
*/
void compLambda (Num flags) {
	DB("-->%s", __func__);
	expr = cdr(expr); /* Skip 'lambda. */

	push(env); /* Save env. */

	/* Extend pseudo environment only if the formals list is not empty to
	   mimic the runtime optimized environment chain.   A pseudo environment
	   is just the pair (parent-environment . formals-list)*/
	if (car(expr) != null) {
		r0=car(expr);
		wscmNormalizeFormals(); /* Create normalized list in r0, length in r3, dotted-formal bool in r4. */
		r1=env;  r2=r0;  objCons12();  env=r0;
	}

	/* Create closures code block in r0. */
	compLambdaBody(flags);

	env = pop(); /* Restore env. */

	/* Generate code that generates a closure.  Closure returned in r0 created
	   from r1 (code) and r16 (current environment). */
	asmAsm(
		MVI1, r0, /* Load r1 with code. */
		SYSI, objNewClosure1Env, /* Create closure from r1 and env (r16) */
		END);

	DB("<--%s", __func__);
}



void compMacro (Num flags) {
	DB("::%s", __func__);

	push(asmstack);
	memNewStack(); asmstack=r0;

	/* Transform (macro ... ...) => (lambda .. ...) assigned to r0 */
	r1=slambda;  r2 = cdr(expr);  objCons12();

	asmAsm(
		PUSH1,
		MVI0, r0,
		SYSI, sysCompile,
		PUSH1D, PUSH1E, PUSH15,
		JAL0,
		POP15, POP1E, POP1D,
		LDI20, 0l, /* load r2 with code and jump. */
		POP1,
		J2,
	END);

	asmNewCode(); /* Transfer code stack to fixed size code vector into r0. */
	asmstack=pop();

	/* Generate code that generates a closure.  Closure returned in r0 created
	   from r1 (code) and r16 (current environment). */
	asmAsm(
		MVI1, r0, /* Load r1 with code block just compiled. */
		SYSI, objNewClosure1Env, /* Create closure from r1 and env (r16) */
		MVI2, null, /* Replace stored lexical environment with null so dynamic environment is used when applied */
		STI20, 1l,
	END);

	DB("  --%s", __func__);
}



void compVerifyVectorRef (void) {
	if (*(Int*)r0 < 0 || memObjectLength(r1) <= *(Int*)r0) {
		fprintf (stderr, "\nERROR::out of bounds:  (vector-ref ");
		wscmWrite(r1, stderr); fprintf (stderr, " ");
		wscmWrite(r0, stderr); fprintf (stderr, ")");
		sysDebugger();
	}
}

void compVerifyVectorSetB (void) {
	if (*(Int*)r2 < 0 || memObjectLength(r1) <= *(Int*)r2) {
		fprintf (stderr, "\nERROR::out of bounds:  (vector-set! ");
		wscmWrite(r1, stderr); fprintf (stderr, " ");
		wscmWrite(r2, stderr); fprintf (stderr, " ");
		wscmWrite(r0, stderr); fprintf (stderr, ")");
		sysDebugger();
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
	if (!objIsPair(r0)) return -1;
	r1 = car(r0);
	if (cdr(r0) != null) return -1;
	return 0;
}

void compCar (Num flags) {
 Num opcodeStart;
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
		SYSI, sysError,
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
	if (!objIsPair(cdr(expr))) {
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
		wscmWrite (expr, stdout);
		goto ret;
	}
	push(car(expr)); /* Save pair expression. */
	expr = cdr(expr);
	if (expr == null) {
		printf ("ERROR: set-car! illegal object expression: ");
		wscmWrite (expr, stdout);
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
		wscmWrite (expr, stdout);
		goto ret;
	}
	push(car(expr)); /* Save pair expression. */
	expr = cdr(expr);
	if (expr == null) {
		printf ("ERROR: set-cdr! illegal object expression: ");
		wscmWrite (expr, stdout);
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

void compProcedureP (Num flags) {
	DB("-->%s", __func__);
	if (!objIsPair(cdr(expr))) {
		write (1, "ERROR: null? illegal operand count: ", 36);
		wscmWrite (expr, stdout);
	}
	expr = cadr(expr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);
	asm(BRTI0); asm(TCLOSURE); asm(4*8l);
	asm(MVI0); asm(false);
	asm(BRA); asm(2*8l);
	asm(MVI0); asm(true);
	DB("<--%s", __func__);
}

void compNullP (Num flags) {
	DB("-->compNullP");
	if (!objIsPair(cdr(expr))) {
		write (1, "ERROR: null? illegal operand count: ", 36);
		wscmWrite (expr, stdout);
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
	if (!objIsPair(cdr(expr))) {
		write (1, "ERROR: pair? illegal operand count: ", 36);
		wscmWrite (expr, stdout);
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
 Num opcodeStart;
	DB("-->%s", __func__);
	if (!objIsPair(cdr(expr))) {
		write (1, "ERROR: vector? illegal operand count: ", 38);
		wscmWrite (expr, stdout);
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
 Num opcodeStart;
	DB("-->%s", __func__);
	if (!objIsPair(cdr(expr))) {
		write (1, "ERROR: string? illegal operand count: ", 38);
		wscmWrite (expr, stdout);
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

void compIntegerP (Num flags) {
 Num opcodeStart;
	DB("-->%s", __func__);
	if (!objIsPair(cdr(expr))) {
		write (1, "ERROR: integer? illegal operand count: ", 38);
		wscmWrite (expr, stdout);
		return;
	}
	expr = cadr(expr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);

	opcodeStart = memStackLength(asmstack);
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

	DB("<--%s", __func__);
}

void compSymbolP (Num flags) {
 Num opcodeStart;
	DB("-->%s", __func__);
	if (!objIsPair(cdr(expr))) {
		write (1, "ERROR: symbol? illegal operand count: ", 38);
		wscmWrite (expr, stdout);
		return;
	}
	expr = cadr(expr);      /* Consider and compile expression. */
	compExpression(flags & ~TAILCALL);

	opcodeStart = memStackLength(asmstack);
	asmAsm (
		BRTI0, TSYMBOL, ADDR, "yes",
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
	if (!objIsPair(cdr(expr))) {
		write (1, "ERROR: vector? illegal operand count: ", 38);
		wscmWrite (expr, stdout);
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
 Num opcodeStart;
	DB("-->%s", __func__);
	if (!objIsPair(cdr(expr))) {
		printf ("ERROR: eof-object? illegal operand count: ");
		wscmWrite (expr, stdout);
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


void compAsmTailCall () {
	/* Keep track of this opcode position for the compiling of the
	   labels and branches. */
 Num opcodeStart = memStackLength(asmstack);
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
 Num opcodeStart = memStackLength(asmstack);
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


/* Compiles expressions of the form (if test consequent alternate).
*/
void compIf (Num flags) {
 Num falseBraAddr, trueContAddr;
	DB("-->%s", __func__);
	expr = cdr(expr); /* Skip 'if symbol. */
	push (cddr(expr)); /* Push alternate expressions list.  Will be NULL or a list containing the alternate expression. */
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

/* Compiles expression of the form (if testExpr (consequentExpr {value of testExpr}) alternateExpr)
*/
void compAIf (Num flags) {
 Num falseBraAddr, trueContAddr;
	DB("-->%s", __func__);
	expr = cdr(expr); /* Skip 'aif symbol. */
	push (cddr(expr)); /* Push alternate expressions list.  Will be NULL or a list containing the alternate expression. */
	push (cadr(expr));  /* Push consequent expressions. */

	/* Compile 'test' expression. */
	DB("   compiling test");
	expr = car(expr);
	compExpression(flags & ~TAILCALL);

	/* The "branch on type" opcode.  Its immediate branch address field
	   is kept track of and will be set with the proper offset below.  */
	asm(BRTI0); asm(TFALSE); asm(0l);
	falseBraAddr = memStackLength(asmstack);

	/* Save execution state, possibly, since the following is the equivalent
	   of compCombination */
	if (!((Num)flags & TAILCALL)) {
		asmAsm (
			PUSH1D,
			PUSH1E,
			PUSH15,
			END
		);
	}

	DB("   compiling consequent");
	asm(PUSH0); /* Push result of test expression on the stack.  Becomes argument to consequent. */
	expr = pop(); /* Compile consequent. */
	compExpression(flags & ~TAILCALL);

	asm(MVI1); asm(1l); /* Set the argument count to 1.  Argument already on the stack. */
	
	if ((Num)flags & TAILCALL) compAsmTailCall();
	else compAsmNonTailCall();

	asm(BRA); asm(0l);
	trueContAddr = memStackLength(asmstack);

	/* Fill in the "branch on false" field. */
	DB("   setting branch on false:%03x brt TFALSE %02x", falseBraAddr, (8*(trueContAddr-falseBraAddr)));
	memVectorSet(asmstack, falseBraAddr, (Obj)(8*(trueContAddr-falseBraAddr)));

	/* Compile alternate.  Might not be specified in expression so just return (). */
	DB("   compiling alternate");
	expr = pop();
	if (expr == null) {
		//asm(MVI0); asm(null);
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

/* Transforms then compiles the cond special form
   (cond <clause> ...)
     clause := (<test>    <expr> ...)
               (<test> => <expr>)
               (<test>)
               (else      <expr> ...)
*/
void compCond (Num flags) {
 Num clauses=0;
	DB("-->%s", __func__);
	expr = cdr(expr); /* Skip symbol 'cond */

	/* Push clauses, checking for non-lists and verifying the else clause is last */
	while (objIsPair(expr)) { /* Over all clauses  expr = (<clause> ....) */
		r5 = car(expr); /* Consider next clause  r5 = <clause>  */
		if (!objIsPair(r5)) { /* Error if clause is not a list */
			fprintf (stderr, "\nWARNING: compCond: Malformed cond clause ");
			wscmWrite(r5, stderr);
		} else {
			DB("Pushing clause");
			DBE wscmWrite(r5, stderr);
			clauses++;
			push(r5);
			expr = cdr(expr); /* Consider next clause for this loop */
			if (selse == car(r5)) {
				/* Else clause matched, so stop pushing clauses and give warning if more clauses follow */
				if (expr != null) {
					fprintf (stderr, "\nWARNING: compCond: cond's else clause followed by more clauses ");
					wscmWrite(expr, stderr);
				}
				expr = null;
			}
		}
	}

	/* Pop clauses building the if/or/begin tree bottom-up into r0 */
	DB (" Creating nested if/or/begin expression");
	r0 = null;
	while (clauses--) {
		r5 = pop(); /* Consider clause r5 = <clause> = (r4 . r3) */
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
				wscmWrite(r5, stderr);
			}
			r1=car(r3); r2=r0; objCons12();         /* (<expr> translated) */
			r1=r4;  r2=r0; objCons12();             /* (<test> <expr> translated) */
			r1=saif; r2=r0; objCons12();            /* (if <test> <expr> translated) */
		} else {
			r1=r0;  r2=null; objCons12(); push(r0); /* (translated) */
			r1=sbegin; r2=r3; objCons12();          /* (begin <expr> ...) */
			r1=r0; r2=pop(); objCons12();           /* ((begin <expr> ...) translated) */
			r1=r4;  r2=r0; objCons12();             /* (<test> (begin <expr> ...) translated) */
			r1=sif; r2=r0; objCons12();             /* (if <test> (begin <expr> ...) translated) */
		}
	}
	DB ("compCond translated ");
	DBE wscmWrite(r0, stdout);
	expr = r0;
	compExpression(flags);
	DB("<--%s", __func__);
}

/* Compiles expressions of the form (or exp ...) into:
		exp
		branch if not false to end
*/
void compOr (Num flags) {
 Num opcodeStart;
	DB("-->%s", __func__);
	expr = cdr(expr); /* Skip 'or. */

	/* Empty or expression returns #f. */
	if (null == expr) {
		asm (MVI0); asm(false);
	} else {
		opcodeStart = memStackLength(asmstack);
		while (objIsPair(expr)) {
			push (cdr(expr)); /* Push rest. */
			/* Is this the last expression?  If so it's tail optimized. */
			if (!objIsPair(cdr(expr))) {
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
 Num opcodeStart;
	DB("-->%s", __func__);
	expr = cdr(expr); /* Skip 'and. */

	/* Empty or expression returns #t. */
	if (null == expr) {
		asm (MVI0); asm(true);
	} else {
		opcodeStart = memStackLength(asmstack);
		while (objIsPair(expr)) {
			push (cdr(expr)); /* Push rest. */
			/* Is this the last expression?  If so it's tail optimized. */
			if (!objIsPair(cdr(expr))) {
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
	DBE wscmWrite(r0, stderr);
}

void compTransformLet (void) {
 Num bindingLen, i;
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

	/* Create ((lambda (var...) body) val...) */
	r1=r0;  r2=pop();  objCons12();

	/* Return transformed expression. */
	expr=r0;

	DB("<--%s => ", __func__);
	DBE wscmWrite(expr, stdout);
}

void compTransformNamedLet (void) {
 Num bindingLen, i;
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
	DBE wscmWrite(expr, stdout);
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
 Num len;
	DB("-->%s", __func__);
	expr=cdr(expr); /* Skip letrec. */

   if (!objIsPair(car(expr))) {
		fprintf (stderr, "letrec malformed: ");
		wscmWrite(expr, stderr);
	}

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
	DBE wscmWrite(r0, stdout);
}

void compLetrec (Num flags) {
	DB("-->%s", __func__);
	compTransformLetrec();
	expr = r0;
	compExpression(flags);
	DB("<--%s", __func__);
}

/* Given <qq template> in expr, create cons tree in r0.
*/
void compTransformQuasiquote (int depth) {
 int isUnquote, isQuasiquote;
	DB("-->%s", __func__);
	if (objIsPair(expr)) { /* Is this (unquote ...) */
		isUnquote    = (car(expr)==sunquote);
		isQuasiquote = (car(expr)==squasiquote);
		if (isUnquote && depth==0) {
			/* (unquote atom) => atom */
			r0 = cadr(expr);
		} else if (objIsPair(car(expr))
		           && caar(expr) == sunquotesplicing
		           && depth==0) {
			/* ((unquote-splicing template) . b) */
			push(car(cdar(expr))); /* Save template */
			expr=cdr(expr);  /* Consider b */
			compTransformQuasiquote(depth); /* => b' */
			/* (append template b') */
			r1=r0;     r2=null;  objCons12(); /* => (b') */
			r1=pop();  r2=r0;    objCons12(); /* => (template b') */
			r1=sappend;  r2=r0;    objCons12(); /* => (append template b') */
		} else { /* Transform (a . b) => (cons a' b') */
			push(cdr(expr)); /* Save b */
			expr=car(expr);  /* Consider a */
			compTransformQuasiquote(depth); /* => a' */
			expr=pop();      /* Restore b */
			push(r0);        /* Save a' */
			compTransformQuasiquote(depth - isUnquote + isQuasiquote); /* => b' */
			r1=r0;     r2=null;  objCons12(); /* => (b') */
			r1=pop();  r2=r0;    objCons12(); /* => (a' b') */
			r1=scons;  r2=r0;    objCons12(); /* => (cons a' b') */
		}
	/* Transform atom into (quote atom) */
	} else {
		r1=expr;   r2=null;  objCons12(); // atom   => (atom)
		r1=squote; r2=r0;    objCons12(); // (atom) => (quote atom)
	}
	DB("<--%s", __func__);
}

void compQuasiquote (Num flags) {
	DB("-->%s", __func__);
	expr = cadr(expr); // Given (quasiquote <qq template>) pass <qq template>
	compTransformQuasiquote(0);
	expr = r0;
	DB("   %s quasiquote transformation => ", __func__);
	DBE wscmWrite (expr, stderr);
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
	if (objIsPair(expr)) {
		/* R2 contains the expression to be transformed.  */
		DB("   Considering:");
		DBE wscmWrite(expr, stderr);
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
	DBE wscmWrite (expr, stderr);
	expr = cadr(expr);
	/* Create new code block. */
	push(asmstack);
	memNewStack(); asmstack=r0;
	asm(POP2);
	compSyntaxRulesHelper();
	asm(RET);
	asmNewCode(); /* Transfer code stack to fixed size code vector. */
	DBE wscmWrite(r0, stdout);
	/* Restore code block. */
	asmstack=pop();
	asm(MVI1); asm(r0); /* Load r1 with code. */
	asm(SYSI); asm(objNewClosure1Env); /* Create closure from r1 and env (r16) */
	DB("<--wscmSyntaxRules");
}

void compNot (Num flags) {
 Num opcodeStart;
	DB("-->%s", __func__);
	expr = cadr(expr);           /* Compile this expression */
	compExpression(flags & ~TAILCALL);
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
	while (objIsPair(expr)) {
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
	while (objIsPair(expr)) {
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


void compCombination (Num flags) {
 Int operandCount=0;
	DB("-->%s expr=", __func__);
	DBE memDebugDumpObject(expr, stdout);

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
	while (objIsPair(expr)) {
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
 Num opcodeStart, operandCount=0;
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

	/* Compile operand expressions the last of which hopefully evaluates to a list of args.
	   The resulting arguments will be pushed onto the stack and passed to the function.  */
	expr = cdr(expr);
	while (objIsPair(expr)) {
		push (cdr(expr)); /* Push rest */
		expr = car(expr); /* Consider expression  */
		compExpression(flags & ~TAILCALL);
		asm(PUSH0);
		operandCount++;
		expr = pop();
	}

	/* Restore and compile operator expression. */
	expr=pop();
	compExpression(flags & ~TAILCALL);
	asm(MV30); /* Save operator in r3 */

	/* At this point stack has the arguments, the argument-list and r3 has function.
	   Want to transfers the argument-list items from list to the stack with r1 ending up
	   with the argument count.  Initially the argument count is the number of initial
	   non-list arguments to apply.
	*/
	opcodeStart = memStackLength(asmstack);
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
 Num opcodeStart;
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
Num compExpression (Num flags) {
	DB("-->%s expr=", __func__);
	DBE memDebugDumpObject(expr, stdout);
	switch (memObjectType(expr)) {
		case TSYMBOL :
			compVariableReference(flags);
			break;
		case TPAIR :
			if      (srem       == car(expr)) compRem(flags);
			else if (sdefine    == car(expr)) compDefine(flags);
			else if (ssetb      == car(expr)) compSetb(flags);
			else if (slambda    == car(expr)) compLambda(flags);
			else if (smacro     == car(expr)) compMacro(flags);
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
			else if (sprocedurep== car(expr)) compProcedureP(flags);
			else if (snullp     == car(expr)) compNullP(flags);
			else if (spairp     == car(expr)) compPairP(flags);
			else if (svectorp   == car(expr)) compVectorP(flags);
			else if (sstringp   == car(expr)) compStringP(flags);
			else if (sintegerp  == car(expr)) compIntegerP(flags);
			else if (ssymbolp   == car(expr)) compSymbolP(flags);
			else if (sportp     == car(expr)) compPortP(flags);
			else if (seofobjectp== car(expr)) compEOFObjectP(flags);
			else if (sbegin     == car(expr)) compBegin(flags);
			else if (squote     == car(expr)) compQuote();
			else if (sif        == car(expr)) compIf(flags);
			else if (saif       == car(expr)) compAIf(flags);
			else if (scond      == car(expr)) compCond(flags);
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
   r18 -> Expression we're compiling.
   r0  <- Resuling code object (vector of VM opcodes).
*/
Num compCompile (void) {
 Num ret;
	DB ("::%s", __func__);

	CompError = 0; /* Clear error flag. */
	//env = tge;   /* Force evaluation in the global environment */

	asmAsm ( /* Keep track of original expression for debugging. */
		BRA, 8l,
		expr,
		END
	);

	ret = compExpression(0);  /* No compiler flags */
	asm(QUIT); /* Emit the QUIT opcode which exits the VM. */
	asmNewCode();

	DB("  --%s", __func__);
	return ret;
}

#undef DB_MODULE
