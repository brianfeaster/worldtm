#define DEBUG 0
#define DB_MODULE "ASM "
#include "debug.h"
#include <stdarg.h>
#include <string.h>
#include "asm.h"

/* Push passed opcodes to assembly stack.  The final opcode must be QUIT.
*/
void asmAsm (Obj o,...) {
 Obj obj;
 va_list ap;
	va_start(ap, o);
	obj = o;
DB("asmAsm: %04d:%08x", memStackLength(asmstack), obj);
	while (obj!=END) {
		memStackPush(asmstack, obj);
		obj=va_arg(ap, Obj);
DB("asmAsm: %04d:%08x", memStackLength(asmstack), obj);
	}
	va_end(ap);
}

/*
   Inline assemble the asmstack.  For now it just takes care of
	'label' and 'address' assembly opcodes.  They refer to branch locations
	and branch points in the code.
*/ 
void asmCompile (int opcodeStart) {
 int j, i, opcodeEnd, opcodeWrite, labelCount=0, addrCount=0;
DB("-->%s", __func__);
	memStackPush(stack, r0);
	memStackPush(stack, r1);
	memStackPush(stack, r2);
	memStackPush(stack, r3);
	/* Store for LABELS and opcode addresses (stored one after another).  */
	memNewVector(TVECTOR, 80); r2=r0;
	/* Store for instruction addresses and LABELS */
	memNewVector(TVECTOR, 80); r3=r0;
	/* Compress stack of opcodes onto itself moving the label and address info
	   to their respective lists.  Iterating over the stack but treating it
	   like a vector thus the shifting by one (skipping over the stack objects
	   pointer.
	*/
	opcodeEnd = memStackLength(asmstack)+1;
	opcodeWrite = opcodeStart+1;
	for (i=opcodeWrite; i<opcodeEnd; i++) {
		r0 = memVectorObject(asmstack, i);
		if (r0 == LABEL) {
			DB("%x label %08x %s", opcodeWrite, r0, memVectorObject(asmstack, i+1));
			/* Store label value. */
			memVectorSet(r2, labelCount++, memVectorObject(asmstack, ++i));
			/* Store opcode address. */
			memVectorSet(r2, labelCount++, (Obj)opcodeWrite);
		} else if (r0 == ADDR) {
			DB("%x addr %08x", opcodeWrite, r0);
			/* Store label value. */
			memVectorSet(r3, addrCount++, memVectorObject(asmstack, ++i));
			/* Store opcode address. */
			memVectorSet(r3, addrCount++, (Obj)opcodeWrite);
			/* Store a 0 for the branch offset for now (could just as well
			   just increment the opcodeWrite address. */
			memVectorSet(asmstack, opcodeWrite++, (Obj)0);
		} else {
			//DB("%x opcode %08x", opcodeWrite, r0);
			memVectorSet(asmstack, opcodeWrite++, r0);
		}
	}

	/* 'Pop' unused opcodes off stack leaving the freshly compiled code
	   behind. */
	*(Obj*)asmstack = asmstack + 4 * (opcodeWrite-1);

	/* Traverse address entries and resolve the branch offsets.
	*/
	for (i=0; i<addrCount; i+=2) {
		r0 = memVectorObject(r3, i); /* Consider addr label. */
		for (j=0; j<labelCount; j+=2) {
			if (r0 == memVectorObject(r2, j)) {
				/* Found label for this address. */
				memVectorSet(asmstack,
				             (int)memVectorObject(r3, i+1),
				             (Obj)(4 * ((memVectorObject(r2, j+1)
				                         - memVectorObject(r3, i+1)-1))));
				break;
			}
		}
		if (j>=labelCount) {
			fprintf(stderr, "ERROR: %s: Can't find label [%s].", __func__, r0);
		}
	}
	r3 = memStackPop(stack);
	r2 = memStackPop(stack);
	r1 = memStackPop(stack);
	r0 = memStackPop(stack);
DB("<--%s", __func__);
}

/* Create code object from stack of opcodes.  Since stack and asmstack
   objects are really just vectors, it's just a matter of copying all the
   objects from the stack over to the asmstack vector.  Creates code object
   in r0 based on assembly stack (r1a).
*/
void asmNewCode (void) {
 int len;
	memNewVector(TCODE, len=memStackLength(asmstack));
	memcpy(r0, asmstack+4, len*4);
	// Reset assembly stack by setting the first entry to
	// the stack itself (points to itself).
	*(Obj*)asmstack = asmstack;
}

void asmInitialize (fp intHandler, fp preGC, fp postGC, fp1 objDumper) {
 static int shouldInitialize=1;
	if (shouldInitialize) {
		shouldInitialize=0;
		vmInitialize(intHandler, preGC, postGC, objDumper);
		LABEL = &LABEL;
		ADDR = &ADDR;
		END = &END;
	}
}

/* Debugging: Syscalls.
*/
static void asmdisplayInteger (void) { printf ("%d", r1); }
static void asmdisplayString (void) { printf ("%s", r1); }

int asmmain (void) {
	setbuf(stdout, NULL);
	asmInitialize(0, 0, 0, 0);

	asmAsm(
		MVI1, "\r\n›1mWelcome to World!›m\r\n",
		SYSI, asmdisplayString,
		MVI0, (Obj)0,
		LABEL, "main",
		MV10,
		LABEL, "loop", SYSI, asmdisplayInteger,
		ADDI1, (Obj)1,
		BEQI1, (Obj)10, ADDR, "cont",
		BRA, ADDR, "loop",
		LABEL, "cont",  MVI1, "\r\n",
		SYSI, asmdisplayString,
		ADDI0, (Obj)1,
		BNEI0, (Obj)10, ADDR, "main",
		LABEL, "done",
		QUIT,
		END
	);
	//memDebugObjectDump(asmstack);
	asmCompile(0);
	asmNewCode();
	vmDebugDump();
	code=r0;
	vmRun();
	return 0;
}
