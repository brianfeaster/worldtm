#define DEBUG 0
#define DB_MODULE "ASM "
#include "debug.h"
#include <stdarg.h>
#include <string.h>
#include "asm.h"


void asmAsm (Obj o,...) {
 Obj obj;
 va_list ap;
	va_start(ap, o);
	obj = o;
	DB (INDENT0 "::" STR, __func__); 
	while (obj!=END) {
		DB(INDENT1 INT4":"OBJ, memStackLength(asmstack), obj);
		memStackPush(asmstack, obj);
		obj=va_arg(ap, Obj);
	}
	va_end(ap);
	DB (INDENT1 "--" STR, __func__); 
}


void asmCompileAsmstack (Num opcodeStart) {
 Num j, i, opcodeEnd, opcodeWrite, labelCount=0, addrCount=0;
	DB(INDENT0"::%s", __func__);
	memStackPush(stack, r0);
	memStackPush(stack, r1);
	memStackPush(stack, r2);
	memStackPush(stack, r3);
	/* Store for LABELS and opcode addresses (stored one after another).  */
	memNewVector(TVECTOR, 80); r2=r0;
	/* Store for instruction addresses and LABELS */
	memNewVector(TVECTOR, 80); r3=r0;

	/* Remove occurrances of pop_15/1e/1d push_1d/1e/15
	// Debug dump un-assembled code stack.
	memNewVector(TCODE, len=(memStackLength(asmstack)-opcodeStart));
	memcpy(r0, asmstack+ObjSize, len*ObjSize);
	vmDebugDumpCode(r0, stderr);

	opcodeEnd = memStackLength(asmstack)+1;
	opcodeWrite = opcodeStart+1l;
	for (i=opcodeWrite; i<opcodeEnd; i++) {
		printf ("looking "INT"\r\n", i);
		if (POP15 == memVectorObject(asmstack, i)    && POP1E == memVectorObject(asmstack, i+1)  && POP1D == memVectorObject(asmstack, i+2) &&
		    PUSH1D == memVectorObject(asmstack, i+3) && PUSH1E == memVectorObject(asmstack, i+4) && PUSH15 == memVectorObject(asmstack, i+5)) {
			i += 6;
			fprintf (stderr, "\r\nFound it at "INT"\r\n", opcodeWrite);
			vmDebugDumpCode(asmstack, stderr);
			exit(0);
		} else {
			memVectorSet(asmstack, opcodeWrite++, memVectorObject(asmstack, i));
		}
	}
	// 'Pop' unused opcodes off stack leaving the freshly compiled code behind.
	*(Obj*)asmstack = asmstack + 8 * (opcodeWrite-1);
	 */

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
			DB(INDENT1 HEX4" label "OBJ" %s", opcodeWrite, r0, memVectorObject(asmstack, i+1));
			/* Store label value. */
			memVectorSet(r2, labelCount++, memVectorObject(asmstack, ++i));
			/* Store opcode address. */
			memVectorSet(r2, labelCount++, (Obj)opcodeWrite);
		} else if (r0 == ADDR) {
			DB(INDENT1 HEX4" addr "OBJ, opcodeWrite, r0);
			/* Store label value. */
			memVectorSet(r3, addrCount++, memVectorObject(asmstack, ++i));
			/* Store opcode address. */
			memVectorSet(r3, addrCount++, (Obj)opcodeWrite);
			/* TODO Store a 0 for the branch offset for now (could just as well
			   just increment the opcodeWrite address without mutating the value. */
			memVectorSet(asmstack, opcodeWrite++, (Obj)0);
		} else {
			//DB(INDENT1 "%x opcode %08x", opcodeWrite, r0);
			memVectorSet(asmstack, opcodeWrite++, r0);
		}
	}

	/* 'Pop' unused opcodes off stack leaving the freshly compiled code
	   behind. */
	*(Obj*)asmstack = asmstack + 8 * (opcodeWrite-1);

	/* Traverse address entries and resolve the branch offsets.
	*/
	for (i=0l; i<addrCount; i+=2l) {
		r0 = memVectorObject(r3, i); /* Consider addr label. */
		for (j=0l; j<labelCount; j+=2l) {
			if (r0 == memVectorObject(r2, j)) {
				/* Found label for this address. */
				memVectorSet(asmstack,
				             (Num)memVectorObject(r3, i+1l),
				             (Obj)(8l * (memVectorObject(r2, j+1l)
				                        - memVectorObject(r3, i+1l)-1l)));
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
	DB(INDENT1"--%s", __func__);
}


void asmNewCode (void) {
 Num len;
	DB("::%s", __func__);
	memNewVector(TCODE, len=memStackLength(asmstack));
	memcpy(r0, asmstack+8, len*8);
	/* Reset assembly stack by setting the first entry to
	   the stack itself (points to itself). */
	*(Obj*)asmstack = asmstack;
	DBE vmDebugDumpCode(r0, stderr);
	DB("  --%s", __func__);
}


/* Called by obj.c */
void asmInitialize (Func scheduler, Func preGC, Func postGC, void(*vmObjDumper)(Obj, FILE*)) {
 static Int shouldInitialize=1;
	if (shouldInitialize) {
		shouldInitialize=0;
		vmInitialize(scheduler, preGC, postGC, vmObjDumper);
		LABEL = &LABEL;
		ADDR = &ADDR;
		END = &END;
	}
}


#undef DB_MODULE
