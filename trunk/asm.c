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
	DB (TAB0 "::" STR, __func__); 
	while (obj!=END) {
		DB(TAB1 INT4":"OBJ, memStackLength(asmstack), obj);
		memStackPush(asmstack, obj);
		obj=va_arg(ap, Obj);
	}
	va_end(ap);
	DB (TAB1 "--" STR, __func__); 
}


void asmCompileAsmstack (Int opcodeStart) {
 Int j, i, opcodeEnd, opcodeWrite, labelCount=0, addrCount=0;
	DB(TAB0"::%s", __func__);
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
	opcodeWrite = opcodeStart+1l;
	for (i=opcodeWrite; i<opcodeEnd; i++) {
		r0 = memVectorObject(asmstack, i);
		if (r0 == LABEL) {
			DB(TAB1 HEX4" label "OBJ" %s", opcodeWrite, r0, memVectorObject(asmstack, i+1));
			/* Store label value. */
			memVectorSet(r2, labelCount++, memVectorObject(asmstack, ++i));
			/* Store opcode address. */
			memVectorSet(r2, labelCount++, (Obj)opcodeWrite);
		} else if (r0 == ADDR) {
			DB(TAB1 HEX4" addr "OBJ, opcodeWrite, r0);
			/* Store label value. */
			memVectorSet(r3, addrCount++, memVectorObject(asmstack, ++i));
			/* Store opcode address. */
			memVectorSet(r3, addrCount++, (Obj)opcodeWrite);
			/* TODO Store a 0 for the branch offset for now (could just as well
			   just increment the opcodeWrite address. */
			memVectorSet(asmstack, opcodeWrite++, (Obj)0);
		} else {
			//DB(TAB1 "%x opcode %08x", opcodeWrite, r0);
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
	DB(TAB1"--%s", __func__);
}


void asmNewCode (void) {
 Int len;
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
void asmInitialize (Func intHandler, Func preGC, Func postGC, void(*vmObjDumper)(Obj, FILE*)) {
 static Int shouldInitialize=1;
	if (shouldInitialize) {
		shouldInitialize=0;
		vmInitialize(intHandler, preGC, postGC, vmObjDumper);
		LABEL = &LABEL;
		ADDR = &ADDR;
		END = &END;
	}
}


#undef DB_MODULE
