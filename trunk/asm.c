#define DEBUG 0
#define DB_DESC "ASM "
#include "debug.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h> // exit()
#include "asm.h"
#include "vm.h"
#include "mem.h"

#define TVECTOR 0x81l

Obj LABEL;/* Branch symbol. */
Obj ADDR; /* Branch opcode offset symbol. */
Obj END;  /* Sentinel for assembly programs.  Must be last assembly opcode. */

void asmAsm (Obj o, ...) {
 Obj obj;
 va_list ap;
	va_start(ap, o);
	obj = o;
	DBBEG ();
	while (obj!=END) {
		DB(INT4":"OBJ, memStackLength(rasmstack), obj);
		memStackPush(rasmstack, obj);
		obj = va_arg(ap, Obj);
	}
	va_end(ap);
	DBEND ();
}

void asmSkipRedundantPopsAndPushes (Num opcodeStart) {
 Num len;
 Num opcodeEnd;
 Num opcodeWrite;
 Num i;
	/* Remove occurrances of pop_15/1e/1d push_1d/1e/15 */
	// Debug dump un-assembled code stack.
	memNewVector(TCODE, len=(memStackLength(rasmstack)-opcodeStart));
	memcpy(r0, rasmstack+ObjSize, len*ObjSize);
	vmDebugDumpCode(r0, stderr);

	opcodeEnd = memStackLength(rasmstack)+1;
	opcodeWrite = opcodeStart+1l;
	for (i=opcodeWrite; i<opcodeEnd+6; i++) {
		printf ("looking "INT"\r\n", i);
		if (POP19 == memVectorObject(rasmstack, i) &&
		    POP1B == memVectorObject(rasmstack, i+1) &&
		    POP1A == memVectorObject(rasmstack, i+2) &&
		    PUSH1A == memVectorObject(rasmstack, i+3) &&
		    PUSH1B == memVectorObject(rasmstack, i+4) &&
		    PUSH19 == memVectorObject(rasmstack, i+5)) {
			i += 6;
			fprintf (stderr, "\r\nFound it at "INT"\r\n", opcodeWrite);
			vmDebugDumpCode(rasmstack, stderr);
			exit(0);
		} else {
			memVectorSet(rasmstack, opcodeWrite++, memVectorObject(rasmstack, i));
		}
	}
	// 'Pop' unused opcodes off stack leaving the freshly compiled code behind.
	*(Obj*)rasmstack = rasmstack + 8 * (opcodeWrite-1);

}

void asmCompileAsmstack (Num opcodeStart) {
 Num j, i, opcodeEnd, opcodeWrite, labelCount=0, addrCount=0;
	DB("::%s", __func__);
	memPush(r0);
	memPush(r1);
	memPush(r2);
	memPush(r3);
	/* Store for LABELS and opcode addresses (stored one after another).  */
	memNewVector(TVECTOR, 80); r2=r0;
	/* Store for instruction addresses and LABELS */
	memNewVector(TVECTOR, 80); r3=r0;

	/* Compress stack of opcodes onto itself moving the label and address info
	   to their respective lists.  Iterating over the stack but treating it
	   like a vector thus the shifting by one (skipping over the stack objects
	   pointer.
	*/
	opcodeEnd = memStackLength(rasmstack)+1;
	opcodeWrite = opcodeStart;
	for (i=opcodeWrite; i<opcodeEnd; i++) {
		r0 = memVectorObject(rasmstack, i);
		if (r0 == LABEL) {
			DB(HEX4" label "OBJ" %s", opcodeWrite, r0, memVectorObject(rasmstack, i+1));
			/* Store label value. */
			memVectorSet(r2, labelCount++, memVectorObject(rasmstack, ++i));
			/* Store opcode address. */
			memVectorSet(r2, labelCount++, (Obj)opcodeWrite);
		} else if (r0 == ADDR) {
			DB(HEX4" addr "OBJ, opcodeWrite, r0);
			/* Store label value. */
			memVectorSet(r3, addrCount++, memVectorObject(rasmstack, ++i));
			/* Store opcode address. */
			memVectorSet(r3, addrCount++, (Obj)opcodeWrite);
			/* TODO Store a 0 for the branch offset for now (could just as well
			   just increment the opcodeWrite address without mutating the value. */
			memVectorSet(rasmstack, opcodeWrite++, (Obj)0);
		} else {
			//DB("%x opcode %08x", opcodeWrite, r0);
			memVectorSet(rasmstack, opcodeWrite++, r0);
		}
	}

	/* 'Pop' unused opcodes off stack leaving the freshly compiled code
	   behind. */
	*(Obj*)rasmstack = rasmstack + 8 * (opcodeWrite-1);

	/* Traverse address entries and resolve the branch offsets.
	*/
	for (i=0l; i<addrCount; i+=2l) {
		r0 = memVectorObject(r3, i); /* Consider addr label. */
		for (j=0l; j<labelCount; j+=2l) {
			if (r0 == memVectorObject(r2, j)) {
				/* Found label for this address. */
				memVectorSet(rasmstack,
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
	r3 = memPop();
	r2 = memPop();
	r1 = memPop();
	r0 = memPop();
	DB("--%s", __func__);
}


void asmNewCode (void) {
 Num len;
	DB("::%s", __func__);
	memNewVector(TCODE, len=memStackLength(rasmstack));
	memcpy(r0, rasmstack+8, len*8);
	/* Reset assembly stack by setting the first entry to
	   the stack itself (points to itself). */
	*(Obj*)rasmstack = rasmstack;
	DBE vmDebugDumpCode(r0, stderr);
	DB("  --%s", __func__);
}


void asmInitialize (void) {
 static Int shouldInitialize=1;
	DB("::"STR, __func__);
	if (shouldInitialize) {
		DB("  Activating module...");
		shouldInitialize=0;
		vmInitialize(0, 0);
		LABEL = &LABEL;
		ADDR = &ADDR;
		END = &END;
		memNewStack(); rasmstack = r0;
	} else {
		DB("  Module already activated");
	}
	DB("  --"STR, __func__);
}

#undef DB_DESC
#undef DEBUG
