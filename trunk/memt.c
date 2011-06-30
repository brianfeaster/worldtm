#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/mman.h>
#include "mem.h"

void memAssertions() {
 u8 *mmapPointer1, *mmapPointer2;
	// Verify typedefs are the correct byte size.
	assert(sizeof(u8)==1);
	assert(sizeof(u16)==2);
	assert(sizeof(u32)==4);
	assert(sizeof(u64)==8);

	assert(sizeof(s8)==1);
	assert(sizeof(s16)==2);
	assert(sizeof(s32)==4);
	assert(sizeof(s64)==8);

	assert(sizeof(r32)==4);
	assert(sizeof(r64)==8);
	assert(sizeof(r128)==16);

	// Verify size of integer constants.
	assert(sizeof(1)==4);
	assert(sizeof(1l)==8);

	// Does mmap return #x1000 (4096) byte chunks?
	mmapPointer1 = mmap(0x0, 1, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	mmapPointer2 = mmap(0x0, 1, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	//printf ("%p - %p = %lx\n", mmapPointer1, mmapPointer2, abs(mmapPointer1 - mmapPointer2));
	assert(abs(mmapPointer1 - mmapPointer2) == 4096);
	munmap(mmapPointer1, 1);
	munmap(mmapPointer2, 1);

	// Verify I can translate array object lengths to the full byte size. */
	assert(memArrayLengthToObjectSize(0)==8);
	assert(memArrayLengthToObjectSize(1)==16);
	assert(memArrayLengthToObjectSize(8)==16);
	assert(memArrayLengthToObjectSize(9)==24);
	assert(memArrayLengthToObjectSize(16)==24);
	assert(memArrayLengthToObjectSize(17)==32);
	assert(memArrayLengthToObjectSize(24)==32);
	assert(memArrayLengthToObjectSize(25)==40);
}

void myFinalizer (Obj o) {
	printf ("myFinalizer called with arg"OBJ"\n", o);
}

extern int GarbageCollectionMode;

int main (int argc, char *argv[]) {
 int i, j;

	// If any arguments passed to test unit, do more fun things.
	if (argc==2) {
		// Force failure if -f arguement passed
		if (!strcmp(argv[1], "-f")) return -1;
	}

	setbuf(stdout,0);
	memInitialize(NULL, NULL);

	memAssertions();

	memNewArray(TSYMBOL, 256); r1=r0;
	memNewArray(TINTEGER, 4); r2=r0;
	memNewArray(TINTEGER, 4); r3=r0;
	memNewVector(TPAIR, 2); r4=r0;
	memNewStack(); rf=r0;

	memStackPush(rf, r1);
	memStackPush(rf, r2);
	memStackPush(rf, r3);
	memStackPush(rf, r4);
	memStackPush(rf, rf);
	memVectorSet(r4, 0, r1);
	memVectorSet(r4, 1, r2);

	for (i=0; i<16; i++) {
		memNewArray(TINTEGER, 16);
		memStackPush(rf, r0);
		for (j=0; j<16; j++) ((char*)r0)[j] = j+i;
	}

	memNewFinalizer();
	memVectorSet(r0, 0, myFinalizer);
	memVectorSet(r0, 1, r3);

	memDebugDumpAll(stdout);
	memGarbageCollect();
	memDebugDumpAll(stdout);
	r0=0;
	memGarbageCollect();
	GarbageCollectionMode = 1;
	memGarbageCollect();
	memDebugDumpAll(stdout);

	memDebugDumpObject(rf, stdout);
	memStackPop(rf);
	(*(Obj**)rf)++; // This is illegal but I want to verify that popping clears the object on the stack.
	memDebugDumpObject(rf, stdout);
	assert(memStackObject(rf, 0)==0);

	goto ret;

	memVectorSet(r9, 1, r2);
	memVectorSet(r9, 2, r3);
	memVectorSet(r9, 3, r4);
	memNewFinalizer  ();  r10 = r0; *(Obj*)r10 = r2;// (cleaner*)(), int fd
	memNewPointer ();  r11 = r0; ((Obj*)r11)[1]=r9;((Obj*)r11)[0]=r9+4;
	memStackPush(rf, r1);
	memStackPush(rf, r2);
	memStackPush(rf, r3);
	memStackPush(rf, r8);
	memStackPush(rf, r9);
//	memGarbageCollect();
	memNewVector (TPAIR, 2);
	memStackPush(rf, r0);
	//GarbageCollectionMode = GC_MODE_OLD; memGarbageCollect();
l:
	for (i=0; i<16; i++) {
		memNewArray(TINTEGER, 16);
		memStackPush(rf, r0);
		for (j=0; j<16; j++) ((char*)r0)[j] = j+i;
	}
	//for (i=0; i<16; i++) memStackPop(rf);
//	GarbageCollectionMode = GC_MODE_OLD;
//	memGarbageCollect();
//	GarbageCollectionMode = GC_MODE_OLD;
//	memGarbageCollect();
//	memNewArray(TSYMBOL, 128);
	//memDebugDumpHeapStructures();
goto  l;
ret:
memGarbageCollect();
	fprintf (stderr,"\n");
	return 0;
}
