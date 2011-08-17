#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/mman.h> /* mmap() */
#include "mem.h"

#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))

Num memHeapLength (Num h);

Obj r0, r1, r2, r3, r4, r1f;

Num TSYMBOL =0x01;
Num TINTEGER=0x02;
Num TSTRING =0x03;

Num TPAIR   =0x80;
Num TVECTOR =0x81;

void push (Obj o) { memStackPush(r1f, o); }
Obj   pop (Obj o) { return memStackPop(r1f); }



/* Verify the expected number of objects in each heap
 */
Num memtVerifyHeapLengths (Num staticLen, Num oldLen, Num youngLen, Num newLen) {
 Num ret;
	ret = 
		memHeapLength(0) == staticLen &&
		memHeapLength(1) == oldLen &&
		memHeapLength(2) == youngLen &&
		memHeapLength(3) == newLen;
	if (!ret) {
		printf ("\nstatic:"NUM"  old:"NUM"  young:"NUM"  new:"NUM"\n", memHeapLength(0), memHeapLength(1), memHeapLength(2), memHeapLength(3));
	}
	return ret;
}



/* Pass a pre-garbage collection callback.
*/
Num GarbageCollectCalled=0;
void memtPreGarbageCollect (void) {
	GarbageCollectCalled=1;
}



/* Verify expected variable sizes
*/
void memtSizeof (void) {
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
}



/* Verify mmap returns a memory block size of 4Kb/#x1000 bytes.
   It does this by differencing the addresses of two consecutive calls.
*/
void memtMmap (void) {
 u8 *p1, *p2;
	p1 = mmap(0x0, 1, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	p2 = mmap(0x0, 1, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	assert(labs(p1 - p2) == BLOCK_BYTE_SIZE);
	munmap(p1, 1);
	munmap(p2, 1);
}



/* Verify I can translate "array" and "vector" object lengths to the object
   byte size on IA64.  The size includes the object type descriptor field.
   TODO This fails on IA32 and Commodore 64s.
*/
void memtObjectSize (void) {
	assert(memArrayLengthToObjectSize(0)==8);
	assert(memArrayLengthToObjectSize(1)==16);
	assert(memArrayLengthToObjectSize(8)==16);
	assert(memArrayLengthToObjectSize(9)==24);
	assert(memArrayLengthToObjectSize(16)==24);
	assert(memArrayLengthToObjectSize(17)==32);
	assert(memArrayLengthToObjectSize(24)==32);
	assert(memArrayLengthToObjectSize(25)==40);

	assert(memVectorLengthToObjectSize(0)==8);
	assert(memVectorLengthToObjectSize(1)==16);
	assert(memVectorLengthToObjectSize(2)==24);
}


/* Create some objects and push/pop them to the stack.
*/
void memtVerifyObjectCreationAndCollection (void) {
 Chr i, j;
	assert(memtVerifyHeapLengths(0, 0, 1, 0));

	r1 = memNewArray(TSYMBOL, 256);
	r2 = memNewArray(TINTEGER, 4);
	r3 = memNewArray(TINTEGER, 4);
	r4 = memNewVector(TPAIR, 2);

	assert(memtVerifyHeapLengths(0, 0, 5, 0));

	push(r1);
	push(r2);
	push(r3);
	push(r4);
	push(r1f);
	assert(memStackLength(r1f) == 5);

	memVectorSet(r4, 0, r1);
	memVectorSet(r4, 1, r2);

	for (i=0; i<16; i++) {
		r0 = memNewArray(TINTEGER, 16);
		push(r0);
		for (j=0; j<16; j++) ((Chr*)r0)[j] = (Chr)(j+i);
	}
	assert(memStackLength(r1f) == 21);


	for (i=0; i<16; i++) memStackPop(r1f);
	assert(memtVerifyHeapLengths(0, 0, 21, 0));

	memGarbageCollect();
	assert(memStackLength(r1f) == 5);

	/* Although I popped the stack of all the integers, register 0 still
	   points to the last integer created */
	assert(memtVerifyHeapLengths(0, 0, 6, 0));

	/* Clear the registers and force a full garbage collection to clean all heaps */
	r0=r1=r2=r3=r4=r1f=0;
	GarbageCollectionMode = 1;
	memGarbageCollect();
	assert(memtVerifyHeapLengths(0, 0, 0, 0));
}



/* Create a finalizer object then set the callback function and protected
   object; in this case an integer.  Verify the integer is passed to the
   C callback function.
*/
Num MyFinalizerFlag = 0;

void callbackFinalizerFunction (Obj o) {
	assert(MyFinalizerFlag==0);
	assert(*(Int*)o = 0xefbeadde);
	MyFinalizerFlag=1;
}

void memtFinalizer (void) {

	/* The integer */
	r1 = memNewArray(TINTEGER, 4);
	*(Int*)r1 = 0xefbeadde;

	/* The finalizer */
	r0 = memNewFinalizer();
	memVectorSet(r0, 0, callbackFinalizerFunction);
	memVectorSet(r0, 1, r1);

	/* Check the finalizer is left alone during a garbage collection */
	memGarbageCollect();
	assert(MyFinalizerFlag==0);

	/* Unreference the finalizer and integer and force a full
	   garbage collection so both are collected  */
	r0 = r1 = 0;
	GarbageCollectionMode = 1;
	memGarbageCollect();

	assert(1 == MyFinalizerFlag);
}



/* Verify a large array and large vector object can be created and collected.
*/
void memtNewLargeVectorObject (void) {
	r1 = memNewArray(TSTRING, 0x1000000 - 16); /* 16Mb sized array object */
	r0 = memNewVector(TVECTOR,  0x1000000/8 - 2); /* 16Mb sized vector object */

	/* Verify the two big object survive in the young heap after a GC */
	assert(memtVerifyHeapLengths(0, 0, 2, 0));
	memGarbageCollect();
	assert(memtVerifyHeapLengths(0, 0, 2, 0));

	/* Verify the two big object move to the old heap after a full GC */
	GarbageCollectionMode = 1;
	memGarbageCollect();
	assert(memtVerifyHeapLengths(0, 2, 0, 0));

	/* Delete all objects */
	r0 = r1 = 0;
	GarbageCollectionMode = 1;
	memGarbageCollect();
	assert(memtVerifyHeapLengths(0, 0, 0, 0));
}



/* Allocate a simple object repeatedly until the garbage collector is triggered.
   One object should exist after collecting.
*/
void memtAutomaticGarbageCollect (void) {
	/* Called 0x40000 times */
	for (GarbageCollectCalled=0; !GarbageCollectCalled ; memNewArray(TSYMBOL, 8));
	/* Better be just one object in the heap, the one that triggered the GC */
	assert(memtVerifyHeapLengths(0, 0, 1, 0));

	/* Delete all objects */
	r0=0;
	GarbageCollectionMode = 1;
	memGarbageCollect();
	assert(memtVerifyHeapLengths(0, 0, 0, 0));
}



int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	memInitialize(memtPreGarbageCollect, 0);

	/* Register root set object.  These are the "registers" or
	   machine's global variables */
	memRegisterRoot(r0);
	memRegisterRoot(r1);
	memRegisterRoot(r2);
	memRegisterRoot(r3);
	memRegisterRoot(r4);
	memRegisterRoot(r1f);

	r1f = memNewStack(); /* Create the stack for the machine */

	memRegisterType(TSYMBOL, "sym");
	memRegisterType(TINTEGER, "int");
	memRegisterType(TPAIR, "pair");
	memRegisterType(TSTRING, "str");
	memRegisterType(TVECTOR, "vec");
	memObjStringSet(callbackFinalizerFunction);

	TEST(memtSizeof);
	TEST(memtMmap);
	TEST(memtObjectSize);
	TEST(memtVerifyObjectCreationAndCollection);
	TEST(memtFinalizer);
	TEST(memtNewLargeVectorObject);
	TEST(memtAutomaticGarbageCollect);

	//memDebugDumpAll(stdout);
	return 0;
}
