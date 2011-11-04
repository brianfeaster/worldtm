#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include "obj.h"
#include "vm.h"
#include "mem.h"

/* A character file buffer and the functions that print to it
*/
FILE *FB;
char *FBBuff=NULL;

/* Initialize character file buffer */
void FBInit (void) {
 static Num size;
	FB = open_memstream(&FBBuff, &size);
	assert(NULL != FB);
}

/* Dump character file buffer's contents.  Finalize related objects. */
void FBDump () {
	fflush(FB);
	fclose(FB);
	fprintf(stderr, FBBuff);
	free(FBBuff);
}

/* Compare character file buffer's contents with string argument. Finalize related objects. */
void FBFinalize (char *goldenString) {
 Num res;
	fflush(FB);
	res = (Num)strcmp(FBBuff, goldenString);
	if (res) fprintf(stderr, "\nReceived [%s]\nExpected [%s] ", FBBuff, goldenString);
	assert(0 == res);
	fclose(FB);
	free(FBBuff);
}



/*******************************************************************************
 TESTS
*******************************************************************************/
#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))

/* Create some objects and assign to registers and
   create a stack with objects pushed on.
*/
void TESTCreateObjects (void) {
	objNewInt((Int)0xdeadbeefb00b1355l); r1=r0;
	objCopyInteger(); r2=r1;
	objNewInt(-1l); r3=r0;
	objNewInt(LONG_MAX); r4=r0;
	objNewInt(0xdea1f00d); vmPush(r0);
	objNewReal(1.3);  vmPush(r0);
	objNewReal(15.0); vmPush(r0);
}



/* Verify registers and stack contain expected object values.
*/
void TESTVerifyObjects (void) {
	assert(0xdeadbeefb00b1355l == *(Int*)r1);
	assert(0xdeadbeefb00b1355l == *(Int*)r2);
	assert(-1l == *(Int*)r3);
	assert(LONG_MAX == *(Int*)r4);
	assert(3 == memStackLength(rf));
	assert(15.0 == *(Real*)memStackObject(rf, 0));
	assert(1.3 == *(Real*)memStackObject(rf, 1));
	assert(0xdea1f00d == *(Int*)memStackObject(rf, 2));
}



/* Mutate the stack and verify behavior.
*/
void TESTVerifyStackMutation (void) {
	assert(15.0 == *(Real*)vmPop());
	assert(1.3 == *(Real*)vmPop());
	assert(0xdea1f00d == *(Int*)vmPop());
	assert(0 == memStackLength(rf));
}



void TESTDoublyLinkedList (void) {
	/* Create doubly linked list in r4 */
	objNewDoublyLinkedListNode(); r4=r0;
	assert(1 == objDoublyLinkedListLength(r4)); /* Verify length */

	objNewInt(69); /* Set its cargo */
	memVectorSet(r4, 0, r0);

	objNewDoublyLinkedListNode(); r1=r0;
	objDoublyLinkedListAdd(r4, r1); /* Add another node to dllist */

	objNewInt(100); /* Set its cargo */
	memVectorSet(r1, 0, r0);

	objNewDoublyLinkedListNode(); r1=r0;
	objDoublyLinkedListInsert(r4, r1); /* Add another node to dllist */
	assert(3 == objDoublyLinkedListLength(r4)); /* Verify length */

	objNewInt(42);
	memVectorSet(r1, 0, r0);

	/* Verify doubly linked list traversal */
	assert(*(Int*)car(r4) == 69);
	r4 = objDoublyLinkedListNext(r4); assert(*(Int*)car(r4) == 100);
	r4 = objDoublyLinkedListNext(r4); assert(*(Int*)car(r4) == 42);
	r4 = objDoublyLinkedListPrev(r4); assert(*(Int*)car(r4) == 100);
	r4 = objDoublyLinkedListPrev(r4); assert(*(Int*)car(r4) == 69);
	r4 = objDoublyLinkedListPrev(r4); assert(*(Int*)car(r4) == 42);
}


void TESTDump (void) {
	FBInit();
	objDump(ofalse, FB);
	objDump(otrue, FB);
	objDump(onull, FB);
	objDump(onullvec, FB);
	objDump(oeof, FB);
	objNewInt(69); objDump(r0, FB);
	objNewInt(-69); objDump(r0, FB);
	objNewSymbol((Str)"donuts", 6); objDump(r0, FB);
	FBFinalize("#f#t()#()#eof69-69donuts");
}


int main (int argc, char *argv[]) {
	setbuf(stdout, 0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	objInitialize();

	/* Perform a full garbage collection to move module related objects to old
	   heap for easier visual debugging of newly created young heap objects */
	GarbageCollectionMode = 1;
	memGarbageCollect();

	TEST(TESTCreateObjects);
	TEST(TESTVerifyObjects);
	TEST(TESTVerifyStackMutation);
	TEST(TESTDoublyLinkedList);
	TEST(TESTDump);

	return 0;
}
