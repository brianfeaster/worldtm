#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include "obj.h"
#include "vm.h"
#include "mem.h"
#include "test.h"


/*******************************************************************************
 TESTS
*******************************************************************************/

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
	objDisplay(ofalse, FB);
	objDisplay(otrue, FB);
	objDisplay(onull, FB);
	objDisplay(oeof, FB);
	objDisplay(onullvec, FB);
	objNewInt(69); r3=r0; objDisplay(r0, FB);
	objNewInt(-42); r4=r0; objDisplay(r0, FB);
	objNewSymbol((Str)"donuts", 6); objDisplay(r0, FB);
	r0 = objCons(r3, r4); objDisplay(r0, FB);
	r0 = objCons(r3, onull); objDisplay(r0, FB);
	r0 = objCons(r4, r0); objDisplay(r0, FB);
	r0 = objCons(onull, objCons(onullvec, objCons(ofalse, objCons(otrue, onullstr)))); objDisplay(r0, FB);
	objNewVector(4);
	memVectorSet(r0, 0, ofalse);
	memVectorSet(r0, 1, otrue);
	memVectorSet(r0, 2, onull);
	memVectorSet(r0, 3, onullvec); objDisplay(r0, FB);
	r0 = (Obj)0xdeadbeefl; objDisplay(r0, FB);
	r0 = (Obj)-0xdeadbeefl; objDisplay(r0, FB);

	FBFinalize("#f#t()#eof#()69-42donuts(69 . -42)(69)(-42 69)(() #() #f #t . )#(#f #t () #())#<deadbeef>#<-deadbeef>");
}


/* Test a serializer can be registered and used to display a new object type
 */
#define TDCHAR 0x70

void objtDisplayTypeDchar (Obj o, FILE *serializer) {
	fwrite("{", 1, 1, serializer);
	fprintf(serializer, CHR, memArrayObject(o, 0));
	fprintf(serializer, CHR, memArrayObject(o, 1));
	fwrite("}", 1, 1, serializer);
}

void TESTDumpSerializeCallback (void) {
	memTypeRegister(TDCHAR); /* Register new type ID */
	objDisplayTypeRegister(TDCHAR, objtDisplayTypeDchar); /* Register serializer */

	FBInit();

	r0 = memNewArray(0x70, 2);
	memArraySet(r0, 0, 'a');
	memArraySet(r0, 1, 'z');
	objDisplay(r0, FB);

	FBFinalize("{az}");
}


int main (int argc, char *argv[]) {
	objInitialize();
	testInitialize();
	/* Perform a full garbage collection to move module related objects to old
	   heap for easier visual debugging of newly created young heap objects */
	GarbageCollectionMode = 1;
	memGarbageCollect();
	TEST(TESTCreateObjects);
	TEST(TESTVerifyObjects);
	TEST(TESTVerifyStackMutation);
	TEST(TESTDoublyLinkedList);
	TEST(TESTDump);
	TEST(TESTDumpSerializeCallback);
	return 0;
}
