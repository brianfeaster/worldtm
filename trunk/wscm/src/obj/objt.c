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
	objNewInt((Int)0xdeadbeefb00b1355l); r01=r00;
	objCopyInteger(); r02=r01;
	objNewInt(-1l); r03=r00;
	objNewInt(LONG_MAX); r04=r00;
	objNewInt(0xdea1f00d); vmPush(r00);
	objNewReal(1.3);  vmPush(r00);
	objNewReal(15.0); vmPush(r00);
}


/* Verify registers and stack contain expected object values.
*/
void TESTVerifyObjects (void) {
	assert(0xdeadbeefb00b1355l == *(Int*)r01);
	assert(0xdeadbeefb00b1355l == *(Int*)r02);
	assert(-1l == *(Int*)r03);
	assert(LONG_MAX == *(Int*)r04);
	assert(3 == memVecStackLength(rstack));
	assert(15.0 == *(Real*)memVecStackObject(rstack, 0));
	assert(1.3 == *(Real*)memVecStackObject(rstack, 1));
	assert(0xdea1f00d == *(Int*)memVecStackObject(rstack, 2));
}


/* Mutate the stack and verify behavior.
*/
void TESTVerifyStackMutation (void) {
	assert(15.0 == *(Real*)vmPop());
	assert(1.3 == *(Real*)vmPop());
	assert(0xdea1f00d == *(Int*)vmPop());
	assert(0 == memVecStackLength(rstack));
}


void TESTDoublyLinkedList (void) {
	/* Create doubly linked list in r04 */
	objNewDoublyLinkedListNode(); r04=r00;
	assert(1 == objDoublyLinkedListLength(r04)); /* Verify length */

	objNewInt(69); /* Set its cargo */
	memVectorSet(r04, 0, r00);

	objNewDoublyLinkedListNode(); r01=r00;
	objDoublyLinkedListAdd(r04, r01); /* Add another node to dllist */

	objNewInt(100); /* Set its cargo */
	memVectorSet(r01, 0, r00);

	objNewDoublyLinkedListNode(); r01=r00;
	objDoublyLinkedListInsert(r04, r01); /* Add another node to dllist */
	assert(3 == objDoublyLinkedListLength(r04)); /* Verify length */

	objNewInt(42);
	memVectorSet(r01, 0, r00);

	/* Verify doubly linked list traversal */
	assert(*(Int*)car(r04) == 69);
	r04 = objDoublyLinkedListNext(r04); assert(*(Int*)car(r04) == 100);
	r04 = objDoublyLinkedListNext(r04); assert(*(Int*)car(r04) == 42);
	r04 = objDoublyLinkedListPrev(r04); assert(*(Int*)car(r04) == 100);
	r04 = objDoublyLinkedListPrev(r04); assert(*(Int*)car(r04) == 69);
	r04 = objDoublyLinkedListPrev(r04); assert(*(Int*)car(r04) == 42);
}


void TESTDump (void) {
	FBInit();
	objDisplay(ofalse, FB);
	objDisplay(otrue, FB);
	objDisplay(onull, FB);
	objDisplay(oeof, FB);
	objDisplay(onullvec, FB);
	objNewInt(69); r03=r00; objDisplay(r00, FB);
	objNewInt(-42); r04=r00; objDisplay(r00, FB);
	objNewSymbol((Str)"donuts", 6); objDisplay(r00, FB);
	r00 = objCons(r03, r04); objDisplay(r00, FB);
	r00 = objCons(r03, onull); objDisplay(r00, FB);
	r00 = objCons(r04, r00); objDisplay(r00, FB);
	r00 = objCons(onull, objCons(onullvec, objCons(ofalse, objCons(otrue, onullstr)))); objDisplay(r00, FB);
	objNewVector(4);
	memVectorSet(r00, 0, ofalse);
	memVectorSet(r00, 1, otrue);
	memVectorSet(r00, 2, onull);
	memVectorSet(r00, 3, onullvec); objDisplay(r00, FB);
	r00 = (Obj)0xdeadbeefl; objDisplay(r00, FB);
	r00 = (Obj)-0xdeadbeefl; objDisplay(r00, FB);

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
	memTypeStringRegister(TDCHAR, (Str)"chr"); /* Register new type ID */
	objDisplayTypeRegister(TDCHAR, objtDisplayTypeDchar); /* Register serializer */

	FBInit();

	r00 = memNewArray(TDCHAR, 2);
	memArraySet(r00, 0, 'a');
	memArraySet(r00, 1, 'z');
	objDisplay(r00, FB);

	FBFinalize("{az}");
}

void TESTobjOrderedSetAdd0102 (void) {
	FBInit();

	r00 = onull; // Initial empty set

	r01 = (Obj)6; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)2; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)4; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)3; objOrderedSetAdd0(r00, r01);

	objDisplay(r00, FB);
	FBFinalize("(#<6> #<4> #<3> #<2>)");
}

void TESTobjOrderedSetSub0102 (void) {
	FBInit();

	r00 = onull; // Initial empty set

	r01 = (Obj)6; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)2; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)4; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)3; objOrderedSetAdd0(r00, r01);

	r01 = (Obj)4;
	objOrderedSetSub0(r00, r01);

	objDisplay(r00, FB);
	FBFinalize("(#<6> #<3> #<2>)");
}

void TESTobjOrderedSetUnion001 (void) {
	FBInit();

	r00 = onull; // Initial empty set
	r01 = (Obj)6; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)4; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)2; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)0; objOrderedSetAdd0(r00, r01);
	r02 = r00;

	r00 = onull; // Initial empty set
	r01 = (Obj)7; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)5; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)3; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)1; objOrderedSetAdd0(r00, r01);

	r01=r02;

	objOrderedSetUnion0(r00, r01);

	objDisplay(r00, FB);
	FBFinalize("(#<7> #<6> #<5> #<4> #<3> #<2> #<1> #<0>)");
}

void TESTobjOrderedSetIntersection001 (void) {
	FBInit();

	r00 = onull; // Initial empty set
	r01 = (Obj)6; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)4; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)2; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)0; objOrderedSetAdd0(r00, r01);
	r02 = r00;

	r00 = onull; // Initial empty set
	r01 = (Obj)7; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)5; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)2; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)1; objOrderedSetAdd0(r00, r01);

	r01=r02;

	objOrderedSetIntersection001();
	objDisplay(r00, FB);

	FBFinalize("(#<2>)");
}
void TESTobjOrderedSetSubtraction001 (void) {
	FBInit();

	r00 = onull; // Set of objects
	r01 = (Obj)6; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)4; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)2; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)0; objOrderedSetAdd0(r00, r01);
	r02 = r00;

	r00 = onull; // Elements to remove
	r01 = (Obj)7; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)5; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)2; objOrderedSetAdd0(r00, r01);
	r01 = (Obj)1; objOrderedSetAdd0(r00, r01);
	r01 = r00;

	r00=r02;

	objOrderedSetSubtract001();
	objDisplay(r00, FB);

	FBFinalize("(#<6> #<4> #<0>)");
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
	TEST(TESTobjOrderedSetAdd0102);
	TEST(TESTobjOrderedSetSub0102);
	TEST(TESTobjOrderedSetUnion001);
	TEST(TESTobjOrderedSetIntersection001);
	TEST(TESTobjOrderedSetSubtraction001);
	return 0;
}
