#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include "obj.h"
#include "vm.h"
#include "mem.h"

#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))



/* Create some objects and assign to registers and
   create a stack with objects pushed on.
*/
void objtCreateObjects (void) {
	objNewInt((Int)0xdeadbeefb00b1355l); r1=r0;
	objCopyInteger(); r2=r1;
	objNewInt(-1); r3=r0;
	objNewInt(LONG_MAX); r4=r0;
	objNewInt(0xdea1f00d); vmPush(r0);
	objNewReal(1.3);  vmPush(r0);
	objNewReal(15.0); vmPush(r0);
}



/* Verify registers and stack contain expected object values.
*/
void objtVerifyObjects (void) {
	assert(0xdeadbeefb00b1355l == *(Int*)r1);
	assert(0xdeadbeefb00b1355l == *(Int*)r2);
	assert(-1 == *(Int*)r3);
	assert(LONG_MAX == *(Int*)r4);
	assert(3 == memStackLength(r1f));
	assert(15.0 == *(Real*)memStackObject(r1f, 0));
	assert(1.3 == *(Real*)memStackObject(r1f, 1));
	assert(0xdea1f00d == *(Int*)memStackObject(r1f, 2));
}



/* Mutate the stack and verify behavior.
*/
void objtVerifyStackMutation (void) {
	assert(15.0 == *(Real*)vmPop());
	assert(1.3 == *(Real*)vmPop());
	assert(0xdea1f00d == *(Int*)vmPop());
	assert(0 == memStackLength(r1f));
}



void objtDoublyLinkedList (void) {

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



int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout, 0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	objInitialize();

	/* Perform a full garbage collection to move module related objects to old
	   heap for easier visual debugging of newly created young heap objects */
	GarbageCollectionMode = 1;
	memGarbageCollect();

	TEST(objtCreateObjects);
	TEST(objtVerifyObjects);
	TEST(objtVerifyStackMutation);
	TEST(objtDoublyLinkedList);

	return 0;
}
