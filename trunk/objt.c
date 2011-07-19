#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include "debug.h"
#include "obj.h"


void helloWorld (void) {printf ("\nHello world!\n");}
void displayInteger$0 (void) {printf ("%08x", *(s32*)r0); }
void displayInteger$1 (void) {printf ("%08x", *(s32*)r1); }
void displayString$0  (void) {write (1, r0, memObjectLength(r0)); }
void displayString$1  (void) {write (1, r1, memObjectLength(r1)); }
void displayCString   (void) {printf (r0); }

/* Create some objects and assign to registers and
   create a stack with objects pushed on.
*/
void objTestCreateObjects (void) {
	objNewInt(0xdeadbeefb00b1355l); r1=r0;
	objCopyInteger(); r2=r1;
	objNewInt(-1); r3=r0;
	objNewInt(LONG_MAX); r4=r0;
	memNewStack(); r1f=r0;
	objNewInt(0xdea1f00d); memStackPush (r1f, r0);
	objNewReal(1.3);  memStackPush (r1f, r0);
	objNewReal(15.0); memStackPush (r1f, r0);
}

/* Verify registers and stack contain expected object values.
*/
void objTestVerifyObjects (void) {
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
void objTestVerifyStackMutation (void) {
	assert(15.0 == *(Real*)memStackPop(r1f));
	assert(1.3 == *(Real*)memStackPop(r1f));
	assert(0xdea1f00d == *(Int*)memStackPop(r1f));
	assert(0 == memStackLength(r1f));
}

void objTestDoublyLinkedList (void) {

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
	// If any arguments passed to test unit, do more fun things.
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout, 0);
	memInitialize(0, 0);
	objInitialize(0);

	objTestCreateObjects();
	objTestVerifyObjects();

	memGarbageCollect();
	objTestVerifyObjects();

	objTestVerifyStackMutation();

	objTestDoublyLinkedList();

	goto done;
	//memDebugDumpHeapStructures();

	/* Assemble a new program. */
	asmAsm(
		SYSI, helloWorld,
		MVI1, r1,
		SYSI, objCopyInteger,
		SYSI, displayInteger$0,
		MVI0, "\n+",
		SYSI, displayCString,
		MVI0, r2,
		SYSI, displayInteger$0,
		ADD10,
		MVI0, "\n=",
		SYSI, displayCString,
		SYSI, displayInteger$1,
		LABEL, "top",
		MVI0, r2,
		ADD10,
		MVI0, "\r",
		SYSI, displayCString,
		SYSI, displayInteger$1,
		BRA, ADDR, "top",
		END
	);
	asmCompileAsmstack(0);
	asmNewCode();
	vmDebugDumpCode(r0,stdout);
	code=r0;  ip=0;  vmRun();
	memGarbageCollect();
	memDebugDumpHeapHeaders(stdout);
	goto done;
	memStackPush(stack, r0);
	memGarbageCollect();
	memStackPop(stack);
	memStackPop(stack);
	memStackPop(stack);
	memGarbageCollect();
	memGarbageCollect();
	memGarbageCollect();
	memGarbageCollect();
done:
	return 0;
}
