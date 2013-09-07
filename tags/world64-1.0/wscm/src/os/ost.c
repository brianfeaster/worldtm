#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include "os.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"
#include "test.h"


/*******************************************************************************
 TESTS
*******************************************************************************/
void TESTEmpty (void) {
}


int main (void) {
	objInitialize();
	testInitialize();

	/* Perform a full garbage collection to move module related objects to old
	   heap for easier visual debugging of newly created young heap objects */
	GarbageCollectionMode = 1;
	memGarbageCollect();

	TEST(TESTEmpty);

	return 0;
}


