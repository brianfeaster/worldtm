#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "cc.h"
#include "sys.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"
#include "test.h"


/*******************************************************************************
 TESTS
*******************************************************************************/
void ccDumpString (char *str) { fprintf(FB, "%s", str); }

void mytest (void) {
	FBInit();
	ccDumpString("hello");
	FBFinalize("hello");
}


int main (int argc, char *argv[]) {
	//ccInitialize();
	testInitialize();
	TEST(mytest);

	return 0;
}
