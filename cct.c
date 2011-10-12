#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "cc.h"
#include "sys.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"


#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))


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

/* Compare character file buffer's contents with string argument */
void FBFinalize (char *goldenString) {
	fflush(FB);
	assert(0 == strcmp(FBBuff, goldenString));
	fclose(FB);
	free(FBBuff);
}


void ccDumpString (char *str) { fprintf(FB, "%s", str); }

void mytest (void) {
	FBInit();
	ccDumpString("hello");
	FBFinalize("hello");
}


int main (int argc, char *argv[]) {
	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	//ccInitialize();
	TEST(mytest);

	return 0;
}
