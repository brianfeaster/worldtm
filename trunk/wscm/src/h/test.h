#ifndef _TEST_H
#define _TEST_H
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define TEST(fn) do {int c=printf("Calling test: "#fn"() ");fn();printf("%*.sPASS\n", 60<c?0:60-c, "");}while(0)


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
 Num status;
	fflush(FB);
	status = (Num)strcmp(FBBuff, goldenString);
	if (status) fprintf(stderr, "\nReceived [%s]\nExpected [%s] ", FBBuff, goldenString);
	assert(0 == status);
	fclose(FB);
	free(FBBuff);
}

void testInitializeInternal (char *fn) {
	setbuf(stdout, 0);
	printf ("--Welcome to unit test %s----------------\n", fn);
}

#define testInitialize() testInitializeInternal(__FILE__)

static inline u64 rdtsc (void)
{
  u32 hi, lo;
  __asm__ volatile("rdtsc" : "=a" (lo), "=d" (hi));
  return ((u64)hi << 32) | lo;
}

#endif
