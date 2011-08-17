#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include "sys.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"

#define TEST(fn) (printf("Calling test: "#fn"()  "), fn(),printf("PASS\n"))



void scantTest (void) {
	yy_scan_string((Str)"(let  ~  ( (i 0) (e 9000) ) (displayl i #\a #t #f #( )) (display \"\\r\") (if (= i e) (display \"\n\" #(1 #(2 3) `(4 ,5)) ) (~ (+ i 1) e)))");
	yyparse();
	//memDebugDumpAll(stdout);
	//objDump(r0, stdout);
}


/* Open a local socket port and connect to it with another socket port */
void networkLocalClientServerConnection (void) {
	r0=0;

	//printf("\nOpening listening socket:  ");
	objNewInt(8080); r1=r0;
	sysOpenLocalSocket();
	r5=r0;
	//objDump(r5, stdout);
	assert(saccepting == memVectorObject(r5, 3));

	//printf("\nOpening connecting socket: ");
	objNewString((Str)"localhost", 9); vmPush(r0);
	objNewInt(8080); vmPush(r0);
	sysOpenRemoteSocket();
	r6=r0;
	//objDump(r6, stdout);
	assert(sconnecting == memVectorObject(r6, 3));

	//printf("\nStart listener stream:                ");
	r1=r5;
	sysAcceptLocalStream();
	r7=r0;
	//objDump(r7, stdout); printf(NL);
	assert(sopen == memVectorObject(r7, 3));

	//printf("\nStart connecting stream:   ");
	r1=r6;
	sysAcceptRemoteStream();
	//objDump(r6, stdout); printf(NL);
	assert(sopen == memVectorObject(r6, 3));

	close(objPortDescriptor(r5));
	close(objPortDescriptor(r6));
	close(objPortDescriptor(r7));
}

/* Verify opening a stream on an unconnected local listener socket
   will signal a fail by returning the original listener socket */
void verifyLocalStreamBlocks () {

	//printf("\nOpening listening socket:  ");
	objNewInt(8080); r1=r0;
	sysOpenLocalSocket();
	r5=r0;
	assert(saccepting == memVectorObject(r5, 3));

	//printf("\nOpening stream: ");
	r0=false;
	r1=r5;
	sysAcceptLocalStream();
	assert(r0==r5);
	assert(saccepting == memVectorObject(r5, 3));
}

void networkingWriteStuff (void) {
	r0=0;

	printf("\nOpening listening socket:  ");
	objNewInt(8080); vmPush(r0);
	sysOpenLocalSocket();

	printf("\nOpening stream: ");
	sysAcceptLocalStream();  r1=r0;
	objDump(r0, stdout); printf(NL);

	/* Write a message */
	r1=r5;
	objNewString((Str)"Welcome to world!", 17);  r2=r0;
	r3=0;
	sysSend();

	/* Read whatever was sent */
	r1 = r5;
	r2 = false;
	r3 = nullstr;
	r4 = 0;
	sysRecv();

	printf("\nRead:[");
	objDump(r0, stdout);
	printf("]");
	objDump(r5, stdout);
}

void networkingReadStuff (void) {
	r0=0;

	//printf("\nOpening a listening socket: ");
	objNewInt(8080); vmPush(r0);
	sysOpenLocalSocket();
	//objDump(r0, stdout);

	sleep(5);
	r1 = r0;
	sysAcceptLocalStream();

	r1 = r0;
	r2 = false;
	r3 = nullstr;
	r4 = 0;
	sysRecv();

	printf("\nRead:[");
	objDump(r0, stdout);
	printf("]");
	//memDebugDumpYoungHeap(stdout);
}

int main (int argc, char *argv[]) {
	/* Force a failure by passing -f to this program */
	if (argc==2 && !strcmp(argv[1], "-f")) return -1;

	setbuf(stdout,0);
	printf ("--Welcome to unit test %s----------------\n", __FILE__);

	objInitialize();

	/* Perform a full garbage collection to move module related objects to old
	   heap for easier visual debugging of newly created young heap objects */
	GarbageCollectionMode = 1;
	memGarbageCollect();

	TEST(scantTest);
	TEST(networkLocalClientServerConnection);
	TEST(verifyLocalStreamBlocks);
	//TEST(networkingReadStuff);
	//TEST(networkingWriteStuff);

	return 0;
}

