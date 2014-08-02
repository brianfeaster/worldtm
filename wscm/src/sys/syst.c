#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include "sys.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"
#include "test.h"


/*******************************************************************************
 TESTS
*******************************************************************************/
void TESTscantTest (void) {
	yy_scan_string((Str)"(let  ~  ( (i 0) (e 9000) ) (displayl i #\a #t #f #( )) (display \"\\r\") (if (= i e) (display \"\n\" #(1 #(2 3) `(4 ,5)) ) (~ (+ i 1) e)))");
	yyparse();
	//memDebugDumpAll(stdout);
	//objDump(r00, stdout);
}

/* Verify sandbox file access
*/
void TESTsandbox (void) {
	assert(sysCanonicalizePath("README"));
	assert(sysCanonicalizePath("../blog"));
	sysInitialize((Str)"syst");
	assert(sysCanonicalizePath("README"));
	assert(sysCanonicalizePath("../wscm/README"));
	assert(0==sysCanonicalizePath("../blog"));
	//objNewSymbol((Str)"*LIBPATH*", 9); r01=r00; sysTGEFind(); objDisplay(r00, stdout);
	//objNewSymbol((Str)"*WORKINGPATH*", 13); r01=r00; sysTGEFind(); objDisplay(r00, stdout);
}

/* Open a local socket port and connect to it with another socket port */
void TESTnetworkLocalClientServerConnection (void) {
	r00=0;

	//printf("\nOpening listening socket:  ");
	objNewInt(7070); r01=r00;
	sysOpenLocalSocket();
	r05=r00;
	//objDump(r05, stdout);
	assert(saccepting == memVectorObject(r05, 3));

	//printf("\nOpening connecting socket: ");
	objNewString((Str)"localhost", 9); r02 = r00;
	objNewInt(7070); r01 = r00;
	sysOpenRemoteSocket();
	r06=r00;
	//objDump(r06, stdout);
	assert(sconnecting == memVectorObject(r06, 3));

	//printf("\nStart listener stream:                ");
	r01=r05;
	sysAcceptLocalStream();
	r07=r00;
	//objDump(r07, stdout); printf(NL);
	assert(sopen == memVectorObject(r07, 3));

	//printf("\nStart connecting stream:   ");
	r01=r06;
	sysAcceptRemoteStream();
	//objDump(r06, stdout); printf(NL);
	assert(sopen == memVectorObject(r06, 3));

	close(objPortDescriptor(r05));
	close(objPortDescriptor(r06));
	close(objPortDescriptor(r07));
}

/* Verify opening a stream on an unconnected local listener socket
   will signal a fail by returning the original listener socket */
void TESTverifyLocalStreamBlocks (void) {

	//printf("\nOpening listening socket:  ");
	objNewInt(7070); r01=r00;
	sysOpenLocalSocket();
	r05=r00;
	assert(saccepting == memVectorObject(r05, 3));

	//printf("\nOpening stream: ");
	r00=ofalse;
	r01=r05;
	sysAcceptLocalStream();
	assert(r00==r05);
	assert(saccepting == memVectorObject(r05, 3));
}

void networkingWriteStuff (void) {
	r00=0;

	printf("\nOpening listening socket:  ");
	objNewInt(7070); vmPush(r00);
	sysOpenLocalSocket();

	printf("\nOpening stream: ");
	sysAcceptLocalStream();  r01=r00;
	objDisplay(r00, stdout); printf(NL);

	/* Write a message */
	r01=r05;
	objNewString((Str)"Welcome to world!", 17);  r02=r00;
	r03=0;
	sysSend();

	/* Read whatever was sent */
	r01 = r05;
	r02 = ofalse;
	r03 = onullstr;
	r04 = 0;
	sysRecv();

	printf("\nRead:[");
	objDisplay(r00, stdout);
	printf("]");
	objDisplay(r05, stdout);
}

void networkingReadStuff (void) {
	r00=0;

	//printf("\nOpening a listening socket: ");
	objNewInt(7070); vmPush(r00);
	sysOpenLocalSocket();
	//objDisplay(r00, stdout);

	sleep(5);
	r01 = r00;
	sysAcceptLocalStream();

	r01 = r00;
	r02 = ofalse;
	r03 = onullstr;
	r04 = 0;
	sysRecv();

	printf("\nRead:[");
	objDisplay(r00, stdout);
	printf("]");
	//memDebugDumpYoungHeap(stdout);
}

int main (int argc, char *argv[]) {
	sysInitialize(0);
	testInitialize();

	/* Perform a full garbage collection to move module related objects to old
	   heap for easier visual debugging of newly created young heap objects */
	GarbageCollectionMode = 1;
	memGarbageCollect();

	TEST(TESTscantTest);
	TEST(TESTsandbox);
	TEST(TESTnetworkLocalClientServerConnection);
	TEST(TESTverifyLocalStreamBlocks);

	// TODO: Implement a network read/write test. These require human intervention.
	//TEST(networkingReadStuff);
	//TEST(networkingWriteStuff);

	return 0;
}
