#define DEBUG_ALL 0
#define DB_DESC "WSCM "
#include "debug.h"
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <math.h>   /* sqrt() */
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>  /* fcntl() */
#include <stdlib.h> /* random() */
#include <string.h> /* strlen() */
#include <sys/time.h> /* gettimeofday() */
#include <errno.h> /* for errno */
#include <sys/types.h>  /* for socket() */
#include <sys/socket.h>
#include <netinet/in.h> /* for htons() */
#include <netdb.h>      /* for gethostbyname() */
#include <arpa/inet.h>  /* for inet_ntoa() */
#include <time.h>  /* for time() */
#include <sys/poll.h>
#include <signal.h>     /* for signal() */
#include <sys/ioctl.h>  /* for ioctl() and struct winsize*/
#include "comp.h"
#include "os.h"
#include "sys.h"
#include "obj.h"
#include "asm.h"
#include "vm.h"
#include "mem.h"
/* 
   Useful_functions
   Networking_stuff
   System_calls
   Initialization_stuff
	Main

 Concepts:
   (Operator operand operand ...)
   parameters:       Expressions evaluated during a procedure application.
   arguments:        Evaluated parameters (operand values) a function is applied to.
   formal arguments: Function variables

   free variable:    Non-local binding.
   bound variable:   Local binding.

   "A bound variable gets a value stored in it's location"
*/

void debugDumpThreadInfo (void);



/*******************************************************************************
 Useful_functions
*******************************************************************************/

void wscmDumpEnv (void) {
	sysDumpEnv(renv);
}

/* Check that r1, argument stack count, is between the argument count range.
*/
Int wscmAssertArgumentCount (Num count, const char *functionName) {
 Num i;
	if (count == (Num)r1) return 0;
	/* Dump error and syscall/arguments. */
	if (count==1)
		fprintf(stderr, "\r\nERROR: Expecting 1 argument (%s",functionName);
	else
		fprintf(stderr, "\r\nERROR: Expecting "NUM" arguments (%s",count,functionName);
	for (i=0; i<(Num)r1; i++) {
		fprintf(stderr, " ");
		sysWrite(memStackObject(rstack,(Num)r1-i-1), stderr);
	}
	fprintf(stderr, ")\r\n");
	/* Pop args from stack. */
	while (r1--) vmPop();
	r0 = false;
	return -1;
}

Int wscmAssertArgumentCountRange (Num min, Num max, const char *functionName) {
 Num i;
	if (min <= (Num)r1 && (Num)r1 <= max) return 0;
	/* Dump error and syscall/arguments. */
	fprintf(stderr, "\r\nERROR: Expecting "NUM" to "NUM" arguments (%s",min,max,functionName);
	for (i=0; i<(Num)r1; i++) {
		fprintf(stderr, " ");
		sysWrite(memStackObject(rstack,(Num)r1-i-1), stderr);
	}
	fprintf(stderr, ")\r\n");
	/* Pop args from stack. */
	while (r1--) vmPop();
	r0 = false;
	return -1;
}

Int wscmAssertArgumentCountMin (Num min, const char *functionName) {
 Num i;
	if (min <= (Num)r1) return 0;
	/* Dump error and syscall/arguments. */
	if (min == 1)
		fprintf(stderr, "\r\nERROR: Expecting at least "NUM" argument ("STR, min, functionName);
	else
		fprintf(stderr, "\r\nERROR: Expecting at least "NUM" arguments ("STR, min, functionName);
	for (i=0; i<(Num)r1; i++) {
		fprintf(stderr, " ");
		sysWrite(memStackObject(rstack,(Num)r1-i-1), stderr);
	}
	fprintf(stderr, ")\r\n");
	/* Pop args from stack. */
	while (r1--) vmPop();
	r0 = false;
	return -1;
}

/* Pop stack operand arguments into a new list.
   Given   r1 operand count
   Uses    r2
   Return  r0 new list
*/
void sysStackToList (void) {
 Num count = (Num)r1;
	r0=null;
	while (count--) { r1=vmPop(); r2=r0; objCons12(); }
}

#undef DB_DESC
#undef DEBUG



/*******************************************************************************
 Networking_stuff

   Any file or socket failures return #eof as the object.  The listener socket
   is separate from the open socket, alllowing multiple sockets to be opened
   from the same listener socket.  Both are non-blocking at the C level but
   block at the Scheme thread level.
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "SYS_NET "


void wscmSocketFinalizer (Obj o) {
	if (memVectorObject(o, 3) == sclosed) {
		DB("wscmSocketFinalizer:  Socket already closed");
	} else {
		DB("wscmSocketFinalizer:  Closing socket");
		DBE sysDisplay(o, stderr);
		close(*(int*)o); /* TODO this should be close((int)memVectorObject(o, 0)); */
		memVectorSet(o, 3, sclosed);
	}
}


/* Called by syscallRecv or VM syscall instruction.
   in   r1 port object
        r2 timout
        r3 string object buffer  "" any length  () one char  " ..." fill string
   use  r4 read count
   out  r0 string buffer
*/
void wscmRecvBlock (void) {
 s64 wakeupTime;
 Num timedOut;
	DBBEG();
	DBE sysWrite(r4, stderr);
	DBE sysWrite(r3, stderr);
	DBE sysWrite(r2, stderr);
	DBE sysWrite(r1, stderr);

	/* Time can be false meaning wait forever or the time the thread
	   should be woken up regardless of character availability. */
	if (r2 != false) {
		wakeupTime = sysTime() + *(Int*)r2;
		objNewInt(wakeupTime);
		r2=r0;
	}

	r4=0; /* Character read count initialized to 0. */

	sysRecv();
	timedOut = (r2!=false) && (*(Int*)r2 <= sysTime());
	if (r0 == false)
		if (!timedOut) {
			/* Nothing read and haven't timed out yet so block thread */
			osUnRun();
			osMoveToQueue(rrunning, rblocked, sreadblocked);
			osScheduler();
		} if (timedOut && 0<(Num)r4) {
			/* Timeout with a partial read so return partial string */
			objNewString(r3, (Num)r4);
		}
	DBEND("  =>  r0:"OBJ, r0);
}


void wscmOpenRemoteStream (void) {
	DBBEG();
	if (memVectorObject(r1, 3) == sconnecting) sysAcceptRemoteStream();

	if (r1!=eof && memVectorObject(r1, 3) == sconnecting) {
		DB("SYS    blocking on a remote connecting socket...");
		osUnRun();
		osMoveToQueue(rrunning, rblocked, sopenblocked);
		osScheduler();
	}
	DBEND();
}


void wscmOpenLocalStream (void) {
	DBBEG();
	/* Socket is listening so try and accept. */
	if (memVectorObject(r1, 3) == saccepting) {
		sysAcceptLocalStream();
		r1 = r0;
	}

	/* Is it the same socket? */
	if (memVectorObject(r1, 3) == saccepting) {
		DB("SYS    blocking on an accept a connection...");
		osUnRun();
		osMoveToQueue(rrunning, rblocked, sopenblocked);
		osScheduler();
	}
	DBEND();
}

#undef DB_DESC
#undef DEBUG



/*******************************************************************************
 System_calls
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "SYS_SYSCALL "

/* Force a call to the error handler/continuation which must be defined in
   the global environment in the ERRORS vector indexed by thread ID.

	Given  r1  number of expression on stack to pop and group into a message list
   r0 holds the invalid operator or message.
*/
void syscallError (void) {
 Num tid;
	DBBEG();

	/* Create list in r3 of the operator and stack operand arguments.
	   Operator is in r0, operands are on the stack and r1 is operand count. */
	objNewString(r0, strlen(r0)); r3=r0;
	sysStackToList(); /* uses r0-2 */
	r1=r3; r2=r0; objCons12(); r3=r0;
	
	/* Look up error function/continuation in ERRORS vector
	   in TGE and set the code register and IP:
	     r0            The ERRORS vector  of closures/continuations
	     (car running) The current thread
	     (cdr thread)  Current thread ID
	     (car closure) Code block
	     (cdr closure) Parent environment */
	objNewSymbol ((Str)"ERRORS", 6);  r1=r0;  sysTGEFind(); r0=car(r0);
	tid = (Num)osThreadId(rrunning);
	r0 = memVectorObject(r0, tid);
	/* r0 needs to remain the closure when a code block is first run
	   since the called code expects to find a lexical enviroment in the
	   closure in r0 */
	rcode=car(r0);  rip=0;

	/* Push operand and set operand count */
	vmPush(r3);
	r1=(Obj)1;

	DBEND();
}


void syscallQuit (void) {
	if ((Num)r1==1) exit(*(int*)vmPop());
	else exit(0);
}


/* This can do anything.  I change it mainly for debugging and prototyping. */
extern Num garbageCollectionCount;
void sysFun (void) {
	debugDumpThreadInfo ();
	//fprintf (stderr, "Stacklength=[%d]", memStackLength(stack));
	memDebugDumpHeapHeaders(stderr);
	//memDebugDumpYoungHeap(stderr);
	//objNewInt((Int)garbageCollectionCount);
}

/* 1. Call the function and pass it 5
   2. Call the function and pass code
   3. Call the function and pass the continuation code
	Maybe this should be compiled?
	(call/cc fn) => (fn continuation)
*/


void syscallDumpThreads (void) {
	sysWrite(rthreads, stderr);
}


void syscallString (void) {
 Num totalLen=0, s=0, len;
	DBBEG();
	/* Consider sum of lengths of string argumens on stack TODO error checking */
	while (s < (Int)r1)
		totalLen += memObjectLength(memStackObject(rstack, s++));

	if (!totalLen) {
		/* All strings were "".  Pop them off stack and return "". */
		while (s--) vmPop();
		r0 = nullstr;
	} else {
		r0 = memNewArray(TSTRING, totalLen);
		while (s--) {
			totalLen -= (len = memObjectLength(r1=vmPop()));
			memcpy(r0+totalLen, r1, len); /* New string is built in reverse */
		}
	}
	DBEND();
}

void syscallMakeString (void) {
 Num len;
 Chr fill=' ';
	DBBEG();
	if (wscmAssertArgumentCountRange(1, 2, __func__)) return;

	/* Fill character if specified. */
	if (2 == (Num)r1) fill = *(Chr*)vmPop();

	/* Create string of passed length. */
	objNewString(NULL, len=*(Num*)vmPop());

	/* Fill string if fill character specified. */
	if (2 == (Num)r1)  while (len--) ((Chr*)r0)[len]=fill;

	DBEND();
}

void syscallSubString (void) {
 Num end=0, start=0;
	DBBEG();
	if (wscmAssertArgumentCount(3, __func__)) return;
	end=*(Num*)(r2=vmPop());
	start=*(Num*)(r1=vmPop());
	if (end==start) {
		r1=vmPop();
		r0=nullstr;
	} else {
		if (end-start < 0) {
			vmPush(r1);
			vmPush(r2);
			r1 = (Obj)3;
			r0 = "Invalid range to substring";
			syscallError();
		} else {
			r0 = memNewArray(TSTRING, end-start);
			r1=vmPop();
			memcpy(r0, r1+start, end-start);
		}
	}
	DBEND();
}

void syscallStringLength (void) {
	DBBEG();
	if (wscmAssertArgumentCount(1, __func__)) return;
	objNewInt((Int)memObjectLength(vmPop()));
	DBEND();
}

/* Returns an array object in r0 (ignore the type) reprsenting an external
   representation of the object.  Only simple object types are converted. 
   Complex ones or immediate pointers below 2^20 are shown as just hex
   addresses.
*/
void syscallSerializeDisplay (void) {
 static u8 buff[8192];
 Int ret;
	DBBEG();
	if (wscmAssertArgumentCount(1, __func__)) return;
	r0 = vmPop();
	if ((Num)r0 < 0x100000l) {
		ret = sprintf((char*)buff, "#"OBJ, (Num)r0);
		assert(0<ret);
		objNewString(buff, (Num)ret);
	} else switch (memObjectType(r0)) {
		case TSTRING : 
		case TSYMBOL : 
		case TNULL   :
		case TNULLSTR:
		case TNULLVEC:
		case TFALSE  :
		case TTRUE   :
			break;
		case TEOF    :
			r0 = seof;
			break;
		case TINTEGER:
			sysSerializeInteger(*(Int*)r0, 10);
			break;
		case TREAL   : 
			ret = sprintf((char*)buff, "%.2f", *(Real*)r0);
			assert(0<ret);
			objNewString(buff, (Num)ret);
			break;
		case TCHAR   : 
			objNewString(r0, 1);
			break;
		default      :
			ret = sprintf((char*)buff, HEX, (Num*)r0);
			assert(0<ret);
			objNewString(buff, (Num)ret);
			break;
	}
	DBEND();
}

void syscallSerializeWrite (void) {
 static Chr buff[8192];
 Chr c;
 Int ret;
 Num len, i;
	DBBEG();
	if (wscmAssertArgumentCount(1, __func__)) return;
	r0 = vmPop();
	if ((Num)r0 < 0x100000l) {
		ret = sprintf((char*)buff, "#"HEX, (Num)r0);
		assert(0<ret);
		objNewString(buff, (Num)ret);
	} else switch (memObjectType(r0)) {
		case TSYMBOL : 
		case TNULL   :
		case TNULLVEC:
		case TFALSE  :
		case TTRUE   :
			break;
		case TEOF    :
			r0 = seof;
			break;
		case TINTEGER:
			sysSerializeInteger(*(Int*)r0, 10);
			break;
		case TREAL   : 
			ret = sprintf((char*)buff, "%.2f", *(Real*)r0);
			assert(0<ret);
			objNewString(buff, (Num)ret);
			break;
		case TCHAR   : 
			ret = sprintf((char*)buff, "#\\%c", *(char*)r0);
			assert(0<ret);
			objNewString(buff, (Num)ret);
			break;
		case TNULLSTR:
		case TSTRING : 
			len = 0;
			buff[len++] = '"';
			for (i=0; i<memObjectLength(r0); i++) {
				c = ((Chr*)r0)[i];
				switch (c) {
				case '\0'  : buff[len++]='\\'; buff[len++]='0'; break; /*   0 */
				case '\a'  : buff[len++]='\\'; buff[len++]='a'; break; /*   7 */
				case '\b'  : buff[len++]='\\'; buff[len++]='b'; break; /*   8 */
				case '\t'  : buff[len++]='\\'; buff[len++]='t'; break; /*   9 */
				case '\n'  : buff[len++]='\\'; buff[len++]='n'; break; /*  10 */
				case '\v'  : buff[len++]='\\'; buff[len++]='v'; break; /*  11 */
				case '\f'  : buff[len++]='\\'; buff[len++]='f'; break; /*  12 */
				case '\r'  : buff[len++]='\\'; buff[len++]='r'; break; /*  13 */
				case '\033': buff[len++]='\\'; buff[len++]='e'; break; /*  27 */
				case '\"'  : buff[len++]='\\'; buff[len++]='"'; break; /*  34 */
				case '\\'  : buff[len++]='\\'; buff[len++]='\\'; break;/*  92 */
				case (Chr)'\233': buff[len++]='\\'; buff[len++]='c'; break; /* 155 */
				default:
					/* The printable chracter or a slashified hex */
					if ((32<=c && c<=126) || (160<=c && c<=255)) buff[len++]=c;
					else { sprintf ((char*)buff+len, "\\x%02x", (Num)c); len+=4; }
				}
			}
			buff[len++] = '"';
			objNewString(buff, len);
			break;
		default      :
			ret = sprintf((char*)buff, "#"HEX, (Num*)r0);
			assert(0<ret);
			objNewString(buff, (Num)ret);
			break;
	}
	DBEND();
}

void syscallNumber2String (void) {
 Int num;
 Num base;
	DBBEG();
	if (wscmAssertArgumentCountRange(1, 2, __func__)) return;
	base = (Num)r1==2 ? *(Num*)vmPop() : 10; /* Default base is 10. */
	num = *(Int*)vmPop();
	sysSerializeInteger (num, base);
	DBEND();
}

/* For internal use only.  Probably to be phased out in favor of a VM implementation.
*/
void syscallWrite (void) {
 Int fd=1;
 FILE *stream=NULL;

	DBBEG();

	if (wscmAssertArgumentCountRange(1, 2, __func__)) return;
	if ((Int)r1==2) fd=*(Int*)vmPop(); /* File descriptor. */

	if (fd==1) stream = stdout;
	if (fd==2) stream = stderr;
	assert(NULL != stream);

	sysWrite(r0=vmPop(), stream);

	DBEND();
}

void syscallDisplay (void) {
 Int fd=1;
 FILE *stream=NULL;
	DBBEG();

	if (wscmAssertArgumentCountRange(1, 2, __func__)) return;
	if ((Int)r1==2) fd=*(Int*)vmPop(); /* Descriptor. */

	if (fd==1) stream = stdout;
	if (fd==2) stream = stderr;
	assert(NULL != stream);

	sysDisplay(r0=vmPop(), stream);

	DBEND();
}

void syscallVector (void) {
 Num l=(Num)r1;
	DBBEG();
	if (l==0) r0=nullvec;
	else {
		objNewVector(l);
		while (l--) memVectorSet(r0, l, vmPop());
	}
	DBEND();
}

void syscallMakeVector (void) {
 Num len;
	DBBEG();
	if (wscmAssertArgumentCountRange(1, 2, __func__)) return;

	r2 = r1==(Obj)2 ? vmPop() : null;
	len = *(Num*)vmPop();
	if (len<1) r0=nullvec;
	else {
		objNewVector(len);
		while (len--) memVectorSet(r0, len, r2);
	}

	DBEND();
}

void syscallRandom (void) {
	DBBEG();
	if (wscmAssertArgumentCountRange(0, 1, __func__)) return;
	if ((Int)r1 == 1)
		objNewInt(random() % *(Int*)vmPop());
	else
		objNewInt(random());
	DBEND();
}

/* Numerical equivalence. */
void syscallEquals (void) {
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1=vmPop();  r0=vmPop();
	r0 = TINTEGER == memObjectType(r0)
	     && TINTEGER == memObjectType(r1)
	     && *(Int*)r0 == *(Int*)r1
	     ? true : false;
	DBEND();
}

void syscallEqP (void) {
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	r0 = (vmPop() == vmPop()) ? true : false;
	DBEND();
}

void syscallStringEqualsP (void) {
 Num len;
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1=vmPop();  r0=vmPop();
	r0 = TSTRING == memObjectType(r0)
	     && TSTRING == memObjectType(r1)
	     && memObjectLength(r0) == (len=memObjectLength(r1))
	     && !strncmp (r0, r1, len) ? true : false;
	DBEND();
}

void syscallNotEquals (void) {
 Int a, b;
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1=vmPop();
	a = *(Int*)r1;
	r2=vmPop();
	b = *(Int*)r2;
	r0 = (a != b) ? true : false;
	DBEND();
}

void syscallLessThan (void) {
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	r0 = (*(Int*)vmPop() > *(Int*)vmPop()) ? true : false;
	DBEND();
}

void syscallLessEqualThan (void) {
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	r0 = (*(Int*)vmPop() >= *(Int*)vmPop()) ? true : false;
	DBEND();
}

void syscallGreaterThan (void) {
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1 = vmPop();
	r0 = vmPop();
	if (TREAL == memObjectType(r0) && TREAL == memObjectType(r1))
		r0 = (*(Real*)r1 < *(Real*)r0) ? true : false;
	else
		r0 = (*(Int*)r1 < *(Int*)r0) ? true : false;
	DBEND();
}

void syscallGreaterEqualThan (void) {
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1 = vmPop();
	r0 = vmPop();
	if (TREAL == memObjectType(r0) && TREAL == memObjectType(r1))
		r0 = (*(Real*)r1 <= *(Real*)r0) ? true : false;
	else
		r0 = (*(Int*)r1 <= *(Int*)r0) ? true : false;
	DBEND();
}

void syscallAdd (void) {
 Int sum=0;
	DBBEG();
	while (r1--) sum += *(Int*)vmPop();
	objNewInt(sum);
	DBEND();
}

void syscallMul (void) {
 Int product=1;
	DBBEG();
	while (r1--) product *= *(Int*)vmPop();
	objNewInt(product);
	DBEND();
}

/* Multiply all but the first, then divide the first by that the product.
*/
void syscallDiv (void) {
 Int product=1, divisor;
	DBBEG();
	while (1 < (Int)r1--) product *= *(Int*)vmPop();
	divisor = *(Int*)vmPop();
	objNewInt(product ? divisor/product : INT_MAX);
	DBEND();
}

void syscallLogAnd (void) {
 Int a, b;
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	a = *(Int*)vmPop();
	b = *(Int*)vmPop();
	objNewInt(a&b);
	DBEND();
}

void syscallSqrt (void) {
	DBBEG();
	if (wscmAssertArgumentCountRange(1, 1, __func__)) return;
	objNewInt((Int)sqrt((double)*(Int*)vmPop()));
	DBEND();
}

void syscallRemainder (void) {
 Int a, b;
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	b = *(Int*)vmPop();
	a = *(Int*)vmPop();
	objNewInt(a%b);
	DBEND();
}

void syscallModulo (void) {
 Int a, b;
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	b = *(Int*)vmPop();
	a = *(Int*)vmPop();
	/* If one argument is negative. */
	if ((a<0) ^ (b<0))
		objNewInt((b+a%b)%b);
	else
		objNewInt(a%b);
	DBEND();
}

void syscallSub (void) {
 Int sum=0;
	DBBEG();
	if (wscmAssertArgumentCountMin(1, __func__)) return;
	if (1==(Int)r1) objNewInt(-*(Int*)vmPop());
	else {
		while (--r1) {sum += *(Int*)vmPop(); }
		sum = *(Int*)vmPop() - sum;
		objNewInt(sum);
	}
	DBEND();
}

void syscallSleep (void) {
	DBBEG();
	if (wscmAssertArgumentCount(1, __func__)) return;
	osSleepThread();
	DBEND();
}

/* Return thread ID. */
void syscallTID (void) {
	DBBEG();
	if (wscmAssertArgumentCount(0, __func__)) return;
	objNewInt((Int)osThreadId(rrunning));
	DBEND();
}

void syscallOpenSocket (void) {
	DBBEG();
	if (1 == (Int)r1) {
		if (memObjectType(r1=vmPop()) != TINTEGER) {
			vmPush(r1); /* Push invalid object */
			r1 = (Obj)1;
			r0 = "Invalid argument to syscall open-socket:";
			syscallError();
			return;
		} else {
			sysOpenLocalSocket(); /* pass in port via r1 */
			/* Create and set the socket's "close" finalizer */
			r1=r0;
			r0 = memNewFinalizer();
			memVectorSet(r0, 0, wscmSocketFinalizer);
			memVectorSet(r0, 1, r1);
			memVectorSet(r1, 5, r0);
			r0=r1;
		}
	} else if (2 == (Int)r1) {
		sysOpenRemoteSocket();
	} else {
		fprintf(stderr, "Invalid arguments to open-socket:");
		sysStackToList();
		vmPush(r0);
		r1 = (Obj)1;
		syscallError();
	}
	DBEND();
}

void syscallOpenStream (void) {
	DBBEG();
	if (wscmAssertArgumentCount(1, __func__)) goto ret;

	r1 = vmPop();
	if (memObjectType(r1) != TPORT) {
		vmPush("Invalid arguments to open-stream:"); /* TODO does this work? */
		r1++;
		syscallError();
		goto ret;
	}

	if (memVectorObject(r1, 3) == sconnecting) /* TODO abstract this vector-ref */
		wscmOpenRemoteStream();
	else if (memVectorObject(r1, 3) == saccepting)
		wscmOpenLocalStream();
	else if (memVectorObject(r1, 3) == sclosed)
		r0=eof;
	else {
		r0 = "Invalid port state to open-stream.\n";
		r1 = (Obj)1;
		syscallError();
	}
 ret:
	DBEND();
}

/* Create a file object used for file I/O.
   r1 <= filename
   r0 => port object of #eof
*/
void wscmOpen (int oflag, mode_t mode, Num silent) {
 Chr name[160]={0};
 Num i, filenameLen;
	DBBEG();

	/* Convert Scheme string to C string */
	filenameLen = memObjectLength(r1);
	assert((0 < filenameLen) && (filenameLen < 160));
	memcpy(name, r1, filenameLen);
	name[filenameLen] = 0;

	/* Filter out directory paths.  Replace all '/' with '!' */
	for (i=0; i<filenameLen; ++i) if (name[i]=='/') name[i]='!';
	DB ("Filename ["STR"]", name);

	r1 = (Obj)(Int)open((char*)name, oflag, mode);

	if (-1 == (Int)r1) {
		if (!silent)
			fprintf (stderr, "ERROR: wscmOpen() Unable to open local file '"STR"' \""INT":"STR"\"",
				name, errno, strerror(errno));
		r0 = eof;
	} else {
		/* Create the port object passing the filename, descriptor flags and object state */
		objNewString(name, filenameLen);  r2=r0;
		objNewInt(oflag);  r3=r0;
		r4 = sopen;
		objNewPort();
	}
	DBEND();
}

void syscallOpenFile (void) {
 Num silent=0;
	DBBEG();
	if (wscmAssertArgumentCountRange(1, 2, __func__)) {
		r0 = eof;
	} else {
		if ((Num)r1==2) { /* If an extra argis passed, make it a silent call */
			vmPop();
			silent=1;
		}
		r1 = vmPop();
		wscmOpen(O_RDWR, S_IRUSR|S_IWUSR, silent);
	}
	DBEND();
}

void syscallOpenNewFile (void) {
 Num silent=0;
	DBBEG();
	if (wscmAssertArgumentCountRange(1, 2, __func__)) {
		r0 = eof;
	} else {
		if ((Num)r1==2) { /* If an extra argis passed, make it a silent call */
			vmPop();
			silent=1;
		}
		r1 = vmPop();
		wscmOpen(O_CREAT|O_TRUNC|O_RDWR, S_IRUSR|S_IWUSR, silent);
	}
	DBEND();
}

void syscallClose(void) {
	DBBEG();
	r0 = vmPop();
	if (memObjectType(r0) != TSOCKET
		 && memObjectType(r0) != TPORT) {
		printf ("WARNING: syscallClose: not a socket: ");
		sysDisplay(r0, stdout);
	} if (memVectorObject(r0, 3) == sclosed) {
		printf ("WARNING: syscallClose: socket already closed: ");
		sysDisplay(r0, stdout);
	} else {
		close(*(int*)r0);
		memVectorSet(r0, 3, sclosed);
	}
	DBEND();
}

/* Given a byte count, timeout and port, read from the port count bytes or if 0 any
   number of bytes.
*/
void syscallRecv (void) {
	DBBEG();

	r1=vmPop(); /* Port object in r1 */
	r2=vmPop(); /* Timeout in r2 */
	/* r3 gets the byte count.  It's then assigned an empty string
	   character buffer with the specified length of characters to
	   read.  A count of zero (null string) implies read any count
	   of bytes. */
	r3=*(Obj*)vmPop();

	if (r3) {
		r0 = memNewArray(TSTRING, (Num)r3);
		r3=r0;
	} else {
		r3 = nullstr;
	}

	if (memObjectType(r1) != TSOCKET
		 && memObjectType(r1) != TPORT) {
		fprintf (stderr, "WARNING: syscallRecv: not a socket: ");
		sysDisplay(r1, stdout);
		r0 = eof;
	} else {
		wscmRecvBlock();
	}
	DBEND();
}

void syscallReadChar (void) {
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1=vmPop(); /* Port object */
	r2=vmPop(); /* Timout */
	if (memObjectType(r1) != TSOCKET
	    && memObjectType(r1) != TPORT) {
		printf ("WARNING: syscallReadChar: not a socket: ");
		sysDisplay(r1, stdout);
		r0 = eof;
	} else {
		r3=null;  /* tells recv that we just want a character. */
		wscmRecvBlock();
	}
	DBEND();
}

void syscallUnreadChar (void) {
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1=vmPop(); /* Port. */
	r0=vmPop(); /* Character. */
	if (memObjectType(r1) != TSOCKET && memObjectType(r1) != TPORT) {
		printf ("WARNING: syscallUnreadChar: arg2 not a socket: ");
		sysDisplay(r1, stdout);
		r0 = eof;
	} else if (memObjectType(r0) != TCHAR) {
		printf ("WARNING: syscallUnreadChar: arg1 not a char: ");
		sysDisplay(r0, stdout);
		r0 = eof;
	} else
		memVectorSet(r1, 4, r0);
	DBEND();
}

/* Call internal C parser on passed string.  Doesn't block so an EOF
   can be returned if the expression is incomplete or empty.
*/
void syscallReadString (void) {
	DBBEG();
	if (wscmAssertArgumentCountRange(1, 1, __func__)) return;
	r1=vmPop(); /* String to parse. */
	yy_scan_bytes(r1, memObjectLength(r1));
	yyparse();
	DBEND();
}

/* Given string and port, send string to port.  Thread blocks until all is
   sent.
*/
void syscallSend (void) {
	DBBEG();
	if (wscmAssertArgumentCountRange(2, 2, __func__)) return;
	/* r1 gets port object. */
	r1=vmPop();
	/* r2 gets string object. */
	r2=vmPop();
	/* Count sent already.  Should be initialized to 0. */
	r3=0;
	if (memObjectType(r1) != TSOCKET && memObjectType(r1) != TPORT) {
		printf ("WARNING: sysSend: not a socket is (type "HEX02"): ", memObjectType(r1));
		*(int*)0=0;
		r0 = eof;
	} else {
		sysSend();
		if (r0 == false) { /* Still more to send. */
			osUnRun();
			osMoveToQueue(rrunning, rblocked, swriteblocked);
			osScheduler();
		}
	}
	DBEND();
}

void syscallSeek (void) {
 int fd, offset, whence;
	DBBEG();
	if (wscmAssertArgumentCount(3, __func__)) return;
	whence = *(int*)vmPop(); /* Whence */
	assert(whence==SEEK_SET || whence==SEEK_CUR || whence==SEEK_END);
	offset = *(int*)vmPop(); /* Offset */
	fd=*(int*)vmPop(); /* Descriptor. */
	lseek(fd, offset, whence);
	DBEND();
}

void syscallTerminalSize (void) {
 struct winsize win;
	DBBEG();
	ioctl(1, TIOCGWINSZ, &win);
	objNewInt(win.ws_col); r1=r0;
	objNewInt(win.ws_row); r2=r0;
	objCons12();
	DBEND();
}

/* Returns character given string and offset.  A negative offset returns
   character offset from the end.  (string-ref "abcde" -2) => #\d
*/
void syscallStringRef (void) {
 Int offset;
 Obj o;
	DBBEG();
	if (wscmAssertArgumentCount(2, __func__)) return;
	offset = *(Int*)vmPop();
	o=vmPop();
	/* A (negative? offset) implies (absolute offset) from end. */
	if (offset < 0) offset = (Int)memObjectLength(o)+offset;
	r0 = memVectorObject(characters, *((Chr*)o+offset));
	DBEND();
}

void syscallStringSetB (void) {
 Obj strObj;
 Int offset;
 Chr ch;
	DBBEG();
	if (wscmAssertArgumentCount(3, __func__)) return;
	ch = *(Chr*)vmPop();
	offset=*(Int*)vmPop();
	strObj = vmPop();
	/* A (negative? offset) implies (absolute offset) from end. */
	if (offset < 0) offset = (Int)memObjectLength(strObj)+offset;
	assert(0<=offset);
	memArraySet(strObj, (Num)offset, ch);
	DBEND();
}

void syscallVectorLength (void) {
	DBBEG();
	if (wscmAssertArgumentCount(1, __func__)) return;
	r0 = vmPop();
	objNewInt(r0==nullvec?0:(Int)memObjectLength(r0));
	DBEND();
}

/* Deserializers:  String representation in r5, length r6 => new atom in r0. */
void sysNewSymbol (void) {
	DBBEG();
	objNewSymbolR5R6(); // objNewSymbol((Str)r5, (Num)r6);
	DBEND();
}
void sysNewString (void) {
 Num len = parseString(r5); /* Mutates the string & returns the new length. */
	DBBEG();
	if ((Int)r6 == 2) r0 = nullstr;
	else {
		objNewString(NULL, len);
   	memcpy(r0, r5, len);
	}
	DBEND();
}
void sysNewCharacter (void) {
	DBBEG();
	r0 = memVectorObject(characters, ((u8*)r5)[2]);
	DBEND();
}
void sysNewInteger (void) {
	DBBEG();
	*((char*)r5+(Int)r6)=0;
	objNewInt(strtol(r5+(*(char*)r5=='#'?2:0),0,10));
	DBEND();
}
void sysNewBinary (void) {
	DBBEG();
	objNewInt(strtol(r5+2,0,2));
	DBEND();
}
void sysNewReal (void) {
	DBBEG();
	objNewReal(strtof(r5,0));
	DBEND();
}
void sysNewOct (void) {
	DBBEG();
	objNewInt(strtol(r5+2,0,8));
	DBEND();
}
void sysNewHex (void) {
	DBBEG();
	objNewInt(strtol(r5+2,0,16));
	DBEND();
}

void syscallDebugDumpAll (void) {
	DBBEG();
	memDebugDumpAll(NULL);
	DBEND();
}

void syscallGarbageCollect (void) {
	DBBEG();
	memGarbageCollect();
	objNewInt((Int)garbageCollectionCount);
	DBEND();
}

void syscallDisassemble (void) {
	DBBEG();
	r0=vmPop();
	if (memObjectType(r0) == TCLOSURE) {
		if (cdr(r0) != null) sysDumpEnv(cdr(r0));
		r0=car(r0);
	}
	vmDebugDumpCode(r0, stderr);
	DBEND();
}

/* Semaphores are just immediate numbers.
*/
void syscallOpenSemaphore (void) {
	if (wscmAssertArgumentCount(1, __func__)) return;
	osOpenSemaphore();
}

void syscallCloseSemaphore (void) {
	if (wscmAssertArgumentCount(1, __func__)) return;
	r0 = vmPop();
	if (memVectorObject(r0,0)==false) /* TODO this vector-ref should be abstracted using semaphore accessors */
		r0 = false; /* Semaphore already closed */
	else {
		memVectorSet(r0, 0, false);
		osUnblockSemaphoreBlocked(r0, 1); /* 1 means unblock all threads on this semaphore */
		r0 = true;
	}
}

void syscallSemaphoreDown (void) {
 Int newCount;
	DBBEG();
	if (wscmAssertArgumentCountRange(1, 1, __func__)) return;

	/* Decrement the semaphore's counter */
	r0 = vmPop();
	if (memVectorObject(r0,0)==false) {
		r0 = false;
	} else {
		newCount = --*(Int*)r0;

		if (newCount < 0) {
			/* Block thread on this semaphore.  Store semaphore index in r1. */
			DB (" !!! Blocking thread %d", osThreadId(rrunning));
			r1 = r0;
			osUnRun();
			osMoveToQueue(rrunning, rblocked, ssemaphore); /* TODO create a separate semaphore blocked queue */
			osScheduler();
		} else
			r0 = true;
	}

	DBEND();
}

void syscallSemaphoreUp (void) {
 Int newCount;
	DBBEG();
	if (wscmAssertArgumentCountRange(1, 1, __func__)) return;

	r0 = vmPop();
	if (memVectorObject(r0,0)==false) {
		r0 = false;
	} else {
		newCount = ++*(Int*)r0;

		/* Unblock a thread blocked by this semaphore. If after incrmenting the
	   	counter the semaphore is still blocking, then this means one
	   	of the blocked threads can be awakened. */
		if (newCount < 1) osUnblockSemaphoreBlocked(r0, 0);
		r0 = true;
	}

	DBEND();
}

void syscallTime (void) {
 struct timeval tv;
	DBBEG();
	if (wscmAssertArgumentCountRange(0, 0, __func__)) return;
	gettimeofday(&tv, NULL);
	objNewInt(tv.tv_sec);
	DBEND();
}

void syscallUTime (void) {
	DBBEG();
	if (wscmAssertArgumentCountRange(0, 0, __func__)) return;
	objNewInt(sysTime());
	DBEND();
}


void syscallToggleDebug (void) {
	wscmDebug ^= 1;
	r0 = wscmDebug ? true : false;
}


void sysDumpCallStackCode (void) {
 Num i;
 Obj o, l=NULL;

	sysDumpEnv(r1c);
	sysDumpEnv(r19);
	for (i=0; i<memStackLength(rstack); ++i) {
		o = memStackObject(rstack, i);
		printf ("Stack "INT" "HEX016" "OBJ NL, i, memIsObjectValid(o)?memObjectDescriptor(o):0, o);
	}

	for (i=0; i<memStackLength(rstack); ++i) {
		o = memStackObject(rstack, i);
		if (memIsObjectValid(o)) {
			if (memObjectType(o)==TCODE) {
				printf (NL "Stack "INT" "HEX016" "OBJ NL, i, memObjectDescriptor(o), o);
				sysWrite(memVectorObject(o, 2), stdout);
				if (l!=NULL) sysDumpEnv(l);
				else printf (NL);
			}
		}
		l = o;
	}
}


void syscallDebugger (void) {
 struct winsize win;
 int done=0, cmd, fl;
 struct termios tios, tios_orig;

	fflush(stdout);

	/* Force cooked mode. */
	tcgetattr(1, &tios_orig); /* Backup termios */
	tcgetattr(1, &tios);
	tios.c_iflag |= (ICRNL | IXON | IXOFF | IXANY);
	tios.c_oflag |= OPOST;
	tios.c_lflag |= (ECHO | ICANON | ISIG | IEXTEN);
	tcsetattr(1, TCSANOW, &tios);

	/* Force blocking input */
	fl=fcntl(0, F_GETFL, 0);
	fcntl (0, F_SETFL, fl&~O_NONBLOCK);

	ioctl(1, TIOCGWINSZ, &win); printf ("\e[0m\e[%dH\n", win.ws_row); /* Move cursor to bottom of screen */
	while (!done) {
		printf ("\n\e[1m-------------------------------");
		printf ("\nc    vmDebugDumpCode(r1c, stderr)");
		printf ("\nC a  vmDebugDumpCode(a, stderr)");
		printf ("\nu    sysDumpCallStackCode()");
		printf ("\ne    sysDumpEnv(env)");
		printf ("\ns    sysWrite(stack, stderr)");
		printf ("\ny    sysWrite(symbols, stderr)");
		printf ("\nh    memDebugDumpHeapHeaders(stderr)"); 
		printf ("\n3    memDebugDumpYoungHeap(stderr)"); 
		printf ("\nw a  sysWrite(a1, stderr)");
		printf ("\nR    return");
		printf ("\nX    crash");
		printf ("\nQ    exit(-1)");
		printf ("\n-------------------------------\e[0m");
		printf ("\nwscmdbg>");
		while (strchr("\r\n \t", cmd=getchar())); /* Skip whitespace */
		if (cmd=='c') vmDebugDumpCode(rcode, stderr);
	   if (cmd=='C') {
			Obj arg;
			scanf("%lx", &arg);
			vmDebugDumpCode(arg, stderr);
		}
		if (cmd=='u') sysDumpCallStackCode();
		if (cmd=='e') sysDumpEnv(renv);
		if (cmd=='s') sysWrite(rstack, stderr);
		if (cmd=='y') sysWrite(rsymbols, stderr);
	   if (cmd=='h') memDebugDumpHeapHeaders(stderr);
	   if (cmd=='3') memDebugDumpYoungHeap (stderr);
	   if (cmd=='w') {
			Obj arg;
			scanf("%lx", &arg);
			if (memObjString(arg)) printf ((char*)memObjString(arg));
			else sysWrite(arg, stderr);
		}
		if (cmd=='R') done=1;
		if (cmd=='X') *(Int*)0=0;
		if (cmd=='Q') exit(-1);
	}

	/* Restore terminal and IO */
	tcsetattr(1, TCSANOW, &tios_orig);
	fcntl (0, F_SETFL, fl);
	r0=true;
}


void wscmSigAlarmHandler (int sig) {
	vmInterrupt=1;
}
void wscmSigAlarmReset (void) {
	ualarm(10*1000,0); /* 10 miliseconds (100 tics/sec) and no repeat interval set*/
}

/* Passed to the virtual machine module during initialization
   and is called periodically to schedule a new thread, spawn a new signal
   handler thread.
*/
void wscmSchedule (void) {
	DBBEG();

	if (signalFlag) { osSpawnSignalHandler(); }

	/* Either schedule a new thread or return back to VM on same thread. */
	/* TODO what about the blocked queue? */
	if ((Num)osThreadCount() != 1 || !osIsQueueEmpty(rsleeping)) {
		DB("  Scheduling another thread");
		osUnRun();
		osScheduler();
	} else {
		DB("  No other threads.  Continuing current thread.");
	}

	wscmSigAlarmReset(); /* Start interrupt timer again */

	DBEND();
}


void wscmSignalHandler (int sig) {
	DBBEG();
	assert(0 < sig && sig < MAX_SIGNAL_VALUE);
	caughtSignals[sig]=1;
	vmInterrupt=1; /* Let virtual machine know an interrupt occured */
	signalFlag=1; /* Let scheduler know a signal handler needs to be instantiated */
	DBEND();
}

/* Syscall to set a signal handler for a specified signal number.
   The signal handler will always be the wscmSignalHandler().  See above. */
void syscallSignal (void) {
 Num s = *(Num*)vmPop();
	DBBEG();
	if (wscmAssertArgumentCount(1, __func__)) return;
	signal((int)s, wscmSignalHandler); /* Have to cast to an uint64 to int32 */
	DBEND();
}


#undef DB_DESC
#undef DEBUG



/*******************************************************************************
 Initialization_stuff
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "SYS_INIT "

/* Bind symbol, created from 'sym', in TGE and assign object in r0 to the location.
	Mutates: r1 r2
	Returns: r0
*/
void wscmDefine (char* sym) {
	DBBEG();
	r1 = r0;
	objNewSymbol((Str)sym, strlen(sym)); r2=r0;
	objCons12();
	/* Insert binding into TGE list:  (TGE (val . sym) . ...) */
	r1=r0;  r2=cdr(rtge);  objCons12();
	memVectorSet(rtge, 1, r0);
	DBEND();
	DBE sysWrite (r1, stderr);
}

/* Given  - r1:port object  r3:pointer to char
            r4:current state (see scanner.c)   r5:yytext  r6:yylen
   Return - r4:next state   r5:yytext   r6:yylen
            r0:final state if complete token scanned (reached final state).
*/
void sysTransition (void) {
	DBBEG(" *r3="HEX" state:r4="HEX, *(Chr*)r3, r4);
	//DBE sysWrite (r3, stderr);
	//DBE sysWrite (r4, stderr);
	/* Make the transition on char in r3 given state in r4. */
	r4 = (Obj)transition(*(Num*)r3, (Num)r4);
	DB("transition to state => "HEX, r4);
	/* Append char to scanned token and inc yylen. */
	*((u8*)r5+(Num)r6++) = *(u8*)r3;
	/*  Reset yylen to 0 if we ever end up back to the initial state.*/
	if ((Int)r4 == 0x00) r6=0;
	else if ((Int)r4 & FINALSTATE) {
		/* Push back character to stream if in pushback state and not eof. */
		if (((Int)r4 & PUSHBACK) && (r3!=eof)) {
			r6--;
			memVectorSet(r1, 4, memVectorObject(characters, *(Num*)r3));
		}
		r0=r4;
	}
	DBEND(" final state r0="HEX, r0);
	//DBE sysWrite(r4, stderr);
	//DBE sysWrite(r5, stderr);
}

/* Call to the read scheme closure will setup registers for calls to internal
   scanning and parsing code blocks.  This function creates that closure.
	Create atom parser code block.  Modeled after the C equivalent in scanner.c

   scanner  - r4:state   r5:token buffer   r6:token length  r7:islist boolean

	Uses -- r1:port object   r2:timeout=0  r3:wantChar=()  r4:used by recv
	        r4:state   r5:token buffer   r6:token length   r7:is list bool.

	wscmRecvBlock in: r1 port  r2 timeout  r3 string buffer
	              use: r4
	              out: r0 string
*/
void wscmCreateRead (void) {
	DBBEG();
	objNewString((Str)"Parser",6); /* Label */
	asmAsm (
		BRA, 8l,
		r0, /* Label */
		MVI4, 0l,             /* r4 <- Initial state. */
		MVI6, 0l,             /* r6 <- initial yylength. */
	LABEL, "scan",
		/* Call recv block to read one char */
		MVI2, false, /* tell recv to not timeout */
		MVI3, null, /* tell recv that we want a character */
		PUSH4,
			SYSI, wscmRecvBlock, /* Syscall to read char. */
		POP4,
		MV30,                     /* move char to r3 */
		MVI0, 0l, /* Initialize final state to 0.  Will return 0 when still in non-final state. */
		SYSI, sysTransition, /* Syscall to get next state. */
		BEQI0, 0l, ADDR, "scan",

		/* close paren? */
		BNEI0, SCLOSEPAREN, ADDR, "dot",
		MVI0, null,
		RET,
	LABEL, "dot",
		/* dot? */
		BNEI0, SDOT, ADDR, "eof",
		PUSH7, PUSH1A, PUSH1B, /* Recurse on this fn (parser) with isList set. */
		MVI7, 1l,
		MV01E,
		JAL0,
		POP1B, POP1A, POP7,
		LDI00, 0l, /* Only want the car of the list we just parsed. */
		RET,
		/* eof? */
	LABEL, "eof",
		BNEI0, SEOF, ADDR, "vector",
		MVI0, eof,
		RET,
		/* vector? */
	LABEL, "vector",
		BNEI0, SVECTOR, ADDR, "quote",
		PUSH7, PUSH1A, PUSH1B, /* Recursive call with isList set */
		MVI7, 1l,
		MV01E,
		JAL0,
		POP1B, POP1A, POP7,
		PUSH1, /* Save r1 since objListToVector requires r1. */
		MV10,
		SYSI, objListToVector,
		POP1,  /* Restore r1. */
		BRA, ADDR, "done",

	LABEL, "quote",
		/* quote? */
		BNEI0, SQUOTE, ADDR, "unquotesplicing",
		PUSH7, PUSH1A, PUSH1B,
		MVI7, 0l,
		MV01E,
		JAL0,
		POP1B, POP1A, POP7,
		PUSH1, PUSH2, /* Save r1 and r2 */
			MV10,           /* Car object. */
			MVI2, null,/* Cdr object. */
			SYSI, objCons12,
			MVI1, squote,/* Car object. */
			MV20,            /* Cdr object. */
			SYSI, objCons12,
		POP2, POP1,   /* Restore r1 and r2 */
		BRA, ADDR, "done",
	LABEL, "unquotesplicing",
		/* unquote-splicing? */
		BNEI0, SUNQUOTESPLICING, ADDR, "unquote",
		PUSH7, PUSH1A, PUSH1B,
		MVI7, 0l,
		MV01E,
		JAL0,
		POP1B, POP1A, POP7,
		PUSH1, PUSH2, /* Save r1 and r2 */
			MV10,           /* Car object. */
			MVI2, null,/* Cdr object. */
			SYSI, objCons12,
			MVI1, sunquotesplicing,/* Car object. */
			MV20,            /* Cdr object. */
			SYSI, objCons12,
		POP2, POP1,   /* Restore r1 and r2 */
		BRA, ADDR, "done",
		/* unquote? */
	LABEL, "unquote",
		BNEI0, SUNQUOTE, ADDR, "quasiquote",
		PUSH7, PUSH1A, PUSH1B,
		MVI7, 0l,
		MV01E,
		JAL0,
		POP1B, POP1A, POP7,
		PUSH1, PUSH2, /* Save r1 and r2 */
			MV10,            /* Car object. */
			MVI2, null,      /* Cdr object. */
			SYSI, objCons12,
			MVI1, sunquote,  /* Car object. */
			MV20,            /* Cdr object. */
			SYSI, objCons12,
		POP2, POP1,   /* Restore r1 and r2 */
		BRA, ADDR, "done",
		/* quasiquote? */
	LABEL, "quasiquote",
		BNEI0, SQUASIQUOTE, ADDR, "openparen",
		PUSH7, PUSH1A, PUSH1B,
		MVI7, 0l,
		MV01E,
		JAL0,
		POP1B, POP1A, POP7,
		PUSH1, PUSH2, /* Save r1 and r2 */
			MV10,             /* Car object. */
			MVI2, null,       /* Cdr object. */
			SYSI, objCons12,
			MVI1, squasiquote,/* Car object. */
			MV20,             /* Cdr object. */
			SYSI, objCons12,
		POP2, POP1,   /* Restore r1 and r2 */
		BRA, ADDR, "done",
		/* open paren? */
	LABEL, "openparen",
		BNEI0, SOPENPAREN, ADDR, "character",
		PUSH7, PUSH1A, PUSH1B, /* Recursive call with isList set */
		MVI7, 1l,
		MV01E,
		JAL0,
		POP1B, POP1A, POP7,
		BRA, ADDR, "done",
		/* character? */
	LABEL, "character",
		BNEI0, SCHARACTER, ADDR, "symbol",
		SYSI, sysNewCharacter,
		BRA, ADDR, "done",
		/* symbol? */
	LABEL, "symbol",
		BNEI0, SSYMBOL, ADDR, "string",
		SYSI, sysNewSymbol,
		BRA, ADDR, "done",
		/* string? */
	LABEL, "string",
		BNEI0, SSTRING, ADDR, "integer",
		SYSI, sysNewString,
		BRA, ADDR, "done",
		/* integer? */
	LABEL, "integer",
		BNEI0, SINTEGER, ADDR, "real",
		SYSI, sysNewInteger,
		BRA, ADDR, "done",
		/* real? */
	LABEL, "real",
		BNEI0, SREAL, ADDR, "oct",
		SYSI, sysNewReal,
		BRA, ADDR, "done",
		/* oct number? */
	LABEL, "oct",
		BNEI0, SOCT, ADDR, "hex",
		SYSI, sysNewOct,
		BRA, ADDR, "done",
		/* hex number? */
	LABEL, "hex",
		BNEI0, SHEX, ADDR, "binary",
		SYSI, sysNewHex,
		BRA, ADDR, "done",
		/* binary number? */
	LABEL, "binary",
		BNEI0, SBINARY, ADDR, "false",
		SYSI, sysNewBinary,
		BRA, ADDR, "done",
		/* false? */
	LABEL, "false",
		BNEI0, SFALSE, ADDR, "true",
		MVI0, false,
		BRA, ADDR, "done",
		/* true? */
	LABEL, "true",
		BNEI0, STRUE, ADDR, "default",
		MVI0, true,
		BRA, ADDR, "done",
		/* default to null? */
	LABEL, "default",
		MVI0, null,

		/* List context?  If so recurse and contruct pair. */
	LABEL, "done",
		BEQI7, 0l, ADDR, "ret",
		PUSH0, /* Save object just parsed. */
		PUSH1A, PUSH1B, /* Recurse. */
		MV01E, /* r0 <- code register*/
		JAL0,
		POP1B, POP1A,
		/* Create pair from top of stack and just parsed object.  Since we need
		   r1 and r2 do a little pushing and moving of objects. */
		POP3, /* Restore object previously parsed. */
		PUSH1, PUSH2, /* Save r1 and r2 */
		MV13, /* Car object. */
		MV20, /* Cdr object. */
		SYSI, objCons12,
		POP2, POP1,   /* Restore r1 and r2 */
	
	LABEL, "ret",
		RET,
		END
	);
	asmCompileAsmstack(0);
	asmNewCode();
	r1=r0;

	/* r5 eventually gets a new token string buffer (copied from this) when the
		following code runs.  BF: TODO: Implement as a dynamic buffer.*/
	objNewString((u8*)"--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------"
	                  "--------------------------------------------------------------------------------",1600);
	r2=r0;
	objNewString((Str)"ParserMain",10); /* Label */
	asmAsm (
		BRA, 8l,
		r0, /* Label */
		MVI1, r2, /* The string buffer.  Lives in r5. */
		SYSI, objCopyString,
		MV50,
		/* Initialize boolean to 'not parsing a list'. */
		MVI7, 0l,
		/* r1 gets port object from the required parameter to read.  (read portObject). */
		POP1,
		/* Call parser. */
		MVI0, r1,  /* Insert code block object directly via r1 from above. */
		PUSH1A, PUSH1B, PUSH19,
		JAL0,
		POP19, POP1B, POP1A,
		RET,
		END
	);
	asmCompileAsmstack(0);
	asmNewCode();  r1=r0;
	sysNewClosure1Env();
	DBEND();
}

/* Create a read-eval-print-loop code object in machine language.
*/
void wscmCreateRepl (void) {
	DBBEG();
	objNewSymbol ((Str)"\nVM>", 4);  r2=r0;
	objNewSymbol ((Str)"stdin", 5);  r1=r0;  sysTGEFind(); r3=r0; /* The stdin binding, not value incase it changes */
	objNewSymbol ((Str)"read", 4);  r1=r0;  sysTGEFind();  r4=caar(r0); /* The binding value is a closure so we need to car again for the code object. */
	objNewString ((Str)"bye\n", 4);  r5=r0;
	objNewString ((Str)"Entering repl2\n", 14);  r6=r0;
	asmAsm (
		MVI0, r6, /* "Entering REPL\n" */
		PUSH0,
		MVI1, 1l,
		SYSI, syscallDisplay,
		/* Display prompt. */
		LABEL, "repl",
		MVI0, r2, // Prompt
		PUSH0,
		MVI1, 1l,
		SYSI, syscallDisplay,
		/* Call read. */
		PUSH1A, PUSH1B, PUSH19,
		MVI0, r3, // in
		LDI00, 0l,
		PUSH0,
		MVI0, r4, // read wscmCreateRead
		JAL0,
		POP19, POP1B, POP1A,
		/* Done if an #eof parsed. */
		BRTI0, TEOF, ADDR, "done",
		/* Compile expression. */
		SYSI, compSysCompile,
		/* Run code. */
		PUSH1A, PUSH1B, PUSH19,
		JAL0,
		POP19, POP1B, POP1A,
		/* (display ...) */
		PUSH0,
		MVI1, 1l,
		SYSI, syscallDisplay,
		BRA, ADDR, "repl",
		LABEL, "done",
		MVI0, r5, /* Bye message. */
		PUSH0,
		MVI1, 1l,
		SYSI, syscallDisplay,
		//RET,
		SYSI, osUnthread,
		END
	);
	asmCompileAsmstack(0);
	asmNewCode(); r1=r0;
	sysNewClosure1Env();
	DBEND();
}

void wscmInitialize (void) {
 Num i;
	DBBEG();

	compInitialize();

	/* Register objects and pointer addresses with their
	   C source names for object debug dumps. */
	memObjStringSet(sysNewClosure1Env);
	memObjStringSet(objNewVector1);
	memObjStringSet(wscmRecvBlock);
	memObjStringSet(sysTransition);
	memObjStringSet(objListToVector);
	memObjStringSet(objCons12);
	memObjStringSet(objCopyString);
	memObjStringSet(objCons23);
	memObjStringSet(osNewThread);
	memObjStringSet(objCopyInteger);
	memObjStringSet(sysEnvGet);
	memObjStringSet("Not enough arguments to closure");
	memObjStringSet("expectedNoArgs");
	memObjStringSet("Too many arguments to function");
	memObjStringSet(wscmSocketFinalizer);

	/* Create empty thread vector.  All active threads are assigned a number
	   1-1024 and stored here for easy constant time lookup.  The first entry
	   in the thread table is the thread count as an immediate number. */
	objNewVector(MAX_THREADS+1);  rthreads=r0;
	memVectorSet(rthreads, 0, 0); /* Initialize thread count. */
	for (i=1; i<=MAX_THREADS; i++) memVectorSet(rthreads, i, null);

	/* Create empty ready thread doubly linked list. */
	objNewDoublyLinkedListNode (); rready=r0;
	rready=r0;
	memVectorSet(rready, 0, sready);
	memVectorSet(rready, 1, rready);
	memVectorSet(rready, 2, rready);

	rrunning = rready;

	/* Create empty sleeping threads doubly linked list. */
	objNewDoublyLinkedListNode (); rsleeping=r0;
	memVectorSet(rsleeping, 0, ssleeping);
	memVectorSet(rsleeping, 1, rsleeping);
	memVectorSet(rsleeping, 2, rsleeping);

	/* Create empty semaphore/IO blocked threads doubly linked list. */
	objNewDoublyLinkedListNode (); rblocked=r0;
	memVectorSet(rblocked, 0, sblocked);
	memVectorSet(rblocked, 1, rblocked);
	memVectorSet(rblocked, 2, rblocked);

	/* Create empty global environment list. */
	objNewSymbol((Str)"TGE", 3);
	r1=r0;  r2=null;  objCons12();  renv=rtge=r0;

	/* Bind usefull values r2=value r1=symbol. */
	wscmDefineSyscall (syscallError, "error");
	wscmDefineSyscall (syscallQuit, "quit");
	wscmDefineSyscall (sysFun, "fun");
	wscmDefineSyscall (wscmDumpEnv, "env");
	wscmDefineSyscall (sysDumpTGE, "tge");
	wscmDefineSyscall (syscallDebugger, "debugger");
	wscmDefineSyscall (syscallDumpThreads, "threads");
	wscmDefineSyscall (syscallString, "string");
	wscmDefineSyscall (syscallMakeString, "make-string");
	wscmDefineSyscall (syscallSubString, "substring");
	wscmDefineSyscall (syscallStringLength, "string-length");
	wscmDefineSyscall (syscallSerializeDisplay, "serialize-display");
	wscmDefineSyscall (syscallSerializeWrite, "serialize-write");
	wscmDefineSyscall (syscallNumber2String, "number->string");
	wscmDefineSyscall (syscallWrite, "write");
	wscmDefineSyscall (syscallDisplay, "display");
	wscmDefineSyscall (syscallVector, "vector");
	wscmDefineSyscall (syscallMakeVector, "make-vector");
	wscmDefineSyscall (syscallRandom, "random");
	wscmDefineSyscall (syscallEquals, "=");
	wscmDefineSyscall (syscallEqP, "eq?");
	wscmDefineSyscall (syscallStringEqualsP, "string=?");
	wscmDefineSyscall (syscallNotEquals, "!=");
	wscmDefineSyscall (syscallLessThan, "<");
	wscmDefineSyscall (syscallLessEqualThan, "<=");
	wscmDefineSyscall (syscallGreaterThan, ">");
	wscmDefineSyscall (syscallGreaterEqualThan, ">=");
	wscmDefineSyscall (syscallAdd, "+");
	wscmDefineSyscall (syscallMul, "*");
	wscmDefineSyscall (syscallDiv, "/");
	wscmDefineSyscall (syscallLogAnd, "logand");
	wscmDefineSyscall (syscallSqrt, "sqrt");
	wscmDefineSyscall (syscallRemainder, "remainder");
	wscmDefineSyscall (syscallModulo, "modulo");
	wscmDefineSyscall (syscallSub, "-");
	wscmDefineSyscall (syscallTime, "time");
	wscmDefineSyscall (syscallUTime, "utime");
	wscmDefineSyscall (syscallSleep, "sleep");
	wscmDefineSyscall (syscallTID, "tid");
	wscmDefineSyscall (osUnthread, "unthread");
	wscmDefineSyscall (syscallOpenSocket, "open-socket");
	wscmDefineSyscall (syscallOpenStream, "open-stream");
	wscmDefineSyscall (syscallOpenFile, "open-file");
	wscmDefineSyscall (syscallOpenNewFile, "open-new-file");
	wscmDefineSyscall (syscallClose, "close");
	wscmDefineSyscall (syscallRecv, "recv");
	wscmDefineSyscall (syscallReadChar, "read-char");
	wscmDefineSyscall (syscallUnreadChar, "unread-char");
	wscmDefineSyscall (syscallReadString, "read-string");
	wscmDefineSyscall (syscallSend, "send");
	wscmDefineSyscall (syscallSeek, "seek");
	wscmDefineSyscall (syscallTerminalSize, "terminal-size");
	wscmDefineSyscall (syscallStringRef, "string-ref");
	wscmDefineSyscall (syscallStringSetB, "string-set!");
	wscmDefineSyscall (syscallVectorLength, "vector-length");
	wscmDefineSyscall (syscallDebugDumpAll, "dump-heap");
	wscmDefineSyscall (syscallGarbageCollect, "garbage-collect");
	wscmDefineSyscall (syscallDisassemble, "disassemble");
	wscmDefineSyscall (syscallOpenSemaphore, "open-semaphore");
	wscmDefineSyscall (syscallCloseSemaphore, "close-semaphore");
	wscmDefineSyscall (syscallSemaphoreDown, "semaphore-down");
	wscmDefineSyscall (syscallSemaphoreUp, "semaphore-up");
	wscmDefineSyscall (syscallSignal, "signal");
	wscmDefineSyscall (syscallToggleDebug, "toggle-debug");

	/* Create the standard I/O port object */
	r1=(Obj)0; /* Descriptor */
	objNewSymbol ((Str)"stdin", 5);  r2=r3=r0; /* Address and port */
	r4 = sopen; /* State */
	objNewPort(); wscmDefine("stdin");

	r1=(Obj)1;
	objNewSymbol ((Str)"stdout", 6);  r2=r3=r0;
	r4 = sopen;
	objNewPort ();  wscmDefine("stdout");

	r1=(Obj)2;
	objNewSymbol ((Str)"stderr", 6);
	r2=r3=r0;
	r4 = sopen;
	objNewPort(); wscmDefine("stderr");

	/* For fun assign symbol 'characters the internal character vector. */
	r0=characters; wscmDefine("characters");
	r0=staticIntegers; wscmDefine("integers");
	r0=rsymbols; wscmDefine("symbols");
	wscmCreateRead();  wscmDefine("read");
	wscmCreateRepl();  wscmDefine("repl2");
	r0=eof; wscmDefine("#eof");
	objNewInt(42); wscmDefine ("y"); /* It's always nice to have x and y defined with useful values */
	objNewInt(69); wscmDefine ("x");

	/* Signal handler vector */
	i=32;
	objNewVector(i);
	while (i--) { memVectorSet(r0, i, null); }
	wscmDefine("SIGNALHANDLERS");

	DBEND();
}



/*******************************************************************************
 Main
*******************************************************************************/

/* Weird hack that implements a read-eval-print loop as a syscall.  The strange
   thing about it is that the entire process is started from this syscall
   rather than starting everything with a call to the virtual machine vmRun.
   It uses the QUIT opcode which will probably be phased out.  Carefull
   attention must be made when calling wscmSchedule so that this thread is
   guaranteed to be the current running thread.  STDIN must be in blocking
   mode.
*/
void wscmCReadEvalPrintLoop (void) {
 Num i, done=0;
	DBBEG();
	yyrestart(0);   /* Tell scanner to use stdin/0 as input. */
	osNewThread();  /* Create a new thread. */
	osScheduler();  /* Prepare it for the VM. */
	while (!done) {
		renv = rtge; /* Evaluate in TGE */
		fprintf(stderr, "\n== Read and parse ===============\nWSCM>");
		yyparse();/* Expr read into r0. */
		if (eof == r0) done=1;

		fprintf(stderr, "\n== Compile ======================\n");
		sysWrite(r0, stderr);
		rexpr=r0; compCompile(); /* Compile r15/expr into VM runable code in r0. */
		rcode=r0; rip=0;
		vmDebugDumpCode(rcode, stderr);

		fprintf(stderr, "== Execute and return value =====\n");
		vmRun();
		sysDisplay(r0, stderr);

		DBE fprintf(stderr, "== Debug =======================");
		DBE memDebugDumpHeapHeaders(stderr);
		DBE sysWrite(rstack, stderr);
		DBE for (i=memStackLength(rstack); 0<i; --i) sysWrite(memStackObject(rstack, i-1), stdout);
	}
	osUnthread();
	printf ("WEL loop done\n");
	DBEND();
}

/* Read eval print loop via an assembled code block
*/
void wscmASMReadEvalPrintLoop (int argc, char *argv[]) {
	DBBEG();

	/* Bind symbol 'input and assign the stdin port or the
	   filename passed as arg 1 to wscm */
	if (argc==2) {
		/* Create port object, push the argument, set arg count to 1 then
		   make the syscall. */
		objNewSymbol((Str)argv[1], strlen(argv[1]));
		vmPush(r0);  r1=(Obj)1;  syscallOpenFile();  r2=r0;
		/* Assign port to existing binding. */
		if (r2 != eof) {
			objNewSymbol ((Str)"stdin", 5);  r1=r0;  sysTGEFind();
			memVectorSet(r0, 0, r2); /* Rebind stdin with the new port */
		}
	}

	objNewSymbol ((Str)"repl2", 5);  r1=r0;  /* wscmCreateRepl creates repl2 code block */
	sysTGEFind(); r0=caar(r0);

	osNewThread();
	osScheduler();
	vmRun();

	DBEND();
}

/* Uses legacy C-based parsing code to parse a string.  It's then compiled
   into a thread and the virtual machine started up.
*/
void wscmStringReadEvalPrintLoop (void) {
	DBBEG();
	/* Must disable stdio blocking since wscheme implements its own blocking I/O */
	fcntl (0, F_SETFL, fcntl(0, F_GETFL, 0)|O_NONBLOCK);
	yy_scan_string ((Str)
"(let ((FILE:SCM.SCM (open-file \"scm.scm\")))\
  (let wscmLoad~ ((wscmLoadExpr (read FILE:SCM.SCM)))\
    (or (eof-object? wscmLoadExpr) (begin\
      (eval wscmLoadExpr)\
      (wscmLoad~ (read FILE:SCM.SCM)))))\
  (close FILE:SCM.SCM)\
  (send \"\r\nbye.\r\n\" stdout))");
	yyparse(); /* Use the internal parser */
	rexpr=r0; compCompile();
	osNewThread(); /* Create a new thread */
	osScheduler();
	vmRun();
	DBEND();
}



/* Bind wscheme's command line arguments to the vector 'argv
*/
void wscmBindArgs (Num argc, char *argv[]) {
 Num i=0;
	DBBEG();
	objNewVector(argc); r1=r0;
	for (i=0; i<argc; i++) {
		objNewString((u8*)argv[i], strlen(argv[i]));
		memVectorSet(r1, i, r0);
	}
	r0=r1; wscmDefine ("argv"); 
	DBEND();
}


int main (int argc, char *argv[]) {
	setbuf(stdout, NULL);
	signal(SIGPIPE, SIG_IGN);
	srandom((unsigned int)time(NULL));

	wscmInitialize();

	/* Although already activated, just pass in a scheduler handler callback.
	   Called when vmInterrupt is set.  */
	vmInitialize(wscmSchedule, 0);

	wscmBindArgs((Num)argc, argv);

	/* Set the C interrupt alarm handler and start its countdown timer.  The
	   handler will periodicaly set vmInterrupt causing wscmSchdule to be called
	   from the vm module. */
	signal(SIGALRM, wscmSigAlarmHandler);
	wscmSigAlarmReset(); /* Enable scheduler's interrupt timer. */


	/* REPL in a blocking C loop */
	//wscmCReadEvalPrintLoop();  return 0;

	/* REPL in a blocking assembled code block */
	//wscmASMReadEvalPrintLoop(argc, argv);  return 0;

	/* REPL as compiled inlined scheme with asynchronous threads */
	wscmStringReadEvalPrintLoop();  return 0;
}

#undef DB_DESC
#undef DEBUG
