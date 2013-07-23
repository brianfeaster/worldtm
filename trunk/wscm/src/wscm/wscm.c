#define DEBUG_ALL 0
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>    /* random() */
#include <dirent.h>    /* opendir */
#include <sys/types.h> /* opendir */
#include <unistd.h>
#include <math.h>      /* sqrt() */
#include <string.h>    /* strlen() */
#include <fcntl.h>     /* fcntl() */
#include <sys/time.h>  /* gettimeofday() */
#include <time.h>      /* time() */
#include <signal.h>    /* signal() */
#include <sys/ioctl.h> /* ioctl(), struct winsize*/
#include <limits.h>
#include <errno.h>     /* errno() */
#include <assert.h>
#include "comp.h"
#include "os.h"
#include "sys.h"
#include "asm.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"
/* 
   Heap_Statistics
   Useful_functions
   Networking_stuff
   System_calls
   Setup
	Main

 Concepts:
   (Operator operand ...)
   parameters:       Expressions evaluated during a procedure application
   arguments:        Evaluated parameters (operand values) a function is applied to
   formal arguments: Function variables

   free variable:    Non-local binding
   bound variable:   Local binding

   "A bound variable gets a value stored in its location"
*/

extern Num garbageCollectionCount;
void osDebugDumpThreadInfo (void);

/*******************************************************************************
 Heap_Statistics
*******************************************************************************/
Obj rheapinfo;

char *memHeapToChar (Heap *h) {
	return
		&heapOld    == h ? "\e[45mo\e[0m" :
		&heap       == h ? "\e[42m*\e[0m" :
		&heapNew    == h ? "\e[41mn\e[0m" :
		&heapStatic == h ? "\e[44mS\e[0m" : "?";
}

void memHeapToColor (Heap *h) {
		&heapOld    == h ? objNewSymbol((Str)"orange", 6):
		&heap       == h ? objNewSymbol((Str)"green", 5):
		&heapNew    == h ? objNewSymbol((Str)"red", 3):
		&heapStatic == h ? objNewSymbol((Str)"blue", 4) : objNewSymbol((Str)"black", 5);
}

Obj min = (Obj)LONG_MAX; /* The lowest memory address used */
Obj max = (Obj)0; /* The highest memory address used */

void wscmCollectHeapInfo (void) {
 FILE *stream;
 struct winsize win;
 long cols, width, i;
 Heap *a, *b, *c, *d, *x;

	stream = fopen("heap.out","a");

	a = &heapStatic;
	b = &heap;
	c = &heapOld;
	d = &heapNew;

	ioctl(1, TIOCGWINSZ, &win); /* Terminal width/column count */
	cols = win.ws_col;

	/* Sort heaps */
	if (a->start > b->start) {x=b; b=a; a=x; }
	if (b->start > c->start) {x=c; c=b; b=x; }
	if (c->start > d->start) {x=d; d=c; c=x; }
	if (a->start > b->start) {x=b; b=a; a=x; }
	if (b->start > c->start) {x=c; c=b; b=x; }
	if (a->start > b->start) {x=b; b=a; a=x; }

	//fprintf(stream, OBJ" "OBJ" "OBJ" "OBJ, a->start, b->start, c->start, d->start);

	if (max < d->last)  max = d->last;
	if (a->start && b->start) {
		if (a->start < min) min = a->start; /* Keep track of the smallest heap address ever used */
		width = max - min;
		i = (cols * (a->start - min))     / width;  while (i--) fprintf(stream, " ");
		i = (cols * (a->last - a->start)) / width;  while (i--) fprintf(stream, memHeapToChar(a));
		i = (cols * (b->start- a->last))  / width;  while (i--) fprintf(stream, " ");
		i = (cols * (b->last - b->start)) / width;  while (i--) fprintf(stream, memHeapToChar(b));
		i = (cols * (c->start- b->last))  / width;  while (i--) fprintf(stream, " ");
		i = (cols * (c->last - c->start)) / width;  while (i--) fprintf(stream, memHeapToChar(c));
		i = (cols * (d->start- c->last))  / width;  while (i--) fprintf(stream, " ");
		i = (cols * (d->last - d->start)) / width;  i++; while (i--) fprintf(stream, memHeapToChar(d));
		fwrite("\n", 1, 1, stream);
	} else if (b->start) {
		if (b->start < min) min = b->start; /* Keep track of the smallest heap address ever used */
		width = max - min;
		i = (cols * (b->start - min))     / width;  while (i--) fprintf(stream, " ");
		i = (cols * (b->last - b->start)) / width;  while (i--) fprintf(stream, memHeapToChar(b));
		i = (cols * (c->start- b->last))  / width;  while (i--) fprintf(stream, " ");
		i = (cols * (c->last - c->start)) / width;  while (i--) fprintf(stream, memHeapToChar(c));
		i = (cols * (d->start- c->last))  / width;  while (i--) fprintf(stream, " ");
		i = (cols * (d->last - d->start)) / width;  i++; while (i--) fprintf(stream, memHeapToChar(d));
		fwrite("\n", 1, 1, stream);
		/* Create a list of percentages and associated color */
		vmPush(r0); /* Save registers */
		vmPush(r1);
		objNewInt((100*(d->last -d->start)) /width);  r1=objCons(r0, onull);// 4
      memHeapToColor(d); r1=objCons(r0, r1);
		objNewInt((100*(d->start-c->last))  /width);  r1=objCons(r0, r1);   // space
      memHeapToColor(0); r1=objCons(r0, r1);
		objNewInt((100*(c->last - c->start))/width);  r1=objCons(r0, r1);   // 3
      memHeapToColor(c); r1=objCons(r0, r1);
		objNewInt((100*(c->start- b->last)) /width);  r1=objCons(r0, r1);   // space
      memHeapToColor(0); r1=objCons(r0, r1);
		objNewInt((100*(b->last - b->start))/width);  r1=objCons(r0, r1);   // 2
      memHeapToColor(b); r1=objCons(r0, r1);
		//objNewInt((100*(b->start- a->last)) /width);  r1=objCons(r0, r1);   // space
		//objNewInt((100*(a->last - a->start))/width);  r1=objCons(r0, r1);   // 1
		//objNewInt((100*(a->start - min))    /width);  r1=objCons(r0, r1);   // space
		objNewInt((100*(b->start- min))     /width);  r1=objCons(r0, r1);   // space
      memHeapToColor(0); r1=objCons(r0, r1);
		memVectorSet(rheapinfo, 1, objCons(r1, cdr(rheapinfo)));
		r1 = vmPop(); /* Restore registers */
		r0 = vmPop();
	} else {
		if (c->start < min) min = c->start; /* Keep track of the smallest heap address ever used */
		width = max - min;
		i = (cols * (c->start - min))     / width;  while (i--) fprintf(stream, " ");
		i = (cols * (c->last - c->start)) / width;  while (i--) fprintf(stream, memHeapToChar(c));
		i = (cols * (d->start- c->last))  / width;  while (i--) fprintf(stream, " ");
		i = (cols * (d->last - d->start)) / width;  i++; while (i--) fprintf(stream, memHeapToChar(d));
		fwrite("\n", 1, 1, stream);
	}
	fclose(stream);
}


/*******************************************************************************
 Useful_functions
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "WSCM_USEFUL"

/*    r1 <= immedate count of register arguments
     msg <= C string message to display
	    r0 => (Info expression) object
*/
void wscmError (Num regArgs, char const *msg) {
	DBBEG();

	/* Push back args to stack */
	assert(regArgs < 4);
	if (0 == regArgs) { } else
	if (1 == regArgs) { vmPush(r1); } else
	if (2 == regArgs) { vmPush(r1); vmPush(r2); } else
	if (3 == regArgs) { vmPush(r1); vmPush(r2); vmPush(r3); }

	/* Update stack argument count */
	r1 = (Obj)(regArgs);

	/* Create ("error message" arguments...) object */
	osException((Obj)msg);
	DBEND();
}


/* Verify function stack argument count in immediate:r1
*/
Int wscmAssertArgCount0 (char const *funcName) {
	if (0 == (Num)r1) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 0 argument");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCount1 (char const *funcName) {
	if (1 == (Num)r1) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 1 argument");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCount2 (char const *funcName) {
	if (2 == (Num)r1) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 2 arguments");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCount3 (char const *funcName) {
	if (3 == (Num)r1) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 3 arguments");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCountRange0To1 (char const *funcName) {
	if ((0 == (Num)r1) || (1 == (Num)r1)) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 0 or 1 arguments");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCountRange1To2 (char const *funcName) {
	if ((1 == (Num)r1) || (2 == (Num)r1)) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 1 or 2 arguments");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCount1OrMore (char const *funcName) {
	if (1 <= (Num)r1) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 1 or more arguments");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}


/* Check object's type matches.  Requires the argument index and current count
   of arguments popped from stack into registers as they are displayed in the
   error message.
*/
Int wscmAssertArgType (Type t1, Obj o, Num arg, Num argCount, char const *funcName) {
	if (t1 == memObjectType(o)) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect '"STR"' type for argument "NUM, memTypeString(t1), arg);
	wscmError(argCount, funcName);
	return -1;
}

Int wscmAssertArgType2 (Type t1, Type t2, Obj o, Num arg, Num argCount, char const *funcName) {
	if (t1 == memObjectType(o) || t2 == memObjectType(o)) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect '"STR"' or '"STR"' type for argument "NUM,
		memTypeString(t1), memTypeString(t2), arg);
	wscmError(argCount, funcName);
	return -1;
}

/* Special case assert returns false, skipping error/exception call
*/
Int wscmAssertArgType3NoException (Num arg, Type t1, Type t2, Type t3 ,Obj o) {
	if (t1 == memObjectType(o) || t2 == memObjectType(o) || t3 == memObjectType(o)) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect '"STR"' or '"STR"' or '"STR"' type for argument "NUM,
		memTypeString(t1), memTypeString(t2), memTypeString(t3), arg);
	return -1;
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
#define DB_DESC "WSCM_NET"

void wscmSocketFinalizer (Obj o) {
	if (objPortState(o) == sclosed) {
		DB("wscmSocketFinalizer:  Socket already closed");
	} else {
		DB("wscmSocketFinalizer:  Closing socket");
		DBE objDisplay(o, stderr);
		close(objPortDescriptor(o));
		memVectorSet(o, 3, sclosed);
	}
}



void wscmOpenRemoteStream (void) {
	DBBEG();
	if (objPortState(r1) == sconnecting) sysAcceptRemoteStream();

	if (r1!=oeof && objPortState(r1) == sconnecting) {
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
	if (objPortState(r1) == saccepting) {
		sysAcceptLocalStream();
		r1 = r0;
	}

	/* Is it the same socket? */
	if (objPortState(r1) == saccepting) {
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
#define DB_DESC "WSCM_SYSCALL "

/* This can do anything.  I change it mainly for debugging and prototyping. */
void syscallFun (void) {
	//objDisplay(rcode, stderr);
	//memTypeStringDumpAll(stderr);
	//fprintf(stderr, "\n");memDebugDumpObject(rstack, stderr);
	//fprintf(stderr, "\n");objDisplay(rstack, stderr);
	//fprintf(stderr, "pwd=%s\n", get_current_dir_name());
	r0 = otrue;
}

void syscallError (void) {
	osException((Obj)"User Error");
}

void wscmDumpEnv (void) {
	sysDumpEnv(renv);
	r0 = onull;
}

void syscallQuit (void) {
	if ((Num)r1==1) exit(*(int*)vmPop());
	else exit(0);
}

void syscallDumpThreads (void) {
	sysWrite(rthreads, stderr); /* TODO make this pretty */
	r0 = otrue;
}

void syscallString (void) {
 Num totalLen=0, s=0, len;
	DBBEG();
	/* Verify each arg is a string and sum the lengths */
	while (s < (Int)r1) {
		r0 = memStackObject(rstack, s++);
		if (wscmAssertArgType3NoException((Num)r1 - s + 1, TSTRING, TCHAR, TSYMBOL, r0)) {
			osException((Obj)__func__);
			goto ret;
		}

		totalLen += memObjectLength(r0);
	}
	/* New string is built in reverse.  Even works if
	   substrings are "" since strcpy length is 0. */
	r0 = totalLen ? memNewArray(TSTRING, totalLen) : onullstr;
	while (s--) {
		totalLen -= (len = memObjectLength(r1=vmPop()));
		memcpy(r0+totalLen, r1, len);
	}
	ret:
	DBEND();
}

void syscallMakeString (void) {
 Num len;
	DBBEG();
	if (wscmAssertArgCountRange1To2(__func__)) goto ret;

	if (2 == (Num)r1) {
		/* Expression has a fill character */
		r2 = vmPop(); /* Fill char */
		r1 = vmPop(); /* New length */

		if (wscmAssertArgType(TINTEGER, r1, 1, 2, __func__) ||
			 wscmAssertArgType(TCHAR,    r2, 2, 2, __func__)) goto ret;

		len = *(Num*)r1;
		objNewString(NULL, len);
		while (len--) ((Chr*)r0)[len] = *(Chr*)r2;
	} else {
		/* Expression has no fill character */
		len = *(Num*)(r1 = vmPop());

		if (wscmAssertArgType(TINTEGER, r1, 1, 1, __func__)) goto ret;

		objNewString(NULL, len=*(Num*)r1);
	}

ret:
	DBEND();
}

void syscallSubString (void) {
 Num start, end;
	DBBEG();
	if (wscmAssertArgCount3(__func__)) goto ret;

	r3 = vmPop();
	r2 = vmPop();
	r1 = vmPop();

	if (wscmAssertArgType(TSTRING,  r1, 1, 3, __func__) ||
	    wscmAssertArgType(TINTEGER, r2, 2, 3, __func__) ||
	    wscmAssertArgType(TINTEGER, r3, 3, 3, __func__)) goto ret;

	end   = *(Num*)r3;
	start = *(Num*)r2;

	if ((start < end) && (end <= memObjectLength(r1))) {
		DB("Creating substring object");
		objNewString(NULL, end-start);
		memcpy(r0, r1+start, end-start);
	} else if (end == start) {
		DB("Zero len");
		r0 = onullstr;
	} else {
		DB("invalid range");
		wscmError(3, "Substring range invalid");
	}

ret:
	DBEND();
}

void syscallStringLength (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r1 = vmPop();
	//if (wscmAssertArgType(TSTRING, r1, 1, 1, __func__)) goto ret;
	objNewInt((Int)memObjectLength(r1));
ret:
	DBEND();
}

/* Returns an array object in r0 (ignore the type) reprsenting an external
   representation of the object.  Only simple object types are converted. 
   Complex ones or immediate pointers below 2^20 are shown as just hex
   addresses.
*/
void syscallSerializeDisplay (void) {
 Obj o;
 static u8 buff[8192];
 Int ret;
	DBBEG(" <= ");

	if (wscmAssertArgCount1(__func__)) goto ret;
	r0 = vmPop();

	DBE objDisplay(r0, stderr);

	if ((Num)r0 < 0x100000l) {
		ret = sprintf((char*)buff, "#"OBJ, (Num)r0);
		assert(0 < ret);
		objNewString(buff, (Num)ret);
	} else switch (memObjectType(r0)) {
		case TINTRINSIC :
			if      (onull == r0)  r0 = snull;
			else if (ofalse == r0) r0 = sfalse;
			else if (otrue == r0)  r0 = strue;
			else if (oeof == r0)   r0 = seof;
			else assert(!"Unknown intrinsic object to serialize");
			break;
		case TCHAR   : 
		case TSYMBOL : 
		case TSTRING : 
			break;
		case TINTEGER:
			sysSerializeInteger(*(Int*)r0, 10);
			break;
		case TREAL   : 
			ret = sprintf((char*)buff, "%.2f", *(Real*)r0);
			assert(0<ret);
			objNewString(buff, (Num)ret);
			break;
		case TPORT:
			if (memIsObjectType(memVectorObject(r0, 1), TINTEGER))
				ret = sprintf((char*)buff, "#PORT<DESC:"NUM" ADDR:"NUM" PORT:"NUM" STATE:"STR">",
				   (Num)memVectorObject(r0, 0),
				   *(Num*)memVectorObject(r0, 1),
				   *(Num*)memVectorObject(r0, 2),
				   objPortState(r0));
			else {
				o = memVectorObject(r0, 2); /* Consider the flags/port/index field */
				ret = sprintf((char*)buff, "#PORT<DESC:"NUM" ADDR:%.*s PORT:"NUM" STATE:"STR">",
				   (Num)memVectorObject(r0, 0),
				   memObjectLength(memVectorObject(r0, 1)), (char*)memVectorObject(r0, 1), /* %* format takes two args */
					memIsObjectValid(o)?*(Num*)o:(Num)o,
					objPortState(r0));
			}
			objNewString(buff, (Num)ret);
/*
			count += fprintf(stream, " STATE:");
			count += sysWriteR(objPortState(a), 0, stream, max);
			count += fprintf(stream, " NEXT:");
			count += sysWriteR(memVectorObject(a, 4), 0, stream, max);
			count += fprintf(stream, " FINALIZER:");
			count += sysWriteR(memVectorObject(a, 5), 0, stream, max);
			count += fprintf(stream, ">");
*/
			break;
		default      :
			ret = sprintf((char*)buff, HEX, (Num*)r0);
			assert(0<ret);
			objNewString(buff, (Num)ret);
			break;
	}
ret:
	DBEND();
}

void syscallSerializeWrite (void) {
 static Chr buff[8192];
 Chr c;
 Int ret;
 Num len, i;
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r0 = vmPop();
	if ((Num)r0 < 0x100000l) {
		ret = sprintf((char*)buff, "#"HEX, (Num)r0);
		assert(0<ret);
		objNewString(buff, (Num)ret);
	} else switch (memObjectType(r0)) {
		case TINTRINSIC :
			if      (onull == r0)  r0 = snull;
			else if (ofalse == r0) r0 = sfalse;
			else if (otrue == r0)  r0 = strue;
			else if (oeof == r0)   r0 = seof;
			else assert(!"Unknown intrinsic object to serialize");
			break;
		case TSYMBOL : 
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
ret:
	DBEND();
}

void syscallNumber2String (void) {
 Int num;
 Num base;
	DBBEG();
	if (wscmAssertArgCountRange1To2(__func__)) goto ret;
	base = (Num)r1==2 ? *(Num*)vmPop() : 10; /* Default base is 10. */
	num = *(Int*)vmPop();
	sysSerializeInteger (num, base);
ret:
	DBEND();
}

/* For internal use only.  Probably to be phased out in favor of a VM implementation.
*/
void syscallWrite (void) {
 Int fd=1;
 FILE *stream=NULL;
	DBBEG();
	if (wscmAssertArgCountRange1To2(__func__)) goto ret;
	if ((Int)r1==2) fd=*(Int*)vmPop(); /* File descriptor. */
	if (fd==1) stream = stdout;
	if (fd==2) stream = stderr;
	assert(NULL != stream);

	sysWrite(r0=vmPop(), stream);
ret:
	DBEND();
}

void syscallDisplay (void) {
 Int fd=1;
 FILE *stream=NULL;
	DBBEG();

	if (wscmAssertArgCountRange1To2(__func__)) goto ret;
	if ((Int)r1==2) fd=*(Int*)vmPop(); /* Descriptor. */

	if (fd==1) stream = stdout;
	if (fd==2) stream = stderr;
	assert(NULL != stream);

	objDisplay(r0=vmPop(), stream);
ret:
	DBEND();
}

void syscallVector (void) {
 Num l=(Num)r1;
	DBBEG();
	if (l==0) r0=onullvec;
	else {
		objNewVector(l);
		while (l--) memVectorSet(r0, l, vmPop());
	}
	DBEND();
}

void syscallMakeVector (void) {
 Num len;
	DBBEG();
	if (wscmAssertArgCountRange1To2(__func__)) goto ret;
	if (1 == (Num)r1) {
		r1 = vmPop(); /* length */
		if (wscmAssertArgType(TINTEGER, r1, 1, 1, __func__)) goto ret;
		len = *(Num*)r1;
		if (0 < len && len <= 0x100000) { /* TODO magic vector size limit */
			objNewVector(len);
			while (len--) memVectorSet(r0, len, r2);
		} else if (0 == len) {
			r0 = onullvec;
		} else {
			wscmError(1, "make-vector invalid size");
		}
	} else {
		r2 = vmPop(); /* Fill object */
		r1 = vmPop(); /* length */
		if (wscmAssertArgType(TINTEGER, r1, 1, 2, __func__)) goto ret;
		len = *(Num*)r1;
		if (0 < len) {
			objNewVector(len);
			while (len--) memVectorSet(r0, len, r2);
		} else if (0 == len) {
			r0 = onullvec;
		} else {
			wscmError(2, "make-vector invalid size");
		}
	}
ret:
	DBEND();
}

void syscallRandom (void) {
	DBBEG();
	if (wscmAssertArgCountRange0To1(__func__)) goto ret;
	if ((Int)r1 == 1)
		objNewInt(random() % *(Int*)vmPop());
	else
		objNewInt(random());
ret:
	DBEND();
}

void syscallEqP (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r0 = (vmPop() == vmPop()) ? otrue : ofalse;
ret:
	DBEND();
}

/* Numerical equivalence. Returns false if either are not integers. */
void syscallEquals (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r2 = vmPop();
	r1 = vmPop();
	r0 = TINTEGER == memObjectType(r1)
	     && TINTEGER == memObjectType(r2)
	     && *(Int*)r1 == *(Int*)r2
		? otrue : ofalse;
	/*
	if (wscmAssertArgType(TINTEGER, r1, 1, 2, __func__) ||
	    wscmAssertArgType(TINTEGER, r2, 2, 2, __func__)) goto ret;
	r0 = *(Int*)r1 == *(Int*)r2 ? otrue : otrue;
	*/
ret:
	DBEND();
}

void syscallNotEquals (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r2 = vmPop();
	r1 = vmPop();
	r0 = TINTEGER == memObjectType(r1)
	     && TINTEGER == memObjectType(r2)
	     && *(Int*)r1 == *(Int*)r2
		? ofalse : otrue;
	/*
	if (wscmAssertArgType(TINTEGER, r1, 1, 2, __func__) ||
	    wscmAssertArgType(TINTEGER, r2, 2, 2, __func__)) goto ret;
	r0 = *(Int*)r1 == *(Int*)r2 ? false : otrue;
	*/
ret:
	DBEND();
}

void syscallStringEqualsP (void) {
 Num len;
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r1 = vmPop();
	r0 = vmPop();
	r0 = TSTRING == memObjectType(r0)
	     && TSTRING == memObjectType(r1)
	     && memObjectLength(r0) == (len=memObjectLength(r1))
	     && !strncmp (r0, r1, len) ? otrue : ofalse;
ret:
	DBEND();
}

void syscallLessThan (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r0 = (*(Int*)vmPop() > *(Int*)vmPop()) ? otrue : ofalse;
ret:
	DBEND();
}

void syscallLessEqualThan (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r0 = (*(Int*)vmPop() >= *(Int*)vmPop()) ? otrue : ofalse;
ret:
	DBEND();
}

void syscallGreaterThan (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r1 = vmPop();
	r0 = vmPop();
	if (TREAL == memObjectType(r0) && TREAL == memObjectType(r1))
		r0 = (*(Real*)r1 < *(Real*)r0) ? otrue : ofalse;
	else
		r0 = (*(Int*)r1 < *(Int*)r0) ? otrue : ofalse;
ret:
	DBEND();
}

void syscallGreaterEqualThan (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r1 = vmPop();
	r0 = vmPop();
	if (TREAL == memObjectType(r0) && TREAL == memObjectType(r1))
		r0 = (*(Real*)r1 <= *(Real*)r0) ? otrue : ofalse;
	else
		r0 = (*(Int*)r1 <= *(Int*)r0) ? otrue : ofalse;
ret:
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

/* Multiply all but the first, then divide the first by the product.
*/
void syscallDiv (void) {
 Int product=1, divisor;
	DBBEG();
	while (1 < (Int)r1--) product *= *(Int*)vmPop();
	divisor = *(Int*)vmPop();
	objNewInt(product ? divisor/product : INT_MAX);
	DBEND();
}

void syscallShiftLeft (void) {
 Int a, b;
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	b = *(Int*)vmPop();
	a = *(Int*)vmPop();
	objNewInt(a<<b);
ret:
	DBEND();
}

void syscallShiftRight (void) {
 Int a, b;
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	b = *(Int*)vmPop();
	a = *(Int*)vmPop();
	objNewInt(a>>b);
ret:
	DBEND();
}

void syscallLogAnd (void) {
 Int a, b;
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	a = *(Int*)vmPop();
	b = *(Int*)vmPop();
	objNewInt(a&b);
ret:
	DBEND();
}

void syscallSqrt (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	objNewInt((Int)sqrt((double)*(Int*)vmPop()));
ret:
	DBEND();
}

void syscallRemainder (void) {
 Int a, b;
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	b = *(Int*)vmPop();
	a = *(Int*)vmPop();
	objNewInt(a%b);
ret:
	DBEND();
}

void syscallModulo (void) {
 Int a, b;
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	b = *(Int*)vmPop();
	a = *(Int*)vmPop();
	/* If one argument is negative. */
	if ((a<0) ^ (b<0))
		objNewInt((b+a%b)%b);
	else
		objNewInt(a%b);
ret:
	DBEND();
}

void syscallSub (void) {
 Int sum=0;
	DBBEG();
	if (wscmAssertArgCount1OrMore(__func__)) goto ret;
	if (1 == (Int)r1)
		sum = -*(Int*)vmPop();
	else {
		while (--r1) sum += *(Int*)vmPop();
		sum = *(Int*)vmPop() - sum;
	}
	objNewInt(sum);
ret:
	DBEND();
}

void syscallSleep (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	osSleepThread();
ret:
	DBEND();
}

/* Return thread ID. */
void syscallTID (void) {
	DBBEG();
	if (wscmAssertArgCount0(__func__)) goto ret;
	r0 = osThreadId(rrunning);
ret:
	DBEND();
}


void syscallOpenSocket (void) {
	DBBEG();

	if (wscmAssertArgCountRange1To2(__func__)) goto ret;

	if (1 == (Int)r1) {
		r1 = vmPop(); /* Consider socket number */
		if (wscmAssertArgType (TINTEGER, r1, 1, 1, __func__)) goto ret;
		sysOpenLocalSocket(); /* pass in port via r1 */
		if (memObjectType(r0) == TPORT) {
			/* Create and set the socket's "close" finalizer */
			r1 = r0;
			r0 = memNewFinalizer();
			memVectorSet(r0, 0, wscmSocketFinalizer);
			memVectorSet(r0, 1, r1);
			memVectorSet(r1, 5, r0);
			r0 = r1; /* Return the port in r0 */
		}
	} else if (2 == (Int)r1) {
		r1 = vmPop();
		r2 = vmPop();
		if (wscmAssertArgType (TSTRING,  r2, 1, 2, __func__) ||
		    wscmAssertArgType (TINTEGER, r1, 2, 2, __func__)) goto ret;
		sysOpenRemoteSocket(); /* pass in port via r1, host via r2 */
	}
ret:
	DBEND();
}

void syscallOpenStream (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;

	r1 = vmPop();
	if (memObjectType(r1) != TPORT) {
		wscmError(1, "Invalid arguments to open-stream:");
		goto ret;
	}

	if (objPortState(r1) == sconnecting)
		wscmOpenRemoteStream();
	else if (objPortState(r1) == saccepting)
		wscmOpenLocalStream();
	else if (objPortState(r1) == sclosed)
		r0=oeof;
	else {
		wscmError(1, "Invalid port state to open-stream");
	}
 ret:
	DBEND();
}

void syscallOpenFile (void) {
 Num silent=0;
 Int len;
 char buff[0x200];
	DBBEG();
	if (wscmAssertArgCountRange1To2(__func__)) goto ret;

	if ((Num)r1==2) { /* An extra arg forces a silent sysOpenFile() call */
		vmPop();
		silent=1;
	}
	r1 = vmPop();
	sysOpenFile(O_RDWR, S_IRUSR|S_IWUSR, silent); /* r1 contains filename object, returns port in r0, original filename in r2 */
	
	if (!memIsObjectType(r0, TPORT)) {
		if (silent) {
			r0 = ofalse;
		} else {
			if (memIsObjectType(r0, TSTRING)) {
				/* Error using the string passed */
				wscmError(0, r0);
			} else {
				/* Error using C's error code */
				len = sprintf(buff, "Unable to open file \""STR"\".  ["INT":"STR"]", r2, errno, strerror(errno));
				assert(len<0x200);
				wscmError(0, buff);
			}
		}
	}
ret:
	DBEND();
}

void syscallOpenString (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;

	r1 = vmPop();
	sysOpenString(); /* r1 contains string buffer to read */
	
	if (!memIsObjectType(r0, TPORT)) {
		if (memIsObjectType(r0, TSTRING)) {
			wscmError(0, "ERROR:: syscallOpenString");
		}
	}
ret:
	DBEND();
}

void syscallOpenNewFile (void) {
 Num silent=0;
 Int len;
 char buff[0x200];
	DBBEG();
	if (wscmAssertArgCountRange1To2(__func__)) goto ret;
	if ((Num)r1==2) { /* If an extra arg is passed, make it a silent call */
		vmPop();
		silent=1;
	}
	r1 = vmPop();
	sysOpenFile(O_CREAT|O_TRUNC|O_RDWR, S_IRUSR|S_IWUSR, silent);

	if (!memIsObjectType(r0, TPORT)) {
		if (silent) {
			r0 = ofalse;
		} else {
			if (memIsObjectType(r0, TSTRING)) {
				/* Error using the string passed */
				wscmError(0, r0);
			} else {
				/* Error using C's error code */
				len = sprintf(buff, "Unable to open new file \""STR"\".  ["INT":"STR"]", r2, errno, strerror(errno));
				assert(len<0x200);
				wscmError(0, buff);
			}
		}
	}
ret:
	DBEND();
}

void syscallReadDirectory (void) {
 DIR *dir;
 Int ret;
 Num len;
 char buff[0x200];
 struct dirent *de;
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r1 = vmPop();
	if (wscmAssertArgType(TSTRING, r1, 1, 1, __func__)) goto ret;

	/* Scheme string to C string */
	len = memObjectLength(r1);
	strncpy(buff, (char*)r1, len);
	buff[len] = 0;

	if (!sysCanonicalizePath(buff)) { /* Sandbox verifies path in 'buff' is valid */
		wscmError(1, "Invalid directory path");
		goto ret;
	}

	dir = opendir(r1);

	if (NULL == dir) {
		r0 = onull;
		ret = sprintf(buff, "Unable to open directory \""STR"\".  ["INT":"STR"]", r1, errno, strerror(errno));
		assert(ret<0x200);
		wscmError(0, buff);
		goto ret;
	} else {
		// Build a list of directory names as strings
		len= 0;
		de = readdir(dir);
		while (de) {
			if (DT_DIR == de->d_type)
				objNewStringString((Str)de->d_name, strlen(de->d_name), (Str)"/", 1);
			else
				objNewString((Str)de->d_name, strlen(de->d_name));
			vmPush(r0);
			++len;
			de = readdir(dir);
		}
		closedir(dir);
		r0 = onull;
		while (len--) {
			r1=vmPop();
			r2=r0;
			objCons12();
		}
	}
ret:
	DBEND();
}

void syscallClose(void) {
	DBBEG();
	r0 = vmPop();
	if (memObjectType(r0) != TPORT) {
		printf ("WARNING: syscallClose: not a socket: ");
		objDisplay(r0, stdout);
	} if (objPortState(r0) == sclosed) {
		printf ("WARNING: syscallClose: socket already closed: ");
		//objDisplay(r0, stdout);
	} else {
		/* Don't try and close a port-string */
		if (onullstr != memVectorObject(r0, 0)) {
			close(*(int*)r0);
		}
		memVectorSet(r0, 3, sclosed);
	}
	DBEND();
}

/* Given a byte count, timeout (integer or #f) and port, read
   from the port count bytes or if 0 any number of bytes
*/
void syscallRecv (void) {
 Num len;
	DBBEG();
	if (wscmAssertArgCount3(__func__)) goto ret;

	r1=vmPop(); /* Port object */
	r2=vmPop(); /* Timeout (integer or #f) */
	r3=vmPop(); /* Byte count integer */

	if (wscmAssertArgType(TINTEGER, r3, 1, 3, __func__)) goto ret;

	if (r2 != ofalse && !memIsObjectType(r2, TINTEGER)) {
		fprintf(stderr, "\r\nWSCM-ASSERT::Expect '"STR"' type or '#f' for argument 2", memTypeString(TINTEGER));
		wscmError(3, __func__);
		goto ret;
	}
	if (wscmAssertArgType(TPORT, r1, 3, 3, __func__)) goto ret;

	/* r3 gets the byte count.  It's then assigned an empty string
	   character buffer with the specified length of characters to
	   read.  A count of zero (null string) implies read any count
	   of bytes. */
	len = *(Num*)r3; /* Consider immediate integer value */

	if (MEMOBJECTMAXSIZE <= len) {
		fprintf(stderr, "\r\nWSCM-ASSERT::Expect byte count to be less than "HEX, MEMOBJECTMAXSIZE);
		wscmError(3, __func__);
		goto ret;
	}

	r3 = len ? memNewArray(TSTRING, len) : onullstr;

	osRecvBlock();
ret:
	DBEND();
}

/* (read-char timeout port)
   timeout : #f     blocking read
              0     non-blocking read
              1000  block for at most 1000ms
*/
void syscallReadChar (void) {
 Num idx;
 Str str;
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r1=vmPop(); /* Port object */
	r2=vmPop(); /* Timout */
	if (memObjectType(r1) != TPORT) {
		printf ("WARNING: syscallReadChar: not a port object: ");
		objDisplay(r1, stdout);
		r0 = oeof;
	} else if (onullstr == memVectorObject(r1, 0)) {
	/* Port object is a port-string.  Return the next char and increment
	   internal pointer.  EOF returned if entire string read. */
		r0 = memVectorObject(r1, 4);
		if (ofalse != r0)
			/* Return push-back char */
			memVectorSet(r1, 4, ofalse);
		else {
			str = memVectorObject(r1,1);
			idx = (Num)memVectorObject(r1, 2);
			if (idx == memObjectLength(str)) {
				r0 = oeof; /* Return EOF */
			} else {
				r0 = objIntegerToChar(str[idx]);
				memVectorSet(r1, 2, (Obj)(idx+1)); /* Increment index */
			}
		}
	} else {
		r3=onull;  /* tells recv that we just want a character. */
		osRecvBlock();
	}
ret:
	DBEND();
}

void syscallUnreadChar (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r1=vmPop(); /* Port. */
	r0=vmPop(); /* Character. */
	if (memObjectType(r1) != TPORT) {
		printf ("WARNING: syscallUnreadChar: arg2 not a port object: ");
		objDisplay(r1, stdout);
		r0 = oeof;
	} else if (memObjectType(r0) != TCHAR) {
		printf ("WARNING: syscallUnreadChar: arg1 not a char: ");
		objDisplay(r0, stdout);
		r0 = oeof;
	} else
		memVectorSet(r1, 4, r0);
ret:
	DBEND();
}

/* Call internal C parser on passed string.  Doesn't block so an EOF
   can be returned if the expression is incomplete or empty.
*/
void syscallReadString (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r1=vmPop(); /* String to parse. */
	yy_scan_bytes(r1, memObjectLength(r1));
	yyparse();
ret:
	DBEND();
}

/* Given string and port, send string to port.  Thread blocks until all is
   sent.
*/
void syscallSend (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	/* r1 gets port object. */
	r1=vmPop();
	/* r2 gets string object. */
	r2=vmPop();
	/* Count sent already.  Should be initialized to 0. */
	r3=0;
	if (memObjectType(r1) != TPORT) {
		printf ("WARNING: syscallSend: not a socket is (type "HEX02"): ", memObjectType(r1));
		*(int*)0=0;
		r0 = oeof;
	} else if (memIsObjectValid(r2)) {
		sysSend();
		if (r0 == ofalse) { /* Still more to send. */
			osUnRun();
			osMoveToQueue(rrunning, rblocked, swriteblocked);
			osScheduler();
		}
	} else {
		fprintf (stderr, "WARNING: trying to send:");
		memDebugDumpObject(r2, stderr);
		syscallDebugger();
		r0 = ofalse;
	}
ret:
	DBEND(" => r0:");
	DBE memDebugDumpObject(r0, stderr);
}

void syscallSeek (void) {
 int fd, offset, whence;
	DBBEG();
	if (wscmAssertArgCount3(__func__)) goto ret;
	whence = *(int*)vmPop(); /* Whence */
	assert(whence==SEEK_SET || whence==SEEK_CUR || whence==SEEK_END);
	offset = *(int*)vmPop(); /* Offset */
	fd=*(int*)vmPop(); /* Descriptor. */
	lseek(fd, offset, whence);
ret:
	DBEND();
}

void syscallFileStat (void) {
 struct stat buf;
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;

	r1 = vmPop();
	if (wscmAssertArgType(TPORT, r1, 1, 1, __func__)) goto ret;

	fstat(objPortDescriptor(r1), &buf);

	objNewVector(13);
	r1 = r0;
	objNewInt((Int)buf.st_dev); memVectorSet(r1, 0, r0); /* dev_t     st_dev;     ID of device containing file */
	objNewInt((Int)buf.st_ino); memVectorSet(r1, 1, r0); /* ino_t     st_ino;     inode number */
	objNewInt(buf.st_mode); memVectorSet(r1, 2, r0);     /* mode_t    st_mode;    protection */
	objNewInt((Int)buf.st_nlink); memVectorSet(r1, 3, r0);/*nlink_t   st_nlink;   number of hard links */
	objNewInt(buf.st_uid); memVectorSet(r1, 4, r0);      /* uid_t     st_uid;     user ID of owner */
	objNewInt(buf.st_gid); memVectorSet(r1, 5, r0);      /* gid_t     st_gid;     group ID of owner */
	objNewInt((Int)buf.st_rdev); memVectorSet(r1, 6, r0);/* dev_t     st_rdev;    device ID (if special file) */
	objNewInt(buf.st_size); memVectorSet(r1, 7, r0);     /* off_t     st_size;    total size, in bytes */
	objNewInt(buf.st_blksize); memVectorSet(r1, 8, r0);  /* blksize_t st_blksize; blocksize for file system I/O */
	objNewInt(buf.st_blocks); memVectorSet(r1, 9, r0);   /* blkcnt_t  st_blocks;  number of 512B blocks allocated */
	objNewInt(buf.st_atime); memVectorSet(r1,10, r0);    /* time_t    st_atime;   time of last access */
	objNewInt(buf.st_mtime); memVectorSet(r1,11, r0);    /* time_t    st_mtime;   time of last modification */
	objNewInt(buf.st_ctime); memVectorSet(r1,12, r0);    /* time_t    st_ctime;   time of last status change */
	r0 = r1;

ret:
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
	if (wscmAssertArgCount2(__func__)) goto ret;
	offset = *(Int*)vmPop();
	o=vmPop();
	/* A (negative? offset) implies (absolute offset) from end. */
	if (offset < 0) offset = (Int)memObjectLength(o)+offset;
	r0 = objIntegerToChar(*((Chr*)o+offset));
ret:
	DBEND();
}

void syscallStringSetB (void) {
 Obj strObj;
 Int offset;
 Chr ch;
	DBBEG();
	if (wscmAssertArgCount3(__func__)) goto ret;
	ch = *(Chr*)vmPop();
	offset=*(Int*)vmPop();
	strObj = vmPop();
	/* A (negative? offset) implies (absolute offset) from end. */
	if (offset < 0) offset = (Int)memObjectLength(strObj)+offset;
	assert(0<=offset);
	memArraySet(strObj, (Num)offset, ch);
ret:
	DBEND();
}

void syscallVectorLength (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r0 = vmPop();
	objNewInt(r0==onullvec?0:(Int)memObjectLength(r0));
ret:
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
	if ((Int)r6 == 2) r0 = onullstr;
	else {
		objNewString(NULL, len);
   	memcpy(r0, r5, len);
	}
	DBEND();
}
void sysNewCharacter (void) {
	DBBEG();
	r0 = objIntegerToChar(((u8*)r5)[2]);
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
	if (wscmAssertArgCount0(__func__)) goto ret;
	memDebugDumpAll(NULL);
ret:
	DBEND();
}

void syscallGarbageCollect (void) {
	DBBEG();
	if (wscmAssertArgCount0(__func__)) goto ret;
	memGarbageCollect();
	objNewInt((Int)garbageCollectionCount);
ret:
	DBEND();
}

void syscallDisassemble (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r0 = vmPop();
	if (memObjectType(r0) == TCLOSURE) {
		if (cdr(r0) != onull) sysDumpEnv(cdr(r0));
		r0 = car(r0); /* Consider closure's code block */
	}
	objDisplay(r0, stderr);
ret:
	DBEND();
}

/* Semaphores are just immediate numbers.
*/
void syscallOpenSemaphore (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	osOpenSemaphore();
ret:
	DBEND();
}

void syscallCloseSemaphore (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r0 = vmPop();
	if (memVectorObject(r0,0)==ofalse) /* TODO this vector-ref should be abstracted using semaphore accessors */
		r0 = ofalse; /* Semaphore already closed so return failure */
	else {
		memVectorSet(r0, 0, ofalse);
		osUnblockSemaphoreBlocked(r0, 1); /* 1 means unblock all threads on this semaphore */
		r0 = otrue;
	}
ret:
	DBEND();
}

void syscallSemaphoreDown (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r1 = vmPop();
	sysSemaphoreDown();
	if (r0 == r1) {
		/* Block thread on this semaphore.  Semaphore remains in r1. */
		DB("Blocking thread "NUM" on semaphore value "NUM, *(Num*)osThreadId(rrunning), *(Int*)r1);
		osUnRun();
		osMoveToQueue(rrunning, rblocked, ssemaphore); /* TODO create a separate semaphore blocked queue */
		osScheduler();
	}
ret:
	DBEND();
}


void syscallSemaphoreUp (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r1 = vmPop();
	sysSemaphoreUp();
	if (r0 == r1) {
		/* Unblock a thread blocked by this semaphore. If after incrmenting the
	   	counter the semaphore is still blocking, then this means one
	   	of the blocked threads can be awakened. */
		osUnblockSemaphoreBlocked(r1, 0);
	}
ret:
	DBEND();
}


void syscallTime (void) {
 struct timeval tv;
	DBBEG();
	if (wscmAssertArgCount0(__func__)) goto ret;
	gettimeofday(&tv, NULL);
	objNewInt(tv.tv_sec);
ret:
	DBEND();
}


void syscallUTime (void) {
	DBBEG();
	if (wscmAssertArgCount0(__func__)) goto ret;
	objNewInt(sysTime());
ret:
	DBEND();
}


void syscallToggleDebug (void) {
	r0 = odebug = (otrue == odebug) ? ofalse : otrue;
}



void wscmSigAlarmHandler (int sig) {
	vmInterrupt = 1;
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
	vmInterrupt = 1; /* Let virtual machine know an interrupt occured */
	signalFlag=1; /* Let scheduler know a signal handler needs to be instantiated */
	DBEND();
}

/* Syscall to set a signal handler for a specified signal number.
   The signal handler will always be the wscmSignalHandler().  See above. */
void syscallSignal (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r0 = vmPop();
	signal(*(int*)r0, wscmSignalHandler); /* Have to cast to an uint64 to int32 */
ret:
	DBEND();
}

#undef DB_DESC
#undef DEBUG



/*******************************************************************************
 Setup
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "WSCM_INIT "

/* Given  - r1:port object  r3:char object or eof
            r4:current state (see scanner.c)   r5:yytext  r6:yylen
   Return - r4:next state   r5:yytext   r6:yylen
            r0:final state if complete token scanned (reached final state).
*/
void wscmSysTransition (void) {
	DBBEG(" <= state:r4="HEX"  char:r3=", r4);
	DBE memDebugDumpObject(r3, stderr);

	/* Make the transition on char in r3 given state in r4. */
	if (oeof == r3)
		r4 = (Obj)transition(256, (Num)r4); /* 256 is treated as the EOF character */
	else
		r4 = (Obj)transition(*(Num*)r3, (Num)r4);
	DB("transition() returned new state => "HEX, r4);

	/* Append char to scanned token and inc yylen. */
	*((u8*)r5+(Num)r6++) = *(u8*)r3;

	/*  Reset yylen to 0 if we ever end up back to the initial state.*/
	if ((Num)r4 == 0x00l) {
		r6 = 0l;
	} else if ((Int)r4 & FINALSTATE) {
		/* Push back character to stream if in pushback state and not eof. */
		if (((Num)r4 & PUSHBACK) && (oeof != r3)) {
			r6--;
			memVectorSet(r1, 4, objIntegerToChar(*(Num*)r3));
		}
		r0 = r4;
	}

	DBEND(" final state r4="HEX"  r0="HEX, r4, r0);
}


/* Call to the read scheme closure will setup registers for calls to internal
   scanning and parsing code blocks.  This function creates that closure.
	Create atom parser code block.  Modeled after the C equivalent in scanner.c

   scanner  - r4:state   r5:token buffer   r6:token length  r7:islist boolean

	Uses -- r1:port object   r2:timeout=0  r3:wantChar=()  r4:used by recv
	        r4:state   r5:token buffer   r6:token length   r7:is list bool.

	osRecvBlock in: r1 port  r2 timeout  r3 string buffer
	           use: r4
	           out: r0 string
*/


void wscmCreateRead (void) {
 Obj Lscan, Ldot, Leof, Lvector, Ldone, Lquote, Lunquotesplicing, Lunquote, Lquasiquote, Lopenparen, Lcharacter, Lsymbol, Lstring, Linteger, Lreal, Loct, Lhex, Lbinary, Lfalse, Ltrue, Ldefault, Lret;
	DBBEG();
	asmInit();
	objNewString((Str)"Parser",6); /* Label */
	Lscan = asmNewLabel();
	Ldot = asmNewLabel();
	Leof = asmNewLabel();
	Lvector = asmNewLabel();
	Ldone = asmNewLabel();
	Lquote = asmNewLabel();
	Lunquotesplicing = asmNewLabel();
	Lunquote = asmNewLabel();
	Lquasiquote = asmNewLabel();
	Lopenparen = asmNewLabel();
	Lcharacter = asmNewLabel();
	Lsymbol = asmNewLabel();
	Lstring = asmNewLabel();
	Linteger = asmNewLabel();
	Lreal = asmNewLabel();
	Loct = asmNewLabel();
	Lhex = asmNewLabel();
	Lbinary = asmNewLabel();
	Lfalse = asmNewLabel();
	Ltrue = asmNewLabel();
	Ldefault = asmNewLabel();
	Lret = asmNewLabel();
	asmAsm (
		MVI, R4, 0l,             /* r4 <- Initial state. */
		MVI, R6, 0l,             /* r6 <- initial yylength. */
	 LABEL, Lscan,
		/* Call syscallReadChar to read one char.  It contains logic
		   to handle string-ports so osRecvBlock not called any longer. */
		PUSH, R4, // r4 clobbered so push
			MVI, R3, ofalse, /* tell recv to not timeout */
			PUSH, R2, /* arg 1: timeout */ 
			PUSH, R1, /* arg 2: port object */
			MVI, R1, 2l, /* arg count */
			SYSI, syscallReadChar,
		POP, R4,
		MV, R3, R0,                     /* move char object to r3 */
		MVI, R0, 0l, /* Initialize final state to 0.  Will return 0 when still in non-final state. */
		SYSI, wscmSysTransition, /* Syscall to get next state. */
		BEQI, R0, 0l, Lscan,
		/* close paren? */
		BNEI, R0, SCLOSEPAREN, Ldot,
		MVI, R0, onull,
		RET,
	 LABEL, Ldot,
		/* dot? */
		BNEI, R0, SDOT, Leof,
		PUSH, R7,
		PUSH, RA,
		PUSH, RB, /* Recurse on this fn (parser) with isList set. */
		MVI, R7, 1l,
		MV, R0, RE,
		JAL, R0,
		POP, RB,
		POP, RA,
		POP, R7,
		LDI, R0, R0, 0l, /* Only want the car of the list we just parsed. */
		RET,
		/* eof? */
	 LABEL, Leof,
		BNEI, R0, SEOF, Lvector,
		MVI, R0, oeof,
		RET,
		/* vector? */
	 LABEL, Lvector,
		BNEI, R0, SVECTOR, Lquote,
		PUSH, R7,
		PUSH, RA,
		PUSH, RB, /* Recursive call with isList set */
		MVI, R7, 1l,
		MV, R0, RE,
		JAL, R0,
		POP, RB,
		POP, RA,
		POP, R7,
		PUSH, R1, /* Save r1 since objListToVector requires r1. */
		MV, R1, R0,
		SYSI, objListToVector,
		POP, R1,  /* Restore r1. */
		BRA, Ldone,
		/* quote? */
	 LABEL, Lquote,
		BNEI, R0, SQUOTE, Lunquotesplicing,
		PUSH, R7,
		PUSH, RA,
		PUSH, RB,
		MVI, R7, 0l,
		MV, R0, RE,
		JAL, R0,
		POP, RB,
		POP, RA, POP, R7,
		PUSH, R1,
		PUSH, R2, /* Save r1 and r2 */
			MV, R1, R0,           /* Car object. */
			MVI, R2, onull,/* Cdr object. */
			SYSI, objCons12,
			MVI, R1, squote,/* Car object. */
			MV, R2, R0,            /* Cdr object. */
			SYSI, objCons12,
		POP, R2,
		POP, R1,   /* Restore r1 and r2 */
		BRA, Ldone,
		/* unquote-splicing? */
	 LABEL, Lunquotesplicing,
		BNEI, R0, SUNQUOTESPLICING, Lunquote,
		PUSH, R7,
		PUSH, RA,
		PUSH, RB,
		MVI, R7, 0l,
		MV, R0, RE,
		JAL, R0,
		POP, RB,
		POP, RA,
		POP, R7,
		PUSH, R1, PUSH, R2, /* Save r1 and r2 */
			MV, R1, R0,           /* Car object. */
			MVI, R2, onull,/* Cdr object. */
			SYSI, objCons12,
			MVI, R1, sunquotesplicing,/* Car object. */
			MV, R2, R0,            /* Cdr object. */
			SYSI, objCons12,
		POP, R2,
		POP, R1,   /* Restore r1 and r2 */
		BRA, Ldone,
		/* unquote? */
	 LABEL, Lunquote,
		BNEI, R0, SUNQUOTE, Lquasiquote,
		PUSH, R7, PUSH, RA, PUSH, RB,
		MVI, R7, 0l,
		MV, R0, RE,
		JAL, R0,
		POP, RB, POP, RA, POP, R7,
		PUSH, R1, PUSH, R2, /* Save r1 and r2 */
			MV, R1, R0,            /* Car object. */
			MVI, R2, onull,      /* Cdr object. */
			SYSI, objCons12,
			MVI, R1, sunquote,  /* Car object. */
			MV, R2, R0,            /* Cdr object. */
			SYSI, objCons12,
		POP, R2, POP, R1,   /* Restore r1 and r2 */
		BRA, Ldone,
		/* quasiquote? */
	 LABEL, Lquasiquote,
		BNEI, R0, SQUASIQUOTE, Lopenparen,
		PUSH, R7,
		PUSH, RA,
		PUSH, RB,
		MVI, R7, 0l,
		MV, R0, RE,
		JAL, R0,
		POP, RB,
		POP, RA,
		POP, R7,
		PUSH, R1,
		PUSH, R2, /* Save r1 and r2 */
			MV, R1, R0,             /* Car object. */
			MVI, R2, onull,       /* Cdr object. */
			SYSI, objCons12,
			MVI, R1, squasiquote,/* Car object. */
			MV, R2, R0,             /* Cdr object. */
			SYSI, objCons12,
		POP, R2,
		POP, R1,   /* Restore r1 and r2 */
		BRA, Ldone,
		/* open paren? */
	 LABEL, Lopenparen,
		BNEI, R0, SOPENPAREN, Lcharacter,
		PUSH, R7, PUSH, RA, PUSH, RB, /* Recursive call with isList set */
		MVI, R7, 1l,
		MV, R0, RE,
		JAL, R0,
		POP, RB, POP, RA, POP, R7,
		BRA, Ldone,
		/* character? */
	 LABEL, Lcharacter,
		BNEI, R0, SCHARACTER, Lsymbol,
		SYSI, sysNewCharacter,
		BRA, Ldone,
		/* symbol? */
	 LABEL, Lsymbol,
		BNEI, R0, SSYMBOL, Lstring,
		SYSI, sysNewSymbol,
		BRA, Ldone,
		/* string? */
	 LABEL, Lstring,
		BNEI, R0, SSTRING, Linteger,
		SYSI, sysNewString,
		BRA, Ldone,
		/* integer? */
	 LABEL, Linteger,
		BNEI, R0, SINTEGER, Lreal,
		SYSI, sysNewInteger,
		BRA, Ldone,
		/* real? */
	 LABEL, Lreal,
		BNEI, R0, SREAL, Loct,
		SYSI, sysNewReal,
		BRA, Ldone,
		/* oct number? */
	 LABEL, Loct,
		BNEI, R0, SOCT, Lhex,
		SYSI, sysNewOct,
		BRA, Ldone,
		/* hex number? */
	 LABEL, Lhex,
		BNEI, R0, SHEX, Lbinary,
		SYSI, sysNewHex,
		BRA, Ldone,
		/* binary number? */
	 LABEL, Lbinary,
		BNEI, R0, SBINARY, Lfalse,
		SYSI, sysNewBinary,
		BRA, Ldone,
		/* false? */
	 LABEL, Lfalse,
		BNEI, R0, SFALSE, Ltrue,
		MVI, R0, ofalse,
		BRA, Ldone,
		/* true? */
	 LABEL, Ltrue,
		BNEI, R0, STRUE, Ldefault,
		MVI, R0, otrue,
		BRA, Ldone,
		/* default to null? */
	 LABEL, Ldefault,
		MVI, R0, onull,

		/* List context?  If so recurse and contruct pair. */
	 LABEL, Ldone,
		BEQI, R7, 0l, Lret,
		PUSH, R0, /* Save object just parsed. */
		PUSH, RA, PUSH, RB, /* Recurse. */
		MV, R0, RE, /* r0 <- code register*/
		JAL, R0,
		POP, RB, POP, RA,
		/* Create pair from top of stack and just parsed object.  Since we need
		   r1 and r2 do a little pushing and moving of objects. */
		POP, R3, /* Restore object previously parsed. */
		PUSH, R1, PUSH, R2, /* Save r1 and r2 */
		MV, R1, R3, /* Car object. */
		MV, R2, R0, /* Cdr object. */
		SYSI, objCons12,
		POP, R2, POP, R1,   /* Restore r1 and r2 */
	
	 LABEL, Lret,
		RET
	);
	asmAssemble();
	r1=r0;

	/* r5 eventually gets a new token string buffer (copied from this) when the
		following code runs.  BF: TODO: Implement as a dynamic buffer.*/
	objNewString((Str)"--------------------------------------------------------------------------------"
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
	asmInit();
	asmAsm (
		//BRA, 8l,
		//r0, /* Label */
		MVI, R1, r2, /* The string buffer.  Lives in r5. */
		SYSI, objCopyString,
		MV, R5, R0,
		/* Initialize boolean to 'not parsing a list'. */
		MVI, R7, 0l,
		/* r1 gets port object from the required parameter to read.  (read portObject). */
		POP, R1,
		/* Call parser. */
		MVI, R0, r1,  /* Insert code block object directly via r1 from above. */
		PUSH, RA,
		PUSH, RB,
		PUSH, R9,
			JAL, R0,
		POP, R9,
		POP, RB,
		POP, RA,
		RET
	);
	asmAssemble();
	r1=r0;
	sysNewClosure1Env();
	DBEND();
}

/* Create a read-eval-print-loop closure in machine language.
*/
void wscmCreateRepl (void) {
 Obj Lrepl, Ldone, Lcompileerror;
	DBBEG();
	objNewSymbol ((Str)"\nVM>", 4);  r2=r0;
	objNewSymbol ((Str)"stdin", 5);  r1=r0;  sysTGEFind(); r3=r0; /* The stdin binding, not value incase it changes */
	objNewSymbol ((Str)"read", 4);  r1=r0;  sysTGEFind();  r4=caar(r0); /* The binding value is a closure so we need to car again for the code object. */
	objNewString ((Str)"bye\n", 4);  r5=r0;
	objNewString ((Str)"Entering repl2\n", 14);  r6=r0;
	asmInit();
	Lrepl = asmNewLabel();
	Ldone = asmNewLabel();
	Lcompileerror = asmNewLabel();
	asmAsm (
		MVI, R0, r6, /* "Entering REPL\n" */
		PUSH, R0,
		MVI, R1, 1l,
		SYSI, syscallDisplay,
		/* Display prompt. */
	 LABEL, Lrepl,
		MVI, R0, r2, // Prompt
		PUSH, R0,
		MVI, R1, 1l,
		SYSI, syscallDisplay,
		/* Call read. */
		PUSH, RA, PUSH, RB, PUSH, R9,
		MVI, R0, r3, // in
		LDI, R0, R0, 0l,
		PUSH, R0,
		MVI, R0, r4, // read wscmCreateRead
		JAL, R0,
		POP, R9, POP, RB, POP, RA,
		/* Done if an #eof parsed. */
		BEQI, R0, oeof, Ldone,
		/* Compile expression. */
		SYSI, compCompile,
		BEQI, R0, ofalse, Lcompileerror,
		/* Run code. */
		PUSH, RA, PUSH, RB, PUSH, R9,
		JAL, R0,
		POP, R9, POP, RB, POP, RA,
		/* (display ...) */
	 LABEL, Lcompileerror,
		PUSH, R0,
		MVI, R1, 1l,
		SYSI, syscallDisplay,
		BRA, Lrepl,
	 LABEL, Ldone,
		MVI, R0, r5, /* Bye message. */
		PUSH, R0,
		MVI, R1, 1l,
		SYSI, syscallDisplay,
		SYSI, osUnthread,
		NOP /* TODO Need this to avoid assembler crash.  the SYSI call requires a next block */
	);
	asmAssemble();
	r1=r0;
	sysNewClosure1Env();
	DBEND();
}


void wscmInitialize (void) {
	DBBEG();

	compInitialize(); /* asm vm mem os sys obj */

	/* Although already activated, pass in a virtual machine interrupt handler callback
	   which eventually calls the schduler.  Called when vmInterrupt is set.  */
	vmInitialize(wscmSchedule, objDisplay);

	/* Register objects and pointer addresses with their
	   C source names for object debug dumps. */
	DB("Registering static pointer description strings");
	memPointerRegister(wscmSysTransition);
	memPointerRegister(sysEnvGet);
	memPointerRegister(wscmSocketFinalizer);

	/* Bind usefull values r2=value r1=symbol. */
	DB("Registering syscalls");
	sysDefineSyscall(syscallFun, "fun");
	sysDefineSyscall(syscallError, "error");
	sysDefineSyscall(wscmDumpEnv, "env");
	sysDefineSyscall(syscallQuit, "quit");
	sysDefineSyscall(syscallDumpThreads, "dump-threads");
	sysDefineSyscall(syscallString, "string");
	sysDefineSyscall(syscallMakeString, "make-string");
	sysDefineSyscall(syscallDebugger, "debugger");
	sysDefineSyscall(syscallSubString, "substring");
	sysDefineSyscall(syscallStringLength, "string-length");
	sysDefineSyscall(syscallSerializeDisplay, "serialize-display");
	sysDefineSyscall(syscallSerializeWrite, "serialize-write");
	sysDefineSyscall(syscallNumber2String, "number->string");
	sysDefineSyscall(syscallWrite, "write");
	sysDefineSyscall(syscallDisplay, "display");
	sysDefineSyscall(syscallVector, "vector");
	sysDefineSyscall(syscallMakeVector, "make-vector");
	sysDefineSyscall(syscallRandom, "random");
	sysDefineSyscall(syscallEqP, "eq?");
	sysDefineSyscall(syscallEquals, "=");
	sysDefineSyscall(syscallNotEquals, "!=");
	sysDefineSyscall(syscallStringEqualsP, "string=?");
	sysDefineSyscall(syscallLessThan, "<");
	sysDefineSyscall(syscallLessEqualThan, "<=");
	sysDefineSyscall(syscallGreaterThan, ">");
	sysDefineSyscall(syscallGreaterEqualThan, ">=");
	sysDefineSyscall(syscallAdd, "+");
	sysDefineSyscall(syscallMul, "*");
	sysDefineSyscall(syscallDiv, "/");
	sysDefineSyscall(syscallShiftLeft, "<<");
	sysDefineSyscall(syscallShiftRight, ">>");
	sysDefineSyscall(syscallLogAnd, "logand");
	sysDefineSyscall(syscallSqrt, "sqrt");
	sysDefineSyscall(syscallRemainder, "remainder");
	sysDefineSyscall(syscallModulo, "modulo");
	sysDefineSyscall(syscallSub, "-");
	sysDefineSyscall(syscallTime, "time");
	sysDefineSyscall(syscallUTime, "utime");
	sysDefineSyscall(syscallSleep, "sleep");
	sysDefineSyscall(syscallTID, "tid");
	sysDefineSyscall(osUnthread, "unthread");
	sysDefineSyscall(syscallOpenSocket, "open-socket");
	sysDefineSyscall(syscallOpenStream, "open-stream");
	sysDefineSyscall(syscallOpenFile, "open-file");
	sysDefineSyscall(syscallOpenString, "open-string");
	sysDefineSyscall(syscallOpenNewFile, "open-new-file");
	sysDefineSyscall(syscallReadDirectory , "read-directory");
	sysDefineSyscall(syscallClose, "close");
	sysDefineSyscall(syscallRecv, "recv");
	sysDefineSyscall(syscallReadChar, "read-char");
	sysDefineSyscall(syscallUnreadChar, "unread-char");
	sysDefineSyscall(syscallReadString, "read-string");
	sysDefineSyscall(syscallSend, "send");
	sysDefineSyscall(syscallSeek, "seek");
	sysDefineSyscall(syscallFileStat, "file-stat");
	sysDefineSyscall(syscallTerminalSize, "terminal-size");
	sysDefineSyscall(syscallStringRef, "string-ref");
	sysDefineSyscall(syscallStringSetB, "string-set!");
	sysDefineSyscall(syscallVectorLength, "vector-length");
	sysDefineSyscall(syscallDebugDumpAll, "dump-heap");
	sysDefineSyscall(syscallGarbageCollect, "garbage-collect");
	sysDefineSyscall(syscallDisassemble, "disassemble");
	sysDefineSyscall(syscallOpenSemaphore, "open-semaphore");
	sysDefineSyscall(syscallCloseSemaphore, "close-semaphore");
	sysDefineSyscall(syscallSemaphoreDown, "semaphore-down");
	sysDefineSyscall(syscallSemaphoreUp, "semaphore-up");
	sysDefineSyscall(syscallSignal, "signal");
	sysDefineSyscall(syscallToggleDebug, "toggle-debug");
	sysDefineSyscall(sysDumpTGE, "tge");

	/* For fun assign TGE symbol to a few internal C obj symbols */
	r0=ocharacters; sysDefine("characters");
	//r0=otaticIntegers; sysDefine("integers");
	//r0=osymbols; sysDefine("symbols");
	wscmCreateRead();  sysDefine("read");
	wscmCreateRepl();  sysDefine("repl2");
	objNewInt(42);  sysDefine ("y"); /* It's always nice to have x and y defined with useful values */
	objNewInt(69);  sysDefine ("x");
	objNewSymbol((Str)"a", 1);  r1=r0; /* It's also nice to have a pair ready to go */
	objNewSymbol((Str)"b", 1);  r2=r0;
	objCons12();  sysDefine("c");

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

	/* Create a code block for the compiled code to return to */
	asmInit();
	asmAsm(QUIT);
	asmAssemble();
	rretcode = r0;
	rretip = (Obj)(-1 * 8);

	while (!done) {
		renv = rtge; /* Evaluate in TGE */
		fprintf(stderr, "\n== Read and parse ===============\nWSCM>");
		yyparse();/* Expr read into r0. */
		if (oeof == r0) done=1;

		fprintf(stderr, "\n== Compile ======================\n");
		sysWrite(r0, stderr);

		compCompile(); /* Compile r0 into VM runable code into r0 */

		rcode = r0;
		rip = 0;

		if (ofalse == r0) {
			fprintf(stderr, "\n*Compile failed*");
		} else {
			//objDisplay(rcode, stderr);
			fprintf(stderr, "== Execute and return value =====\n");
			if (ofalse != r0) {
				vmRun();
			}
			objDisplay(r0, stderr);
		}

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
		/* Create port object, push the argument, set arg count to 1 then make the syscall. */
		objNewSymbol((Str)argv[1], strlen(argv[1]));
		vmPush(r0);  r1=(Obj)1;  syscallOpenFile();  r2=r0;
		/* Assign port to existing binding. */
		if (r2 != oeof) {
			objNewSymbol ((Str)"stdin", 5);  r1=r0;  sysTGEFind();
			memVectorSet(r0, 0, r2); /* Rebind stdin with the new port */
		}
	}

	objNewSymbol ((Str)"repl2", 5);  r1=r0;  /* wscmCreateRepl creates repl2 closure */
	sysTGEFind();
	r0 = caar(r0);

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

	yy_scan_string ((Str)
"(let ((PORT-SCMLIB (open-file (string *LIBPATH* \"/scm.scm\"))))\
  (let wscmLoad~ ((wscmLoadExpr (read PORT-SCMLIB)))\
    (or (eof-object? wscmLoadExpr) (begin\
      (eval wscmLoadExpr)\
      (wscmLoad~ (read PORT-SCMLIB)))))\
  (close PORT-SCMLIB)\
  (send \"\r\nbye.\r\n\" stdout)\
  (quit))");
	yyparse(); /* Use the internal parser */
	//objDisplay(r0, stderr);
	compCompile(); /* Use the internal and only compiler */
	//objDisplay(r0, stderr);
	if (ofalse == r0) {
		/* Compiler failed and returned #f instead of a code block */
		fprintf(stderr, "Could not compile internal REPL expression.  Calling internal REPL...");
		wscmCReadEvalPrintLoop();
	} else {
		/* Must disable stdio blocking since wscheme implements its own blocking I/O */
		fcntl (0, F_SETFL, fcntl(0, F_GETFL, 0)|O_NONBLOCK);
		osNewThread(); /* Create a new thread */
		osScheduler();
		vmRun();
	}
	DBEND();
}



/* Bind wscheme's command line arguments to the vector 'argv
   Also set *SCMILB* to the path of the running binary.
*/
void wscmBindArgs (Num argc, char *argv[]) {
 Num i=0;
	DBBEG();
	objNewVector(argc); r1=r0;
	for (i=0; i<argc; i++) {
		objNewString((u8*)argv[i], strlen(argv[i]));
		memVectorSet(r1, i, r0);
	}
	r0=r1; sysDefine ("argv"); 

	DBEND();
}


int main (int argc, char *argv[]) {
	setbuf(stdout, NULL);
	signal(SIGPIPE, SIG_IGN);
	srandom((unsigned int)time(NULL));

	wscmInitialize();
	sysInitialize((Str)argv[0]); /* Engage filesystem sandboxing */

	/* FUN: keep track of collector info.  Create a rootset object that holds list of
	   lists each containing info on the 4 current heaps. */
	/*
	memRootSetRegister(rheapinfo);
	rheapinfo = r0 = objCons(onull, onull);
	sysDefine("heapinfo");
	memInitialize(0, wscmCollectHeapInfo, 0);
	*/

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
	wscmStringReadEvalPrintLoop();
	return 0;
}

#undef DB_DESC
#undef DEBUG
