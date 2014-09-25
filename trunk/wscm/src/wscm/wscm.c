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
#include "mem.h"
#include "vm.h"
#include "obj.h"
#include "asm.h"
#include "sys.h"
#include "os.h"
#include "comp.h"
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
		vmPush(r00); /* Save registers */
		vmPush(r01);
		objNewInt((100*(d->last -d->start)) /width);  r01=objCons(r00, onull);// 4
      memHeapToColor(d); r01=objCons(r00, r01);
		objNewInt((100*(d->start-c->last))  /width);  r01=objCons(r00, r01);   // space
      memHeapToColor(0); r01=objCons(r00, r01);
		objNewInt((100*(c->last - c->start))/width);  r01=objCons(r00, r01);   // 3
      memHeapToColor(c); r01=objCons(r00, r01);
		objNewInt((100*(c->start- b->last)) /width);  r01=objCons(r00, r01);   // space
      memHeapToColor(0); r01=objCons(r00, r01);
		objNewInt((100*(b->last - b->start))/width);  r01=objCons(r00, r01);   // 2
      memHeapToColor(b); r01=objCons(r00, r01);
		//objNewInt((100*(b->start- a->last)) /width);  r01=objCons(r00, r01);   // space
		//objNewInt((100*(a->last - a->start))/width);  r01=objCons(r00, r01);   // 1
		//objNewInt((100*(a->start - min))    /width);  r01=objCons(r00, r01);   // space
		objNewInt((100*(b->start- min))     /width);  r01=objCons(r00, r01);   // space
      memHeapToColor(0); r01=objCons(r00, r01);
		memVectorSet(rheapinfo, 1, objCons(r01, cdr(rheapinfo)));
		r01 = vmPop(); /* Restore registers */
		r00 = vmPop();
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

/*    r01 <= immedate count of register arguments
     msg <= C string message to display
	    r00 => (Info expression) object
*/
void wscmError (Num regArgs, char const *msg) {
	DBBEG();

	/* Push back args to stack */
	assert(regArgs < 4);
	if (0 == regArgs) { } else
	if (1 == regArgs) { vmPush(r01); } else
	if (2 == regArgs) { vmPush(r01); vmPush(r02); } else
	if (3 == regArgs) { vmPush(r01); vmPush(r02); vmPush(r03); }

	/* Update stack argument count */
	r01 = (Obj)(regArgs);

	/* Create ("error message" arguments...) object */
	osException((Obj)msg);
	DBEND();
}


/* Verify function stack argument count in immediate:r01
*/
Int wscmAssertArgCount0 (char const *funcName) {
	if (0 == (Num)r10) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 0 argument");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCount1 (char const *funcName) {
	if (1 == (Num)r10) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 1 argument");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCount2 (char const *funcName) {
	if (2 == (Num)r10) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 2 arguments");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCount3 (char const *funcName) {
	if (3 == (Num)r10) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 3 arguments");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCountRange0To1 (char const *funcName) {
	if ((0 == (Num)r10) || (1 == (Num)r10)) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 0 or 1 arguments");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCountRange1To2 (char const *funcName) {
	if ((1 == (Num)r10) || (2 == (Num)r10)) return 0;
	fprintf(stderr, "\r\nWSCM-ASSERT::Expect 1 or 2 arguments");
	/* Create ("error message" arguments...) object */
	osException((Obj)funcName);
	return -1;
}

Int wscmAssertArgCount1OrMore (char const *funcName) {
	if (1 <= (Num)r10) return 0;
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
	if (objPortState(r01) == sconnecting) sysAcceptRemoteStream();

	if (r01!=oeof && objPortState(r01) == sconnecting) {
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
	if (objPortState(r01) == saccepting) {
		sysAcceptLocalStream();
		r01 = r00;
	}

	/* Is it the same socket? */
	if (objPortState(r01) == saccepting) {
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
	//fprintf(stderr, "\n");memPrintObject(rstack, stderr);
	//fprintf(stderr, "\n");objDisplay(rstack, stderr);
	//fprintf(stderr, "pwd=%s\n", get_current_dir_name());
	r00 = otrue;
}

void syscallError (void) {
	osException((Obj)"User Error");
}

void wscmDumpEnv (void) {
	sysDumpEnv(renv);
	r00 = onull;
}

void syscallQuit (void) {
	if ((Num)r01==1) exit(*(int*)vmPop());
	else exit(0);
}

void syscallDumpThreads (void) {
	sysWrite(rthreads, stderr); /* TODO make this pretty */
	r00 = otrue;
}

void syscallString (void) {
 Num totalLen=0, s=0, len;
	DBBEG();
	/* Verify each arg is a string and sum the lengths */
	while (s < (Int)r01) {
		r00 = memVecStackObject(rstack, s++);
		if (wscmAssertArgType3NoException((Num)r01 - s + 1, TSTRING, TCHAR, TSYMBOL, r00)) {
			osException((Obj)__func__);
			goto ret;
		}

		totalLen += memObjectLength(r00);
	}
	/* New string is built in reverse.  Even works if
	   substrings are "" since strcpy length is 0. */
	r00 = totalLen ? memNewArray(TSTRING, totalLen) : onullstr;
	while (s--) {
		totalLen -= (len = memObjectLength(r01=vmPop()));
		memcpy(r00+totalLen, r01, len);
	}
	ret:
	DBEND();
}

void syscallMakeString (void) {
 Num len;
	DBBEG();
	if (wscmAssertArgCountRange1To2(__func__)) goto ret;

	if (2 == (Num)r01) {
		/* Expression has a fill character */
		r02 = vmPop(); /* Fill char */
		r01 = vmPop(); /* New length */

		if (wscmAssertArgType(TINTEGER, r01, 1, 2, __func__) ||
			 wscmAssertArgType(TCHAR,    r02, 2, 2, __func__)) goto ret;

		len = *(Num*)r01;
		objNewString(NULL, len);
		while (len--) ((Chr*)r00)[len] = *(Chr*)r02;
	} else {
		/* Expression has no fill character */
		len = *(Num*)(r01 = vmPop());

		if (wscmAssertArgType(TINTEGER, r01, 1, 1, __func__)) goto ret;

		objNewString(NULL, len=*(Num*)r01);
	}

ret:
	DBEND();
}

void syscallSubString (void) {
 Num start, end;
	DBBEG();
	if (wscmAssertArgCount3(__func__)) goto ret;

	r03 = vmPop();
	r02 = vmPop();
	r01 = vmPop();

	if (wscmAssertArgType(TSTRING,  r01, 1, 3, __func__) ||
	    wscmAssertArgType(TINTEGER, r02, 2, 3, __func__) ||
	    wscmAssertArgType(TINTEGER, r03, 3, 3, __func__)) goto ret;

	end   = *(Num*)r03;
	start = *(Num*)r02;

	if ((start < end) && (end <= memObjectLength(r01))) {
		DB("Creating substring object");
		objNewString(NULL, end-start);
		memcpy(r00, r01+start, end-start);
	} else if (end == start) {
		DB("Zero len");
		r00 = onullstr;
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
	r01 = vmPop();
	//if (wscmAssertArgType(TSTRING, r01, 1, 1, __func__)) goto ret;
	objNewInt((Int)memObjectLength(r01));
ret:
	DBEND();
}

/* Returns an array object in r00 (ignore the type) reprsenting an external
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
	r00 = vmPop();

	DBE objDisplay(r00, stderr);

	if ((Num)r00 < 0x100000l) {
		ret = sprintf((char*)buff, "#"OBJ, (Num)r00);
		assert(0 < ret);
		objNewString(buff, (Num)ret);
	} else switch (memObjectType(r00)) {
		case TINTRINSIC :
			if      (onull == r00)  r00 = snull;
			else if (ofalse == r00) r00 = sfalse;
			else if (otrue == r00)  r00 = strue;
			else if (oeof == r00)   r00 = seof;
			else assert(!"Unknown intrinsic object to serialize");
			break;
		case TCHAR   : 
		case TSYMBOL : 
		case TSTRING : 
			break;
		case TINTEGER:
			sysSerializeInteger(*(Int*)r00, 10);
			break;
		case TREAL   : 
			ret = sprintf((char*)buff, "%.2f", *(Real*)r00);
			assert(0<ret);
			objNewString(buff, (Num)ret);
			break;
		case TPORT:
			if (memIsObjectType(memVectorObject(r00, 1), TINTEGER))
				ret = sprintf((char*)buff, "#PORT<DESC:"NUM" ADDR:"NUM" PORT:"NUM" STATE:"STR">",
				   (Num)memVectorObject(r00, 0),
				   *(Num*)memVectorObject(r00, 1),
				   *(Num*)memVectorObject(r00, 2),
				   objPortState(r00));
			else {
				o = memVectorObject(r00, 2); /* Consider the flags/port/index field */
				ret = sprintf((char*)buff, "#PORT<DESC:"NUM" ADDR:%.*s PORT:"NUM" STATE:"STR">",
				   (Num)memVectorObject(r00, 0),
				   memObjectLength(memVectorObject(r00, 1)), (char*)memVectorObject(r00, 1), /* %* format takes two args */
					memIsObjectValid(o)?*(Num*)o:(Num)o,
					objPortState(r00));
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
			ret = sprintf((char*)buff, HEX, (Num*)r00);
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
	r00 = vmPop();
	if ((Num)r00 < 0x100000l) {
		ret = sprintf((char*)buff, "#"HEX, (Num)r00);
		assert(0<ret);
		objNewString(buff, (Num)ret);
	} else switch (memObjectType(r00)) {
		case TINTRINSIC :
			if      (onull == r00)  r00 = snull;
			else if (ofalse == r00) r00 = sfalse;
			else if (otrue == r00)  r00 = strue;
			else if (oeof == r00)   r00 = seof;
			else assert(!"Unknown intrinsic object to serialize");
			break;
		case TSYMBOL : 
			break;
		case TINTEGER:
			sysSerializeInteger(*(Int*)r00, 10);
			break;
		case TREAL   : 
			ret = sprintf((char*)buff, "%.2f", *(Real*)r00);
			assert(0<ret);
			objNewString(buff, (Num)ret);
			break;
		case TCHAR   : 
			ret = sprintf((char*)buff, "#\\%c", *(char*)r00);
			assert(0<ret);
			objNewString(buff, (Num)ret);
			break;
		case TSTRING : 
			len = 0;
			buff[len++] = '"';
			for (i=0; i<memObjectLength(r00); i++) {
				c = ((Chr*)r00)[i];
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
			ret = sprintf((char*)buff, "#"HEX, (Num*)r00);
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
	base = (Num)r10==2 ? *(Num*)vmPop() : 10; /* Default base is 10. */
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
	if ((Int)r10==2) fd=*(Int*)vmPop(); /* File descriptor. */
	if (fd==1) stream = stdout;
	if (fd==2) stream = stderr;
	assert(NULL != stream);

	sysWrite(r00=vmPop(), stream);
ret:
	DBEND();
}

void syscallDisplay (void) {
 Int fd=1;
 FILE *stream=NULL;
	DBBEG();

	if (wscmAssertArgCountRange1To2(__func__)) goto ret;
	if ((Int)r01==2) fd=*(Int*)vmPop(); /* Descriptor. */

	if (fd==1) stream = stdout;
	if (fd==2) stream = stderr;
	assert(NULL != stream);

	objDisplay(r00=vmPop(), stream);
ret:
	DBEND();
}

void syscallVector (void) {
 Num l=(Num)r10;
	DBBEG();
	if (l==0) r00=onullvec;
	else {
		objNewVector(l);
		while (l--) memVectorSet(r00, l, vmPop());
	}
	DBEND();
}

void syscallMakeVector (void) {
 Num len;
	DBBEG();
	if (wscmAssertArgCountRange1To2(__func__)) goto ret;
	if (1 == (Num)r10) {
		r01 = vmPop(); /* length */
		if (wscmAssertArgType(TINTEGER, r01, 1, 1, __func__)) goto ret;
		len = *(Num*)r01;
		if (0 < len && len <= 0x100000) { /* TODO magic vector size limit */
			objNewVector(len);
			while (len--) memVectorSet(r00, len, r02);
		} else if (0 == len) {
			r00 = onullvec;
		} else {
			wscmError(1, "make-vector invalid size");
		}
	} else {
		r02 = vmPop(); /* Fill object */
		r01 = vmPop(); /* length */
		if (wscmAssertArgType(TINTEGER, r01, 1, 2, __func__)) goto ret;
		len = *(Num*)r01;
		if (0 < len) {
			objNewVector(len);
			while (len--) memVectorSet(r00, len, r02);
		} else if (0 == len) {
			r00 = onullvec;
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
	if ((Int)r01 == 1)
		objNewInt(random() % *(Int*)vmPop());
	else
		objNewInt(random());
ret:
	DBEND();
}

void syscallEqP (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r00 = (vmPop() == vmPop()) ? otrue : ofalse;
ret:
	DBEND();
}

/* Numerical equivalence. Returns false if either are not integers. */
void syscallEquals (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r02 = vmPop();
	r01 = vmPop();
	r00 = TINTEGER == memObjectType(r01)
	     && TINTEGER == memObjectType(r02)
	     && *(Int*)r01 == *(Int*)r02
		? otrue : ofalse;
	/*
	if (wscmAssertArgType(TINTEGER, r01, 1, 2, __func__) ||
	    wscmAssertArgType(TINTEGER, r02, 2, 2, __func__)) goto ret;
	r00 = *(Int*)r01 == *(Int*)r02 ? otrue : otrue;
	*/
ret:
	DBEND();
}

void syscallNotEquals (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r02 = vmPop();
	r01 = vmPop();
	r00 = TINTEGER == memObjectType(r01)
	     && TINTEGER == memObjectType(r02)
	     && *(Int*)r01 == *(Int*)r02
		? ofalse : otrue;
	/*
	if (wscmAssertArgType(TINTEGER, r01, 1, 2, __func__) ||
	    wscmAssertArgType(TINTEGER, r02, 2, 2, __func__)) goto ret;
	r00 = *(Int*)r01 == *(Int*)r02 ? false : otrue;
	*/
ret:
	DBEND();
}

void syscallStringEqualsP (void) {
 Num len;
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r01 = vmPop();
	r00 = vmPop();
	r00 = TSTRING == memObjectType(r00)
	     && TSTRING == memObjectType(r01)
	     && memObjectLength(r00) == (len=memObjectLength(r01))
	     && !strncmp (r00, r01, len) ? otrue : ofalse;
ret:
	DBEND();
}

void syscallLessThan (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r00 = (*(Int*)vmPop() > *(Int*)vmPop()) ? otrue : ofalse;
ret:
	DBEND();
}

void syscallLessEqualThan (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r00 = (*(Int*)vmPop() >= *(Int*)vmPop()) ? otrue : ofalse;
ret:
	DBEND();
}

void syscallGreaterThan (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r01 = vmPop();
	r00 = vmPop();
	if (TREAL == memObjectType(r00) && TREAL == memObjectType(r01))
		r00 = (*(Real*)r01 < *(Real*)r00) ? otrue : ofalse;
	else
		r00 = (*(Int*)r01 < *(Int*)r00) ? otrue : ofalse;
ret:
	DBEND();
}

void syscallGreaterEqualThan (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r01 = vmPop();
	r00 = vmPop();
	if (TREAL == memObjectType(r00) && TREAL == memObjectType(r01))
		r00 = (*(Real*)r01 <= *(Real*)r00) ? otrue : ofalse;
	else
		r00 = (*(Int*)r01 <= *(Int*)r00) ? otrue : ofalse;
ret:
	DBEND();
}

/*
void syscallAdd (void) {
 Int sum=0, c=(Int)r01;
	DBBEG();
	while (c--) sum += *(Int*)vmPop();
	objNewInt(sum);
	DBEND();
}

void syscallMul (void) {
 Int product=1, c=(Int)r01;
	DBBEG();
	while (c--) product *= *(Int*)vmPop();
	objNewInt(product);
	DBEND();
}
*/

/* Multiply all but the first, then divide the first by the product.
*/
void syscallDiv (void) {
 Int product=1, divisor, c=(Int)r01;
	DBBEG();
	while (1 < c--) product *= *(Int*)vmPop();
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
 Int sum=0, count=(Int)r10;
	DBBEG();
	if (wscmAssertArgCount1OrMore(__func__)) goto ret;
	if (1 == count)
		sum = -*(Int*)vmPop();
	else {
		while (--count) sum += *(Int*)vmPop();
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
	r00 = osThreadId(rrunning);
ret:
	DBEND();
}


void syscallOpenSocket (void) {
	DBBEG();

	if (wscmAssertArgCountRange1To2(__func__)) goto ret;

	if (1 == (Int)r01) {
		r01 = vmPop(); /* Consider socket number */
		if (wscmAssertArgType (TINTEGER, r01, 1, 1, __func__)) goto ret;
		sysOpenLocalSocket(); /* pass in port via r01 */
		if (memObjectType(r00) == TPORT) {
			/* Create and set the socket's "close" finalizer */
			r01 = r00;
			r00 = memNewFinalizer();
			memVectorSet(r00, 0, wscmSocketFinalizer);
			memVectorSet(r00, 1, r01);
			memVectorSet(r01, 5, r00);
			r00 = r01; /* Return the port in r00 */
		}
	} else if (2 == (Int)r01) {
		r01 = vmPop();
		r02 = vmPop();
		if (wscmAssertArgType (TSTRING,  r02, 1, 2, __func__) ||
		    wscmAssertArgType (TINTEGER, r01, 2, 2, __func__)) goto ret;
		sysOpenRemoteSocket(); /* pass in port via r01, host via r02 */
	}
ret:
	DBEND();
}

void syscallOpenStream (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;

	r01 = vmPop();
	if (memObjectType(r01) != TPORT) {
		wscmError(1, "Invalid arguments to open-stream:");
		goto ret;
	}

	if (objPortState(r01) == sconnecting)
		wscmOpenRemoteStream();
	else if (objPortState(r01) == saccepting)
		wscmOpenLocalStream();
	else if (objPortState(r01) == sclosed)
		r00=oeof;
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

	if ((Num)r01==2) { /* An extra arg forces a silent sysOpenFile() call */
		vmPop();
		silent=1;
	}
	r01 = vmPop();
	sysOpenFile(O_RDWR, S_IRUSR|S_IWUSR, silent); /* r01 contains filename object, returns port in r00, original filename in r02 */
	
	if (!memIsObjectType(r00, TPORT)) {
		if (silent) {
			r00 = ofalse;
		} else {
			if (memIsObjectType(r00, TSTRING)) {
				/* Error using the string passed */
				wscmError(0, r00);
			} else {
				/* Error using C's error code */
				len = sprintf(buff, "Unable to open file \""STR"\".  ["INT":"STR"]", r02, errno, strerror(errno));
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

	r01 = vmPop();
	sysOpenString(); /* r01 contains string buffer to read */
	
	if (!memIsObjectType(r00, TPORT)) {
		if (memIsObjectType(r00, TSTRING)) {
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
	if ((Num)r01==2) { /* If an extra arg is passed, make it a silent call */
		vmPop();
		silent=1;
	}
	r01 = vmPop();
	sysOpenFile(O_CREAT|O_TRUNC|O_RDWR, S_IRUSR|S_IWUSR, silent);

	if (!memIsObjectType(r00, TPORT)) {
		if (silent) {
			r00 = ofalse;
		} else {
			if (memIsObjectType(r00, TSTRING)) {
				/* Error using the string passed */
				wscmError(0, r00);
			} else {
				/* Error using C's error code */
				len = sprintf(buff, "Unable to open new file \""STR"\".  ["INT":"STR"]", r02, errno, strerror(errno));
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
	r01 = vmPop();
	if (wscmAssertArgType(TSTRING, r01, 1, 1, __func__)) goto ret;

	/* Scheme string to C string */
	len = memObjectLength(r01);
	strncpy(buff, (char*)r01, len);
	buff[len] = 0;

	if (!sysCanonicalizePath(buff)) { /* Sandbox verifies path in 'buff' is valid */
		wscmError(1, "Invalid directory path");
		goto ret;
	}

	dir = opendir(r01);

	if (NULL == dir) {
		r00 = onull;
		ret = sprintf(buff, "Unable to open directory \""STR"\".  ["INT":"STR"]", r01, errno, strerror(errno));
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
			vmPush(r00);
			++len;
			de = readdir(dir);
		}
		closedir(dir);
		r00 = onull;
		while (len--) {
			r01=vmPop();
			r02=r00;
			objCons012();
		}
	}
ret:
	DBEND();
}

void syscallClose(void) {
	DBBEG();
	r00 = vmPop();
	if (memObjectType(r00) != TPORT) {
		printf ("WARNING: syscallClose: not a socket: ");
		objDisplay(r00, stdout);
	} if (objPortState(r00) == sclosed) {
		printf ("WARNING: syscallClose: socket already closed: ");
		//objDisplay(r00, stdout);
	} else {
		/* Don't try and close a port-string */
		if (onullstr != memVectorObject(r00, 0)) {
			close(*(int*)r00);
		}
		memVectorSet(r00, 3, sclosed);
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

	r01=vmPop(); /* Port object */
	r02=vmPop(); /* Timeout (integer or #f) */
	r03=vmPop(); /* Byte count integer */

	if (wscmAssertArgType(TINTEGER, r03, 1, 3, __func__)) goto ret;

	if (r02 != ofalse && !memIsObjectType(r02, TINTEGER)) {
		fprintf(stderr, "\r\nWSCM-ASSERT::Expect '"STR"' type or '#f' for argument 2", memTypeString(TINTEGER));
		wscmError(3, __func__);
		goto ret;
	}
	if (wscmAssertArgType(TPORT, r01, 3, 3, __func__)) goto ret;

	/* r03 gets the byte count.  It's then assigned an empty string
	   character buffer with the specified length of characters to
	   read.  A count of zero (null string) implies read any count
	   of bytes. */
	len = *(Num*)r03; /* Consider immediate integer value */

	if (MEMOBJECTMAXSIZE <= len) {
		fprintf(stderr, "\r\nWSCM-ASSERT::Expect byte count to be less than "HEX, MEMOBJECTMAXSIZE);
		wscmError(3, __func__);
		goto ret;
	}

	r03 = len ? memNewArray(TSTRING, len) : onullstr;

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
	r01=vmPop(); /* Port object */
	r02=vmPop(); /* Timout */
	if (memObjectType(r01) != TPORT) {
		printf ("WARNING: syscallReadChar: not a port object: ");
		objDisplay(r01, stdout);
		r00 = oeof;
	} else if (onullstr == memVectorObject(r01, 0)) {
	/* Port object is a port-string.  Return the next char and increment
	   internal pointer.  EOF returned if entire string read. */
		r00 = memVectorObject(r01, 4);
		if (ofalse != r00)
			/* Return push-back char */
			memVectorSet(r01, 4, ofalse);
		else {
			str = memVectorObject(r01,1);
			idx = (Num)memVectorObject(r01, 2);
			if (idx == memObjectLength(str)) {
				r00 = oeof; /* Return EOF */
			} else {
				r00 = objIntegerToChar(str[idx]);
				memVectorSet(r01, 2, (Obj)(idx+1)); /* Increment index */
			}
		}
	} else {
		r03=onull;  /* tells recv that we just want a character. */
		osRecvBlock();
	}
ret:
	DBEND();
}

void syscallUnreadChar (void) {
	DBBEG();
	if (wscmAssertArgCount2(__func__)) goto ret;
	r01=vmPop(); /* Port. */
	r00=vmPop(); /* Character. */
	if (memObjectType(r01) != TPORT) {
		printf ("WARNING: syscallUnreadChar: arg2 not a port object: ");
		objDisplay(r01, stdout);
		r00 = oeof;
	} else if (memObjectType(r00) != TCHAR) {
		printf ("WARNING: syscallUnreadChar: arg1 not a char: ");
		objDisplay(r00, stdout);
		r00 = oeof;
	} else
		memVectorSet(r01, 4, r00);
ret:
	DBEND();
}

/* Call internal C parser on passed string.  Doesn't block so an EOF
   can be returned if the expression is incomplete or empty.
*/
void syscallReadString (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r01=vmPop(); /* String to parse. */
	yy_scan_bytes(r01, memObjectLength(r01));
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
	/* r01 gets port object. */
	r01=vmPop();
	/* r02 gets string object. */
	r02=vmPop();
	/* Count sent already.  Should be initialized to 0. */
	r03=0;
	if (memObjectType(r01) != TPORT) {
		printf ("WARNING: syscallSend: not a socket is (type "HEX02"): ", memObjectType(r01));
		__builtin_trap();
		r00 = oeof;
	} else if (memIsObjectValid(r02)) {
		sysSend();
		if (r00 == ofalse) { /* Still more to send. */
			osUnRun();
			osMoveToQueue(rrunning, rblocked, swriteblocked);
			osScheduler();
		}
	} else {
		fprintf (stderr, "WARNING: trying to send:");
		memPrintObject(r02, stderr);
		syscallDebugger();
		r00 = ofalse;
	}
ret:
	DBEND(" => r00:");
	DBE memPrintObject(r00, stderr);
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

	r01 = vmPop();
	if (wscmAssertArgType(TPORT, r01, 1, 1, __func__)) goto ret;

	fstat(objPortDescriptor(r01), &buf);

	objNewVector(13);
	r01 = r00;
	objNewInt((Int)buf.st_dev); memVectorSet(r01, 0, r00); /* dev_t     st_dev;     ID of device containing file */
	objNewInt((Int)buf.st_ino); memVectorSet(r01, 1, r00); /* ino_t     st_ino;     inode number */
	objNewInt(buf.st_mode); memVectorSet(r01, 2, r00);     /* mode_t    st_mode;    protection */
	objNewInt((Int)buf.st_nlink); memVectorSet(r01, 3, r00);/*nlink_t   st_nlink;   number of hard links */
	objNewInt(buf.st_uid); memVectorSet(r01, 4, r00);      /* uid_t     st_uid;     user ID of owner */
	objNewInt(buf.st_gid); memVectorSet(r01, 5, r00);      /* gid_t     st_gid;     group ID of owner */
	objNewInt((Int)buf.st_rdev); memVectorSet(r01, 6, r00);/* dev_t     st_rdev;    device ID (if special file) */
	objNewInt(buf.st_size); memVectorSet(r01, 7, r00);     /* off_t     st_size;    total size, in bytes */
	objNewInt(buf.st_blksize); memVectorSet(r01, 8, r00);  /* blksize_t st_blksize; blocksize for file system I/O */
	objNewInt(buf.st_blocks); memVectorSet(r01, 9, r00);   /* blkcnt_t  st_blocks;  number of 512B blocks allocated */
	objNewInt(buf.st_atime); memVectorSet(r01,10, r00);    /* time_t    st_atime;   time of last access */
	objNewInt(buf.st_mtime); memVectorSet(r01,11, r00);    /* time_t    st_mtime;   time of last modification */
	objNewInt(buf.st_ctime); memVectorSet(r01,12, r00);    /* time_t    st_ctime;   time of last status change */
	r00 = r01;

ret:
	DBEND();
}

void syscallTerminalSize (void) {
 struct winsize win;
	DBBEG();
	ioctl(1, TIOCGWINSZ, &win);
	objNewInt(win.ws_col); r01=r00;
	objNewInt(win.ws_row); r02=r00;
	objCons012();
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
	r00 = objIntegerToChar(*((Chr*)o+offset));
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
	r00 = strObj; 
ret:
	DBEND();
}

void syscallVectorLength (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r00 = vmPop();
	objNewInt(r00==onullvec?0:(Int)memObjectLength(r00));
ret:
	DBEND();
}

/* Deserializers:  String representation in r05, length r06 => new atom in r00. */
void sysNewSymbol (void) {
	DBBEG();
	objNewSymbolR05R06(); // objNewSymbol((Str)r05, (Num)r06);
	DBEND();
}
void sysNewString (void) {
 Num len = parseString(r05); /* Mutates the string & returns the new length. */
	DBBEG();
	if ((Int)r06 == 2) r00 = onullstr;
	else {
		objNewString(NULL, len);
   	memcpy(r00, r05, len);
	}
	DBEND();
}
void sysNewCharacter (void) {
	DBBEG();
	r00 = objIntegerToChar(((u8*)r05)[2]);
	DBEND();
}
void sysNewInteger (void) {
	DBBEG();
	*((char*)r05+(Int)r06)=0;
	objNewInt(strtol(r05+(*(char*)r05=='#'?2:0),0,10));
	DBEND();
}
void sysNewBinary (void) {
	DBBEG();
	objNewInt(strtol(r05+2,0,2));
	DBEND();
}
void sysNewReal (void) {
	DBBEG();
	objNewReal(strtof(r05,0));
	DBEND();
}
void sysNewOct (void) {
	DBBEG();
	objNewInt(strtol(r05+2,0,8));
	DBEND();
}
void sysNewHex (void) {
	DBBEG();
	objNewInt(strtol(r05+2,0,16));
	DBEND();
}

void syscallDebugDumpAll (void) {
	DBBEG();
	if (wscmAssertArgCount0(__func__)) goto ret;
	memPrintAll(NULL);
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
	r00 = vmPop();
	if (memObjectType(r00) == TCLOSURE) {
		if (cdr(r00) != onull) sysDumpEnv(cdr(r00));
		r00 = car(r00); /* Consider closure's code block */
	}
	objDisplay(r00, stderr);
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
	r00 = vmPop();
	if (memVectorObject(r00,0)==ofalse) /* TODO this vector-ref should be abstracted using semaphore accessors */
		r00 = ofalse; /* Semaphore already closed so return failure */
	else {
		memVectorSet(r00, 0, ofalse);
		osUnblockSemaphoreBlocked(r00, 1); /* 1 means unblock all threads on this semaphore */
		r00 = otrue;
	}
ret:
	DBEND();
}

void syscallSemaphoreDown (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	r01 = vmPop();
	sysSemaphoreDown();
	if (r00 == r01) {
		/* Block thread on this semaphore.  Semaphore remains in r01. */
		DB("Blocking thread "NUM" on semaphore value "NUM, *(Num*)osThreadId(rrunning), *(Int*)r01);
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
	r01 = vmPop();
	sysSemaphoreUp();
	if (r00 == r01) {
		/* Unblock a thread blocked by this semaphore. If after incrmenting the
	   	counter the semaphore is still blocking, then this means one
	   	of the blocked threads can be awakened. */
		osUnblockSemaphoreBlocked(r01, 0);
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
	r00 = odebug = (otrue == odebug) ? ofalse : otrue;
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
	assert((0 < sig) && (sig < MAX_SIGNAL_VALUE));
	caughtSignals[sig]=1;
	vmInterrupt = 1; /* Let virtual machine know an interrupt occured */
	signalFlag = 1; /* Let scheduler know a signal handler needs to be instantiated */
	DBEND();
}

/* Syscall to set a signal handler for a specified signal number.
   The signal handler will always be the wscmSignalHandler().  See above. */
void syscallSignal (void) {
	DBBEG();
	if (wscmAssertArgCount1(__func__)) goto ret;
	signal(*(int*)vmPop(), wscmSignalHandler); /* Have to cast to an uint64 to int32 */
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

/* Given  - r01:port object  r03:char object or eof
            r04:current state (see scanner.c)   r05:yytext  r06:yylen
   Return - r04:next state   r05:yytext   r06:yylen
            r00:final state if complete token scanned (reached final state).
*/
void wscmSysTransition (void) {
	DBBEG(" <= state:r04="HEX"  char:r03=", r04);
	DBE memPrintObject(r03, stderr);

	/* Make the transition on char in r03 given state in r04. */
	if (oeof == r03)
		r04 = (Obj)transition(256, (Num)r04); /* 256 is treated as the EOF character */
	else
		r04 = (Obj)transition(*(Num*)r03, (Num)r04);
	DB("transition() returned new state => "HEX, r04);

	/* Append char to scanned token and inc yylen. */
	*((u8*)r05+(Num)r06++) = *(u8*)r03;

	/*  Reset yylen to 0 if we ever end up back to the initial state.*/
	if ((Num)r04 == 0x00l) {
		r06 = 0l;
	} else if ((Int)r04 & FINALSTATE) {
		/* Push back character to stream if in pushback state and not eof. */
		if (((Num)r04 & PUSHBACK) && (oeof != r03)) {
			r06--;
			memVectorSet(r01, 4, objIntegerToChar(*(Num*)r03));
		}
		r00 = r04;
	}

	DBEND(" final state r04="HEX"  r00="HEX, r04, r00);
}


/* Call to the read scheme closure will setup registers for calls to internal
   scanning and parsing code blocks.  This function creates that closure.
	Create atom parser code block.  Modeled after the C equivalent in scanner.c

   scanner  - r04:state   r05:token buffer   r06:token length  r7:islist boolean

	Uses -- r01:port object   r02:timeout=0  r03:wantChar=()  r04:used by recv
	        r04:state   r05:token buffer   r06:token length   r7:is list bool.

	osRecvBlock in: r01 port  r02 timeout  r03 string buffer
	           use: r04
	           out: r00 string
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
		MVI, R04, 0l,             /* r04 <- Initial state. */
		MVI, R06, 0l,             /* r06 <- initial yylength. */
	 LABEL, Lscan,
		/* Call syscallReadChar to read one char.  It contains logic
		   to handle string-ports so osRecvBlock not called any longer. */
		PUSH, R04, // r04 clobbered so push
			MVI, R02, ofalse, /* tell recv to not timeout */
			PUSH, R02, /* arg 1: timeout */ 
			PUSH, R01, /* arg 2: port object */
			MVI, R01, 2l, /* arg count */
			SYSI, syscallReadChar,
		POP, R04,
		MV, R03, R00,                     /* move char object to r03 */
		MVI, R00, 0l, /* Initialize final state to 0.  Will return 0 when still in non-final state. */
		SYSI, wscmSysTransition, /* Syscall to get next state. */
		BEQI, R00, 0l, Lscan,
		/* close paren? */
		BNEI, R00, SCLOSEPAREN, Ldot,
		MVI, R00, onull,
		RET,
	 LABEL, Ldot,
		/* dot? */
		BNEI, R00, SDOT, Leof,
		PUSH, R07,
		PUSH, RIPLINK, // was R0A
		PUSH, RCODELINK, /* Recurse on this fn (parser) with isList set. */
		MVI, R07, 1l,
		MV, R00, RCODE,
		JAL, R00,
		POP, RCODELINK,
		POP, RIPLINK,
		POP, R07,
		LDI, R00, R00, 0l, /* Only want the car of the list we just parsed. */
		RET,
		/* eof? */
	 LABEL, Leof,
		BNEI, R00, SEOF, Lvector,
		MVI, R00, oeof,
		RET,
		/* vector? */
	 LABEL, Lvector,
		BNEI, R00, SVECTOR, Lquote,
		PUSH, R07,
		PUSH, RIPLINK,
		PUSH, RCODELINK, /* Recursive call with isList set */
		MVI, R07, 1l,
		MV, R00, RCODE,
		JAL, R00,
		POP, RCODELINK,
		POP, RIPLINK,
		POP, R07,
		PUSH, R01, /* Save r01 since objListToVector requires r01. */
		MV, R01, R00,
		SYSI, objListToVector,
		POP, R01,  /* Restore r01. */
		BRA, Ldone,
		/* quote? */
	 LABEL, Lquote,
		BNEI, R00, SQUOTE, Lunquotesplicing,
		PUSH, R07,
		PUSH, RIPLINK,
		PUSH, RCODELINK,
		MVI, R07, 0l,
		MV, R00, RCODE,
		JAL, R00,
		POP, RCODELINK,
		POP, RIPLINK, POP, R07,
		PUSH, R01,
		PUSH, R02, /* Save r01 and r02 */
			MV, R01, R00,           /* Car object. */
			MVI, R02, onull,/* Cdr object. */
			SYSI, objCons012,
			MVI, R01, squote,/* Car object. */
			MV, R02, R00,            /* Cdr object. */
			SYSI, objCons012,
		POP, R02,
		POP, R01,   /* Restore r01 and r02 */
		BRA, Ldone,
		/* unquote-splicing? */
	 LABEL, Lunquotesplicing,
		BNEI, R00, SUNQUOTESPLICING, Lunquote,
		PUSH, R07,
		PUSH, RIPLINK,
		PUSH, RCODELINK,
		MVI, R07, 0l,
		MV, R00, RCODE,
		JAL, R00,
		POP, RCODELINK,
		POP, RIPLINK,
		POP, R07,
		PUSH, R01, PUSH, R02, /* Save r01 and r02 */
			MV, R01, R00,           /* Car object. */
			MVI, R02, onull,/* Cdr object. */
			SYSI, objCons012,
			MVI, R01, sunquotesplicing,/* Car object. */
			MV, R02, R00,            /* Cdr object. */
			SYSI, objCons012,
		POP, R02,
		POP, R01,   /* Restore r01 and r02 */
		BRA, Ldone,
		/* unquote? */
	 LABEL, Lunquote,
		BNEI, R00, SUNQUOTE, Lquasiquote,
		PUSH, R07, PUSH, RIPLINK, PUSH, RCODELINK,
		MVI, R07, 0l,
		MV, R00, RCODE,
		JAL, R00,
		POP, RCODELINK, POP, RIPLINK, POP, R07,
		PUSH, R01, PUSH, R02, /* Save r01 and r02 */
			MV, R01, R00,            /* Car object. */
			MVI, R02, onull,      /* Cdr object. */
			SYSI, objCons012,
			MVI, R01, sunquote,  /* Car object. */
			MV, R02, R00,            /* Cdr object. */
			SYSI, objCons012,
		POP, R02, POP, R01,   /* Restore r01 and r02 */
		BRA, Ldone,
		/* quasiquote? */
	 LABEL, Lquasiquote,
		BNEI, R00, SQUASIQUOTE, Lopenparen,
		PUSH, R07,
		PUSH, RIPLINK,
		PUSH, RCODELINK,
		MVI, R07, 0l,
		MV, R00, RCODE,
		JAL, R00,
		POP, RCODELINK,
		POP, RIPLINK,
		POP, R07,
		PUSH, R01,
		PUSH, R02, /* Save r01 and r02 */
			MV, R01, R00,             /* Car object. */
			MVI, R02, onull,       /* Cdr object. */
			SYSI, objCons012,
			MVI, R01, squasiquote,/* Car object. */
			MV, R02, R00,             /* Cdr object. */
			SYSI, objCons012,
		POP, R02,
		POP, R01,   /* Restore r01 and r02 */
		BRA, Ldone,
		/* open paren? */
	 LABEL, Lopenparen,
		BNEI, R00, SOPENPAREN, Lcharacter,
		PUSH, R07, PUSH, RIPLINK, PUSH, RCODELINK, /* Recursive call with isList set */
		MVI, R07, 1l,
		MV, R00, RCODE,
		JAL, R00,
		POP, RCODELINK, POP, RIPLINK, POP, R07,
		BRA, Ldone,
		/* character? */
	 LABEL, Lcharacter,
		BNEI, R00, SCHARACTER, Lsymbol,
		SYSI, sysNewCharacter,
		BRA, Ldone,
		/* symbol? */
	 LABEL, Lsymbol,
		BNEI, R00, SSYMBOL, Lstring,
		SYSI, sysNewSymbol,
		BRA, Ldone,
		/* string? */
	 LABEL, Lstring,
		BNEI, R00, SSTRING, Linteger,
		SYSI, sysNewString,
		BRA, Ldone,
		/* integer? */
	 LABEL, Linteger,
		BNEI, R00, SINTEGER, Lreal,
		SYSI, sysNewInteger,
		BRA, Ldone,
		/* real? */
	 LABEL, Lreal,
		BNEI, R00, SREAL, Loct,
		SYSI, sysNewReal,
		BRA, Ldone,
		/* oct number? */
	 LABEL, Loct,
		BNEI, R00, SOCT, Lhex,
		SYSI, sysNewOct,
		BRA, Ldone,
		/* hex number? */
	 LABEL, Lhex,
		BNEI, R00, SHEX, Lbinary,
		SYSI, sysNewHex,
		BRA, Ldone,
		/* binary number? */
	 LABEL, Lbinary,
		BNEI, R00, SBINARY, Lfalse,
		SYSI, sysNewBinary,
		BRA, Ldone,
		/* false? */
	 LABEL, Lfalse,
		BNEI, R00, SFALSE, Ltrue,
		MVI, R00, ofalse,
		BRA, Ldone,
		/* true? */
	 LABEL, Ltrue,
		BNEI, R00, STRUE, Ldefault,
		MVI, R00, otrue,
		BRA, Ldone,
		/* default to null? */
	 LABEL, Ldefault,
		MVI, R00, onull,

		/* List context?  If so recurse and contruct pair. */
	 LABEL, Ldone,
		BEQI, R07, 0l, Lret,
		PUSH, R00, /* Save object just parsed. */
		PUSH, RIPLINK, PUSH, RCODELINK, /* Recurse. */
		MV, R00, RCODE, /* r00 <- code register*/
		JAL, R00,
		POP, RCODELINK, POP, RIPLINK,
		/* Create pair from top of stack and just parsed object.  Since we need
		   r01 and r02 do a little pushing and moving of objects. */
		POP, R03, /* Restore object previously parsed. */
		PUSH, R01, PUSH, R02, /* Save r01 and r02 */
		MV, R01, R03, /* Car object. */
		MV, R02, R00, /* Cdr object. */
		SYSI, objCons012,
		POP, R02, POP, R01,   /* Restore r01 and r02 */
	
	 LABEL, Lret,
		RET
	);
	asmAssemble();
	r01=r00;

	/* r05 eventually gets a new token string buffer (copied from this) when the
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
	r02=r00;
	objNewString((Str)"ParserMain",10); /* Label */
	asmInit();
	asmAsm (
		//BRA, 8l,
		//r00, /* Label */
		MVI, R01, r02, /* The string buffer.  Lives in r05. */
		SYSI, objCopyString,
		MV, R05, R00,
		/* Initialize boolean to 'not parsing a list'. */
		MVI, R07, 0l,
		/* r01 gets port object from the required parameter to read.  (read portObject). */
		POP, R01,
		/* Call parser. */
		MVI, R00, r01,  /* Insert code block object directly via r01 from above. */
		PUSH, RIPLINK,
		PUSH, RCODELINK,
		PUSH, R09,
			JAL, R00,
		POP, R09,
		POP, RCODELINK,
		POP, RIPLINK,
		RET
	);
	asmAssemble();
	r01=r00;
	sysNewClosure1Env();
	DBEND();
}

/* Create a read-eval-print-loop closure in machine language.
*/
void wscmCreateRepl (void) {
 Obj Lrepl, Ldone, Lcompileerror;
	DBBEG();
	objNewSymbol ((Str)"\nVM>", 4);  r02=r00;
	objNewSymbol ((Str)"stdin", 5);  r01=r00;  sysTGEFind(); r03=r00; /* The stdin binding, not value incase it changes */
	objNewSymbol ((Str)"read", 4);  r01=r00;  sysTGEFind();  r04=caar(r00); /* The binding value is a closure so we need to car again for the code object. */
	objNewString ((Str)"bye\n", 4);  r05=r00;
	objNewString ((Str)"Entering repl2\n", 15);  r06=r00;
	asmInit();
	Lrepl = asmNewLabel();
	Ldone = asmNewLabel();
	Lcompileerror = asmNewLabel();
	asmAsm (
		MVI, R00, r06, /* "Entering REPL\n" */
		PUSH, R00,
		MVI, R01, 1l,
		SYSI, syscallDisplay,
		/* Display prompt. */
	 LABEL, Lrepl,
		MVI, R00, r02, // Prompt
		PUSH, R00,
		MVI, R01, 1l,
		SYSI, syscallDisplay,
		/* Call read. */
		PUSH, RIPLINK, PUSH, RCODELINK, PUSH, R09,
		MVI, R00, r03, // in
		LDI, R00, R00, 0l,
		PUSH, R00,
		MVI, R00, r04, // read wscmCreateRead
		JAL, R00,
		POP, R09, POP, RCODELINK, POP, RIPLINK,
		/* Done if an #eof parsed. */
		BEQI, R00, oeof, Ldone,
		/* Compile expression. */
		SYSI, compCompile,
		BEQI, R00, ofalse, Lcompileerror,
		/* Run code. */
		PUSH, RIPLINK, PUSH, RCODELINK, PUSH, R09,

		/* Display code block
		PUSH, R00,
		MVI, R01, 1l,
		SYSI, syscallDisplay,
		*/

		JAL, R00,
		POP, R09, POP, RCODELINK, POP, RIPLINK,
		/* (display ...) */
	 LABEL, Lcompileerror,
		PUSH, R00,
		MVI, R01, 1l,
		SYSI, syscallDisplay,
		BRA, Lrepl,
	 LABEL, Ldone,
		MVI, R00, r05, /* Bye message. */
		PUSH, R00,
		MVI, R01, 1l,
		SYSI, syscallDisplay,
		SYSI, osUnthread,
		NOP /* TODO Need this to avoid assembler crash.  the SYSI call requires a next block */
	);
	asmAssemble();
	r01=r00;
	sysNewClosure1Env();
	DBEND();
}


void wscmInitialize (void) {
	DBBEG();

	compInitialize(); /* asm vm mem os sys obj */

	/* Although already activated, pass in a virtual machine interrupt handler callback
	   which eventually calls the schduler.  Called when vmInterrupt is set.  */
	vmInitialize(wscmSchedule, objWrite);

	/* Register objects and pointer addresses with their
	   C source names for object debug dumps. */
	DB("Registering static pointer description strings");
	MEM_ADDRESS_REGISTER(wscmSysTransition);
	MEM_ADDRESS_REGISTER(sysEnvGet);
	MEM_ADDRESS_REGISTER(wscmSocketFinalizer);

	/* Bind usefull values r02=value r01=symbol. */
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
	//sysDefineSyscall(syscallAdd, "+");
	//sysDefineSyscall(syscallMul, "*");
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
	r00=ocharacters; sysDefine("characters");
	//r00=otaticIntegers; sysDefine("integers");
	//r00=osymbols; sysDefine("symbols");
	wscmCreateRead();  sysDefine("read");
	wscmCreateRepl();  sysDefine("repl2");
	objNewInt(0);  sysDefine ("a"); /* It's always nice to have x and y defined with useful values */
	objNewInt(1);  sysDefine ("b");
	objNewInt(42);  sysDefine ("y"); /* It's always nice to have x and y defined with useful values */
	objNewInt(69);  sysDefine ("x");
	objNewSymbol((Str)"a", 1);  r01=r00; /* It's also nice to have a pair ready to go */
	objNewSymbol((Str)"b", 1);  r02=r00;
	objCons012();  sysDefine("c");
	objNewVector(3); memVectorSet(r00, 0, (Obj)1);
	                 memVectorSet(r00, 1, (Obj)2);
	                 memVectorSet(r00, 2, (Obj)3);
	                 sysDefine("v");
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
	rcodelink = r00;
	riplink = (Obj)(0 * ObjSize);

	while (!done) {
		renv = rtge; /* Evaluate in TGE */
		fprintf(stderr, "\n== Read and parse ===============\nWSCM>");
		yyparse();/* Expr read into r00. */
		if (oeof == r00) done=1;

		fprintf(stderr, "\n== Compile ======================\n");
		sysWrite(r00, stderr);

		compCompile(); /* Compile r00 into VM runable code into r00 */

		rcode = r00;
		rip = 0;

		if (ofalse == r00) {
			fprintf(stderr, "\n*Compile failed*");
		} else {
			objDisplay(rcode, stderr);
			fprintf(stderr, "== Execute and return value =====\n");
			if (ofalse != r00) {
				vmRun();
			}
			objDisplay(r00, stderr);
		}

		DBE fprintf(stderr, "== Debug =======================");
		DBE memPrintStructures(stderr);
		DBE sysWrite(rstack, stderr);
		DBE for (i=memVecStackLength(rstack); 0<i; --i) sysWrite(memVecStackObject(rstack, i-1), stdout);
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
		vmPush(r00);  r01=(Obj)1;  syscallOpenFile();  r02=r00;
		/* Assign port to existing binding. */
		if (r02 != oeof) {
			objNewSymbol ((Str)"stdin", 5);  r01=r00;  sysTGEFind();
			memVectorSet(r00, 0, r02); /* Rebind stdin with the new port */
		}
	}

	objNewSymbol ((Str)"repl2", 5);  r01=r00;  /* wscmCreateRepl creates repl2 closure */
	sysTGEFind();
	r00 = caar(r00);

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
	//objDisplay(r00, stderr);
	compCompile(); /* Use the internal and only compiler */
	//objDisplay(r00, stderr);
	if (ofalse == r00) {
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
	objNewVector(argc); r01=r00;
	for (i=0; i<argc; i++) {
		objNewString((u8*)argv[i], strlen(argv[i]));
		memVectorSet(r01, i, r00);
	}
	r00=r01; sysDefine ("argv"); 

	DBEND();
}

int main (int argc, char *argv[]) {
	//setbuf(stdout, NULL);
	signal(SIGPIPE, SIG_IGN);
	srandom((unsigned int)time(NULL));

	wscmInitialize();
	sysInitialize((Str)argv[0]); /* Engage filesystem sandboxing */

	/* FUN: keep track of collector info.  Create a rootset object that holds list of
	   lists each containing info on the 4 current heaps. */
	/*
	memRootSetRegister(rheapinfo);
	rheapinfo = r00 = objCons(onull, onull);
	sysDefine("heapinfo");
	memInitialize(0, wscmCollectHeapInfo, 0);
	*/

	wscmBindArgs((Num)argc, argv);

	/* Set the C interrupt alarm handler and start its countdown timer.  The
	   handler will periodicaly set vmInterrupt causing wscmSchdule to be called
	   from the vm module. */
	//signal(SIGALRM, wscmSigAlarmHandler);
	//wscmSigAlarmReset(); /* Enable scheduler's interrupt timer. */

	/* REPL in a blocking C loop */
	wscmCReadEvalPrintLoop();  return 0;

	/* REPL in a blocking assembled code block */
	//wscmASMReadEvalPrintLoop(argc, argv);  return 0;

	/* REPL as compiled inlined scheme with asynchronous threads */
	//wscmStringReadEvalPrintLoop();
	return 0;
}

#undef DB_DESC
#undef DEBUG
