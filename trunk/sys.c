#define DEBUG 0
#define DB_MODULE "SYS "
#include "debug.h"

#include <assert.h>
#include <sys/stat.h>
#include "sys.h"
#include <termios.h>

const int MaxSemaphoreCount=64;

/* Concepts:
     (Operator operand operand operand ...)

     parameters:       Expressions evaluated during a procedure application.
     arguments:        Evaluated parameters (operand values) a function is
                       applied to.
     formal arguments: Function variables

     free variable:    Non-local binding.
     bound variable:   Local binding.

     "A bound variable gets a value stored in it's location"
*/

/******************************************************************************
 Usefull functions.
******************************************************************************/

/* Return number of milliseconds since epoch as a 64bit unsigned.
*/
s64 wscmTime (void) {
 struct timeval tv;
	gettimeofday(&tv, NULL);
	return (s64)tv.tv_sec*1000 + (s64)tv.tv_usec/1000;
}

/* Check that r1, argument stack count, is between the argument count range.
*/
Int wscmAssertArgumentCount (Num count, const char *functionName) {
 Int i;
	if (count == (Num)r1) return 0;
	/* Dump error and syscall/arguments. */
	if (count==1)
		fprintf(stderr, "\r\nERROR: Expecting 1 argument (%s",functionName);
	else
		fprintf(stderr, "\r\nERROR: Expecting %u arguments (%s",count,functionName);
	for (i=0; i<(Num)r1; i++) {
		write(1, " ", 1);
		wscmWrite(memStackObject(stack,(Num)r1-i-1), stderr);
	}
	write(1, ")\r\n", 3);
	/* Pop args from stack. */
	while (r1--) pop();
	r0 = false;
	return -1;
}

Int wscmAssertArgumentCountRange (Num min, Num max, const char *functionName) {
 Int i;
	if (min <= (Num)r1 && (Num)r1 <= max) return 0;
	/* Dump error and syscall/arguments. */
	fprintf(stderr, "\r\nERROR: Expecting %u to %u arguments (%s",min,max,functionName);
	for (i=0; i<(Num)r1; i++) {
		write(1, " ", 1);
		wscmWrite(memStackObject(stack,(Num)r1-i-1), stderr);
	}
	write(1, ")\r\n", 3);
	/* Pop args from stack. */
	while (r1--) pop();
	r0 = false;
	return -1;
}

Int wscmAssertArgumentCountMin (Num min, const char *functionName) {
 Int i;
	if (min <= (Num)r1) return 0;
	/* Dump error and syscall/arguments. */
	if (min == 1)
		fprintf(stderr, "\r\nERROR: Expecting at least %u argument (%s",min,functionName);
	else
		fprintf(stderr, "\r\nERROR: Expecting at least %u arguments (%s",min,functionName);
	for (i=0; i<(Num)r1; i++) {
		write(1, " ", 1);
		wscmWrite(memStackObject(stack,(Num)r1-i-1), stderr);
	}
	write(1, ")\r\n", 3);
	/* Pop args from stack. */
	while (r1--) pop();
	r0 = false;
	return -1;
}

/* Pop stack operand arguments into a new list.
		Requires:  r1 operand count
		Mutates: r2
		Returns: r0 new list
*/
void sysList (void) {
 Int count = (Int)r1;
	r0=null;
	while (count--) { r1=pop(); r2=r0; objCons12(); }
}



/******************************************************************************
 Internal object serializers and output function.
******************************************************************************/

/* Given a signed integer and base, create a new string object in r0
   representing the number's external signed representation.
   IE: wscmSerializeInteger(-256 16) => "-100"
*/
void wscmSerializeInteger (Int num, Int base) {
 u8 buff[32], /* Big enough to contain signed 32bit binary ascii number. */
    *ptr=buff+32;
 Int  signBit, nybble;
	DB("-->%s", __func__);
	// Special cases which the general algorithm doesn't handle gracefully.
	if (num == 0x80000000) objNewString((u8*)"-2147483648", 11);
	else if (num == 0) objNewString((u8*)"0", 1);
	else {
	 	if (num<0) {    /* Figure sign and normalize into a positive value. */
 			signBit=1;
 			num *= -1;
 		} else
 			signBit=0;
		while (num) { /* Construct the string in reverse. */
			nybble = num%base;
			*--ptr = nybble + (nybble<10 ? '0' : 'a'-10);
			num /= base;
		}
		if (signBit) *--ptr = '-'; /* Here's the sign. */
		objNewString(ptr, buff+32-ptr);
	}
	DB("<--%s", __func__);
}

Int wscmWriteR (Obj a, long islist, FILE *stream, Int max) {
 Int i, count=0;

	if (NULL==stream) stream=stderr;

	if (max <= 0) count += fprintf (stream, "...");
	else if (!memIsObjectValid(a)) count += fprintf(stream, "#<"HEX">", a);
	else switch (memObjectType(a)) {
		case TFALSE:
			count += fprintf(stream, "#f");
			break;
		case TTRUE:
			count += fprintf(stream, "#t");
			break;
		case TNULL:
			count += fprintf(stream, "()");
			break;
		case TNULLVEC:
			count += fprintf(stream, "#()");
			break;
		case TNULLSTR:
			count += fprintf(stream, "\"\"");
			break;
		case TEOF:
			count += fprintf(stream, "#eof");
			break;
		case TCHAR:
			 switch (*(char*)a) {
				case '\a'   : count += fprintf(stream, "\\bell");  break;
				case '\033' : count += fprintf(stream, "\\esc");  break;
				case '\233' : count += fprintf(stream, "\\csi");  break; /* CSI character.  ESC + 8th bit*/
				case '\n'   : count += fprintf(stream, "\\newline");  break;
				case '\r'   : count += fprintf(stream, "\\return");  break;
				case '\t'   : count += fprintf(stream, "\\tab");  break;
				case '\v'   : count += fprintf(stream, "\\vtab");  break;
				case ' '   : count += fprintf(stream, "\\space");  break;
				default     : count += fprintf(stream, "#\\%c", *(char*)a);
			}
			break;
		case TSTRING:
			count += fprintf(stream, "\"");
			for (i=0; i<memObjectLength(a); i++)
			 switch (((char*)a)[i]) {
				case '\\'   : count += fprintf(stream, "\\\\"); break;
				case '\"'   : count += fprintf(stream, "\\\""); break;
				case '\a'   : count += fprintf(stream, "\\a");  break;
				case '\033' : count += fprintf(stream, "\\e");  break;
				case '\233' : count += fprintf(stream, "\\c");  break; /* CSI character.  ESC + 8th bit*/
				case '\n'   : count += fprintf(stream, "\\n");  break;
				case '\r'   : count += fprintf(stream, "\\r");  break;
				case '\t'   : count += fprintf(stream, "\\t");  break;
				case '\v'   : count += fprintf(stream, "\\v");  break;
				default     : count += fprintf(stream, "%c", ((char*)a)[i]);
			 }
			count += fprintf(stream, "\"");
			break;
		case TSYMBOL:
			count += fwrite(a, 1, memObjectLength(a), stream);
			break;
		case TINTEGER:
			count += fprintf(stream, INT, *(Int*)a);
			break;
		case TREAL: 
			count += fprintf(stream, REAL, *(Real*)a);
			break;
	 	case TPAIR:
			if (!islist) count += fprintf(stream, "(");
			count += wscmWriteR(car(a), 0, stream, max-count);
			if (TPAIR == memObjectType(cdr(a))) {
				count += fprintf(stream, " ");
				count += wscmWriteR(cdr(a), 1, stream, max-count);
			} else {
				if (cdr(a)!=null) {
					count += fprintf(stream, " . ");
					count += wscmWriteR(cdr(a), 0, stream, max-count);
				}
			}
      	if (!islist) count += fprintf(stream, ")");
			break;
		case TVECTOR:
			count += fprintf(stream, "#(");
			for (i=0; i<memObjectLength(a) && count < max; i++) {
				if (i) count += fprintf(stream, " ");
				count += wscmWriteR(memVectorObject(a, i), 0, stream, max-count);
			}
			if (count > max) count += fprintf(stream, "...");
			count += fprintf(stream, ")");
			break;
		case TCLOSURE:
			count += fprintf(stream, "#CLOSURE<CODE "HEX"  ENV "HEX">", car(a), cdr(a));
			break;
		case TCONTINUATION:
			count += fprintf(stream, "#CONTINUATION<"HEX">", a);
			break;
		case TCODE:
			count += fprintf(stream, "#CODE<"HEX">", a);
			break;
		case TPORT:
		case TSOCKET:
			count += fprintf(stream, "#SOCKET<DESC:");
			count += wscmWriteR(memVectorObject(a, 0), 0, stream, max);
			count += fprintf(stream, " ADDR:");
			count += wscmWriteR(memVectorObject(a, 1), 0, stream, max);
			count += fprintf(stream, " PORT:");
			count += wscmWriteR(memVectorObject(a, 2), 0, stream, max);
			count += fprintf(stream, " STATE:");
			count += wscmWriteR(memVectorObject(a, 3), 0, stream, max);
			count += fprintf(stream, " NEXT:");
			count += wscmWriteR(memVectorObject(a, 4), 0, stream, max);
			count += fprintf(stream, ">");
			break;
		case TSYSCALL:
			count += fprintf(stream, "#SYSCALL<"OBJ">", a);
			break;
		case TSTACK :
			count += fprintf(stream, "#[%x |", memStackLength(a));
			for (i=0; i<memStackLength(a); i++) {
				count += fprintf(stream, " "HEX, memStackObject(a, memStackLength(a)-i-1));
			}
			count += fprintf(stream, "]");
			break;
	 	default:
			count += fprintf(stream, "#???<"HEX">", a);
	}

	return count;
}

Int wscmWrite    (Obj a, FILE *stream) { return wscmWriteR (a, 0, stream, LONG_MAX); }
Int wscmWriteMax (Obj a, FILE *stream, Int max) { return wscmWriteR (a, 0, stream, max); }

/* TODO convert this to FILE oriented output like I did to wscmWrite */
void wscmDisplay (Obj a, long islist, int fd) {
 static char buff[64];
 Int i, len;
	if ((Num)a < 0x100000) {
		i = sprintf(buff, "#<%x>", a);
		write(fd, buff, i);
	}
	else switch (memObjectType(a)) {
		case TFALSE:
			write (fd, "#f", 2);
			break;
		case TTRUE:
			write (fd, "#t", 2);
			break;
		case TNULL:
			write (fd, "()", 2);
			break;
		case TNULLVEC:
			write (fd, "#()", 3);
			break;
		case TNULLSTR:
			break;
		case TEOF:
			write (fd, "#eof", 4);
			break;
		case TCHAR:
		case TSTRING:
		case TSYMBOL:
			i=0;
			while (i<memObjectLength(a)) {
				len = write (fd, a+i, memObjectLength(a)-i);
				if (len > 0) i+=len;
			}
			break;
		case TINTEGER:
			len = sprintf(buff, INT, *(Int*)a);
			write(fd, buff, len);
			break;
		case TREAL: 
			len = sprintf(buff, "%.2f", *(Real*)a);
			write(fd, buff, len);	
			break;
	 	case TPAIR:
			if (!islist) write (fd, "(", 1);
			wscmDisplay(car(a), 0, fd);
			if (TPAIR == memObjectType(cdr(a))) {
				write (fd, " ", 1);
				wscmDisplay(cdr(a), 1, fd);
			} else {
				if (cdr(a)!=null) {
					write (fd, " . ", 3);
					wscmDisplay(cdr(a), 0, fd);
				}
			}
      	if (!islist) write (fd, ")", 1);
			break;
		case TVECTOR:
			write (fd, "#(", 2);
			for (i=0; i<memObjectLength(a); i++) {
				if (i) write (fd, " ", 1);
				wscmDisplay(memVectorObject(a, i), 0, fd);
			}
			write (fd, ")", 1);
			break;
		case TCLOSURE:
			len=sprintf (buff, "#CLOSURE<CODE "OBJ"  ENV:"OBJ">", car(a), cdr(a));
			write(fd, buff, len);
			break;
		case TCONTINUATION:
			break;
		case TCODE:
			len = sprintf(buff, "#CODE<"OBJ">", a);
			write(fd, buff, len);
			break;
		case TPORT:
		case TSOCKET:
			write(fd, "#SOCKET<", 8);
			wscmDisplay (memVectorObject(a, 0), 0, fd); write(fd, " ", 1);
			wscmDisplay (memVectorObject(a, 1), 0, fd); write(fd, " ", 1);
			wscmDisplay (memVectorObject(a, 2), 0, fd); write(fd, " ", 1);
			wscmDisplay (memVectorObject(a, 3), 0, fd); write(fd, " ", 1);
			wscmDisplay (memVectorObject(a, 4), 0, fd);
			write(fd, ">", 1);
			break;
		case TSYSCALL:
			i = sprintf(buff, "#SYSCALL<"OBJ">", a);
			write(fd, buff, i);
			break;
		case TSTACK :
			len = sprintf(buff, "#["HEX" |", memStackLength(a));
			write(fd, buff, len);
			for (i=0; i<memStackLength(a); i++) {
				len=sprintf(buff, " "HEX, memStackObject(a, memStackLength(a)-i-1));
				write(fd, buff, len);
			}
			write(fd, "]", 1);
			break;
	 	default:
			len = sprintf(buff, "#<"OBJ">", a);
			write(fd, buff, len);
	}
}

void wscmDumpTGE (void) {
 Obj o;
 Int len;
	o = cdr(tge);
	fprintf (stderr, "\n----TGE "OBJ"------------------------------------", o);
	while (o != null) {
		fprintf (stderr, "\n"OBJ" ", caar(o));
		len=wscmWrite (cdar(o), stderr);
		fprintf(stderr, "                "+((len<17)?len-1:15));
		wscmWriteMax (caar(o), stderr, 80);
		o=cdr(o);
	}
	fprintf (stderr, "\n\\------------------------------------------------------\n");
}

void sysDumpEnv (Obj env) {
 Obj formals;
 Int i, len;

	if (env==tge) fprintf (stderr, "\n----TGE "OBJ"------------------------------------\n...", env);

	while (env!=tge) {
		formals = memVectorObject(env, 1);
		fprintf (stderr, "\n----ENV "OBJ"------------------------------------", env);
		for (i=2; memObjectType(formals) == TPAIR; i++) {
			fprintf (stderr, "\n"OBJ" ", memVectorObject(env, i));
			len=wscmWrite (car(formals), stderr);
			fprintf(stderr, "                "+((len<17)?len-1:15));
			wscmWriteMax (memVectorObject(env, i), stderr, 80);
			formals = cdr(formals);
		}
		env=memVectorObject(env, 0); /* Consider parent env */
	}
	fprintf (stderr, "\n\\------------------------------------------------------\n");
}



/******************************************************************************
 Environment functions.
******************************************************************************/

/* Look for the symbol in r1 in the global environment.  Return in r0 the
   binding (value . symbol) or null if not found.
*/
void wscmTGEFind (void) {
	DB("ENV     -->wscmTGEFind <= ");
	DBE wscmWrite(r1, stderr);
	/* Scan over the list of (value . symbol) pairs. */
	for (r0=cdr(tge); r0!=null; r0=cdr(r0)) {
		//DBE wscmWrite(car(r0), stderr);
		if (cdar(r0) == r1) {
			r0 = car(r0);
			break;
		}
	}
	DB("ENV     <--wscmTGEFind => ");
	DBE wscmWrite(r0, stderr);
}

/* Given a symbol in r1, bind it to a new location if it doesn't already
   exist in the global environment.  The 'binding' is returned in r0. */
void wscmTGEBind (void) {
	DB("ENV -->wscmTGEBind");
	/* Look for symbol r1 in TGE. Binding returned in r0. */
	wscmTGEFind();
	if (null == r0) {
		/* Create new empty binding. */
		r2=r1;  r1=null;  objCons12();  /* (() . sym) */
		/* Insert binding after the first in the global environment list. */
		r1=r0;  r2=cdr(tge);  objCons12();   /* ((val . sym) . ...) */
		memVectorSet(tge, 1, r0);
		r0 = r1; /* return binding. */
		DB("ENV    Added binding ");
		DBE wscmWrite(cdr(r0), stderr);
	}
	DB("ENV <--wscmTGEBind");
}

/* Looks for first occurance of symbol in r1 in a chain of pseudo environments.
   Returns env link depth and local environment binding vector offset in the
   2nd and 1st byte fields respectively.
*/
Int wscmEnvFind (void) {
 Int ret, depth=0, offset;
	DB("COMP -->wscmEnvFind: ");
	DBE wscmWrite(r1, stderr);
	push(env);
	while (env != tge) {
		DB("        Examining env: ");
		DBE wscmWrite(cdr(env), stderr);
		/* Start at 2 since local environment values start at offset 2. */
		offset=2;
		for (r0=cdr(env); memObjectType(r0) == TPAIR; r0=cdr(r0)) {
			DB("        looking at:");
			DBE wscmWrite(car(r0), stderr);
			if (car(r0) == r1) { /* Pseudo env ( ^ a b c), so just check car */
				ret = (depth<<8) | offset;
				DB("        found in local environment");
				goto ret;
			}
			offset++;
		}
		env = memVectorObject(env, 0);
		depth++;
	}
	ret = 0;
 ret:
	env=pop();
	DB("COMP <--wscmEnvFind => %02x", ret);
	return ret;
}

/* Resolve the symbol value in current envionrment chain.
   env: of the form #(parent-env (z y x) 1 2 3)
        where tge is 
   r1 : symbol in question
   Clobbers: r2
   Return: r0
*/
void wscmEnvGet (void) {
 Int offset;
	DB("-->%s", __func__);
	push(env); /* Save environment. */
	while (env != tge) {
		/* Start at 2 since local environment values start at offset 2. */
		offset=2;
		r0=memVectorObject(env, 1);
		while (memObjectType(r0) == TPAIR) {
			if (car(r0) == r1) {
				r0 = memVectorObject(env, offset); /* Found in a local env. */
				goto ret;
			}
			offset++;
			r0=cdr(r0);
		}
		env = memVectorObject(env, 0);
	}
	wscmTGEFind();

	if (r0==null) {
		/* Symbol nonexistant.  Display error and return symbol. */
		fprintf (stderr, "\nERROR: wscmEnvGet() Unbound symbol:");
		wscmWrite(r1, stderr);
		fprintf (stderr, ".\n");
		r0 = r1;
	} else {
		r0 = car(r0);
	}
 ret:
	env=pop(); /* Restore environment. */
	DB("<--%s", __func__);
}



/*******************************************************************************
 Networking stuff.
*******************************************************************************/
void wscmOpenRemoteSocket (void);
void wscmOpenRemoteStream (void);

void wscmAcceptRemoteStream (void);

void wscmOpenLocalSocket (void);
void wscmOpenLocalStream (void);

void wscmAcceptLocalStream (void);

void wscmRecv (void);
void wscmRecvBlock (void);
void wscmSend (void);

void wscmOpenRemoteSocket (void) {
 char  hostname[128]={0};
 Int fd;
 struct hostent *he;
 struct sockaddr_in sai;
	DB("-->%s", __func__);
	r3=pop(); /* Port number. */
	r2=pop(); /* Internet address string. */
	fd = socket(PF_INET, SOCK_STREAM, 0);
	if (fd == -1) {
		fprintf (stderr, "ERROR: socket(PF_INET, SOCK_STREAM, 0) => -1\r\n");
		r0 = eof;
		goto ret;
	}

	/* Disable blocking on file descriptor. */
	fcntl (fd, F_SETFL, fcntl (fd, F_GETFL) | O_NONBLOCK);

	strncat(hostname, r2, memObjectLength(r2));
	he = gethostbyname(hostname);
	if (he == NULL) {
		fprintf (stderr, "ERROR wscmOpenRemoteSocket() gethostbyname(\"%s\")==NULL %d::%s\r\n", hostname, errno, strerror(errno));
		r0 = eof;
		goto ret;
	}
	sai.sin_family = AF_INET;
	sai.sin_port = htons(*(Num*)r3);
	sai.sin_addr.s_addr = *((Num*)he->h_addr);
	if (-1 == connect (fd, (struct sockaddr*)&sai, sizeof(sai))) {
		r4=sconnecting;
	} else {
		r4=sopen;
	}
	r1=(Obj)fd;
	objNewPort();
	
ret:
	DB("<--%s", __func__);
}

void wscmOpenRemoteStream (void) {
	DB("-->%s", __func__);
	if (memVectorObject(r1, 3) == sconnecting) wscmAcceptRemoteStream();

	if (r1!=eof && memVectorObject(r1, 3) == sconnecting) {
		DB("SYS    blocking on a remote connecting socket...");
		wscmUnRun();
		wscmMoveToQueue(running, blocked, sopenblocked);
		wscmSchedule();
	}
	DB("<--%s", __func__);
}

/* Expect listening socket port in r1.
*/
void wscmAcceptRemoteStream (void) {
 Int ret;
 struct pollfd fds={(Int)car(r1), POLLOUT, 0};
	DB("-->%s", __func__);
	ret = poll(&fds, 1, 0);
	if (ret == 1) {
		if (fds.revents & POLLOUT) {
			memVectorSet(r1, 3, sopen);
			r0=r1;
		} else {
			r0=r1=eof;
		}
	} else if (ret == -1) {
		r0=r1=eof;
	}
	DB("<--%s", __func__);
}


/* If any of this fails, like open-file, it should just return eof I guess.
   I'll be separating the listener socket from that returned from the
   accept call.  So listen should be non-blocking (polled by the thread).
   This blows but eventually it'll all be handeled in the kernel so the
   thread will be woken up when an accepted socket has been created.
	This has been done.
*/

void wscmOpenLocalSocket (void) {
 int ld, on=1;
 struct sockaddr_in sai;
	DB("-->%s", __func__);

	/* Local host port number. */
	if (memObjectType(r3=pop()) != TINTEGER) {
		r0 = "Invalid argument to wscmOpenLocalSocket().";
		r1 = (Obj)1;
		sysError();
		return;
	}

	ld = socket(PF_INET, SOCK_STREAM, 0);
	if (-1 == ld) {
		//printf ("ERROR: socket(PF_INET, SOCK_STREAM, 0)");
		r0 = eof;
		goto ret;
	}

	if (-1 == setsockopt (ld, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(int))) {
		//printf ("ERROR: setsockopt()");
		r0 = eof;
		goto ret;
	}

	sai.sin_family = AF_INET;
	sai.sin_port = htons(*(Num*)r3);
	sai.sin_addr.s_addr = htons(INADDR_ANY);
	if (-1 == bind (ld, (struct sockaddr*)&sai, sizeof(sai))) {
		//printf ("ERROR: bind() [%s]\r\n", strerror(errno));
		r0 = eof;
		goto ret;
	}

	if (-1 == listen(ld, 128)) {
		//printf("ERROR: listen()\r\n");
		r0 = eof;
		goto ret;
	}

	fcntl (ld, F_SETFL, fcntl (ld, F_GETFL) | O_NONBLOCK);

	r1=(Obj)(Int)ld;
	r2=r3;         /* Put port number in host field for the hell of it. */
	r4=saccepting; /* State object starts out in accepting state. */
	objNewPort();

ret:
	DB("<--%s", __func__);
}

void wscmOpenLocalStream (void) {
	DB("-->%s", __func__);
	/* Socket is listening so try and accept. */
	if (memVectorObject(r1, 3) == saccepting) wscmAcceptLocalStream();

	/* Is it the same socket? */
	if (memVectorObject(r1, 3) == saccepting) {
		DB("SYS    blocking on an accept a connection...");
		wscmUnRun();
		wscmMoveToQueue(running, blocked, sopenblocked);
		wscmSchedule();
	}
	DB("<--%s", __func__);
}

/* IN:  r1:port
  OUT:  r1:modified port
   Accept blocks.  Need to implement a blocking mechanism with the
   scheduler.  Hopefully it can coexist with the current fd blocked
   mechanism.  Expect a listening socket in r1.

   This should be called by read and the thread blocked if no connection
   has been made.
*/
void wscmAcceptLocalStream (void) {
 struct sockaddr sa;
 socklen_t salen;
 Int ld, fd2;
 char *name;
	DB("SYS -->wscmAcceptLocalStream <=");
	DBE wscmWrite(r1, stderr);
	ld = (Int)memVectorObject(r1, 0);
	salen = sizeof(struct sockaddr);
	if (-1 == (fd2=accept(ld, &sa, &salen))) {
		goto ret;
	}

	//close(ld); /* Close listening socket. */

	/* Disable C level blocking on the new descriptor. */
	fcntl (fd2, F_SETFL, fcntl (fd2, F_GETFL) | O_NONBLOCK);

	/* The file descriptor. */
	r1 = (Obj)fd2;

	/* The host IP address string. */
	name=inet_ntoa(((struct sockaddr_in*)&sa)->sin_addr);
	objNewString((u8*)name, strlen(name));
	r2=r0;

	objNewInt(ntohs(((struct sockaddr_in*)&sa)->sin_port));
	r3=r0;

	/* Port state to open "ready to read and write". */
	r4=sopen;

	objNewPort();
	r1=r0;

ret:
	DB("SYS <--wscmAcceptLocalStream => ");
	DBE wscmDisplay(r1, 0, 2);
}



/* In  r1 Port object.
       r2 Timeout integer or #f
       r3 Thing to read:  "..." specific length  "" any length including nothing  () one char
       r4 Bytes currently read already as an immediate integer

   Out r0 ""     No data ready yet
          #eof   No data will ever be ready
          "blah" All the read characters
          #f     No characters ready yet

	Blocks until timeout is reached (unless timeout is #f) or all the specified characters
	have been successfully read.
*/
void wscmRecv (void) {
 u8 buffer[0x2000];
 Int ret=0, len;
	DB("SYS -->wscmRecv <= ");
	DBE wscmWrite(r1, stderr);
	DBE wscmWrite(r2, stderr);
	DBE wscmWrite(r3, stderr);
	DBE wscmWrite(r4, stderr);
	if (r2 == 0) sysDebugger();

	/* Port is open and data is flowing. */
	if (memVectorObject(r1, 3) == sopen) {
		DB("SYS    reading from filedescriptor");
		/* Consider push-back character, will be #f if none there. */
		r0 = memVectorObject(r1, 4);

		/* Deal with 'any length' read request. */
		if (r3 == nullstr) {
			DB("SYS    reading any length");
			/* If char in push-back, deal with it. */
			if (r0 != false) {
				*buffer = *(char*)r0;
				ret = read(*(Int*)r1, buffer+1, 0xfff);
				if (ret<=0) ret=1;
				else ret++;
				memVectorSet(r1, 4, false); /* Clear push-back character. */
			} else
				ret = read(*(Int*)r1, buffer, 0x1000);
			if (ret>0) objNewString(buffer, ret); /* Return new string. */
			else if (ret==0) r0=eof;              /* File Descriptor closed. */
			else r0=nullstr;                      /* No bytes available yet. */
		/* Deal with character read and return. */
		} else if (r3==null) {
			DB("SYS    reading single character");
			/* Character already in push-back buffer. */
			if (r0 != false) {
				memVectorSet(r1, 4, false); /* Clear push-back character. */
			} else {
				DB("SYS    before ret="INT" *buffer="HEX02" errno="INT" %s", ret, *buffer, errno, strerror(errno));
				ret = read(*(Int*)r1, buffer, 1);
				DB("SYS    after  ret="INT" *buffer="HEX02" errno="INT" %s", ret, *buffer, errno, strerror(errno));
				if (ret == 1) r0=memVectorObject(characters, *buffer);
				else if (ret==0) r0=eof;
				else r0=false;
			}
		/* Deal with fixed length read request. */
		} else {
			DB("SYS    dealing with fixed length read request.  r3="HEX" Buffer = ", r3);
			len = memObjectLength(r3);
			DB("SYS    reading fixed length "NUM"/"INT, r4, len);
			if (r0 != false) {
				*((char*)r3+(Int)r4) = *(char*)r0;
				ret = read(*(Int*)r1, r3+(Int)r4+1, len-(Int)r4-1);
				if (ret<=0) ret = 1;
				else ret++;
				memVectorSet(r1, 4, false); /* Clear push-back character. */
			} else
				ret = read(*(Int*)r1, r3+(Int)r4, len-(Int)r4);
			if (ret>0) {
				r4+=ret;
				if ((Int)r4 == len) r0=r3; /* Return the string buffer obj. */
				else r0 = false;           /* Not ready yet ret false and block. */
			} else if (ret<0) r0=false;   /* Nothing read so keep blocking. */
			else { /* ret==0 File descriptor closed...see if anything was read. */
				if (r4 == 0) r0 = eof;
				else objNewString(r3, (Int)r4);
			}
		}
	} else { /* Port state must be closed. */
		DB("SYS    port must be closed");
		r0 = eof;
	}

	DB("SYS <--wscmRecv => r0="HEX, r0);
	//DBE wscmWrite (r0, stderr);
}

/* Called by sysRecv or VM syscall instruction.
   in   r1 port object
        r2 timout
        r3 string object buffer  "" any length  () one char  " ..." fill string
   use  r4 read count
   out  r0 string buffer
*/
void wscmRecvBlock (void) {
 s64 wakeupTime;
	DB("::%s  <= ", __func__);
	DBE wscmWrite(r4, stderr);
	DBE wscmWrite(r3, stderr);
	DBE wscmWrite(r2, stderr);
	DBE wscmWrite(r1, stderr);

	/* Time can be false meaning wait forever or the time the thread
	   should be woken up regardless of character availability. */
	if (r2 != false) {
		wakeupTime = wscmTime() + *(Int*)r2;
		objNewInt(wakeupTime);
		r2=r0;
	}

	r4=0; /* Character read count initialized to 0. */

	wscmRecv();
	if ((r2==false || (wscmTime() < *(Int*)r2)) && (r0 == false)) {
		wscmUnRun();
		wscmMoveToQueue(running, blocked, sreadblocked);
		wscmSchedule();
	}
	DB("  --%s  r0="OBJ, __func__, r0);
}


/* Given port object in r1, string in r2 and sent count in r3, send string to
   port object.
*/
void wscmSend (void) {
 Int ret, len;
	DB("-->%s : ", __func__);
	//DB("   r1 "); DBE wscmWrite(r1, stderr);
	//DB("   r2 "); DBE wscmWrite(r2, stderr);
	//DB("   r3 "); DBE wscmWrite(r3, stderr);

	if (r2 == nullstr) r0=nullstr;
	/* Port is open and data is flowing. */
	else if (memVectorObject(r1, 3) == sopen) {
		len = memObjectLength(r2);
		DB("   sent so far %d/%d", r3, len);
		ret = write(*(Int*)r1, r2+(Int)r3, len-(Int)r3);
		if (ret>0) {
			r3 = (Obj)(ret + (Int)r3);
			/* Return true. Can't return the string in case the 'string' is #f.
			   which implies nothing sent and that the thread should block (bug).*/
			r0 = (len == (Int)r3) ? true : false;
		} else if (ret<=0) {
			if(errno == EAGAIN) {
				r0=false;/* Nothing sent so block. */
			} else {
				DB("ERROR: Unknown return value from system send [%s]", strerror(errno));
				r0=eof; /* ret==0 closed fd so return eof. */
			}
		}
	} else
		r0 = eof;

	DB("   sent so far %d/%d", r3, len);
	DB("<--%s", __func__);
}


/*******************************************************************************
 Scheduling stuff.  Process queues are doubly linked lists implemented as
 vectors #(datum next prev).
*******************************************************************************/

/* Insert thread queue entry t between p and q where p is the previous thread
   to q.  Also works when the queue is empty.
      p     t     q              q     t     q
      t->   q->   p->            t->   q->   t->            
    <-q   <-p   <-t            <-t   <-q   <-t
*/
void wscmInsertThread (Obj t, Obj q) {
 Obj p, n;
	p = memVectorObject(q, 2);
	n = q;
	memVectorSet(t, 1, n); /* Set next link. */
	memVectorSet(t, 2, p); /* Set previous link. */
	memVectorSet(n, 2, t); /* Set next's previous link. */
	memVectorSet(p, 1, t); /* Set previous' next link. */
}

/* Remove thread from its doubly linked list.
*/
void wscmRemoveThread (Obj t) {
 Obj p, n;
	DB("-->%s", __func__);
	if (t!=ready && t!=sleeping && t!=blocked) {
		p = memVectorObject(t, 2); /* Previous doubly linked list entry. */
		n = memVectorObject(t, 1); /* Next doubly linked list entry. */
		memVectorSet(p, 1, n); /* Set previous' next to next. */
		memVectorSet(n, 2, p); /* Set next's previous to previous. */
		/* Keep running pointer on the ready queue. */
		if (t==running) running=p;
	} else {
		fprintf (stderr, "WARNING: %s: Attempting to remove thread queue head.\r\n", __func__);
	}
	DB("<--%s", __func__);
}

/* Move thread from its current queue to specified queue.
	State one of: sready srunning ssleeping ssemaphore sopenblocked sreadblocked swriteblocked
*/
void wscmMoveToQueue (Obj thread, Obj queue, Obj state) {
	DB("-->%s", __func__);
	/* To keep round robin scheduler happy we need to trick it by moving
	   the running pointer back a thread. */
	memVectorSet(car(thread), 2, state); /* Set thread's new state. */
	wscmRemoveThread(thread);
	/* If inserting into ready queue, insert behind the running thread,
	   otherwise insert at end of passed queue. */
	wscmInsertThread(thread, queue==ready?running:queue);
	DB("<--%s", __func__);
}

/* Creates a new thread object which is inserted into the running queue.

	Requires: r0 code block
	Mutates: r0 r1 r2
	Returns: r0 thread id
*/
void wscmNewThread (void) {
 Int tid;
	DB("-->%s", __func__);

	/* Find next available thread id.  Thread table's first entry
	   is the count so index range is 1 to MAX_THREADS inclusive. */
	for (tid=1; memVectorObject(threads, tid) != null; ++tid)
		if (MAX_THREADS <= tid) {
			fprintf (stderr, "WARNING: wscmNewThread: Too many threads.");
			r0 = false;
			goto done;
		}

	DB("\nOS    Thread id: %d\n", tid);

	/* Create thread's stack (in an un-running state). */
	r1=r0; /* Code block */
	memNewStack(); r2=r0;
	memStackPush(r2, env); /* Initial environment. */
	memStackPush(r2, 0);   /* Initial ip. */
	memStackPush(r2, r1);  /* Initial code. */
	memStackPush(r2, 0);   /* Initial retenv. */
	memStackPush(r2, 0);   /* Initial retip. */
	memStackPush(r2, 0);   /* Initial retcode. */
	memStackPush(r2, 0);   /* Initial r7. */
	memStackPush(r2, 0);   /* Initial r6. */
	memStackPush(r2, 0);   /* Initial r5. */
	memStackPush(r2, 0);   /* Initial r4. */
	memStackPush(r2, 0);   /* Initial r3. */
	memStackPush(r2, 0);   /* Initial r2. */
	memStackPush(r2, 0);   /* Initial r1. */
	memStackPush(r2, 0);   /* Initial r0. */

	/* Create thread descriptor #( #<stack> tid 'state) and add to thread table vector */
	objNewVector(3);
	memVectorSet(r0, 0, r2);      /* Set stack */
	memVectorSet(r0, 1, (Obj)tid); /* Set id */
	memVectorSet(r0, 2, sready);  /* Set 'ready' state */

	/* Add thread descriptor to thread table vector and increment count */
	memVectorSet(threads, tid, r0);
	memVectorSet(threads, 0, memVectorObject(threads, 0)+1);

	/* Create new doubly linked list queue element for this thread descriptor and
	   insert into the ready queue before the current running thread */
	r1=r0; /* Thread descriptor */
	objNewVector(3);
	memVectorSet(r0, 0, r1);
	wscmInsertThread (r0, running);

	objNewInt(tid);
done:
	DB("<--%s id:"INT, __func__, tid);
}

Obj wscmThreadStack (Obj t) { return memVectorObject (t, 0); }
Obj wscmThreadId    (Obj t) { return memVectorObject (t, 1); }
Obj wscmThreadState (Obj t) { return memVectorObject (t, 2); }
Int wscmIsQueueEmpty (Obj q) { return cdr(q)==q; }

Int wscmQueueCount (Obj q) {
 Obj t;
	Int i=0;
	for (t=cdr(q); t!=q; t=cdr(t)) i++;
	return i;
}

/* Make the running thread (the stack) ready to be stashed away.
*/
void wscmUnRun (void) {
 Obj threadDescriptor;
	DB("-->%s", __func__);

	/* Verify thread is in runningstate then change to ready state. */
	threadDescriptor = car(running);
	assert(memVectorObject(threadDescriptor, 2) == srunning);
	memVectorSet(threadDescriptor, 2, sready);

	push (env);
	push (ip);
	push (code);
	push (retenv);
	push (retip);
	push (retcode);
	push (r7);
	push (r6);
	push (r5);
	push (r4);
	push (r3);
	push (r2);
	push (r1);
	push (r0);

	DB("<--%s", __func__);
}

/* Make running thread ready for the VM.  Pop all the saved registers. */
void wscmRun (void) {
	DB("OS -->wscmRun() <= thread:%d ", memVectorObject(car(running),1));
	if (memVectorObject(car(running),2) != sready) {
		fprintf (stderr, "WARNING: wscmRun: not a 'ready thread:");
		wscmDisplay(memVectorObject(car(running),2), 0, 2);
	} else {
		/* Get stack from descriptor. */
		stack = memVectorObject(car(running),0);
		r0 = pop();
		r1 = pop();
		r2 = pop();
		r3 = pop();
		r4 = pop();
		r5 = pop();
		r6 = pop();
		r7 = pop();
		retcode=pop();
		retip=pop();
		retenv=pop();
		code=pop();
		ip=pop();
		env=pop();
		memVectorSet(car(running), 2, srunning);
	}
	DB("OS <--wscmRun() => ");
	DBE wscmWrite(car(running), stderr);
}

/* Put thread on sleep list.  Stack contains the millisecond count to sleep.
*/
void wscmSleepThread (void) {
 s64 wakeupTime;
	DB("OS -->wscmSleepThread=>");
	r0 = pop(); /* The call to sleep returns the argument passed to it. */
	wscmUnRun();
	wakeupTime = wscmTime() + *(Int*)r0;
	DB("      wakeupTime = %lld/%lld", wakeupTime, wscmTime());
	/* Wakeup time (u64) goes on top of stack. */
	objNewInt(wakeupTime);
	push(r0);

	/* Put this thread in order of wakup time in the sleeping list.  */
	r3=cdr(sleeping);
	while (r3 != sleeping) {
		if (*(s64*)memStackObject(memVectorObject(car(r3),0),0) > wakeupTime)
			break;
		r3 = cdr(r3);
	}
	wscmMoveToQueue(running, r3, ssleeping); /* Insert thread into list. */

	/* Go setup another thread to start running. */
	wscmSchedule();
	DB("OS <--wscmSleepThread");
}

/* Deal with sleeping threads that need to wake up.  If nothing in ready queue
   nor blocked queue then wait for topmost thread to wakeup. */
void wscmScheduleSleeping (void) {
 Obj sleepingThreadDescriptor;
 s64 wakeupTime;
	DB("-->%s", __func__);
	sleepingThreadDescriptor = cadr(sleeping);
	/* Next thread's wakeup time on top of its stack. */
	wakeupTime = *(s64*)memStackObject(memVectorObject(sleepingThreadDescriptor,0),0) - wscmTime();
	/* Only sleeping threads exist so wait for next one to wakeup. */
	if (wakeupTime>0 && wscmIsQueueEmpty(ready) && wscmIsQueueEmpty(blocked)) {
		DB("OS    Waiting %lld", wakeupTime);
		/* Disable scheduler's interrupt timer as it'll interrupt our
		   sleeping.  It will be reactivated by the scheduler*/
		ualarm(0,0);
		usleep(wakeupTime*1000);
		wakeupTime=0;
	}

	/* If next sleeping thread is ready to be woken up, insert into ready
	   queue. */
	if (wakeupTime <= 0) {
		DB("OS    Waking");
		/* Pop wake-time from stack. */
		memStackPop(memVectorObject(sleepingThreadDescriptor, 0));
		wscmMoveToQueue(cdr(sleeping), ready, sready);
	}

	DB("<--%s", __func__);
}

/* Move all blocked threads that can and have read a chracter from their
   descriptor to the ready queue.
   Have yet to block threads on a remote connection...blocks the
   entire process on an (open-socket "remote" port). FIXED.

	use r1 r5

*/
void wscmScheduleBlocked (void) {
	DB("-->%s <= %d blocked threads", __func__, objListLength(blocked)-1);

	DBE fprintf(stderr, "   %s          sleeping queue:%d\r\n", __func__, wscmQueueCount(sleeping));
	DBE fprintf(stderr, "   %s          blocked queue :%d\r\n", __func__, wscmQueueCount(blocked));
	/* For each thread r5 in blocked queue... */
	r5=cdr(blocked);
	while (r5!=blocked) {
	DB("   %s considering thread %d\r", __func__, memVectorObject(car(r5), 1));
		/* Consider status in the descriptor of this thread. */
		r1 = memVectorObject(car(r5),2);

		if (r1 == sreadblocked) {
			DB("   dealing with a read blocked thread");
			r1 = memStackObject(memVectorObject(car(r5),0),1l);
			r2 = memStackObject(memVectorObject(car(r5),0),2l);
			r3 = memStackObject(memVectorObject(car(r5),0),3l);
			r4 = memStackObject(memVectorObject(car(r5),0),4l);
			wscmRecv();
			if (r0 != false || (r2!=false && *(Int*)r2 <= wscmTime())) {
				/* Set thread's return value (r0 register top of stack) with
			   	newly-read string or #f if it has timed out. */
				memStackSet(memVectorObject(car(r5), 0), 0, r0);
				r1=cdr(r5);
				wscmMoveToQueue(r5, ready, sready);
				r5=r1;
			/* Store back registers into thread keeping it blocked. */
			} else {
				memStackSet(memVectorObject(car(r5), 0), 1l, r1);
				memStackSet(memVectorObject(car(r5), 0), 2l, r2);
				memStackSet(memVectorObject(car(r5), 0), 3l, r3);
				memStackSet(memVectorObject(car(r5), 0), 4l, r4);
				r5 = cdr(r5);
			}
		}
		else if (r1 == swriteblocked) {
			DB("   dealing with a write blocked thread");
			r1 = memStackObject(memVectorObject(car(r5),0),1);
			r2 = memStackObject(memVectorObject(car(r5),0),2);
			r3 = memStackObject(memVectorObject(car(r5),0),3);
			wscmSend();
			if (r0 != false) {
				DB("   unblocking thread");
				/* Set thread's return value (r0 register top of stack) with
			   	sent string. */
				memStackSet(memVectorObject(car(r5), 0), 0, r2);
				r1=cdr(r5);
				wscmMoveToQueue(r5, ready, sready);
				r5=r1;
			/* Store back registers into thread since wscmSend more than likely
			   changed them and keep this thread blocked. */
			} else {
				DB("   not unblocking thread");
				memStackSet(memVectorObject(car(r5), 0), 1, r1);
				memStackSet(memVectorObject(car(r5), 0), 2, r2);
				memStackSet(memVectorObject(car(r5), 0), 3, r3);
				r5 = cdr(r5);
			}
		} else if (r1 == sopenblocked) {
			DB("OS    dealing with a open-blocked thread");
			/* Snag port from sleeping thread (r1). */
			r1 = memStackObject(memVectorObject(car(r5),0),1);
			/* If a connection is made on the port and set to a non-accepting
			   state, set the threads return value (r0) to the port and move the
			   thread to the ready queue. */
			if (memVectorObject(r1, 3) == saccepting) {
				DB("OS    dealing with a new incomming stream connection thread");
				push(r5); /* Since the following clobbers r5. */
				wscmAcceptLocalStream();
				r5=pop();
				if (memVectorObject(r1, 3) != saccepting) {
					memStackSet(memVectorObject(car(r5), 0), 0, r1);
					r1=cdr(r5);
					wscmMoveToQueue(r5, ready, sready);
					r5=r1;
				}
				r5 = cdr(r5);
			} else if (memVectorObject(r1, 3) == sconnecting) {
				DB("OS    dealing with a new remote stream connection thread");
				wscmAcceptRemoteStream();
				if (r1==eof || memVectorObject(r1, 3) != sconnecting) {
					memStackSet(memVectorObject(car(r5), 0), 0, r1);
					r1=cdr(r5);
					wscmMoveToQueue(r5, ready, sready);
					r5=r1;
				}
				r5 = cdr(r5);
			} else if (memVectorObject(r1, 3) == sclosed) {
				memStackSet(memVectorObject(car(r5), 0), 0, eof);
				r1=cdr(r5);
				wscmMoveToQueue(r5, ready, sready);
				r5=r1;
			} else { /* Must be in a connecting state. */
				r5 = cdr(r5);
			}
		} else if (r1 == ssemaphore) {
			/* Skip semaphore blocked threads. */
			r5 = cdr(r5);
		} else {
			fprintf (stderr, "ERROR; wscmScheduleBlocked: unknown thread state.");
			r5 = cdr(r5);
		}
	}
	DB("<--%s", __func__);
}

void wscmScheduleSemaphoreBlocked (Int sem) {
 Int value, found=0;
	DB("\e[33m-->%s\n", __func__);
	r4=cdr(blocked); /* For each thread r4 in blocked queue... */
	while (r4!=blocked) {
		/* Check thread status in its descriptor. */
		if (memVectorObject(car(r4),2) == ssemaphore) {
			/* Look at thread's r0 register stored on its stack. */
			value = (Int)memStackObject(memVectorObject(car(r4),0),1);
			if (value == sem) {
				DB("   unblocking thread");
				DB("   %s UnBlocking thread %d\n", __func__, memVectorObject(car(r4), 1));
				/* Set thread's return value (r0 register top of stack) with
			   	new semaphore value. */
				objNewInt(*(Int*)memVectorObject(semaphores, sem));
				memStackSet(memVectorObject(car(r4), 0), 0, r0);
				wscmMoveToQueue(r4, ready, sready);
				r4=blocked; /* Force exit from loop. */
				found=1;
			} else {
				r4 = cdr(r4);
			}
		} else {
			r4 = cdr(r4);
		}
	}
	if (!found) {
		DB ("ERROR: Couldn't find thread blocked on semaphore %d.", sem);
		exit (-1);
	}
	DB("<--%s\e[0m\n", __func__);
}

void wscmSchedule (void) {
	//fprintf(stderr, "::%s\r", __func__);
	if (!wscmIsQueueEmpty(sleeping)) wscmScheduleSleeping();
	if (!wscmIsQueueEmpty(blocked)) wscmScheduleBlocked();
	while (wscmIsQueueEmpty(ready)) {
// All are aparently getting blocked.
	//fprintf (stderr, "   %s [t:%d b:%d s:%d sem:%d]\r", __func__, objDoublyLinkedListLength(ready), objDoublyLinkedListLength(blocked), objDoublyLinkedListLength(sleeping), *(Int*)memVectorObject(semaphores, 0));
		//fprintf(stderr, "   %s looping: ready queue empty\rn", __func__);
		/* No more threads so shutdown. */
		if (wscmIsQueueEmpty(sleeping) && wscmIsQueueEmpty(blocked)) {
			//fprintf(stderr, "   No more threads.  Bye bye.");
			exit(0);
		}
		if (!wscmIsQueueEmpty(sleeping)) wscmScheduleSleeping();
		if (!wscmIsQueueEmpty(blocked))  wscmScheduleBlocked();
		/* Since sleeping threads have been dealt with and there are no ready
		   theads but we need to wait for blocked threads, sleep a bit and try
		   again later. */
		if (wscmIsQueueEmpty(ready)  && !wscmIsQueueEmpty(blocked)) {
			ualarm(0,0);       /* Disable scheduler's interrupt timer. */
			usleep(50*1000);  /* Sleep 50 milliseconds and try again. */
		}
	}

	/* Switch to another thread.  Round robin scheme.  Just go to next thread. */
	//fprintf(stderr, "   Round robin");
	running = cdr(running);
	if (running==ready) running=cdr(ready); // Can this happen?
	if (running==ready) fprintf (stderr, "ERROR: deal with this!");
	vmSigAlarmReset(); /* Enable scheduler's interrupt timer. */
	wscmRun();
	//fprintf(stderr, "  --%s =>%d\rn", __func__, memVectorObject(car(running), 1));
}

/* Force a call to the error continuation.  This better be defined in
   The Global Environment.  R1 contains number of expressions on stack
	to display as part of the error handling.  R0 holds the invalid
	operator or message.
*/
void sysError (void) {
 Int tid;
	DB("::%s", __func__);

	/* Create list in r4 of the operator and stack operand arguments.
	   Operator is in r0, operands are on the stack and r1 is operand count. */
	r3=r0;
	sysList();
	r1=r3; r2=r0; objCons12(); r3=r0;
	
	/* Look up error function/continuation in ERRORS vector
	   in TGE and set the code register and IP:
	     r0            The ERRORS vector  of closures/continuations
	     (car running) The current thread
	     (cdr thread)  Current thread ID
	     (car closure) Code block
	     (cdr closure) Parent environment */
	objNewSymbol ((Str)"ERRORS", 6);  r1=r0;  wscmTGEFind(); r0=car(r0);
	tid = (Int)cdr(car(running));
	/* r0 needs to remain the closure when a code block is first run
	   since it needs to initially extend the lexical environment */
	r0 = memVectorObject(r0, tid);
	code=car(r0);  ip=0;

	/* Push operand and set operand count */
	push(r3);
	r1=(Obj)1;

	DB("  --%s", __func__);
}

/******************************************************************************
 System calls
******************************************************************************/

void sysQuit (void) { exit(0); }

/* This can do anything.  I change it mainly for debugging and prototyping. */
void sysFun (void) {
 Int i;
	//memDebugDumpYoungHeap(stderr);
	//memValidateHeapStructures();
	//sysDumpCallStackCode();
	fprintf (stderr, "[%d]", memStackLength(stack));
	for (i=memStackLength(stack)-1; 0<=i; --i) wscmWrite(memStackObject(stack, i), stdout);
}


/* 1. Call the function and pass it 5
   2. Call the function and pass code
   3. Call the function and pass the continuation code
	Maybe this should be compiled?
	(call/cc fn) => (fn continuation)
*/

/* Stored stack expected in r1.
*/
void sysReinstateContinuation (void) {
 Int length;

	if ((Int)r1==1) r0=pop();
	else {
		fprintf (stderr, "ERROR: %s() bad argument count %d.\n", __func__, (Int)r1);
		exit (-1);
	}

	/* Reinstate stack and registers.
	*/
	length = memObjectLength(r3); /* The stored stack is in r3. */
	memcpy(stack+8, r3, length*8); /* Copy objects into stack vector. */
	*(Obj*)stack = stack+length*8; /* Set the stack object pointer. */
	code = pop();
	retcode = pop();
	ip = pop();
	retip = pop();
	env = pop();
	retenv = pop();
}
void sysCreateContinuation (void) {
	DB("-->%s", __func__);
 Int length;
	push(retenv);
	push(env);
	push(retip);
	push(ip);
	push(retcode);
	push(code);
	length = memStackLength(stack);
	objNewVector(length);
	memcpy(r0, stack+ObjSize, length*ObjSize);
	pop(); pop(); pop(); pop(); pop(); pop();
	r1=r0;
	/* r1 now has a copy of the stack */

	/* Create a function that will reinstate this stack at runtime. */
	push(asmstack);
	memNewStack(); asmstack=r0;
	asmAsm(
		MVI3, r1,  /* Stored copy of stack in r3. */
		SYSI, sysReinstateContinuation,
		END
	);
	asmNewCode();  r1=r0;
	objNewClosure1Env();
	memVectorSet(r0, 1, tge); /* Set to TGE just in case. */
	asmstack = pop();

	/* Skip the "continuation" jump in the code just after this syscall
	   in the compiled code.  See compCallcc() */
	ip += 2;
	DB("<--%s", __func__);
}

void sysDumpThreads (void) {
	wscmWrite(threads, stderr);
}

void sysString (void) {
 Int i=0, l=0, len;
	DB("SYS -->sysString");
	while (l<(Int)r1) i += memObjectLength(memStackObject(stack, l++));
	if (!i) {
		while (l--) pop();
		r0= nullstr;
	} else {
		memNewArray(TSTRING, i);
		while (l--) {
			r1 = pop();
			i -= (len = memObjectLength(r1));
			memcpy(r0+i, r1, len);
		}
	}
	DB("SYS <--sysString");
}

void sysMakeString (void) {
 Int len;
 char fill=' ';
	DB("-->%s", __func__);
	if (wscmAssertArgumentCountRange(1, 2, __func__)) return;

	/* Fill character if specified. */
	if (2 == (Int)r1)  fill = *(char*)pop();

	/* Create string of passed length. */
	objNewString(NULL, len=*(Int*)pop());

	/* Fill string if fill character specified. */
	if (2 == (Int)r1)  while (len--) ((char*)r0)[len]=fill;

	DB("<--%s", __func__);
}

void sysSubString (void) {
 Int end=0, start=0;
	DB("SYS -->sysSubString");
	if (wscmAssertArgumentCount(3, __func__)) return;
	end=*(Int*)(r2=pop());
	start=*(Int*)(r1=pop());
	if (end==start) {
		r1=pop();
		r0=nullstr;
	} else {
		if (end-start < 0) {
			pop();
			objNewString((u8*)"Invalid range to substring.", 27);  push(r0);
			push(r2); push(r1);
			r1 = (Obj)3;
			sysError();
		} else {
			memNewArray(TSTRING, end-start);
			r1=pop();
			memcpy(r0, r1+start, end-start);
		}
	}
	DB("SYS <--sysSubString");
}

void sysStringLength (void) {
	DB("SYS -->sysStringLength");
	if (wscmAssertArgumentCount(1, __func__)) return;
	objNewInt(memObjectLength(pop()));
	DB("SYS <--sysStringLength");
}

/* Returns an array object in r0 (ignore the type) reprsenting an external
   representation of the object.  Only simple object types are converted. 
   Complex ones or immediate pointers below 2^20 are shown as just hex
   addresses.
*/
void sysSerializeDisplay (void) {
 static u8 buff[8192];
 Int len;
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(1, __func__)) return;
	r0 = pop();
	if ((Num)r0 < 0x100000l) {
		len = sprintf((char*)buff, "#"OBJ, (Num)r0);
		objNewString(buff, len);
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
			wscmSerializeInteger(*(Int*)r0, 10);
			break;
		case TREAL   : 
			len = sprintf((char*)buff, "%.2f", *(Real*)r0);
			objNewString(buff, len);
			break;
		case TCHAR   : 
			objNewString(r0, 1);
			break;
		default      :
			len = sprintf((char*)buff, HEX, (Num*)r0);
			objNewString(buff, len);
			break;
	}
	DB("<--%s", __func__);
}
void sysSerializeWrite (void) {
 static Chr buff[8192];
 Int len, i;
	DB("SYS -->%s", __func__);
	if (wscmAssertArgumentCount(1, __func__)) return;
	r0 = pop();
	if ((Num)r0 < 0x100000l) {
		len = sprintf((char*)buff, "#"HEX, (Num)r0);
		objNewString(buff, len);
	} else switch (memObjectType(r0)) {
		case TSYMBOL : 
		case TEOF    :
		case TNULL   :
		case TNULLVEC:
		case TFALSE  :
		case TTRUE   :
			break;
		case TINTEGER:
			wscmSerializeInteger(*(Int*)r0, 10);
			break;
		case TREAL   : 
			len = sprintf((char*)buff, "%.2f", *(Real*)r0);
			objNewString(buff, len);
			break;
		case TCHAR   : 
			len = sprintf((char*)buff, "#\\%c", *(char*)r0);
			objNewString(buff, len);
			break;
		case TNULLSTR:
		case TSTRING : 
			len = 0;
			buff[len++] = '"';
			for (i=0; i<memObjectLength(r0); i++)
			 switch (((char*)r0)[i]) {
				case '\\'   : buff[len++]='\\'; buff[len++]='\\'; break;
				case '\"'   : buff[len++]='\\'; buff[len++]='"'; break;
				case '\a'   : buff[len++]='\\'; buff[len++]='a'; break;
				case '\033' : buff[len++]='\\'; buff[len++]='e'; break;
				case '\233' : buff[len++]='\\'; buff[len++]='c'; break;
				case '\n'   : buff[len++]='\\'; buff[len++]='n'; break;
				case '\r'   : buff[len++]='\\'; buff[len++]='r'; break;
				case '\t'   : buff[len++]='\\'; buff[len++]='t'; break;
				case '\v'   : buff[len++]='\\'; buff[len++]='v'; break;
				case '\b'   : buff[len++]='\\'; buff[len++]='b'; break;
				default     : buff[len++]=((char*)r0)[i];
			 }
			buff[len++] = '"';
			objNewString(buff, len);
			break;
		default      :
			len = sprintf((char*)buff, "#"HEX, (Num*)r0);
			objNewString(buff, len);
			break;
	}
	DB("SYS <--%s", __func__);
}

void sysNumber2String (void) {
 Int num, base;
	DB("SYS -->%s", __func__);
	if (wscmAssertArgumentCountRange(1, 2, __func__)) return;
	base = (Num)r1==2 ? *(Num*)pop() : 10; /* Default base is 10. */
	num = *(Int*)pop();
	wscmSerializeInteger (num, base);
	DB("SYS <--%s", __func__);
}

/* For internal use only.  Probably to be phased out in favor of a VM
   implementation.
*/
void sysWrite (void) {
 Int fd=1;
 FILE *stream=NULL;
	DB("SYS -->sysWrite");

	if (wscmAssertArgumentCountRange(1, 2, __func__)) return;
	if ((Int)r1==2) fd=*(Int*)pop(); /* File descriptor. */

	if (fd==1) stream = stdout;
	if (fd==2) stream = stderr;
	assert(NULL != stream);

	wscmWrite(r0=pop(), stream);
	DB("SYS <--sysWrite");
}
void sysDisplay (void) {
 Int fd=1;
	DB("SYS -->sysDisplay");
	if (wscmAssertArgumentCountRange(1, 2, __func__)) return;
	if ((Int)r1==2) fd=*(Int*)pop(); /* Descriptor. */
	wscmDisplay(r0=pop(), 0, fd);
	DB("SYS <--sysDisplay");
	return;
}

void sysVector (void) {
 Int l=(Int)r1;
	DB("SYS -->sysVector");
	if (l==0) r0=nullvec;
	else {
		objNewVector(l);
		while (l--) memVectorSet(r0, l, pop());
	}
	DB("SYS <--sysVector");
}

void sysMakeVector (void) {
 Num len;
	DB("-->%s", __func__);
	if (wscmAssertArgumentCountRange(1, 2, __func__)) return;

	r2 = r1==(Obj)2 ? pop() : null;
	len = *(Num*)pop();
	if (len<1) r0=nullvec;
	else {
		objNewVector(len);
		while (len--) memVectorSet(r0, len, r2);
	}

	DB("<--%s", __func__);
}

void sysRandom (void) {
	DB("SYS -->sysRandom");
	if (wscmAssertArgumentCountRange(0, 1, __func__)) return;
	if ((Int)r1 == 1)
		objNewInt(random()%*(Num*)pop());
	else
		objNewInt(random());
	DB("SYS <--sysRandom");
}

/* Numerical equivalence. */
void sysEquals (void) {
	DB("SYS -->sysEquals");
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1=pop();  r0=pop();
	r0 = TINTEGER == memObjectType(r0)
	     && TINTEGER == memObjectType(r1)
	     && *(Int*)r0 == *(Int*)r1
	     ? true : false;
	DB("SYS <--sysEquals");
}

void sysEqP (void) {
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(2, __func__)) return;
	r0 = (pop() == pop()) ? true : false;
	DB("<--%s", __func__);
}

void sysStringEqualsP (void) {
 Int len;
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1=pop();  r0=pop();
	r0 = TSTRING == memObjectType(r0)
	     && TSTRING == memObjectType(r1)
	     && memObjectLength(r0) == (len=memObjectLength(r1))
	     && !strncmp (r0, r1, len) ? true : false;
	DB("<--%s", __func__);
}

void sysNotEquals (void) {
 Int a, b;
	DB("SYS -->sysNotEquals");
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1=pop();
	a = *(Int*)r1;
	r2=pop();
	b = *(Int*)r2;
	r0 = (a != b) ? true : false;
	DB("SYS <--sysNotEquals");
}

void sysLessThan (void) {
	DB("SYS -->sysLessThan");
	if (wscmAssertArgumentCount(2, __func__)) return;
	r0 = (*(Int*)pop() > *(Int*)pop()) ? true : false;
	DB("SYS <--sysLessThan");
}

void sysLessEqualThan (void) {
	DB("SYS -->sysLessThan");
	if (wscmAssertArgumentCount(2, __func__)) return;
	r0 = (*(Int*)pop() >= *(Int*)pop()) ? true : false;
	DB("SYS <--sysLessThan");
}

void sysGreaterThan (void) {
	DB("SYS -->sysLessThan");
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1 = pop();
	r0 = pop();
	if (TREAL == memObjectType(r0) && TREAL == memObjectType(r1))
		r0 = (*(Real*)r1 < *(Real*)r0) ? true : false;
	else
		r0 = (*(Int*)r1 < *(Int*)r0) ? true : false;
	DB("SYS <--sysLessThan");
}

void sysGreaterEqualThan (void) {
	DB("SYS -->sysLessThan");
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1 = pop();
	r0 = pop();
	if (TREAL == memObjectType(r0) && TREAL == memObjectType(r1))
		r0 = (*(Real*)r1 <= *(Real*)r0) ? true : false;
	else
		r0 = (*(Int*)r1 <= *(Int*)r0) ? true : false;
	DB("SYS <--sysLessThan");
}

void sysAdd (void) {
 Int sum=0;
	DB("SYS -->sysAdd");
	while (r1--) sum += *(Int*)pop();
	objNewInt(sum);
	DB("SYS <--sysAdd");
}

void sysMul (void) {
 Int product=1;
	DB("SYS -->sysMul");
	while (r1--) product *= *(Int*)pop();
	objNewInt(product);
	DB("SYS <--sysMul");
}

/* Multiply all but the first, then divide the first by that the product.
*/
void sysDiv (void) {
 Int product=1, divisor;
	DB("-->%s", __func__);
	while (1 < (Int)r1--) product *= *(Int*)pop();
	divisor = *(Int*)pop();
	objNewInt(product ? divisor/product : INT_MAX);
	DB("<--%s", __func__);
}

void sysLogAnd (void) {
 Int a, b;
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(2, __func__)) return;
	a = *(Int*)pop();
	b = *(Int*)pop();
	objNewInt(a&b);
	DB("<--%s", __func__);
}

void sysSqrt (void) {
	DB("-->%s", __func__);
	if (wscmAssertArgumentCountRange(1, 1, __func__)) return;
	objNewInt(sqrt((double)*(Int*)pop()));
	DB("<--%s", __func__);
}

void sysRemainder (void) {
 Int a, b;
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(2, __func__)) return;
	b = *(Int*)pop();
	a = *(Int*)pop();
	objNewInt(a%b);
	DB("<--%s", __func__);
}

void sysModulo (void) {
 Int a, b;
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(2, __func__)) return;
	b = *(Int*)pop();
	a = *(Int*)pop();
	/* If one argument is negative. */
	if ((a<0) ^ (b<0))
		objNewInt((b+a%b)%b);
	else
		objNewInt(a%b);
	DB("<--%s", __func__);
}

void sysSub (void) {
 Int sum=0;
	DB("-->%s", __func__);
	if (wscmAssertArgumentCountMin(1, __func__)) return;
	if (1==(Int)r1) objNewInt(-*(Int*)pop());
	else {
		while (--r1) {sum += *(Int*)pop(); }
		sum = *(Int*)pop() - sum;
		objNewInt(sum);
	}
	DB("<--%s", __func__);
}

void sysSleep (void) {
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(1, __func__)) return;
	wscmSleepThread();
	DB("<--%s", __func__);
}

/* Return thread ID. */
void sysTID (void) {
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(0, __func__)) return;
	objNewInt((Int)memVectorObject(car(running), 1));
	DB("<--%s", __func__);
}

void sysUnthread (void) {
	DB("SYS -->sysUnthread <= %d", memVectorObject(car(running), 1));
	/* Remove from thread vector table. */
	memVectorSet(threads, (Int)memVectorObject(car(running), 1), null);
	/* Decrement thread count. */
	memVectorSet(threads, 0, memVectorObject(threads, 0)-1);
	wscmRemoveThread(running);
	wscmSchedule();
	DB("SYS <--sysUnthread");
}

void sysOpenSocket (void) {
	DB("-->%s", __func__);
	if (1 == (Int)r1)
		wscmOpenLocalSocket();
	else if (2 == (Int)r1)
		wscmOpenRemoteSocket();
	else {
		sysList();  r1=r0;
		objNewString((u8*)"Invalid arguments to open-socket:", 33);  push(r0);
		push(r1);
		r1 = (Obj)2;
		sysError();
	}
	DB("<--%s", __func__);
}

void sysOpenStream (void) {
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(1, __func__)
	    || memObjectType(r1=pop()) != TPORT) {
		r0 = "Invalid arguments to open-stream.\n";
		r1 = (Obj)1;
		sysError();
		goto ret;
	}
	if (memVectorObject(r1, 3) == sconnecting)
		wscmOpenRemoteStream();
	else if (memVectorObject(r1, 3) == saccepting)
		wscmOpenLocalStream();
	else if (memVectorObject(r1, 3) == sclosed)
		r0=eof;
	else {
		r0 = "Invalid port state to open-stream.\n";
		r1 = (Obj)1;
		sysError();
	}
 ret:
	DB("<--%s", __func__);
}

/* Create a file object used for file I/O.
	For now all files are opened for reading and writing.
*/
void sysOpen (int oflag, mode_t mode) {
 Chr name[160]={0};
 Int i, filenameLen;
	DB("::%s", __func__);
	if (wscmAssertArgumentCount(1, __func__)) return;

	/* Consider filename */
	r2=pop();
	filenameLen = memObjectLength(r2);

	/* Make sure string is terminated and replace all / with ! */
	assert(0<filenameLen  && filenameLen<160);
	memcpy(name, r2, filenameLen);
	name[filenameLen]=0;
	for (i=0; i<filenameLen; ++i) if (name[i]=='/') name[i]='!';

	DB ("  Filename [%s]", name);
	objNewString(name, filenameLen);
	r2=r0;

	r1 = (Obj)(Int)open((char*)name, oflag, mode);

	if ((Int)r1==-1) {
		fprintf (stderr, "ERROR: sysOpen() Unable to open local file '%s' errno %d %s.", name, errno, strerror(errno));
		r0 = false;
	} else {
		objNewInt(oflag);  r3=r0;
		r4=sopen;
		r5=false; /* TODO this not needed as the pushback char is forced to #f anyways in objNewPort */
		objNewPort();
	}
	DB("  --%s", __func__);
}

void sysOpenFile (void) {
	sysOpen(O_RDWR, S_IRUSR|S_IWUSR);
}

void sysOpenNewFile (void) {
	sysOpen(O_CREAT|O_TRUNC|O_RDWR, S_IRUSR|S_IWUSR);
}

void sysClose(void) {
	DB("::%s", __func__);
	r0 = pop();
	if (memObjectType(r0) != TSOCKET
		 && memObjectType(r0) != TPORT) {
		printf ("WARNING: sysClose: not a socket: ");
		wscmDisplay(r0, 0, 1);
	} else {
		close(*(Int*)r0);
		memVectorSet(r0, 3, sclosed);
	}
	DB("  --%s", __func__);
}

/* Given a byte count and port, read from the port count bytes or if 0 any
   number of bytes.
*/
void sysRecv (void) {
	DB("::%s", __func__);

	r1=pop(); /* Port object in r1 */

	r2=pop(); /* Timeout in r2 */

	/* r3 gets the byte count.  It's then assigned an empty string
	   character buffer with the specified length of characters to
	   read.  A count of zero (null string) implies read any count
	   of bytes. */
	r3=*(Obj*)pop();

	if (r3) {
		memNewArray(TSTRING, (Int)r3);
		r3=r0;
	} else {
		r3 = nullstr;
	}

	if (memObjectType(r1) != TSOCKET
		 && memObjectType(r1) != TPORT) {
		fprintf (stderr, "WARNING: sysRecv: not a socket: ");
		wscmDisplay(r1, 0, 1);
		r0 = eof;
	} else {
		wscmRecvBlock();
	}
	DB("  --%s", __func__);
}

void sysReadChar (void) {
	DB("::%s", __func__);
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1=pop(); /* Port object */
	r2=pop(); /* Timout */
	if (memObjectType(r1) != TSOCKET
	    && memObjectType(r1) != TPORT) {
		printf ("WARNING: sysReadChar: not a socket: ");
		wscmDisplay(r1, 0, 1);
		r0 = eof;
	} else {
		r3=null;  /* tells recv that we just want a character. */
		wscmRecvBlock();
	}
	DB("  --%s", __func__);
}

void sysUnreadChar (void) {
	DB("-->%s()", __func__);
	if (wscmAssertArgumentCount(2, __func__)) return;
	r1=pop(); /* Port. */
	r0=pop(); /* Character. */
	if (memObjectType(r1) != TSOCKET && memObjectType(r1) != TPORT) {
		printf ("WARNING: sysUnreadChar: arg2 not a socket: ");
		wscmDisplay(r1, 0, 1);
		r0 = eof;
	} else if (memObjectType(r0) != TCHAR) {
		printf ("WARNING: sysUnreadChar: arg1 not a char: ");
		wscmDisplay(r0, 0, 1);
		r0 = eof;
	} else
		memVectorSet(r1, 4, r0);
	DB("<--%s()", __func__);
}

/* Call internal C parser on passed string.  Doesn't block so an EOF
   can be returned if the expression is incomplete or empty.
*/
void sysReadString (void) {
	DB("-->%s", __func__);
	if (wscmAssertArgumentCountRange(1, 1, __func__)) return;
	r1=pop(); /* String to parse. */
	yy_scan_bytes(r1, memObjectLength(r1));
	yyparse();
	DB("<--%s()", __func__);
}

/* Given string and port, send string to port.  Thread blocks until all is
   sent.
*/
void sysSend (void) {
	DB("-->%s", __func__);
	if (wscmAssertArgumentCountRange(2, 2, __func__)) return;
	/* r1 gets port object. */
	r1=pop();
	/* r2 gets string object. */
	r2=pop();
	/* Count sent already.  Should be initialized to 0. */
	r3=0;
	if (memObjectType(r1) != TSOCKET && memObjectType(r1) != TPORT) {
		printf ("WARNING: sysSend: not a socket is (type 02x%x): ", memObjectType(r1));
		wscmDisplay(r1, 0, 1);
		*(int*)0=0;
		r0 = eof;
	} else {
		wscmSend();
		if (r0 == false) { /* Still more to send. */
			wscmUnRun();
			wscmMoveToQueue(running, blocked, swriteblocked);
			wscmSchedule();
		}
	}
	DB("<--%s", __func__);
}

void sysSeek (void) {
 Int fd, offset, whence;
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(3, __func__)) return;
	whence = *(Int*)pop(); /* Whence */
	assert(whence==SEEK_SET || whence==SEEK_CUR || whence==SEEK_END);
	offset = *(Int*)pop(); /* Offset */
	fd=*(Int*)pop(); /* Descriptor. */
	lseek(fd, offset, whence);
	DB("<--%s", __func__);
}

void sysIllegalOperator (void) {
 Int i;
	fprintf (stderr, "ERROR: Illegal expression ");
	write(2, "(", 1);
	wscmWrite (r0, stderr);
	i=(Int)r1;
	while (0 < i--) {
		write (2, " ", 1);
		wscmWrite (memStackObject(stack, i), stderr);
	}
	write(2, ")", 1);
	sysError();
}

/* Run time symbol lookup syscall.  If a symbol in r1 found in TGE mutate code
   to just reference the binding's value rather than make this syscall.
*/
void sysTGELookup (void) {
	DB("SYS -->sysTGELookup");
	wscmTGEFind();
	if (r0 == null) {
		fprintf (stderr, "\nERROR: sysTGELookup() Unbound symbol:");
		wscmWrite(r1, stderr);
		fprintf (stderr, ".\n");
		r0 = r1;
		// TODO  Kill thread, stop machine, return to monitor/shell?
	} else {
		DB("SYS    found in tge @ opcode %x", (Int)ip-4);
		/* Specialization optimization.  Muate code that originally called
		   this function into a code that references the binding's value. */
		memVectorSet(code, (Int)ip-4, MVI0);  memVectorSet(code, (Int)ip-3, r0);
		memVectorSet(code, (Int)ip-2, LDI00); memVectorSet(code, (Int)ip-1, 0);
		/* Force virtual machine to run this code. */
		ip -= 4;
	}
	DB("SYS <--sysTGELookup");
}

/* Run time symbol mutate syscall.  If a symbol in r1 found in TGE mutate code
   to just reference the binding's and mutate binding's value with r0.
*/
void sysTGEMutate (void) {
	DB("SYS -->sysTGEMutate");
	r2=r0; /* Since a syscall, save value we're trying to set!. */
	wscmTGEFind();
	if (r0 == null) {
		write (2, "ERROR: sysTGEMutate(): Unbound symbol: ", 37);
		wscmWrite(r1, stderr);
		r0 = r2; /* Return value. */
		// TODO  Kill thread, stop machine, return to monitor/shell?
	} else {
		DB("SYS    found in tge at opcode %0x", (Int)ip-4);
		/* Specialization optimization.  Muate code that originally called
		   this function into a code that references the binding's value. */
		memVectorSet(code, (Int)ip-4, MVI1);  memVectorSet(code, (Int)ip-3, r0);
		memVectorSet(code, (Int)ip-2, STI01); memVectorSet(code, (Int)ip-1, 0);
		r0 = r2; /* Restore value we're trying to set!. */
		/* Force virtual machine to run this code. */
		ip -= 4;
	}
	DB("SYS <--sysTGEMutate");
}

void sysTerminalSize (void) {
 struct winsize win;
	DB("SYS -->sysTerminalSize");
	ioctl(1, TIOCGWINSZ, &win);
	objNewInt(win.ws_col); r1=r0;
	objNewInt(win.ws_row); r2=r0;
	objCons12();
	DB("SYS <--sysTerminalSize");
}

/* Returns character given string and offset.  A negative offset returns
   character offset from the end.  (string-ref "abcde" -2) => #\d
*/
void sysStringRef (void) {
 Int offset;
 Obj o;
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(2, __func__)) return;
	offset = *(Int*)pop();
	o=pop();
	/* A (negative? offset) implies (absolute offset) from end. */
	if (offset < 0) offset = memObjectLength(o)+offset;
	r0 = memVectorObject(characters, *((Chr*)o+offset));
	DB("<--%s", __func__);
}

void sysStringSetB (void) {
 Obj strObj;
 Int offset;
 char ch;
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(3, __func__)) return;
	ch = *(char*)pop();
	offset=*(Int*)pop();
	strObj = pop();
	if (offset < 0) offset = memObjectLength(strObj)+offset;
	memArraySet(strObj, offset, ch);
	DB("<--%s", __func__);
}

void sysVectorLength (void) {
	DB("-->%s", __func__);
	if (wscmAssertArgumentCount(1, __func__)) return;
	r0 = pop();
	objNewInt(r0==nullvec?0:memObjectLength(r0));
	DB("<--%s", __func__);
}

/* Deserializers:  String representation in r5, length r6 => new atom in r0. */
void sysNewSymbol (void) {
	DB("-->sysNewSymbol()");
	objNewSymbol((Str)r5, (Int)r6);
	DB("<--sysNewSymbol()");
}
void sysNewString (void) {
 Int len = parseString(r5); /* Mutates the string & returns the new length. */
	DB("-->sysNewString()");
	if ((Int)r6 == 2) r0 = nullstr;
	else objNewString((u8*)r5, len);
	DB("<--sysNewString()");
}
void sysNewCharacter (void) {
	DB("-->sysNewCharacter()");
	r0 = memVectorObject(characters, ((u8*)r5)[2]);
	DB("<--sysNewCharacter()");
}
void sysNewInteger (void) {
	DB("-->sysNewInteger()");
	*((char*)r5+(Int)r6)=0;
	objNewInt(strtol(r5+(*(char*)r5=='#'?2:0),0,10));
	DB("<--sysNewInteger()");
}
void sysNewBinary (void) {
	DB("-->%s()", __func__);
	objNewInt(strtol(r5+2,0,2));
	DB("<--%s()", __func__);
}
void sysNewReal (void) {
	DB("-->%s()", __func__);
	objNewReal(strtof(r5,0));
	DB("<--%s()", __func__);
}
void sysNewOct (void) {
	DB("-->%s()", __func__);
	objNewInt(strtol(r5+2,0,8));
	DB("<--%s()", __func__);
}
void sysNewHex (void) {
	DB("-->%s()", __func__);
	objNewInt(strtol(r5+2,0,16));
	DB("<--%s()", __func__);
}

/* Compiles s-expression in r0 into code block in r0.  Probably messes up
   a bunch of registers.
	TODO: does this mangle r15/1d/1e retenv/retip/retcode?
*/
extern Num CompError;
void sysCompile (void) {
	DB("-->%s", __func__);
	expr=r0;
	push(env);
	CompError=0;
	asmAsm ( /* Keep track of original expression for debugging. */
		BRA, 8,
		expr,
		END
	);
	if (compExpression(0))
	{
		sysError();
		goto ret;
	}
	asmAsm(
		RET,
		END
	);
	asmNewCode();
	if (wscmDebug) vmDebugDumpCode(r0, stderr); // Dump the code block after compiling code during runtime.
	env=pop();
ret:
	DB("<--%s", __func__);
	DBE wscmWrite (r0, stderr);
}

void sysDebugDumpAll (void) {
	memDebugDumpAll(NULL);
}

void sysDisassemble (void) {
	r0=pop();
	if (memObjectType(r0) == TCLOSURE) r0=car(r0);
	vmDebugDumpCode(r0, stderr);
}

void sysTrace (void) {
	r0 = memVectorObject(code, 2);
}

void sysOpenSemaphore (void) {
 int i=0;
	if (wscmAssertArgumentCount(1, __func__)) return;
	/* Look for next available semaphore. */
	while (memVectorObject(semaphores, i) != null) i++;
	assert(i<MaxSemaphoreCount);
	/* Create new integer object from passed initial count value. */
	objNewInt(*(Num*)pop());
	memVectorSet(semaphores, i, r0);
	/* Return the semaphore index. */
	objNewInt(i);
}

void sysCloseSemaphore (void) {
 Int index;
	if (wscmAssertArgumentCount(1, __func__)) return;
	r0 = memVectorObject(semaphores, index=*(Num*)pop());
	memVectorSet(semaphores, index, null);
}

void sysSemaphoreDown (void) {
 Obj sem, value;
	DB("-->%s ", __func__);
	if (wscmAssertArgumentCountRange(1, 1, __func__)) return;
	/* Store semaphore index in r0. */
	sem = pop();
	value = memVectorObject(semaphores, *(Num*)sem);
	(*(Int*)value)--;
	if (*(Int*)value < 0) {
		/* Block thread on this semaphore.  Store semaphore index in r1. */
		DB ("   %s !!! Blocking thread %d", __func__, memVectorObject(car(running), 1));
		r1 = (Obj)*(Num*)sem;
		wscmUnRun();
		wscmMoveToQueue(running, blocked, ssemaphore);
		wscmSchedule();
	} else {
		objNewInt(*(Int*)value);
	}
	DB("<--%s ", __func__);
}

void sysSemaphoreUp (void) {
 Obj sem, value;
	DB("-->%s\n", __func__);
	if (wscmAssertArgumentCountRange(1, 1, __func__)) return;
	sem = pop();
	value = memVectorObject(semaphores, *(Num*)sem);
	(*(Int*)value)++;
	if (*(Int*)value < 1) {
		/* Unblock a semaphore blocked thread. */
		wscmScheduleSemaphoreBlocked(*(Num*)sem);
	}
	DB("<--%s\n", __func__);
}

void sysTime (void) {
 struct timeval tv;
	DB("-->%s\n", __func__);
	if (wscmAssertArgumentCountRange(0, 0, __func__)) return;
	gettimeofday(&tv, NULL);
	objNewInt(tv.tv_sec);
	DB("<--%s\n", __func__);
}

void sysUTime (void) {
 struct timeval tv;
	DB("-->%s\n", __func__);
	if (wscmAssertArgumentCountRange(0, 0, __func__)) return;
	gettimeofday(&tv, NULL);
	objNewInt(tv.tv_sec*1000 + tv.tv_usec/1000);
	DB("<--%s\n", __func__);
}

#define MAX_SIGNAL_VALUE 32
Int caughtSignals[MAX_SIGNAL_VALUE]={0};

/* This function passed to the virtual machine module during initialization
   and is called periodically to reschedule a new thread.
*/
void sysSchedule (void) {
 Int i=0;
	DB("-->%s", __func__);

	for (; i < MAX_SIGNAL_VALUE; ++i)
		/* Spawn new signal handler thread.  Function exist in global signal-handler vector.*/
		if (caughtSignals[i]) {
			caughtSignals[i]=0;
			push(r0); /* Save state */
			push(r1);
			push(r2);
				r1=signalhandlers;  wscmTGEFind(); r0=car(r0); /* Consider vector of signalhandlers. */
				r1 = memVectorObject(r0, i);
				r0 = car(r1);
				/* Hack:  replace inital opcode with NOP since closure obj
				   code expects the closure to be passed in via r0 as well.
				   TODO: BF: IS THIS STILL REQUIRD? */
				memVectorSet(r0, 3, NOP);
				memVectorSet(r0, 4, NOP);
				wscmNewThread();
			r2=pop();
			r1=pop();
			r0=pop();
		}

	/* If just a single thread in existence, leave it alone and just continue on. */
	if ((Int)memVectorObject(threads, 0) != 1 || !wscmIsQueueEmpty(sleeping)) {
		wscmUnRun();
		/* BF: TODO: This thread's stack is still active.  It MIGHT get clobbered
		   while a new thread is scheduled, wakes up or unblocked.*/
		wscmSchedule();
	}

	DB("<--%s", __func__);
}

void catchSignal (int sig) {
	assert(0 < sig && sig < MAX_SIGNAL_VALUE);
	caughtSignals[sig]=1;
	interrupt=1; /* Let virtual machine know an interrupt occured. */
}

void sysSignal (void) {
/*
	mem_vec_set(signals, sig, r2);
*/
	DB("-->%s\n", __func__);
	if (wscmAssertArgumentCount(1, __func__)) return;
	signal((int)(*(Num*)pop()), catchSignal);
	DB("<--%s\n", __func__);
}

void sysToggleDebug (void) {
/*
	mem_vec_set(signals, sig, r2);
*/
	wscmDebug ^= 1;
	r0 = wscmDebug ? true : false;
}



/*******************************************************************************
 Initialization stuff.
*******************************************************************************/

/* Bind 'symbol in global environment and assign a syscall object
   created from 'function.
*/
void wscmDefineSyscall (Func function, char* symbol) {
	DB("-->%s", __func__);
	/* Create binding: (val . sym) */
	objNewSyscall(function); r1=r0;
	objNewSymbol((Str)symbol, strlen(symbol)); r2=r0;
	objCons12();
	/* Insert binding into TGE list:  (TGE (newVal . newSym) ...) */
	r1=r0;
	r2=cdr(tge);
	objCons12();
	memVectorSet(tge, 1, r0);

	memObjStringRegister(function, symbol); /* Register internal pointer with memory module. */

	DB("<--%s => ", __func__);
	DBE wscmWrite (r1, stderr);
}

/* Bind symbol, created from 'sym', in TGE and assign object in r0 to the location.
	Mutates: r1 r2
	Returns: r0
*/
void wscmDefine (char* sym) {
	DB("-->%s", __func__);
	r1 = r0;
	objNewSymbol((Str)sym, strlen(sym)); r2=r0;
	objCons12();
	/* Insert binding into TGE list:  (TGE (val . sym) . ...) */
	r1=r0;  r2=cdr(tge);  objCons12();
	memVectorSet(tge, 1, r0);
	DB("<--%s => ", __func__);
	DBE wscmWrite (r1, stderr);
}

/* Given  - r1:port object  r3:pointer to char
            r4:current state (see scanner.c)   r5:yytext  r6:yylen
   Return - r4:next state   r5:yytext   r6:yylen
            r0:final state if complete token scanned (reached final state).
*/
void sysTransition (void) {
	DB(INDENT0"::"STR" *r3="HEX" state:r4="HEX, __func__, *(Chr*)r3, r4);
	//DBE wscmWrite (r3, stderr);
	//DBE wscmWrite (r4, stderr);
	/* Make the transition on char in r3 given state in r4. */
	r4 = (Obj)transition(*(Num*)r3, (Num)r4);
	DB(INDENT1"transition to state => "HEX, r4);
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
	DB(INDENT1"--"STR" final state r0="HEX, __func__, r0);
	//DBE wscmWrite(r4, stderr);
	//DBE wscmWrite(r5, stderr);
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
	DB("::%s\n", __func__);
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
		PUSH7, PUSH1D, PUSH1E, /* Recurse on this fn (parser) with isList set. */
		MVI7, 1l,
		MV01C,
		JAL0,
		POP1E, POP1D, POP7,
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
		PUSH7, PUSH1D, PUSH1E, /* Recursive call with isList set */
		MVI7, 1l,
		MV01C,
		JAL0,
		POP1E, POP1D, POP7,
		PUSH1, /* Save r1 since objListToVector requires r1. */
		MV10,
		SYSI, objListToVector,
		POP1,  /* Restore r1. */
		BRA, ADDR, "done",

	LABEL, "quote",
		/* quote? */
		BNEI0, SQUOTE, ADDR, "unquotesplicing",
		PUSH7, PUSH1D, PUSH1E,
		MVI7, 0l,
		MV01C,
		JAL0,
		POP1E, POP1D, POP7,
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
		PUSH7, PUSH1D, PUSH1E,
		MVI7, 0l,
		MV01C,
		JAL0,
		POP1E, POP1D, POP7,
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
		PUSH7, PUSH1D, PUSH1E,
		MVI7, 0l,
		MV01C,
		JAL0,
		POP1E, POP1D, POP7,
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
		PUSH7, PUSH1D, PUSH1E,
		MVI7, 0l,
		MV01C,
		JAL0,
		POP1E, POP1D, POP7,
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
		PUSH7, PUSH1D, PUSH1E, /* Recursive call with isList set */
		MVI7, 1l,
		MV01C,
		JAL0,
		POP1E, POP1D, POP7,
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
		PUSH1D, PUSH1E, /* Recurse. */
		MV01C, /* r0 <- code register*/
		JAL0,
		POP1E, POP1D,
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
	                  "--------------------------------------------------------------------------------", 640);
	r2=r0;
	objNewString((Str)"ParserMain",10); /* Label */
	asmAsm (
		BRA, 8l,
		r0, /* Label */
		MVI1, r2, /* The string buffer. */
		SYSI, objCopyString,
		MV50,
		/* Initialize boolean to 'not parsing a list'. */
		MVI7, 0l,
		/* r1 gets port object from the required parameter to read.  (read portObject). */
		POP1,
		/* Call parser. */
		MVI0, r1,  /* Insert code block object directly via r1 from above. */
		PUSH1D, PUSH1E, PUSH15,
		JAL0,
		POP15, POP1E, POP1D,
		RET,
		END
	);
	asmCompileAsmstack(0);
	asmNewCode();  r1=r0;
	objNewClosure1Env();
	DB("  --%s\n", __func__);
}

/* Create and run a read-eval-print-loop thread in machine language.
*/
void wscmCreateRepl (void) {
	DB("-->%s\n", __func__);
	objNewSymbol ((Str)"\nVM>", 4);  r2=r0;
	objNewSymbol ((Str)"stdin", 5);  r1=r0;  wscmTGEFind(); r3=car(r0); /* The binding value is the car */
	objNewSymbol ((Str)"read", 4);  r1=r0;  wscmTGEFind();  r4=caar(r0); /* The binding value is a closure so we need to car again for the code object. */
	objNewString ((Str)"bye\n", 4);  r5=r0;
	objNewString ((Str)"Entering REPL\n", 14);  r6=r0;
	asmAsm (
		MVI0, r6,
		PUSH0,
		MVI1, 1l,
		SYSI, sysDisplay,
		/* Display prompt. */
		LABEL, "repl",
		MVI0, r2, // Prompt
		PUSH0,
		MVI1, 1l,
		SYSI, sysDisplay,
		/* Call read. */
		PUSH1D, PUSH1E, PUSH15,
		MVI0, r3, // in
		PUSH0,
		MVI0, r4, // read
		JAL0,
		POP15, POP1E, POP1D,
		/* Done if an #eof parsed. */
		BRTI0, TEOF, ADDR, "done",
		/* Compile expression. */
		SYSI, sysCompile,
		/* Run code. */
		PUSH1D, PUSH1E, PUSH15,
		JAL0,
		POP15, POP1E, POP1D,
		/* (display ...) */
		PUSH0,
		MVI1, 1l,
		SYSI, sysDisplay,
		BRA, ADDR, "repl",
		LABEL, "done",
		MVI0, r5, /* Bye message. */
		PUSH0,
		MVI1, 1l,
		SYSI, sysDisplay,
		RET,
		SYSI, sysUnthread,
		END
	);
	asmCompileAsmstack(0);
	asmNewCode(); r1=r0;
	objNewClosure1Env();
	DB("<--%s\n", __func__);
}

void wscmInitialize (void) {
 Int i;
	DB("::%s", __func__);

	/* Register objects and pointer addresses with their
	   C source names for object debug dumps. */
	memObjStringSet(sysIllegalOperator);
	memObjStringSet(sysTGELookup);
	memObjStringSet(sysTGEMutate);
	memObjStringSet(objNewClosure1Env);
	memObjStringSet(objNewVector1);
	memObjStringSet(wscmRecvBlock);
	memObjStringSet(sysTransition);
	memObjStringSet(objListToVector);
	memObjStringSet(objCons12);
	memObjStringSet(objCopyString);
	memObjStringSet(sysCompile);
	memObjStringSet(objCons23);
	memObjStringSet(compVerifyVectorRef);
	memObjStringSet(compVerifyVectorSetB);
	memObjStringSet(wscmNewThread);
	memObjStringSet(objCopyInteger);
	memObjStringSet(sysCreateContinuation);
	memObjStringSet(sysReinstateContinuation);
	memObjStringSet(wscmEnvGet);
	memObjStringSet("Not enough arguments to closure");
 
	objInitialize(sysSchedule);

	/* Create empty thread vector.  All active threads are assigned a number
	   1-1024 and stored here for easy constant time lookup.  The first entry
	   in the thread table is the thread count as an immediate number. */
	objNewVector(MAX_THREADS+1);  threads=r0;
	memVectorSet(threads, 0, 0); /* Initialize thread count. */
	for (i=1; i<=MAX_THREADS; i++) memVectorSet(threads, i, null);

	/* Create empty ready thread doubly linked list. */
	objNewVector(3);  running=ready=r0;
	memVectorSet(ready, 0, sready);
	memVectorSet(ready, 1, ready);
	memVectorSet(ready, 2, ready);

	/* Create empty sleeping threads doubly linked list. */
	objNewVector(3);  sleeping=r0;
	memVectorSet(sleeping, 0, ssleeping);
	memVectorSet(sleeping, 1, sleeping);
	memVectorSet(sleeping, 2, sleeping);

	/* Create empty sleeping threads doubly linked list. */
	objNewVector(3);  blocked=r0;
	memVectorSet(blocked, 0, sblocked);
	memVectorSet(blocked, 1, blocked);
	memVectorSet(blocked, 2, blocked);

	/* Create blocked on descriptor vector table.  For now a list later a vector
	   relying on asynchronous interrupts to identify ready descriptor. */
	//objNewVector(1024);  blocked=r0;

	/* Create semaphore counters. */
	i=MaxSemaphoreCount;
	objNewVector(i);  semaphores=r0;
	while (i--) memVectorSet(semaphores, i, null);

	/* Create empty global environment list. */
	objNewSymbol((Str)"TGE", 3);
	r1=r0;  r2=null;  objCons12();  env=tge=r0;

	/* Bind usefull values r2=value r1=symbol. */
	wscmDefineSyscall (sysError, "error");
	wscmDefineSyscall (sysQuit, "quit");
	wscmDefineSyscall (sysFun, "fun");
	wscmDefineSyscall (sysDumpEnv, "env");
	wscmDefineSyscall (wscmDumpTGE, "tge");
	wscmDefineSyscall (sysDebugger, "debugger");
	wscmDefineSyscall (sysDumpThreads, "threads");
	wscmDefineSyscall (sysString, "string");
	wscmDefineSyscall (sysMakeString, "make-string");
	wscmDefineSyscall (sysSubString, "substring");
	wscmDefineSyscall (sysStringLength, "string-length");
	wscmDefineSyscall (sysSerializeDisplay, "serialize-display");
	wscmDefineSyscall (sysSerializeWrite, "serialize-write");
	wscmDefineSyscall (sysNumber2String, "number->string");
	wscmDefineSyscall (sysWrite, "write");
	wscmDefineSyscall (sysDisplay, "display");
	wscmDefineSyscall (sysVector, "vector");
	wscmDefineSyscall (sysMakeVector, "make-vector");
	wscmDefineSyscall (sysRandom, "random");
	wscmDefineSyscall (sysEquals, "=");
	wscmDefineSyscall (sysEqP, "eq?");
	wscmDefineSyscall (sysStringEqualsP, "string=?");
	wscmDefineSyscall (sysNotEquals, "!=");
	wscmDefineSyscall (sysLessThan, "<");
	wscmDefineSyscall (sysLessEqualThan, "<=");
	wscmDefineSyscall (sysGreaterThan, ">");
	wscmDefineSyscall (sysGreaterEqualThan, ">=");
	wscmDefineSyscall (sysAdd, "+");
	wscmDefineSyscall (sysMul, "*");
	wscmDefineSyscall (sysDiv, "/");
	wscmDefineSyscall (sysLogAnd, "logand");
	wscmDefineSyscall (sysSqrt, "sqrt");
	wscmDefineSyscall (sysRemainder, "remainder");
	wscmDefineSyscall (sysModulo, "modulo");
	wscmDefineSyscall (sysSub, "-");
	wscmDefineSyscall (sysTime, "time");
	wscmDefineSyscall (sysUTime, "utime");
	wscmDefineSyscall (sysSleep, "sleep");
	wscmDefineSyscall (sysTID, "tid");
	wscmDefineSyscall (sysUnthread, "unthread");
	wscmDefineSyscall (sysOpenSocket, "open-socket");
	wscmDefineSyscall (sysOpenStream, "open-stream");
	wscmDefineSyscall (sysOpenFile, "open-file");
	wscmDefineSyscall (sysOpenNewFile, "open-new-file");
	wscmDefineSyscall (sysClose, "close");
	wscmDefineSyscall (sysRecv, "recv");
	wscmDefineSyscall (sysReadChar, "read-char");
	wscmDefineSyscall (sysUnreadChar, "unread-char");
	wscmDefineSyscall (sysReadString, "read-string");
	wscmDefineSyscall (sysSend, "send");
	wscmDefineSyscall (sysSeek, "seek");
	wscmDefineSyscall (sysTerminalSize, "terminal-size");
	wscmDefineSyscall (sysStringRef, "string-ref");
	wscmDefineSyscall (sysStringSetB, "string-set!");
	wscmDefineSyscall (sysVectorLength, "vector-length");
	wscmDefineSyscall (sysDebugDumpAll, "dump-heap");
	wscmDefineSyscall (memGarbageCollect, "garbage-collect");
	wscmDefineSyscall (sysDisassemble, "disassemble");
	wscmDefineSyscall (sysTrace, "trace");
	wscmDefineSyscall (sysOpenSemaphore, "open-semaphore");
	wscmDefineSyscall (sysCloseSemaphore, "close-semaphore");
	wscmDefineSyscall (sysSemaphoreDown, "semaphore-down");
	wscmDefineSyscall (sysSemaphoreUp, "semaphore-up");
	wscmDefineSyscall (sysSignal, "signal");
	wscmDefineSyscall (sysToggleDebug, "toggle-debug");

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
	wscmCreateRead();  wscmDefine("read");
	wscmCreateRepl();  wscmDefine("repl2");
	r0=semaphores;  wscmDefine("semaphores");
	r0=eof; wscmDefine("#eof");
	objNewInt(42); wscmDefine ("y");
	objNewInt(1); wscmDefine ("x");

	/* Signal handler vector */
	i=32;
	objNewVector(i);
	while (i--) { memVectorSet(r0, i, null); }
	wscmDefine("SIGNALHANDLERS");

	DB("  --%s", __func__);
}

void sysDumpCallStackCode (void) {
 Int i;
 Obj o, l=NULL;

	sysDumpEnv(r16);
	sysDumpEnv(r15);
	for (i=0; i<memStackLength(stack); ++i) {
		o = memStackObject(stack, i);
		printf ("Stack "INT" "HEX016" "OBJ NL, i, memIsObjectValid(o)?memObjectDescriptor(o):0, o);
	}

	for (i=0; i<memStackLength(stack); ++i) {
		o = memStackObject(stack, i);
		if (memIsObjectValid(o)) {
			if (memObjectType(o)==TCODE) {
				printf (NL "Stack "INT" "HEX016" "OBJ NL, i, memObjectDescriptor(o), o);
				wscmWrite(memVectorObject(o, 2), stdout);
				if (l!=NULL) sysDumpEnv(l);
				else printf (NL);
			}
		}
		l = o;
	}
}

void sysDebugger (void) {
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
		printf ("\ns    wscmWrite(stack, stderr)");
		printf ("\ny    wscmWrite(symbols, stderr)");
		printf ("\nh    memDebugDumpHeapHeaders(stderr)"); 
		printf ("\n3    memDebugDumpYoungHeap(stderr)"); 
		printf ("\nw a  wscmWrite(a1, stderr)");
		printf ("\nR    return");
		printf ("\nX    crash");
		printf ("\nQ    exit(-1)");
		printf ("\n-------------------------------\e[0m");
		printf ("\nwscmdbg>");
		while (strchr("\r\n \t", cmd=getchar())); /* Skip whitespace */
		if (cmd=='c') vmDebugDumpCode(code, stderr);
	   if (cmd=='C') {
			Obj arg;
			scanf("%lx", &arg);
			vmDebugDumpCode(arg, stderr);
		}
		if (cmd=='u') sysDumpCallStackCode();
		if (cmd=='e') sysDumpEnv(env);
		if (cmd=='s') wscmWrite(stack, stderr);
		if (cmd=='y') wscmWrite(symbols, stderr);
	   if (cmd=='h') memDebugDumpHeapHeaders(stderr);
	   if (cmd=='3') memDebugDumpYoungHeap (stderr);
	   if (cmd=='w') {
			Obj arg;
			scanf("%lx", &arg);
			if (memObjString(arg)) printf (memObjString(arg));
			else wscmWrite(arg, stderr);
		}
		if (cmd=='R') done=1;
		if (cmd=='X') *(Int*)0=0;
		if (cmd=='Q') exit(-1);
	}

	/* Restore terminal and IO */
	tcsetattr(1, TCSANOW, &tios_orig);
	fcntl (0, F_SETFL, fl);
}

#undef DB_MODULE
