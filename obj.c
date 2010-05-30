#define DEBUG 0
#define DB_MODULE "OBJ "
#include "debug.h"

#include <stdio.h>
#include <unistd.h>
#include <string.h> /* memcpy */

#include "obj.h"



/* Static symbol objects. */
Obj null, nullvec, nullstr, false, true, eof,
    srem, srunning, sready, ssleeping, sblocked, sdead, ssemaphore,
    sopenblocked, sreadblocked, swriteblocked,
    saccepting, sconnecting, sopen, sclosed,
    sdefine, slambda, squote, sunquote, squasiquote, sbegin, sunquotesplicing,
    sif, sor, sand, ssetb,
    svectorref, svectorvectorref, svectorvectorsetb, svectorsetb, svectorlength,
    scons, scar, scdr, ssetcarb, ssetcdrb,
    snullp,
    spairp, svectorp, sstringp, sportp, sappend, seofobjectp,
    sthread, slet, sletrec,
    seval, sapply, scallcc, ssyntaxrules, seof,
    snot, sadd, ssub, smul, sdiv, slogand, characters, signalhandlers;

int wscmDebug=0;


/* This is a very popular hashing algorithm found online and in various texts.
   My tweak is to feed the last char back into the algorithm which seems to
   help distribute more single-length symbols.
*/
unsigned hashpjw (Str s, int len) {
 unsigned ret=0, mask;
	while (len--) {
		if ((mask=(ret=(ret<<4)+*s++)&0xf<<28)) ret^=(mask|mask>>24);
	}
	if ((mask=(ret=(ret<<4)+*--s)&0xf<<28)) ret^=(mask|mask>>24);
	return ret;
}

#if 0
void tree_copy (void) { /* COPIES TREE ACC INTO ACC */
	if (is_pair(acc)) {
		push(cdr(acc)); /* SAVE CDR STATE */
		acc=car(acc);   /* RECURSE ON CAR */
		tree_copy();
		r1=acc;         /* RESTORE CDR, STORE CAR */
		acc=pop();
		push(r1);
		tree_copy();    /* RECURSE ON CDR */
		push(acc);      /* FORM NEW PAIR */
		new_pair();
	}
}
#endif

int objListLength (Obj o) {
 int i=0;
	while (memObjectType(o) == TPAIR) {
		o = cdr(o);
		i++;
	}
	return i;
}

int objDoublyLinkedListLength (Obj o) {
 Obj next=cdr(o);
 int i=0;
	while (next!=o) {next=cdr(next); i++; }
	return i;
}

/* Creates vector in r0 from list in r1.
*/
void objListToVector (void) {
 int i=0, len;
	r1=r0;
	len = objListLength(r0);
	if (len) {
		memNewVector(TVECTOR, len); /* Create empty vector */
		while (i<len) { /* Stuff vector*/
			memVectorSet(r0, i++, car(r1));
			r1 = cdr(r1);
		}
	} else
		r0 = nullvec;
}

/* Creates a new integer object in r0.
*/
void objNewInt  (Int i) {
   memNewArray(TINTEGER, sizeof(Int));
   *(Int*)r0 = i;
}
/* Create and set object in r0 to immediate signed integer value in r1.
*/
void objCopyInteger (void) {
   memNewArray(TINTEGER, sizeof(Int));
   *(Int*)r0 = *(Int*)r1;
}

/* Creates a new real object in r0.
*/
void objNewReal (Real x) {
   memNewArray(TREAL, sizeof(Real));
   *(Real*)r0 = x;
}
/* Create and set object in r0 to immediate real value in r1.
*/
void objCopyReal (void) {
   memNewArray(TREAL, sizeof(r32));
   *(r32*)r0 = *(r32*)r1;
}

/* Create new string copying len bytes from str to object in r0.
*/
void objNewString (Str str, int len) {
   memNewArray(TSTRING, len);
   if (str) memcpy(r0, str, len);
}

/* Create new string based on the string object in r1.
*/
void objCopyString (void) {
 int len;
   memNewArray(TSTRING, len=memObjectLength(r1));
	memcpy(r0, r1, len);
}

void objNewSymbol (Str str, int len) {
 static unsigned hash, i;
	i = hash = hashpjw(str, len) % 2029;
	do {
		r0 = memVectorObject(symbols, i);
		/* Bucket empty so insert into symbol table. */
		if (r0 == null) {
			memNewArray(TSYMBOL, len);
			memcpy(r0, str, len);
			memVectorSet(symbols, i, r0);
			return;
		}
		/* If something here, check if it's the symbol */
		if(memObjectLength(r0)==len && !memcmp(r0,str,len)) {
			return;
		}
		/* Otherwise continue linear sweep for empty bucket or symbol. */
	} while ( (i=(++i==2029)?0:i) != hash);
	printf ("WARNING!!!  Symbol table full!!!!\n");
	memNewArray(TSYMBOL, len);
	memcpy((char*)r0, str, len);
}

void objNewSymbolStatic (char *s) {
 Num len = strlen(s);
 static unsigned hash, i;
	i = hash = hashpjw((Str)s, len) % 2029;
	do {
		r0 = memVectorObject(symbols, i);
		/* Bucket empty so insert into symbol table. */
		if (r0 == null) {
			memNewStatic(TSYMBOL, len); /* r0 now holds new symbol */
			memcpy(r0, s, len);
			memVectorSet(symbols, i, r0);
			return;
		}
		/* If something here, check if it's the symbol */
		if(memObjectLength(r0)==len && !memcmp(r0,s,len)) {
			return;
		}
		/* Otherwise continue linear sweep for empty bucket or symbol. */
	} while ( (i=(++i==2029)?0:i) != hash);
	printf ("WARNING!!!  Symbol table full!!!!\n");
	memNewStatic(TSYMBOL, len);
	memcpy((char*)r0, s, len);
}

void objNewSyscall (Func f) {
   memNewArray(TSYSCALL, 1);
	memVectorSet(r0, 0, f);
}

void objCons12 (void) {
   memNewVector(TPAIR, 2);
	memVectorSet(r0, 0, r1);
	memVectorSet(r0, 1, r2);
}
void objCons23 (void) {
   memNewVector(TPAIR, 2);
	memVectorSet(r0, 0, r2);
	memVectorSet(r0, 1, r3);
}

/* Create UNITIALIZED vector in r0 of length r1:immediate
*/
void objNewVector (int len) {
   memNewVector(TVECTOR, len);
}
void objNewVector1 (void) {
   memNewVector(TVECTOR, (LengthType)r1);
}

/* Create new closure in r0 which is (<code> . <environment>)
   Code pased in r1.
*/
void objNewClosure1Env (void) {
   memNewVector(TCLOSURE, 2);
	memVectorSet(r0, 0, r1);
	memVectorSet(r0, 1, env);
}

#if 0
void objNewSocket (void) {
	memNewVector(TSOCKET, 5);
	memVectorSet(r0, 0, r1); /* Descriptor. */
	memVectorSet(r0, 1, r2); /* Internet address string. */
	memVectorSet(r0, 2, r3); /* Port number. */
	memVectorSet(r0, 3, r4); /* State: accepting, connecting, open, closed*/
	memVectorSet(r0, 4, false);  /* Push back or next available character. */
}
#endif

void objNewPort (void) {
	memNewVector(TPORT, 5);
	memVectorSet(r0, 0, r1); /* Descriptor. */
	memVectorSet(r0, 1, r2); /* Path. */
	memVectorSet(r0, 2, r3); /* Flags. */
	memVectorSet(r0, 3, r4); /* State: open, closed. */
	memVectorSet(r0, 4, false); /* Push back or next available character. */
}

/*
Obj objPortDescriptor (Obj o) { return memVectorObject(o, 0); }
Obj objPortPath       (Obj o) { return memVectorObject(o, 1); }
Obj objPortFlags      (Obj o) { return memVectorObject(o, 2); }
Obj objPortState      (Obj o) { return memVectorObject(o, 3); }
Obj objPortPushback   (Obj o) { return memVectorObject(o, 4); }
*/

#if 0
void new_continuation(void) {
 U32 length = mem_stk_count(stack);
   mem_new_vec(TCONTINUATION, length);
   memcpy(acc, stack+sizeof(OBJ), length*sizeof(OBJ)); /* IS THIS NAUGHTY? */
}

#endif


void objDump (Obj a, int fd) {
 Int len, i;
 static char buff[128];
 char *c;
	if ((Num)a < 0x100000) {
		len = sprintf(buff, "#<%x>", a);
		write(fd, buff, len);
		return;
	}
	switch (memObjectType(a)) {
		case TNULL   :
		case TNULLSTR:
		case TNULLVEC:
		case TFALSE  :
		case TTRUE   :
		case TEOF    :
		case TSYMBOL :
			write (fd, a, memObjectLength(a));
			break;
		case TINTEGER:
			len = sprintf(buff, "%d", *(u32*)a);
			write(fd, buff, len);
			break;
		case TSTRING : 
			write (fd, "\"", 1);
			c = a;
			for (i=memObjectLength(a); i; i--) {
				switch (*c) {
					case '\\' : write(fd, "\\\\", 2); break;
					case '\"' : write(fd, "\\\"", 2); break;
					case '\a' : write(fd, "\\a", 2); break;
					case '\e' : write(fd, "\\e", 2); break;
					case '\233' : write(fd, "\\c", 2); break;
					case '\n' : write(fd, "\\n", 2); break;
					case '\r' : write(fd, "\\r", 2); break;
					case '\t' : write(fd, "\\t", 2); break;
					default   : write(fd, c, 1);
				}
				c++;
			}
			write (fd, "\"", 1);
			break;
		case TCLOSURE :
			if (cdr(a) == tge)
				len = sprintf(buff, "#CLOSURE<CODE:"OBJ" TGE:"OBJ">", car(a), cdr(a));
			else
				len = sprintf(buff, "#CLOSURE<CODE:"OBJ" ENV:"OBJ">", car(a), cdr(a));
			write(fd, buff, len);
			break;
		case TPAIR :
			write (fd, "(", 1);
			objDump(memVectorObject(a, 0), fd);
			write (fd, " . ", 3);
			objDump(memVectorObject(a, 1), fd);
			write (fd, ")", 1);
			break;
		case TVECTOR :
			write (fd, "#(", 2);
			for (i=0; i<memObjectLength(a); i++) {
				if (i) write (fd, " ", 1);
				len = sprintf(buff, OBJ, *((Obj*)a+i));
				write(fd, buff, len);
			}
			write (fd, ")", 1);
			break;
		case TCODE :
			len = sprintf(buff, "#<CODE "OBJ">", a);
			write(fd, buff, len);
			break;
		case TSTACK :
			len = sprintf(buff, "#<STACK "OBJ">", a);
			write(fd, buff, len);
			break;
		case TSYSCALL :
			len = sprintf(buff, "#<SYSCALL "OBJ">", *(Obj*)a);
			write(fd, buff, len);
			break;
		default :
			if (tge == a) {
				len = sprintf(buff, "#<TGE "OBJ">", a);
				write(fd, buff, len);
			} else {
				len = sprintf(buff, OBJ, a);
				write(fd, buff, len);
			}
			/* Dump the object description. */
			c = memObjString(a);
			if (c) {
				write (1, ":", 1);
				write (1, c, strlen(c));
			}
	}
}

/* caar <=> (car (car x)) <=>
    LDI0 #<binding x>
    BRT  TPAIR a
    ; error code here
    a: LD00 hmmmm LD needs an offset value
*/
Obj  car  (Obj o) { return memVectorObject(o, 0);}
Obj  caar (Obj o) { return car(car(o));}
Obj  cdar (Obj o) { return cdr(car(o));}

Obj  cdr  (Obj o) { return memVectorObject(o, 1);}
Obj  cadr (Obj o) { return car(cdr(o));}
Obj  cddr (Obj o) { return cdr(cdr(o));}

void push (Obj o) { memStackPush(stack, o);}
Obj  pop  (void)  { return memStackPop(stack);}


Func objCallerPreGarbageCollect = 0,
   objCallerPostGarbageCollect = 0;

int objGCCounter=0;

void objGCPre (void) {
	if (objGCCounter++ == 0)
		if (objCallerPreGarbageCollect) objCallerPreGarbageCollect ();
}

void objGCPost (void) {
	if (objGCCounter-- == 1)
		if (objCallerPostGarbageCollect) objCallerPostGarbageCollect ();
}

//extern void wscmDisplay (Obj a, long islist, int fd);
void objObjectDumper (Obj o) {
	objDump (o, 1); // This modules version of write.
	//wscmDisplay (o, 0, 1); // WSCMs version of display.
}
 

void objInitialize (Func intHandler) {
 Int i;
	DB("::%s", __func_);
	DB("  initializing memory module");
	asmInitialize(intHandler, objGCPre, objGCPost, objObjectDumper);
	memInitialize(0, 0);
	/* These primitive types are also external (display) strings. */
	memNewStatic(TNULL, 2);    null=r0;    memcpy(r0, "()", 2);
	/* This is a strange object with a descriptor and no content.
	   Since little endian a valid poitner to empty C string.  */
	memNewStatic(TNULLSTR, 0); nullstr=r0;
	memNewStatic(TNULLVEC, 3); nullvec=r0; memcpy(r0, "#()", 3);
	memNewStatic(TFALSE, 2);   false=r0;   memcpy(r0, "#f", 2);
	memNewStatic(TTRUE, 2);    true=r0;    memcpy(r0, "#t", 2);

	memNewVector(TVECTOR, 2029);        symbols = r0; /* Symbol table */
	for (i=0; i<2029; i++) memVectorSet (symbols, i, null);
	objNewSymbolStatic("define");       sdefine = r0;
	objNewSymbolStatic("lambda");       slambda = r0;
	objNewSymbolStatic("quote");        squote = r0;
	objNewSymbolStatic("unquote");      sunquote = r0;
	objNewSymbolStatic("quasiquote");   squasiquote = r0;
	objNewSymbolStatic("unquote-splicing"); sunquotesplicing = r0;
	objNewSymbolStatic("begin");        sbegin = r0;
	objNewSymbolStatic("if");           sif = r0;
	objNewSymbolStatic("or");           sor = r0;
	objNewSymbolStatic("and");          sand = r0;
	objNewSymbolStatic("set!");         ssetb = r0;
	objNewSymbolStatic("vector-ref");   svectorref= r0;
	objNewSymbolStatic("vector-vector-ref"); svectorvectorref= r0;
	objNewSymbolStatic("vector-set!");  svectorsetb= r0;
	objNewSymbolStatic("vector-vector-set!");  svectorvectorsetb= r0;
	objNewSymbolStatic("vector-length");svectorlength= r0;
	objNewSymbolStatic("cons");         scons= r0;
	objNewSymbolStatic("car");          scar= r0;
	objNewSymbolStatic("cdr");          scdr= r0;
	objNewSymbolStatic("set-car!");     ssetcarb= r0;
	objNewSymbolStatic("set-cdr!");     ssetcdrb= r0;
	objNewSymbolStatic("null?");        snullp= r0;
	objNewSymbolStatic("pair?");        spairp= r0;
	objNewSymbolStatic("vector?");      svectorp= r0;
	objNewSymbolStatic("string?");      sstringp= r0;
	objNewSymbolStatic("port?");        sportp= r0;
	objNewSymbolStatic("append");       sappend= r0;
	objNewSymbolStatic("eof-object?");  seofobjectp= r0;
	objNewSymbolStatic("thread");       sthread= r0;
	objNewSymbolStatic("let");          slet= r0;
	objNewSymbolStatic("letrec");       sletrec= r0;
	objNewSymbolStatic("eval");         seval= r0;
	objNewSymbolStatic("apply");        sapply= r0;
	objNewSymbolStatic("call/cc");      scallcc= r0;
	objNewSymbolStatic("syntax-rules"); ssyntaxrules= r0;
	objNewSymbolStatic("#eof");         seof= r0;
	objNewSymbolStatic("not");          snot = r0;
	objNewSymbolStatic("+");            sadd = r0;
	objNewSymbolStatic("-");            ssub = r0;
	objNewSymbolStatic("*");            smul = r0;
	objNewSymbolStatic("/");            sdiv = r0;
	objNewSymbolStatic("logand");       slogand = r0;
	objNewSymbolStatic("rem");          srem = r0;
	objNewSymbolStatic("running");      srunning = r0;
	objNewSymbolStatic("ready");        sready = r0;
	objNewSymbolStatic("sleeping");     ssleeping = r0;
	objNewSymbolStatic("blocked");      sblocked = r0;
	objNewSymbolStatic("dead");         sdead = r0;
	objNewSymbolStatic("semaphore");    ssemaphore = r0;
	objNewSymbolStatic("openblocked");  sopenblocked = r0;
	objNewSymbolStatic("readblocked");  sreadblocked = r0;
	objNewSymbolStatic("writeblocked"); swriteblocked = r0;
	objNewSymbolStatic("accepting");    saccepting = r0;
	objNewSymbolStatic("connecting");   sconnecting = r0;
	objNewSymbolStatic("open");         sopen = r0;
	objNewSymbolStatic("closed");       sclosed = r0;
	objNewSymbolStatic("SIGNALHANDLERS");  signalhandlers=r0;

	/* Table of character objects.  The 257th is the EOF character. */
	memNewStaticVector(TVECTOR, 257);   characters = r0;
	for (i=0; i<256; i++) {
		memNewStatic(TCHAR, 1);  *(Int*)r0=i;
		memVectorSet(characters, i, r0);
	}

	/* Treat character number 256 0x100 as a char and as the eof object. */
	/* TODO Something fishy going on here.  When ctrl-c in telnet to an ipc
	    connection sysTransition fails because r2 (the character in question)
	    is an invalid pointer to object (pointing to the character-vector's
	    descriptor. Debugging... */
	memNewStatic(TEOF, 4);              eof = r0;
	*(Int*)eof = 256;                   memVectorSet(characters, 256, eof);

	DB("  --%s", __func_);
}
