#define DEBUG 0
#define DB_DESC "OBJ "
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h> /* memcpy */
#include <assert.h>
#include "obj.h"
#include "vm.h"
#include "mem.h"


const Num HashTableSize=8191;

/* Static symbol objects. */
Obj null, nullvec, nullstr, false, true, eof,
    srem, srunning, sready, ssleeping, sblocked, sdead, ssemaphore,
    sopenblocked, sreadblocked, swriteblocked,
    saccepting, sconnecting, sopen, sclosed,
    sdefine, slambda, smacro, squote, sunquote, squasiquote, sbegin, sunquotesplicing,
    sif, saif, scond, selse, sor, sand, ssetb,
    svectorref, svectorvectorref, svectorvectorsetb, svectorsetb, svectorlength,
    scons, scar, scdr, ssetcarb, ssetcdrb,
    sprocedurep, snullp,
    spairp, svectorp, sstringp, sintegerp, ssymbolp, sportp, sappend, seofobjectp,
    sthread, slet, sletrec,
    seval, sapply, scallcc, ssyntaxrules, seof,
    snot, sadd, ssub, smul, sdiv, slogand, characters, staticIntegers, signalhandlers;



/* This is a very popular hashing algorithm found online and in various texts.
   My tweak is to feed the last char back into the algorithm which seems to
   help distribute more single-length symbols.
*/
Num hashpjw (Str s, Num len) {
 Num ret=0, mask;
	while (len--) {
		if ((mask=(ret=(ret<<4)+*s++)&(Num)0xf<<28)) ret^=(mask|mask>>24);
	}
	if ((mask=(ret=(ret<<4)+*--s)&(Num)0xf<<28)) ret^=(mask|mask>>24);
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

/* Creates a new integer object in r0.  A few small
   integers are cached and used instead.
*/
void objNewInt  (Int i) {
	if (-1023l<=i && i<=1024l) {
		/* Lookup cached integer */
		r0 = ((Obj*)staticIntegers)[i+1023];
	} else {
		/* Generate a new integer object */
   	r0 = memNewArray(TINTEGER, sizeof(Int));
   	*(Int*)r0 = i;
	}
}

/* Create and set object in r0 to immediate signed integer value in r1.
*/
void objCopyInteger (void) {
   r0 = memNewArray(TINTEGER, sizeof(Int));
   *(Int*)r0 = *(Int*)r1;
}

/* Creates a new real object in r0.
*/
void objNewReal (Real x) {
   r0 = memNewArray(TREAL, sizeof(Real));
   *(Real*)r0 = x;
}
/* Create and set object in r0 to immediate real value in r1.
*/
void objCopyReal (void) {
   r0 = memNewArray(TREAL, sizeof(r32));
   *(r32*)r0 = *(r32*)r1;
}

/* Create new string copying len bytes from str to object in r0.
*/
void objNewString (Str str, Num len) {
   r0 = memNewArray(TSTRING, len);
   if (str) memcpy(r0, str, len);
}

/* Create new string based on the string object in r1.
*/
void objCopyString (void) {
 Num len;
   r0 = memNewArray(TSTRING, len=memObjectLength(r1));
	memcpy(r0, r1, len);
}

/* Create or return existing hashed string object.
	Copy the string if the copyStrp is set.
	Return 0 if the symbol exists in the hash table, 1 if not and was created.
*/
Num objNewSymbolBase (Str str, Num len, Num copyStrP) {
 static Num hash, i;
	i = hash = hashpjw(str, len) % HashTableSize;
	do {
		r0 = memVectorObject(rsymbols, i);
		/* Bucket empty so insert into symbol table. */
		if (r0 == null) {
			r0 = memNewArray(TSYMBOL, len);
			if (copyStrP) memcpy(r0, str, len);
			memVectorSet(rsymbols, i, r0);
			return 1;
		}
		/* If something here, check if it's the symbol */
		if(memObjectLength(r0)==len && !memcmp(r0,str,len)) {
			return 0;
		}
		/* Otherwise continue linear sweep for empty bucket or symbol. */
	} while ( (i=(++i==HashTableSize)?0:i) != hash);
	printf ("WARNING!!!  Symbol table full!!!!\n");
	r0 = memNewArray(TSYMBOL, len);
	if (copyStrP) memcpy((char*)r0, str, len);
	return 1;
}

void objNewSymbol (Str str, Num len) {
	objNewSymbolBase (str, len, 1); // 1 tells fn to copy the str
}

void objNewSymbolR5R6 (void) {
	if (objNewSymbolBase (r5, (Num)r6, 0)) // 0 tells fn to not copy the str
		memcpy((char*)r0, r5, (Num)r6);
}

void objNewSymbolStatic (char *s) {
 static Num hash, i;
 Num len = strlen(s);
	i = hash = hashpjw((Str)s, len) % HashTableSize;
	do {
		r0 = memVectorObject(rsymbols, i);
		/* Bucket empty so insert into symbol table. */
		if (r0 == null) {
			r0 = memNewStatic(TSYMBOL, len); /* r0 now holds new symbol */
			memcpy(r0, s, len);
			memVectorSet(rsymbols, i, r0);
			return;
		}
		/* If something here, check if it's the symbol */
		if(memObjectLength(r0)==len && !memcmp(r0,s,len)) {
			return;
		}
		/* Otherwise continue linear sweep for empty bucket or symbol. */
	} while ( (i=(++i==HashTableSize)?0:i) != hash);
	printf ("WARNING!!!  Symbol table full!!!!\n");
	r0 = memNewStatic(TSYMBOL, len);
	memcpy((char*)r0, s, len);
}

void objNewSyscall (Func f) {
   r0 = memNewStaticVector(TSYSCALL, 1);
	memVectorSet(r0, 0, f);
}

void objCons10 (void) {
 Obj o;
   o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, r1);
	memVectorSet(o, 1, r0);
	r0 = o;
}

void objCons12 (void) {
   r0 = memNewVector(TPAIR, 2);
	memVectorSet(r0, 0, r1);
	memVectorSet(r0, 1, r2);
}

void objCons23 (void) {
   r0 = memNewVector(TPAIR, 2);
	memVectorSet(r0, 0, r2);
	memVectorSet(r0, 1, r3);
}

void objNewDoublyLinkedListNode (void) {
	r0 = memNewVector(TVECTOR, 3);
	memVectorSet(r0, 0, null);
	memVectorSet(r0, 1, r0); /* Next */
	memVectorSet(r0, 2, r0); /* Prev */
}

void objNewVector (Num len) {
   r0 = memNewVector(TVECTOR, len);
}

/* Create uninitialized vector in r0 of length imm:r1
*/
void objNewVector1 (void) {
	DBBEG(" len="INT, (Int)r1);
   r0 = memNewVector(TVECTOR, (Length)r1);
	DBEND();
}

void objNewPort (void) {
	r0 = memNewVector(TPORT, 6);
	memVectorSet(r0, 0, r1); /* Descriptor. */
	memVectorSet(r0, 1, r2); /* Path or internet address string. */
	memVectorSet(r0, 2, r3); /* Flags or port number. */
	memVectorSet(r0, 3, r4); /* State: accepting, connecting, open, closed. */
	memVectorSet(r0, 4, false); /* Push back or next available character. */
	memVectorSet(r0, 5, false); /* Can hold a finalizer if you want. */
}

int objPortDescriptor (Obj p) {
	return (int)(Int)(memVectorObject(p, 0));
}

Obj objPortState (Obj p) {
	return memVectorObject(p, 3);
}

/*
Obj objPortDescriptor (Obj o) { return memVectorObject(o, 0); }
Obj objPortPath       (Obj o) { return memVectorObject(o, 1); }
Obj objPortFlags      (Obj o) { return memVectorObject(o, 2); }
Obj objPortState      (Obj o) { return memVectorObject(o, 3); }
Obj objPortPushback   (Obj o) { return memVectorObject(o, 4); }
*/

Num objIsPair (Obj o) {
	return memObjectType(o) == TPAIR;
}


Obj  car  (Obj o) { return memVectorObject(o, 0);}
Obj  cdr  (Obj o) { return memVectorObject(o, 1);}

Obj  caar (Obj o) { return car(car(o));}
Obj  cdar (Obj o) { return cdr(car(o));}

Obj  cadr (Obj o) { return car(cdr(o));}
Obj  cddr (Obj o) { return cdr(cdr(o));}


Num objListLength (Obj o) {
 Num i=0;
	while (objIsPair(o)) {
		o = cdr(o);
		i++;
	}
	return i;
}


Num objDoublyLinkedListLength (Obj o) {
 Obj next=cdr(o);
 Num i=1;
	while (next!=o) {next=cdr(next); i++; }
	return i;
}

Obj objDoublyLinkedListNext (Obj node) {
	return memVectorObject(node, 1);
}

Obj objDoublyLinkedListPrev (Obj node) {
	return memVectorObject(node, 2);
}

/* Insert thread node 'node' before node 'lst' 
      prev  node  lst            lst   node  lst
      t->   q->   p->            t->   q->   t-->
    <-q   <-p   <-t           <--t   <-q   <-t
 Implemented as vectors #(datum next prev).
*/
void objDoublyLinkedListInsert (Obj lst, Obj node) {
 Obj prev;
	prev = objDoublyLinkedListPrev(lst);
	memVectorSet(node, 1, lst);
	memVectorSet(node, 2, prev);
	memVectorSet(lst,  2, node);
	memVectorSet(prev, 1, node);
}

/* Add doubly linked list node 'node' after node 'lst'
*/
void objDoublyLinkedListAdd (Obj lst, Obj node) {
 Obj next;
	next = objDoublyLinkedListNext(lst);
	memVectorSet(node, 1, next);
	memVectorSet(node, 2, lst);
	memVectorSet(next, 2, node);
	memVectorSet(lst,  1, node);
}

/* Creates vector in r0 from list in r1.
*/
void objListToVector (void) {
 Num i=0, len;
	r1=r0;
	len = objListLength(r0);
	if (len) {
		r0 = memNewVector(TVECTOR, len); /* Create empty vector */
		while (i<len) { /* Stuff vector*/
			memVectorSet(r0, i++, car(r1));
			r1 = cdr(r1);
		}
	} else {
		r0 = nullvec;
	}
}

/* Motivation.
*/
void objDumpR (Obj o, FILE *stream, Num islist) {
 Num i;
 char *c;
 Str s;

	if (-0x430000 < (Int)o && (Int)o < 0x430000) {
		fprintf(stream, "#<");
		if ((Int)o < 0l)
			fprintf(stream, "-"HEX, labs((Int)o));
		else
			fprintf(stream, HEX, o);
		fprintf(stream, ">");
		return;
	}

	switch (memObjectType(o)) {
		case TNULL   :
		case TNULLSTR:
		case TNULLVEC:
		case TFALSE  :
		case TTRUE   :
		case TEOF    :
		case TSYMBOL :
			fwrite (o, 1, memObjectLength(o), stream);
			break;
		case TINTEGER:
			fprintf(stream, INT, *(Int*)o);
			break;
		case TSTRING : 
			fwrite ("\"", 1, 1, stream);
			c = o;
			for (i=memObjectLength(o); i; i--) {
				switch (*c) {
					case '\\' : fwrite("\\\\",1, 2, stream); break;
					case '\"' : fwrite("\\\"",1, 2, stream); break;
					case '\a' : fwrite("\\a", 1, 2, stream); break;
					case '\e' : fwrite("\\e", 1, 2, stream); break;
					case '\233':fwrite("\\c", 1, 2, stream); break;
					case '\n' : fwrite("\\n", 1, 2, stream); break;
					case '\r' : fwrite("\\r", 1, 2, stream); break;
					case '\t' : fwrite("\\t", 1, 2, stream); break;
					default   : fwrite(c, 1, 1, stream);
				}
				c++;
			}
			fwrite ("\"", 1, 1, stream);
			break;
		case TCLOSURE :
			if (cdr(o) == rtge)
				fprintf(stream, "#CLOSURE<CODE:"OBJ" TGE:"OBJ">", car(o), cdr(o));
			else
				fprintf(stream, "#CLOSURE<CODE:"OBJ" ENV:"OBJ">", car(o), cdr(o));
			break;
		case TPAIR :
			if (!islist) fwrite ("(", 1, 1, stream);
			objDumpR(car(o), stream, 0);

			if (objIsPair(cdr(o))) {
				fwrite (" ", 1, 1, stream);
				objDumpR(cdr(o), stream, 1);
			} else {
				if (cdr(o) != null) {
					fwrite (" . ", 1, 3, stream);
					objDumpR(cdr(o), stream, 0);
				}
			}

			if (!islist) fwrite (")", 1, 1, stream);
			break;
		case TVECTOR :
			fwrite ("#(", 1, 2, stream);
			for (i=0; i<memObjectLength(o); i++) {
				if (i) fwrite (" ", 1, 1, stream);
				//fprintf(stream, OBJ, *((Obj*)o+i));
				objDumpR(((Obj*)o)[i], stream, 0);
			}
			fwrite (")", 1, 1, stream);
			break;
		case TSYSCALL :
			fprintf(stream, "#<SYSCALL "OBJ">", *(Obj*)o);
			break;
		case TPORT :
			fprintf(stream, "#<PORT ");
			objDumpR(memVectorObject(o, 0), stream, 0);
			fprintf(stream, " ");
			objDumpR(memVectorObject(o, 1), stream, 0);
			fprintf(stream, " ");
			objDumpR(memVectorObject(o, 2), stream, 0);
			fprintf(stream, " ");
			objDumpR(memVectorObject(o, 3), stream, 0);
			fprintf(stream, " ");
			objDumpR(memVectorObject(o, 4), stream, 0);
			fprintf(stream, " ");
			objDumpR(memVectorObject(o, 5), stream, 0);
			fprintf(stream, ">");
			break;
		default :
			if (rtge == o) {
				fprintf(stream, "#<TGE "OBJ">", o);
			} else {
				fprintf(stream, HEX, o);
			}
			/* Dump the object description. */
			s = memPointerString(o);
			if (s) {
				fwrite (":", 1, 1, stream);
				fwrite (s, 1, strlen((char*)s), stream);
			}
	}
}

void objDump (Obj o, FILE *stream) {
	objDumpR(o, stream, 0);
}


void objInitialize (void) {
 static Num shouldInitialize=1;
 Int i;
 Num n;
	DBBEG();
	if (shouldInitialize) {
		DB("Activating module...");
		shouldInitialize=0;
		vmInitialize(0, 0);
		memInitialize(0, 0);
		DB("Register the internal object types");
		memTypeRegisterString(TFALSE, "false");
		memTypeRegisterString(TTRUE, "true");
		memTypeRegisterString(TNULL, "null");
		memTypeRegisterString(TNULLVEC, "nullvec");
		memTypeRegisterString(TNULLSTR, "nullstr");
		memTypeRegisterString(TEOF, "eof");
		memTypeRegisterString(TCHAR, "chr");
		memTypeRegisterString(TSTRING, "str");
		memTypeRegisterString(TSYMBOL, "symb");
		memTypeRegisterString(TINTEGER, "int");
		memTypeRegisterString(TREAL, "real");
		memTypeRegisterString(TPAIR, "pair");
		memTypeRegisterString(TVECTOR, "vector");
		memTypeRegisterString(TCLOSURE, "closure");
		memTypeRegisterString(TCONTINUATION, "contin");
		memTypeRegisterString(TPORT, "port");
		memTypeRegisterString(TSOCKET, "socket");
		memTypeRegisterString(TSYSCALL, "syscall");

		/* These primitive types are also external (display) strings. */
		null = memNewStatic(TNULL, 2);         memcpy(null, "()", 2);
		/* This is a strange object with a descriptor and no content.
		   Since little endian a valid pointer to empty C string.  */
		nullstr = memNewStatic(TNULLSTR, 0);
		nullvec = memNewStatic(TNULLVEC, 3);  memcpy(nullvec, "#()", 3);
		false = memNewStatic(TFALSE, 2);      memcpy(false, "#f", 2);
		true = memNewStatic(TTRUE, 2);        memcpy(true, "#t", 2);

		rsymbols = memNewVector(TVECTOR, HashTableSize); /* Symbol table */
		for (n=0; n<HashTableSize; n++) memVectorSet (rsymbols, n, null);

		objNewSymbolStatic("define");       sdefine = r0;
		objNewSymbolStatic("lambda");       slambda = r0;
		objNewSymbolStatic("macro");        smacro = r0;
		objNewSymbolStatic("quote");        squote = r0;
		objNewSymbolStatic("unquote");      sunquote = r0;
		objNewSymbolStatic("quasiquote");   squasiquote = r0;
		objNewSymbolStatic("unquote-splicing"); sunquotesplicing = r0;
		objNewSymbolStatic("begin");        sbegin = r0;
		objNewSymbolStatic("if");           sif = r0;
		objNewSymbolStatic("=>");           saif = r0;
		objNewSymbolStatic("cond");         scond = r0;
		objNewSymbolStatic("else");         selse = r0;
		objNewSymbolStatic("or");           sor = r0;
		objNewSymbolStatic("and");          sand = r0;
		objNewSymbolStatic("set!");         ssetb = r0;
		objNewSymbolStatic("vector-ref");   svectorref = r0;
		objNewSymbolStatic("vector-vector-ref"); svectorvectorref = r0;
		objNewSymbolStatic("vector-set!");  svectorsetb = r0;
		objNewSymbolStatic("vector-vector-set!"); svectorvectorsetb = r0;
		objNewSymbolStatic("vector-length");svectorlength = r0;
		objNewSymbolStatic("cons");         scons = r0;
		objNewSymbolStatic("car");          scar = r0;
		objNewSymbolStatic("cdr");          scdr = r0;
		objNewSymbolStatic("set-car!");     ssetcarb = r0;
		objNewSymbolStatic("set-cdr!");     ssetcdrb = r0;
		objNewSymbolStatic("procedure?");   sprocedurep = r0;
		objNewSymbolStatic("null?");        snullp = r0;
		objNewSymbolStatic("pair?");        spairp = r0;
		objNewSymbolStatic("vector?");      svectorp = r0;
		objNewSymbolStatic("string?");      sstringp = r0;
		objNewSymbolStatic("integer?");     sintegerp = r0;
		objNewSymbolStatic("symbol?");      ssymbolp = r0;
		objNewSymbolStatic("port?");        sportp = r0;
		objNewSymbolStatic("append");       sappend = r0;
		objNewSymbolStatic("eof-object?");  seofobjectp = r0;
		objNewSymbolStatic("thread");       sthread = r0;
		objNewSymbolStatic("let");          slet = r0;
		objNewSymbolStatic("letrec");       sletrec = r0;
		objNewSymbolStatic("eval");         seval = r0;
		objNewSymbolStatic("apply");        sapply = r0;
		objNewSymbolStatic("call/cc");      scallcc = r0;
		objNewSymbolStatic("syntax-rules"); ssyntaxrules = r0;
		objNewSymbolStatic("#eof");         seof = r0;
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
		objNewSymbolStatic("SIGNALHANDLERS");  signalhandlers = r0;

		/* Table of character objects.  The 257th character is the EOF object. */
		characters = memNewStaticVector(TVECTOR, 257);
		for (n=0; n<256; n++) {
			r0 = memNewStatic(TCHAR, 1);
			*(Num*)r0 = n;
			memVectorSet(characters, n, r0);
		}

		/* Treat character number 256 0x100 as a char and as the eof object. */
		eof = memNewStatic(TEOF, 4);
		*(Int*)eof = 256l;
		memVectorSet(characters, 256, eof);

		/* Table of integer constants. */
		staticIntegers = memNewStaticVector(TVECTOR, 2048);
		for (i=-1023l; i<=1024l; ++i) {
	  	 	r0 = memNewStatic(TINTEGER, sizeof(Int));
			*(Int*)r0 = i;
			memVectorSet(staticIntegers, (Num)i+1023, r0);
		}
	} else {
		DB("Module already activated");
	}

	DBEND();
}

#undef DB_DESC
#undef DEBUG
