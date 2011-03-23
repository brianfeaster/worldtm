#define DEBUG 0
#define DB_MODULE "OBJ "
#include "debug.h"

#include <stdio.h>
#include <unistd.h>
#include <string.h> /* memcpy */
#include <assert.h>

#include "obj.h"



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

Num wscmDebug=0;


/* This is a very popular hashing algorithm found online and in various texts.
   My tweak is to feed the last char back into the algorithm which seems to
   help distribute more single-length symbols.
*/
Num hashpjw (Str s, Num len) {
 Num ret=0, mask;
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
 Num i=0;
	while (next!=o) {next=cdr(next); i++; }
	return i;
}

/* Creates vector in r0 from list in r1.
*/
void objListToVector (void) {
 Num i=0, len;
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
/* Attempt at implementing static integers. */
	if (-1023l<=i && i<=1024l) {
		r0 = ((Obj*)staticIntegers)[i+1023];
		assert (*(Int*)r0 == i);
	} else {
   	memNewArray(TINTEGER, sizeof(Int));
   	*(Int*)r0 = i;
	}
/*
   memNewArray(TINTEGER, sizeof(Int));
   *(Int*)r0 = i;
*/
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
void objNewString (Str str, Num len) {
   memNewArray(TSTRING, len);
   if (str) memcpy(r0, str, len);
}

/* Create new string based on the string object in r1.
*/
void objCopyString (void) {
 Num len;
   memNewArray(TSTRING, len=memObjectLength(r1));
	memcpy(r0, r1, len);
}

void objNewSymbol (Str str, Num len) {
 static Num hash, i;
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
 static Num hash, i;
 Num len = strlen(s);
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
   memNewStaticVector(TSYSCALL, 1);
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

/* Create uninitialized vector in r0 of length r1:immediate
*/
void objNewVector (Num len) {
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
	memVectorSet(r0, 1, env); /* r16 */
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

Num objIsPair (Obj o) {
	return memObjectType(o) == TPAIR;
}

void objDumpR (Obj o, FILE *stream, Num islist) {
 Num i;
 char *c;

	if ((Num)o < 0x100000) {
		fprintf(stream, "#<%x>", o);
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
			if (cdr(o) == tge)
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
				fprintf(stream, OBJ, *((Obj*)o+i));
			}
			fwrite (")", 1, 1, stream);
			break;
		case TCODE :
			fprintf(stream, "#<CODE "OBJ">", o);
			break;
		case TSTACK :
			fprintf(stream, "#<STACK "OBJ">", o);
			break;
		case TSYSCALL :
			fprintf(stream, "#<SYSCALL "OBJ">", *(Obj*)o);
			break;
		default :
			if (tge == o) {
				fprintf(stream, "#<TGE "OBJ">", o);
			} else {
				fprintf(stream, HEX, o);
			}
			/* Dump the object description. */
			c = memObjString(o);
			if (c) {
				fwrite (":", 1, 1, stream);
				fwrite (c, 1, strlen(c), stream);
			}
	}
}

void objDump (Obj o, FILE *stream) {
	objDumpR(o, stream, 0);
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


/* Called by sys.c */
void objInitialize (Func scheduler) {
 Int i;
 Num n;
	DB("::%s", __func_);
	DB("  initializing memory module");
	asmInitialize(scheduler, objGCPre, objGCPost, objDump);
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
	for (n=0; n<2029; n++) memVectorSet (symbols, n, null);
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
	objNewSymbolStatic("procedure?");   sprocedurep= r0;
	objNewSymbolStatic("null?");        snullp= r0;
	objNewSymbolStatic("pair?");        spairp= r0;
	objNewSymbolStatic("vector?");      svectorp= r0;
	objNewSymbolStatic("string?");      sstringp= r0;
	objNewSymbolStatic("integer?");     sintegerp= r0;
	objNewSymbolStatic("symbol?");      ssymbolp= r0;
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

	/* Table of character objects.  The 257th character is the EOF object. */
	memNewStaticVector(TVECTOR, 257);   characters = r0;
	for (n=0; n<256; n++) {
		memNewStatic(TCHAR, 1);  *(Num*)r0=n;
		memVectorSet(characters, n, r0);
	}

	/* Treat character number 256 0x100 as a char and as the eof object. */
	memNewStatic(TEOF, 4);  eof = r0;
	*(Int*)eof = 256l;
 	memVectorSet(characters, 256, eof);

	/* Table of integer constants. */
	memNewStaticVector(TVECTOR, 2048);  staticIntegers = r0;
	for (i=-1023l; i<=1024l; ++i) {
   	memNewStatic(TINTEGER, sizeof(Int));
		*(Int*)r0 = i;
		memVectorSet(staticIntegers, (Num)i+1023, r0);
	}

	DB("  --%s", __func_);
}

#undef DB_MODULE
