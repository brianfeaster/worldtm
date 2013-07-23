#define DEBUG 0
#define DB_DESC "OBJ "
#include "debug.h"
#include <stdio.h>
#include <stdlib.h> /* labs */
#include <unistd.h> /* getcwd */
#include <fcntl.h> /* fcntl */
#include <string.h> /* memcpy */
#include <assert.h>
#include "obj.h"
#include "vm.h"
#include "mem.h"

/*
 Objects
 Algorithms
 Serializers
 Init

  ABOUT
*/


const Num HashTableSize=8191; /* Best if prime */


/*******************************************************************************
 Objects
*******************************************************************************/
Obj onull, ofalse, otrue, oeof, onullvec, onullstr; /* Intrinsic static objects */
Obj ocharacters, ointegers; /* Intrinsic static aggregates */
Obj odebug; /* Mutable */
Obj osymbols; /* Intrinsic mutable aggregates */

/* Symbol objects */
Obj snull, sfalse, strue, seof, 
    srem, srunning, sready, ssleeping, sblocked, sdead, ssemaphore,
    sopenblocked, sreadblocked, swriteblocked,
    saccepting, sconnecting, sopen, sclosed,
    sdefine, slambda, smacro, squote, sunquote, squasiquote, sbegin, sunquotesplicing,
    sif, saif, scond, scase, selse, sor, sand, ssetb,
    svectorref, svectorvectorref, svectorvectorsetb, svectorsetb, svectorlength,
    scons, scar, scdr, ssetcarb, ssetcdrb,
    smemv, sprocedurep, snullp,
    spairp, svectorp, scharp, sstringp, sintegerp, ssymbolp, sportp, sappend, seofobjectp,
    sthread, slet, sletstar, sletrec,
    seval, sapply, scallcc, ssyntaxrules,
    snot, sadd, ssub, smul, sdiv, slogand, ssignalhandlers;



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
	if (-1024l<=i && i<=1023l) {
		/* Lookup cached integer */
		r0 = ((Obj*)ointegers)[i+1024];
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

/* Create new string copying len bytes from str to object in r0.
*/
void objNewStringString (Str str1, Num len1, Str str2, Num len2) {
   r0 = memNewArray(TSTRING, (len1 + len2));
   if (str1) memcpy(r0, str1, len1);
   if (str2) memcpy((r0 + len1), str2, len2);
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
		r0 = memVectorObject(osymbols, i);
		/* Bucket empty so insert into symbol table. */
		if (r0 == onull) {
			r0 = memNewArray(TSYMBOL, len);
			if (copyStrP) memcpy(r0, str, len);
			memVectorSet(osymbols, i, r0);
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
	if (objNewSymbolBase(r5, (Num)r6, 0)) // 0 tells fn to not copy the str
		memcpy((char*)r0, r5, (Num)r6);
}

void objNewSymbolStatic (char *s) {
 static Num hash, i;
 Num len = strlen(s);
	i = hash = hashpjw((Str)s, len) % HashTableSize;
	do {
		r0 = memVectorObject(osymbols, i);
		/* Bucket empty so insert into symbol table. */
		if (r0 == onull) {
			r0 = memNewStatic(TSYMBOL, len); /* r0 now holds new symbol */
			memcpy(r0, s, len);
			memVectorSet(osymbols, i, r0);
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

void objNewPrimitive (Func f) {
   r0 = memNewArray(TPRIMITIVE, 4);
   *(Func*)r0 = f;
}

/* Safely creates and returns a new pair from C args
*/
Obj objCons (Obj a, Obj b) {
 Obj o;
	vmPush(b);
	vmPush(a);
	o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, vmPop());
	memVectorSet(o, 1, vmPop());
	return o;
}

void objCons010(void) {
 Obj o;
	o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, r1);
	memVectorSet(o, 1, r0);
	r0 = o;
}

void objCons101 (void) {
 Obj o;
	o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, r0);
	memVectorSet(o, 1, r1);
	r1 = o;
}

void objCons303 (void) {
 Obj o;
   o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, r0);
	memVectorSet(o, 1, r3);
	r3 = o;
}

void objConsStack0 (void) {
 Obj o;
   o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, vmPop());
	memVectorSet(o, 1, r0);
	r0 = o;
}

void objCons01 (void) {
 Obj o;
   o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, r0);
	memVectorSet(o, 1, r1);
	r0 = o;
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
	memVectorSet(r0, 0, onull);
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

/*
       FILE         SOCKET                            STRING
       descriptor   descriptor                        ""
       path         url                               string buffer
       flags        port                              imm:string index
       open/closed  accepting/connecting/open/closed  open/closed
       
*/
void objNewPort (void) {
	r0 = memNewVector(TPORT, 6);
	memVectorSet(r0, 0, r1); /* Descriptor or empty string. */
	memVectorSet(r0, 1, r2); /* Path or internet address string or string buffer. */
	memVectorSet(r0, 2, r3); /* Flags or port number or string index. */
	memVectorSet(r0, 3, r4); /* State: accepting, connecting, open, closed. */
	memVectorSet(r0, 4, ofalse); /* Push back or next available character. */
	memVectorSet(r0, 5, ofalse); /* Can hold a finalizer if you want. */
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

Obj objIntegerToChar (Num i) {
	assert(i < 256);
	return memVectorObject(ocharacters, i);
}

Num objIsPair (Obj o)   { return memIsObjectType(o, TPAIR); }
Num objIsSymbol (Obj o) { return memIsObjectType(o, TSYMBOL); }


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



/*******************************************************************************
 Algorithms
*******************************************************************************/
/* Copies list to a new vector
  r0 <= List
  r1  = Clobbered
  r0  => Vector
*/
void objListToVector (void) {
 Num i=0, len;
	len = objListLength(r0);
	if (len) {
		r1=r0;
		r0 = memNewVector(TVECTOR, len); /* Create empty vector */
		while (i<len) { /* Stuff vector*/
			memVectorSet(r0, i++, car(r1));
			r1 = cdr(r1);
		}
	} else {
		r0 = onullvec;
	}
}



/*******************************************************************************
 Serializers

 Parameter 'stream' should actually be a 'FILE*' object even though passed
 around as an Obj.
*******************************************************************************/
Func2ObjFile ObjectDisplayCallbacks[MEMMAXTYPES];
Func2ObjFile ObjectWriteCallbacks[MEMMAXTYPES];


void objDisplayTypeDefault (Obj o, FILE *stream) {
 Type t;
 Str s;
	s = memTypeString(t = memObjectType(o));
	if (s) fprintf(stream, "#"STR"<"HEX">", s, o);
	else fprintf(stream, "#"HEX"<"HEX">", memObjectType(o), o);
}


void objDisplayTypeIntrinsic (Obj o, FILE *stream) {
	if      (onull == o)  fwrite("()", 1, 2, stream);
	else if (ofalse == o) fwrite("#f", 1, 2, stream);
	else if (otrue == o)  fwrite("#t", 1, 2, stream);
	else if (oeof == o)   fwrite("#eof", 1, 4, stream);
	else assert(!"Unknown object");
}


void objDisplayTypeChar (Obj o, FILE *stream) {
	fwrite(o, 1, 1, stream);
}


void objWriteTypeChar (Obj o, FILE *stream) {
	fprintf(stream, "#\\%c", *(Chr*)o);
}


void objDisplayTypeSymbol (Obj o, FILE *stream) {
	fwrite (o, 1, memObjectLength(o), stream);
}


void objDisplayTypeString (Obj o, FILE *stream) {
	fwrite (o, 1, memObjectLength(o), stream);
}

void objWriteTypeString (Obj o, FILE *stream) {
 Num i, len;
 Chr c;
	fwrite ("\"", 1, 1, stream);
	len = memObjectLength(o);
	for (i = 0; i < len; ++i) {
		c = ((Chr*)r0)[i];
		switch (c) {
		case '\0'  : fwrite("\\0", 1, 2, stream); break; /* 0 */
		case '\a'  : fwrite("\\a", 1, 2, stream); break; /* 7 */
		case '\b'  : fwrite("\\b", 1, 2, stream); break; /* 8 */
		case '\t'  : fwrite("\\t", 1, 2, stream); break; /* 9 */
		case '\n'  : fwrite("\\n", 1, 2, stream); break; /* 10 */
		case '\v'  : fwrite("\\v", 1, 2, stream); break; /* 11 */
		case '\f'  : fwrite("\\f", 1, 2, stream); break; /* 12 */
		case '\r'  : fwrite("\\r", 1, 2, stream); break; /* 13 */
		case '\033': fwrite("\\e", 1, 2, stream); break; /* 27 */
		case '\"'  : fwrite("\\\"", 1, 2, stream); break; /* 34 */
		case '\\'  : fwrite("\\\\", 1, 2, stream); break; /* 92 */
		case (Chr)'\233': fwrite("\\c", 1, 2, stream); break; /* 155 */
		default:
			/* The printable chracter or a slashified hex */
			if ((32<=c && c<=126) || (160<=c && c<=255)) fputc(c, stream);
			else fprintf (stream, "\\x"HEX02, (Num)c);
		}
	}
}


void objDisplayTypeInteger (Obj o, FILE *stream) {
	fprintf(stream, INT, *(Int*)o);
}


void objDisplayTypeReal (Obj o, FILE *stream) {
	fprintf(stream, REAL, *(Real*)o);
}

void objDisplayTypePrimitive (Obj o, FILE *stream) {
	fprintf(stream, "#PRIM<" HEX ">", *(Func*)o);
}


void objDisplayTypeVector (Obj o, FILE *stream) {
 Num i;
	fwrite("#(", 1, 2, stream);
	for (i=0; i<memObjectLength(o); i++) {
		if (i) fwrite (" ", 1, 1, stream);
		objDisplay(((Obj*)o)[i], stream);
	}
	fwrite(")", 1, 1, stream);
}


/* Displays a dotted pair or pretty list
*/
void objDisplayTypePair (Obj o, FILE *stream) {
	fwrite ("(", 1, 1, stream);
	objDisplay(car(o), stream);

	o = cdr(o);
	while (objIsPair(o)) {
		fwrite (" ", 1, 1, stream);
		objDisplay(car(o), stream);
		o = cdr(o);
	}

	if (onull != o) {
		fwrite (" . ", 1, 3, stream);
		objDisplay(o, stream);
	}
	fwrite (")", 1, 1, stream);
}


/* Serialize a stack object converting objects/pointers/immediates
   to basic strings.
*/
void objDisplayTypeStack (Obj o, FILE *stream) {
 Num i;
 Obj p;
	fprintf(stream, "#stack["HEX04" |", memStackLength(o)); /* Length */
	for (i=0; i<memStackLength(o); i++) {
		p = memStackObject(o, memStackLength(o)-i-1);
		fwrite(" ", 1, 1, stream);
		if (memIsObjectValid(p)) /* Live objects get a type description */
			objDisplayTypeDefault(p, stream);
		else  /* Immediate/pointers shown as signed hex */
			if ((Int)p < 0l) fprintf(stream, "-"HEX, labs((Int)p));
			else fprintf(stream, HEX, p);
	}
	fprintf(stream, "]");
/*
	for (i=0; i<memStackLength(o); i++) {
		p = memStackObject(o, memStackLength(o)-i-1);
		fwrite("\n", 1, 1, stream);
		objDisplay(p, stream);
	}
*/
}


void objDisplay (Obj o, FILE *stream) {
 int fdState;
 Str s;

	/* Enable I/O state blocking */
	fcntl(0, F_SETFL, (fdState=fcntl(0, F_GETFL, 0)) & ~O_NONBLOCK);

	if (memIsObjectValid(o)) {
		/* A live scheme object */
		ObjectDisplayCallbacks[memObjectType(o)](o, stream);
	} else {
		/* An immediate or C pointer */
		fwrite("#<", 1, 2, stream);

		/* Display pointer/immediate as a signed HEX value */
		if ((Int)o < 0l) fprintf(stream, "-"HEX, labs((Int)o));
		else fprintf(stream, HEX, (Int)o);

		/* The C pointer's registered string */
		if ((s = memPointerString(o))) fprintf (stream, ":%s", s);

		fwrite(">", 1, 1, stream);
	}
	/* Restore I/O state */
	fcntl(0, F_SETFL, fdState);
}

void objWrite (Obj o, FILE *stream) {
 int fdState;
 Str s;
	/* Enable I/O state blocking */
	fcntl(0, F_SETFL, (fdState=fcntl(0, F_GETFL, 0)) & ~O_NONBLOCK);

	if (memIsObjectValid(o)) {
		ObjectDisplayCallbacks[memObjectType(o)](o, stream);
	} else {
		fwrite("#<", 1, 2, stream);
		/* Display pointer/immediate as a signed HEX value */
		if ((Int)o < 0l) fprintf(stream, "-"HEX, labs((Int)o));
		else fprintf(stream, HEX, (Int)o);
		/* Dump the object description. */
		if ((s = memPointerString(o))) fprintf (stream, ":%s", s);
		fwrite(">", 1, 1, stream);
	}
	/* Restore I/O state */
	fcntl(0, F_SETFL, fdState);
}


/* The type serializer callback can be set once or initialized
   to the default then set.
*/
void objDisplayTypeRegister (Type type, Func2ObjFile serializer) {
	assert(type < MEMMAXTYPES);
	if (objDisplayTypeDefault == serializer) {
		if (!ObjectDisplayCallbacks[type])
			ObjectDisplayCallbacks[type] = serializer;
	} else {
		assert(!ObjectDisplayCallbacks[type] || ObjectDisplayCallbacks[type] == objDisplayTypeDefault);
		ObjectDisplayCallbacks[type] = serializer;
	}
}

void objWriteTypeRegister (Type type, Func2ObjFile serializer) {
	assert(type < MEMMAXTYPES);
	if (objDisplayTypeDefault == serializer) {
		if (!ObjectWriteCallbacks[type])
			ObjectWriteCallbacks[type] = serializer;
	} else {
		assert(!ObjectWriteCallbacks[type] || ObjectWriteCallbacks[type] == objDisplayTypeDefault);
		ObjectWriteCallbacks[type] = serializer;
	}
}


void objSerializerInitialize (void) {
 Num i;
	for (i=0; i < MEMMAXTYPES; ++i) {
		objDisplayTypeRegister(i, objDisplayTypeDefault);
		objWriteTypeRegister(i, objDisplayTypeDefault);
	}

	objDisplayTypeRegister(TINTRINSIC, objDisplayTypeIntrinsic);
	objDisplayTypeRegister(TCHAR,      objDisplayTypeChar);
	objDisplayTypeRegister(TSYMBOL,    objDisplayTypeSymbol);
	objDisplayTypeRegister(TSTRING,    objDisplayTypeString);
	objDisplayTypeRegister(TINTEGER,   objDisplayTypeInteger);
	objDisplayTypeRegister(TREAL,      objDisplayTypeReal);
	objDisplayTypeRegister(TPRIMITIVE, objDisplayTypePrimitive);
	objDisplayTypeRegister(TPAIR,      objDisplayTypePair);
	objDisplayTypeRegister(TVECTOR,    objDisplayTypeVector);
	objDisplayTypeRegister(TSTACK,     objDisplayTypeStack);
	objDisplayTypeRegister(TCODE,      vmDisplayTypeCode);

	objWriteTypeRegister(TINTRINSIC, objDisplayTypeIntrinsic);
	objWriteTypeRegister(TCHAR,      objWriteTypeChar);
	objWriteTypeRegister(TSYMBOL,    objDisplayTypeSymbol);
	objWriteTypeRegister(TSTRING,    objWriteTypeString);
	objWriteTypeRegister(TINTEGER,   objDisplayTypeInteger);
	objWriteTypeRegister(TPRIMITIVE, objDisplayTypePrimitive);
	objWriteTypeRegister(TPAIR,      objDisplayTypePair);
	objWriteTypeRegister(TVECTOR,    objDisplayTypeVector);
	objWriteTypeRegister(TSTACK,     objDisplayTypeStack);
	objWriteTypeRegister(TCODE,      vmDisplayTypeCode);
}



/*******************************************************************************
 Init
*******************************************************************************/
void objInitialize (void) {
 static Num shouldInitialize=1;
 Int i;
 Num n;
	DBBEG();
	if (shouldInitialize) {
		DB("Activating module");
		shouldInitialize=0;

		DB("Initializing submodules");
		vmInitialize(0, 0);

		DB("Initialize serializers");
		objSerializerInitialize();

		DB("Register the internal object types");
		memTypeRegisterString(TINTRINSIC, (Str)"intrinsic");
		memTypeRegisterString(TCHAR, (Str)"chr");
		memTypeRegisterString(TSTRING, (Str)"str");
		memTypeRegisterString(TSYMBOL, (Str)"symb");
		memTypeRegisterString(TINTEGER, (Str)"int");
		memTypeRegisterString(TREAL, (Str)"real");
		memTypeRegisterString(TPRIMITIVE, (Str)"primitive");
		memTypeRegisterString(TPAIR, (Str)"pair");
		memTypeRegisterString(TVECTOR, (Str)"vector");
		memTypeRegisterString(TCLOSURE, (Str)"closure");
		//memTypeRegisterString(TCONTINUATION, (Str)"contin");
		memTypeRegisterString(TPORT, (Str)"port");
		//memTypeRegisterString(TSOCKET, (Str)"socket");
		memTypeRegisterString(TSYSCALL, (Str)"syscall");

		/* Intrinsic objects are defined by their static object pointer address.
		   Their display/write strings are also stored, even though their official
		   length is zero, using format string "%.2s".  */
		ofalse = memNewStatic(TINTRINSIC, 0); memPointerRegisterString(ofalse, (Str)"#f");
		otrue  = memNewStatic(TINTRINSIC, 0); memPointerRegisterString(otrue, (Str)"#t");
		onull  = memNewStatic(TINTRINSIC, 0); memPointerRegisterString(onull, (Str)"()");
		oeof   = memNewStatic(TINTRINSIC, 0); memPointerRegisterString(oeof, (Str)"#eof");
		onullvec = memNewStatic(TVECTOR, 0);  memPointerRegisterString(onullvec, (Str)"#()");
		onullstr = memNewStatic(TSTRING, 0);  memPointerRegisterString(onullstr, (Str)"\"\"");

		/* Table of character objects */
		DB("Creating vector of character constants");
		ocharacters = memNewStaticVector(TVECTOR, 256);
		for (n=0; n<256; n++) {
			r0 = memNewStatic(TCHAR, 1);
			*(Num*)r0 = n;
			memVectorSet(ocharacters, n, r0);
		}

		DB("Creating global symbol table and initial symbols");
		memRootSetRegister(osymbols);
		osymbols = memNewVector(TVECTOR, HashTableSize);
		for (n=0; n<HashTableSize; n++) memVectorSet (osymbols, n, onull);

		objNewSymbolStatic("#f");           sfalse = r0;
		objNewSymbolStatic("#t");           strue = r0;
		objNewSymbolStatic("()");           snull = r0;
		objNewSymbolStatic("#eof");         seof = r0;

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
		objNewSymbolStatic("case");         scase = r0;
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
		objNewSymbolStatic("memv");         smemv = r0;
		objNewSymbolStatic("procedure?");   sprocedurep = r0;
		objNewSymbolStatic("null?");        snullp = r0;
		objNewSymbolStatic("pair?");        spairp = r0;
		objNewSymbolStatic("vector?");      svectorp = r0;
		objNewSymbolStatic("char?");        scharp = r0;
		objNewSymbolStatic("string?");      sstringp = r0;
		objNewSymbolStatic("integer?");     sintegerp = r0;
		objNewSymbolStatic("symbol?");      ssymbolp = r0;
		objNewSymbolStatic("port?");        sportp = r0;
		objNewSymbolStatic("append");       sappend = r0;
		objNewSymbolStatic("eof-object?");  seofobjectp = r0;
		objNewSymbolStatic("thread");       sthread = r0;
		objNewSymbolStatic("let");          slet = r0;
		objNewSymbolStatic("let*");         sletstar = r0;
		objNewSymbolStatic("letrec");       sletrec = r0;
		objNewSymbolStatic("eval");         seval = r0;
		objNewSymbolStatic("apply");        sapply = r0;
		objNewSymbolStatic("call/cc");      scallcc = r0;
		objNewSymbolStatic("syntax-rules"); ssyntaxrules = r0;
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
		objNewSymbolStatic("SIGNALHANDLERS"); ssignalhandlers = r0;

		DB("Creating vector of integer constants");
		ointegers = memNewStaticVector(TVECTOR, 2048);
		for (i=-1024l; i<=1023l; ++i) {
			r0 = memNewStatic(TINTEGER, sizeof(Int));
			*(Int*)r0 = i;
			memVectorSet(ointegers, (Num)i+1024, r0);
		}

		DB("Registering static pointer description strings");
		memPointerRegister(objCopyInteger);
		memPointerRegister(objCopyString);
		memPointerRegister(objCons12);
		memPointerRegister(objCons23);
		memPointerRegister(objCons10);
		memPointerRegister(objNewVector1);
		memPointerRegister(objListToVector);

		odebug = ofalse;

	} else {
		DB("Module already activated");
	}

	DBEND();
}

#undef DB_DESC
#undef DEBUG
