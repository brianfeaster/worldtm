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
 Ordered_Sets
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

/* Creates a new integer object in r00.  A few small
   integers are cached and used instead.
*/
void objNewInt  (Int i) {
	if (-1024l<=i && i<=1023l) {
		/* Lookup cached integer */
		r00 = ((Obj*)ointegers)[i+1024];
	} else {
		/* Generate a new integer object */
   	r00 = memNewArray(TINTEGER, sizeof(Int));
   	*(Int*)r00 = i;
	}
}

/* Create and set object in r00 to immediate signed integer value in r01.
*/
void objCopyInteger (void) {
   r00 = memNewArray(TINTEGER, sizeof(Int));
   *(Int*)r00 = *(Int*)r01;
}

/* Creates a new real object in r00.
*/
void objNewReal (Real x) {
   r00 = memNewArray(TREAL, sizeof(Real));
   *(Real*)r00 = x;
}
/* Create and set object in r00 to immediate real value in r1.
*/
void objCopyReal (void) {
   r00 = memNewArray(TREAL, sizeof(r32));
   *(r32*)r00 = *(r32*)r01;
}

/* Create new string copying len bytes from str to object in r00.
*/
void objNewString (Str str, Num len) {
   r00 = memNewArray(TSTRING, len);
   if (str) memcpy(r00, str, len);
}

/* Create new string copying len bytes from str to object in r00.
*/
void objNewStringString (Str str1, Num len1, Str str2, Num len2) {
   r00 = memNewArray(TSTRING, (len1 + len2));
   if (str1) memcpy(r00, str1, len1);
   if (str2) memcpy((r00 + len1), str2, len2);
}

/* Create new string based on the string object in r1.
*/
void objCopyString (void) {
 Num len;
   r00 = memNewArray(TSTRING, len=memObjectLength(r01));
	memcpy(r00, r01, len);
}

/* Create or return existing hashed string object.
	Copy the string if the copyStrp is set.
	Return 0 if the symbol exists in the hash table, 1 if not and was created.
*/
Num objNewSymbolBase (Str str, Num len, Num copyStrP) {
 static Num hash, i;
	i = hash = hashpjw(str, len) % HashTableSize;
	do {
		r00 = memVectorObject(osymbols, i);
		/* Bucket empty so insert into symbol table. */
		if (r00 == onull) {
			r00 = memNewArray(TSYMBOL, len);
			if (copyStrP) memcpy(r00, str, len);
			memVectorSet(osymbols, i, r00);
			return 1;
		}
		/* If something here, check if it's the symbol */
		if(memObjectLength(r00)==len && !memcmp(r00,str,len)) {
			return 0;
		}
		/* Otherwise continue linear sweep for empty bucket or symbol. */
	} while ( (i=(++i==HashTableSize)?0:i) != hash);
	printf ("WARNING!!!  Symbol table full!!!!\n");
	r00 = memNewArray(TSYMBOL, len);
	if (copyStrP) memcpy((char*)r00, str, len);
	return 1;
}

void objNewSymbol (Str str, Num len) {
	objNewSymbolBase (str, len, 1); // 1 tells fn to copy the str
}

void objNewSymbolR05R06 (void) {
	if (objNewSymbolBase(r05, (Num)r06, 0)) // 0 tells fn to not copy the str in case a GC occurs moving the objects location in the heap.
		memcpy((char*)r00, r05, (Num)r06);
}

void objNewSymbolStatic (char *s) {
 static Num hash, i;
 Num len = strlen(s);
	i = hash = hashpjw((Str)s, len) % HashTableSize;
	do {
		r00 = memVectorObject(osymbols, i);
		/* Bucket empty so insert into symbol table. */
		if (r00 == onull) {
			r00 = memNewStatic(TSYMBOL, len); /* r00 now holds new symbol */
			memcpy(r00, s, len);
			memVectorSet(osymbols, i, r00);
			return;
		}
		/* If something here, check if it's the symbol */
		if(memObjectLength(r00)==len && !memcmp(r00,s,len)) {
			return;
		}
		/* Otherwise continue linear sweep for empty bucket or symbol. */
	} while ( (i=(++i==HashTableSize)?0:i) != hash);
	printf ("WARNING!!!  Symbol table full!!!!\n");
	r00 = memNewStatic(TSYMBOL, len);
	memcpy((char*)r00, s, len);
}

void objNewSyscall (Func f) {
   r00 = memNewStaticVector(TSYSCALL, 1);
	memVectorSet(r00, 0, f);
}

void objNewPrimitive (Func f) {
   r00 = memNewArray(TPRIMITIVE, 4);
   *(Func*)r00 = f;
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

/* return => (cons (pop)  r00)
*/
Obj objConsSTK0 (void) {
 Obj o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, vmPop());
	memVectorSet(o, 1, r00);
	return o;
}

/* r00 => (cons r01  r00)
*/
void objCons010(void) {
 Obj o;
	o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, r01);
	memVectorSet(o, 1, r00);
	r00 = o;
}

/* r01 => (cons r00  r01)
*/
void objCons101 (void) {
 Obj o;
	o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, r00);
	memVectorSet(o, 1, r01);
	r01 = o;
}

/* r03 => (cons r00  r03)
*/
void objCons303 (void) {
 Obj o;
	o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, r00);
	memVectorSet(o, 1, r03);
	r03 = o;
}

/* r00 = (cons pop() r00)
void objConsStack0 (void) {
 Obj o;
   o = memNewVector(TPAIR, 2);
	memVectorSet(o, 0, vmPop());
	memVectorSet(o, 1, r00);
	r00 = o;
}
*/

void objCons012 (void) {
   r00 = memNewVector(TPAIR, 2);
	memVectorSet(r00, 0, r01);
	memVectorSet(r00, 1, r02);
}

void objCons023 (void) {
   r00 = memNewVector(TPAIR, 2);
	memVectorSet(r00, 0, r02);
	memVectorSet(r00, 1, r03);
}

void objNewDoublyLinkedListNode (void) {
	r00 = memNewVector(TVECTOR, 3);
	memVectorSet(r00, 0, onull);
	memVectorSet(r00, 1, r00); /* Next */
	memVectorSet(r00, 2, r00); /* Prev */
}

void objNewVector (Num len) {
   r00 = memNewVector(TVECTOR, len);
}

/* Create uninitialized vector in r00 of length imm:r1
*/
void objNewVector01 (void) {
	DBBEG(" len="INT, (Int)r01);
   r00 = memNewVector(TVECTOR, (Length)r01);
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
	r00 = memNewVector(TPORT, 6);
	memVectorSet(r00, 0, r01); /* Descriptor or empty string. */
	memVectorSet(r00, 1, r02); /* Path or internet address string or string buffer. */
	memVectorSet(r00, 2, r03); /* Flags or port number or string index. */
	memVectorSet(r00, 3, r04); /* State: accepting, connecting, open, closed. */
	memVectorSet(r00, 4, ofalse); /* Push back or next available character. */
	memVectorSet(r00, 5, ofalse); /* Can hold a finalizer if you want. */
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

Num objIsPair   (Obj o) { return memIsObjectType(o, TPAIR); }
Num objIsVector (Obj o) { return memIsObjectType(o, TVECTOR); }
Num objIsSymbol (Obj o) { return memIsObjectType(o, TSYMBOL); }


Obj  car  (Obj o) { return memVectorObject(o, 0);}
Obj  cdr  (Obj o) { return memVectorObject(o, 1);}

Obj  caar (Obj o) { return car(car(o));}
Obj  cadr (Obj o) { return car(cdr(o));}

Obj  cdar (Obj o) { return cdr(car(o));}
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

/* Performs recursive eq? on object (can be list as well)
   WARNING: NOT TESTED
*/
Num objEqualP (Obj a, Obj b) {
 Num i;
	if (a != b) return 0;

	if (objIsPair(a) && objIsPair(b)) return (objEqualP(car(a), car(b)) && objEqualP(cdr(a), cdr(b)));

	if (objIsVector(a) &&
	    objIsVector(b) &&
	    (i=memObjectLength(a)) == memObjectLength(b)) {
		while (i--) if (!objEqualP(memVectorObject(a,i), memVectorObject(b,i))) return 0;
	}

	return 1;
}


/* Implementation of library function memq
    e  <=  e
   lst <=  (a e f)
   r00  => (e f) or ()
*/
Obj objMemq (Obj e, Obj lst) {
	for (; (onull != lst); lst = cdr(lst)) {
		if (e == car(lst)) {
			return lst;
		}
	}
	return onull;
}

/* Implementation of library function assq
    e  <=  e
   lst <=  ((d ...) (e ...) ...)
   r00  => (e ...) or ()
*/
Obj objAssq (Obj e, Obj lst) {
	for (; (onull != lst); lst = cdr(lst)) {
		if (objIsPair(car(lst)) && e == caar(lst)) {
			return car(lst);
		}
	}
	return onull;
}

/* Copies list to a new vector
  r00 <= List
  r01  = Clobbered
  r00  => Vector
*/
void objListToVector (void) {
 Num i=0, len;
	len = objListLength(r00);
	if (len) {
		r01=r00;
		r00 = memNewVector(TVECTOR, len); /* Create empty vector */
		while (i < len) { /* Stuff vector*/
			memVectorSet(r00, i++, car(r01));
			r01 = cdr(r01);
		}
	} else {
		r00 = onullvec;
	}
}

/* Count and push all list elements in r00 to current stack
   r00 <= {the list}
    r00 => ()
    ret => {list length}
*/
Num objCountListToStack (Obj o) {
 Num count=0;
	while (objIsPair(r00)) {
		++count;
		vmPush(car(r00));
		r00 = cdr(r00);
	}
	return count;
}

/* Pop c elements into a new list in r00.
   c       <=  {number of elements to pop into new list}
   RET r00  => {the new list}
*/
Obj objCountStackToList0 (Num c) {
	r00 = onull;
	while (c--) { r00 = objConsSTK0(); }
	return r00;
}



/*******************************************************************************
 Ordered_Sets
*******************************************************************************/

/* Copies ordered set seta adding element o if not in list,
   keeping decending order.  Returns new ordered set list in r00.

  seta <=  list
  o    <=  item
  r00   => new liset (... item ...)
*/
Obj objOrderedSetAdd0 (Obj seta, Obj o) {
 Num count=0;
 Num found=0;
	// Count and push list elements in r00 to stack.
	while (objIsPair(seta)) {
		r00 = car(seta);
		if (!found) {
			if (r00 < o) {++count; found=1; vmPush(o); }
			else if (r00 == o) { found=1; }
		}
		++count;
		vmPush(r00);
		seta = cdr(seta);
	}
	if (!found) {++count; vmPush(o); } // Must be smaller than all elements so push as new last element in ordered set

	return objCountStackToList0(count);
}

/* Copies list r00 removing element r01 if in list.  Keep decending order.
  r00  <=   (... item ...)
  r01  <=   item
  r00   =>  (... ...)
*/
Obj objOrderedSetSub0 (Obj set, Obj e) {
 Num count=0;
 Num found=0;
	// Count and push list elements in r00 to stack.
	while (objIsPair(set)) {
		r00 = car(set);
		if (!found && e == r00) {
			found=1;
		} else {
			++count;
			vmPush(r00);
		}
		set = cdr(set);
	}
	return objCountStackToList0(count);
}

/* Unions r00 and r01 into a new list.  Keep decending order.
  seta setb  <=   {the sets to union}
  ret r00     =>  {new set}
*/
Obj objOrderedSetUnion0 (Obj seta, Obj setb) {
 Num count=0;
 Obj a, b;

	// Push elements from each list in order
	while (seta != onull && setb != onull) {
		a = car(seta);
		b = car(setb);
		if (a == b)     { vmPush(a);  seta = cdr(seta);  setb = cdr(setb); }
		else if (a > b) { vmPush(a);  seta = cdr(seta); }
		else            { vmPush(b);  setb = cdr(setb); }
		++count;
	}

	// Might have ended up with an empty list, so push the rest of the other
	while (seta != onull) { vmPush(car(seta));  seta = cdr(seta);  ++count; }
	while (setb != onull) { vmPush(car(setb));  setb = cdr(setb);  ++count; }

	return objCountStackToList0(count);
}

/* Intersection r00 and r01 into a new list.  Keep decending order.
  r00  <=   {set}
  r01   =>  element to add
  r00   =>  {new set}
*/
void objOrderedSetIntersection001 (void) {
 Num count=0;
 Obj a, b;

	// Push elements from each list in order
	while (r00 != onull && r01 != onull) {
		a = car(r00);
		b = car(r01);
		if (a==b) { vmPush(a); r00 = cdr(r00); r01 = cdr(r01); ++count;}
		else if (a > b) { r00 = cdr(r00); }
		else  { r01 = cdr(r01); }
	}

	r00 = onull;

	while (count--) {
		r01 = vmPop();
		objCons010();
	}
}

/* Subtracts r01 from r00 into a new list.  Keep decending order.
  r00 <=  {set}
  r01  => {element to subtract}
  r00  => {new set}
*/
void objOrderedSetSubtract001 (void) {
 Num count=0;
 Obj a, b;

	// Push elements from each list in order
	while (r00 != onull && r01 != onull) {
		a = car(r00);
		b = car(r01);
		if (a==b) { r00 = cdr(r00); r01 = cdr(r01); }
		else if (a > b) { vmPush(a); r00 = cdr(r00); ++count;}
		else  { r01 = cdr(r01); }
	}

	while (r00 != onull) { vmPush(car(r00)); r00 = cdr(r00); ++count; }

	// Build new list.  r00 already null.
	while (count--) {
		r01 = vmPop();
		objCons010();
	}
}

Num objOrderedSetIsMember (Obj s, Obj e) {
	while (objIsPair(s)) {
		if (e == car(s)) return 1;
		s = cdr(s);
	}
	return 0;
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
		c = ((Chr*)o)[i];
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
	fwrite ("\"", 1, 1, stream);
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

void objWriteTypeVector (Obj o, FILE *stream) {
 Num i;
	fwrite("#(", 1, 2, stream);
	for (i=0; i<memObjectLength(o); i++) {
		if (i) fwrite (" ", 1, 1, stream);
		objWrite(((Obj*)o)[i], stream);
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

void objWriteTypePair (Obj o, FILE *stream) {
	fwrite ("(", 1, 1, stream);
	objWrite(car(o), stream);

	o = cdr(o);
	while (objIsPair(o)) {
		fwrite (" ", 1, 1, stream);
		objWrite(car(o), stream);
		o = cdr(o);
	}

	if (onull != o) {
		fwrite (" . ", 1, 3, stream);
		objWrite(o, stream);
	}
	fwrite (")", 1, 1, stream);
}


/* Serialize a stack object converting objects/pointers/immediates
   to basic strings.
*/
void objDisplayTypeStack (Obj o, FILE *stream) {
 Num i;
 Obj p;
	fprintf(stream, "#stack["HEX04" |", memVecStackLength(o)); /* Length */
	for (i=0; i<memVecStackLength(o); i++) {
		p = memVecStackObject(o, memVecStackLength(o)-i-1);
		fwrite(" ", 1, 1, stream);
		if (memIsObjectValid(p)) /* Live objects get a type description */
			objDisplayTypeDefault(p, stream);
		else  /* Immediate/pointers shown as signed hex */
			if ((Int)p < 0l) fprintf(stream, "-"HEX, labs((Int)p));
			else fprintf(stream, HEX, p);
	}
	fprintf(stream, "]");
/*
	for (i=0; i<memVecStackLength(o); i++) {
		p = memVecStackObject(o, memVecStackLength(o)-i-1);
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
		if ((s = memAddressString(o))) fprintf (stream, ":%s", s);

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
		ObjectWriteCallbacks[memObjectType(o)](o, stream);
	} else {
		fwrite("#<", 1, 2, stream);
		/* Display pointer/immediate as a signed HEX value */
		if ((Int)o < 0l) fprintf(stream, "-"HEX, labs((Int)o));
		else fprintf(stream, HEX, (Int)o);
		/* Dump the object description. */
		if ((s = memAddressString(o))) fprintf (stream, ":%s", s);
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
	objDisplayTypeRegister(TVECSTACK,  objDisplayTypeStack);
	objDisplayTypeRegister(TCODE,      vmDisplayTypeCode);

	objWriteTypeRegister(TINTRINSIC, objDisplayTypeIntrinsic);
	objWriteTypeRegister(TCHAR,      objWriteTypeChar);
	objWriteTypeRegister(TSYMBOL,    objDisplayTypeSymbol);
	objWriteTypeRegister(TSTRING,    objWriteTypeString);
	objWriteTypeRegister(TINTEGER,   objDisplayTypeInteger);
	objWriteTypeRegister(TPRIMITIVE, objDisplayTypePrimitive);
	objWriteTypeRegister(TPAIR,      objWriteTypePair);
	objWriteTypeRegister(TVECTOR,    objWriteTypeVector);
	objWriteTypeRegister(TVECSTACK,  objDisplayTypeStack);
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
		memTypeStringRegister(TINTRINSIC, (Str)"intrinsic");
		memTypeStringRegister(TCHAR, (Str)"chr");
		memTypeStringRegister(TSTRING, (Str)"str");
		memTypeStringRegister(TSYMBOL, (Str)"symb");
		memTypeStringRegister(TINTEGER, (Str)"int");
		memTypeStringRegister(TREAL, (Str)"real");
		memTypeStringRegister(TPRIMITIVE, (Str)"primitive");
		memTypeStringRegister(TPAIR, (Str)"pair");
		memTypeStringRegister(TVECTOR, (Str)"vector");
		memTypeStringRegister(TCLOSURE, (Str)"closure");
		//memTypeStringRegister(TCONTINUATION, (Str)"contin");
		memTypeStringRegister(TPORT, (Str)"port");
		//memTypeStringRegister(TSOCKET, (Str)"socket");
		memTypeStringRegister(TSYSCALL, (Str)"syscall");

		/* Intrinsic objects are defined by their static object pointer address.
		   Their display/write strings are also stored, even though their official
		   length is zero, using format string "%.2s".  */
		ofalse = memNewStatic(TINTRINSIC, 0); memAddressStringRegister(ofalse, (Str)"#f");
		otrue  = memNewStatic(TINTRINSIC, 0); memAddressStringRegister(otrue, (Str)"#t");
		onull  = memNewStatic(TINTRINSIC, 0); memAddressStringRegister(onull, (Str)"()");
		oeof   = memNewStatic(TINTRINSIC, 0); memAddressStringRegister(oeof, (Str)"#eof");
		onullvec = memNewStatic(TVECTOR, 0);  memAddressStringRegister(onullvec, (Str)"#()");
		onullstr = memNewStatic(TSTRING, 0);  memAddressStringRegister(onullstr, (Str)"\"\"");

		/* Table of character objects */
		DB("Creating vector of character constants");
		ocharacters = memNewStaticVector(TVECTOR, 256);
		for (n=0; n<256; n++) {
			r00 = memNewStatic(TCHAR, 1);
			*(Num*)r00 = n;
			memVectorSet(ocharacters, n, r00);
		}

		DB("Creating global symbol table and initial symbols");
		memRootSetAddressRegister(&osymbols); MEM_ADDRESS_REGISTER(&osymbols);

		osymbols = memNewVector(TVECTOR, HashTableSize);
		for (n=0; n<HashTableSize; n++) memVectorSet (osymbols, n, onull);

		objNewSymbolStatic("#f");           sfalse = r00;
		objNewSymbolStatic("#t");           strue = r00;
		objNewSymbolStatic("()");           snull = r00;
		objNewSymbolStatic("#eof");         seof = r00;

		objNewSymbolStatic("define");       sdefine = r00;
		objNewSymbolStatic("lambda");       slambda = r00;
		objNewSymbolStatic("macro");        smacro = r00;
		objNewSymbolStatic("quote");        squote = r00;
		objNewSymbolStatic("unquote");      sunquote = r00;
		objNewSymbolStatic("quasiquote");   squasiquote = r00;
		objNewSymbolStatic("unquote-splicing"); sunquotesplicing = r00;
		objNewSymbolStatic("begin");        sbegin = r00;
		objNewSymbolStatic("if");           sif = r00;
		objNewSymbolStatic("=>");           saif = r00;
		objNewSymbolStatic("cond");         scond = r00;
		objNewSymbolStatic("case");         scase = r00;
		objNewSymbolStatic("else");         selse = r00;
		objNewSymbolStatic("or");           sor = r00;
		objNewSymbolStatic("and");          sand = r00;
		objNewSymbolStatic("set!");         ssetb = r00;
		objNewSymbolStatic("vector-ref");   svectorref = r00;
		objNewSymbolStatic("vector-vector-ref"); svectorvectorref = r00;
		objNewSymbolStatic("vector-set!");  svectorsetb = r00;
		objNewSymbolStatic("vector-vector-set!"); svectorvectorsetb = r00;
		objNewSymbolStatic("vector-length");svectorlength = r00;
		objNewSymbolStatic("cons");         scons = r00;
		objNewSymbolStatic("car");          scar = r00;
		objNewSymbolStatic("cdr");          scdr = r00;
		objNewSymbolStatic("set-car!");     ssetcarb = r00;
		objNewSymbolStatic("set-cdr!");     ssetcdrb = r00;
		objNewSymbolStatic("memv");         smemv = r00;
		objNewSymbolStatic("procedure?");   sprocedurep = r00;
		objNewSymbolStatic("null?");        snullp = r00;
		objNewSymbolStatic("pair?");        spairp = r00;
		objNewSymbolStatic("vector?");      svectorp = r00;
		objNewSymbolStatic("char?");        scharp = r00;
		objNewSymbolStatic("string?");      sstringp = r00;
		objNewSymbolStatic("integer?");     sintegerp = r00;
		objNewSymbolStatic("symbol?");      ssymbolp = r00;
		objNewSymbolStatic("port?");        sportp = r00;
		objNewSymbolStatic("append");       sappend = r00;
		objNewSymbolStatic("eof-object?");  seofobjectp = r00;
		objNewSymbolStatic("thread");       sthread = r00;
		objNewSymbolStatic("let");          slet = r00;
		objNewSymbolStatic("let*");         sletstar = r00;
		objNewSymbolStatic("letrec");       sletrec = r00;
		objNewSymbolStatic("eval");         seval = r00;
		objNewSymbolStatic("apply");        sapply = r00;
		objNewSymbolStatic("call/cc");      scallcc = r00;
		objNewSymbolStatic("syntax-rules"); ssyntaxrules = r00;
		objNewSymbolStatic("not");          snot = r00;
		objNewSymbolStatic("+");            sadd = r00;
		objNewSymbolStatic("-");            ssub = r00;
		objNewSymbolStatic("*");            smul = r00;
		objNewSymbolStatic("/");            sdiv = r00;
		objNewSymbolStatic("logand");       slogand = r00;
		objNewSymbolStatic("rem");          srem = r00;
		objNewSymbolStatic("running");      srunning = r00;
		objNewSymbolStatic("ready");        sready = r00;
		objNewSymbolStatic("sleeping");     ssleeping = r00;
		objNewSymbolStatic("blocked");      sblocked = r00;
		objNewSymbolStatic("dead");         sdead = r00;
		objNewSymbolStatic("semaphore");    ssemaphore = r00;
		objNewSymbolStatic("openblocked");  sopenblocked = r00;
		objNewSymbolStatic("readblocked");  sreadblocked = r00;
		objNewSymbolStatic("writeblocked"); swriteblocked = r00;
		objNewSymbolStatic("accepting");    saccepting = r00;
		objNewSymbolStatic("connecting");   sconnecting = r00;
		objNewSymbolStatic("open");         sopen = r00;
		objNewSymbolStatic("closed");       sclosed = r00;
		objNewSymbolStatic("SIGNALHANDLERS"); ssignalhandlers = r00;

		DB("Creating vector of integer constants");
		ointegers = memNewStaticVector(TVECTOR, 2048);
		for (i=-1024l; i<=1023l; ++i) {
			r00 = memNewStatic(TINTEGER, sizeof(Int));
			*(Int*)r00 = i;
			memVectorSet(ointegers, (Num)i+1024, r00);
		}

		DB("Registering static pointer description strings");
		MEM_ADDRESS_REGISTER(objCopyInteger);
		MEM_ADDRESS_REGISTER(objCopyReal);
		MEM_ADDRESS_REGISTER(objCons010);
		MEM_ADDRESS_REGISTER(objCons101);
		MEM_ADDRESS_REGISTER(objCons303);
		MEM_ADDRESS_REGISTER(objCons012);
		MEM_ADDRESS_REGISTER(objCons023);
		MEM_ADDRESS_REGISTER(objNewVector01);
		MEM_ADDRESS_REGISTER(objListToVector);

		odebug = ofalse;

	} else {
		DB("Module already activated");
	}

	DBEND();
}

#undef DB_DESC
#undef DEBUG
