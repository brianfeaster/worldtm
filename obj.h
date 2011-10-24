#ifndef _OBJ_H
#define _OBJ_H
#include "globals.h"


/* Rootset objects
*/
extern Obj rsymbols, rdebug;


/* Scheme object types used by the scanner and obj module.  A byte with
   highest bit signifying the base type which is either a "vector" or "array"
   object.
*/
#define TFALSE         0x00l
#define TTRUE          0x01l
#define TNULL          0x02l
#define TNULLVEC       0x03l
#define TNULLSTR       0x04l
#define TEOF           0x05l
#define TCHAR          0x06l
#define TSTRING        0x07l
#define TSYMBOL        0x08l
#define TINTEGER       0x09l
#define TREAL          0x0al

#define TPAIR          0x80l
#define TVECTOR        0x81l
#define TCLOSURE       0x82l
#define TCONTINUATION  0x83l
#define TPORT          0x84l
#define TSOCKET        0x85l
#define TSYSCALL       0x86l


/* Object constants.
*/
extern Obj null, nullvec, nullstr, false, true, eof,
       srem, srunning, sready, ssleeping, sblocked, sdead, ssemaphore,
       sopenblocked, sreadblocked, swriteblocked,
       saccepting, sconnecting, sopen, sclosed,
       sdefine, slambda, smacro, squote, sunquote, squasiquote, sunquotesplicing,
       sbegin, sif, saif, scond, selse, sor, sand, ssetb,
       svectorref, svectorvectorref, svectorvectorsetb, svectorsetb,
       svectorlength,
       scons, scar, scdr, ssetcarb, ssetcdrb,
       sprocedurep, snullp, spairp, svectorp, sstringp, sintegerp, ssymbolp, sportp, sappend,
       seofobjectp, sthread, slet, sletrec, seval, sapply, scallcc,
       ssyntaxrules, seof,
       snot, sadd, ssub, smul, sdiv, slogand, scharacters, staticIntegers,
       ssignalhandlers;

extern Num wscmDebug;

extern const Num HashTableSize;

Num hashpjw (Str s, Num len);

/* Object creators.  All new objects returned in r0.
*/
void objNewInt      (Int x);
void objCopyInteger (void);

void objNewReal     (Real x);
void objCopyReal    (void);

void objNewString  (Str str, Num len);
void objCopyString  (void);
void objNewSymbol  (Str str, Num len);
void objNewSymbolR5R6 (void); /* Like objNewSymbol only 'str'/'len' are in r5/r6 */
void objNewSymbolStatic (char *s);
void objNewSyscall (Func f);
Obj  objCons   (Obj a, Obj b);
void objCons101(void);
void objCons303 (void);
void objCons01 (void);
void objCons10 (void); /* New pair using r1 and r0 */
void objCons12 (void); /* New pair using r1 and r2 */
void objCons23 (void);
void objNewDoublyLinkedListNode (void); /* Doubly linked list node.  #(item prev next) */
void objNewVector  (Num len);
void objNewVector1 ();

void objNewPort (void);
int objPortDescriptor (Obj p);
Obj objPortState (Obj p);

/* Object operations
 */
Num objIsPair   (Obj o);
Num objIsSymbol (Obj o);

Obj car (Obj o);
Obj caar (Obj o);
Obj cdar (Obj o);

Obj cdr (Obj o);
Obj cadr (Obj o);
Obj cddr (Obj o);


Num  objListLength (Obj o);
Num  objDoublyLinkedListLength (Obj o);

Obj  objDoublyLinkedListNext   (Obj node);
Obj  objDoublyLinkedListPrev   (Obj node);
void objDoublyLinkedListAdd    (Obj lst, Obj node);
void objDoublyLinkedListInsert (Obj lst, Obj node);

void objListToVector (void);

/* Module
*/
void objDump (Obj a, FILE *stream);

void objInitialize (void);

#endif
