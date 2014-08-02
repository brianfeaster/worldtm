#ifndef _OBJ_H
#define _OBJ_H
#include "globals.h"
#include "mem.h"


/* Useful objects
*/
extern Obj symbols, debug;


/* Scheme object types used by the scanner and obj module.  A byte with
   highest bit signifying the base type which is either a "vector" or "array"
   object.
*/
#define TINTRINSIC     0x00l
#define TCHAR          0x01l
#define TSYMBOL        0x02l
#define TSTRING        0x03l
#define TINTEGER       0x04l
#define TREAL          0x05l
#define TPRIMITIVE     0x06l

#define TVECTOR        0x80l
#define TPAIR          0x81l
#define TCLOSURE       0x82l
//#define TCONTINUATION  0x83l
#define TPORT          0x83l
//#define TSOCKET        0x85l
#define TSYSCALL       0x84l


/* Objects */
extern Obj onull, onullvec, onullstr, ofalse, otrue, oeof; /* Intrinsic objects */
extern Obj ocharacters, ointegers; /* Intrinsic aggregates */
extern Obj odebug; /* Mutable */
extern Obj osymbols; /* Intrinsic mutable aggregates */


/* Symbol objects
*/
extern Obj snull, sfalse, strue, seof, 
       srem, srunning, sready, ssleeping, sblocked, sdead, ssemaphore,
       sopenblocked, sreadblocked, swriteblocked,
       saccepting, sconnecting, sopen, sclosed,
       sdefine, slambda, smacro, squote, sunquote, squasiquote, sunquotesplicing,
       sbegin, sif, saif, scond, scase, selse, sor, sand, ssetb,
       svectorref, svectorvectorref, svectorvectorsetb, svectorsetb,
       svectorlength,
       scons, scar, scdr, ssetcarb, ssetcdrb,
       smemv, sprocedurep, snullp, spairp, svectorp, scharp, sstringp, sintegerp, ssymbolp, sportp, sappend,
       seofobjectp, sthread, slet, sletstar, sletrec, seval, sapply, scallcc,
       ssyntaxrules, //seof,
       snot, sadd, ssub, smul, sdiv, slogand,
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
void objNewStringString  (Str str1, Num len1, Str str2, Num len2);
void objCopyString  (void);
void objNewSymbol  (Str str, Num len);
void objNewSymbolR05R06 (void); /* Like objNewSymbol only 'str'/'len' are in r05/r06 */
void objNewSymbolStatic (char *s);
void objNewSyscall   (Func f);
void objNewPrimitive (Func f);

Obj  objCons    (Obj a, Obj b); // r00 <= (cons a b)
Obj objConsSTK0 (void);         // return => (cons (pop) r00) 
void objCons010 (void);         // r00 <= (cons r01  r00)
void objCons101 (void);         // r01 <= (cons r00  r01)
void objCons303 (void);         // r03 <= (cons r00  r03)
void objCons012 (void);         // r00 <= (cons r01  r02)
void objCons023 (void);         // r00 <= (cons r02  r03)

void objNewDoublyLinkedListNode (void); /* Doubly linked list node.  #(item prev next) */
void objNewVector  (Num len);
void objNewVector01 ();

void objNewPort (void);
int  objPortDescriptor (Obj p);
Obj  objPortState (Obj p);

Obj objIntegerToChar (Num i);

/* Object operations
 */
Num objIsPair   (Obj o);
Num objIsVector (Obj o);
Num objIsSymbol (Obj o);

Obj car  (Obj o);
Obj cdr  (Obj o);

Obj caar (Obj o);
Obj cadr (Obj o);

Obj cdar (Obj o);
Obj cddr (Obj o);


Num  objListLength (Obj o);
Num  objDoublyLinkedListLength (Obj o);

Obj  objDoublyLinkedListNext   (Obj node);
Obj  objDoublyLinkedListPrev   (Obj node);
void objDoublyLinkedListAdd    (Obj lst, Obj node);
void objDoublyLinkedListInsert (Obj lst, Obj node);

void objListToVector (void);

/* Predicates
*/
Num objEqualP (Obj a, Obj b); // WARNING: NOT TESTED

Obj objMemq (Obj e, Obj lst);
Obj objAssq (Obj e, Obj lst);

/* Count and push all list elements in o to current stack
   r00 ends up being null (or first non-pair cdr)
*/
Num objCountListToStack (Obj o);

/* Pop c elements into a new list in r00.
   RET, r00 => {new list}
*/
Obj objCountStackToList0 (Num c);

/* Copy decending ordered set in r00 adding immediate integer r01.
   Really a non-duplicate scheme list.
 */
Obj objOrderedSetAdd0 (Obj set, Obj o);
Obj objOrderedSetSub0 (Obj set, Obj e);
Obj objOrderedSetUnion0 (Obj seta, Obj setb);
void objOrderedSetIntersection001 (void);
void objOrderedSetSubtract001 (void);
Num objOrderedSetIsMember (Obj s, Obj e);

/* Module
*/
void objDisplayTypeRegister (Type type, Func2ObjFile serializer);
void objWriteTypeRegister   (Type type, Func2ObjFile serializer);
void objDisplay (Obj a, FILE *stream);
void objWrite   (Obj a, FILE *stream);

void objInitialize (void);

#endif
