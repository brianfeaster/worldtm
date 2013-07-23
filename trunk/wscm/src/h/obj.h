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
void objNewSymbolR5R6 (void); /* Like objNewSymbol only 'str'/'len' are in r5/r6 */
void objNewSymbolStatic (char *s);
void objNewSyscall   (Func f);
void objNewPrimitive (Func f);
Obj  objCons   (Obj a, Obj b);
void objCons010(void);
void objCons101(void);
void objCons303 (void);
void objConsStack0 (void);
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

Obj objIntegerToChar (Num i);

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
void objDisplayTypeRegister (Type type, Func2ObjFile serializer);
void objWriteTypeRegister   (Type type, Func2ObjFile serializer);
void objDisplay (Obj a, FILE *stream);
void objWrite   (Obj a, FILE *stream);

void objInitialize (void);

#endif
