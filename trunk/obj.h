#ifndef _OBJ_H
#define _OBJ_H

#include "asm.h"

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
       snot, sadd, ssub, smul, sdiv, slogand, characters, staticIntegers,
       signalhandlers;

extern Num wscmDebug;

extern const Int HashTableSize;

Num hashpjw (Str s, Num len);
Num objListLength (Obj o);
Num objDoublyLinkedListLength (Obj o);
void objListToVector (void);
/* Object creators.  All new objects returned in r0.
*/
void objNewInt      (Int x);
void objCopyInteger (void);

void objNewReal     (Real x);
void objCopyReal    (void);

void objNewString  (Str str, Num len);
void objCopyString  (void);
void objNewSymbol  (Str str, Num len);
void objNewSyscall (Func f);
void objCons12   (void); /* New pair using r1 and r2 */
void objCons23   (void);
void objNewVector  (Num len);
void objNewVector1 ();
void objNewClosure1Env (void);
//void objNewSocket (void);
void objNewPort (void);

Num objIsPair (Obj o);

void objDump (Obj a, FILE *stream);

Obj car (Obj o);
Obj caar (Obj o);
Obj cdar (Obj o);

Obj cdr (Obj o);
Obj cadr (Obj o);
Obj cddr (Obj o);

void push (Obj o);
Obj  pop  (void);



void objInitialize (Func scheduler);



#endif
