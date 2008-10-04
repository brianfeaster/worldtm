#ifndef _OBJ_H
#define _OBJ_H

#include "asm.h"

/* Object constants.
*/
extern Obj null, nullvec, nullstr, false, true, eof,
       srem, srunning, sready, ssleeping, sblocked, sdead, ssemaphore,
       sopenblocked, sreadblocked, swriteblocked,
       saccepting, sconnecting, sopen, sclosed,
       sdefine, slambda, squote, sunquote, squasiquote, sunquotesplicing,
       sbegin, sif, sor, sand, ssetb,
       svectorref, svectorvectorref, svectorvectorsetb, svectorsetb,
       svectorlength,
       scons, scar, scdr, ssetcarb, ssetcdrb,
       snullp, spairp, svectorp, sstringp, sportp, sappend,
       seofobjectp, sthread, slet, sletrec, seval, sapply, scallcc,
       ssyntaxrules, seof,
       snot, sadd, ssub, smul, sdiv, characters;


unsigned hashpjw (char *s, int len);
int  objListLength (Obj o);
int  objDoublyLinkedListLength (Obj o);
void objListToVector (void);
/* Object creators.  All new objects returned in r0.
*/
void objNewLong     (u64 x);
void objNewInteger  (s32 x);
void objCopyInteger (void);

void objNewReal     (r32 x);
void objCopyReal    (void);

void objNewString  (char *str, int len);
void objCopyString  (void);
void objNewSymbol  (char *str, int len);
void objNewSymbol_static (char *s, int len);
void objNewSyscall (fp f);
void objCons12   (void); /* New pair using r1 and r2 */
void objCons23   (void);
void objNewVector  (int len);
void objNewVector1 ();
void objNewClosure1Env (void);
//void objNewSocket (void);
void objNewPort (void);

void objDump (Obj a, int fd);

Obj car (Obj o);
Obj caar (Obj o);
Obj cdar (Obj o);

Obj cdr (Obj o);
Obj cadr (Obj o);
Obj cddr (Obj o);

void push (Obj o);
Obj  pop  (void);



void objInitialize (fp intHandler);



#endif
