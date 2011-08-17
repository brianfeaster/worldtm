#ifndef _VM_H
#define _VM_H

#include <stdio.h>
#include "globals.h"

#define TCODE 0x87l

/* Global virtual machine register aliases */
#define rblocked   r10 /* WSCM: I/O and Semaphore blocked threads */
#define rthreads   r11 /* WSCM: Thread vector */
#define rsleeping  r12 /* WSCM: Sleeping thread */
#define rrunning   r13 /* WSCM: Current thread */
#define rready     r14 /* WSCM: Thread list */

#define rexpr      r15 /* WSCM: Expression being compiled */
#define rasmstack  r16 /* VM: Opcode stack where machine code is emitted */
#define rsymbols   r17 /* OBJ: Symbol table used by scanner and OS */

#define rtge       r18 /* WSCM: Global environment */
#define rretenv    r19 /* VM: Caller's env */
#define rretip     r1a /* VM: Caller's ip */
#define rretcode   r1b /* VM: Caller's code block */
#define renv       r1c /* VM: Current running thread's environment */
#define rip        r1d /* VM: Current running program instruction pointer */
#define rcode      r1e /* VM: Currently running code object */

#define rstack     r1f /* Register alias: Global stack used by VM */


/* Registers:  These make up the root set for the garbage collector.  All
   computation should use only these as variables since a GC could move an 
   objects location in memory at any time.
*/
extern Obj r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7,
           r8,  r9,  ra,  rb,  rc,  rd,  re,  rf,
           r10, r11, r12, r13, r14, r15, r16, r17,
           r18, r19, r1a, r1b, r1c, r1d, r1e, r1f;



void vmPush (Obj o);

Obj vmPop (void);



/* Virtual machine instruction opcodes.  Really just C goto addresses.
*/
extern void *NOP,
     *MVI0, *MVI1, *MVI2, *MVI3, *MVI4, *MVI5, *MVI6, *MVI7,
     *MV01, *MV02, *MV03, *MV04, *MV07, *MV01E, *MV10, *MV13,
     *MV20, *MV23, *MV30, *MV51C, *MV518,
     *MV50, *MV1C0, *MV1C18,
     *MV61, *MV72,
     *LDI00, *LDI02, *LDI05, *LDI01C, *LDI11, *LDI20, *LDI22, *LDI50, *LDI1C0, *LDI11C,
     *LDI40,
     *LD012,
     *STI01, *STI01C, *STI21, *STI20, *STI30, *STI40, *STI50,
     *ST012, *ST201,
     *PUSH0, *PUSH1, *PUSH2, *PUSH3, *PUSH4, *PUSH5, *PUSH7, *PUSH19,
     *PUSH1A, *PUSH1B,
     *POP0,  *POP1,  *POP2,  *POP3,  *POP4,  *POP7, *POP19, *POP1A,  *POP1B,
     *ADDI0, *ADDI1, *ADD10, *MUL10,
     *BLTI1,
     *BEQI0, *BEQI1, *BEQI7, *BNEI0, *BNEI1, *BNEI5, *BRTI0, *BNTI0, *BRA,
     *J0, *J2, *JAL0, *JAL2, *RET,
     *SYSI, *SYS0, *QUIT;

extern Int vmInterrupt;

void vmSigAlarmReset (void);

void vmRun (void);
void vmInitialize (Func intHandler, void(*vmObjDumper)(Obj, FILE*));
void vmDebugDumpCode (Obj c, FILE *stream);

#endif
