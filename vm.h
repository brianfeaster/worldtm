#ifndef _VM_H
#define _VM_H

#include <stdio.h>
#include "globals.h"

#define TCODE 0xefl

/* Global virtual machine register aliases */
#define rblocked   r10 /* WSCM: I/O and Semaphore blocked threads */
#define rthreads   r11 /* WSCM: Thread vector */
#define rsleeping  r12 /* WSCM: Sleeping thread */
#define rrunning   r13 /* WSCM: Current thread */
#define rready     r14 /* WSCM: Thread list */

#define rexpr      r15 /* WSCM: Expression being compiled */
#define rasmstack  r16 /* ASM,COMP: Opcode stack where machine code is emitted (ribloc in new compiler) */
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
extern void *vmNOP,
     *vmMVI0, *vmMVI1, *vmMVI2, *vmMVI3, *vmMVI4, *vmMVI5, *vmMVI6, *vmMVI7,
     *vmMV01, *vmMV02, *vmMV03, *vmMV04, *vmMV07, *vmMV01E, *vmMV10, *vmMV13,
     *vmMV20, *vmMV23, *vmMV30, *vmMV51C, *vmMV518,
     *vmMV50, *vmMV1C0, *vmMV1C18,
     *vmMV61, *vmMV72,
     *vmLDI00, *vmLDI02, *vmLDI01C, *vmLDI11, *vmLDI20, *vmLDI22, *vmLDI50, *vmLDI1C0, *vmLDI11C,
     *vmLD012,
     *vmSTI01, *vmSTI01C, *vmSTI21, *vmSTI20, *vmSTI30, *vmSTI40, *vmSTI50,
     *vmST012, *vmST201,
     *vmPUSH0, *vmPUSH1, *vmPUSH2, *vmPUSH3, *vmPUSH4, *vmPUSH5, *vmPUSH7, *vmPUSH19,
     *vmPUSH1A, *vmPUSH1B,
     *vmPOP0,  *vmPOP1,  *vmPOP2,  *vmPOP3,  *vmPOP4,  *vmPOP7, *vmPOP19, *vmPOP1A,  *vmPOP1B,
     *vmADDI0, *vmADDI1, *vmADD10, *vmMUL10,
     *vmBLTI1,
     *vmBEQI0, *vmBEQI1, *vmBEQI7, *vmBNEI0, *vmBNEI1, *vmBNEI5, *vmBRTI0, *vmBNTI0, *vmBRA,
     *vmJ0, *vmJ2, *vmJAL0, *vmJAL2, *vmRET,
     *vmSYSI, *vmSYS0, *vmQUIT;

extern Int vmInterrupt;

void vmSigAlarmReset (void);

void vmRun (void);
void vmInitialize (Func intHandler, void(*vmObjDumper)(Obj, FILE*));
void vmDebugDumpCode (Obj c, FILE *stream);

#endif
