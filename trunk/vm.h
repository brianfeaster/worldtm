#ifndef _VM_H
#define _VM_H

#include <stdio.h>
#include "globals.h"

/* Global virtual machine register aliases */
#define rtge       r8 /* Global Environment */
#define rretenv    r9 /* Caller's env */
#define rretip     ra /* Caller's ip */
#define rretcode   rb /* Caller's code block */
#define renv       rc /* Current running thread's environment */
#define rip        rd /* Current running program instruction pointer */
#define rcode      re /* Currently running code object */
#define rstack     rf /* Global stack created and used by VM module */

#define TCODE 0xefl

/* Registers:  These make up the root set for the garbage collector.  All
   computation should use only these as variables since a GC could move an 
   objects location in memory at any time.
*/
extern Obj r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7,
           r8,  r9,  ra,  rb,  rc,  rd,  re,  rf;


void vmPush (Obj o);
Obj vmPop (void);


/* Virtual machine instruction opcodes.  Really just C goto addresses.
*/
extern void *vmNOP,
     *vmMVI0, *vmMVI1, *vmMVI2, *vmMVI3, *vmMVI4, *vmMVI5, *vmMVI6, *vmMVI7,
     *vmMV01, *vmMV02, *vmMV03, *vmMV04, *vmMV07, *vmMV0E,
     *vmMV10, *vmMV13, *vmMV20, *vmMV23, *vmMV30,
     *vmMV50, *vmMV58, *vmMV5C, *vmMV61, *vmMV72, *vmMVC0, *vmMVC5, *vmMVC8,
     *vmLDI00, *vmLDI02, *vmLDI0C, *vmLDI11, *vmLDI20, *vmLDI22,
     *vmLDI50, *vmLDIC0, *vmLDI1C,
     *vmLD012,
     *vmSTI01, *vmSTI05, *vmSTI0C, *vmSTI21, *vmSTI20, *vmSTI30, *vmSTI40, *vmSTI50,
     *vmST012, *vmST201,
     *vmPUSH0, *vmPUSH1, *vmPUSH2, *vmPUSH3, *vmPUSH4, *vmPUSH5, *vmPUSH7,
     *vmPUSH9, *vmPUSHA, *vmPUSHB, *vmPUSHC,
     *vmPOP0,  *vmPOP1,  *vmPOP2,  *vmPOP3,  *vmPOP4,  *vmPOP5, *vmPOP7, *vmPOP9, *vmPOPA,  *vmPOPB, *vmPOPC,
     *vmADDI0, *vmADDI1, *vmADDI2, *vmADD10,
     *vmMUL10,
     *vmBLTI1,
     *vmBEQI0, *vmBEQI1, *vmBEQI7,
     *vmBNEI0, *vmBNEI1, *vmBNEI2, *vmBNEI5,
     *vmBRTI0, *vmBNTI0, *vmBRA,
     *vmJMP0, *vmJMP2,
     *vmJAL0, *vmJAL2, *vmRET,
     *vmSYSI, *vmSYS0, *vmQUIT;

extern Int vmInterrupt;

void vmRun (void);
void vmInitialize (Func intHandler, void(*vmObjDumper)(Obj, FILE*));
void vmDebugDumpCode (Obj c, FILE *stream);

#endif
