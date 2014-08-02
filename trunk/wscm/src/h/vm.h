#ifndef _VM_H
#define _VM_H

#include <stdio.h>
#include "globals.h"


/* Object registers:  These make up the root set for the garbage collector.
   All scheme objects should only ever be assigned to these variables since
   a GC moves objects around.
*/
extern Obj r00, r01, r02, r03, r04, r05, r06, r07, r08,
           r09, // TGE global environment
           r0a, // environment link
           r0b, // environment
           r0c, // code link
           r0d, // code 
           r0e, // stack of integers
           r0f; // stack of objects

/* Immediate/Integer registers (not garbage collected)
*/
extern Obj r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r1a, r1b,
           r1c, // code index link
           r1d, // code index/ptr
           r1e, // stack of integers index/ptr
           r1f; // stack of objects index/ptr

/* Register aliases
 */
#define rtge      r09 /* Global Environment */
#define renvlink  r0a /* Linked Local environment */
#define renv      r0b /* Local environment */
#define rcodelink r0c /* Linked code object */
#define rcode     r0d /* Currently code object */
#define rdstack   r0e /* Data stack.  Only $1? and integer constants are pushed/popped. */
#define rstack    r0f /* Object stack.  Only $0? are pushed/popped. */

#define riplink   r1c /* Linked code instruction pointer index */
#define rip       r1d /* Code instruction pointer or index into the code object */
#define rdstackp  r1e /* Data stack pointer.  Synced as the stack's pointer when the VM is inerrupted. */
#define rstackp   r1f /* Object stack pointer.  Synced as the stack's pointer when the VM is interrupted. */


/* Object types used by this module
*/
#define TCODE 0xefl


/* Stack calls
*/
void vmPush (Obj o);
Obj vmPop (void);

void vmAryPush (Obj o);
Obj vmAryPop (void);


/* Opcodes.  Create all the global external declarations for the opcode goto label pointers.
   VMOP's parameter o is ignored as it is only required for the serializer.
    void *vmNOP;
    void *vmMV_R00_I;
    void *vmPUSH_R01;
    ...
*/
#define _
#define VMOP(op,d,n,i,o) VMOP_(op, _##d, _##n, _##i)
#define VMOP_(op,d,n,i) VMOP__(op, d, n, i)
#define VMOP__(op,d,n,i) VMOP___(vm##op##d##n##i)
#define VMOP___(op) extern void *op;
// Load and transform the opcode definitions
#include "op.h"
// Cleanup the unneeded macros
#undef VMOP___
#undef VMOP__
#undef VMOP_
#undef VMOP
#undef _

extern Int vmInterrupt;

void vmRun (void);
void vmInitialize (Func intHandler, Func2ObjFile vmObjDumper);
void vmDisplayTypeCode (Obj c, FILE *stream);
void vmPrintRegisters (FILE *stream);

#endif
