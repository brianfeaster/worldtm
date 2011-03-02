#ifndef _ASM_H
#define _ASM_H

#include "vm.h"

/* A few assembly opcodes. */
Obj LABEL;/* Branch symbol. */
Obj ADDR; /* Branch opcode offset symbol. */
Obj END;  /* Sentinel for assembly programs.  Must be last assembly opcode. */

/* Push passed opcodes to assembly stack.  The final opcode must be END. */
#define asm(o) asmAsm((Obj)(o), END)
void asmAsm (Obj o,...);

/* Assemble code in the asmstack inline.  For now it just takes care of
   'label' and 'address' assembly opcodes.  They refer to branch locations
   and branch points in the code. */ 
void asmCompileAsmstack (Num opcodeStart);

/* Create code object from stack of opcodes.  Since stack and asmstack
   objects are really just vectors, it's just a matter of copying all the
   objects from the stack over to the asmstack vector.  Creates code object
   in r0 based on assembly stack (r1a). */
void asmNewCode (void);

void asmInitialize (Func scheduler, Func preGC, Func postGC, void(*objDumper)(Obj, FILE*));

#endif
