#ifndef _ASM_H
#define _ASM_H

#include "vm.h"

/* A few assembly opcodes
*/
void *LABEL;/* Branch symbol. */
void *ADDR; /* Branch opcode offset symbol. */
void *END;  /* Sentinel for assembly programs.  Must be last assembly opcode. */

void asmAsm (Obj o,...);

/* Legacy...will be phased out eventually. */
#define asm(o) asmAsm((Obj)(o), END)

void asmCompile (int opcodeStart);
void asmNewCode (void);
void asmInitialize (fp intHandler, fp preGC, fp postGC, fp1 objDumper);

#endif
