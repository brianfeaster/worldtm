#ifndef _VM_H
#define _VM_H

#include "mem.h"

/* Virtual machine instruction opcodes.  Really just C goto addresses.
*/
void *NOP,
     *MVI0, *MVI1, *MVI2, *MVI3, *MVI4, *MVI5, *MVI6, *MVI7,
     *MV01, *MV02, *MV03, *MV04, *MV07, *MV016, *MV01C, *MV10, *MV13, *MV116,
     *MV20, *MV23, *MV30, *MV416,
     *MV50, *MV1516, *MV160, *MV162, *MV164, *MV1615,
     *LDI00, *LDI02, *LDI016, *LDI11, *LDI20, *LDI22, *LDI50, *LDI160, *LDI116,
     *LDI40, *LDI1616,
     *LD012,
     *STI01, *STI016, *STI21, *STI20, *STI30, *STI40, *STI50,
     *ST012, *ST201,
     *PUSH0, *PUSH1, *PUSH2, *PUSH3, *PUSH4, *PUSH7, *PUSH15, *PUSH16,
     *PUSH1D, *PUSH1E,
     *POP0,  *POP1,  *POP2,  *POP3,  *POP4,  *POP7, *POP15, *POP1D,  *POP1E,
     *ADDI0, *ADDI1, *ADD10, *MUL10,
     *BLTI1,
     *BEQI0, *BEQI1, *BEQI7, *BEQI0, *BNEI0, *BNEI1, *BRTI0, *BNTI0, *BRA,
     *J0, *J2, *JAL0, *JAL2, *RET,
     *SYSI, *SYS0, *QUIT;

extern int interrupt;

void vmSigAlarmReset (void);

void vmRun (void);
void vmInitialize (Func intHandler, Func preGC, Func postGC, void(*vmObjDumper)(Obj, FILE*));
void vmDebugDumpCode (Obj c, FILE *stream);

#endif
