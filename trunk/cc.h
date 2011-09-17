#ifndef _CC_H
#define _CC_H

#include "globals.h"

/* Register values */
extern Obj R0;
extern Obj R1;
extern Obj R2;
extern Obj R3;
extern Obj R4;
extern Obj R5;
extern Obj R6;
extern Obj R7;
extern Obj R1C;

extern Obj MV;
extern Obj MVI;
extern Obj LDI;
extern Obj PUSH;
extern Obj POP;
extern Obj ADDI;
extern Obj BNEI;
extern Obj BEQI;
extern Obj BRTI;
extern Obj BRA;
extern Obj SYSI;
extern Obj NOP;
extern Obj QUIT;
extern Obj LABEL;
extern Obj END;

/***************************************
 Igraph_and_iblocks
***************************************/
Obj ccIBlock (Num id);
void ccIBlockSetDefault (Num parentid, Num childid);
void ccIBlockSetConditional (Num parentid, Num childid);
Num ccGenerateNewIBlock (Num icodeSize);
Num ccNewDefaultIBlock (Num parentID, Num icodeCount);
Num ccNewConditionalIBlock (Num parentID, Num icodeCount);
void ccMV (Obj ra, Obj rb);
void ccMVI (Obj ra, Obj o);
void ccLDI (Obj ra, Obj rb, Obj o);
void ccPUSH (Obj o);
void ccPOP ();
void ccADDI (Obj ra, Obj o);
void ccBNEI (Obj ra, Obj imm, Obj o);
void ccBEQI (Obj ra, Obj imm, Obj o);
void ccBRTI (Obj ra, Obj imm, Obj o);
void ccBRA (Obj o);
void ccSYSI (Obj o);
void ccNOP ();
void ccQUIT ();
void ccResetIGraph (void);
void ccAssembleIGraph (void);


/***************************************
 ASM
***************************************/
void ccAsmInit (void);
Num ccAsmLabelNew();
void ccAsmAsm (Obj f, ...);
#define ccAsm(...) ccAsmAsm(__VA_ARGS__, END)


/***************************************
 Init
***************************************/
void ccInitialize (void);


#endif
