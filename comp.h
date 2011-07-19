#ifndef _COMP_H
#define _COMP_H

/* Compiler flags passed by comp module functions.
*/
static const Num TAILCALL  = 0x00010000;
static const Num NODEFINES = 0x00020000;
/*
static const Num R8 =        0x00000100;
static const Num R7 =        0x00000080;
static const Num R6 =        0x00000040;
static const Num R5 =        0x00000020;
static const Num R4 =        0x00000010;
static const Num R3 =        0x00000008;
static const Num R2 =        0x00000004;
static const Num R1 =        0x00000002;
static const Num R0 =        0x00000001;
*/

void compEval (Num flags);
void compSelfEvaluating (void);
void compVariableReference (Num flags);
void compDefine (Num flags);
void compSetb (Num flags);
void compLambdaBody (Num flags);
void compLambda (Num flags);
void compAdd (Num flags);
void compVerifyVectorRef (void);
void compVerifyVectorSetB (void);
void compVectorRef (Num flags);
void compVectorVectorRef (Num flags);
void compCons (Num flags);
void compBegin (Num flags);
void compQuote (void);
void compIf (Num flags);
void compSyntaxRules (void);
void compCombination (Num flags);
Num compExpression (Num flags);
Num compCompile (void);

#endif
