#ifndef _COMP_H
#define _COMP_H

void compEval (u32 flags);
void compSelfEvaluating (void);
void compVariableReference (void);
void compDefine (u32 flags);
void compSetb (u32 flags);
void compLambdaBody (u32 flags);
void compLambda (u32 flags);
void compAdd (u32 flags);
void compVectorRef (u32 flags);
void compVectorVectorRef (u32 flags);
void compCons (u32 flags);
void compBegin (u32 flags);
void compQuote (void);
void compIf (u32 flags);
void compSyntaxRules (void);
void compCombination (u32 flags);
int  compExpression (u32 flags);
int compCompile (void);

#endif
