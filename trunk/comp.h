#ifndef _COMP_H
#define _COMP_H

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
