#ifndef _COMP_H
#define _COMP_H

#include "globals.h"


// Aliases for the reserved registers
#define RTGE      R09
#define RENVLINK  R0A
#define RENV      R0B
#define RCODELINK R0C
#define RCODE     R0D
#define RDSTACK   R0E
#define RSTACK    R0F

#define RIPLINK   R1C
#define RIP       R1D
#define RDSTACKP  R1E
#define RSTACKP   R1F


/***************************************
 Compiler
***************************************/
void compCompile (void);


/***************************************
 Init
***************************************/
void compInitialize (void);


#endif
