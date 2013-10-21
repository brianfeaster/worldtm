#ifndef _COMP_H
#define _COMP_H

#include "globals.h"


// Aliases for the reserved registers
#define RTGE      R09    // was R08 x
#define RENVLINK  R0A    // was R09 x
#define RENV      R0B    // was R0C x
#define RCODELINK R0C    // was R0B x
#define RCODE     R0D    // was R0E x
#define RDSTACK   R0E
#define RSTACK    R0F    // was R0F x

#define RIPLINK   R1C    // was R0A x
#define RIP       R1D    // was R0D x
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
