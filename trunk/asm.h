#ifndef _ASM_H
#define _ASM_H

#include "globals.h"

/* Opcodes and fields */
#define R0 (Obj)0x0
#define R1 (Obj)0x1
#define R2 (Obj)0x2
#define R3 (Obj)0x3
#define R4 (Obj)0x4
#define R5 (Obj)0x5
#define R6 (Obj)0x6
#define R7 (Obj)0x7
#define R8 (Obj)0x8
#define R9 (Obj)0x9
#define RA (Obj)0xa
#define RB (Obj)0xb
#define RC (Obj)0xc
#define RD (Obj)0xd
#define RE (Obj)0xe
#define RF (Obj)0xf
#define R10 (Obj)0x10
#define R11 (Obj)0x11
#define R12 (Obj)0x12
#define R13 (Obj)0x13
#define R14 (Obj)0x14
#define R15 (Obj)0x15
#define R16 (Obj)0x16
#define R17 (Obj)0x17
#define R18 (Obj)0x18
#define R19 (Obj)0x19
#define R1A (Obj)0x1a
#define R1B (Obj)0x1b
#define R1C (Obj)0x1c
#define R1D (Obj)0x1d
#define R1E (Obj)0x1e
#define R1F (Obj)0x1f

#define MV   (Obj)0x20
#define MVI  (Obj)0x21
#define LDI  (Obj)0x22
#define LD   (Obj)0x23
#define STI  (Obj)0x24
#define ST   (Obj)0x25
#define PUSH (Obj)0x26
#define POP  (Obj)0x27
#define ADDI (Obj)0x28
#define BLTI (Obj)0x29
#define BEQI (Obj)0x2a
#define BNEI (Obj)0x2b
#define BRTI (Obj)0x2c
#define BNTI (Obj)0x2d
#define BRA  (Obj)0x2e
#define JMP  (Obj)0x2f
#define JAL  (Obj)0x30
#define RET  (Obj)0x31
#define SYS  (Obj)0x32
#define SYSI (Obj)0x33
#define NOP  (Obj)0x34
#define QUIT (Obj)0x35

#define LABEL (Obj)0xfe

#define END sasmend
extern Obj sasmend;
#define NA sasmna
extern Obj sasmna;

/***************************************
 ASM
***************************************/
void asmInit (void);
Obj asmNewLabel();
void asmAsmInternal (Obj f, ...);
#define asmAsm(...) asmAsmInternal(__VA_ARGS__, sasmend)


/***************************************
 ASM
***************************************/
void asmStart (void);
void asmAsmIGraph (void);


/*******************************************************************************
 Init
*******************************************************************************/
void asmInitialize (void);


#endif
