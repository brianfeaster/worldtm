#ifndef _ASM_H
#define _ASM_H

#include "globals.h"

/* Opcodes and fields */
#define R00 (Obj)0x00
#define R01 (Obj)0x01
#define R02 (Obj)0x02
#define R03 (Obj)0x03
#define R04 (Obj)0x04
#define R05 (Obj)0x05
#define R06 (Obj)0x06
#define R07 (Obj)0x07
#define R08 (Obj)0x08
#define R09 (Obj)0x09
#define R0A (Obj)0x0a
#define R0B (Obj)0x0b
#define R0C (Obj)0x0c
#define R0D (Obj)0x0d
#define R0E (Obj)0x0e
#define R0F (Obj)0x0f
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
#define IMM (Obj)0x20
#define LABEL (Obj)0x21

#define ANDI (Obj)0x30
#define LSLI (Obj)0x31
#define LSRI (Obj)0x32
#define ADD  (Obj)0x33
#define ADDI (Obj)0x34
#define MUL  (Obj)0x35
#define MULI (Obj)0x36
#define MVI  (Obj)0x37
#define MV   (Obj)0x38
#define LDI  (Obj)0x39
#define LD   (Obj)0x3a
#define POP  (Obj)0x3b
#define PUSH (Obj)0x3c
#define STI  (Obj)0x3d
#define ST   (Obj)0x3e
#define BLTI (Obj)0x3f
#define BGTI (Obj)0x40
#define BGT  (Obj)0x41
#define BEQI (Obj)0x42
#define BNEI (Obj)0x43
#define BRA  (Obj)0x44
#define JMP  (Obj)0x45
#define JAL  (Obj)0x46
#define RET  (Obj)0x47
#define SYS  (Obj)0x48
#define SYSI (Obj)0x49
#define NOP  (Obj)0x4a
#define QUIT (Obj)0x4b


#define END sasmend
extern Obj sasmend;

#define NA sasmna
extern Obj sasmna;


/* Rootset objects */
extern Obj ropcodes, riblock, riblocks, ricodes, rlabels, rexpr, rcodenew;


/***************************************
 Labels
***************************************/
Obj asmNewLabel();


/***************************************
 Iregisters
***************************************/
Num asmNewOregister(void);
Num asmNewIregister(void);
Num asmIsOregister (Obj o);
Num asmIsIregister (Obj o);


/***************************************
 ASM
***************************************/
void asmStart (void);
void asmInit (void);
void asmReset (void);
void asmAsmInternal (Obj f, ...);
#define asmAsm(...) asmAsmInternal(__VA_ARGS__, END)


/***************************************
 Assemble
***************************************/
void asmAssemble (void);


/*******************************************************************************
 Init
*******************************************************************************/
void asmInitialize (void);


#endif
