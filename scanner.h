#ifndef _SCANNER_H
#define _SCANNER_H

#include "globals.h"

/* Scanner state fields.
*/
#define FINALSTATE  0x80
#define PUSHBACK    0x40

/* Scanner final states including states requiring a character push-back.
*/
#define SOPENPAREN        FINALSTATE|0x00
#define SSTRING           FINALSTATE|0x01
#define SVECTOR           FINALSTATE|0x02
#define SQUOTE            FINALSTATE|0x03
#define SCLOSEPAREN       FINALSTATE|0x04
#define STRUE             FINALSTATE|0x05
#define SFALSE            FINALSTATE|0x06
#define SCHARACTER        FINALSTATE|0x07
#define SEOF              FINALSTATE|0x08
#define SQUASIQUOTE       FINALSTATE|0x09
#define SUNQUOTESPLICING  FINALSTATE|0x0a
#define SUNQUOTE FINALSTATE|PUSHBACK|0x0b
#define SBINARY  FINALSTATE|PUSHBACK|0x0c
#define SSYMBOL  FINALSTATE|PUSHBACK|0x0d
#define SHEX     FINALSTATE|PUSHBACK|0x0e
#define SDOT     FINALSTATE|PUSHBACK|0x0f
#define SREAL    FINALSTATE|PUSHBACK|0x10
#define SINTEGER FINALSTATE|PUSHBACK|0x11

extern u8  yytext[];
extern Num yyleng;

Num parseString (u8* str);
void yyrestart(int fd);
void yy_scan_string(u8 *buff);
void yy_scan_bytes(u8 *buff, Num len);
Num transition (Num ch, Num state);
Num yylex (void);
void yyparse (void);

#endif
