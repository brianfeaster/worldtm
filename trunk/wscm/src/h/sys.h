#ifndef _SYS_H
#define _SYS_H
#include <stdio.h>
#include "globals.h"
#include <sys/stat.h> /* mode_t */

void sysNewClosure1Env (void);

/* Scanner state fields */
#define FINALSTATE  0x80l
#define PUSHBACK    0x40l

/* Scanner final states including states requiring a character push-back */
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
#define SOCT     FINALSTATE|PUSHBACK|0x0f
#define SDOT     FINALSTATE|PUSHBACK|0x10
#define SREAL    FINALSTATE|PUSHBACK|0x11
#define SINTEGER FINALSTATE|PUSHBACK|0x12

/* Useful */
s64 sysTime (void);
void sysStackToList (void);
void sysListToStack (void);
void sysDumpCallStackCode (void);
void syscallDebugger (void);

/* Scanning_parsing */
extern Chr yytext[];
extern Num yyleng;

Num transition (Num ch, Num state);
void yyrestart(int fd);
void yy_scan_string(Str buff);
void yy_scan_bytes(u8 *buff, Num len);
Num parseString (Str str);
Num yylex (void);
void yyparse (void);

/* Environment */
void sysTGEFindNew (void);
void sysTGEFind (void);
void sysTGEBind (void);
Num sysEnvFind (void);
void sysEnvGet (void);
void sysDefine (char* sym);
void sysDefineSyscallString (Func function, Str functionStr, char* symbol);
#define sysDefineSyscall(f,s) sysDefineSyscallString(f, (Str)#f, s)
void sysDefinePrimitiveString (Func function, Str functionStr, char* symbol);
#define sysDefinePrimitive(f,s) sysDefinePrimitiveString(f, (Str)#f, s)

/* Serializers_internal */
void sysSerializeInteger (Int theint, Num base);
Int sysWriteR (Obj a, Num islist, FILE *stream, Int max);
Num sysWrite (Obj a, FILE *stream);
Num sysWriteMax (Obj a, FILE *stream, Int max);
void sysDumpTGE (void);
void sysDumpEnv (Obj e);

/* Ports */
void sysOpenRemoteSocket (void);
void sysOpenLocalSocket (void);
void sysAcceptRemoteStream (void);
void sysAcceptLocalStream (void);
void sysRecv (void);
void sysSend (void);
Int  sysCanonicalizePath (char *path);
void sysOpenFile (int oflag, mode_t mode, Num silent);
void sysOpenString ();
void sysSemaphoreDown (void);
void sysSemaphoreUp (void);

/* Initialization */
void sysInitialize (Str sandbox);

#endif
