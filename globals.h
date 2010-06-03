#ifndef _GLOBALS_H
#define _GLOBALS_H


/* Register aliases.
*/
#define semaphores r9  /* WSCM: semaphore counters. */
#define blocked    r10 /* WSCM: I/O and Semaphore blocked threads. */
#define threads    r11 /* WSCM: Thread vector.  An id implementation. */
#define sleeping   r12 /* WSCM: Sleeping thread. */
#define running    r13 /* WSCM: Current thread. */
#define ready      r14 /* WSCM: Thread list. */

#define retenv     r15 /* VM: Caller's environment.*/
#define env        r16 /* WSCM: Current active environment. */
#define tge        r17 /* WSCM: Global environment. */
#define expr       r18 /* WSCM: Expression being compiled */
#define symbols    r19 /* OBJ: Symbol table used by scanner and OS. */
#define asmstack   r1a /* VM: Opcode stack where machine code is emitted. */
#define ip         r1b /* VM: Current running program instruction pointer */
#define code       r1c /* VM: Currently running code object. */
#define retip      r1d /* VM: Caller's ip. */
#define retcode    r1e /* VM: Caller's code block. */
#define stack      r1f /* VM: Global stack used by VM. */


/* C Type declarations representing the number of bits for unsigned, signed and
   real types.  Currently these match the Linode virtual machines I'm hosted on.
 */
typedef unsigned char  u8;
typedef unsigned short u16;
typedef unsigned int   u32;
typedef unsigned long  u64;

typedef char  s8;
typedef short s16;
typedef int   s32;
typedef long  s64;

typedef float       r32;
typedef double      r64;
typedef long double r128;


/* Common types.
*/
typedef u8    Chr;
typedef u8*   Str;
typedef s64   Int;
typedef u64   Num;
typedef r64   Real;
typedef void* Obj;


/* Type declaration for functions.
*/
typedef void (*Func)  (void);
typedef void (*Func1) (Obj a);
typedef void (*Func2) (Obj a, Obj b);


/* Printf format strings for the above types.
*/
#define STR "%s"

#define X8  "%x"
#define X16 "%x"
#define X32 "%x"
#define X64 "%lx"

#define U8  "%u"
#define U16 "%u"
#define U32 "%u"
#define U64 "%lu"

#define S8  "%d"
#define S16 "%d"
#define S32 "%d"
#define S64 "%ld"

#define INT   "%ld"
#define INT4 "%4ld"

#define NUM  "%lu"

#define HEX     "%lx"
#define HEX2   "%2lx"
#define HEX02 "%02lx"
#define HEX4   "%4lx"
#define HEX04 "%04lx"

#define REAL "%lf"

/* Memory pointers seem to be in the 12 hex-digit
   range so this is the usual format string length.  */
#define OBJ  "%012lx"
#define OBJ0 "%016lx"


/* Scheme object types.  Byte with highest bit signifying a memory vector
   object.  The lower 56 bits are used in descriptors for the object length
   either number of bytes in the array or the vector size.
 */
#define TBASEARRAY     0x00
#define TFALSE         0x01
#define TTRUE          0x02
#define TNULL          0x03
#define TNULLVEC       0x04
#define TNULLSTR       0x05
#define TEOF           0x06
#define TCHAR          0x07
#define TSTRING        0x08
#define TSYMBOL        0x09
#define TINTEGER       0x0a
#define TREAL          0x0b

#define TBASEVECTOR    0x80
#define TPAIR          0x81
#define TVECTOR        0x82
#define TCLOSURE       0x83
#define TCONTINUATION  0x84
#define TCODE          0x85
#define TPORT          0x86
#define TSOCKET        0x87
#define TSYSCALL       0x88

#define TFINALIZER     0xfc
#define TPOINTER       0xfd
#define TSTACK         0xfe
#define TSHADOW        0xff


/* Compiler flags.  Make sure only 20 bits are used as garbage collector
   assumes vector object pointers less than 2^20 are numeric constants.
   This doesn't make sense now since the compiler flag register is passed
   in C land.
*/
static const unsigned TAILCALL = 0x00080000;
/*
static const unsigned R8 =       0x00000100;
static const unsigned R7 =       0x00000080;
static const unsigned R6 =       0x00000040;
static const unsigned R5 =       0x00000020;
static const unsigned R4 =       0x00000010;
static const unsigned R3 =       0x00000008;
static const unsigned R2 =       0x00000004;
static const unsigned R1 =       0x00000002;
static const unsigned R0 =       0x00000001;
*/

#define MAX_THREADS 1024

#define ASSERT assert

#define NL "\n"

#define TAB0 ""
#define TAB1 TAB0 "   "
#define TAB2 TAB1 "   "
#define TAB3 TAB2 "   "
#define TAB4 TAB3 "   "
#define TAB5 TAB4 "   "

#endif
