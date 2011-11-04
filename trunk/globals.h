#ifndef _GLOBALS_H
#define _GLOBALS_H

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
#define CHR "%c"
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

#define HEX       "%lx"
#define HEX2     "%2lx"
#define HEX4     "%4lx"
#define HEX02   "%02lX"
#define HEX03   "%03lx"
#define HEX04   "%04lx"
#define HEX08   "%08lx"
#define HEX016 "%016lx"

#define REAL "%lf"

/* Memory pointers seem to be in the 12 hex-digit
   range so this is the usual format string length.  */
#define OBJ  "%012lX"
#define OBJ0 "%016lx"


#define NL "\n"
#define SP " "


#endif
