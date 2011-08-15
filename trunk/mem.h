#ifndef _MEM_H
#define _MEM_H
/* Automatic compacting object heap system
   Allows one to create one of two basic automatically memory managed heap
   objects:  An "array" of bytes and a "vector" of arrays and vectors.  The
   module will automatically reclaim unused memory when necessary so no implicit
   removal of objects are required.  Currently, a generational bi-heap collector
   is implemented.
*/ 
#include "globals.h"


/* Global virtual machine register aliases */
#define rblocked   r10 /* WSCM: I/O and Semaphore blocked threads */
#define rthreads   r11 /* WSCM: Thread vector */
#define rsleeping  r12 /* WSCM: Sleeping thread */
#define rrunning   r13 /* WSCM: Current thread */
#define rready     r14 /* WSCM: Thread list */
#define rexpr      r15 /* WSCM: Expression being compiled */

#define rasmstack  r16 /* VM: Opcode stack where machine code is emitted */

#define rsymbols   r17 /* OBJ: Symbol table used by scanner and OS */
#define rtge       r18 /* WSCM: Global environment */

#define rretenv    r19 /* VM: Caller's env */
#define rretip     r1a /* VM: Caller's ip */
#define rretcode   r1b /* VM: Caller's code block */
#define renv       r1c /* VM: Current running thread's environment */
#define rip        r1d /* VM: Current running program instruction pointer */
#define rcode      r1e /* VM: Currently running code object */

#define rstack     r1f /* Register alias: Global stack used by VM */


/* Registers:  These make up the root set for the garbage collector.  All
   computation should use only these as variables since a GC could move an 
   objects location in memory at any time.
*/
extern Obj r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7,
           r8,  r9,  ra,  rb,  rc,  rd,  re,  rf,
           r10, r11, r12, r13, r14, r15, r16, r17,
           r18, r19, r1a, r1b, r1c, r1d, r1e, r1f;


/* Byte count of a Linux virtual memory block and the resolution of mmap.
   0x001000 = 2^12 = 4Kb */
#define BLOCK_BYTE_SIZE ((Num)0x1000)


/* Byte count of an object which is a pointer */
#define ObjSize sizeof(Obj)



/* An object's 'type' and length type.  They are unioned to form a descriptor.
   For the type, only the lower byte used so it can be used as a token in the
   scanner.  */
typedef Num Descriptor;
typedef Num Type;       /* Highest byte of descriptor */
typedef Num LengthType; /* Remaining bytes of descriptor TODO rename this just Length */


Num memArrayLengthToObjectSize  (LengthType length);
Num memVectorLengthToObjectSize  (LengthType length);



/* Object Creators that store new object in r0 */
void memNewStatic      (Type t, LengthType byteLength);
void memNewStaticVector(Type t, LengthType objLength);
void memNewArray       (Type t, LengthType byteLength);
void memNewVector      (Type t, LengthType objLength);
void memNewSemaphore   (void);
void memNewFinalizer   (void);
void memNewPointer     (void);
void memNewStack       (void);

int memIsObjectValid  (Obj o);



/* Object mutators.  Need to go through this abstraction since the generational
   collector might need to keep track of mutated vector objects in the 'old'
   heap.  */
void memArraySet  (Obj obj, Num offset, u8  item);
void memVectorSet (Obj obj, Num offset, Obj item);
void memStackPush (Obj stack, Obj item);
void memStackSet  (Obj stack, Num topOffset, Obj item);
Obj  memStackPop  (Obj stack);



/* Object selectors.  */
Descriptor memObjectDescriptor (Obj o);
Type memObjectType   (Obj obj);
Num  memObjectLength (Obj obj);
Num  memStackLength  (Obj obj);

u8   memArrayObject  (Obj obj, Num offset);
Obj  memVectorObject (Obj obj, Num offset);
Obj  memStackObject  (Obj obj, Num topOffset);

/* Stack manipulation on r1f */
void memPush (Obj o);
Obj  memPop  (void);



/* Force heap to be collected */
extern Num GarbageCollectionMode;
void memGarbageCollect ();



/* Debugging aids */
void memDebugDumpHeapHeaders (FILE *stream);
void memDebugDumpObject (Obj o, FILE *stream);
void memDebugDumpStaticHeap (FILE *stream);
void memDebugDumpOldHeap (FILE *stream);
void memDebugDumpYoungHeap (FILE *stream);
void memDebugDumpNewHeap (FILE *stream);
void memDebugDumpAll (FILE *stream);
void memValidateObject (Obj o);
void memValidateHeapStructures (void);
Str memTypeString (Type t);



/* Mechanism to associate a C pointer address with a string.  A macro is
   provided to associate a pointer addresses and its string representation.
   char* must be the type (instead of my Str type) due to how the preprocessor
   generates symbols. */
char* memObjString        (Obj obj);
void memObjStringRegister (Obj obj, char* str);
#define memObjStringSet(o) memObjStringRegister(o, #o);



/* This must be called before usage of this library.  Two functions are passed
   (or NULL for either) and are called before and after every GC.
   It sets up the various heaps.  These include static, old and current the
   current generational collector.  */
void memInitialize (Func preGC, Func postGC);

void memRegisterType (Type type, char *description);



#endif /* _MEM_H */
