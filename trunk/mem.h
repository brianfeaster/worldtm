/* Automatic compacting object heap system
 
   Allows one to create one of two basic automatically memory managed heap
   objects:  An "array" of bytes and a "vector" of arrays and vectors.  The
   module will automatically reclaim unused memory when necessary so no implicit
   removal of objects are required.  Currently, a generational bi-heap collector
   is implemented.
*/ 
#ifndef _MEM_H
#define _MEM_H

#include <stdio.h>
#include "globals.h"

/* Byte size of a Linux virtual memory block and the resolution of mmap.
   0x001000 = 2^12 = 4Kb
*/
#define BLOCK_BYTE_SIZE ((Num)0x1000)

#define ObjSize sizeof(Obj)

/* Limit the size of any object to 16Mb (2^24) objects for sanity. */
#define MEMOBJECTMAXSIZE ((Num)0x1000000)

/* This must be called before usage of this library.  Two functions are passed
   (or NULL for either) and are called before and after every GC.
   It sets up the various heaps.  These include static, old and current the
   current generational collector.
*/
void memInitialize (Func preGC, Func postGC);



/* An object's 'type' and length type.  They are unioned to form a descriptor.
   For the type, only the lower byte used so it can be used as a token in the
   scanner.
*/
typedef Num Descriptor;
typedef Num Type;       // Highest byte of descriptor.
typedef Num LengthType; // Remaining bytes of descriptor.



/* Registers:  These make up the root set for the garbage collector.  All
   computation should use only these as variables since a GC could move an 
   objects location in memory at any time.
*/
extern Obj r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7,
           r8,  r9,  ra,  rb,  rc,  rd,  re,  rf,
           r10, r11, r12, r13, r14, r15, r16, r17,
           r18, r19, r1a, r1b, r1c, r1d, r1e, r1f;


Num memArrayLengthToObjectSize  (LengthType length);
Num memVectorLengthToObjectSize  (LengthType length);

/* Object Creators that store new object in r0.
*/
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
   heap.
*/
void memArraySet  (Obj obj, Num offset, u8  item);
void memVectorSet (Obj obj, Num offset, Obj item);
void memStackPush (Obj stack, Obj item);
void memStackSet  (Obj stack, Num topOffset, Obj item);
Obj  memStackPop  (Obj stack);



/* Object selectors.
*/
Descriptor memObjectDescriptor (Obj o);
Type memObjectType   (Obj obj);
Num  memObjectLength (Obj obj);
Num  memStackLength  (Obj obj);

u8   memArrayObject  (Obj obj, Num offset);
Obj  memVectorObject (Obj obj, Num offset);
Obj  memStackObject  (Obj obj, Num topOffset);



/* Force heap to be collected.
*/
void memGarbageCollect ();



/* Debugging aids.
*/
void memDebugDumpHeapHeaders (FILE *stream);
void memDebugDumpObject (Obj o, FILE *stream);
void memDebugDumpStaticHeap (FILE *stream);
void memDebugDumpOldHeap (FILE *stream);
void memDebugDumpYoungHeap (FILE *stream);
void memDebugDumpNewHeap (FILE *stream);
void memDebugDumpAll (FILE *stream);
void memValidateObject (Obj o);
void memValidateHeapStructures (void);

Chr* memTypeString        (Type t);
/* Mechanism to associate a pointer address with a string.  A macro is
   provided to associate a pointer addresses and its string representation.
   char* must be the type due to how the preprocessor generates symbols. */
char* memObjString        (Obj obj);
void memObjStringRegister (Obj obj, char* str);
#define memObjStringSet(o) memObjStringRegister(o, #o);


#endif /* _MEM_H */
