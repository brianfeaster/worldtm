/*
 Automatic compacting object heap system.
 
 Allows one to create one of two basic automatically memory managed heap
 objects:  an array of bytes and a vector of said objects.  The module will
 automatically reclaim unused memory when necessary so no implicit removal
 of objects are required.  Currently, a generational bi-heap collector is
 implemented.
*/ 
#ifndef _MEM_H
#define _MEM_H

#include "globals.h"



/* This must be called before usage of this library.  Two functions are passed
   (or NULL for either) and are called before and after every GC.
*/
void memInitialize (fp preGC, fp postGC);



/* An object type.
*/
typedef void* Obj;



/* An object's 'type'.  A byte so it can be used as a scanned token type.
*/
typedef u32 Type;



/* Reserved object type constants.  8 bit field with the top bit representing
   a 1 for vector and 0 for array.
*/
#define TFINALIZER  0xfc000000
#define TPOINTER    0xfd000000
#define TSTACK      0xfe000000
#define TSHADOW     0xff000000



/* Registers:  These make up the root set for the garbage collector.  All
   computation should use only these as variables since a GC could move an 
   objects location in memory at any time.
*/
extern Obj r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7,
           r8,  r9,  ra,  rb,  rc,  rd,  re,  rf,
           r10, r11, r12, r13, r14, r15, r16, r17,
           r18, r19, r1a, r1b, r1c, r1d, r1e, r1f;



/* Object Creators that store new object in r0.
*/
void memNewStatic   (Type t, u32 byteLength);
void memNewStaticVector(Type t, u32 byteLength);
void memNewArray    (Type t, u32 byteLength);
void memNewVector   (Type t, u32 objLength);
void memNewFinalizer(void);
void memNewPointer  (void);
void memNewStack    (void);



/* Object mutators.  Need to go through this abstraction since the generational
   collector might need to keep track of mutated vector objects in the 'old'
   heap.
*/
void memArraySet  (Obj obj, u32 offset, u8  item);
void memVectorSet (Obj obj, u32 offset, Obj item);
void memStackPush (Obj stack, Obj item);
void memStackSet  (Obj stack, u32 topOffset, Obj item);
Obj  memStackPop  (Obj stack);



/* Object selectors.
*/
Type memObjectType   (Obj obj);
u32  memObjectLength (Obj obj);
int  memStackLength (Obj obj);

u8   memArrayObject  (Obj obj, u32 offset);
Obj  memVectorObject (Obj obj, u32 offset);
Obj  memStackObject (Obj obj, u32 topOffset);



/* Force heap to be collected.
*/
void memGarbageCollect (void);



/* Debugging aids.
*/
void memDebugObjectDump (Obj o);
void memDebugDumpHeapHeaders (void);
void memDebugDumpHeapStructures (void);



#endif
