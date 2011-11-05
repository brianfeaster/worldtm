#ifndef _MEM_H
#define _MEM_H
#include "globals.h"
/*
 Pointer_Strings
 Type_stuff
 Descriptors
 Root_Set
 Object_creation
 Garbage_collector
 Debugging_aids
 Init

   Automatic compacting object heap system
   Allows one to create one of two basic automatically memory managed heap
   objects:  An "array" of bytes and a "vector" of arrays and vectors.  The
   module will automatically reclaim unused memory when necessary so no implicit
   removal of objects are required.  Currently, a generational bi-heap collector
   is implemented.
*/ 
#define MEMMAXTYPES   0x100l
#define TSTACK         0xfbl


/* Byte count of a Linux virtual memory block and the resolution of mmap.
   0x1000 = 2^12 = 4Kb = 4096b*/
#define BLOCK_BYTE_SIZE ((Num)0x1000)


/* Byte count of an object which is a pointer */
#define ObjSize sizeof(Obj)


/* An object's 'type' and length type.  They are unioned to form a descriptor.
   For the type, only the lower byte used so it can be used as a token in the
   scanner.  */
typedef Num Descriptor;
typedef Num Type;       /* Highest byte of descriptor */
typedef Num Length; /* Remaining bytes of descriptor */


/***************************************
 Pointer_Strings

 Mechanism to associate a C pointer address with a string.  A macro is
 provided to associate a pointer addresses and its string representation.
***************************************/
void memPointerRegisterString (Obj obj, Str str);
#define memPointerRegister(o) memPointerRegisterString(o, (Str)#o)

Str memPointerString (Obj obj);

void memPointerStringDumpAll (FILE *stream);


/***************************************
 Type_stuff
***************************************/
void memTypeRegisterString (Type type, Str description);
#define memTypeRegister(t) memTypeRegisterString(t, (Str)#t)

Str memTypeString (Type t);

void memTypeStringDumpAll (FILE *stream);


/***************************************
 Descriptors
***************************************/
Descriptor memObjectDescriptor (Obj o);
Type memObjectType   (Obj obj);
Num  memObjectLength (Obj obj);

Num memIsObjectBaseArray (Obj o);
Num memIsObjectStack (Obj o);
Num memIsObjectType (Obj o, Type t);

Num memArrayLengthToObjectSize  (Length length);
Num memVectorLengthToObjectSize  (Length length);


/***************************************
 Root_Set
***************************************/
void memRootSetRegisterString (Obj *objp, Str desc);
#define memRootSetRegister(op) memRootSetRegisterString(&op, (Str)#op)


/***************************************
 Object_creation

 Object Creators that return new objects
 in the new young/heap
***************************************/
Obj memNewStatic      (Type t, Length byteLength);
Obj memNewStaticVector(Type t, Length objLength);
Obj memNewArray       (Type t, Length byteLength);
Obj memNewVector      (Type t, Length objLength);
Obj memNewSemaphore   (void);
Obj memNewFinalizer   (void);
Obj memNewPointer     (void);
Obj memNewStack       (void);

Num memIsObjectValid  (Obj o);

/* Object mutators.  Need to go through this abstraction since the generational
   collector might need to keep track of mutated vector objects in the 'old'
   heap.  */
void memArraySet  (Obj obj, Num offset, u8  item);
void memVectorSet (Obj obj, Num offset, Obj item);
void memStackPush (Obj stack, Obj item);
void memStackSet  (Obj stack, Num topOffset, Obj item);
Obj  memStackPop  (Obj stack);

u8   memArrayObject  (Obj obj, Num offset);
Obj  memVectorObject (Obj obj, Num offset);
Obj  memStackObject  (Obj obj, Num topOffset);
Num  memStackLength  (Obj obj);


/***************************************
 Garbage_collector
***************************************/
extern Num GarbageCollectionMode;
void memGarbageCollect ();


/***************************************
 Debugging_aids
***************************************/
void memDebugDumpHeapHeaders (FILE *stream);
void memDebugDumpObject (Obj o, FILE *stream);
void memDebugDumpStaticHeap (FILE *stream);
void memDebugDumpOldHeap (FILE *stream);
void memDebugDumpYoungHeap (FILE *stream);
void memDebugDumpNewHeap (FILE *stream);
void memDebugDumpAll (FILE *stream);
void memValidateObject (Obj o);
void memValidateHeapStructures (void);


/***************************************
 Init

 This must be called before usage of this library.  Two functions are passed
 (or NULL for either) and are called before and after every GC.
 It sets up the various heaps.  These include static, old and current the
 current generational collector.
***************************************/
void memInitialize (Func preGC, Func postGC, Func exceptionHandlerCallback);


#endif /* _MEM_H */
