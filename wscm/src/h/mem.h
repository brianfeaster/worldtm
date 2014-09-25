#ifndef _MEM_H
#define _MEM_H
#include "globals.h"
/*
 Types
 Descriptors
 Object_creation
 Root_Set
 Garbage_collector
 Type_Strings
 Address_Strings
 Debugging_And_Printing
 Init

   Automatic compacting object heap system
   Allows one to create one of two basic automatically memory managed heap
   objects:  An "array" of bytes and a "vector" of arrays and vectors.  The
   module will automatically reclaim unused memory when necessary so no implicit
   removal of objects are required.  Currently, a generational bi-heap collector
   is implemented.
*/ 

#define MEMMAXTYPES   0x100l
#define TARYSTACK      0x7bl
#define TVECSTACK      0xfbl

/* Limit the size of any object to 16Mb (2^24) bytes for sanity. */
#define MEMOBJECTMAXSIZE ((Num)0x1000000)

/* Byte count of a Linux virtual memory block and the resolution of mmap.
   0x1000 = 2^12 = 4Kb = 4096b*/
#define BLOCK_BYTE_SIZE ((Num)0x1000)

/* Byte count of an object (which is really a void pointer) */
#define ObjSize (Int)sizeof(Obj)

/* Descriptor details */
#define DescSize           sizeof(Descriptor)
#define DescBitCount       (8 * DescSize)
#define DescTypeBitCount   8
#define DescLengthBitCount (DescBitCount - DescTypeBitCount)

#define DescTypeBitMask   (~(Length)0 << (DescBitCount - DescTypeBitCount))   /* 0xff000000... */
#define DescLengthBitMask (~(Length)0 >> (DescBitCount - DescLengthBitCount)) /* 0x00ffffff... */


/* An object's 'type' and length type.  They are unioned to form a descriptor.
   For the type, only the lower byte used so it can be used as a token in the
   scanner.  */
typedef Num Descriptor;
typedef Num Type;       /* Highest byte of descriptor */
typedef Num Length; /* Remaining bytes of descriptor */

typedef struct {
	Obj start;  /* Initial heap location. */
	Obj next;   /* Next available heap location. */
	Obj last;   /* One byte past the last heap location (exclusive). */
	Num finalizerCount; /* Number of finalizer types contained in this heap. */
} Heap;

extern Heap heapStatic, heap, heapOld, heapNew;


/***************************************
 Descriptors
***************************************/
Descriptor memObjectDescriptor (Obj o);
Type memObjectType   (Obj obj);
Num  memObjectLength (Obj obj);

Num memIsObjectBaseArray (Obj o);
Num memIsObjectVecStack (Obj o);
Num memIsObjectAryStack (Obj o);
Num memIsObjectType (Obj o, Type t);

Num memArrayLengthToObjectSize  (Length length);
Num memVectorLengthToObjectSize  (Length length);


/***************************************
 Heap_stuff
***************************************/
Num memHeapUsedSize (Heap *h);


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
Obj memNewVecStack    (void);
Obj memNewAryStack    (void);

Num memIsObjectValid  (Obj o);

/* Object mutators and accessors.  Need to go through this abstraction since the
   generational collector might need to keep track of mutated vector objects in
   the 'old' heap.
*/
void memArraySet    (Obj obj, Num offset, u8  item);
u8   memArrayObject (Obj obj, Num offset);

void memVectorSet    (Obj obj, Num offset, Obj item);
Obj  memVectorObject (Obj obj, Num offset);

Num  memVecStackLength (Obj stack);
void memVecStackPush   (Obj stack, Obj item);
Obj  memVecStackPop    (Obj stack);
void memVecStackSet    (Obj stack, Num topOffset, Obj item);
Obj  memVecStackObject (Obj stack, Num topOffset);

/* Array stacks are just that, array objects used as a stack.  The GC skips
   the contents entirely.  Used for static pointers or immediate numbers.
*/
Num  memAryStackLength (Obj stack);
void memAryStackPush   (Obj stack, Obj item);
Obj  memAryStackPop    (Obj stack);
void memAryStackSet    (Obj stack, Num topOffset, Obj item);
Obj  memAryStackObject (Obj stack, Num topOffset);


/***************************************
 Root_Set
***************************************/
#define DEFOBJ(x) Obj x; memRootSetPush(&x);
#define UNDEFOBJ(x) memRootSetPop(&x);

#define memRootSetPush memRootSetAddressRegister 
#define memRootSetPop  memRootSetAddressUnRegister 
void memRootSetAddressRegister (Obj *objp);
void memRootSetAddressUnRegister (Obj *objp);


/***************************************
 Garbage_collector
***************************************/
extern Num GarbageCollectionMode;
void memGarbageCollect ();


/***************************************
 Type_Strings
***************************************/
void memTypeStringRegister (Type type, Str description);
Str  memTypeString (Type t);


/***************************************
 Address_Strings

 Mechanism to associate a C pointer address with a string.  A macro is
 provided to associate a pointer addresses and its string representation.

 The anonymous obj pointer functions are used to temporarily protect
 local C obj variables whos address should be registered in case a GC
 occurs wich would move the object it points to.
***************************************/
#define MEM_ADDRESS_REGISTER(o) memAddressStringRegister(o, (Str)#o)
void memAddressStringRegister (Obj obj, Str str);
Str  memAddressString (Obj obj);


/***************************************
 Debugging_And_Printing
***************************************/
void memPrintStructures (FILE *stream);
void memPrintRootSet (FILE *stream);
void memPrintTypes (FILE *stream);
void memPrintAddresses (FILE *stream);
void memPrintObject (Obj o, FILE *stream);
void memPrintHeapStatic (FILE *stream);
void memPrintHeapOld (FILE *stream);
void memPrintHeapYoung (FILE *stream);
void memPrintHeapNew (FILE *stream);
void memPrintHeaps (FILE *stream);
void memPrintAll (FILE *stream);
Int  memValidateObject (Obj o);
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
