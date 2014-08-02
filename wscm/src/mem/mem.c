#define DEBUG 0
#define DB_DESC "MEM"
#define DEBUG_ASSERT 1
#define DEBUG_ASSERT_STACK 1
#define VALIDATE_HEAP 1
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <assert.h>
#include <errno.h>
#include "mem.h"
/*
 Types
 Descriptors
 Heap_stuff
 Object_creation
 Root_Set
 Garbage_collector
 Type_Strings
 Address_Strings
 Debugging_And_Printing
 Init

  About mem.c

   Two sets of objects, old and young,  will be maintained.  Normally, the
   young object heap will be copy collected, when it fills up, to a new young
   heap.  The old heap will not be touched.  Only when the young heap doesn't
   have enough garabge objects to reduce its size substantially will a GC over
   the old and young be performed.  The resulting new heap of objects will
   become a new old heap with the young heap being completely empty.

   Cheesy state diagram:
   [old 0XXX] [young0X  ]
   [old 0XXX] ([young0XXX] -> [newX  ] -> [young 1X  ])
   [old 0XXX] [young1X  ]
   ([old 0XXX] ([young1XXX] -> [newXXX])) -> [newXXXX] -> [old1XXXX]
   [old 1XXXX] [young2   ]

  TERMINOLOGY

   Size   Inspired by sizeof() operator.  The foot print this object requires
          internally in bytes.  This also includes the object's descriptor,
          unset vector locations, unused bytes for uneven sized strings or
          unused stack locations.

   Count  Number of blocks this object uses in memory.  Will always be a multiple/
          fraction of its size such as a memory block or bit field.

   Length Inspired by scheme primitives length and string-length.  Number of active
          'elements' this object references.  In a vector, the number of objects
          locations.  In an array, the bytes required to represent the value or string
          (such as number of characters).  In a stack, the number of objects pushed.
          Mainly used for external representations of objects.

  Canonicalized
  Normalized
*/

/* Number of generational collections until a complete collection
   is forced across all generations */
#define FORCE_OLD_GC_COUNT 47


/* Internal types which are registered during initialization */
#define TSEMAPHORE     0xfcl
#define TFINALIZER     0xfdl
#define TPOINTER       0xfel
#define TSHADOW        0xffl


void memGarbageCollectInternal (Descriptor desc, Num byteSize);



/* Any error or exceptional condition will arrive here.  A call back function
   can be registered.
*/
Func memExceptionHandler = 0;

void memRaiseException (void) {
	if (memExceptionHandler)
		memExceptionHandler();
	else {
		fprintf (stderr, "\nERROR: memRaiseException() Halting process with a seg fault.\n");
		fflush(stdout);
		__builtin_trap(); // *(int*)0 = 0; // Force a crash to catch in debugger.
	}
}



/*******************************************************************************
 Types

 Object types are defined by 8 bits.  The valid range/count for the core
   types are as follows (all values in hex):
     arrays   00...7f / 80
     vector   80...ef / 70
     internal f0...ff / 10
*******************************************************************************/
#define TMAXARRAY     0x7fl
#define TMAXVECTOR    0xefl
#define TMAXINTERNAL  0xffl



/*******************************************************************************
 Descriptors
*******************************************************************************/
/* A 'Type' is 8 bits composed of a 1 bit 'class' and 7 bit 'id'.  If the class
   bit is 0 then the object and contents are considered raw data and copied
   as-is during a garbage collection.

 Type: [.|.......]
      class  id

   A 'Descriptor' is a 64 bit word composed of an 8 bit 'type' shifted and
   or'ed with a 56 bit 'length' field.  On 32 bit architectures, the size is 24 bits.

   Descriptor:  [--------][-------- -------- -------- -------- -------- -------- --------]
                   type                          size (24 or 56 bits)
*/

/* Create a descriptor given 'type' and 'length' field values
*/
Descriptor memMakeDescriptor (Type type, Length length) {
	return type << DescLengthBitCount | length;
}


/* Get object's 'descriptor' value which is the machine word just before the object */
Descriptor memObjectDescriptor (Obj o) {
	return *((Descriptor*)o - 1);
}

/* Get object's 'type' which is the highest byte in the descriptor */
Type memObjectType (Obj o) {
	return memObjectDescriptor(o) >> DescLengthBitCount;
}

/* Get object's 'length' which is in the lowest bytes of the descriptor */
Num memObjectLength (Obj o) {
	return memObjectDescriptor(o) & DescLengthBitMask;
}



/* Is object a base array or vector base type class? */
Num memIsObjectBaseArray (Obj o)         { return (memObjectType(o) <= TMAXARRAY); }
Num memIsObjectBaseVectorOrArray (Obj o) { return memObjectType(o) <= TMAXVECTOR; }

/* Is object a special type? */
Num memIsObjectSemaphore (Obj o)   { return memIsObjectValid(o) && memObjectType(o) == TSEMAPHORE; }
Num memIsObjectFinalizer (Obj o)   { return memIsObjectValid(o) && memObjectType(o) == TFINALIZER; }
Num memIsObjectPointer (Obj o)     { return                        memObjectType(o) == TPOINTER; } /* During a garbage collection, memIsObjectValid would fail because the object is in the temporary 'new' heap */
Num memIsObjectVecStack (Obj o)       { return memIsObjectValid(o) && memObjectType(o) == TVECSTACK; }
Num memIsObjectAryStack (Obj o)       { return memIsObjectValid(o) && memObjectType(o) == TARYSTACK; }
Num memIsObjectShadow (Obj o)      { return memIsObjectValid(o) && memObjectType(o) == TSHADOW; }
Num memIsObjectType (Obj o, Type t){ return memIsObjectValid(o) && memObjectType(o) == t; }



/* Compute object size (total memory footprint in bytes) based on an array's 'length'.
   It's complicated since a one byte object, such as a character, will still take up
   a machine word size number of bytes (8 bytes on a 64bit architecture) */
Num memArrayLengthToObjectSize (Length length) {
	return DescSize + ((length + DescSize-1) & -DescSize);
}

/* Compute object size based on vector's 'length'.
*/
Num memVectorLengthToObjectSize (Length length) {
	return DescSize + length * ObjSize;
}

/* Get object's size in bytes including the descriptor.  Used to find the
   next object in the heap or size (byte count) for copying into another heap.
*/
Num memObjectSize (Obj o) {
	return memIsObjectBaseArray(o)
	       ? memArrayLengthToObjectSize(memObjectLength(o))
	       : memVectorLengthToObjectSize(memObjectLength(o));
}



/*******************************************************************************
 Heap_stuff
*******************************************************************************/
const Num HEAP_STATIC_BLOCK_COUNT =  0x010; /*    64Kb 0x010000 2^16 */
const Num HEAP_BLOCK_COUNT        =  0x400; /*  4Mb    0x400000 2^22 */
const Num STACK_COUNT            = 0x04000; /*    16Kb 0x004000 2^14 */

#if 0
typedef struct {
	Obj start;  /* Initial heap location. */
	Obj next;   /* Next available heap location. */
	Obj last;   /* One byte past the last heap location (exclusive). */
	Num finalizerCount; /* Number of finalizer types contained in this heap. */
} Heap;
#endif

/* The four heap structure instances
*/
Heap heapOld;   /* Where old objects live during runtime. */
Heap heap;      /* Where new young objects are created during runtime. */
Heap heapNew;   /* GC work area. */
Heap heapStatic;/* Objects that are never deleted nor touched by the GC. */



/* Return number of objects in heap.  Used for unit tests.
*/
Num memHeapLength (Num heapIndex) {
 Heap *h;
 Obj o;
 Num count = 0;
	h = (heapIndex==0) ? &heapStatic :
	    (heapIndex==1) ? &heapOld :
	    (heapIndex==2) ? &heap :
	    (heapIndex==3) ? &heapNew : NULL; /* Default to the young heap */
	assert(h != NULL);
	o = h->start + DescSize;
	while (o < h->next) {
		++count;
		o += memObjectSize(o);
	}
	return count;
}


/* Get a simple one word description of a heap pointer
*/
char* memHeapPointerToString (Heap *h) {
	return h == &heapStatic ? "Static" :
	       h == &heapOld    ? "Old" :
	       h == &heap       ? "Young" :
	       h == &heapNew    ? "New" : "???";
}

Num memHeapBlockCount (Heap *h) { return (Num)(h->last - h->start)/BLOCK_BYTE_SIZE; }
Num memHeapSize (Heap *h) { return (Num)(h->last - h->start); }
Num memHeapUsedSize (Heap *h) { return (Num)(h->next - h->start); }
Num memHeapUnusedSize (Heap *h) { return (Num)(h->last - h->next); }



/* Initialize a heap struct and allocate 'blockCount' blocks
 */
void memInitializeHeap (Heap *h, Num blockCount) {
 Num bytes = BLOCK_BYTE_SIZE * blockCount;
	DBBEG(" "STR, memHeapPointerToString(h));
	h->start = h->next = mmap(0x0, bytes, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	//h->start = h->next = calloc(bytes, 1);
	if (MAP_FAILED == h->start) {
		fprintf (stderr, "\nERROR: memInitializeHeap(): Can't create heap.  mmap() => "OBJ, h->start);
		memRaiseException();
	}
	h->last = h->start + bytes; /* Last pointer is exclusive. */
	h->finalizerCount=0;

	DB("Start "OBJ, h->start);
	DB("Last  "OBJ, h->last);
	DB("Size  "OBJ"  Blocks "HEX, bytes, blockCount);
	DBEND();
}

/* Reset heap objects internals so that it appears empty again
 */
void memResetHeapObject (Heap *h) {
	DBBEG(" "STR, memHeapPointerToString(h));
	h->next = h->start = h->last = (Obj)0;
	h->finalizerCount = 0;
	DBEND();
}

/* Unallocate blockCount from tail part of object linux-memory. */
void memShrinkHeap (Heap *h, Num blockCount) {
 Num oldBlockCount;
 Num newBlockCount;
 Int mret;
// void *ret;
	DBBEG(" "STR, memHeapPointerToString(h));

	if (0 < blockCount) {
		oldBlockCount = memHeapBlockCount(h);
		newBlockCount = oldBlockCount - blockCount;
		DB ("["HEX"] -> ["HEX"] - ["HEX"]", oldBlockCount, newBlockCount, blockCount);
		assert(blockCount <= oldBlockCount);

		mret = munmap(h->start + newBlockCount * BLOCK_BYTE_SIZE, blockCount * BLOCK_BYTE_SIZE - 1);
		if (!mret) {
			h->last = h->start + newBlockCount * BLOCK_BYTE_SIZE;
		} else {
		 	fprintf (stderr, "WARNING: memShrinkHeap() can't shrink heap region");
		}
		//ret = realloc(h->start, newBlockCount * BLOCK_BYTE_SIZE);
		//fprintf (stderr, OBJ" "OBJ, ret, h->start);
		//assert(ret==newBlockCount || ret == h->start);
		h->last = h->start + newBlockCount * BLOCK_BYTE_SIZE;
		/* Update the heap's "last" pointer if munmap succeeds */
	} else {
		DB ("skipping 0 blockCount");
	}

	DBEND();
}

/* Release heap's blocks back to the system and reset the heap object's
   internal data
*/
void memFreeHeap (Heap *h) {
	DBBEG(" "STR, memHeapPointerToString(h));
	memShrinkHeap (h, memHeapBlockCount(h));
	memResetHeapObject(h);
	DBEND();
}



/*******************************************************************************
 Object_creation
*******************************************************************************/

/* TODO Objects in old heap that were mutated with objects in young heap.
   This is an additional set of root set pointers besides the registers
   above.  Any old object that has been mutated to point to an object in the
   young heap must exist in this set.  Otherwise during a young only GC, a
   live object will be collected which is bad.
int mutatedOldObjectsLength=0;
Obj *(mutatedOldObjects[0x400])={0};
*/


/* Allocate an object in the heap:

   The descriptor paramater is a long [tsssssss] t=type-field s=size=field.
   A type >= 0x80 is assumed a vector of objects (pointers, length 8 on IA64
   boxen). A type < 0x80 is raw bytes.  This restriction is used by the gc.
  
   The size paramater is the number of actual bytes the object is composed
   of including the descriptor size.  it should be at least two words in length
   (descriptor size + obj size) so that a shadow pointer can be stored after
   the descriptor.

   If the heap is full, a garbage collection occurs and an attempt is made
   again recursively.  If this happens again then enough new heap space can't
   be attained and the process is terminated.

   There is no need to zero out the new object since mmap will do so for each
   new heap
*/
Obj memNewObject (Descriptor desc, Num byteSize) {
 Obj o;
 static Num recursed=0;
	DBBEG("  desc:"HEX016"  byteSize:"HEX, desc, byteSize);

	if (MEMOBJECTMAXSIZE <= byteSize) {
		printf ("ERROR:: "STR" (desc "HEX016"  byteSize "HEX"): size too big.", __func__, desc, byteSize);
		memRaiseException();
	}

	/* Set the new object's descriptor in the heap, set object pointer 'o' to the
	   new obj location (immediately after the descriptor) and increment the
	   heap's 'next' pointer */
	*(Descriptor*)(heap.next) = desc;
	o = heap.next + DescSize;
	heap.next += byteSize;

	/* Perform a GC if we've surpassed the heap size, calling this function recursively */
	if (heap.next >= heap.last) {
		/* Check that we're not stuck in a recursive loop */
		if (1 == recursed) {
			fprintf (stderr, "ERROR::"STR" Unable to allocate enough heap space from the underlying system", __func__);
			memRaiseException();
		}
		heap.next -= byteSize;
		memGarbageCollectInternal(desc, byteSize);

		++recursed;
		o = memNewObject(desc, byteSize);
		--recursed;
	}

	DBEND("  =>  "OBJ, o);
	return o;
}

/* Allocate a static object.  Called by memNewObject.
*/
Obj memNewStaticObject (Descriptor desc, Num byteSize) {
 Obj o;
	DBBEG("  desc:"HEX016"  byteSize:"HEX, desc, byteSize);
	*(Descriptor*)heapStatic.next = desc;
	o = heapStatic.next + DescSize;
	heapStatic.next += byteSize;

	/* Check that we're still within the static buffer boundary. */
	if (heapStatic.next >= heapStatic.last) {
		fprintf (stderr, "ERROR: memNewStaticObject(): Static buffer overflow.\a\n");
		memRaiseException();
	}
	DBEND(" => "OBJ, o);
	return o;
}


Obj memNewStatic (Type type, Length length) {
 Obj o;
	DBBEG("  type:"HEX02"  length:"HEX, type, length);
	o = memNewStaticObject (memMakeDescriptor(type, length),
	                    memArrayLengthToObjectSize(length));
	DBEND("");
	return o;
}

/* The static heap isn't used as a root set by the garbage collector.
   There's no check verifying a static array has a vector object.
*/
Obj memNewStaticVector (Type type, Length length) {
 Obj o;
	DBBEG("  type:"HEX02"  length:"HEX, type, length);
	o = memNewStaticObject (memMakeDescriptor(type, length),
	                    memVectorLengthToObjectSize(length));
	DBEND();
	return o;
}

Obj memNewArray (Type type, Length length) {
 Obj o;
	DBBEG("  type:"HEX02"  length:"HEX, type, length);
	o = memNewObject (memMakeDescriptor(type, length),
	              memArrayLengthToObjectSize(length));
	DBEND();
	return o;
}

Obj memNewVector (Type type, Length length) {
 Obj o;
	DBBEG("  type:"HEX02"  length:"HEX, type, length);
	o = memNewObject (memMakeDescriptor(type, length),
	              memVectorLengthToObjectSize(length));
	assert(length);
	DBEND();
	return o;
}

/* Semaphore is a pair containing a semaphore counter (number) and a finalizer.
*/
Obj memNewSemaphore (void) {
 Obj o;
	DBBEG();
	o = memNewObject (memMakeDescriptor(TSEMAPHORE, 1),
	                  memVectorLengthToObjectSize(1));
	DBEND();
	return o;
}

/* Finalizer is a pair containg a C function pointer in the CAR and an object
   in the CDR.  The C function is called with the object pointer when it is
   realized to be garbage.
*/
Obj memNewFinalizer (void) {
 Obj o;
	DBBEG();
	o = memNewObject (memMakeDescriptor(TFINALIZER, 2),
	              memVectorLengthToObjectSize(2));
	heap.finalizerCount++;
	DBEND();
	return o;
}

/* Pointer object is a normal C pointer followed by a scheme object pointer
   which the first pointer should be pointing somewhere inside of.
*/
Obj memNewPointer (void) {
 Obj o;
	DBBEG();
	o = memNewObject (memMakeDescriptor(TPOINTER, 2),
	              memVectorLengthToObjectSize(2));
	DBEND();
	return o;
}

/* First item in stack vector is a byte offset to the top object.
   Initially it references itself as 0, implying an empty stack.
*/
Obj memNewVecStack (void) {
 Obj o;
	DBBEG();
	o = memNewObject (memMakeDescriptor(TVECSTACK, STACK_COUNT+1),
	                  memVectorLengthToObjectSize(STACK_COUNT+1));
	*(Num*)o = 1*ObjSize;
	DBEND();
	return o;
}

Obj memNewAryStack (void) {
 Obj o;
	DBBEG();
	o = memNewObject (memMakeDescriptor(TARYSTACK, (STACK_COUNT+1)*ObjSize),
	                  memVectorLengthToObjectSize(STACK_COUNT+1));
	*(Num*)o = 1*ObjSize;
	DBEND();
	return o;
}



/* Object heap queries.
*/
Num memIsObjectInHeap (Heap *heap, Obj o) {
	return (heap->start <= o) && (o < heap->last);
}

/* Check that object pointer is in a valid heap address. */
Num memIsObjectValid  (Obj o) {
	return memIsObjectInHeap(&heap, o)    ||
	       memIsObjectInHeap(&heapOld, o) ||
	       memIsObjectInHeap(&heapStatic, o);
}


/* Calculate a pointer object's index offset into the object it's pointing
   at. A pointer is a vector whos first element is a void* and the second
   element is the object the void* should be pointing somewhere inside of. */
Num memPointerOffset (Obj obj) {
	return (Num)(memVectorObject(obj, 0) - memVectorObject(obj, 1));
}


/* Object mutators
*/
void memArraySet (Obj obj, Num offset, u8 item) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		printf("ERROR memArraySet(obj "OBJ" offset "NUM" item %02x) Invalid object.",
		    obj, offset, item);
		memRaiseException();
	} else if(!memIsObjectBaseArray(obj)) {
		printf("ERROR memArraySet(obj "OBJ" offset "NUM" item %02x) Not array class.",
		    obj, offset, item);
		memRaiseException();
	} else if(memObjectLength(obj)<=offset ) {
		printf("ERROR memArraySet(obj "OBJ" offset "NUM" item %02x) Invalid offset.",
		    obj, offset, item);
		memRaiseException();
	}
	#endif
	*((u8*)obj+offset)=item;
}

void memVectorSet (Obj obj, Num offset, Obj item) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj) && !memIsObjectInHeap(&heapNew, obj)) {
		printf ("ERROR memVectorSet(obj "OBJ" offset "NUM" item "OBJ") Invalid object.",
		    obj, offset, item);
		memRaiseException();
	} else if (memIsObjectBaseArray(obj)) {
		printf ("ERROR memVectorSet(obj "OBJ" offset "NUM" item "OBJ") Not vector class.",
		    obj, offset, item);
		memRaiseException();
	} else if (memObjectLength(obj) <= offset ) {
		printf ("ERROR memVectorSet(obj "OBJ" offset "NUM" item "OBJ") Invalid offset.",
		    obj, offset, item);
		memRaiseException();
	}
	#endif
	*((Obj*)obj+offset)=item;
}

void memVecStackPush (Obj obj, Obj item) {
	#if DEBUG_ASSERT_STACK
	if (!memIsObjectValid(obj) && !memIsObjectInHeap(&heapNew, obj)) {
		printf ("ERROR memVecStackPush(obj "OBJ" item "OBJ") Invalid object.", obj, item);
		memRaiseException();
	} else if (!memIsObjectVecStack(obj)) {
		printf ("ERROR memVecStackPush(obj "OBJ" item "OBJ") Not a vector-stack type.", obj, item);
		memRaiseException();
	} else if (STACK_COUNT <= memVecStackLength(obj)) {
		printf ("\nERROR: Stack Overflow:  memVecStackPush(obj, item)\n", obj, item);
		memPrintObject(obj, stderr);
		printf("\n");
		memPrintObject(item, stderr);
		memRaiseException();
	} else
	#endif
	{
		*(Obj*)(obj + *(Num*)obj) = item;
		*(Num*)obj += ObjSize;
	}
}

void memVecStackSet (Obj obj, Num topOffset, Obj item) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		printf ("ERROR memVecStackSet(obj "OBJ" topOffset %x item "OBJ") Invalid object.",
		    obj, topOffset, item);
		memRaiseException();
	} else if (!memIsObjectVecStack(obj)) {
		printf ("ERROR memVecStackSet(obj "OBJ" topOffset %x item "OBJ") Not a vector-stack type.",
		    obj, topOffset, item);
		memRaiseException();
	} else if (memVecStackLength(obj)<=topOffset) {
		printf ("ERROR memVecStackSet(obj "OBJ" topOffset %x item "OBJ") Invalid offset. memVecStackLength(obj)=>%x", obj, topOffset, item, memVecStackLength(obj));
		memRaiseException();
	}
	#endif

	*(Obj*)(obj + *(Num*)obj - (topOffset + 1)*ObjSize) = item;
}

Obj  memVecStackPop (Obj obj) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj) && !memIsObjectInHeap(&heapNew, obj)) {
		printf ("ERROR memVecStackPop(obj "OBJ") Invalid object.", obj);
		memRaiseException();
	} else if (!memIsObjectVecStack(obj)) {
		printf ("ERROR memVecStackPop(obj "OBJ") Not a vector-stack type.", obj);
		memRaiseException();
	} else if (memVecStackLength(obj) < 1 || STACK_COUNT < memVecStackLength(obj)) {
		printf ("ERROR: Stack out of bounds:  memVecStackPop(obj "OBJ")", obj);
		memRaiseException();
	}
	#endif
	*(Num*)obj -= ObjSize;
	return *(Obj*)(obj + *(Num*)obj);
}

void memAryStackPush (Obj obj, Obj item) {
	#if DEBUG_ASSERT_STACK
	if (!memIsObjectValid(obj) && !memIsObjectInHeap(&heapNew, obj)) {
		printf ("ERROR memAryStackPush(obj "OBJ" item "NUM") Invalid object.", obj, item);
		memRaiseException();
	} else if (!memIsObjectAryStack(obj)) {
		printf ("ERROR memAryStackPush(obj "OBJ", item "NUM") Not an array-stack type.", obj, item);
		memPrintObject(obj, stderr);
		printf("\n");
		memPrintObject((Obj)item, stderr);
		memRaiseException();
	} else if (STACK_COUNT <= memAryStackLength(obj)) {
		printf ("\nERROR: Stack Overflow:  memAryStackPush(obj "OBJ", item"NUM")\n", obj, item);
		memPrintObject(obj, stderr);
		printf("\n");
		memRaiseException();
	} else
	#endif
	{
		*(Obj*)(obj + *(Num*)obj) = item;
		*(Num*)obj += ObjSize;
	}
}

void memAryStackSet (Obj obj, Num topOffset, Obj item) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		printf ("ERROR memAryStackSet(obj "OBJ" topOffset %x item "NUM") Invalid object.",
		    obj, topOffset, item);
		memRaiseException();
	} else if (!memIsObjectAryStack(obj)) {
		printf ("ERROR memAryStackSet(obj "OBJ" topOffset %x item "NUM") Not an array-stack type.",
		    obj, topOffset, item);
		memRaiseException();
	} else if (memAryStackLength(obj)<=topOffset) {
		printf ("ERROR memAryStackSet(obj "OBJ" topOffset %x item "NUM") Invalid offset. memAryStackLength(obj)=>%x", obj, topOffset, item, memAryStackLength(obj));
		memRaiseException();
	}
	#endif
	*(Obj*)(obj + *(Num*)obj - (topOffset+1)*ObjSize) = item;
}

Obj memAryStackPop (Obj obj) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj) && !memIsObjectInHeap(&heapNew, obj)) {
		printf ("ERROR memAryStackPop(obj "OBJ") Invalid object.", obj);
		memRaiseException();
	} else if (!memIsObjectAryStack(obj)) {
		printf ("ERROR memAryStackPop(obj "OBJ") Not an array-stack type.", obj);
		memRaiseException();
	} else if (memAryStackLength(obj) < 1 || STACK_COUNT < memAryStackLength(obj)) {
		printf ("ERROR: Stack out of bounds:  memAryStackPop(obj "OBJ")", obj);
		memRaiseException();
	}
	#endif
	*(Num*)obj -= ObjSize;
	return *( (Obj*)obj + (*(Num*)obj) );
}



/* Object accessors.
 */
u8 memArrayObject (Obj obj, Num offset) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		printf ("ERROR memArrayObject(obj "OBJ" offset "NUM") Invalid object.",
		    obj, offset);
		memRaiseException();
	} else if (!memIsObjectBaseArray(obj)) {
		printf ("ERROR memArrayObject(obj "OBJ" offset "NUM") Not array class.",
		    obj, offset);
		memRaiseException();
	} else if (memObjectLength(obj)<=offset ) {
		printf ("ERROR memArrayObject(obj "OBJ" offset "NUM") Invalid offset.",
		    obj, offset);
		memRaiseException();
	}
	#endif
	return *((u8*)obj+offset);
}

//extern Obj r1e;
Obj memVectorObject (Obj obj, Num offset) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj) && !memIsObjectInHeap(&heapNew, obj)) {
		printf ("ERROR memVectorObject(obj "OBJ" offset "NUM") Invalid object.",
		    obj, offset);
		memRaiseException();
	} else if (memIsObjectBaseArray(obj)) {
		printf ("ERROR memVectorObject(obj ["OBJ"] "OBJ" offset "NUM") Not vector class.",
			memObjectDescriptor(obj),
		   obj, offset);
//		objDump(obj, stderr);
//		vmDebugDumpCode(r1e, stderr);
		memRaiseException();
	} else if (memObjectLength(obj)<=offset ) {
		printf ("ERROR memVectorObject(obj "OBJ" offset "NUM") Invalid index.",
		    obj, offset);
		memRaiseException();
	}
	#endif
	return *((Obj*)obj + offset);
}

Obj memVecStackObject (Obj obj, Num topOffset) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		printf ("ERROR memVecStackObject(obj "OBJ" topOffset "NUM"): Invalid object.",
		    obj, topOffset);
		memRaiseException();
	} else if (!memIsObjectVecStack(obj)) {
		printf ("ERROR memVecStackObject(obj "OBJ" topOffset "NUM"): Not stack type.",
		    obj, topOffset);
		memRaiseException();
	} else if (memVecStackLength(obj)<=topOffset ) {
		printf ("ERROR memVecStackObject(obj "OBJ" topOffset "NUM"): Invalid top offset.",
		    obj, topOffset);
		memRaiseException();
	}
	#endif
	return *(Obj*)(obj + *(Num*)obj - (topOffset+1)*ObjSize);
}

/* Number of elements pushed onto stack.  It is empty if the pointer address
   is the same as the object's address.
*/
Num memVecStackLength (Obj obj) {
	#if DEBUG_ASSERT_STACK
	if (!(memIsObjectInHeap(&heap, obj)
	      || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj)
		   || memIsObjectInHeap(&heapNew, obj))) {
		printf("ERROR memVecStackLength(obj "OBJ") Invalid object.", obj);
		memRaiseException();
	} else if (!memIsObjectVecStack(obj)) {
		printf("ERROR memVecStackLength(obj "OBJ") Not stack type.", obj);
		memRaiseException();
	}
	#endif
	return *(Num*)obj/ObjSize - 1;
}

Obj memAryStackObject (Obj obj, Num topOffset) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		printf ("ERROR memAryStackObject(obj "OBJ" topOffset "NUM"): Invalid object.",
		    obj, topOffset);
		memRaiseException();
	} else if (!memIsObjectAryStack(obj)) {
		printf ("ERROR memAryStackObject(obj "OBJ" topOffset "NUM"): Not stack type.",
		    obj, topOffset);
		memRaiseException();
	} else if (memAryStackLength(obj)<=topOffset ) {
		printf ("ERROR memAryStackObject(obj "OBJ" topOffset "NUM"): Invalid top offset.",
		    obj, topOffset);
		memRaiseException();
	}
	#endif
	//return *(*(Obj**)obj-topOffset);
	return *(Obj*)(obj + *(Num*)obj - (topOffset+1)*ObjSize);
}

/* Number of elements pushed onto stack.  It is empty if the pointer address
   is one word past the object's address (vector index 1).
*/
Num memAryStackLength (Obj obj) {
	#if DEBUG_ASSERT_STACK
	if (!(memIsObjectInHeap(&heap, obj)
	      || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj)
		   || memIsObjectInHeap(&heapNew, obj))) {
		printf("ERROR memAryStackLength(obj "OBJ") Invalid object.", obj);
		memRaiseException();
	} else if (!memIsObjectAryStack(obj)) {
		printf("ERROR memAryStackLength(obj "OBJ") Not stack type.", obj);
		memRaiseException();
	}
	#endif
	return *(Num*)obj - 1*ObjSize;
}



/*******************************************************************************
 Root_Set

 Register the address of an object pointer for the garbage collector to be
 included in the root set during a garbage collection.  Registered pointers
 can be popped/un-registered which allows temporary root pointers.

*******************************************************************************/
#define MemRootSetCountMax 64
Obj *MemRootSet[MemRootSetCountMax];
Num MemRootSetCount = 0;

void memRootSetAddressRegister (Obj *objp) {
	assert(MemRootSetCount < MemRootSetCountMax);
	MemRootSet[MemRootSetCount++] = objp;
}

void memRootSetAddressUnRegister (Obj *objp) {
	assert(0 < MemRootSetCount);
	assert(objp == MemRootSet[--MemRootSetCount]);
}



/*******************************************************************************
 Garbage_collector
*******************************************************************************/

/* Ring buffer.  Used for debugging internal behavior.
*/ 
#if 0
const Int RBUFMAX=0x100;
Int rbufi=0;
Int rbuf[0x100]={0};
const Int RBUFTOP=-1;

void memRbufDump (void) {
 Num i;
	for (i=0; i<RBUFMAX; ++i)
		fprintf (stderr, INT" ", rbuf[i]);
}

void memRbufAdd (Int p) {
	rbuf[rbufi++]=p;
	if (rbufi==RBUFMAX) rbufi=0;
	rbuf[rbufi]=RBUFTOP;
}
#endif

const Num GC_MODE_YOUNG=0;
const Num GC_MODE_OLD=1;

Num GarbageCollectionMode;
Num garbageCollectionCount = 0;

/* Function pointers called before and after a GC proper.  */
Func memPreGarbageCollect = 0;
Func memPostGarbageCollect = 0;



/* Given pointer to an object, move object into the new heap and update
   the object to point to new address.  Object must be valid and exist in
   either the old or young heap so this check must be done before calling.
*/
void memObjectCopyToHeapNew (Obj *objp) {
 Obj newObjectLocation;
 Num len;
	DBBEG();
	/* Only move objects from concerned generation.  Either young heap or both
	   young and old heap. */
	if (!(memIsObjectInHeap(&heap, *objp)
	      || (GarbageCollectionMode==GC_MODE_OLD
	          && memIsObjectInHeap(&heapOld, *objp)))) {
		//DB ("    Ignoring");
		goto ret;
	}

	//fprintf(stderr, "\n  location:"OBJ"  obj:"OBJ, objp, *objp);

	/* If shadow object, grab the forwarding pointer which is found in the
	   first word, otherwise copy object over. */
	if (memIsObjectShadow (*objp)) {
		DB ("Shadow");
		newObjectLocation = memVectorObject(*objp, 0);
	} else {
		/* Next object location in new heap. */
		newObjectLocation = heapNew.next + DescSize;
	
		/* Copy bytes that make up the stack which include the descriptor,
		   pointer and live object pointers... */
		if (memIsObjectVecStack (*objp)) {
			DB ("Stack");
			len = memVecStackLength(*objp);
			/* Add 1 to stack length to account for the stack length in the first location. */
			memcpy(heapNew.next, (*objp - DescSize), (DescSize + ObjSize * (len + 1)));
			/* Update this stack's internal pointer (first word in the vector) to
				point to the proper position in the vector. */
			//*(Obj*)newObjectLocation = (Obj*)newObjectLocation + len;
		} else { /* ... or just copy bytes that make up this object. */
			DB ("Array/Vector  heapNew.next %p  *objp-8 %p  memObjectSize(*objp) %x", heapNew.next, *objp-DescSize, memObjectSize(*objp));
			memcpy(heapNew.next, *objp-DescSize, memObjectSize(*objp));
			/* Tally copied dirty objs. */
			if (memIsObjectFinalizer(*objp)) heapNew.finalizerCount++;
		} /* TODO handle pointer objects */
	
		/* Mutate this object into a shadow object.  This involves
		   mutating the descriptor's type into a shadow and length into the
		   proper vector length.  This length is only used for debugging
			purposes when traversing over a heap containing shadow types... */
		DB("Descriptor from "HEX" to "HEX, memObjectDescriptor(*objp), memMakeDescriptor(TSHADOW, memObjectSize(*objp)/DescSize - 1));
		*((Descriptor*)*objp-1) = memMakeDescriptor(TSHADOW, memObjectSize(*objp)/DescSize - 1);
		/* ... and mutate object's first word into a pointer to the new
		   location in the new heap thus completing shadow type mutation. */
		*(Obj*)*objp = newObjectLocation;
	
		/* Increment new heap's next pointer, the next spot for any new copied
		   or created object. */
		heapNew.next += memObjectSize(*objp);
	}

	/* Using the pointer to the object, mutate the object so that it points at the
	   new location in the new heap. */
	*objp = newObjectLocation;

	/* The new heap should always be big enough for a full garbage collection. */
	assert(heapNew.next < heapNew.last);

	ret:
	DBEND();
	return;
} /* memObjectCopyToHeapNew */


/* Compact objects referenced by base-vector type objects.  Special attention
   is given to stacks and pointers.  In a young-only collection, the old
   objects are root set so each object in the old heap must be checked for
   references into the young heap. */
void memCopyHeapObjectsToHeapNew (Heap *heap) {
 Obj newObj;
 Num len, i;
	DB("COLLECTING OBJECTS REFERENCED BY OLD HEAP OBJECTS");
	newObj = heap->start + DescSize;
	while (newObj < heap->next) {
		if (!memIsObjectBaseArray(newObj)) {
			if (memIsObjectPointer(newObj)) {
				DB("  Pointer in old heap");
				/* Figure out pointer offset. */
				len = memPointerOffset(newObj);
				/* Update pointer object's object pointer. */
				memObjectCopyToHeapNew((Obj*)newObj+1);
				/* Update pointer object's index pointer.  This should even work
				   if the object is in an old heap and a young GC is being
				   performed.*/
				memVectorSet(newObj, 0, (Obj)memVectorObject(newObj, 1)+len);
			} else if (memIsObjectVecStack(newObj)) {
				for (i=0; i<memVecStackLength(newObj); i++) {
					DB("  Stack in old heap   location "HEX"/"HEX" "OBJ, i, memVecStackLength(newObj), (Obj*)newObj+i+1);
					memObjectCopyToHeapNew((Obj*)newObj+i+1);
				}
			} else {
				for (i=0; i<memObjectLength(newObj); i++) {
					DB("  Vector in old heap   location "HEX"/"HEX" "OBJ, i, memObjectLength(newObj), (Obj*)newObj+i);
					memObjectCopyToHeapNew((Obj*)newObj+i);
				}
			}
		}
		/* Consider next object in old heap and continue compacting. */
		newObj += memObjectSize(newObj);
	}
} /* memCopyHeapObjectsToHeapNew  */


/* If a finalizer oject is found in the heap, call it. */
void memScanAndCallFinalizers(Heap *heap) {
 Obj o;
	o = heap->start + DescSize;
	while (o < heap->next) {
		if (memObjectType(o) == TFINALIZER) {
			DB("  Found finalizer "OBJ" in young heap", o);
			(*(Func1*)o)(((Obj*)o)[1]);
			--(heap->finalizerCount);
		}
		o += memObjectSize(o);
	}
}


/* Internal garbage collection call.  Passed in for debugging is the
   desired new object descriptor as well as the number of bytes used
   to represent (descriptor byte count included) the object.
*/
void memGarbageCollectInternal (Descriptor desc, Num byteSize) {
 static Num memGCFlag=0;
 Num newHeapSize, i;

	/* Verify no recursive call. */
	assert(memGCFlag==0);
	memGCFlag=1;

// It doesn't make sense to validate the heap before a GC.  It might contain, at least, a spent finalizer
//#if VALIDATE_HEAP
//	memValidateHeapStructures();
//#endif

	if (GC_MODE_YOUNG == GarbageCollectionMode) {
		DBBEG("  mode=YOUNG  old_count:"HEX"  young_count:"HEX, memHeapBlockCount(&heapOld), memHeapBlockCount(&heap));
	} else {
		DBBEG("  mode=OLD    old_count:"HEX"  young_count:"HEX, memHeapBlockCount(&heapOld), memHeapBlockCount(&heap));
	}

	++garbageCollectionCount;

	/* Initialize heap 'new' with enough space to contain all of heap 'old'
	   and double heap 'young's objects plus the blocks needed to contain
	   the desired object size.  This will eventually become a new 'old' heap
	   or young heap.  The block count is shrunk to fit perfectly the old heap's
	   block usage.  The young heap is shrunk to double a constant value.  If
	   the usage is half this 'double' size, then the next GC will be a complete
	   one (collect over young and old).
	*/
	newHeapSize = (GarbageCollectionMode==GC_MODE_OLD ? memHeapBlockCount(&heapOld) : 0) +
	              memHeapBlockCount(&heap) +
	              HEAP_BLOCK_COUNT +
	              byteSize/BLOCK_BYTE_SIZE;
	DB ("[mode "NUM"][newHeap byte size "NUM"][desired "NUM"]", GarbageCollectionMode, memHeapBlockCount(&heap), byteSize/BLOCK_BYTE_SIZE);
	memInitializeHeap (&heapNew, newHeapSize);

	if (memPreGarbageCollect) memPreGarbageCollect();

	/* Copy objects in registers to new heap. */
	DB("Collecting root set");
	for (i=0; i<MemRootSetCount; ++i) memObjectCopyToHeapNew(MemRootSet[i]);

	/* Treat objects in heapOld as a root set. This is temporary until I re-enable
	   the mutated:old-object list (commented code below). */
	if (GarbageCollectionMode==GC_MODE_YOUNG) {
		memCopyHeapObjectsToHeapNew(&heapOld);
	}

	/* TODO Compact the objects referenced by mutated objects in the old heap.
	   Also optimize this list by compacting this list while were here by
	   removing copies of the same references.
	if (GarbageCollectionMode == GC_MODE_YOUNG) {
		DB("   collecting and compacting mutated old object references...");
		for (i=0; i<mutatedOldObjectsLength; i++) {
			// Compact
			if (memIsObjectInHeap(&heapNew, *mutatedOldObjects[i])) {
				mutatedOldObjects[i]
				= mutatedOldObjects[--mutatedOldObjectsLength];;
			// Collect object this points to.
			} else {
				memObjectCopyToHeapNew(mutatedOldObjects[i]);
			}
		}
	} */
//memPrintHeapNew(stderr);
	/* The new heap is now a root set.  Continue compacting on these objects */
	memCopyHeapObjectsToHeapNew(&heapNew);

	/* Check finalizer count. */
	if (heapNew.finalizerCount != (heap.finalizerCount + (GarbageCollectionMode == GC_MODE_OLD
	                                                      ? heapOld.finalizerCount : 0))) {
		DB ("FINALIZER COUNT DIFFERENCE DETECTED");

		memScanAndCallFinalizers(&heap);

		if (GarbageCollectionMode == GC_MODE_OLD) {
			memScanAndCallFinalizers(&heapOld);
		}
	}

	DB("FREEING AND SHRINKING");

	/* Free up heap and old heap if doing an old heap collection.  The live
	   objects have all been copied over to the new heap which will be reassigned
	   as the young new-incomming-objects heap. */
	memFreeHeap (&heap);

	if (GarbageCollectionMode == GC_MODE_OLD) {
		/* Since this is a full garbage collection the old heap is freed as well. */
		memFreeHeap (&heapOld);
		/* The heapNew's bounds are shrunk to fit before it is assigned
		   as new "old heap" */
		memShrinkHeap(&heapNew, (Num)(heapNew.last-heapNew.next)/BLOCK_BYTE_SIZE);
		heapOld = heapNew;
		memResetHeapObject(&heapNew); /* Reset the new heap's interal data */
		/*  A brand new young heap is created for new object creation */
		memInitializeHeap (&heap, HEAP_BLOCK_COUNT + byteSize/BLOCK_BYTE_SIZE);
		GarbageCollectionMode = GC_MODE_YOUNG;
		//mutatedOldObjectsLength = 0; /* TODO */
	} else { /* GarbageCollectionMode == GC_MODE_YOUNG */
		/* Reassign new heap to young heap. */
		heap = heapNew;
		memResetHeapObject(&heapNew); /* Reset the new heap's interal data */

		/* Shrink the heap by HEAP_BLOCK_COUNT if the free space is greater than the used */
		if (memHeapBlockCount(&heap)/2 < ((Num)(heap.last-heap.next) - byteSize) / BLOCK_BYTE_SIZE) {
			DB ("  [shrinking "NUM" by "NUM"]", memHeapBlockCount(&heap), HEAP_BLOCK_COUNT / 1);
			memShrinkHeap(&heap, HEAP_BLOCK_COUNT);
		}
		/* Repeat for the desire block size */
		if (memHeapBlockCount(&heap)/2 < ((Num)(heap.last-heap.next) - byteSize) / BLOCK_BYTE_SIZE) {
			DB ("  [shrinking by "NUM"]",byteSize/BLOCK_BYTE_SIZE); 
			memShrinkHeap(&heap, byteSize/BLOCK_BYTE_SIZE);
		}
	}

	if (garbageCollectionCount % FORCE_OLD_GC_COUNT == 0)
		GarbageCollectionMode = GC_MODE_OLD;

	assert(memGCFlag==1);
	memGCFlag=0;

#if VALIDATE_HEAP
	memValidateHeapStructures();
#endif

	if (memPostGarbageCollect) memPostGarbageCollect();

	/* Debug dump heap [used | unused]objCount info
	 */
	DB("heapOld["HEX" | "HEX"]"HEX"  heapYoung["HEX" | "HEX"]"HEX, memHeapUsedSize(&heapOld), memHeapUnusedSize(&heapOld), memHeapLength(1), memHeapUsedSize(&heap), memHeapUnusedSize(&heap), memHeapLength(2));
	DBEND("  heapOld["NUM"]  heapYoung["NUM"]", memHeapBlockCount(&heapOld), memHeapBlockCount(&heap));
} /* memGarbageCollectInternal */


/* External call not triggered on lack of space so no descriptor nor byteSize passed
*/
void memGarbageCollect () {
	memGarbageCollectInternal(0, 0);
}



/*******************************************************************************
 Type_Strings

 Object types are defined by 8 bits.  The valid range/count for the core
   types are as follows (all values in hex):
     arrays   00...7f / 80
     vector   80...ef / 70
     internal f0...ff / 10  (not useable by user)
*******************************************************************************/
Str memTypeStringDB[TMAXINTERNAL+1];

void memTypeStringRegisterInternal (Type type, Str description) {
	DBBEG("  Type type="HEX"  char *description="STR, type, description);

	assert(strlen((char*)description) <= 9);
	assert(type <= TMAXINTERNAL);

	/* Object types can't be redefined */
	if (memTypeStringDB[type] != NULL) {
		fprintf (stderr, "Trying to register type "NUM":"STR" but assigned to "STR, type, description, memTypeStringDB[type]);
		exit(-1);
	}

	memTypeStringDB[type] = description;
	DBEND();
}

/* Only allow user to register arrays and vector types (not internal use only types)
 */
void memTypeStringRegister (Type type, Str description) {
	assert(type <= TMAXVECTOR); /* Only arrays and vector can be created externally */
	memTypeStringRegisterInternal(type, description);
}

/* Get type's string description
 */
Str memTypeString (Type t) {
	DB("Type t="NUM, t);
	assert(t <= TMAXINTERNAL);
	return memTypeStringDB[t]
		?  memTypeStringDB[t]
		: (Str)"?????";
}



/*******************************************************************************
 Address_Strings

 Simple table insertion and lookup of a string to a pointer value aka object
*******************************************************************************/
#define OBJ_STR_MAX 512
typedef struct {Obj obj;  Str str;} ObjStr;
ObjStr memObjStrDB[OBJ_STR_MAX];
Num ObjStrCount = 0;

/* Assign a C address with a C string.  Useful for assigning a descriptive
   string to a C function, C variable address or static scheme object.

   The macro memAddressRegister(o) is provided which will pass the C variable
   and string representation to this function.
*/
void memAddressStringRegister (Obj obj, Str str) {
	DBBEG("  Registering "OBJ"  string="STR" on existing str="STR, obj, str, memAddressString(obj));
	assert(NULL == memAddressString(obj));
	assert(ObjStrCount < OBJ_STR_MAX);
	memObjStrDB[ObjStrCount].obj = obj;
	memObjStrDB[ObjStrCount++].str = str;
	DBEND();
}

/* Retrieve string associated with the pointer object
*/
Str memAddressString (Obj obj) {
 Num i;
	for (i=0; i<ObjStrCount; ++i) {
		if (memObjStrDB[i].obj == obj) return memObjStrDB[i].str;
	}
	return NULL;
}



/*******************************************************************************
 Debugging_And_Printing
*******************************************************************************/

/* Dump heap and root object details
*/
void memPrintStructures (FILE *stream) {
	// Set a default stream.
	if (stream == NULL) stream=stderr;

	fprintf (stream, "\n-- memPrintStructures ----\n       Static       Old          Current      New\n");
	fprintf (stream, " Start "OBJ" "OBJ" "OBJ" "OBJ"\n",
	        heapStatic.start,  heapOld.start,
	        heap.start,  heapNew.start);
	fprintf (stream, " Next  "OBJ" "OBJ" "OBJ" "OBJ"\n",
	        heapStatic.next,  heapOld.next,
	        heap.next,  heapNew.next);
	fprintf (stream, " last  "OBJ" "OBJ" "OBJ" "OBJ"\n",
	        heapStatic.last,  heapOld.last,
	        heap.last,  heapNew.last);
	fprintf (stream, "  Size   %10x   %10x   %10x   %10x\n",
	        heapStatic.last-heapStatic.start,  heapOld.last-heapOld.start,
	        heap.last-heap.start,              heapNew.last-heapNew.start);
	fprintf (stream, "  Blocks %10lx   %10lx   %10lx   %10lx\n",
	        memHeapBlockCount(&heapStatic),  memHeapBlockCount(&heapOld),
	        memHeapBlockCount(&heap),        memHeapBlockCount(&heapNew));
	fprintf (stream, "  Objects%10lx   %10lx   %10lx   %10lx\n",
	        memHeapLength(0),  memHeapLength(1),
	        memHeapLength(2),  memHeapLength(3));
	fprintf (stream, "  Finalizer%8lx   %10lx   %10lx   %10lx",
	        heapStatic.finalizerCount,  heapOld.finalizerCount,
	        heap.finalizerCount,        heapNew.finalizerCount);
#if 0
	if (stk) // Display the top 5 stack elements
	{
		i = memVecStackLength(stk);
		fprintf(stream, NL HEX04"[", i);
		if (6 < i) {
			i = 6;
			fprintf(stream, "...");
		}
		while (0<i) {
			fprintf(stream, OBJ " ", memVecStackObject(stk, i-1));
			--i;
		}
	}

	for (i=0; i<MemDataSetCount; ++i) {
		if (0 == i%4) printf ("\n");
		o = *MemDataSet[i];
		fprintf (stream, "%5s "OBJ0, memAddressString(MemDataSet[i]), o);
	}

	if (istk) // Display the top 5 integer stack elements
	{
		i = memAryStackLength(istk);
		fprintf(stream, NL HEX04"[", i);
		if (6 < i) {
			i = 6;
			fprintf(stream, "...");
		}
		while (0<i) {
			fprintf(stream, OBJ " ", memAryStackObject(istk, i-1));
			--i;
		}
	}
#endif
}

void memPrintRootSet (FILE *stream) {
 Chr buff[17];
 Num i;
 Obj *op, o;
// Obj stk=NULL;
// Obj istk=NULL;
 Str str;
	if (stream == NULL) stream=stderr;

	fprintf (stream, "\n-- memPrintRootSet ----");

	for (i=0; i<MemRootSetCount; ++i) {
		printf ("\n");
		op = MemRootSet[i];
		o = *op;
//		if (memIsObjectVecStack(o)) { stk = o; } // Look for stack
//		if (memIsObjectAryStack(o)) { istk = o; } // Look for istack

		// Print the string description and root object's address
		fprintf(stream, " *"OBJ" == ", op);

		// Print the object's value (simple value)
		sprintf ((char*)buff, "%012llX", o);
		fprintf (stream, "%-16s ", buff);

		// Consider the root object's address string description
		str = memAddressString(op);
		if (str) fprintf(stream, "\"%s\"", str);
	}

}

void memPrintTypes (FILE *stream) {
 Num i=0;
	if (stream == NULL) stream=stderr;
	fprintf (stream, "\n-- memPrintTypes ----\n");
	for (i=0; i<=TMAXINTERNAL; ++i) {
		if (memTypeStringDB[i])
			fprintf(stream, " "HEX02":\""STR"\"", i, memTypeStringDB[i]);
		if (TMAXARRAY==i || TMAXVECTOR==i) fprintf (stream, "\n");
	}
}

void memPrintAddresses (FILE *stream) {
 Num i, j, isRoot;
 Str s;
	if (stream == NULL) stream=stderr;
	fprintf (stream, "\n-- memPrintAddresses ----");
	for (i=0; i<OBJ_STR_MAX; ++i) {
		s = memObjStrDB[i].str; 
		if (s) {

			// Index
			fprintf(stream, NL SP HEX03 SP, i);

			// Is it a root node address?
			for (isRoot=j=0; !isRoot && j<MemRootSetCount; ++j) { isRoot = (MemRootSet[j] == memObjStrDB[i].obj); }

			fprintf(stream, OBJ SP STR " \"" STR "\"", memObjStrDB[i].obj, isRoot?"root":"    ", s);
		}
	}
}

/* Generic object to string dumper.
   #<11112222f000 y  int:02 0004  (61 62 63 64) "abcd">
   #<11112222f010 y pair:02 0002 #(11112222f000 11112222f010)>
*/
void memPrintObject (Obj o, FILE *stream) {
 Int i, fdState;
 Num len;
 Str s;
 Obj obj;
 Chr c;

	/* Always expect a stream */
	assert(stream);
	/* Temporarily disable non-blocking mode on the I/O channel.  Affects both input/output. */
	fdState = fcntl(0, F_GETFL, 0);
	fcntl (0, F_SETFL, fdState&~O_NONBLOCK);

	/* The object's address  "#<XXXXXXXXXXXX" */
	fprintf(stream, "#<"OBJ, o);

	/* If object is in any valid heap, display the object's meta data and actual content */
	if (memIsObjectValid(o) || memIsObjectInHeap(&heapNew, o)) {
		/* The object's metadata  " H TYPE:XX XXXX" */
		fprintf (stream, " "CHR" "STR":"HEX02" "HEX04" ",
			memIsObjectInHeap(&heapStatic, o)?'s':
				memIsObjectInHeap(&heapOld, o)?'o':
					memIsObjectInHeap(&heap, o)?'y':
						memIsObjectInHeap(&heapNew, o)?'n':'?',
			memTypeString(memObjectType(o)),
			memObjectType(o),
			memObjectLength(o));
			
 		if (!memObjectLength(o)) {
			/* Some objects are just an instance of the descriptor such as #f and #() they have no content. */
			fprintf(stderr, (char*)memAddressString(o));
		} else if (memIsObjectBaseArray(o)) {
         if (memIsObjectAryStack(o)) {
				/* Array stack's ontents */
				len = memAryStackLength(o);
				fprintf (stream, "("HEX04" |", len);
				if (STACK_COUNT < len) { len = STACK_COUNT; }
				for (i=0; (i < len); i++) {
					fprintf (stream, " %x", ((Num*)o)[i+1]);
				}
				fprintf (stream, ")");
         } else {
				/* Array's contents generically dumped */
				for (i=0; i<memObjectLength(o); i++) {
					fprintf (stream, "%s%x", i==0?"(":" ", ((u8*)o)[i]);
				}
				fprintf (stream, ") \"");
				for (i=0; i<memObjectLength(o); i++) {
					c = ((Chr*)o)[i];
					fprintf (stream, CHR, ((32<=c && c<=126) || (161<=c && c<=255)) ? c : '.'); // 0xb7
				}
				fprintf (stream, "\"");
			}
		} else {
			if (memIsObjectFinalizer(o)) {
				/* Finalizer */
				fprintf (stream, "#("HEX":"STR" . "OBJ")", ((Obj*)o)[0], memAddressString(((Obj*)o)[0]), ((Obj*)o)[1]);
			} else if (memIsObjectPointer(o)) {
				/* Pointer */
				fprintf (stream, OBJ" "OBJ"["NUM"]",((Obj*)o)[0], ((Obj*)o)[1], ((Obj*)o)[0] - ((Obj*)o)[1]);
			} else if (memIsObjectVecStack(o)) {
				/* Vector Stack */
				len = memVecStackLength(o);
				fprintf (stream, "["HEX04" |", len);
				if (STACK_COUNT < len) { len = STACK_COUNT; }
				for (i=0; (i < len); i++) {
					fprintf (stream, " "OBJ, ((Obj*)o)[i+1]);
				}
				fprintf (stream, "]");
			} else if (memIsObjectShadow(o)) {
				/* Shadow */
				fprintf (stream, "*"OBJ"", *(Obj*)o);
			} else {
				/* Vector's contents generically dumped */
				for (i=0; i<memObjectLength(o); i++) {
					obj = ((Obj*)o)[i];
					fprintf (stream, "%s"HEX, ((i==0)?"#(":" "), obj);
					/* Internal pointer address */
					if ((s = memAddressString(obj))) fprintf (stream, ":%s", s);
				}
				fprintf (stream, ")");
			}
		}
	}

	fprintf (stream, ">");

	fcntl (0, F_SETFL, fdState);
}

void memPrintHeapStatic (FILE *stream) {
 Obj o;
	if (stream == NULL) stream=stderr;
	fprintf (stream, "\n--MEM Static Heap----");
	o = heapStatic.start + DescSize;
	while (o < heapStatic.next) {
		fprintf (stream, "\n");
		memPrintObject (o, stream);
		o += memObjectSize(o);
	}
}

void memPrintHeapOld (FILE *stream) {
 Obj o;
	if (stream == NULL) stream=stderr;
	fprintf (stream, "\n--MEM Old Heap----");
	o = heapOld.start + DescSize;
	while (o<heapOld.next) {
		fprintf (stream, "\n");
		memPrintObject (o, stream);
		o += memObjectSize(o);
	}
}

void memPrintHeapYoung (FILE *stream) {
 Obj o;
	if (stream == NULL) stream=stderr;
	fprintf (stream, "\n--MEM Young Heap----");
	o = heap.start + DescSize;
	while (o<heap.next) {
		fprintf (stream, "\n");
		memPrintObject (o, stream);
		o += memObjectSize(o);
	}
}

void memPrintHeapNew (FILE *stream) {
 Obj o;
	if (stream == NULL) stream=stderr;
	fprintf (stream, "\n--MEM New Heap----");
	o = heapNew.start + DescSize;
	while (o<heapNew.next) {
		fprintf (stream, "\n");
		memPrintObject (o, stream);
		o += memObjectSize(o);
	}
}

void memPrintHeaps (FILE *stream) {
	memPrintHeapStatic(stream);
	memPrintHeapOld(stream);
	memPrintHeapYoung(stream);
	memPrintHeapNew(stream);
}

void memPrintAll (FILE *stream) {
	DBBEG();
	memPrintStructures(stream);
	memPrintRootSet(stream);
	memPrintTypes(stream);
	memPrintAddresses(stream);
	memPrintHeaps(stream);
	/* TODO
	for (i=0; i<mutatedOldObjectsLength; i++) {
		fprintf (stream " "OBJ, mutatedOldObjects[i]);
	}
	*/
	DBEND();
}


Int memValidateObject (Obj o) {
 Obj oo, op;
 Int valid=1;
 Num i, stackLen;
//	DBBEG();
	
	if ((memIsObjectInHeap(&heapOld, o)
		  || memIsObjectInHeap(&heap, o)
		  || memIsObjectInHeap(&heapStatic, o))
		 && memObjectLength(o)
		 && !memIsObjectBaseArray(o)) {
		if (memIsObjectPointer(o)) {
			oo = memVectorObject(o, 1);
			op = memVectorObject(o, 0);
			/* Verify pointer is into an object in the current heap. */
			if (!memIsObjectInHeap(&heap, oo)) {
				fprintf (stderr, "\nERROR memValidateObject() pointer "OBJ" is to invalid object ", o);
				valid=0;
			}
			/* Verify pointer is within object */
			if (!((oo <= op)	
			      && (op < oo + memObjectSize(oo) - DescSize))) {
				fprintf (stderr, "\nERROR memValidateObject() pointer "OBJ" is out of object range ", o);
				valid=0;
			}
		} else if (memIsObjectVecStack(o)) {
			//op = memVectorObject(o, 0);
			stackLen = memVecStackLength(o);
			//if (!(o <= op && (op < o + memObjectSize(o) - DescSize))) {
			if (STACK_COUNT < stackLen) {
				fprintf (stderr, "\nERROR memValidateObject() stack "OBJ" out of bounds. len:"NUM"", o, stackLen);
				valid=0;
			}
		} else if (memIsObjectShadow(o))
			fprintf (stderr, "\nERROR memValidateObject() found shadow "OBJ, o);
		else if (memIsObjectFinalizer(o)) {
			// Nothing wrong with finding a finalizer.
			//fprintf (stderr, "\nERROR memValidateObject() found finalizer "OBJ, o);
		} else if (memIsObjectBaseVectorOrArray(o)) /* The outer if verified this is not an array */
			for (i=0; i<memObjectLength(o); i++) {
				oo = memVectorObject(o, i);
				/* Originally immediate value range was 0xffff.  Allowing positive and negative
				   immediate values also include internal C code goto addresses which are used
				   as opcodes in the virtual machine. */
				if (!(memIsObjectInHeap(&heapOld, oo)
				      || memIsObjectInHeap(&heap, oo)
				      || memIsObjectInHeap(&heapStatic, oo))
				    && (oo > (Obj)0x500000 && oo < (Obj)-0x500000) /* Immediate values */
				    && !memAddressString(oo)) { /* Ignore registered internal pointers. */
					fprintf (stderr, "\nERROR memValidateObject() vector object "OBJ"["INT"]="OBJ" invalid.", o, i, oo);
					//wscmWrite(memVectorObject(o, 2), 0, 1);
					valid=0;
				}
			}
	}
	if (!valid) {
		fprintf (stderr, "\nERROR memValidateObject() found bad object:"OBJ NL, o);
		memPrintObject (o, stderr);
		__builtin_trap(); // *(int*)0=0;
	}
//	DBEND();
	return valid;
}

void memValidateHeapStructures (void) {
 Obj o;
 Int valid = 1;
	DBBEG();

	// Static Heap
	o = heapStatic.start + DescSize;
	while (o<heapStatic.next) {
		valid &= memValidateObject (o);
		o += memObjectSize(o);
	}

	o = heapOld.start + DescSize;
	while (o<heapOld.next) {
		valid &= memValidateObject (o);
		o += memObjectSize(o);
	}

	o = heap.start + DescSize;
	while (o<heap.next) {
		valid &= memValidateObject (o);
		o += memObjectSize(o);
	}

	if (!valid) { memPrintAll(stderr); }

	DBEND();
}



/*******************************************************************************
 Init
*******************************************************************************/
void memInitialize (Func preGC, Func postGC, Func exceptionHandlerCallback) {
 static Num shouldInitialize=1;
	DBBEG();
	if (shouldInitialize) {
		DB("Activating module");
		DB("Sanity assertions");
		assert(TMAXARRAY < TMAXVECTOR);
		assert(TMAXVECTOR < TMAXINTERNAL);
		assert(TMAXINTERNAL < MEMMAXTYPES);
		shouldInitialize=0;
		DB("Register the internal object types");
		memTypeStringRegisterInternal(TARYSTACK, (Str)"ASTK");
		memTypeStringRegisterInternal(TVECSTACK, (Str)"VSTK");
		memTypeStringRegisterInternal(TSEMAPHORE,(Str)"SEM");
		memTypeStringRegisterInternal(TFINALIZER,(Str)"FIN");
		memTypeStringRegisterInternal(TPOINTER,  (Str)"PTR");
		memTypeStringRegisterInternal(TSHADOW,   (Str)"SHDW");
		DB("Create heaps");
		memInitializeHeap  (&heap, HEAP_BLOCK_COUNT);
		memInitializeHeap  (&heapStatic, HEAP_STATIC_BLOCK_COUNT);
		memResetHeapObject (&heapOld); /* Old heap unused initially */
		GarbageCollectionMode = GC_MODE_YOUNG;
	} else {
		DB("Module already activated");
	}

	if (preGC) {
		DB("Setting preGC callback function");
		assert(!memPreGarbageCollect);
		memPreGarbageCollect = preGC;
	}

	if (postGC) {
		DB("Setting postGC callback function");
		assert(!memPostGarbageCollect);
		memPostGarbageCollect = postGC;
	}
	if (exceptionHandlerCallback) {
		DB("Setting exception handler callback function");
		assert(!memExceptionHandler);
		memExceptionHandler = exceptionHandlerCallback;
	}

	DBEND();
}



#undef DB_DESC
#undef DEBUG
