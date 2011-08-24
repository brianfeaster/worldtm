#define DEBUG 0
#define DB_DESC "MEM"
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <errno.h>
#include "mem.h"
#define DEBUG_ASSERT 0
#define DEBUG_ASSERT_STACK 0
#define VALIDATE_HEAP 0

/*
  Type_stuff
  Descriptors
  Heap_stuff
  Object_creation
  Garbage_collector
  Debugging_aids
  External_calls

  ABOUT THIS MODULE

   Two sets of objects, old and young,  will be maintained.  Normally, the
   young object heap will be copy collected, when it fills up, to a new
   young heap.  The old heap will not be touched.  Only when the young heap
   doesn't have enough garabge objects to reduce its size substantially will
   a GC over the old and young be performed.  The resulting new heap of objects
   will become a new old heap with the young heap being completely empty.

   Cheesy state diagram:
   [old 0XXX] [young0X  ]
   [old 0XXX] ([young0XXX] -> [newX  ] -> [young 1X  ])
   [old 0XXX] [young1X  ]
   ([old 0XXX] ([young1XXX] -> [newXXX])) -> [newXXXX] -> [old1XXXX]
   [old 1XXXX] [young2   ]

  TERMINOLOGY

  Size    Inspired by sizeof() operator.  The foot print this object requires
          internally in bytes.  This also includes the object's descriptor,
          unset vector locations, unused bytes for uneven sized strings or
          unused stack locations.

  Count   Number of blocks this object uses in memory.  May be equivalent to
          its size, a multiple of size (memory block), or fraction of its size
          (bit field).

  Length  Inspired by scheme primitives length and string-length.  Number of active
          'elements' this object references.  In a vector, the number of objects
          locations.  In an array, the bytes required to represent the value or string
          (such as number of characters).  In a stack, the number of objects pushed.
          Mainly used for external representations of objects.

  Canonicalized
  Normalized
*/

void memGarbageCollectInternal (Descriptor desc, Num byteSize);
Num memHeapLength (Num heapIndex);
void memError (void);



/*******************************************************************************
 Type_stuff

 Object types are defined by 8 bits.  The valid range/count for the core
   types are as follows (all values in hex):
     arrays   00...7f  80
     vector   80...ef  70
     internal f0...ff  10
*******************************************************************************/

#define TMAXARRAY     0x7fl
#define TMAXVECTOR    0xefl
#define TMAXINTERNAL  0xffl

typedef struct {
	Str description;
} MemTypeDescriptor;

MemTypeDescriptor memTypeDB[TMAXINTERNAL+1];

void memRegisterInternalType (Type type, char *description) {
	DBBEG();
	assert(strlen(description)<=8);
	DB("Type type="HEX"  char *description="STR, type, description);
	assert(type <= TMAXINTERNAL); /* Only arrays and vector can be created externally */
	assert(memTypeDB[type].description == NULL); /* Object types can't be redefined */
	memTypeDB[type].description = (Str)description;
	DBEND();
}

void memDebugDumpAllTypes(void) {
 Num i=0;
	for (i=0; i<256; ++i) {
		if (memTypeDB[i].description) printf("("HEX02"="STR") ", i, memTypeDB[i].description);
		if (TMAXARRAY==i || TMAXVECTOR==i) printf ("\n");
	}
}

/* Internal types which are registered during initialization */
#define TSEMAPHORE     0xfbl
#define TFINALIZER     0xfcl
#define TPOINTER       0xfdl
#define TSTACK         0xfel
#define TSHADOW        0xffl



/*******************************************************************************
 Descriptors
*******************************************************************************/
#define DescSize           sizeof(Descriptor)
#define DescBitCount       (8 * DescSize)
#define DescTypeBitCount   8
#define DescLengthBitCount (DescBitCount - DescTypeBitCount)

#define DescTypeBitMask   (~(LengthType)0 << (DescBitCount - DescTypeBitCount))
#define DescLengthBitMask (~(LengthType)0 >> (DescBitCount - DescLengthBitCount))
//assert(DescTypeBitMask   == 0xff00000000000000l);
//assert(DescLengthBitMask == 0x00ffffffffffffffl);

/* A 'Type' is 8 bits composed of a 1 bit 'class' and 7 bit 'id'.  If the class bit
   is 0 it is considered raw data and copied as is during a garbage collection.

 Type: [.|.......]
      class  id

   A 'Descriptor' is a 64 bit word composed of an 8 bit 'type' shifted and
   or'ed with a 56 bit 'length' field.  On 32 bit architectures, the size is 24 bits.

   Descriptor:  [--------][-------- -------- -------- -------- -------- -------- --------]
                   type               size
*/

/* Create a descriptor given 'type' and 'length' values.
   [........|........ ........ ........ ........ ........ ........ ........]
      desc                               length
 */
Descriptor memMakeDescriptor (Type type, LengthType length) {
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
// memIsObjectVector() REIMPLEMENTED

/* Is object a special type? */
Num memIsObjectSemaphore (Obj o) { return memObjectType(o) == TSEMAPHORE; }
Num memIsObjectFinalizer (Obj o) { return memObjectType(o) == TFINALIZER; }
Num memIsObjectPointer (Obj o)   { return memObjectType(o) == TPOINTER; }
Num memIsObjectStack (Obj o)     { return memObjectType(o) == TSTACK; }
Num memIsObjectShadow (Obj o)    { return memObjectType(o) == TSHADOW; }



/* Compute object size (total memory footprint in bytes) based on an array's 'length' */
Num memArrayLengthToObjectSize (LengthType length) {
	return DescSize + ((length + DescSize-1) & -DescSize);
}

/* Compute object size based on vector's 'length'.
*/
Num memVectorLengthToObjectSize (LengthType length) {
	return DescSize + length*ObjSize;
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
/* Limit the size of any object to 16Mb (2^24) bytes for sanity. */
#define MEMOBJECTMAXSIZE ((Num)0x1000000)

const Num HEAP_STATIC_BLOCK_COUNT =  0x010; /*    64Kb 0x010000 2^16 */
const Num HEAP_BLOCK_COUNT        =  0x400; /*  4Mb    0x400000 2^22 */
const Num STACK_LENGTH           = 0x04000; /*    16Kb 0x004000 2^14 */

typedef struct {
	Obj start;  /* Initial heap location. */
	Obj next;   /* Next available heap location. */
	Obj last;   /* One byte past the last heap location (exclusive). */
	Num finalizerCount; /* Number of finalizer types contained in this heap. */
} Heap;

/* The four heap structure instances
*/
Heap heapOld;   /* Where old objects live during runtime. */
Heap heap;      /* Where new young objects are created during runtime. */
Heap heapNew;   /* GC work area. */
Heap heapStatic;/* Objects that are never deleted nor touched by the GC. */


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
	if (MAP_FAILED == h->start) {
		fprintf (stderr, "\nERROR: memInitializeHeap(): Can't create heap.  mmap() => "OBJ, h->start);
		memError();
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
 int ret;
	DBBEG(" "STR, memHeapPointerToString(h));

	if (0 < blockCount) {
		oldBlockCount = memHeapBlockCount(h);
		newBlockCount = oldBlockCount - blockCount;
		DB ("["HEX"] -> ["HEX"] - ["HEX"]", oldBlockCount, newBlockCount, blockCount);
		assert(blockCount <= oldBlockCount);

		ret = munmap(h->start + newBlockCount * BLOCK_BYTE_SIZE, blockCount * BLOCK_BYTE_SIZE - 1);
		/* Update the heap's "last" pointer if munmap succeeds */
		if (!ret) {
			h->last = h->start + newBlockCount * BLOCK_BYTE_SIZE;
		} else {
		 	fprintf (stderr, "WARNING: memShrinkHeap() can't shrink heap region");
		}
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

/* Objects in old heap that were mutated with objects in young heap.
   This is an additional set of root set pointers besides the registers
   above.  Any old object that has been mutated to point to an object in the
   young heap must exist in this set.  Otherwise during a young only GC, a
   live object will be collected which is bad.  TODO
int mutatedOldObjectsLength=0;
Obj *(mutatedOldObjects[0x400])={0};
*/


const Num GC_MODE_YOUNG=0;
const Num GC_MODE_OLD=1;

Num GarbageCollectionMode;
Num garbageCollectionCount = 0;

/* Function pointers called before and after a GC proper.  */
Func memPreGarbageCollect = 0;
Func memPostGarbageCollect = 0;


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

	#if DEBUG_ASSERT
	if (MEMOBJECTMAXSIZE <= byteSize) {
		printf ("ERROR:: "STR" (desc "HEX016"  byteSize "HEX"): size too big.", __func__, desc, byteSize);
		memError();
	}
	#endif

	/* Set the new object's descriptor in the heap, set register r0 to the new
	   object location (immediately after the descriptor) and increment the heap's
	   'next' pointer. */
	*(Descriptor*)(heap.next) = desc;
	o = heap.next + DescSize;
	heap.next += byteSize;

	/* Perform a GC if we've surpassed the heap size, calling this function recursively */
	if (heap.next >= heap.last) {
		/* Check that we're not stuck in a recursive loop */
		if (1 == recursed) {
			fprintf (stderr, "ERROR:: "STR"  Unable to find enough heap space.", __func__);
			memError();
		}
		/* Make sure reg 0's bogus value isn't garbage collected and undo the next pointer.
		   Zeroing out r0 also has the side affect of making its object garbage.  */
		//r0 = 0;
		heap.next -= byteSize;
		memGarbageCollectInternal(desc, byteSize);

		++recursed;
		o = memNewObject(desc, byteSize);
		--recursed;
	}

	DBEND(" => "OBJ, o);
	return o;
}

/* Allocate a static object.  Like memNewObject except it can't fail.
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
		memError();
	}
	DBEND(" => "OBJ, o);
	return o;
}


Obj memNewStatic (Type type, LengthType length) {
Obj o;
	DBBEG("  type:"HEX02"  length:"HEX, type, length);
	o = memNewStaticObject (memMakeDescriptor(type, length),
	                    memArrayLengthToObjectSize(length));
	DBEND();
	return o;
}

Obj memNewStaticVector (Type type, LengthType length) {
Obj o;
	DBBEG("  type:"HEX02"  length:"HEX, type, length);
	o = memNewStaticObject (memMakeDescriptor(type, length),
	                    memVectorLengthToObjectSize(length));
	DBEND();
	return o;
}

Obj memNewArray (Type type, LengthType length) {
Obj o;
	DBBEG("  type:"HEX02"  length:"HEX, type, length);
	o = memNewObject (memMakeDescriptor(type, length),
	              memArrayLengthToObjectSize(length));
	DBEND();
	return o;
}

Obj memNewVector (Type type, LengthType length) {
Obj o;
	DBBEG("  type:"HEX02"  length:"HEX, type, length);
	o = memNewObject (memMakeDescriptor(type, length),
	              memVectorLengthToObjectSize(length));
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

/* First item in stack vector is a pointer to an address in the vector.
   Initially it points to itself, implying an empty stack.
*/
Obj memNewStack (void) {
 Obj o;
	DBBEG();
	o = memNewObject (memMakeDescriptor(TSTACK, STACK_LENGTH+1),
	              memVectorLengthToObjectSize(STACK_LENGTH+1));
	*(Obj*)o = (Obj)o;
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
	return memIsObjectInHeap(&heap, o)
	       || memIsObjectInHeap(&heapOld, o)
	       || memIsObjectInHeap(&heapStatic, o);
}


/* Object mutators.
*/
void memArraySet (Obj obj, Num offset, u8 item) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		printf("ERROR memArraySet(obj "OBJ" offset "NUM" item %02x) Invalid object.",
		    obj, offset, item);
		memError();
	} else if(!memIsObjectBaseArray(obj)) {
		printf("ERROR memArraySet(obj "OBJ" offset "NUM" item %02x) Not array class.",
		    obj, offset, item);
		memError();
	} else if(offset<0 || memObjectLength(obj)<=offset ) {
		printf("ERROR memArraySet(obj "OBJ" offset "NUM" item %02x) Invalid offset.",
		    obj, offset, item);
		memError();
	}
	#endif
	*((u8*)obj+offset)=item;
}

void memVectorSet (Obj obj, Num offset, Obj item) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		printf ("ERROR memVectorSet(obj "OBJ" offset "NUM" item "OBJ") Invalid object.",
		    obj, offset, item);
		memError();
	} else if (memIsObjectBaseArray(obj)) {
		printf ("ERROR memVectorSet(obj "OBJ" offset "NUM" item "OBJ") Not vector class.",
		    obj, offset, item);
		memError();
	} else if (offset < 0 || memObjectLength(obj) <= offset ) {
		printf ("ERROR memVectorSet(obj "OBJ" offset "NUM" item "OBJ") Invalid offset.",
		    obj, offset, item);
		memError();
	}
	#endif
	*((Obj*)obj+offset)=item;
}

void memStackPush (Obj obj, Obj item) {
	#if DEBUG_ASSERT_STACK
	if (!memIsObjectValid(obj) && !memIsObjectInHeap(&heapNew, obj)) {
		printf ("ERROR memStackPush(obj "OBJ" item "OBJ") Invalid object.", obj, item);
		memError();
	} else if (!memIsObjectStack(obj)) {
		printf ("ERROR memStackPush(obj "OBJ" item "OBJ") Not stack type.", obj, item);
		memError();
	} else if (STACK_LENGTH <= memStackLength(obj)) {
		printf ("ERROR memStackPush(obj "OBJ" item "OBJ") Stack overflow.", obj, item);
		memDebugDumpObject(r1f, stderr);
		memError();
	}
	#endif
	*++*(Obj**)obj=item;
}

void memStackSet (Obj obj, Num topOffset, Obj item) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		printf ("ERROR memStackSet(obj "OBJ" topOffset %x item "OBJ") Invalid object.",
		    obj, topOffset, item);
		memError();
	} else if (!memIsObjectStack(obj)) {
		printf ("ERROR memStackSet(obj "OBJ" topOffset %x item "OBJ") Not stack type.",
		    obj, topOffset, item);
		memError();
	} else if (topOffset<0 || memStackLength(obj)<=topOffset) {
		printf ("ERROR memStackSet(obj "OBJ" topOffset %x item "OBJ") Invalid offset. memStackLength(obj)=>%x", obj, topOffset, item, memStackLength(obj));
		memError();
	}
	#endif
	*(*(Obj**)obj-topOffset)=item;
}

Obj  memStackPop (Obj obj) {
 Obj ret;
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj) && !memIsObjectInHeap(&heapNew, obj)) {
		printf ("ERROR memStackPop(obj "OBJ") Invalid object.", obj);
		memError();
	} else if (!memIsObjectStack(obj)) {
		printf ("ERROR memStackPop(obj "OBJ") Not stack type.", obj);
		memError();
	} else if (memStackLength(obj) <= 0) {
		printf ("ERROR memStackPop(obj "OBJ") Stack underflow.", obj);
		memError();
	}
	#endif
	ret = *(*(Obj**)obj)--; /* Dereference the pointer then decrement it */

	return  ret;
}



/* Object accessors.
 */
u8 memArrayObject (Obj obj, Num offset) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		printf ("ERROR memArrayObject(obj "OBJ" offset "NUM") Invalid object.",
		    obj, offset);
		memError();
	} else if (!memIsObjectBaseArray(obj)) {
		printf ("ERROR memArrayObject(obj "OBJ" offset "NUM") Not array class.",
		    obj, offset);
		memError();
	} else if (offset<0 || memObjectLength(obj)<=offset ) {
		printf ("ERROR memArrayObject(obj "OBJ" offset "NUM") Invalid offset.",
		    obj, offset);
		memError();
	}
	#endif
	return *((u8*)obj+offset);
}

Obj memVectorObject (Obj obj, Num offset) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj) && !memIsObjectInHeap(&heapNew, obj)) {
		printf ("ERROR memVectorObject(obj "OBJ" offset "NUM") Invalid object.",
		    obj, offset);
		memError();
	} else if (memIsObjectBaseArray(obj)) {
		printf ("ERROR memVectorObject(obj ["OBJ"] "OBJ" offset "NUM") Not vector class.",
			memObjectDescriptor(obj),
		   obj, offset);
		memError();
	} else if (offset<0 || memObjectLength(obj)<=offset ) {
		printf ("ERROR memVectorObject(obj "OBJ" offset "NUM") Invalid index.",
		    obj, offset);
		memError();
	}
	#endif
	return *((Obj*)obj+offset);
}

Obj memStackObject (Obj obj, Num topOffset) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		printf ("ERROR memStackObject(obj "OBJ" topOffset "NUM"): Invalid object.",
		    obj, topOffset);
		memError();
	} else if (!memIsObjectStack(obj)) {
		printf ("ERROR memStackObject(obj "OBJ" topOffset "NUM"): Not stack type.",
		    obj, topOffset);
		memError();
	} else if (topOffset<0 || memStackLength(obj)<=topOffset ) {
		printf ("ERROR memStackObject(obj "OBJ" topOffset "NUM"): Invalid top offset.",
		    obj, topOffset);
		memError();
	}
	#endif
	return *(*(Obj**)obj-topOffset);
}

/* Number of elements pushed onto stack.  It is empty if the pointer address
   is the same as the object's address.
*/
Num memStackLength (Obj obj) {
	#if DEBUG_ASSERT_STACK
	if (!(memIsObjectInHeap(&heap, obj)
	      || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj)
		   || memIsObjectInHeap(&heapNew, obj))) {
		printf("ERROR memStackLength(obj "OBJ") Invalid object.", obj);
		memError();
	} else if (!memIsObjectStack(obj)) {
		printf("ERROR memStackLength(obj "OBJ") Not stack type.", obj);
		memError();
	}
	#endif
	return (Num)(((Obj**)obj)[0] - (Obj*)obj);
}

//void memPush (Obj o) { memStackPush(r1f, o); }
//Obj memPop (void) { return memStackPop(r1f); }



/*******************************************************************************
 Garbage_collector
*******************************************************************************/
#define MemRootSetCountMax 64
Num MemRootSetCount=0;
Obj *MemRootSet[MemRootSetCountMax];

/* Calculate a pointer object's index offset into the object it's pointing
   at. A pointer is a vector whos first element is a void* and the second
   element is the object the void* should be pointing somewhere inside of. */
Num memPointerOffset (Obj obj) {
	return (Num)((Obj*)memVectorObject(obj, 0) - (Obj*)memVectorObject(obj, 1));
}


/* Ring buffer.  Used for debugging internal behavior.
*/ 
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


/* Given pointer to an object, move object into the new heap and update
   the object to point to new address.  Object must be valid and exist in
   either the old or young heap so this check must be done before calling.
*/
void memObjectCopyToHeapNew (Obj *objp) {
 Obj newObjectLocation;
 Num len;
	/* Only move objects from concerned generation.  Either young heap or both
	   young and old heap. */
	if (!(memIsObjectInHeap(&heap, *objp)
	      || (GarbageCollectionMode==GC_MODE_OLD
	          && memIsObjectInHeap(&heapOld, *objp)))) {
		//DB ("    Ignoring");
		goto ret;
	}

	DBBEG("  location:"OBJ"  obj:"OBJ, objp, *objp);

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
		if (memIsObjectStack (*objp)) {
			DB ("Stack");
			len = memStackLength(*objp);
			/* Add 1 to stack length to account for the stack pointer in the first location. */
			memcpy(heapNew.next, (*objp - DescSize), (DescSize + ObjSize * (len + 1)));
			/* Update this stack's internal pointer (first word in the vector) to
				point to the proper position in the vector. */
			*(Obj*)newObjectLocation = (Obj*)newObjectLocation + len;
		} else { /* ... or just copy bytes that make up this object. */
			DB ("Array/Vector");
			memcpy(heapNew.next, *objp-DescSize, memObjectSize(*objp));
			/* Tally copied dirty objs. */
			if (memIsObjectFinalizer(*objp)) heapNew.finalizerCount++;
		} /* TODO handle pointer objects */
	
		/* Mutate this object into a shadow object.  This involves
		   mutating the descriptor's type into a shadow and length into the
		   proper vector length.  This length is only used for debugging
			purposes when traversing over a heap containing shadow types... */
		*((Descriptor*)*objp-1) = memMakeDescriptor(TSHADOW,
		                            memObjectSize(*objp)/DescSize - 1);
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

	DBEND();
	ret:
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
				memVectorSet(newObj, 0, (Obj*)memVectorObject(newObj, 1)+len);
			} else if (memIsObjectStack(newObj)) {
				for (i=0; i<memStackLength(newObj); i++) {
					DB("  Stack in old heap   location "HEX"/"HEX" "OBJ, i, memStackLength(newObj), (Obj*)newObj+i+1);
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

#if VALIDATE_HEAP
	memValidateHeapStructures();
#endif

	if (GC_MODE_YOUNG == GarbageCollectionMode) {
		DBBEG("  mode=YOUNG  old_count:"HEX"  young_count:"HEX, memHeapBlockCount(&heapOld), memHeapBlockCount(&heap));
	} else {
		DBBEG("  mode=OLD    old_count:"HEX"  young_count:"HEX, memHeapBlockCount(&heapOld), memHeapBlockCount(&heap));
	}

	if (memPreGarbageCollect) memPreGarbageCollect();

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

	/* Copy objects in registers to new heap. */
	DB("Collecting root set");
	for (i=0; i<MemRootSetCount; ++i) memObjectCopyToHeapNew(MemRootSet[i]);
	/*
	memObjectCopyToHeapNew (&r0);  memObjectCopyToHeapNew (&r1);
	memObjectCopyToHeapNew (&r2);  memObjectCopyToHeapNew (&r3);
	memObjectCopyToHeapNew (&r4);  memObjectCopyToHeapNew (&r5);
	memObjectCopyToHeapNew (&r6);  memObjectCopyToHeapNew (&r7);
	memObjectCopyToHeapNew (&r8);  memObjectCopyToHeapNew (&r9);
	memObjectCopyToHeapNew (&ra);  memObjectCopyToHeapNew (&rb);
	memObjectCopyToHeapNew (&rc);  memObjectCopyToHeapNew (&rd);
	memObjectCopyToHeapNew (&re);  memObjectCopyToHeapNew (&rf);
	memObjectCopyToHeapNew (&r10); memObjectCopyToHeapNew (&r11);
	memObjectCopyToHeapNew (&r12); memObjectCopyToHeapNew (&r13);
	memObjectCopyToHeapNew (&r14); memObjectCopyToHeapNew (&r15);
	memObjectCopyToHeapNew (&r16); memObjectCopyToHeapNew (&r17);
	memObjectCopyToHeapNew (&r18); memObjectCopyToHeapNew (&r19);
	memObjectCopyToHeapNew (&r1a); memObjectCopyToHeapNew (&r1b);
	memObjectCopyToHeapNew (&r1c); memObjectCopyToHeapNew (&r1d);
	memObjectCopyToHeapNew (&r1e); memObjectCopyToHeapNew (&r1f);
	*/

	/* Treat objects in heapOld as a root set. This is temporary until I re-enable
	   the mutated:old-object list (commented code below). */
	if (GarbageCollectionMode==GC_MODE_YOUNG) {
		memCopyHeapObjectsToHeapNew(&heapOld);
	}

	/* Compact the objects referenced by mutated objects in the old heap.
	   Also optimize this list by compacting this list while were here by
	   removing copies of the same references.
	if (GarbageCollectionMode == GC_MODE_YOUNG) {
		DB("   collecting and compacting mutated old object references...");
		for (i=0; i<mutatedOldObjectsLength; i++) {
			/ Compact
			if (memIsObjectInHeap(&heapNew, *mutatedOldObjects[i])) {
				mutatedOldObjects[i]
				= mutatedOldObjects[--mutatedOldObjectsLength];;
			/ Collect object this points to.
			} else {
				memObjectCopyToHeapNew(mutatedOldObjects[i]);
			}
		}
	} */

	/* The new heap is the new root set.  Continue compacting on these objects */
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
		//mutatedOldObjectsLength = 0;
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

	if (garbageCollectionCount%50 == 0) GarbageCollectionMode = GC_MODE_OLD;

	if (memPostGarbageCollect) memPostGarbageCollect();

	assert(memGCFlag==1);
	memGCFlag=0;

#if VALIDATE_HEAP
	memValidateHeapStructures();
#endif

	/* Debug dump heap [used | unused]objCount info
	 */
	//fprintf(stderr, "\nheapOld["HEX" | "HEX"]"HEX"  heapYoung["HEX" | "HEX"]"HEX, memHeapUsedSize(&heapOld), memHeapUnusedSize(&heapOld), memHeapLength(1), memHeapUsedSize(&heap), memHeapUnusedSize(&heap), memHeapLength(2));

	DBEND("  heapOld["NUM"]  heapYoung["NUM"]", memHeapBlockCount(&heapOld), memHeapBlockCount(&heap));
} /* memGarbageCollectInternal */


/* External call not triggered on lack of space so no descriptor
   nor byteSize passed
*/
void memGarbageCollect () {
	memGarbageCollectInternal(0, 0);
}



/*******************************************************************************
 Debugging_aids
*******************************************************************************/

/* Dump heap and root object details
*/
void memDebugDumpHeapHeaders (FILE *stream) {
 Num i;
	// Set a default stream.
	if (stream == NULL) stream=stderr;

	fprintf (stream, "\n      Static       Old          Current      New\n");
	fprintf (stream, "Start "OBJ" "OBJ" "OBJ" "OBJ"\n",
	        heapStatic.start,  heapOld.start,
	        heap.start,  heapNew.start);
	fprintf (stream, "Next  "OBJ" "OBJ" "OBJ" "OBJ"\n",
	        heapStatic.next,  heapOld.next,
	        heap.next,  heapNew.next);
	fprintf (stream, "last  "OBJ" "OBJ" "OBJ" "OBJ"\n",
	        heapStatic.last,  heapOld.last,
	        heap.last,  heapNew.last);
	fprintf (stream, "Size    %10x   %10x   %10x   %10x\n",
	        heapStatic.last-heapStatic.start,  heapOld.last-heapOld.start,
	        heap.last-heap.start,              heapNew.last-heapNew.start);
	fprintf (stream, "Blocks  %10lx   %10lx   %10lx   %10lx\n",
	        memHeapBlockCount(&heapStatic),  memHeapBlockCount(&heapOld),
	        memHeapBlockCount(&heap),        memHeapBlockCount(&heapNew));
	fprintf (stream, "Objects %10lx   %10lx   %10lx   %10lx\n",
	        memHeapLength(0),  memHeapLength(1),
	        memHeapLength(2),  memHeapLength(3));
	fprintf (stream, "Finalizer %8lx   %10lx   %10lx   %10lx",
	        heapStatic.finalizerCount,  heapOld.finalizerCount,
	        heap.finalizerCount,        heapNew.finalizerCount);

	for (i=0; i<MemRootSetCount; ++i) {
		if (0 == i%4) printf ("\n");
		fprintf (stream, "%8s "OBJ, memObjString(MemRootSet[i]), *MemRootSet[i]);
	}
	fprintf (stream, "\n");
}

void memDebugDumpObject (Obj o, FILE *stream) {
 Int i, fdState;
 Str s;
 Obj obj;

	fcntl (0, F_SETFL, (fdState=fcntl(0, F_GETFL, 0))&~O_NONBLOCK);

	if (!memIsObjectValid(o) && !memIsObjectInHeap(&heapNew, o)) {
		fprintf(stream, NL "???" OBJ, o);
	}

	if (stream == NULL) stream=stderr;

	/* Dump the object's address and descriptor information.
	   7fe8f5f44610 81 PAIR      2 #(7fe8f5f445f0 7fe8f5f44600) */
	fprintf (stream, "\n%c "OBJ" "HEX02"%-5s"HEX4,
		memIsObjectInHeap(&heapStatic, o)?'s':
			memIsObjectInHeap(&heapOld, o)?'o':
				memIsObjectInHeap(&heap, o)?'y':
					memIsObjectInHeap(&heapNew, o)?'n':'?',
		o, memObjectType(o), memTypeString(memObjectType(o)), memObjectLength(o));

	/* Some objects are just an instance of the descriptor
	   such as #f and #() they have no content. */
 	if (!memObjectLength(o)) {
		fprintf(stderr, " EMPTY");
	} else if (!memIsObjectBaseArray(o)) { // Vector
		if (memIsObjectPointer(o)) {
			fprintf (stream, " "OBJ" -> "OBJ, ((Obj*)o)[0], ((Obj*)o)[1]);
		} else if (memIsObjectStack(o)) {
			fprintf (stream, " ["HEX" | ", memStackLength(o));
			for (i=0; i<memStackLength(o); i++) {
				fprintf (stream, HEX" ", ((Obj*)o)[i+1]);
			}
			fprintf (stream, "]");
		} else if (memIsObjectShadow(o)) {
			fprintf (stream, " *"OBJ"", *(Obj*)o);
		} else if (memIsObjectFinalizer(o)) {
			fprintf (stream, " ("HEX":"STR" . "OBJ")", ((Obj*)o)[0], memObjString(((Obj*)o)[0]), ((Obj*)o)[1]);


		} else {
			for (i=0; i<memObjectLength(o); i++) {
				obj = ((Obj*)o)[i];
				fprintf (stream, "%s"HEX, ((i==0)?"#(":" "), obj);
				s = memObjString(obj); /* Internal pointer address */
				if (s) fprintf (stream, ":%s", s);
			}
			fprintf (stream, ")");
		}
	} else { // Array
		for (i=0; i<memObjectLength(o); i++) {
			fprintf (stream, " %s%x", i==0?"(":"", ((u8*)o)[i]);
		}
		fprintf (stream, ")");
	}

	fcntl (0, F_SETFL, fdState);
}


void memDebugDumpStaticHeap (FILE *stream) {
 Obj o;
	if (stream == NULL) stream=stderr;
	o = heapStatic.start + DescSize;
	while (o<heapStatic.next) {
		memDebugDumpObject (o, stream);
		o += memObjectSize(o);
	}
}

void memDebugDumpOldHeap (FILE *stream) {
 Obj o;
	if (stream == NULL) stream=stderr;
	o = heapOld.start + DescSize;
	while (o<heapOld.next) {
		memDebugDumpObject (o, stream);
		o += memObjectSize(o);
	}
}

void memDebugDumpYoungHeap (FILE *stream) {
 Obj o;
	if (stream == NULL) stream=stderr;
	o = heap.start + DescSize;
	while (o<heap.next) {
		memDebugDumpObject (o, stream);
		o += memObjectSize(o);
	}
}

void memDebugDumpNewHeap (FILE *stream) {
 Obj o;
	if (stream == NULL) stream=stderr;
	o = heapNew.start + DescSize;
	while (o<heapNew.next) {
		memDebugDumpObject (o, stream);
		o += memObjectSize(o);
	}
}

void memDebugDumpAll (FILE *stream) {
	DBBEG();

	if (stream == NULL) stream=stderr;

	memDebugDumpHeapHeaders(stream);

	/* DUMP EACH HEAP OBJECT */
	fprintf (stream, "----STATIC HEAP----");
	memDebugDumpStaticHeap(stream);

	fprintf (stream, "\n----OLD HEAP----");
//	for (i=0; i<mutatedOldObjectsLength; i++) {
//		fprintf (stream " "OBJ, mutatedOldObjects[i]);
//	}
	memDebugDumpOldHeap(stream);

	fprintf (stream, "\n----YOUNG HEAP----");
	memDebugDumpYoungHeap(stream);

	fprintf (stream, "\n----NEW HEAP----");
	memDebugDumpNewHeap(stream);

	fprintf (stream, "\n----DONE DUMPING HEAP STRUCTURES----\n");
	fflush(stream);

	DBEND();
}



void memValidateObject (Obj o) {
 Obj oo, op;
 Int valid=1;
 Num i;
	DBBEG();
	
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
				fprintf (stderr, "\nERROR memValidateObject() pointer is to invalid object.");
				valid=0;
			}
			/* Verify pointer is within object */
			if (!((oo <= op)	
			      && (op < oo + memObjectSize(oo) - DescSize))) {
				fprintf (stderr, "\nERROR memValidateObject() pointer is out of object range.");
				valid=0;
			}
		} else if (memIsObjectStack(o)) {
			op = memVectorObject(o, 0);
			if (!(o <= op
			      && (op < o + memObjectSize(o) - DescSize))) {
				fprintf (stderr, "\nERROR memValidateObject() stack "OBJ" out of bounds. ptr:"OBJ"  end:"OBJ"", o, op, o + memObjectSize(o) - DescSize);
				valid=0;
			}
		} else if (memIsObjectShadow(o))
			fprintf (stderr, "\nERROR memValidateObject() found shadow.");
		else if (memIsObjectFinalizer(o))
			fprintf (stderr, "\nERROR memValidateObject() found finalizer.");
		else if (memIsObjectBaseVectorOrArray(o)) /* The outer if verified this is not an array */
			for (i=0; i<memObjectLength(o); i++) {
				oo = memVectorObject(o, i);
				/* Originally immediate value range was 0xffff.  Allowing positive and negative
				   immediate values also include internal C code goto addresses which are used
				   as opcodes in the virtual machine. */
				if (!(memIsObjectInHeap(&heapOld, oo)
				      || memIsObjectInHeap(&heap, oo)
				      || memIsObjectInHeap(&heapStatic, oo))
				    && (oo > (Obj)0x430000 && oo < (Obj)-0x430000) /* Immediate values */
				    && !memObjString(oo)) { /* Ignore registered internal pointers. */
					fprintf (stderr, "\nERROR memValidateObject() vector object "OBJ"["INT"]="OBJ" invalid.", o, i, oo);
					//wscmWrite(memVectorObject(o, 2), 0, 1);
					valid=0;
				}
			}
	}
	if (!valid) {
		fprintf (stderr, "\nERROR memValidateObject() found bad object:"OBJ NL, o);
		memDebugDumpObject (o, NULL);
	}
	DBEND();
}



void memValidateHeapStructures (void) {
 Obj o;
	DBBEG();

	// Static Heap
	o = heapStatic.start + DescSize;
	while (o<heapStatic.next) {
		memValidateObject (o);
		o += memObjectSize(o);
	}

	o = heapOld.start + DescSize;
	while (o<heapOld.next) {
		memValidateObject (o);
		o += memObjectSize(o);
	}

	o = heap.start + DescSize;
	while (o<heap.next) {
		memValidateObject (o);
		o += memObjectSize(o);
	}
	DBEND();
}



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


/* Get external description of the type for debugging and object dumps */
Str memTypeString (Type t) {
	DB("Type t="NUM, t);
	assert(t <= TMAXINTERNAL);
	return memTypeDB[t].description
		?  memTypeDB[t].description
		: (Str)"?????";
}


/* Simple table insertion and lookup of a string
   to a pointer value aka object. char* must be
   the type due to how the preprocessor generates
   symbols. */
#define TABLEMAX 4096
typedef struct {Obj obj;  Str str;} osPair;
osPair osTable[TABLEMAX];
Num osCount=0;

Str memObjString (Obj obj) {
 Num i;
	for (i=0; i<osCount; ++i)
		if (osTable[i].obj==obj) return osTable[i].str;
	return NULL;
}

/* Assign a fixed pointer location with a string name
*/
void memObjStringRegister (Obj obj, Str str) {
	DBBEG("  Registering "OBJ"  string="STR" on existing str="STR, obj, str, memObjString(obj));
	assert(NULL == memObjString(obj));
	assert(osCount < TABLEMAX);
	osTable[osCount].obj = obj;
	osTable[osCount++].str = str;
	DBEND();
}

void memError (void) {
	fprintf (stderr, "\nERROR: memError() called: Halting process with a seg fault.\n");
	*(int*)0 = 0; // Force a crash to catch in debugger.
}



/*******************************************************************************
 External_calls
*******************************************************************************/

void memInitialize (Func preGC, Func postGC) {
 static Num shouldInitialize=1;
	DBBEG();
	if (shouldInitialize) {
		DB("Activating module...");
		shouldInitialize=0;
		DB("Register the internal object types");
		memRegisterInternalType(TSEMAPHORE, "semaphr");
		memRegisterInternalType(TFINALIZER, "finalal");
		memRegisterInternalType(TPOINTER, "pointer");
		memRegisterInternalType(TSTACK, "stack");
		memRegisterInternalType(TSHADOW, "shadow");
		DB("Create heaps");
		memInitializeHeap  (&heapStatic, HEAP_STATIC_BLOCK_COUNT);
		memInitializeHeap  (&heap, HEAP_BLOCK_COUNT);
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

	DBEND();
}

void memRegisterType (Type type, char *description) {
	assert(type <= TMAXVECTOR); /* Only arrays and vector can be created externally */
	memRegisterInternalType (type, description);
}


/* Register the address of an object pointer.  All of these object locations
   are used as the root set for the garbage collector.
*/
void memRegisterRootObject (Obj *objp, Str desc) {
	assert(MemRootSetCount<MemRootSetCountMax);
	MemRootSet[MemRootSetCount++] = objp;
	memObjStringRegister (objp, desc);
}



#undef DB_DESC
#undef DEBUG
