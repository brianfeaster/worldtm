#define DEBUG 0
#define DB_MODULE "MEM "
/* Generational garbage collector based on mem.c.
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

 Terminology:
   size  -- Number of bytes this object is made up of.  Inspired by
            sizeof() operator.
   count -- Number of blocks this object uses in memory.  Might
            be bytes or other block size.
   length-- Number of 'elements' this object contains.  In a vector the
            number of objects contained.  In an array, the byte length
            used.  Inspired by scheme primtives length and string-length.  Note
            the array length might not be count aligned.  Also there might be
            other meta data associated with the object.

*/
#include "debug.h"

#include <stdio.h>
#include <stdlib.h> /* For exit(). */
#include <string.h> /* For memcpy(). */
#include <sys/mman.h>
#include "mem.h"



/* Dumps object to stdout in a raw generic way. */
void memDebugObjectDump (Obj o);
void memDebugDumpHeapStructures (void);

void memError (void);

/* Constants and other fun global variables.
*/

/* Byte size of a Linux virtual memory block.  Resolution of mmap. */
const u32 BLOCK_BYTE_SIZE         = 0x1000; //   4Kb 0x001000   2^12

const u32 HEAP_STATIC_BLOCK_COUNT = 0x002;  //     8Kb 0x002000 2^13
const u32 HEAP_BLOCK_COUNT        = 0x400;  //   4Mb   0x400000 2^22
//const u32 HEAP_INCREMENT_COUNT   = 0x040; //   256Kb 0x040000 2^18
//const u32 HEAP_DECREMENT_COUNT   = 0x004; //    16Kb 0x004000 2^14
const u32 STACK_LENGTH           = 0x02000; //     8Kb 0x002000 2^13

u32 garbageCollectionCount = 0;



/* Heap structure.
 */
typedef struct {
   void *start;  /* Initial heap location. */
   void *next;   /* Next available heap location. */
   void *last;   /* One byte past the last heap location. */
   u32 blockCount; /* Number of blocks this heap uses.  4096 assumed. */
	u32 finalizerCount; /* Number of finalizer types contained in this heap. */
} Heap;

/* The three heaps used in this generational collector. */
Heap heapOld,   /* Where old objects live during runtime. */
     heap,      /* Where new young objects are created during runtime. */
     heapNew,   /* GC work area. */
     heapStatic;/* Objects that are never deleted nor touched by the GC. */



/* Initialize a heap struct and allocate 'count' number of blocks.
 */
void memInitializeHeap (Heap *h, long blockCount) {
 int bytes = BLOCK_BYTE_SIZE * blockCount;
	DB ("-->memInitializeHeap(heap &%s)", h==&heapOld?"heapOld":h==&heap?"heap":h==&heapNew?"heapNew":h==&heapStatic?"heapStatic":"???");
	h->start = h->next =
		mmap(0x0, bytes, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	if (-1 == (int)(h->start)) {
		fprintf (stderr, "ERROR heap_init() Can't create initial heap ret=%p\n",
				h->start);
		memError();
	}
	h->last = h->start + bytes; /* Last pointer is exclusive. */
	h->blockCount = blockCount;
	h->finalizerCount=0;
	DB ("<--memInitializeHeap()");
}

/* Unallocate blockCount from tail part of object linux-memory. */
void memShrinkHeap (Heap *heap, long blockCount) {
 int newBlockCount;
	DB ("-->memShrinkHeap()");
	if (blockCount < 1 || blockCount > heap->blockCount) return;
	newBlockCount = heap->blockCount - blockCount;
	if (munmap(heap->start+newBlockCount*BLOCK_BYTE_SIZE, blockCount*BLOCK_BYTE_SIZE))
		fprintf (stderr, "\aWARNING: memShrinkHeap() can't shrink *heap region");
	heap->last = heap->start + newBlockCount * BLOCK_BYTE_SIZE;
	heap->blockCount = newBlockCount;
	DB ("<--memShrinkHeap()");
}

/* Release heap's blocks back to Linux.
*/
void memFreeHeap (Heap *heap) {
	DB ("-->memFreeHeap()");
	memShrinkHeap (heap, heap->blockCount);
	DB ("<--memFreeHeap()");
}

/* Reset heap so that it appears empty again.  Called after a heap structure
   is moved into another.
 */
void memResetHeap (Heap *heap) {
	DB ("-->memResetHeap()");
	heap->next = heap->start = heap->last = (void*)0;
	heap->blockCount = heap->finalizerCount = 0;
	DB ("<--memResetHeap()");
}



/* A 'type' is an 32 bit byte composed of a 1 bit 'class' and 7 bit 'id'.  The
   lower 24 bits are unused.

                type:  [-|-------][-------- -------- --------]
                      class   id              unused

   A 'descriptor' is a 32 bit word composed of an 8 bit 'type' or'ed with
   a 24 bit 'count'.
                descriptor:  [--------][-------- -------- --------]
                                type               size
*/
typedef u32 Descriptor;

/* Create a descriptor given 'type' and 'length' values. */
Descriptor memMakeDescriptor (Type type, u32 length) {
	return type | length;
}



/* Get object's 'descriptor' value.
*/
Descriptor memObjectDescriptor (Obj o) {
	return *((Descriptor*)o - 1);
}

/* Get object's 'type'.
*/
Type memObjectType (Obj o) {
	return memObjectDescriptor(o) & 0xff000000;
}

/* Get object's 'length'.
*/
u32 memObjectLength (Obj o) {
	return memObjectDescriptor(o) & 0x00ffffff;
}



/* Is object an array class? */
int memIsObjectArray (Obj o)  { return !(memObjectDescriptor(o) & 0x80000000); }

/* Is object a vector class? */
int memIsObjectVector (Obj o) { return memObjectDescriptor(o) & 0x80000000; }

/* Is object a finalizer? */
int memIsObjectFinalizer (Obj o) { return memObjectType(o) == 0xfc000000; }

int memIsObjectPointer (Obj o) { return memObjectType(o) == 0xfd000000; }

int memIsObjectStack (Obj o) { return memObjectType(o) == 0xfe000000; }

int memIsObjectShadow (Obj o) { return memObjectType(o) == 0xff000000; }



/* Compute object size based on an array's 'length'.
   BF:  Can probably (length+3+sizeof(Descriptor))
*/
u32 memArrayLengthToObjectSize  (u32 length) {
	return ((length+3) & -sizeof(Descriptor)) + sizeof(Descriptor);
}

/* Compute object size based on vector's 'length'.
*/
u32 memVectorLengthToObjectSize (u32 length) {
	return sizeof(Descriptor) + length*sizeof(Descriptor);
}

/* Get object's byte 'size' including the descriptor.  Used to find next
   object in the heap or byte count for copying into another heap.
*/
u32 memObjectSize (Obj o) {
	return memIsObjectVector(o)
	       ? memVectorLengthToObjectSize(memObjectLength(o))
	       : memArrayLengthToObjectSize(memObjectLength(o));
}



/* Registers.  These make up the root set used by the garbage collector.
 */
Obj r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7,
    r8,  r9,  ra,  rb,  rc,  rd,  re,  rf,
    r10, r11, r12, r13, r14, r15, r16, r17,
    r18, r19, r1a, r1b, r1c, r1d, r1e, r1f;



/* Objects in old heap that were mutated with objects in young heap.
   This is an additional set of root set pointers besides the registers
   above.  Any old object that has been mutated to point to an object in the
   young heap must exist in this set.  Otherwise during a young only GC, a
   live object will be collected which is bad.
int mutatedOldObjectsLength=0;

Obj *(mutatedOldObjects[0x400])={0};
*/



/* ALLOCATED AN OBJECT IN THE HEAP:
 *  THE DESCRIPTOR PARAMATER IS A LONG [tsss] t=type-field s=size=field.
 *  A TYPE >= 0X80 IS ASSUMED A VECTOR OF OBJECTS (POINTERS, LENGTH 4 ON INTEL
 *  BOXEN). A TYPE < 0X80 IS RAW BYTES.  THIS RESTRICTION IS USED BY THE GC.
 *
 * THE SIZE PARAMATER IS THE NUMBER OF ACTUAL BYTES THE OBJECT IS COMPOSED
 * OF PLUS THE DESCRIPTOR SIZE.  IT SHOULD BE AT LEAST 3 TO ALLOW FOR MUTATION
 * INTO A SHADOW BY THE GC.
 */
u32 NewDesiredObjectSize=0; /* Size of object desired just before this GC. */

void memNewObject (Descriptor desc, u32 byteSize) {
	DB("-->memNewObject(desc %08x, byteSize %d)", desc, byteSize);
	*(Descriptor*)(heap.next) = desc; /* Set descriptor. */
	r0 = heap.next + sizeof(Descriptor); /* Set r0 to object memory location. */
	heap.next += byteSize; /* Set pointer to next available heap position. */
	/* Check that we're still within the buffer boundary. */
	if (heap.next >= heap.last) {
		r0 = 0;
		heap.next -= byteSize;
		// Put the desires size in global variable.
		NewDesiredObjectSize = byteSize;
		memGarbageCollect();
		memNewObject(desc, byteSize);
	}
	DB("<--memNewObject()");
}

void memNewStaticObject (Descriptor desc, u32 byteSize) {
	DB("-->memNewStaticObject(desc %08x, byteSize %x)", desc, byteSize);
	*(Descriptor*)heapStatic.next = desc;
	r0 = heapStatic.next + sizeof(Descriptor);
	heapStatic.next += byteSize;
	/* Check that we're still within the buffer boundary. */
	if (heapStatic.next >= heapStatic.last) {
		fprintf (stderr, "ERROR: memNewStaticObject(): Static buffer overflow.\a\n");
		memError();
	}
	DB("<--memNewStaticObject()");
}


void memNewStatic (Type type, u32 length) {
	DB("-->memNewStatic(type %08x, length %x)", type, length);
	memNewStaticObject (memMakeDescriptor(type, length),
	                    memArrayLengthToObjectSize(length));
	DB("<--memNewStatic()");
}

void memNewStaticVector (Type type, u32 length) {
	DB("-->memNewStaticVector(type %08x, length %x)", type, length);
	memNewStaticObject (memMakeDescriptor(type, length),
	                    memVectorLengthToObjectSize(length));
	DB("<--memNewStaticVector()");
}

void memNewArray (Type type, u32 length) {
	DB("-->memNewArray(type %08x, length %x)", type, length);
	memNewObject (memMakeDescriptor(type, length),
	              memArrayLengthToObjectSize(length));
	DB("<--memNewArray()");
}

void memNewVector (Type type, u32 length) {
	DB("-->memNewVector(type %08x, length %d)", type, length);
	memNewObject (memMakeDescriptor(type, length),
	              memVectorLengthToObjectSize(length));
	DB("<--memNewVector()");
}

/* Finalizer is just a C function pointer that is called when this object
   becomes garbage.
*/
void memNewFinalizer (void) {
	DB("-->memNewFinalizer()");
	memNewObject (memMakeDescriptor(TFINALIZER, 1),
	              memVectorLengthToObjectSize(1));
	heap.finalizerCount++;
	DB("<--memNewFinalizer");
}

/* Pointer object is a normal C pointer followed by an object pointer.
 */
void memNewPointer (void) {
	DB("-->memNewPointer()");
	memNewObject (memMakeDescriptor(TPOINTER, 2),
	              memVectorLengthToObjectSize(2));
	DB("<--memNewPointer");
}

/* First item in stack vector is a pointer to an address in the vector.
   Initially it points to itself, implying an empty stack.
*/
void memNewStack (void) {
	DB("-->memNewStack()");
	memNewObject (memMakeDescriptor(TSTACK, STACK_LENGTH+1),
	              memVectorLengthToObjectSize(STACK_LENGTH+1));
	*(Obj*)r0 = (Obj)r0;
	DB("<--memNewStack");
}



/* Object heap queries.
*/
int memIsObjectInHeap (Heap *heap, Obj o) {
	return heap->start<=o && o<heap->last;
}

/* Check that object pointer is in a valid heap address.
int memIsObjectValid  (Obj o) {
	return memIsObjectInHeap(&heap, obj)
	       || memIsObjectInHeap(&heapOld, obj)
	       || memIsObjectInHeap(&heapStatic, obj);
}
*/


/* Object mutators.
*/
void memArraySet (Obj obj, u32 offset, u8 item) {
	#if DEBUG
	if(!(memIsObjectInHeap(&heap, obj)
	      || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj))) {
		DB("ERROR memArraySet(obj %08x offset %x item %08x) Invalid object.",
		    obj, offset, item);
		memError();
	}else if(!memIsObjectArray(obj)) {
		DB("ERROR memArraySet(obj %08x offset %x item %08x) Not array class.",
		    obj, offset, item);
		memError();
	}else if(offset<0 || memObjectLength(obj)<=offset ) {
		DB("ERROR memArraySet(obj %08x offset %x item %08x) Invalid offset.",
		    obj, offset, item);
		memError();
	}else
	#endif
	*((u8*)obj+offset)=item;
}

void memVectorSet (Obj obj, u32 offset, Obj item) {
	#if DEBUG
	if (!(memIsObjectInHeap(&heap, obj) || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj))) {
		DB ("ERROR memVectorSet(obj %08x offset %x item %08x) Invalid object.",
		    obj, offset, item);
		memError();
	} else if (!memIsObjectVector(obj)) {
		DB ("ERROR memVectorSet(obj %08x offset %x item %08x) Not vector class.",
		    obj, offset, item);
		memError();
	} else if (offset < 0 || memObjectLength(obj) <= offset ) {
		DB ("ERROR memVectorSet(obj %08x offset %x item %08x) Invalid offset.",
		    obj, offset, item);
		memError();
	} else
	#endif
	*((Obj*)obj+offset)=item;
}

void memStackPush (Obj obj, Obj item) {
	#if DEBUG
	if (!(memIsObjectInHeap(&heap, obj) || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj))) {
		DB ("ERROR memStackPush(obj %08x item %08x) Invalid object.", obj, item);
		memError();
	} else if (!memIsObjectStack(obj)) {
		DB ("ERROR memStackPush(obj %08x item %08x) Not stack type.", obj, item);
		memError();
	} else if (STACK_LENGTH <= memStackLength(obj)) {
		DB ("ERROR memStackPush(obj %08x item %08x) Stack overflow.", obj, item);
		memError();
	} else
	#endif
	*++(*(Obj**)obj)=item;
}

void memStackSet (Obj obj, u32 topOffset, Obj item) {
	#if DEBUG
	if (!(memIsObjectInHeap(&heap, obj) || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj))) {
		DB ("ERROR memStackSet(obj %08x topOffset %x item %08x) Invalid object.",
		    obj, topOffset, item);
		memError();
	} else if (!memIsObjectStack(obj)) {
		DB ("ERROR memStackSet(obj %08x topOffset %x item %08x) Not stack type.",
		    obj, topOffset, item);
		memError();
	} else if (topOffset<0 || memStackLength(obj)<=topOffset) {
		DB ("ERROR memStackSet(obj %08x topOffset %x item %08x) Invalid offset. memStackLength(obj)=>%x", obj, topOffset, item, memStackLength(obj));
		memError();
	} else
	#endif
	*(*(Obj**)obj-topOffset)=item;
}

Obj  memStackPop (Obj obj) {
	#if DEBUG
	if (!(memIsObjectInHeap(&heap, obj)
	      || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj))) {
		DB ("ERROR memStackPop(obj %08x) Invalid object.", obj);
		memError();
	} else if (!memIsObjectStack(obj)) {
		DB ("ERROR memStackPop(obj %08x) Not stack type.", obj);
		memError();
	} else if (memStackLength(obj) <= 0) {
		DB ("ERROR memStackPop(obj %08x) Stack underflow.", obj);
		memError();
	}
	#endif
	return *(*(Obj**)obj)--;
}



/* Object accessors.
 */
u8 memArrayObject (Obj obj, u32 offset) {
	#if DEBUG
	if (!(memIsObjectInHeap(&heap, obj)
	      || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj))) {
		DB ("ERROR memArrayObject(obj %08x offset%x) Invalid object.",
		    obj, offset);
		memError();
	} else if (!memIsObjectArray(obj)) {
		DB ("ERROR memArrayObject(obj %08x offset %x) Not array class.",
		    obj, offset);
		memError();
	} else if (offset<0 || memObjectLength(obj)<=offset ) {
		DB ("ERROR memArrayObject(obj %08x offset %x) Invalid offset.",
		    obj, offset);
		memError();
	}
	#endif
	return *((u8*)obj+offset);
}

Obj memVectorObject (Obj obj, u32 offset) {
	#if DEBUG
	if (!(memIsObjectInHeap(&heap, obj) || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj))) {
		DB ("ERROR memVectorObject(obj %08x offset %x) Invalid object.",
		    obj, offset);
		memError();
	} else if (!memIsObjectVector(obj)) {
		DB ("ERROR memVectorObject(obj %08x offset %x) Not vector class.",
		   obj, offset);
		memError();
	} else if (offset<0 || memObjectLength(obj)<=offset ) {
		DB ("ERROR memVectorObject(obj %08x offset %x) Invalid index.",
		    obj, offset);
		memError();
	}
	#endif
	return *((Obj*)obj+offset);
}

Obj memStackObject (Obj obj, u32 topOffset) {
	#if DEBUG
	if (!(memIsObjectInHeap(&heap, obj)
	      || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj))) {
		DB ("ERROR memStackObject(obj %08x topOffset %x): Invalid object.",
		    obj, topOffset);
		memError();
	} else if (!memIsObjectStack(obj)) {
		DB ("ERROR memStackObject(obj %08x topOffset %x): Not stack type.",
		    obj, topOffset);
		memError();
	} else if (topOffset<0 || memStackLength(obj)<=topOffset ) {
		DB ("ERROR memStackObject(obj %08x topOffset %x): Invalid top offset.",
		    obj, topOffset);
		memError();
	}
	#endif
	return *(*(Obj**)obj-topOffset);
}

/* Number of elements pushed onto stack.
*/
int memStackLength (Obj obj) {
	#if DEBUG
	if (!(memIsObjectInHeap(&heap, obj)
	      || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj)
		   || memIsObjectInHeap(&heapNew, obj))) {
		DB ("ERROR memStackLength(obj %08x) Invalid object.", obj);
		memError();
	} else if (!memIsObjectStack(obj)) {
		DB ("ERROR memStackLength(obj %08x) Not stack type.", obj);
		memError();
	}
	#endif
	return ((Obj**)obj)[0] - (Obj*)obj;
}

/* Calculate a pointer object's index offset into the object it's pointing
   at. A pointer is a vector the first element a void*, the second the object
   that the void* should be pointing somewhere inside of. */
unsigned memPointerOffset (Obj obj) {
	return (Obj*)memVectorObject(obj, 0) - (Obj*)memVectorObject(obj, 1);
}



/* Given pointer to an object, move object into the new heap and update
   the object to point to new address.  Object must be valid and exist in
   either the old or young heap so this check must be done before calling.
*/
const int GC_MODE_YOUNG=0;
const int GC_MODE_OLD=1;

int GarbageCollectionMode;

void memObjectCopyToHeapNew (Obj *obj) {
 Obj newObjectLocation;
	DB("   -->memObjectCopyToHeapNew(obj %x)", obj);

	/* Only move objects from concerned generation.  Either young heap or both
	   young and old heap. */
	if (!(memIsObjectInHeap(&heap, *obj)
	      || (GarbageCollectionMode==GC_MODE_OLD
	          && memIsObjectInHeap(&heapOld, *obj)))) {
		DBE fprintf (stderr, "...Ignoring");
		goto ret;
	}

	/* If shadow object, grab the forwarding pointer which is found in the
	   first word, otherwise copy object over. */
	if (memIsObjectShadow (*obj)) {
		DBE fprintf (stderr, "...Shadow");
		newObjectLocation = memVectorObject(*obj, 0);
	} else {
		/* Next object location in new heap. */
		newObjectLocation = heapNew.next + sizeof(Descriptor);
	
		/* Copy bytes that make up the stack which include the descriptor,
		   pointer and live object pointers... */
		if (memIsObjectStack (*obj)) {
			DBE fprintf (stderr, "...Stack");
			/* Add 1 to stack length to account for pointer. */
			memcpy(heapNew.next,  *obj-sizeof(Descriptor),
			       sizeof(Descriptor) + sizeof(Obj)
			       + (1+memStackLength(*obj))*sizeof(Obj));
			/* Update this stack's internal pointer (first word in vector) to
				point to the proper position in the vector. */
			*(Obj*)newObjectLocation = (Obj*)newObjectLocation
			                           + memStackLength(*obj);
		/* ... or just copy bytes that make up this object. */
		} else {
			DBE fprintf (stderr, "...Array");
			memcpy(heapNew.next, *obj-sizeof(Descriptor), memObjectSize(*obj));
			/* Tally copied dirty objs. */
			if (memIsObjectFinalizer(*obj)) heapNew.finalizerCount++;
		}
	
		/* Mutate object in young heap into a shadow type.  This involves
		   mutating the descriptor's type into a shadow and length into the
		   proper vector length.  This length is only used for debugging
			purposes when traversing over a heap containing shadow types... */
		*((Descriptor*)*obj-1) = memMakeDescriptor(TSHADOW,
		                            memObjectSize(*obj)/sizeof(Descriptor) - 1);
		/* ... and mutate object's first word into a pointer to the new
		   location in the new heap thus completing shadow type mutation. */
		*(Obj*)*obj = newObjectLocation;
	
		/* Increment new heap's next pointer, the next spot for any new copied
		   or created object. */
		heapNew.next += memObjectSize(*obj);
	}

	/* Using the pointer to the object, mutate the object so that it points at the new location in the new heap. */
	*obj = newObjectLocation;


	/* This assertion should never occur. */
	if (heapNew.next >= heapNew.last) {
		fprintf(stderr, "\aERROR: heapNew overflow. Should never have happened.");
		memError();
	}

	ret:
	DB("   <--memObjectCopyToHeapNew");
}



/* Function pointers called before and after a GC proper.
*/

fp memPreGarbageCollect  = 0,
   memPostGarbageCollect = 0;

void memGarbageCollect (void) {
 Obj newObj;
 long  i, len;
	DB("-->memGarbageCollect()");
	//memDebugDumpHeapStructures();
	//fprintf (stderr, "\a");

	if (memPreGarbageCollect) memPreGarbageCollect();

	if (GarbageCollectionMode==GC_MODE_YOUNG) {
		DB("   memGarbageCollect   young gc   old count %x   young count %x",
		heapOld.blockCount, heap.blockCount);
	} else {
		DB("   memGarbageCollect   old gc   old count %x   young count %x",
		heapOld.blockCount, heap.blockCount);
	}


	garbageCollectionCount++;

	/* Initialize heap 'new' with enough space to contain all of heap 'old'
	   and double heap 'young's objects plus the blocks needed to contain
	   the desired object size.  This will eventually become a new 'old' heap
	   or young heap.  The block count is shrunk to fit perfectly the old heap's
	   block usage.  The young heap is shrunk to double a constant value.  If
	   the usage is half this 'double' size, then the next GC will be a complete
	   one (collect over young and old).
	*/
	memInitializeHeap (&heapNew, heapOld.blockCount + HEAP_BLOCK_COUNT
	           + (NewDesiredObjectSize+BLOCK_BYTE_SIZE-1)/BLOCK_BYTE_SIZE);

	/* Reset the global variable that specifies how many were needed. */
	NewDesiredObjectSize=0;

	/* Copy objects in registers to new heap. */
	DB("   COLLECTING REGISTERS");
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
	}
	*/


	/* Compact object referenced by vectors in old heap. */
	if ( GarbageCollectionMode==GC_MODE_YOUNG) {
		DB("   Collecting objects referenced by old heap objects.");
		newObj = heapOld.start + sizeof(Descriptor);
		while (newObj < heapOld.next) {
			if (memIsObjectVector(newObj)) {
				if (memIsObjectPointer(newObj)) {
					DB("      Pointer in old heap");
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
						DB("   Stack in old heap.  Object %d/%d %08x", i, memStackLength(newObj), (Obj*)newObj+i+1);
						memObjectCopyToHeapNew((Obj*)newObj+i+1);
					}
				} else {
					for (i=0; i<memObjectLength(newObj); i++) {
						DB("   Vector in old heap.  Object %d/%d %x ", i, memObjectLength(newObj), (Obj*)newObj+i);
						memObjectCopyToHeapNew((Obj*)newObj+i);
					}
				}
			}
			/* Consider next object in old heap and continue compacting. */
			newObj += memObjectSize(newObj);
		}
	}


	/* Look at first object copied to new heap.  This starts the copying of the
	   objects found in all vectors in this heap. */
	DB("   COLLECTING OBJECTS IN YOUNG HEAP");
	newObj = heapNew.start + sizeof(Descriptor);
	while (newObj < heapNew.next) {
		if (memIsObjectVector(newObj)) {
			if (memIsObjectPointer(newObj)) {
				DB("   Pointer in young heap.");
				/* Figure out pointer offset. */
				len = memPointerOffset(newObj);
				/* Update pointer object's object pointer. */
				memObjectCopyToHeapNew((Obj*)newObj+1);
				/* Update pointer object's index pointer.  This should even work
				   if the object is in an old heap and a young GC is being
				   performed.*/
				memVectorSet(newObj, 0, (Obj*)memVectorObject(newObj, 1)+len);
			} else if (memIsObjectStack(newObj)) {
				DB("   Stack in young heap.");
				for (i=0; i<memStackLength(newObj); i++) {
					DB("   stack %d/%d %x", i, memStackLength(newObj), (Obj*)newObj+i+1);
					memObjectCopyToHeapNew((Obj*)newObj+i+1);
				}
			} else {
				DB("   Vector in young heap.");
				for (i=0; i<memObjectLength(newObj); i++) {
					DB("   vector %d/%d %x", i, memObjectLength(newObj), (Obj*)newObj+i);
					memObjectCopyToHeapNew((Obj*)newObj+i);
				}
			}
		}
		/* Consider next object in new heap and continue compacting. */
		newObj += memObjectSize(newObj);
	}

	/* Check finalizer count. */
	if (heapNew.finalizerCount != (heap.finalizerCount
	                            + (GarbageCollectionMode == GC_MODE_OLD
	                               ? heapOld.finalizerCount : 0))) {
		fprintf (stderr, "\n\aWARNING: memGarbageCollect() finalizer count difference");
	}

	/* Free up heap and old heap if doing an old heap collection.  The live
	   objects have all been copied over to the new heap now. */
	DB("   FREEING AND SHRINKING");
	memFreeHeap (&heap);
	memResetHeap(&heap);
	if (GarbageCollectionMode == GC_MODE_OLD) {
		memFreeHeap (&heapOld);
		memResetHeap(&heapOld);
		/* Shrink oldHeap bounds to fit actual usage. */
		memShrinkHeap(&heapNew, (heapNew.last-heapNew.next)/BLOCK_BYTE_SIZE);
		/* Reassign new heap to old heap. */
		heapOld = heapNew;
		memInitializeHeap (&heap, HEAP_BLOCK_COUNT);
		GarbageCollectionMode = GC_MODE_YOUNG;
//		mutatedOldObjectsLength = 0;
	} else { /* GarbageCollectionMode == GC_MODE_YOUNG */
		/* Reassign new heap to young heap. */
		heap = heapNew;
		/* If a non-generational collection results in a certain heap usage still,
		   then force a generational collection next time around. */
		if (((heap.next-heap.start)/BLOCK_BYTE_SIZE)*BLOCK_BYTE_SIZE
		    > HEAP_BLOCK_COUNT/2) {
			//printf ("\n\aWARNING: memGarbageCollect() young heap usage beyond half.\n");
			GarbageCollectionMode = GC_MODE_OLD;
		} else {
			/* Shrink oldHeap bounds to fit actual usage. */
			memShrinkHeap(&heap, heap.blockCount-HEAP_BLOCK_COUNT);
		}
	}

	if (garbageCollectionCount%5 == 0)
		GarbageCollectionMode = GC_MODE_OLD;

	/* Reset empty young heap...the bigger this is the fewwer GC's occuring.
	   and since it's a generational collector the GC shouldn't copy very
	   many objects anyways (most objects will be short lived). */
	memResetHeap(&heapNew);

	if (memPostGarbageCollect) memPostGarbageCollect();
	//memDebugDumpHeapStructures();
	DB("<--memGarbageCollect");
}



void memDebugObjectDump (Obj o) {
 int i;
  printf ("\n%08x %08x", (unsigned)o, (unsigned)memObjectDescriptor(o));
  if (memObjectLength(o)) {
	if (memIsObjectVector(o)) {
		if (memIsObjectPointer(o))
			printf (" %x -> %x", ((unsigned*)o)[0], ((unsigned*)o)[1]);
		else if (memIsObjectStack(o)) {
			printf (" [%x | ", memStackLength(o));
			for (i=0; i<memStackLength(o); i++)
				printf ("%x ", ((unsigned*)o)[i+1]);
			printf ("]");
		} else if (memIsObjectShadow(o))
			printf (" *%08x", *(unsigned*)o);
		else if (memIsObjectFinalizer(o))
			printf (" (%08x)()", *(unsigned*)o);
		else {
			for (i=0; i<memObjectLength(o); i++) {
				printf (i==0 ? " #(" : " ");
				printf ("%x", ((unsigned*)o)[i]);
			}
			printf (")");
		}
	} else {
		for (i=0; i<memObjectLength(o); i++) {
			printf (i==0 ? "  (" : " ");
			printf ("%x", ((u8*)o)[i]);
		}
		printf (")");
	}
  }
}

void memDebugDumpHeapStructures (void) {
 Obj o;
	DB("-->memDebugDumpHeapStructures()");
	printf ("\n       Static    Old    Current    New\n");
	printf ("Start %08x %08x %08x %08x\n",
	        (unsigned)heapStatic.start,  (unsigned)heapOld.start,
	        (unsigned)heap.start,  (unsigned)heapNew.start);
	printf ("Next  %08x %08x %08x %08x\n",
	        (unsigned)heapStatic.next,  (unsigned)heapOld.next,
	        (unsigned)heap.next,  (unsigned)heapNew.next);
	printf ("last  %08x %08x %08x %08x\n",
	        (unsigned)heapStatic.last,  (unsigned)heapOld.last,
	        (unsigned)heap.last,  (unsigned)heapNew.last);
	printf ("Blocks  %6lx   %6lx   %6lx   %6lx\n",
	        heapStatic.blockCount,  heapOld.blockCount,
	        heap.blockCount,        heapNew.blockCount);
	printf ("Bytes   %6x   %6x   %6x   %6x\n",
	        heapStatic.last-heapStatic.start,  heapOld.last-heapOld.start,
	        heap.last-heap.start,              heapNew.last-heapNew.start);
	printf ("Finalizer %4lx   %6lx   %6lx   %6lx\n",
	        heapStatic.finalizerCount,  heapOld.finalizerCount,
	        heap.finalizerCount,        heapNew.finalizerCount);
	printf ("   -0-      -1-      -2-      -3-      -4-      -5-      -6-     -7-\n");
	printf ("%08x %08x %08x %08x %08x %08x %08x %08x\n",
	        (unsigned)r0, (unsigned)r1, (unsigned)r2, (unsigned)r3,
	        (unsigned)r4, (unsigned)r5, (unsigned)r6, (unsigned)r7);
	printf ("%08x %08x %08x %08x %08x %08x %08x %08x\n",
	        (unsigned)r8, (unsigned)r9, (unsigned)ra, (unsigned)rb,
	        (unsigned)rc, (unsigned)rd, (unsigned)re, (unsigned)rf);
	printf ("   -8-      -9-      -a-      -b-      -c-      -d-      -e-      -f-\n");
	printf ("%08x %08x %08x %08x %08x %08x %08x %08x\n",
	        (unsigned)r10, (unsigned)r11, (unsigned)r12, (unsigned)r13,
	        (unsigned)r14, (unsigned)r15, (unsigned)r16, (unsigned)r17);
	printf ("%08x %08x %08x %08x %08x %08x %08x %08x\n",
	        (unsigned)r18, (unsigned)r19, (unsigned)r1a, (unsigned)r1b,
	        (unsigned)r1c, (unsigned)r1d, (unsigned)r1e, (unsigned)r1f);

	/* DUMP EACH HEAP OBJECT */
	printf ("----STATIC HEAP----");
	o = heapStatic.start + sizeof(Descriptor);
	while (o<heapStatic.next) {
		memDebugObjectDump (o);
		o += memObjectSize(o);
	}
	printf ("\n----OLD HEAP----");
//	for (i=0; i<mutatedOldObjectsLength; i++) {
//		printf (" %08x", mutatedOldObjects[i]);
//	}
	o = heapOld.start + sizeof(Descriptor);
	while (o<heapOld.next) {
		memDebugObjectDump (o);
		o += memObjectSize(o);
	}
	printf ("\n----YOUNG HEAP----");
	o = heap.start + sizeof(Descriptor);
	while (o<heap.next) {
		memDebugObjectDump (o);
		o += memObjectSize(o);
	}
	printf ("\n----NEW HEAP----");
	o = heapNew.start + sizeof(Descriptor);
	while (o<heapNew.next) {
		memDebugObjectDump (o);
		o += memObjectSize(o);
	}
/*
	// Dump raw words from this heap.
	o =heapNew.start;
	while (o<heapNew.next) {
		printf ("\n%08x %08x", o, *(unsigned*)o);
		o += 4;
	}
*/

	DB("<--memDebugDumpHeapStructures()");
}



/* This must be called before usage of this library.  It sets up the various
   heaps.  These include static, old and current the current generational
   collector.
 */
void memInitialize (fp preGC, fp postGC) {
 int static shouldInitialize=1;
	if (shouldInitialize) {
		shouldInitialize=0;
		DB("-->memInitialize()");
		if (preGC) memPreGarbageCollect = preGC;
		if (postGC) memPostGarbageCollect = postGC;
		memInitializeHeap (&heapStatic, HEAP_STATIC_BLOCK_COUNT);
		/* Create bogus old heap so things work properly. */
		memInitializeHeap (&heapOld,    1);
		memInitializeHeap (&heap,       HEAP_BLOCK_COUNT);
		GarbageCollectionMode = GC_MODE_OLD;
		DB("<--memInitialize");
	}
}


void memError (void) {
	fprintf (stderr, "\nHalted on error.  Want a Heap dump (y/n)?");
	memDebugDumpHeapStructures();
	if (getchar()=='y') memDebugDumpHeapStructures();
	exit(-1);
}

int memmain (void) {
 int i, j;
	setbuf(stdout,0);
	memInitialize(0, 0);

	memNewArray(TSYMBOL, 8); r1=r0;
	memNewArray(TINTEGER, 4); r2=r0;
	memNewVector(TPAIR, 1); r3=r0;
	memNewStack(); rf=r0;
	memStackPush(rf, r1);
	memStackPush(rf, r2);
	memStackPush(rf, r3);
	memVectorSet(r3, 0, r1);
	memVectorSet(r3, 1, r2);
	memDebugDumpHeapStructures();
	memGarbageCollect();
	memDebugDumpHeapStructures();
	memGarbageCollect();
	memDebugDumpHeapStructures();
	goto ret;

	memVectorSet(r9, 1, r2);
	memVectorSet(r9, 2, r3);
	memVectorSet(r9, 3, r4);
	memNewFinalizer  ();  r10 = r0; *(Obj*)r10 = r2;// (cleaner*)(), int fd
	memNewPointer ();  r11 = r0; ((Obj*)r11)[1]=r9;((Obj*)r11)[0]=r9+4;
	memStackPush(rf, r1);
	memStackPush(rf, r2);
	memStackPush(rf, r3);
	memStackPush(rf, r8);
	memStackPush(rf, r9);
//	memGarbageCollect();
	memNewVector (TPAIR, 2);
	memStackPush(rf, r0);
	//GarbageCollectionMode = GC_MODE_OLD; memGarbageCollect();
l:
	for (i=0; i<16; i++) {
		memNewArray(TINTEGER, 16);
		memStackPush(rf, r0);
		for (j=0; j<16; j++) ((char*)r0)[j] = j+i;
	}
	//for (i=0; i<16; i++) memStackPop(rf);
//	GarbageCollectionMode = GC_MODE_OLD;
//	memGarbageCollect();
//	GarbageCollectionMode = GC_MODE_OLD;
//	memGarbageCollect();
//	memNewArray(TSYMBOL, 128);
	//memDebugDumpHeapStructures();
goto  l;
ret:
	fprintf (stderr,"\n");
	return 0;
}
