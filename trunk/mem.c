#define DEBUG 0
#define DEBUG_ASSERT 1
#define DEBUG_ASSERT_STACK 1
#define VALIDATE_HEAP 0
#define DB_MODULE "MEM "
#include "debug.h"
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
/* Two sets of objects, old and young,  will be maintained.  Normally, the
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
   size  -- Inspired by sizeof() operator.  The foot print this object requires
            internally in bytes.
   count -- Number of blocks this object uses in memory.  Might be bytes or
            other block size.
   length-- Inspired by scheme primitives length and string-length.  Number of active
            'elements' this object references.  In a vector, the number of objects
            locations.  In an array, the byte required to represent the value (such
            as number of characters).  Mainly used for external representations of objects.

            Note: the array length might be smaller than the object's count.  There
            might be other meta data associated with the object that size will return.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <assert.h>
#include "mem.h"


extern void vmDebugDumpCode (Obj c, FILE *stream);

void memGarbageCollectInternal (Descriptor desc, Num byteSize);
void memError (void);


/* Byte size of a Linux virtual memory block.  Resolution of mmap. */
const Num BLOCK_BYTE_SIZE         = 0x1000; //     4Kb 0x001000 2^12
const Num HEAP_STATIC_BLOCK_COUNT =  0x008; //    32Kb 0x008000 2^15
const Num HEAP_BLOCK_COUNT        =  0x400; // 4Mb     0x400000 2^22
//const Num HEAP_INCREMENT_COUNT   = 0x040; //   256Kb 0x040000 2^18
//const Num HEAP_DECREMENT_COUNT   = 0x010; //    64Kb 0x010000 2^16
const Num STACK_LENGTH           = 0x04000; //    16Kb 0x004000 2^14


Num garbageCollectionCount = 0;
const Num DescSize = sizeof(Descriptor);
const Num ObjSize = sizeof(Obj);



/* Heap structures and the four instances.
*/
typedef struct {
	Obj start;  /* Initial heap location. */
	Obj next;   /* Next available heap location. */
	Obj last;   /* One byte past the last heap location (exclusive). */
	Num blockCount; /* Number of blocks this heap uses.  4096 byte blocks assumed. */
	Num finalizerCount; /* Number of finalizer types contained in this heap. */
} Heap;

Heap heapOld,   /* Where old objects live during runtime. */
     heap,      /* Where new young objects are created during runtime. */
     heapNew,   /* GC work area. */
     heapStatic;/* Objects that are never deleted nor touched by the GC. */



/* Initialize a heap struct and allocate 'count' number of blocks.
 */
void memInitializeHeap (Heap *h, Num blockCount) {
 Num bytes = BLOCK_BYTE_SIZE * blockCount;
	DB ("   ::memInitializeHeap(heap &%s)", h==&heapOld?"heapOld":h==&heap?"heap":h==&heapNew?"heapNew":h==&heapStatic?"heapStatic":"???");
	h->start = h->next =
		mmap(0x0, bytes, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	if (MAP_FAILED == h->start) {
		fprintf (stderr, "ERROR heap_init() Can't create initial heap ret=%p\n",
				h->start);
		memError();
	}
	h->last = h->start + bytes; /* Last pointer is exclusive. */
	h->blockCount = blockCount;
	h->finalizerCount=0;
	DB ("   "OBJ"..."OBJ"  "HEX, h->start, h->last, bytes);
	DB ("   --memInitializeHeap()");
}

/* Unallocate blockCount from tail part of object linux-memory. */
void memShrinkHeap (Heap *h, Num blockCount) {
 Num newBlockCount;
	DB ("::memShrinkHeap(%s)", h==&heapOld?"old":h==&heap?"heap":h==&heapNew?"new":"???");
	if (0 < blockCount && blockCount <= h->blockCount) {
		newBlockCount = h->blockCount - blockCount;
		DB ("   range "OBJ"...%08lx", h->start+newBlockCount*BLOCK_BYTE_SIZE, blockCount*BLOCK_BYTE_SIZE);
		if (munmap(h->start+newBlockCount*BLOCK_BYTE_SIZE, blockCount*BLOCK_BYTE_SIZE-1))
			fprintf (stderr, "\aWARNING: memShrinkHeap() can't shrink *heap region");
		h->last = h->start + newBlockCount * BLOCK_BYTE_SIZE;
		h->blockCount = newBlockCount;
	}
	DB ("--memShrinkHeap()");
}

/* Release heap's blocks back to Linux.
*/
void memFreeHeap (Heap *h) {
	DB ("::memFreeHeap(%s)", h==&heapOld?"old":h==&heap?"heap":h==&heapNew?"new":"???");
	memShrinkHeap (h, h->blockCount);
	DB ("--memFreeHeap()");
}

/* Reset heap so that it appears empty again.  Called after a heap structure
   is moved into another.
 */
void memResetHeap (Heap *h) {
	DB ("::memResetHeap(%s)", h==&heapOld?"old":h==&heap?"heap":h==&heapNew?"new":"???");
	h->next = h->start = h->last = (void*)0;
	h->blockCount = h->finalizerCount = 0;
	DB ("--memResetHeap()");
}



/* A 'Type' is an 64 bit byte composed of a 1 bit 'class' and 7 bit 'id'.  The
   upper 56 bits are unused.

 Type:
  [-------- -------- -------- -------- -------- -------- --------][c|idididi]
                  unused                                         class  id

   A 'Descriptor' is a 64 bit word composed of an 8 bit 'type' shifted and
   or'ed with a 56 bit 'count'.

                Descriptor:  [--------][-------- -------- -------- -------- -------- -------- --------]
                                type               size
*/

/* Create a descriptor given 'type' and 'length' values. */
Descriptor memMakeDescriptor (Type type, LengthType length) {
	return (type<<56) | length;
}



/* Get object's 'descriptor' value.
*/
Descriptor memObjectDescriptor (Obj o) {
	return *((Descriptor*)o - 1);
}

/* Get object's 'type'.
*/
Type memObjectType (Obj o) {
	return memObjectDescriptor(o)>>56;
}

/* Get object's 'length'.
*/
Num memObjectLength (Obj o) {
	return memObjectDescriptor(o) & 0x00ffffffffffffff;
}



/* Is object an array or vector base type class? */
Num memIsObjectArray (Obj o) { return !(memObjectType(o) & TBASEVECTOR); }
Num memIsObjectVector (Obj o) { return memObjectType(o) & TBASEVECTOR; }

/* Is object a special type? */
Num memIsObjectFinalizer (Obj o) { return memObjectType(o) == TFINALIZER; }
Num memIsObjectPointer (Obj o) { return memObjectType(o) == TPOINTER; }
Num memIsObjectStack (Obj o) { return memObjectType(o) == TSTACK; }
Num memIsObjectShadow (Obj o) { return memObjectType(o) == TSHADOW; }



/* Compute object size (total memory footprint
   in bytes) based on an array's 'length'.
*/
Num memArrayLengthToObjectSize  (LengthType length) {
	return DescSize + (length + DescSize-1) & -DescSize;
}

/* Compute object size based on vector's 'length'.
*/
Num memVectorLengthToObjectSize (LengthType length) {
	return DescSize + length*DescSize;
}

/* Get object's byte 'size' including the descriptor.  Used to find next
   object in the heap or byte count for copying into another heap.
*/
Num memObjectSize (Obj o) {
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
   live object will be collected which is bad.  TODO
int mutatedOldObjectsLength=0;
Obj *(mutatedOldObjects[0x400])={0};
*/



/* Allocate an object in the heap:

   The descriptor paramater is a long [tsssssss] t=type-field s=size=field.
   A type >= 0x80 is assumed a vector of objects (pointers, length 8 on IA64
   boxen). A type < 0x80 is raw bytes.  This restriction is used by the gc.
  
   The size paramater is the number of actual bytes the object is composed
   of plus the descriptor size.  it should be at least 3 to allow for mutation
   into a shadow by the gc.
*/
void memNewObject (Descriptor desc, Num byteSize) {
 int i;
	DB("   ::memNewObject(desc "HEX", byteSize "NUM")", desc, byteSize);
	*(Descriptor*)(heap.next) = desc; /* Set descriptor. */
	r0 = heap.next + DescSize; /* Set r0 to object memory location. */
	heap.next += byteSize; /* Set pointer to next available heap position. */

	/* Check that we're still within the buffer boundary. Perform a GC if no more space. */
	if (heap.next >= heap.last) {
		r0 = 0;
		heap.next -= byteSize;
		memGarbageCollectInternal(desc, byteSize);
		memNewObject(desc, byteSize);
	} else {
		 // Zero out the new object.
		for (i=0; i<(byteSize-DescSize)/DescSize; ++i) { ((Num*)r0)[i] = 0x00; }
	}
	DB("   --memNewObject()");
}

/* Allocate a static object.  Like memNewObject except it can't fail.
*/
void memNewStaticObject (Descriptor desc, Num byteSize) {
	DB("::memNewStaticObject(desc "HEX", byteSize %lx)", desc, byteSize);
	*(Descriptor*)heapStatic.next = desc;
	r0 = heapStatic.next + DescSize;
	heapStatic.next += byteSize;

	/* Check that we're still within the static buffer boundary. */
	if (heapStatic.next >= heapStatic.last) {
		fprintf (stderr, "ERROR: memNewStaticObject(): Static buffer overflow.\a\n");
		memError();
	}
	DB("--memNewStaticObject()");
}


void memNewStatic (Type type, LengthType length) {
	DB("::memNewStatic(type "X64", length %lx)", type, length);
	memNewStaticObject (memMakeDescriptor(type, length),
	                    memArrayLengthToObjectSize(length));
	DB("--memNewStatic()");
}

void memNewStaticVector (Type type, LengthType length) {
	DB("::memNewStaticVector(type "X64", length %x)", type, length);
	memNewStaticObject (memMakeDescriptor(type, length),
	                    memVectorLengthToObjectSize(length));
	DB("--memNewStaticVector()");
}

void memNewArray (Type type, LengthType length) {
	DB("::memNewArray(type "X64", length %x)", type, length);
	memNewObject (memMakeDescriptor(type, length),
	              memArrayLengthToObjectSize(length));
	DB("--memNewArray()");
}

void memNewVector (Type type, LengthType length) {
	DB("::memNewVector(type "X64", length %d)", type, length);
	#if DEBUG_ASSERT
	if (MEMOBJECTMAXSIZE < length) { /* Sanity check. For now prevent 4G sized objects. */
		fprintf (stderr, "ERROR memNewVector (Type "HEX016" length "HEX016"): length too big.", type, length);
		memError();
	}
	#endif
	memNewObject (memMakeDescriptor(type, length),
	              memVectorLengthToObjectSize(length));
	DB("--memNewVector()");
}

/* Finalizer is just a C function pointer that is called when this object
   becomes garbage.
*/
void memNewFinalizer (void) {
	DB("::memNewFinalizer()");
	memNewObject (memMakeDescriptor(TFINALIZER, 1),
	              memVectorLengthToObjectSize(1));
	heap.finalizerCount++;
	DB("--memNewFinalizer");
}

/* Pointer object is a normal C pointer followed by an object pointer
   which the first pointer should be pointing into.
*/
void memNewPointer (void) {
	DB("::memNewPointer()");
	memNewObject (memMakeDescriptor(TPOINTER, 2),
	              memVectorLengthToObjectSize(2));
	DB("--memNewPointer");
}

/* First item in stack vector is a pointer to an address in the vector.
   Initially it points to itself, implying an empty stack.
*/
void memNewStack (void) {
	DB("::memNewStack()");
	memNewObject (memMakeDescriptor(TSTACK, STACK_LENGTH+1),
	              memVectorLengthToObjectSize(STACK_LENGTH+1));
	*(Obj*)r0 = (Obj)r0;
	DB("--memNewStack");
}



/* Object heap queries.
*/
int memIsObjectInHeap (Heap *heap, Obj o) {
	return heap->start <= o && o < heap->last;
}

/* Check that object pointer is in a valid heap address. */
int memIsObjectValid  (Obj o) {
	return memIsObjectInHeap(&heap, o)
	       || memIsObjectInHeap(&heapOld, o)
	       || memIsObjectInHeap(&heapStatic, o);
}


/* Object mutators.
*/
void memArraySet (Obj obj, Num offset, u8 item) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		DB("ERROR memArraySet(obj "OBJ" offset "NUM" item %02x) Invalid object.",
		    obj, offset, item);
		memError();
	} else if(!memIsObjectArray(obj)) {
		DB("ERROR memArraySet(obj "OBJ" offset "NUM" item %02x) Not array class.",
		    obj, offset, item);
		memError();
	} else if(offset<0 || memObjectLength(obj)<=offset ) {
		DB("ERROR memArraySet(obj "OBJ" offset "NUM" item %02x) Invalid offset.",
		    obj, offset, item);
		memError();
	}
	#endif
	*((u8*)obj+offset)=item;
}

void memVectorSet (Obj obj, Num offset, Obj item) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		DB ("ERROR memVectorSet(obj "OBJ" offset "NUM" item "OBJ") Invalid object.",
		    obj, offset, item);
		memError();
	} else if (!memIsObjectVector(obj)) {
		DB ("ERROR memVectorSet(obj "OBJ" offset "NUM" item "OBJ") Not vector class.",
		    obj, offset, item);
		memError();
	} else if (offset < 0 || memObjectLength(obj) <= offset ) {
		DB ("ERROR memVectorSet(obj "OBJ" offset "NUM" item "OBJ") Invalid offset.",
		    obj, offset, item);
		memError();
	}
	#endif
	*((Obj*)obj+offset)=item;
}

void memStackPush (Obj obj, Obj item) {
	#if DEBUG_ASSERT_STACK
	if (!memIsObjectValid(obj)) {
		fprintf (stderr, "ERROR memStackPush(obj "OBJ" item "OBJ") Invalid object.", obj, item);
		memError();
	} else if (!memIsObjectStack(obj)) {
		fprintf (stderr, "ERROR memStackPush(obj "OBJ" item "OBJ") Not stack type.", obj, item);
		memError();
	} else if (STACK_LENGTH <= memStackLength(obj)) {
		fprintf (stderr, "ERROR memStackPush(obj "OBJ" item "OBJ") Stack overflow.", obj, item);
		memDebugDumpObject(r1f, stderr);
		memError();
	}
	#endif
	*++(*(Obj**)obj)=item;
}

void memStackSet (Obj obj, Num topOffset, Obj item) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		DB ("ERROR memStackSet(obj "OBJ" topOffset %x item "OBJ") Invalid object.",
		    obj, topOffset, item);
		memError();
	} else if (!memIsObjectStack(obj)) {
		DB ("ERROR memStackSet(obj "OBJ" topOffset %x item "OBJ") Not stack type.",
		    obj, topOffset, item);
		memError();
	} else if (topOffset<0 || memStackLength(obj)<=topOffset) {
		DB ("ERROR memStackSet(obj "OBJ" topOffset %x item "OBJ") Invalid offset. memStackLength(obj)=>%x", obj, topOffset, item, memStackLength(obj));
		memError();
	}
	#endif
	*(*(Obj**)obj-topOffset)=item;
}

Obj  memStackPop (Obj obj) {
 Obj ret;
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		DB ("ERROR memStackPop(obj "OBJ") Invalid object.", obj);
		memError();
	} else if (!memIsObjectStack(obj)) {
		DB ("ERROR memStackPop(obj "OBJ") Not stack type.", obj);
		memError();
	} else if (memStackLength(obj) <= 0) {
		DB ("ERROR memStackPop(obj "OBJ") Stack underflow.", obj);
		memError();
	}
	#endif
	ret = **(Obj**)obj;
	/* After popping, clear the value.  Mainly for debugging.  During a garbage
	   collection, the object pointers will be ignored. */
	*(*(Obj**)obj)-- = 0;

	return  ret;
}



/* Object accessors.
 */
u8 memArrayObject (Obj obj, Num offset) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		DB ("ERROR memArrayObject(obj "OBJ" offset "NUM") Invalid object.",
		    obj, offset);
		memError();
	} else if (!memIsObjectArray(obj)) {
		DB ("ERROR memArrayObject(obj "OBJ" offset "NUM") Not array class.",
		    obj, offset);
		memError();
	} else if (offset<0 || memObjectLength(obj)<=offset ) {
		DB ("ERROR memArrayObject(obj "OBJ" offset "NUM") Invalid offset.",
		    obj, offset);
		memError();
	}
	#endif
	return *((u8*)obj+offset);
}

Obj memVectorObject (Obj obj, Num offset) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		DB ("ERROR memVectorObject(obj "OBJ" offset "NUM") Invalid object.",
		    obj, offset);
		memError();
	} else if (!memIsObjectVector(obj)) {
		DB ("ERROR memVectorObject(obj ["OBJ"] "OBJ" offset "NUM") Not vector class.",
			memObjectDescriptor(obj),
		   obj, offset);
		memError();
	} else if (offset<0 || memObjectLength(obj)<=offset ) {
		DB ("ERROR memVectorObject(obj "OBJ" offset "NUM") Invalid index.",
		    obj, offset);
		//r0=code; vmDebugDump();  // Debug dump the code object.
		memError();
	}
	#endif
	return *((Obj*)obj+offset);
}

Obj memStackObject (Obj obj, Num topOffset) {
	#if DEBUG_ASSERT
	if (!memIsObjectValid(obj)) {
		DB ("ERROR memStackObject(obj "OBJ" topOffset "NUM"): Invalid object.",
		    obj, topOffset);
		memError();
	} else if (!memIsObjectStack(obj)) {
		DB ("ERROR memStackObject(obj "OBJ" topOffset "NUM"): Not stack type.",
		    obj, topOffset);
		memError();
	} else if (topOffset<0 || memStackLength(obj)<=topOffset ) {
		DB ("ERROR memStackObject(obj "OBJ" topOffset "NUM"): Invalid top offset.",
		    obj, topOffset);
		memError();
	}
	#endif
	return *(*(Obj**)obj-topOffset);
}

/* Number of elements pushed onto stack.
*/
Num memStackLength (Obj obj) {
	#if DEBUG_ASSERT_STACK
	if (!(memIsObjectInHeap(&heap, obj)
	      || memIsObjectInHeap(&heapOld, obj)
	      || memIsObjectInHeap(&heapStatic, obj)
		   || memIsObjectInHeap(&heapNew, obj))) {
		DB ("ERROR memStackLength(obj "OBJ") Invalid object.", obj);
		memError();
	} else if (!memIsObjectStack(obj)) {
		DB ("ERROR memStackLength(obj "OBJ") Not stack type.", obj);
		memError();
	}
	#endif
	return (Num)(((Obj**)obj)[0] - (Obj*)obj);
}

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
 Int i;
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
const int GC_MODE_YOUNG=0;
const int GC_MODE_OLD=1;

int GarbageCollectionMode;

void memObjectCopyToHeapNew (Obj *obj) {
 Obj newObjectLocation;
	DB("   ::memObjectCopyToHeapNew(obj "OBJ")", obj);

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
		newObjectLocation = heapNew.next + DescSize;
	
		/* Copy bytes that make up the stack which include the descriptor,
		   pointer and live object pointers... */
		if (memIsObjectStack (*obj)) {
			DBE fprintf (stderr, "...Stack");
			/* Add 1 to stack length to account for pointer. */
			memcpy(heapNew.next, *obj-DescSize, (DescSize + ObjSize + ObjSize*memStackLength(*obj)));
			/* Update this stack's internal pointer (first word in vector) to
				point to the proper position in the vector. */
			*(Obj*)newObjectLocation = (Obj*)newObjectLocation + memStackLength(*obj);
		/* ... or just copy bytes that make up this object. */
		} else {
			DBE fprintf (stderr, "...Array");
			memcpy(heapNew.next, *obj-DescSize, memObjectSize(*obj));
			/* Tally copied dirty objs. */
			if (memIsObjectFinalizer(*obj)) heapNew.finalizerCount++;
		}
	
		/* Mutate object in young heap into a shadow type.  This involves
		   mutating the descriptor's type into a shadow and length into the
		   proper vector length.  This length is only used for debugging
			purposes when traversing over a heap containing shadow types... */
		*((Descriptor*)*obj-1) = memMakeDescriptor(TSHADOW,
		                            memObjectSize(*obj)/DescSize - 1);
		/* ... and mutate object's first word into a pointer to the new
		   location in the new heap thus completing shadow type mutation. */
		*(Obj*)*obj = newObjectLocation;
	
		/* Increment new heap's next pointer, the next spot for any new copied
		   or created object. */
		heapNew.next += memObjectSize(*obj);
	}

	/* Using the pointer to the object, mutate the object so that it points at the new location in the new heap. */
	*obj = newObjectLocation;


	/* This assertion should never occur.  Should never garbage collect into a heap not big enough. */
	assert(heapNew.next < heapNew.last);

	ret:
	DB("   --memObjectCopyToHeapNew");
}



/* Function pointers called before and after a GC proper.
*/

Func memPreGarbageCollect  = 0,
     memPostGarbageCollect = 0;
char memGCFlag=0;


/* Internal garbage collection call.  Passed in for debuggin is the
   desired new object descriptor as well as the number of bytes used
   to represent (descriptor byte count included) the object.
*/
void memGarbageCollectInternal (Descriptor desc, Num byteSize) {
 Obj newObj;
 Num i, len;

#if 0
 FILE *stream=NULL;
	/* Debug dump heap headers.  Keep stream open for
	   final header dump below. */
	if (garbageCollectionCount==30) exit(-1);
	if (GarbageCollectionMode==GC_MODE_OLD && 24<garbageCollectionCount) {
		stream = fopen("headers.out", "w");
		assert(stream != NULL);
		fprintf(stream, "::memGarbageCollectInternal "INT"  desc "OBJ0"  byteSize "HEX"::::::::::::::::::::::::",garbageCollectionCount, desc, byteSize);
		memDebugDumpAll(stream);
		vmDebugDumpCode(r1c, stream);
		memDebugDumpObject(r1f, stream);
	}
#endif
	//memRbufAdd(GarbageCollectionMode+10); // BF: TODO: Temporary debugging.

	/* Verify no recursive call. */
	assert(memGCFlag==0);
	memGCFlag=1;

#if VALIDATE_HEAP
	memValidateHeapStructures();
#endif

	DB("::memGarbageCollectInternal()");
//	fprintf (stderr, "-->memGarbageCollectInternal() %c\n", GarbageCollectionMode==GC_MODE_YOUNG?'y':'o');

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
	memInitializeHeap (&heapNew, heapOld.blockCount + heap.blockCount + HEAP_BLOCK_COUNT + (byteSize+BLOCK_BYTE_SIZE)/BLOCK_BYTE_SIZE);

	/* Reset the global variable that specifies how many were needed. */
	byteSize=0;

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
		newObj = heapOld.start + DescSize;
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
	newObj = heapNew.start + DescSize;
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
		fprintf (stderr, "\n\aWARNING: memGarbageCollectInternal() finalizer count difference");
	}

	/* Free up heap and old heap if doing an old heap collection.  The live
	   objects have all been copied over to the new heap now. */
	DB("   FREEING AND SHRINKING");
	memFreeHeap (&heap);
	memResetHeap(&heap);
	if (GarbageCollectionMode == GC_MODE_OLD) {
		memFreeHeap (&heapOld);
		memResetHeap(&heapOld);
		/* Shrink newHeap (soon to be oldHeap) bounds to fit actual usage. */
		memShrinkHeap(&heapNew, (Num)(heapNew.last-heapNew.next)/BLOCK_BYTE_SIZE);
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
		if (((Num)(heap.next-heap.start)/BLOCK_BYTE_SIZE)*BLOCK_BYTE_SIZE
		    > HEAP_BLOCK_COUNT/2) {
			//printf ("\n\aWARNING: memGarbageCollectInternal() young heap usage beyond half.\n");
			GarbageCollectionMode = GC_MODE_OLD;
		} else {
			/* Shrink oldHeap bounds to fit actual usage. */
			memShrinkHeap(&heap, heap.blockCount-HEAP_BLOCK_COUNT);
		}
	}

	if (garbageCollectionCount%50 == 0)
		GarbageCollectionMode = GC_MODE_OLD;

	/* Reset empty young heap...the bigger this is the fewwer GC's occuring.
	   and since it's a generational collector the GC shouldn't copy very
	   many objects anyways (most objects will be short lived). */
	memResetHeap(&heapNew);

	if (memPostGarbageCollect) memPostGarbageCollect();

#if 0
	/* Final header dump. */
	if (stream!=NULL) {
		memDebugDumpAll(stream);
		vmDebugDumpCode(r1c, stream);
		memDebugDumpObject(r1f, stream);
		fprintf(stream, "\n--memGarbageCollectInternal----------------------------------------------------------------\ncalling memValidateHeapStructures()...\n");
		memValidateHeapStructures();
		fprintf(stream, "success.\n");
		fclose(stream);
	}
#endif

	if (memGCFlag != 1) fprintf (stderr, "\n\aWARNING: objGCPost() memGCFlag==%d", memGCFlag);
	memGCFlag=0;

#if VALIDATE_HEAP
	memValidateHeapStructures();
#endif

	//printf ("[count %d  heapOld %08x  heap %08x(%08x)  heapNew %08x  count %x]\n", garbageCollectionCount, heapOld.start, heap.start, heap.last-heap.start, heapNew.start, heapNew.blockCount);

	DB("--memGarbageCollectInternal");
}


/* External call not triggered on lack of space so no descriptor nor byteSize passed.  */
void memGarbageCollect () {
	memGarbageCollectInternal(0, 0);
}


void memDebugDumpHeapHeaders (FILE *stream) {

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
	fprintf (stream, "Blocks  %10lx   %10lx   %10lx   %10lx\n",
	        heapStatic.blockCount,  heapOld.blockCount,
	        heap.blockCount,        heapNew.blockCount);
	fprintf (stream, "Bytes   %10x   %10x   %10x   %10x\n",
	        heapStatic.last-heapStatic.start,  heapOld.last-heapOld.start,
	        heap.last-heap.start,              heapNew.last-heapNew.start);
	fprintf (stream, "Finalizer %8lx   %10lx   %10lx   %10lx\n",
	        heapStatic.finalizerCount,  heapOld.finalizerCount,
	        heap.finalizerCount,        heapNew.finalizerCount);

	fprintf (stream, "r00 "OBJ"  r08 "OBJ"  r10 "OBJ"  r18 "OBJ"     blocked  expr\n", r0, r8, r10, r18);
	fprintf (stream, "r01 "OBJ"  r09 "OBJ"  r11 "OBJ"  r19 "OBJ"     threads  symb\n", r1, r9, r11, r19);
	fprintf (stream, "r02 "OBJ"  r0a "OBJ"  r12 "OBJ"  r1a "OBJ"     sleeping asmstk\n", r2, ra, r12, r1a);
	fprintf (stream, "r03 "OBJ"  r0b "OBJ"  r13 "OBJ"  r1b "OBJ"     running  ip\n", r3, rb, r13, r1b);
	fprintf (stream, "r04 "OBJ"  r0c "OBJ"  r14 "OBJ"  r1c "OBJ"     ready    code\n", r4, rc, r14, r1c);
	fprintf (stream, "r05 "OBJ"  r0d "OBJ"  r15 "OBJ"  r1d "OBJ"     retenv   rip\n", r5, rd, r15, r1d);
	fprintf (stream, "r06 "OBJ"  r0e "OBJ"  r16 "OBJ"  r1e "OBJ"     env      rcode\n", r6, re, r16, r1e);
	fprintf (stream, "r07 "OBJ"  r0f "OBJ"  r17 "OBJ"  r1f "OBJ" sem tge      stack\n", r7, rf, r17, r1f);
}
#define semaphores rf  /* WSCM: semaphore counters */
#define blocked    r10 /* WSCM: I/O and Semaphore blocked threads */
#define threads    r11 /* WSCM: Thread vector */
#define sleeping   r12 /* WSCM: Sleeping thread */
#define running    r13 /* WSCM: Current thread */
#define ready      r14 /* WSCM: Thread list */

#define retenv     r15 /* VM: Caller's environment */
#define env        r16 /* WSCM: Current active environment */
#define tge        r17 /* WSCM: Global environment */
#define expr       r18 /* WSCM: Expression being compiled */
#define symbols    r19 /* OBJ: Symbol table used by scanner and OS */
#define asmstack   r1a /* VM: Opcode stack where machine code is emitted */
#define ip         r1b /* VM: Current running program instruction pointer */
#define code       r1c /* VM: Currently running code object */
#define retip      r1d /* VM: Caller's ip */
#define retcode    r1e /* VM: Caller's code block */
#define stack      r1f /* VM: Global stack used by VM */



void memDebugDumpObject (Obj o, FILE *stream) {
 Int i, fdState;
 char *s;
 Obj obj;

	fcntl (0, F_SETFL, (fdState=fcntl(0, F_GETFL, 0))&~O_NONBLOCK);

	if (!memIsObjectValid(o) && !memIsObjectInHeap(&heapNew, o)) {
		fprintf(stream, NL "???" OBJ, o);
	}

	if (stream == NULL) stream=stderr;

	/* Dump the object's address and descriptor information.
	   7fe8f5f44610 81 PAIR      2 #(7fe8f5f445f0 7fe8f5f44600) */
	fprintf (stream, "\n"OBJ" "HEX02" %-7s"HEX4,
		o, memObjectType(o), memTypeString(memObjectType(o)), memObjectLength(o));

	/* Some objects are just an instance of the descriptor
	   such as #f and #() they have no content. */
 	if (!memObjectLength(o)) {
		fprintf(stderr, " EMPTY");
	} else if (memIsObjectVector(o)) { // Vector
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
			fprintf (stream, " ("OBJ")()", *(Obj*)o);
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
	DB("::%s", __func__);

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

	DB("  --%s", __func__);
}



void memValidateObject (Obj o) {
 Obj oo, op;
 Int valid=1;
 Num i;
	DB("   ::%s", __func__);
	
	if ((memIsObjectInHeap(&heapOld, o)
		  || memIsObjectInHeap(&heap, o)
		  || memIsObjectInHeap(&heapStatic, o))
		 && memObjectLength(o)
		 && memIsObjectVector(o)) {
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
		else if (memIsObjectVector(o))
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
		//sysDebugger();
	}
	DB("      --%s", __func__);
}



void memValidateHeapStructures (void) {
 Obj o;
	DB("::%s", __func__);

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
	DB("   --%s", __func__);
}



char* memTypeString (Type t) {
 char *s;
	switch(t) {
		case TBASEARRAY:   s="BASEARRAY"; break;
		case TFALSE:       s="FALSE"; break;
		case TTRUE:        s="TRUE"; break;
		case TNULL:        s="NULL"; break;
		case TNULLVEC:     s="NULLVEC"; break;
		case TNULLSTR:     s="NULLSTR"; break;
		case TEOF:         s="EOF"; break;
		case TCHAR:        s="CHAR"; break;
		case TSTRING:      s="STRING"; break;
		case TSYMBOL:      s="SYMBOL"; break;
		case TINTEGER:     s="INTEGER"; break;
		case TREAL:        s="REAL"; break;

		case TBASEVECTOR:  s="BASEVEC"; break;
		case TPAIR:        s="PAIR"; break;
		case TVECTOR:      s="VECTOR"; break;
		case TCLOSURE:     s="CLOSURE"; break;
		case TCONTINUATION:s="CONT"; break;
		case TCODE:        s="CODE"; break;
		case TPORT:        s="PORT"; break;
		case TSOCKET:      s="SOCKET"; break;
		case TSYSCALL:     s="SYSCALL"; break;

		case TFINALIZER:   s="FINALIZER"; break;
		case TPOINTER:     s="POINTER"; break;
		case TSTACK:       s="STACK"; break;
		case TSHADOW:      s="SHADOW"; break;
		default:           s="!!InvaliD!!";
	}
	return s;
}


/* Simple table insertion and lookup of a string
   to a pointer value aka object. */
#define TABLEMAX 4096
typedef struct {Obj obj;  char* str;} osPair;
osPair osTable[TABLEMAX];
Num osCount=0;

char* memObjString (Obj obj) {
 Num i;
	for (i=0; i<osCount; ++i)
		if (osTable[i].obj==obj) return osTable[i].str;
	return NULL;
}

void memObjStringRegister (Obj obj, char *str) {
	ASSERT(NULL == memObjString(obj));
	osTable[osCount].obj = obj;
	osTable[osCount++].str = str;
}


/* Called by obj.c and vm.c */
void memInitialize (Func preGC, Func postGC) {
 int static shouldInitialize=1;
	if (shouldInitialize) {
		shouldInitialize=0;
		DB("::memInitialize()");
		if (preGC) memPreGarbageCollect = preGC;
		if (postGC) memPostGarbageCollect = postGC;
		memInitializeHeap (&heapStatic, HEAP_STATIC_BLOCK_COUNT);
		/* Create bogus old heap so things work properly. */
		memInitializeHeap (&heap,    HEAP_BLOCK_COUNT);
		memInitializeHeap (&heapOld, 1);
		GarbageCollectionMode = GC_MODE_OLD;
		DB("--memInitialize");
	}
}


void memError (void) {
	fprintf (stderr, "\nHalted on memory module error.\n");
	//memDebugdumpAll();
	//r0=code;
	//	vmDebugDumpCode();
	//if (getchar()=='y') memDebugDumpAll(NULL);
	*(int*)0 = 1; // Force a crash.
	//exit(-1);
}

#undef DB_MODULE
