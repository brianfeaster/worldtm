#define DEBUG 0
#define DB_DESC "ASM "
#include "debug.h"
#include <stdarg.h>
#include <fcntl.h>
#include <assert.h>
#include "asm.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"

/*
TABLE OF CONTENTS
 I_Code
 Igraph_and_iblocks
 Labels
 ASM
 Debugging
 Init

TERMS
  I-Graph   Intermediate graph composed of I-blocks
  I-Block   I-graph node composed of a list of incoming iblocks, outgoing iblocks and icode statements
  I-Code    I-block statement composed of multiple code fields
*/

void asmDumpIBlock (Obj ib);
void asmDumpIBlocks (void);
void asmDumpIBlockParentAndChildren (Obj ib);


/* Rootset objects
*/
Obj ropcodes, riblock, riblocks, ricodes, rlabels, rexpr, rcodenew;


/* Local static scheme symbol used to delimit assembly opcodes */
Obj sasmend, sasmna;


/* Object types used by compiler
*/
#define TICODE  0x89l
#define TIBLOCK 0x8Al
Num asmIsObjectTypeICode  (Obj ic) { return memIsObjectType(ic, TICODE); }
Num asmIsObjectTypeIBlock (Obj ib) { return memIsObjectType(ib, TIBLOCK); }



/*******************************************************************************
 I_Code

 Vector's of length 6 containing: an opcode, 0-3 register, possible immediate and
 possible branch field.

 New icode objects are pushed to a vector with the count in ICodeCount.  They are
 eventually popped off the vector and added to new iblocks.
*******************************************************************************/
#define ICODE_VECTOR_SIZE 0x1000 /* I don't expect an iblock to have this many instructions */
Obj ricodes;
Num ICodeCount = 0;

void asmICodePush (Obj ic) {
	assert(asmIsObjectTypeICode(ic));
	assert(ICodeCount < ICODE_VECTOR_SIZE);
	memVectorSet(ricodes, ICodeCount++, ic);
}

Obj asmICodePop (void) {
 Obj ic;
	assert(0 < ICodeCount);
	ic = memVectorObject(ricodes, --ICodeCount);
	return ic;
}

/* Will always return 6
*/
Num asmICodeFieldLength (Obj ic) {
	assert(asmIsObjectTypeICode(ic));
	return memObjectLength(ic);
}

Obj asmICodeField (Obj ic, Num i) {
	assert(i < asmICodeFieldLength(ic));
	return memVectorObject(ic, i);
}

void asmNewICode (Obj op, Obj r, Obj s, Obj t, Obj i, Obj b) {
	vmPush(i); /* Immediate value might be a scheme object and moved during a GC within memNewVector() */
	r0 = memNewVector(TICODE, 6);
	i = vmPop();
	memVectorSet(r0, 0, op);/* Opcode */
	memVectorSet(r0, 1, r); /* Register 0 */
	memVectorSet(r0, 2, s); /* Register 0 */
	memVectorSet(r0, 3, t); /* Register 0 */
	memVectorSet(r0, 4, i); /* Immediate value */
	memVectorSet(r0, 5, b); /* Branch offset */
	asmICodePush(r0);
}
	
void asmICodePushNewMV (Obj r, Obj s)         { asmNewICode(MV,   r,  s, NA, NA, NA); }
void asmICodePushNewMVI (Obj r, Obj i)        { asmNewICode(MVI,  r, NA, NA,  i, NA); }
void asmICodePushNewLDI (Obj r, Obj s, Obj i) { asmNewICode(LDI,  r,  s, NA,  i, NA); }
void asmICodePushNewLD (Obj r, Obj s, Obj t)  { asmNewICode(LD,   r,  s,  t, NA, NA); }
void asmICodePushNewSTI (Obj r, Obj s, Obj i) { asmNewICode(STI,  r,  s, NA,  i, NA); }
void asmICodePushNewST (Obj r, Obj s, Obj t)  { asmNewICode(ST,   r,  s,  t, NA, NA); }
void asmICodePushNewPUSH (Obj r)              { asmNewICode(PUSH, r, NA, NA, NA, NA); }
void asmICodePushNewPOP (Obj r)               { asmNewICode(POP,  r, NA, NA, NA, NA); }
void asmICodePushNewADDI (Obj r, Obj i)       { asmNewICode(ADDI, r, NA, NA,  i, NA); }
void asmICodePushNewBLTI (Obj r, Obj i, Obj l){ asmNewICode(BLTI, r, NA, NA,  i,  l); }
void asmICodePushNewBEQI (Obj r, Obj i, Obj l){ asmNewICode(BEQI, r, NA, NA,  i,  l); }
void asmICodePushNewBNEI (Obj r, Obj i, Obj l){ asmNewICode(BNEI, r, NA, NA,  i,  l); }
void asmICodePushNewBRTI (Obj r, Obj t, Obj l){ asmNewICode(BRTI, r, NA, NA,  t,  l); }
void asmICodePushNewBNTI (Obj r, Obj t, Obj l){ asmNewICode(BNTI, r, NA, NA,  t,  l); }
void asmICodePushNewBRA (Obj l)               { asmNewICode(BRA, NA, NA, NA, NA,  l); }
void asmICodePushNewJMP (Obj r)               { asmNewICode(JMP,  r, NA, NA, NA, NA); }
void asmICodePushNewJAL (Obj r)               { asmNewICode(JAL,  r, NA, NA, NA, NA); }
void asmICodePushNewRET ()                    { asmNewICode(RET, NA, NA, NA, NA, NA); }
void asmICodePushNewSYS (Obj r)               { asmNewICode(SYS,  r, NA, NA, NA, NA); }
void asmICodePushNewSYSI (Obj i)              { asmNewICode(SYSI,NA, NA, NA,  i, NA); }
void asmICodePushNewNOP (void)                { asmNewICode(NOP, NA, NA, NA, NA, NA); }
void asmICodePushNewQUIT (void)               { asmNewICode(QUIT,NA, NA, NA, NA, NA); }

Num asmICodeOpcodeSize (Obj icode) {
	assert(asmIsObjectTypeICode(icode));
	if (NA == asmICodeField(icode, 0)) return 0; /* Ignore NAs since they're not emitted */
	return 1 + (Num)(NA != asmICodeField(icode, 4)) + (Num)(NA != asmICodeField(icode, 5));
}



/*******************************************************************************
 Igraph_and_iblocks

 An intermediate graph is a graph of intermediate blocks.  Each iblock can have
 any number of incoming iblocks and up to two outgoing: the default iblock and
 the conditional iblock.  If the iblock represents a conditional statement, its
 last icode will be a conditional branch code and it's target block will be the
 "conditional" block.  All iblocks will generally have a "default" block which
 represents the continued flow of instructions.  Iblocks contain icode
 instructions optimized and eventually assembled into a VM code block object.

 The default and conditional tag values are considered the destination block.
*******************************************************************************/
#define IBLOCK_VECTOR_SIZE 0x1000
Num IBlockCount=0;

#define IBLOCK_INDEX_ID          0
#define IBLOCK_INDEX_TAG         1
#define IBLOCK_INDEX_DEFAULT     2
#define IBLOCK_INDEX_CONDITIONAL 3
#define IBLOCK_INDEX_INCOMING    4
#define IBLOCK_INDEX_ICODE       5

Num asmIBlockID (Obj ib)             { return (Num)memVectorObject(ib, IBLOCK_INDEX_ID); }
Obj asmIBlockTag (Obj ib)            { return memVectorObject(ib, IBLOCK_INDEX_TAG); }
Obj asmIBlockDefaultTag (Obj ib)     { return memVectorObject(ib, IBLOCK_INDEX_DEFAULT); }
Obj asmIBlockConditionalTag (Obj ib) { return memVectorObject(ib, IBLOCK_INDEX_CONDITIONAL); }
Obj asmIBlockIncomingList (Obj ib)   { return memVectorObject(ib, IBLOCK_INDEX_INCOMING); }

Num asmIBlockICodeLength (Obj ib) {
	assert(asmIsObjectTypeIBlock(ib));
	return memObjectLength(ib) - IBLOCK_INDEX_ICODE;
}

/* Get ith icode object in iblock 
*/
Obj asmIBlockICode (Obj ib, Num i) {
	assert(asmIsObjectTypeIBlock(ib));
	return memVectorObject(ib, IBLOCK_INDEX_ICODE + i);
}

/* Lookup iblock by ID
*/
Obj asmIBlock (Num id) {
 Obj ib;
	ib = memVectorObject(riblocks, id);
	assert(asmIsObjectTypeIBlock(ib));
	assert(asmIBlockID(ib) == id);
	return ib;
}


Num asmIBlockIsValid (Obj ib) {
	return asmIBlockID(ib) < IBlockCount;
}


/* Given an iblock ID, return the next valid live iblock based on
   incrementing ID numbers.
*/
Obj asmIBlockNextValid (Num id) {
 Obj ib;
	while (++id < IBlockCount) {
		ib = asmIBlock(id);
		if (otrue == asmIBlockTag(ib)) return ib;
	}
	return ofalse;
}


/* Set iblock's various tag values
*/

void asmIBlockTagSet (Obj ib, Obj tag) {
	memVectorSet(ib, IBLOCK_INDEX_TAG, tag);
}

void asmIBlockDefaultTagSet (Obj ib, Obj tag) {
	memVectorSet(ib, IBLOCK_INDEX_DEFAULT, tag);
}

void asmIBlockConditionalTagSet (Obj ib, Obj tag) {
	memVectorSet(ib, IBLOCK_INDEX_CONDITIONAL, tag);
}

/*   r1 = temp
     r2 = temp
     r3 = temp
*/
void asmIBlockIncomingListAdd (Obj ib, Obj o) {
	r3 = ib;
	r1 = o;
	r2 = memVectorObject(r3, IBLOCK_INDEX_INCOMING);
	objCons12();
	memVectorSet(r3, IBLOCK_INDEX_INCOMING, r0);
}

void asmIBlockIncomingListDel (Obj ib, Obj o) {
 Obj l, n;

	/* Consider list */
	l = asmIBlockIncomingList(ib);

	/* First element is obj? */
	if (objIsPair(l) && o == car(l)) {
		memVectorSet(ib, IBLOCK_INDEX_INCOMING, cdr(l));
		return;
	}

	n = cdr(l);
	while (objIsPair(n)) {
		if (objIsPair(n) && o == car(n)) {
			memVectorSet(l, 1, cdr(n));
			return;
		}
		l = n;
		n = cdr(l);
	}
}

void asmIBlockSetICode (Num offset, Obj op) {
	assert(offset < memObjectLength(riblock));
	memVectorSet(riblock, IBLOCK_INDEX_ICODE+offset, op);
}


/* Create new igraph block. Increments IBlockCount which is
   also used as the block's ID.
    iblock => iblock object
*/
void asmGenerateNewIBlock (Num icodeSize) {
	riblock = memNewVector(TIBLOCK, icodeSize + IBLOCK_INDEX_ICODE);

	/* Unique ID number set automatically */
	memVectorSet(riblock, IBLOCK_INDEX_ID, (Obj)IBlockCount);

	/* Tag defaults to #f */
	memVectorSet(riblock, IBLOCK_INDEX_TAG, ofalse);

	/* Incoming iblock list defaults to empty list */
	memVectorSet(riblock, IBLOCK_INDEX_INCOMING, onull);

	/* Outgoing default and conditional branch tags */
	memVectorSet(riblock, IBLOCK_INDEX_DEFAULT, ofalse);
	memVectorSet(riblock, IBLOCK_INDEX_CONDITIONAL, ofalse);

	/* Append to igraph vector */
	memVectorSet(riblocks, IBlockCount, riblock);

	++IBlockCount;
}



/*******************************************************************************
 Labels

 DB of iblock numbers.  Used to associate a label, just a number, with an iblock
 index, a number.
*******************************************************************************/
#define LABELS_DB_SIZE 0x1000
const Num LABELS_INVALID_ID = (Num)-1;
Num LabelsCount;

/* Generate a label for the label opcode and branch opcode address fields.
   Initialize it to INVALID.
*/
Obj asmNewLabel() {
	assert(LabelsCount < LABELS_DB_SIZE);
	memVectorSet(rlabels, LabelsCount, (Obj)LABELS_INVALID_ID);
	return (Obj)LabelsCount++;
}

Num asmLabels (Num i) {
 Num labelsBlockID;
	assert(i < LabelsCount); /* Invalid label */
	labelsBlockID = (Num)memVectorObject(rlabels, i);
	assert(LABELS_INVALID_ID != labelsBlockID); /* Label has no registered block */
	return labelsBlockID;
}

void asmLabelsSet (Num i, Num blockNumber) {
 Num labelsBlockID;
	assert(i < LABELS_DB_SIZE); /* Invalid label */
	labelsBlockID = (Num)memVectorObject(rlabels, i);
	assert(LABELS_INVALID_ID == labelsBlockID); /* Label has already been registered */
	memVectorSet(rlabels, i, (Obj)blockNumber);
}



/*******************************************************************************
 ASM

 Accept opcode and opcode fields and compile into an igraph.
*******************************************************************************/
#define OPCODES_VEC_SIZE 0x1000
Num OpcodesCount=0;
Num OpcodesNext=0;

void asmOpcodesPush (Obj o) {
	assert(OpcodesCount < OPCODES_VEC_SIZE);
	memVectorSet(ropcodes, OpcodesCount++, o);
}

Obj asmOpcodesNext (void) {
 Obj o;
	DBBEG();
	DB("OpcodesNext="HEX, OpcodesNext);
	if (OpcodesNext >= OpcodesCount) {
		DB("\nWe have a problems!!!\n");
		*(int*)0=0;
	}
	o = memVectorObject(ropcodes, OpcodesNext++);
	if (OpcodesNext == OpcodesCount) {
		OpcodesCount = OpcodesNext = 0;
	}
	DBEND();
	return o;
}

Num icodeOffset=0;
Num iblockOffset=0;

/* Get number of iblocks in current context/frame
*/
Num asmIBlockFrameCount (void) {
	return IBlockCount - iblockOffset;
}

/* Get iblocks indexed in current assembly context/frame
*/
Obj asmIBlockFrame (Num i) {
	assert(i < asmIBlockFrameCount());
	return asmIBlock(iblockOffset + i);
}


/* Begin a new ASM context saving the current context state on the stack
*/
void asmStart (void) {
	vmPush((Obj)icodeOffset);
	vmPush((Obj)iblockOffset);
	vmPush((Obj)riblock);

	riblock = 0;
	icodeOffset = ICodeCount;
	iblockOffset = IBlockCount;
}

/* Reset ASM context from stack along with some sanity assertions
*/
void asmEnd (void) {
	IBlockCount = iblockOffset;

	riblock = vmPop();
	assert(riblock == 0 || asmIsObjectTypeIBlock(riblock));

	iblockOffset = (Num)vmPop();
	assert(iblockOffset < IBLOCK_VECTOR_SIZE);

	icodeOffset = (Num)vmPop();
	assert(icodeOffset < ICODE_VECTOR_SIZE);
}


/* Prepare assembler for multiple calls to asmAsm()
*/
void asmInit (void) {
	DBBEG();

	riblock = 0; /* Clear current working iblock variable */
	IBlockCount = 0; /* Reset iblock vector count */

	assert(0 == ICodeCount); /* Check we're not initializing the assembler in the middle of assembly */
	//ICodeCount = 0;

	LabelsCount = 0; /* Clear lables vector count */

	asmStart();

	DBEND();
}

/* Called if an exception occured during compilation and asmAssemble() won't be called
*/
void asmReset (void) {
	while (icodeOffset < ICodeCount) asmICodePop(); /* This decrements ICodeCount */
	asmEnd();
}

/* Create a new iblock containing the most recent set of icodes created.  Skip if no icodes left.
*/
void asmGenerateIBlockWithPushedIcodes () {
 Obj ic;
	/* Create new empty iblock in riblock and 'pop' icodes from stack into it */
	asmGenerateNewIBlock(ICodeCount - icodeOffset);

	while (icodeOffset < ICodeCount) {
		ic = asmICodePop(); /* This decrement ICodeCount */
		asmIBlockSetICode(ICodeCount - icodeOffset, ic);
	}
}

/* Generates icode objects and continually adds to a set which are
   eventually added to the next new iblock.
*/
void asmAsmInternal (Obj f, ...) {
 va_list ap;
 Obj obj, r, rr, rrr, i, o, l; /* r,rr=reg  i=index  o=obj  l=label */
	DBBEG("  ICodeCount "NUM"   IBlockCount "NUM, ICodeCount, IBlockCount);

	/* Temporarily push all opcodes to a vector in case one of the opcodes
	   is an immediate field object and gets garbage collected. */
	assert(0 == OpcodesCount);
	va_start(ap, f);
	for (obj = f; (obj != END); obj = va_arg(ap, Obj)) {
		DB ("["HEX"|"HEX"]", OpcodesCount, obj);
		asmOpcodesPush(obj);
	}
	va_end(ap); /* stdarg */

	DB("OpcodesCount="HEX, OpcodesCount);

	while (OpcodesCount) {
		obj = asmOpcodesNext();
		if (MV == obj) {
			r  = asmOpcodesNext();
			rr = asmOpcodesNext();
			DB("mv ["HEX" "HEX" "HEX"]", obj, r, rr);
			asmICodePushNewMV(r, rr);
		} else if (MVI == obj) {
			r = asmOpcodesNext();
			o = asmOpcodesNext();
			DB("mvi ["HEX" "HEX" "HEX"]", obj, r, o);
			asmICodePushNewMVI(r, o);
		} else if (LDI == obj) {
			r = asmOpcodesNext();
			rr = asmOpcodesNext();
			o = asmOpcodesNext();
			DB("ldi ["HEX" "HEX" "HEX"]", r, rr, o);
			asmICodePushNewLDI(r, rr, o);
		} else if (LD == obj) {
			r = asmOpcodesNext();
			rr = asmOpcodesNext();
			rrr = asmOpcodesNext();
			DB("ld ["HEX" "HEX" "HEX"]", r, rr, rrr);
			asmICodePushNewLD(r, rr, rrr);
		} else if (STI == obj) {
			r = asmOpcodesNext();
			rr = asmOpcodesNext();
			o = asmOpcodesNext();
			DB("sti ["HEX" "HEX" "HEX"]", r, rr, o);
			asmICodePushNewSTI(r, rr, o);
		} else if (ST == obj) {
			r = asmOpcodesNext();
			rr = asmOpcodesNext();
			rrr = asmOpcodesNext();
			DB("st ["HEX" "HEX" "HEX"]", r, rr, rrr);
			asmICodePushNewST(r, rr, rrr);
		} else if (PUSH == obj) {
			o = asmOpcodesNext();
			DB("push["HEX"]", o);
			asmICodePushNewPUSH(o);
		} else if (POP == obj) {
			o = asmOpcodesNext();
			DB("pop ["HEX"]", o);
			asmICodePushNewPOP(o);
		} else if (ADDI == obj) {
			r = asmOpcodesNext();
			o = asmOpcodesNext();
			DB("addi["HEX" "HEX"]", r, o);
			asmICodePushNewADDI(r, o);
		} else if (BLTI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("blti["HEX" "HEX" "HEX"]", r, i, l);
			asmICodePushNewBLTI(r, i, l);
			asmGenerateIBlockWithPushedIcodes();
			asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */
			asmIBlockConditionalTagSet(riblock, l); /* signal this block conditional is a label */
		} else if (BEQI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("beqi["HEX" "HEX" "HEX"]", r, i, l);
			asmICodePushNewBEQI(r, i, l);
			asmGenerateIBlockWithPushedIcodes();
			asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */
			asmIBlockConditionalTagSet(riblock, l); /* signal this block conditional is a label */
		} else if (BNEI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("bnei["HEX" "HEX" "HEX"]", r, i, l);
			asmICodePushNewBNEI(r, i, l);
			asmGenerateIBlockWithPushedIcodes();
			asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */
			asmIBlockConditionalTagSet(riblock, l); /* signal this block conditional is a label */
		} else if (BRTI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("brti["HEX" "HEX" "HEX"]", r, i, l);
			asmICodePushNewBRTI(r, i, l);
			asmGenerateIBlockWithPushedIcodes();
			asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */
			asmIBlockConditionalTagSet(riblock, l); /* signal this block conditional is a label */
		} else if (BNTI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("bnti["HEX" "HEX" "HEX"]", r, i, l);
			asmICodePushNewBNTI(r, i, l);
			asmGenerateIBlockWithPushedIcodes();
			asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */
			asmIBlockConditionalTagSet(riblock, l); /* signal this block conditional is a label */
		} else if (BRA == obj) {
			l = asmOpcodesNext();
			DB("bra ["HEX"]", l);
			//asmICodePushNewBRA(l);
			asmGenerateIBlockWithPushedIcodes();
			asmIBlockDefaultTagSet(riblock, l); /* Set next block as the default */
		} else if (JMP == obj) {
			r = asmOpcodesNext();
			DB("jmp ["HEX"]", r);
			asmICodePushNewJMP(r);
			asmGenerateIBlockWithPushedIcodes();
			// no default block after a jump
		} else if (JAL == obj) {
			r = asmOpcodesNext();
			DB("jal ["HEX"]", r);
			asmICodePushNewJAL(r);
			asmGenerateIBlockWithPushedIcodes();
			asmIBlockDefaultTagSet(riblock, otrue); /* Set next block as the default */
		} else if (RET == obj) {
			DB("ret []");
			asmICodePushNewRET();
			asmGenerateIBlockWithPushedIcodes();
			/* no default block after a ret */
		} else if (SYS == obj) {
			r = asmOpcodesNext();
			DB("sys ["HEX"]",r);
			asmICodePushNewSYS(r);
			asmGenerateIBlockWithPushedIcodes();
			asmIBlockDefaultTagSet(riblock, otrue); /* Set next block as the default */
		} else if (SYSI == obj) {
			o = asmOpcodesNext();
			DB("sysi["HEX"]", o);
			asmICodePushNewSYSI(o);
			asmGenerateIBlockWithPushedIcodes();
			asmIBlockDefaultTagSet(riblock, otrue); /* Set next block as the default */
		} else if (NOP == obj) {
			DB("nop[]");
			asmICodePushNewNOP();
		} else if (QUIT == obj) {
			DB("quit[]");
			asmICodePushNewQUIT();
			asmGenerateIBlockWithPushedIcodes();
			/* no default block after a QUIT */
		} else if (LABEL == obj) {
			l = asmOpcodesNext();
			DB("label["HEX"]", l);
			if (0 < ICodeCount - icodeOffset) {
				asmGenerateIBlockWithPushedIcodes();
				asmIBlockDefaultTagSet(riblock, otrue);  /* default block is next */
			}
			/* Set the next block's ID in the label/iblockID table */
			asmLabelsSet((Num)l, IBlockCount);
		} else {
			DB("["HEX"]", obj);
			assert(!"Unhandled asm opcode");
		}
	}
	assert(0 == OpcodesCount);
//	sysWrite(rlabels, stdout);
//	asmDumpIBlocks();
	DBEND("  ICodeCount "NUM"   IBlockCount "NUM, ICodeCount, IBlockCount);
}




/*******************************************************************************
 Assemble

 Assemble the iblocks in an igraph into a VM runable code block object
*******************************************************************************/
Num pccode = 0;  /* Pointer into code block */


void asmEmitOpcode (Obj op) {
	if (pccode >= memObjectLength(rcodenew)) {
		asmDumpIBlocks();
		//vmDebugDumpCode(rcodenew, stderr);
	}
	assert(pccode < memObjectLength(rcodenew) && "Code object can't fit more icodes.");
	memVectorSet(rcodenew, pccode++, op);
}

void asmEmitOpcode2 (Obj op1, Obj op2) {
	memVectorSet(rcodenew, pccode++, op1);
	memVectorSet(rcodenew, pccode++, op2);
}


void asmEmitIblockOpcodes (void) {
 Num i;
 Obj field0, field1, field2, field3, field4;
	DBBEG("      iblock="NUM, asmIBlockID(riblock));

	/* Re-tag the iblock with its initial location in the code block */
	assert(otrue == asmIBlockTag(riblock));
	asmIBlockTagSet (riblock, (Obj)pccode);

	for (i=0; i<asmIBlockICodeLength(riblock); ++i) {
		r0 = asmIBlockICode(riblock, i); /* Consider icode object in r0 */
		field0 = asmICodeField(r0, 0);
		DB("field0 = "HEX, field0);
		switch ((Num)field0) {
		case (Num)MV:
			field1 = asmICodeField(r0, 1);
			field2 = asmICodeField(r0, 2);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				               case (Num)R1 : asmEmitOpcode(vmMV01); break;
				               case (Num)R3 : asmEmitOpcode(vmMV03); break;
				               case (Num)R4 : asmEmitOpcode(vmMV04); break;
				               case (Num)RE : asmEmitOpcode(vmMV0E); break;
				               default : assert(!"Unsuported icode MV $0 ??"); } break;
				case (Num)R1 : switch ((Num)field2) {
				               case (Num)R0 : asmEmitOpcode(vmMV10); break;
				               case (Num)R3 : asmEmitOpcode(vmMV13); break;
				               default : assert(!"Unsuported icode MV $1 ??"); } break;
				case (Num)R2 : switch ((Num)field2) {
				               case (Num)R0 : asmEmitOpcode(vmMV20); break;
				               default : assert(!"Unsuported icode MV $2 ??"); } break;
				case (Num)R3 : switch ((Num)field2) {
				               case (Num)R0 : asmEmitOpcode(vmMV30); break;
				               default : assert(!"Unsuported icode MV $3 ??"); } break;
				case (Num)R5 : switch ((Num)field2) {
				               case (Num)R0 : asmEmitOpcode(vmMV50); break;
				               case (Num)R8 : asmEmitOpcode(vmMV58); break;
				               case (Num)RC : asmEmitOpcode(vmMV5C); break;
				               default : assert(!"Unsuported icode MV $5 ??"); } break;
				case (Num)RC: switch ((Num)field2) {
				               case (Num)R0 : asmEmitOpcode(vmMVC0); break;
				               case (Num)R5 : asmEmitOpcode(vmMVC5); break;
				               case (Num)R8 : asmEmitOpcode(vmMVC8); break;
				               default : assert(!"Unsuported icode MV $C ??"); } break;
				default : assert(!"Unsuported icode MV ?? reg"); }
			break;
		case (Num)MVI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			switch ((Num)field1) {
				case (Num)R0 : asmEmitOpcode(vmMVI0); break;
				case (Num)R1 : asmEmitOpcode(vmMVI1); break;
				case (Num)R2 : asmEmitOpcode(vmMVI2); break;
				case (Num)R3 : asmEmitOpcode(vmMVI3); break;
				case (Num)R4 : asmEmitOpcode(vmMVI4); break;
				case (Num)R5 : asmEmitOpcode(vmMVI5); break;
				case (Num)R6 : asmEmitOpcode(vmMVI6); break;
				case (Num)R7 : asmEmitOpcode(vmMVI7); break;
				default : assert(!"Unsuported field MVI ?reg? imm"); }
			asmEmitOpcode(field4);
			break;
		case (Num)LDI:
			field1 = asmICodeField(r0, 1);
			field2 = asmICodeField(r0, 2);
			field4 = asmICodeField(r0, 4);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				          case (Num)R0 : asmEmitOpcode(vmLDI00); break;
				          case (Num)R2 : asmEmitOpcode(vmLDI02); break;
				          case (Num)RC: asmEmitOpcode(vmLDI0C); break;
				          default : assert(!"Unsuported field LDI $0 ?reg? imm"); } break;
				case (Num)R1 : switch ((Num)field2) {
				          case (Num)R1 : asmEmitOpcode(vmLDI11); break;
				          case (Num)RC: asmEmitOpcode(vmLDI1C); break;
				          default : assert(!"Unsuported field LDI $1 ?reg? imm"); } break;
				case (Num)R2 : switch ((Num)field2) {
				          case (Num)R0 : asmEmitOpcode(vmLDI20); break;
				          case (Num)R2 : asmEmitOpcode(vmLDI22); break;
				          default : assert(!"Unsuported field LDI $2 ?reg? imm"); } break;
				case (Num)R5 : switch ((Num)field2) {
				          case (Num)R0 : asmEmitOpcode(vmLDI50); break;
				          default : assert(!"Unsuported field LDI $5 ?reg? imm"); } break;
				case (Num)RC: switch ((Num)field2) {
				          case (Num)R0 : asmEmitOpcode(vmLDIC0); break;
				          default : assert(!"Unsuported field LDI $C ?reg? imm"); } break;
				default : assert(!"Unsuported field LDI ?reg? reg imm"); }
			asmEmitOpcode(field4);
			break;
		case (Num)LD:
			field1 = asmICodeField(r0, 1);
			field2 = asmICodeField(r0, 2);
			field3 = asmICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				               case (Num)R1 : switch ((Num)field3) {
				                              case (Num)R2 : asmEmitOpcode(vmLD012); break;
				                              default : assert(!"Unsuported field LD $0 $1 ?reg?"); } break;
				               default : assert(!"Unsuported field LD $0 ?reg? reg"); } break;
				default : assert(!"Unsuported field LD ?reg? reg reg"); }
			break;
		case (Num)STI:
			field1 = asmICodeField(r0, 1);
			field2 = asmICodeField(r0, 2);
			field4 = asmICodeField(r0, 4);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				               case (Num)R1 : asmEmitOpcode(vmSTI01); break;
				               case (Num)R5: asmEmitOpcode(vmSTI05); break;
				               case (Num)RC: asmEmitOpcode(vmSTI0C); break;
				               default : assert(!"Unsuported field STI $0 ?reg? imm"); } break;
				case (Num)R2 : switch ((Num)field2) {
				               case (Num)R0 : asmEmitOpcode(vmSTI20); break;
				               case (Num)R1 : asmEmitOpcode(vmSTI21); break;
				               default : assert(!"Unsuported field STI $2 ?reg? imm"); } break;
				case (Num)R3 : switch ((Num)field2) {
				               case (Num)R0 : asmEmitOpcode(vmSTI30); break;
				               default : assert(!"Unsuported field STI $3 ?reg? imm"); } break;
				case (Num)R5 : switch ((Num)field2) {
				               case (Num)R0 : asmEmitOpcode(vmSTI50); break;
				               default : assert(!"Unsuported field STI $5 ?reg? imm"); } break;
				default : assert(!"Unsuported field STI ?reg? reg imm"); }
			asmEmitOpcode(field4);
			break;
		case (Num)ST:
			field1 = asmICodeField(r0, 1);
			field2 = asmICodeField(r0, 2);
			field3 = asmICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				               case (Num)R1 : switch ((Num)field3) {
				                              case (Num)R2 :  asmEmitOpcode(vmST012); break;
				                              default : assert(!"Unsuported field ST $0 $1 ?reg?"); } break;
				               default : assert(!"Unsuported field ST $0 ?reg? reg"); } break;
				default : assert(!"Unsuported field ST ?reg? reg reg"); }
			break;
		case (Num)PUSH:
			field1 = asmICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : asmEmitOpcode(vmPUSH0); break;
				case (Num)R1 : asmEmitOpcode(vmPUSH1); break;
				case (Num)R2 : asmEmitOpcode(vmPUSH2); break;
				case (Num)R3 : asmEmitOpcode(vmPUSH3); break;
				case (Num)R4 : asmEmitOpcode(vmPUSH4); break;
				case (Num)R5 : asmEmitOpcode(vmPUSH5); break;
				case (Num)R7 : asmEmitOpcode(vmPUSH7); break;
				case (Num)R9: asmEmitOpcode(vmPUSH9); break;
				case (Num)RA: asmEmitOpcode(vmPUSHA); break;
				case (Num)RB: asmEmitOpcode(vmPUSHB); break;
				case (Num)RC: asmEmitOpcode(vmPUSHC); break;
				default : assert(!"Unsuported field PUSH ?reg?"); }
			break;
		case (Num)POP:
			field1 = asmICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : asmEmitOpcode(vmPOP0); break;
				case (Num)R1 : asmEmitOpcode(vmPOP1); break;
				case (Num)R2 : asmEmitOpcode(vmPOP2); break;
				case (Num)R3 : asmEmitOpcode(vmPOP3); break;
				case (Num)R4 : asmEmitOpcode(vmPOP4); break;
				case (Num)R5 : asmEmitOpcode(vmPOP5); break;
				case (Num)R7 : asmEmitOpcode(vmPOP7); break;
				case (Num)R9: asmEmitOpcode(vmPOP9); break;
				case (Num)RA: asmEmitOpcode(vmPOPA); break;
				case (Num)RB: asmEmitOpcode(vmPOPB); break;
				case (Num)RC: asmEmitOpcode(vmPOPC); break;
				default : assert(!"Unsuported field POP ?reg?"); }
			break;
		case (Num)ADDI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			switch ((Num)field1) {
				case (Num)R0 : asmEmitOpcode(vmADDI0); break;
				case (Num)R1 : asmEmitOpcode(vmADDI1); break;
				case (Num)R2 : asmEmitOpcode(vmADDI2); break;
				default : assert(!"Unsuported field ADDI ?reg? imm"); }
			asmEmitOpcode(field4);
			break;
		case (Num)BLTI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			/* field5 ignored */
			switch ((Num)field1) {
				case (Num)R1 : asmEmitOpcode(vmBLTI1); break;
				default : assert(!"Unsuported field BLTI ?reg? imm offset"); }
			asmEmitOpcode(field4);
			asmEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BEQI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			/* field5 ignored */
			switch ((Num)field1) {
				case (Num)R0 : asmEmitOpcode(vmBEQI0); break;
				case (Num)R1 : asmEmitOpcode(vmBEQI1); break;
				case (Num)R7 : asmEmitOpcode(vmBEQI7); break;
				default : assert(!"Unsuported field BEQI ?reg? imm offset"); }
			asmEmitOpcode(field4);
			asmEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BNEI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			/* field5 ignored */
			switch ((Num)field1) {
				case (Num)R0 : asmEmitOpcode(vmBNEI0); break;
				case (Num)R1 : asmEmitOpcode(vmBNEI1); break;
				case (Num)R2 : asmEmitOpcode(vmBNEI2); break;
				case (Num)R5 : asmEmitOpcode(vmBNEI5); break;
				default : assert(!"Unsuported field BNEI ?reg? imm offset"); }
			asmEmitOpcode(field4);
			asmEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BRTI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			/* field5 ignored */
			switch ((Num)field1) {
				case (Num)R0 : asmEmitOpcode(vmBRTI0); break;
				default : assert(!"Unsuported field BRTI ?reg? imm offset"); }
			asmEmitOpcode(field4);
			asmEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BNTI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			/* field5 ignored */
			switch ((Num)field1) {
				case (Num)R0 : asmEmitOpcode(vmBNTI0); break;
				default : assert(!"Unsuported field BNTI ?reg? imm offset"); }
			asmEmitOpcode(field4);
			asmEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BRA :
			/* Emitted by parent logic */
			break;
		case (Num)JMP :
			field1 = asmICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : asmEmitOpcode(vmJMP0); break;
				case (Num)R2 : asmEmitOpcode(vmJMP2); break;
				default : assert(!"Unsuported field JMP ?reg?"); }
			break;
		case (Num)JAL :
			field1 = asmICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : asmEmitOpcode(vmJAL0); break;
				case (Num)R2 : asmEmitOpcode(vmJAL2); break;
				default : assert(!"Unsuported field JAL ?reg?"); }
			break;
		case (Num)RET :
			asmEmitOpcode(vmRET);
			break;
		case (Num)SYS :
			field1 = asmICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : asmEmitOpcode(vmSYS0); break;
				default : assert(!"Unsuported field SYS ?imm?"); }
			break;
		case (Num)SYSI:
			field4 = asmICodeField(r0, 4);
			asmEmitOpcode2(vmSYSI, field4);
			break;
		case (Num)QUIT:
			asmEmitOpcode(vmQUIT);
			break;
		case (Num)NOP:
			asmEmitOpcode(vmNOP);
			break;
		default:
			if (field0 == NA) break; /* The NA fake opcode is OK.  It's an opcode removed during optimization. */
			fprintf(stderr, "\nCan't assemble opcode ");
			objDisplay(r3, stderr);
			assert(!"Unsuported opcode");
		}
	}

	DBEND();
}

/* Emit a jump opcode if the iblock's default iblock will not be emitted after this
   one.  Also set the default/conditional branch field code-block offsets in the
   iblock structure.
*/
void asmPrepareIBlockBranches (void) {
 Obj defBlock, condBlock;
 Num codeBlockOffset, id;
 Obj nextib;
	DBBEG("  iblock="NUM, asmIBlockID(riblock));
	/* If no default block is set then verify no conditional block either as it's
	   probably the final "quit" iblock. */
	if (ofalse == asmIBlockDefaultTag(riblock)) {
		assert(ofalse == asmIBlockConditionalTag(riblock));
		goto ret;
	}

	/* If the iblock has a conditional iblock, cache the branch opcode's offset-field location and
	   set the field value to the target iblock temporarily */
	condBlock = asmIBlockConditionalTag(riblock);
	if (ofalse != condBlock) {
		assert(asmIsObjectTypeIBlock(condBlock));
		codeBlockOffset = pccode - 1;
		memVectorSet(rcodenew, codeBlockOffset, condBlock);
		asmIBlockConditionalTagSet(riblock, (Obj)codeBlockOffset);
	}

	/* A default iblock exists.  If the iblock is the last iblock in the igraph vector
	   or the next iblock to emit is not my default, emit a jump opcode. */
	id = asmIBlockID(riblock);
	nextib = asmIBlockNextValid(id); /* Consider next iblock to be emitted */
	defBlock = asmIBlockDefaultTag(riblock);
	if ((id == IBlockCount - 1) || (defBlock != nextib)) {
		assert(asmIsObjectTypeIBlock(defBlock));
		asmEmitOpcode2(vmBRA, ofalse); /* Emit jump opcode */
		codeBlockOffset = pccode - 1;
		memVectorSet(rcodenew, codeBlockOffset, defBlock);
		asmIBlockDefaultTagSet(riblock, (Obj)codeBlockOffset);
	}
ret:
	DBEND();
}

/* For every live (tagged #t) iblock in the igraph, icode is emitted
   to the code object.  The iblock is tagged with its address in the code
   block.  The branch field for branch instruction are also stored so when
   the offset can be determined, the branch opcode's field can be set quickly.

   riblock/r16  <= current iblock
   rcodenew/r10    <= code emitted to
   pccode       <= C var code object index
*/
void asmPlaceAllIBlocks (void) {
 Num i;
	DBBEG();
	for (i=0; i < asmIBlockFrameCount(); ++i) { // TODO use this asmIBlockNextValid()?
		riblock = asmIBlockFrame(i); /* Consider iblock from vector of all iblocks in the current frame */

		/* This means the iblock is not connected to the igraph as it wasn't
		   recursively found when counting the igraph fields */
		if (ofalse != asmIBlockTag(riblock)) {

			/* Translate and emit the icodes as virtual machine opcodes */
			asmEmitIblockOpcodes();

			/* Emit a jump, if the default block does not follow immediatley after.
		   	Set up the iblock to resolve the target default and conditional branch offsets by
		   	moving the iblock from the default/conditional tag to the opcode's offset field
		   	and resetting the tag to the opcode's offset field offset in the code block. */
			asmPrepareIBlockBranches();
		}
	}
	DBEND();
}


/* Resolve current iblock's jump-opcode offset field
*/
void asmResolveDefault (void) {
 Obj defBlock;
 Obj defBlockAddr;
 Num opcodeFieldAddr;
	DBBEG("      iblock="NUM, asmIBlockID(riblock));
	/* Only resolve if the default opcode index is an immediate integer.
	   False means no jump was required as its default iblock was
	   emitted after this block. */
	opcodeFieldAddr = (Num)asmIBlockDefaultTag(riblock);

	if (!memIsObjectValid((Obj)opcodeFieldAddr)) {
		/* Consider default block and it's address in the code block*/
		defBlock = memVectorObject(rcodenew, opcodeFieldAddr);
		assert(ofalse != defBlock);
		defBlockAddr = asmIBlockTag(defBlock);
		assert(otrue != defBlockAddr); /* If it wasn't placed, it wouldn't be tagged #t */
 		/* Set the jump-opcode's offset */
		memVectorSet(rcodenew, opcodeFieldAddr, (Obj)(((Int)defBlockAddr-(Int)opcodeFieldAddr-1)*8));
		asmIBlockDefaultTagSet(riblock, defBlock); /* Set default tag back to target iblock */
	}
	DBEND();
}

/* Resolve current iblock's branch-opcode offset field
*/
void asmResolveConditional (void) {
 Obj condBlock;
 Obj condBlockAddr;
 Num opcodeFieldAddr;
	DBBEG("  iblock="NUM, asmIBlockID(riblock));
	/* Only resolve if the conditional opcode index is an immediate integer.
	   False means no final branch op at the end of the block */
	opcodeFieldAddr = (Num)asmIBlockConditionalTag(riblock);

	if (!memIsObjectValid((Obj)opcodeFieldAddr)) {
		/* Consider default block and it's address in the code block*/
		condBlock = memVectorObject(rcodenew, opcodeFieldAddr);
		assert(ofalse != condBlock);
		condBlockAddr = asmIBlockTag(condBlock);
		assert(otrue != condBlockAddr); /* If it wasn't placed, it would be tagged #t */
 		/* Set the jump-opcode's offset */
		memVectorSet(rcodenew, opcodeFieldAddr, (Obj)(((Int)condBlockAddr-(Int)opcodeFieldAddr-1)*8));
		asmIBlockConditionalTagSet(riblock, condBlock); /* Set conditional tag back to target iblock */
	}
	DBEND();
}

/* Assumes default and conditional tags are code block offsets to
   branch instructions which temporarily have the branch field set
   to the target iblock. */
void asmResolveBranchOpcodeAddresses (void) {
 Num i;
	DBBEG();
	/* Resolve branch offsets for the current iblock segment */
	for (i=0; i< asmIBlockFrameCount(); ++i) {
		riblock = asmIBlockFrame(i); /* Consider iblock from vector of all iblocks */
		/* This means the iblock is not connected to the igraph as it wasn't
		   recursively found when counting the igraph fields */
		if (ofalse != asmIBlockTag(riblock)) {
			/* Resolve my branch opcode offsets */
			asmResolveDefault();
			asmResolveConditional();
		}
	}
//	asmDumpIBlock(riblock);
//	vmDebugDumpCode(rcodenew, stderr);
	DBEND();
}


/* Set the default/conditional tags to an actual iblock.
   Also add this iblock to the child's incoming list.
*/
void asmIBlockLinkDefault (Num IDparent, Num IDchild) {
 Obj parentib, childib;

	parentib = asmIBlock(IDparent);
	childib = asmIBlock(IDchild);
	assert(asmIsObjectTypeIBlock(parentib));
	assert(asmIsObjectTypeIBlock(childib));

	asmIBlockDefaultTagSet(parentib, childib);
	asmIBlockIncomingListAdd(childib, parentib);
}

void asmIBlockLinkConditional (Num IDparent, Num IDchild) {
 Obj parentib, childib;

 	parentib = asmIBlock(IDparent);
 	childib = asmIBlock(IDchild);
	assert(asmIsObjectTypeIBlock(parentib));
	assert(asmIsObjectTypeIBlock(childib));

	asmIBlockConditionalTagSet(parentib, childib);
	asmIBlockIncomingListAdd(childib, parentib);
}

/* Initialize iblock default and conditional tags with their child iblocks (if it has them)
    r4 = temp 
*/
void asmInitIBlockBranchTagsToIBlocks (Obj ib) {
 Obj tag;
	DBBEG();
	DBE asmDumpIBlock(ib);

	r4 = ib; /* Protect object reference from garbage collector */
	tag = asmIBlockDefaultTag(r4);
	if (ofalse != tag) {
		if (otrue == tag) {
			/* Default block is via 'next logical' */
			asmIBlockLinkDefault(asmIBlockID(r4), 1 + asmIBlockID(r4)); 
		} else if (!asmIsObjectTypeIBlock(tag)) { /* Could already be connected if non ASM flow */
			/* Default block is via 'labeled block' */
			asmIBlockLinkDefault(asmIBlockID(r4), asmLabels((Num)tag));
		}
	}

	tag = asmIBlockConditionalTag(r4);
	if (ofalse != tag) {
		/* Conditional block is a labeled block */
		if (!asmIsObjectTypeIBlock(tag)) { /* Could already be connected if non ASM flow */
			asmIBlockLinkConditional(asmIBlockID(r4), asmLabels((Num)tag)); 
		}
	}
	DBEND();
}


/* Set's the iblock's icode op field to NA.  Used to remove
   icodes from an iblock during optimization.
*/
void asmIBlockDeleteICode (Obj ib, Num icidx) {
 Obj ic;
	assert(asmIsObjectTypeIBlock(ib));

	ic = asmIBlockICode(riblock, icidx);
	assert(asmIsObjectTypeICode(ic));

	memVectorSet(ic, 0, NA);
}

/* riblock <= iblock
     start <= index
       reg <= register field
 Look for "pop reg" icode in iblock in riblock starting at icode start,
 skipping nop and instructions that don't use the same register.
*/
Num asmOptimizePeepHolePushPopFindMatchingPop(Num start, Obj reg) {
 Obj ic;
 Num i;
 Obj field0, field1, field2, field3;

	for (i=start; i<asmIBlockICodeLength(riblock); ++i) {

		/* Consider next instruction and its fields */
		ic = asmIBlockICode(riblock, i);
		field0 = asmICodeField(ic, 0);
		field1 = asmICodeField(ic, 1);
		field2 = asmICodeField(ic, 2);
		field3 = asmICodeField(ic, 3);

		/* Found a matching push so stop looking and succeed passing index back */
		if ((POP == field0) && (reg == field1))
			return i;

		/* Found an instruction that alters the stack so stop looking and fail*/
		// TODO SYSI should delimt blocks
		if ((PUSH == field0) || (POP == field0) || (SYSI == field0))
			return 0;

		/* Found instruction that requires register, stop looking and fail */
		if ((reg == field1) || (reg == field2) || (reg == field3))
			return 0;
	}

	return 0;
}

/* riblock <= iblock
     start <= index
       reg <= register field
 Look for "push reg" icode in iblock in riblock starting at icode start,
 skipping nop and instructions that don't use the same register.
*/
Num asmOptimizePeepHolePopPushFindMatchingPush(Num start, Obj reg) {
 Obj ic;
 Num i;
 Obj field0, field1, field2, field3;

	for (i=start; i<asmIBlockICodeLength(riblock); ++i) {

		/* Consider next instruction and its fields */
		ic = asmIBlockICode(riblock, i);
		field0 = asmICodeField(ic, 0);
		field1 = asmICodeField(ic, 1);
		field2 = asmICodeField(ic, 2);
		field3 = asmICodeField(ic, 3);

		/* Found a matching push so stop looking and succeed passing index back */
		if ((PUSH == field0) && (reg == field1))
			return i;

		/* Found an instruction that alters the stack so stop looking and fail*/
		// TODO SYSI should delimt blocks
		if ((PUSH == field0) || (POP == field0) || (SYSI == field0))
			return 0;

		/* Found instruction that requires register, stop looking and fail */
		if ((reg == field1) || (reg == field2) || (reg == field3))
			return 0;
	}

	return 0;
}

/* Two functions which optimize simple push/pop and pop/push redundant icodes in all active iblocks
   riblock <= iblock to optimize
   return => optimization performed
*/
Num asmOptimizePeepHolePushPop(void) {
 Num changed=0;
 Obj ic;
 Num idx, i, j;
 Obj field0, field1;
	DBBEG();
	/* Consider every live iblock */
	for (i=0; i< asmIBlockFrameCount(); ++i) {
		riblock = asmIBlockFrame(i);
		if (otrue == asmIBlockTag(riblock)) do {
			idx = 0;
			/* Over every POP instruction */
			for (j=0; !idx && j<asmIBlockICodeLength(riblock); ++j) {
				ic = asmIBlockICode(riblock, j);
				field0 = asmICodeField(ic, 0); /* opcode */
				field1 = asmICodeField(ic, 1); /* reg 0 */
				if (PUSH == field0) {
					/* Find a matching push that cancels this pop */
					idx = asmOptimizePeepHolePushPopFindMatchingPop(j+1, field1);
					if (idx) {
						DB("Omitting "HEX" and "HEX, j, idx);
						changed = 1;
						DBE asmDumpIBlock(riblock);
						asmIBlockDeleteICode(riblock, j);
						asmIBlockDeleteICode(riblock, idx);
					}
				}
			}
		} while (idx);
	}
	DBEND(" => ", changed);
	return changed;
}

Num asmOptimizePeepHolePopPush(void) {
 Num changed=0;
 Obj ic;
 Num idx, i, j;
 Obj field0, field1;
	DBBEG();
	/* Consider every live iblock */
	for (i=0; i< asmIBlockFrameCount(); ++i) {
		riblock = asmIBlockFrame(i);
		if (otrue == asmIBlockTag(riblock)) do {
			idx = 0;
			/* Over every POP instruction */
			for (j=0; !idx && j<asmIBlockICodeLength(riblock); ++j) {
				ic = asmIBlockICode(riblock, j);
				field0 = asmICodeField(ic, 0); /* opcode */
				field1 = asmICodeField(ic, 1); /* reg 0 */
				if (POP == field0) {
					/* Find a matching push that cancels this pop */
					idx = asmOptimizePeepHolePopPushFindMatchingPush(j+1, field1);
					if (idx) {
						DB("Omitting "HEX" and "HEX, j, idx);
						changed = 1;
						DBE asmDumpIBlock(riblock);
						asmIBlockDeleteICode(riblock, j);
						asmIBlockDeleteICode(riblock, idx);
					}
				}
			}
		} while (idx);
	}
	DBEND(" => ", changed);
	return changed;
}

/* Incoming connections moved to outoing connections,
   outgoing connection removed, iblock invalidated.
    riblock <= Empty iblock to remove from igraph
*/
void asmOptimizeEmptyIBlock(void) {
 Obj mydef, lst, inib;
	if ((otrue == asmIBlockTag(riblock)) && (0 == asmIBlockICodeLength(riblock))) {
		DB("Found empty iblock:");

		assert(ofalse == asmIBlockConditionalTag(riblock)); /* Verify my empty conditional iblock */
		/* Consider default iblock */
		mydef = asmIBlockDefaultTag(riblock);

		/* Make sure my default tag is an iblock and not myself */
		assert(asmIsObjectTypeIBlock(mydef));

		if (riblock == mydef) {
			DB("Skipping empty blocks that jump to themselves");
			goto ret;
		}
		DBE asmDumpIBlockParentAndChildren(riblock);

		/* Remove myself from default iblock */
		asmIBlockIncomingListDel(mydef, riblock);
		/* Set incoming blocks' default and/or conditional block to my default */
		lst = asmIBlockIncomingList(riblock);
		assert(objIsPair(lst)); /* It's guaranteed to have an incoming list otherwise it wouldn't be tagged #t */
		/* Register local C variables with GC */
		memRootSetRegisterAnonymous(&mydef);
		memRootSetRegisterAnonymous(&lst);
		memRootSetRegisterAnonymous(&inib);
		while (onull != lst) {
			inib = car(lst); /* Consider an incoming block */
			if (riblock == asmIBlockDefaultTag(inib)) asmIBlockLinkDefault(asmIBlockID(inib), asmIBlockID(mydef));
			if (riblock == asmIBlockConditionalTag(inib)) asmIBlockLinkConditional(asmIBlockID(inib), asmIBlockID(mydef));
			lst = cdr(lst);
		}
		/* Unregister local C variables */
		memRootSetUnRegisterAnonymous(&inib);
		memRootSetUnRegisterAnonymous(&lst);
		memRootSetUnRegisterAnonymous(&mydef);
		asmIBlockTagSet(riblock, ofalse); /* Now invalidate this now unused iblock */

		DB("The result:");
		DBE asmDumpIBlockParentAndChildren(riblock);
	}
ret:
	return;
}

void asmOptimizeEmptyIBlocks(void) {
 Num i;
	DBBEG();
	/* Consider every live iblock */
	for (i=0; i< asmIBlockFrameCount(); ++i) {
		riblock = asmIBlockFrame(i);
		asmOptimizeEmptyIBlock();
	}
	DBEND();
}

/* riblock = temp
*/
void asmPeepHoleOptimization (void) {
 Num changed, optimizeLoop=0;
	DBBEG();
	do {
		assert(optimizeLoop < 100);
		changed = 0;
		if (asmOptimizePeepHolePushPop()) changed = 1;
		if (asmOptimizePeepHolePopPush()) changed = 1;
	} while (changed);
	
	DBEND();
}


/* Recursively traverse the igraph's iblocks.  Tag each with #t.
   Also resolve default and conditional branch tags.
*/
void asmPrepareIGraph (Obj ib) {
	/* Base case.  Not an iblock or the iblock has been traversed already (tagged with #t) */
	if (!asmIsObjectTypeIBlock(ib) || otrue == asmIBlockTag(ib))
		return;

	asmIBlockTagSet(ib, otrue); /* Tag iblock #t */

	vmPush(ib);
	asmInitIBlockBranchTagsToIBlocks(ib);
	ib = vmPop();

	vmPush(ib);
	asmPrepareIGraph(asmIBlockDefaultTag(ib));
	ib = vmPop();
	asmPrepareIGraph(asmIBlockConditionalTag(ib));
}


Num asmCountIGraphFields (void) {
 Obj ib, dib, icode, nextib;
 Num i, j, len=0;

	for (i=0; i< asmIBlockFrameCount(); ++i) {
		ib = asmIBlockFrame(i); /* Consider next iblock from vector of all iblocks */
		/* Only live iblocks are emitted found when performing a recursive walk
		   on the igraph and possibly removed during optimization */
		if (otrue == asmIBlockTag(ib)) {
			/* Count the number of fields in each icode in this iblock */
			for (j=0; j<asmIBlockICodeLength(ib); ++j) {
				icode = asmIBlockICode(ib, j);
				len += asmICodeOpcodeSize(icode);
			}
			/* Include room for a branch if the default iblock does not come after this iblock */
			dib = asmIBlockDefaultTag(ib);
			nextib = asmIBlockNextValid(asmIBlockID(ib)); /* Consider next iblock to be emitted */
			if (asmIsObjectTypeIBlock(dib) && dib != nextib)
				len += 2;
		}
	}
	return len;
}

void asmOptimizeIGraph (void) {
	asmPeepHoleOptimization();
	asmOptimizeEmptyIBlocks();
}

/* The IGraph's iblocks are found in riblocks.  The icode found in each iblock
   and the links between icodes, are assembled into a VM code object object
   rcodenew/r10 which can be run in the VM.

	Also restores previous ASM context
*/
void asmAssemble (void) {
 Num len;
	DBBEG("  ICodeCount="NUM"   iblockOffset="NUM, ICodeCount, iblockOffset);

	/* Might have to create one more last iblock with the remaining new icodes */
	if (icodeOffset < ICodeCount) asmGenerateIBlockWithPushedIcodes();

	assert((0 < IBlockCount) && "There are no iblocks to assemble");

	/* Create the code block object which all iblocks are compiled to */
	asmPrepareIGraph(asmIBlock(iblockOffset));

	if (ofalse != odebug) {
		fprintf(stderr, "\nasmAssemble() Un-optimized IGraph:");
		asmDumpIBlocks();
	}

	asmOptimizeIGraph();

	if (ofalse != odebug) {
		fprintf(stderr, "\nasmAssemble() Optimized IGraph:");
		asmDumpIBlocks();
	}

	len = asmCountIGraphFields();
	if (len) {
		rcodenew = memNewVector(TCODE, len);
		pccode = 0;
		asmPlaceAllIBlocks();

		asmResolveBranchOpcodeAddresses();

		r0 = rcodenew;

//		if (ofalse != odebug) objDisplay(rlabels, stdout);
		if (ofalse != odebug) objDisplay(rcodenew, stderr);
	} else {
		r0 = ofalse;
	}

	asmEnd();

	DBEND();
}



/*******************************************************************************
 Debugging
*******************************************************************************/
void asmDumpICodeFields (Obj ic) {
 Obj f;
	if ((Obj)NA != (f = asmICodeField(ic, 1))) { assert(f<=RF); fprintf(stderr, " $"HEX, f); }
	if ((Obj)NA != (f = asmICodeField(ic, 2))) { assert(f<=RF); fprintf(stderr, " $"HEX, f); }
	if ((Obj)NA != (f = asmICodeField(ic, 3))) { assert(f<=RF); fprintf(stderr, " $"HEX, f); }
	if ((Obj)NA != (f = asmICodeField(ic, 4))) { fprintf(stderr, " "); objDisplay(f, stderr); fflush(stderr); }
	if ((Obj)NA != (f = asmICodeField(ic, 5))) { fprintf(stderr, " "); objDisplay(f, stderr); fflush(stderr); }
}
void asmDumpICode (Obj ic) {
 Obj field;
	if (memIsObjectValid(ic)) {
		field = asmICodeField(ic, 0);
		switch ((Num)field) {
			case (Num)MV  : fprintf(stderr, "mv  "); asmDumpICodeFields(ic); break;
			case (Num)MVI : fprintf(stderr, "mvi "); asmDumpICodeFields(ic); break;
			case (Num)LDI : fprintf(stderr, "ldi "); asmDumpICodeFields(ic); break;
			case (Num)LD  : fprintf(stderr, "ld  "); asmDumpICodeFields(ic); break;
			case (Num)STI : fprintf(stderr, "sti "); asmDumpICodeFields(ic); break;
			case (Num)ST  : fprintf(stderr, "st  "); asmDumpICodeFields(ic); break;
			case (Num)PUSH: fprintf(stderr, "push"); asmDumpICodeFields(ic); break;
			case (Num)POP : fprintf(stderr, "pop "); asmDumpICodeFields(ic); break;
			case (Num)ADDI: fprintf(stderr, "addi"); asmDumpICodeFields(ic); break;
			case (Num)BLTI: fprintf(stderr, "blti"); asmDumpICodeFields(ic); break;
			case (Num)BEQI: fprintf(stderr, "beqi"); asmDumpICodeFields(ic); break;
			case (Num)BNEI: fprintf(stderr, "bnei"); asmDumpICodeFields(ic); break;
			case (Num)BRTI: fprintf(stderr, "brti"); asmDumpICodeFields(ic); break;
			case (Num)BNTI: fprintf(stderr, "bnti"); asmDumpICodeFields(ic); break;
			case (Num)BRA : fprintf(stderr, "bra "); asmDumpICodeFields(ic); break;
			case (Num)JMP : fprintf(stderr, "jmp "); asmDumpICodeFields(ic); break;
			case (Num)JAL : fprintf(stderr, "jal "); asmDumpICodeFields(ic); break;
			case (Num)RET : fprintf(stderr, "ret");                          break;
			case (Num)SYS : fprintf(stderr, "sys "); asmDumpICodeFields(ic); break;
			case (Num)SYSI: fprintf(stderr, "sysi"); asmDumpICodeFields(ic); break;
			case (Num)NOP : fprintf(stderr, "nop");                          break;
			case (Num)QUIT: fprintf(stderr, "quit");                         break;
			default:
				if (field == NA) { fprintf(stderr, "---"); break; }
				fprintf(stderr, "**UNKNOWN OPCODE**");
				objDisplay(ic, stderr);
		}
	}
}

void asmDumpIBlock (Obj ib) {
 Num i;
 Obj o, block;
	assert(asmIsObjectTypeIBlock(ib));
	/* ID */
	fprintf(stderr, "\n#<"HEX03"  ", asmIBlockID(ib));

	/* Tag */
	o = asmIBlockTag(ib);
	if (ofalse == o)
		fprintf(stderr, "---");
	else
		fprintf(stderr, HEX04, asmIBlockTag(ib));

	/* Default block */
	block = asmIBlockDefaultTag(ib);
	if (ofalse==block)
		fprintf(stderr, "  [---]");
	else {
		fprintf (stderr, "  [");
		if (asmIsObjectTypeIBlock(block)) fprintf(stderr, HEX03, asmIBlockID(block));
		else objDisplay(block, stderr);
		fprintf (stderr, "]");
	}

	/* Conditional block */
	block = asmIBlockConditionalTag(ib);
	if (ofalse==block)
		fprintf(stderr, "  [---]");
	else {
		fprintf (stderr, "  [");
		if (asmIsObjectTypeIBlock(block)) fprintf(stderr, HEX03, asmIBlockID(block));
		else objDisplay(block, stderr);
		fprintf (stderr, "]");
	}
	/* Incoming block IDs */
	fprintf(stderr, "  (");
	for (o = asmIBlockIncomingList(ib); onull != o; ) {
		fprintf(stderr, HEX03, asmIBlockID(car(o)));
		o = cdr(o);
		if (onull != o) fprintf(stderr, " ");
	}
	fprintf(stderr, ")");

	/* Code */
	for (i=0; i<asmIBlockICodeLength(ib); ++i) {
		fprintf(stderr, "\n  "HEX02"  ", i);
		asmDumpICode(asmIBlockICode(ib, i));
	}
	fprintf (stderr, ">");
}

void asmDumpIBlocks (void) {
 Num i;
 int fl;
	/* Temporarily enable blocking I/O */
	fl = fcntl(0, F_GETFL, 0);
	fcntl (0, F_SETFL, fl&~O_NONBLOCK);

	DBBEG();

	for (i=iblockOffset; i<IBlockCount; ++i)
		asmDumpIBlock(asmIBlock(i));

	DBEND();

	fcntl (0, F_SETFL, fl);
}

void asmDumpIBlockParentAndChildren (Obj ib) {
 Obj lst, last, inib, dib, cib;
	lst = asmIBlockIncomingList(ib);
	last = onull;
	while (onull != lst) {
		inib = car(lst); /* Consider an incoming block */
		if (last != inib) asmDumpIBlock(inib);
		last = inib;
		lst = cdr(lst);
	}
	asmDumpIBlock(ib);
	dib = asmIBlockDefaultTag(ib);
	cib = asmIBlockConditionalTag(ib);
	if (asmIsObjectTypeIBlock(dib)) asmDumpIBlock(dib);
	if (asmIsObjectTypeIBlock(cib)) asmDumpIBlock(cib);
}



/*******************************************************************************
 Init
*******************************************************************************/
void asmInitialize (void) {
 static Num shouldInitialize=1;
	DBBEG();
	if (shouldInitialize) {
		DB("Activating module");
		shouldInitialize=0;

		DB("Initializing submodules");
		objInitialize(); /* objInitialize -> vmInitialize -> memInitialize */

		DB("Registering rootset objects");
		memRootSetRegister(ropcodes);
		memRootSetRegister(riblock);
		memRootSetRegister(riblocks);
		memRootSetRegister(ricodes);
		memRootSetRegister(rlabels);
		memRootSetRegister(rcodenew);

		DB("Registering types");
		memTypeRegisterString(TICODE, (Str)"icode");
		memTypeRegisterString(TIBLOCK, (Str)"iblock");

		DB("Initializing compiler related objects");
		objNewSymbolStatic("sasmend"); sasmend = r0;
		objNewSymbolStatic("sasmna"); sasmna = r0;
		objNewVector(IBLOCK_VECTOR_SIZE);
		riblocks = r0;
		objNewVector(ICODE_VECTOR_SIZE);
		ricodes = r0;
		objNewVector(LABELS_DB_SIZE);
		rlabels = r0;
		objNewVector(OPCODES_VEC_SIZE);
		ropcodes = r0;
	} else {
		DB("Module already activated");
	}
	DBEND();
}



#undef DB_DESC
#undef DEBUG
