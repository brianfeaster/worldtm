#define DEBUG 0
#define DB_DESC "ASM "
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>
#include <fcntl.h>
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
*/

void ccDumpIBlock (Obj ib);
void ccDumpIBlocks (void);
void compDumpIBlockParentAndChildren (Obj ib);

/* Local static scheme symbol used to delimit assembly opcodes */
Obj sasmend;
Obj sasmna;

/* Register aliases
*/
#define ropcodes    ra /* Vector of incoming assembly opcodes */
#define riblock     rb /* The current iblock or most recently created */
#define riblocks    rc /* Vector of all iblocks */
#define ricodes     rd /* Vector where new icodes are initially placed before being stuffed into a new iblock */
#define rlabels     re /* Vector of blocks indexed by its assembly label value */
#define rexpr       rf /* Expression being compiled.  See vm.h */
#define rcodenew    r10 /* Where a new code block is created and emitted to */

/* Object types used by compiler
*/
#define TICODE  0x87l
#define TIBLOCK 0x88l
Num ccIsObjectTypeICode  (Obj ic) { return memIsObjectType(ic, TICODE); }
Num ccIsObjectTypeIBlock (Obj ib) { return memIsObjectType(ib, TIBLOCK); }



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

void ccICodePush (Obj ic) {
	assert(ccIsObjectTypeICode(ic));
	assert(ICodeCount < ICODE_VECTOR_SIZE);
	memVectorSet(ricodes, ICodeCount++, ic);
}

Obj ccICodePop (void) {
 Obj ic;
	assert(0 < ICodeCount);
	ic = memVectorObject(ricodes, --ICodeCount);
	return ic;
}

/* Will always return 6
*/
Num asmICodeFieldLength (Obj ic) {
	assert(ccIsObjectTypeICode(ic));
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
	ccICodePush(r0);
}
	
void ccICodePushNewMV (Obj r, Obj s)         { asmNewICode(MV,   r,  s, NA, NA, NA); }
void ccICodePushNewMVI (Obj r, Obj i)        { asmNewICode(MVI,  r, NA, NA,  i, NA); }
void ccICodePushNewLDI (Obj r, Obj s, Obj i) { asmNewICode(LDI,  r,  s, NA,  i, NA); }
void ccICodePushNewLD (Obj r, Obj s, Obj t)  { asmNewICode(LD,   r,  s,  t, NA, NA); }
void ccICodePushNewSTI (Obj r, Obj s, Obj i) { asmNewICode(STI,  r,  s, NA,  i, NA); }
void ccICodePushNewST (Obj r, Obj s, Obj t)  { asmNewICode(ST,   r,  s,  t, NA, NA); }
void ccICodePushNewPUSH (Obj r)              { asmNewICode(PUSH, r, NA, NA, NA, NA); }
void ccICodePushNewPOP (Obj r)               { asmNewICode(POP,  r, NA, NA, NA, NA); }
void ccICodePushNewADDI (Obj r, Obj i)       { asmNewICode(ADDI, r, NA, NA,  i, NA); }
void ccICodePushNewBLTI (Obj r, Obj i, Obj l){ asmNewICode(BLTI, r, NA, NA,  i,  l); }
void ccICodePushNewBEQI (Obj r, Obj i, Obj l){ asmNewICode(BEQI, r, NA, NA,  i,  l); }
void ccICodePushNewBNEI (Obj r, Obj i, Obj l){ asmNewICode(BNEI, r, NA, NA,  i,  l); }
void ccICodePushNewBRTI (Obj r, Obj t, Obj l){ asmNewICode(BRTI, r, NA, NA,  t,  l); }
void ccICodePushNewBNTI (Obj r, Obj t, Obj l){ asmNewICode(BNTI, r, NA, NA,  t,  l); }
void ccICodePushNewBRA (Obj l)               { asmNewICode(BRA, NA, NA, NA, NA,  l); }
void ccICodePushNewJMP (Obj r)               { asmNewICode(JMP,  r, NA, NA, NA, NA); }
void ccICodePushNewJAL (Obj r)               { asmNewICode(JAL,  r, NA, NA, NA, NA); }
void ccICodePushNewRET ()                    { asmNewICode(RET, NA, NA, NA, NA, NA); }
void ccICodePushNewSYS (Obj r)               { asmNewICode(SYS,  r, NA, NA, NA, NA); }
void ccICodePushNewSYSI (Obj i)              { asmNewICode(SYSI,NA, NA, NA,  i, NA); }
void ccICodePushNewNOP (void)                { asmNewICode(NOP, NA, NA, NA, NA, NA); }
void ccICodePushNewQUIT (void)               { asmNewICode(QUIT,NA, NA, NA, NA, NA); }

Num asmICodeOpcodeSize (Obj icode) {
	assert(ccIsObjectTypeICode(icode));
	if (NOP == asmICodeField(icode, 0)) return 0; /* Ignore NOPs since they're not emitted */
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

Num ccIBlockID (Obj ib)             { return (Num)memVectorObject(ib, IBLOCK_INDEX_ID); }
Obj ccIBlockTag (Obj ib)            { return memVectorObject(ib, IBLOCK_INDEX_TAG); }
Obj ccIBlockDefaultTag (Obj ib)     { return memVectorObject(ib, IBLOCK_INDEX_DEFAULT); }
Obj ccIBlockConditionalTag (Obj ib) { return memVectorObject(ib, IBLOCK_INDEX_CONDITIONAL); }
Obj ccIBlockIncomingList (Obj ib)   { return memVectorObject(ib, IBLOCK_INDEX_INCOMING); }

Num ccIBlockICodeLength (Obj ib) {
	assert(ccIsObjectTypeIBlock(ib));
	return memObjectLength(ib) - IBLOCK_INDEX_ICODE;
}

/* Get ith icode object in iblock 
*/
Obj ccIBlockICode (Obj ib, Num i) {
	assert(ccIsObjectTypeIBlock(ib));
	return memVectorObject(ib, IBLOCK_INDEX_ICODE + i);
}

/* Lookup iblock by ID
*/
Obj ccIBlock (Num id) {
 Obj ib;
	ib = memVectorObject(riblocks, id);
	assert(ccIsObjectTypeIBlock(ib));
	assert(ccIBlockID(ib) == id);
	return ib;
}


Num ccIBlockIsValid (Obj ib) {
	return ccIBlockID(ib) < IBlockCount;
}


/* Set iblock's various tag values
*/

void ccIBlockTagSet (Obj ib, Obj tag) {
	memVectorSet(ib, IBLOCK_INDEX_TAG, tag);
}

void ccIBlockDefaultTagSet (Obj ib, Obj tag) {
	memVectorSet(ib, IBLOCK_INDEX_DEFAULT, tag);
}

void ccIBlockConditionalTagSet (Obj ib, Obj tag) {
	memVectorSet(ib, IBLOCK_INDEX_CONDITIONAL, tag);
}

/*   r1 = temp
     r2 = temp
     r3 = temp
*/
void ccIBlockIncomingListAdd (Obj ib, Obj o) {
	r3 = ib;
	r1 = o;
	r2 = memVectorObject(r3, IBLOCK_INDEX_INCOMING);
	objCons12();
	memVectorSet(r3, IBLOCK_INDEX_INCOMING, r0);
}

void ccIBlockIncomingListDel (Obj ib, Obj o) {
 Obj l, n;

	/* Consider list */
	l = ccIBlockIncomingList(ib);

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

void ccIBlockSetICode (Num offset, Obj op) {
	assert(offset < memObjectLength(riblock));
	memVectorSet(riblock, IBLOCK_INDEX_ICODE+offset, op);
}


/* Create new igraph block. Increments IBlockCount which is
   also used as the block's ID.
    iblock => iblock object
*/
void ccGenerateNewIBlock (Num icodeSize) {
	riblock = memNewVector(TIBLOCK, icodeSize + IBLOCK_INDEX_ICODE);

	/* Unique ID number set automatically */
	memVectorSet(riblock, IBLOCK_INDEX_ID, (Obj)IBlockCount);

	/* Tag defaults to #f */
	memVectorSet(riblock, IBLOCK_INDEX_TAG, false);

	/* Incoming iblock list defaults to empty list */
	memVectorSet(riblock, IBLOCK_INDEX_INCOMING, null);

	/* Outgoing default and conditional branch tags */
	memVectorSet(riblock, IBLOCK_INDEX_DEFAULT, false);
	memVectorSet(riblock, IBLOCK_INDEX_CONDITIONAL, false);

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

Num ccLabels (Num i) {
 Num labelsBlockID;
	assert(i < LabelsCount); /* Invalid label */
	labelsBlockID = (Num)memVectorObject(rlabels, i);
	assert(LABELS_INVALID_ID != labelsBlockID); /* Label has no registered block */
	return labelsBlockID;
}

void ccLabelsSet (Num i, Num blockNumber) {
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
void ccEnd (void) {
	IBlockCount = iblockOffset;

	riblock = vmPop();
	assert(riblock == 0 || ccIsObjectTypeIBlock(riblock));

	iblockOffset = (Num)vmPop();
	assert(iblockOffset < IBLOCK_VECTOR_SIZE);

	icodeOffset = (Num)vmPop();
	assert(icodeOffset < ICODE_VECTOR_SIZE);
}


/* Prepare aassembler for multiple calls to ccAsm()
*/
void asmInit (void) {
	DBBEG();

	riblock = 0;
	IBlockCount = 0;
	assert(0 == ICodeCount); /* Check we're not initializing the assembler in the middle of assembly */
	ICodeCount = 0;
	LabelsCount = 0;

	asmStart();

	DBEND();
}


/* Create a new iblock containing the most recent set of icodes created.  Skip if no icodes left.
*/
void ccGenerateIBlockWithPushedIcodes () {
 Obj ic;
	/* Create new empty iblock in riblock and 'pop' icodes from stack into it */
	ccGenerateNewIBlock(ICodeCount - icodeOffset);

	while (icodeOffset < ICodeCount) {
		ic = ccICodePop(); /* This decrement ICodeCount */
		ccIBlockSetICode(ICodeCount - icodeOffset, ic);
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
			ccICodePushNewMV(r, rr);
		} else if (MVI == obj) {
			r = asmOpcodesNext();
			o = asmOpcodesNext();
			DB("mvi ["HEX" "HEX" "HEX"]", obj, r, o);
			ccICodePushNewMVI(r, o);
		} else if (LDI == obj) {
			r = asmOpcodesNext();
			rr = asmOpcodesNext();
			o = asmOpcodesNext();
			DB("ldi ["HEX" "HEX" "HEX"]", r, rr, o);
			ccICodePushNewLDI(r, rr, o);
		} else if (LD == obj) {
			r = asmOpcodesNext();
			rr = asmOpcodesNext();
			rrr = asmOpcodesNext();
			DB("ld ["HEX" "HEX" "HEX"]", r, rr, rrr);
			ccICodePushNewLD(r, rr, rrr);
		} else if (STI == obj) {
			r = asmOpcodesNext();
			rr = asmOpcodesNext();
			o = asmOpcodesNext();
			DB("sti ["HEX" "HEX" "HEX"]", r, rr, o);
			ccICodePushNewSTI(r, rr, o);
		} else if (ST == obj) {
			r = asmOpcodesNext();
			rr = asmOpcodesNext();
			rrr = asmOpcodesNext();
			DB("st ["HEX" "HEX" "HEX"]", r, rr, rrr);
			ccICodePushNewST(r, rr, rrr);
		} else if (PUSH == obj) {
			o = asmOpcodesNext();
			DB("push["HEX"]", o);
			ccICodePushNewPUSH(o);
		} else if (POP == obj) {
			o = asmOpcodesNext();
			DB("pop ["HEX"]", o);
			ccICodePushNewPOP(o);
		} else if (ADDI == obj) {
			r = asmOpcodesNext();
			o = asmOpcodesNext();
			DB("addi["HEX" "HEX"]", r, o);
			ccICodePushNewADDI(r, o);
		} else if (BLTI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("blti["HEX" "HEX" "HEX"]", r, i, l);
			ccICodePushNewBLTI(r, i, l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockDefaultTagSet(riblock, true); /* signal this block's default is the next one */
			ccIBlockConditionalTagSet(riblock, l);  /* signal this block conditional is a label */
		} else if (BEQI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("beqi["HEX" "HEX" "HEX"]", r, i, l);
			ccICodePushNewBEQI(r, i, l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockDefaultTagSet(riblock, true); /* signal this block's default is the next one */
			ccIBlockConditionalTagSet(riblock, l);  /* signal this block conditional is a label */
		} else if (BNEI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("bnei["HEX" "HEX" "HEX"]", r, i, l);
			ccICodePushNewBNEI(r, i, l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockDefaultTagSet(riblock, true); /* signal this block's default is the next one */
			ccIBlockConditionalTagSet(riblock, l);  /* signal this block conditional is a label */
		} else if (BRTI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("brti["HEX" "HEX" "HEX"]", r, i, l);
			ccICodePushNewBRTI(r, i, l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockDefaultTagSet(riblock, true); /* signal this block's default is the next one */
			ccIBlockConditionalTagSet(riblock, l);  /* signal this block conditional is a label */
		} else if (BNTI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("bnti["HEX" "HEX" "HEX"]", r, i, l);
			ccICodePushNewBNTI(r, i, l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockDefaultTagSet(riblock, true); /* signal this block's default is the next one */
			ccIBlockConditionalTagSet(riblock, l);  /* signal this block conditional is a label */
		} else if (BRA == obj) {
			l = asmOpcodesNext();
			DB("bra ["HEX"]", l);
			//ccICodePushNewBRA(l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockDefaultTagSet(riblock, l);  /* signal this block's default is a label */
		} else if (JMP == obj) {
			r = asmOpcodesNext();
			DB("j   ["HEX"]", r);
			ccICodePushNewJMP(r);
			ccGenerateIBlockWithPushedIcodes();
			// no default block after a jump
		} else if (JAL == obj) {
			r = asmOpcodesNext();
			DB("jal ["HEX"]", r);
			ccICodePushNewJAL(r);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockDefaultTagSet(riblock, true);  /* default block is next */
		} else if (RET == obj) {
			DB("ret []");
			ccICodePushNewRET();
			ccGenerateIBlockWithPushedIcodes();
			// no default block after a ret
		} else if (SYS == obj) {
			r = asmOpcodesNext();
			DB("sys ["HEX"]",r);
			ccICodePushNewSYS(r);
		} else if (SYSI == obj) {
			o = asmOpcodesNext();
			DB("sysi["HEX"]", o);
			ccICodePushNewSYSI(o);
		} else if (NOP == obj) {
			DB("nop[]");
			ccICodePushNewNOP();
		} else if (QUIT == obj) {
			DB("quit[]");
			ccICodePushNewQUIT();
		} else if (LABEL == obj) {
			l = asmOpcodesNext();
			DB("label["HEX"]", l);
			if (0 < ICodeCount - icodeOffset) {
				ccGenerateIBlockWithPushedIcodes();
				ccIBlockDefaultTagSet(riblock, true);  /* default block is next */
			}
			/* Set the next block's ID in the label/iblockID table */
			ccLabelsSet((Num)l, IBlockCount);
		} else {
			DB("["HEX"]", obj);
			assert(!"Unhandled asm opcode");
		}
	}
	assert(0 == OpcodesCount);
//fprintf(stderr, "ICodeCount="NUM, ICodeCount);
//sysWrite(rlabels, stdout);
//ccDumpIBlocks();
//sysWrite(rlabels, stdout); printf("\n");
	DBEND ("  ICodeCount "NUM"   IBlockCount "NUM, ICodeCount, IBlockCount);
}



/*******************************************************************************
 Assemble

 Assemble the iblocks in an igraph into a VM runable code block object
*******************************************************************************/
Num pccode = 0;  /* Pointer into code block */


void ccEmitOpcode (Obj op) {
	if (pccode >= memObjectLength(rcodenew)) vmDebugDumpCode(rcodenew, stderr);
	assert(pccode < memObjectLength(rcodenew) && "Code object can't fit more icodes.");
	memVectorSet(rcodenew, pccode++, op);
}

void ccEmitOpcode2 (Obj op1, Obj op2) {
	memVectorSet(rcodenew, pccode++, op1);
	memVectorSet(rcodenew, pccode++, op2);
}


void ccEmitIblockOpcodes (void) {
 Num i;
 Obj field0, field1, field2, field3, field4;
	DBBEG("      iblock="NUM, ccIBlockID(riblock));

	/* Re-tag the iblock with its initial location in the code block */
	assert(true == ccIBlockTag(riblock));
	ccIBlockTagSet (riblock, (Obj)pccode);

	for (i=0; i<ccIBlockICodeLength(riblock); ++i) {
		r0 = ccIBlockICode(riblock, i); /* Consider icode object in r0 */
		field0 = asmICodeField(r0, 0);
		DB("field0 = "HEX, field0);
		switch ((Num)field0) {
		case (Num)MV:
			field1 = asmICodeField(r0, 1);
			field2 = asmICodeField(r0, 2);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				               case (Num)R1 : ccEmitOpcode(vmMV01); break;
				               case (Num)R3 : ccEmitOpcode(vmMV03); break;
				               case (Num)R4 : ccEmitOpcode(vmMV04); break;
				               case (Num)R1E: ccEmitOpcode(vmMV01E); break;
				               default : assert(!"Unsuported icode MV $0 ??"); } break;
				case (Num)R1 : switch ((Num)field2) {
				               case (Num)R0 : ccEmitOpcode(vmMV10); break;
				               case (Num)R3 : ccEmitOpcode(vmMV13); break;
				               default : assert(!"Unsuported icode MV $1 ??"); } break;
				case (Num)R2 : switch ((Num)field2) {
				               case (Num)R0 : ccEmitOpcode(vmMV20); break;
				               default : assert(!"Unsuported icode MV $2 ??"); } break;
				case (Num)R3 : switch ((Num)field2) {
				               case (Num)R0 : ccEmitOpcode(vmMV30); break;
				               default : assert(!"Unsuported icode MV $3 ??"); } break;
				case (Num)R5 : switch ((Num)field2) {
				               case (Num)R0  : ccEmitOpcode(vmMV50); break;
				               case (Num)R18 : ccEmitOpcode(vmMV518); break;
				               case (Num)R1C : ccEmitOpcode(vmMV51C); break;
				               default : assert(!"Unsuported icode MV $5 ??"); } break;
				case (Num)R1C: switch ((Num)field2) {
				               case (Num)R0  : ccEmitOpcode(vmMV1C0); break;
				               case (Num)R18 : ccEmitOpcode(vmMV1C18); break;
				               default : assert(!"Unsuported icode MV $1C ??"); } break;
				default : assert(!"Unsuported icode MV ?? reg"); }
			break;
		case (Num)MVI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmMVI0); break;
				case (Num)R1 : ccEmitOpcode(vmMVI1); break;
				case (Num)R2 : ccEmitOpcode(vmMVI2); break;
				case (Num)R3 : ccEmitOpcode(vmMVI3); break;
				case (Num)R4 : ccEmitOpcode(vmMVI4); break;
				case (Num)R6 : ccEmitOpcode(vmMVI6); break;
				case (Num)R7 : ccEmitOpcode(vmMVI7); break;
				default : assert(!"Unsuported field MVI ?reg? imm"); }
			ccEmitOpcode(field4);
			break;
		case (Num)LDI:
			field1 = asmICodeField(r0, 1);
			field2 = asmICodeField(r0, 2);
			field4 = asmICodeField(r0, 4);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				          case (Num)R0 : ccEmitOpcode(vmLDI00); break;
				          case (Num)R2 : ccEmitOpcode(vmLDI02); break;
				          case (Num)R1C: ccEmitOpcode(vmLDI01C); break;
				          default : assert(!"Unsuported field LDI $0 ?reg? imm"); } break;
				case (Num)R1 : switch ((Num)field2) {
				          case (Num)R1 : ccEmitOpcode(vmLDI11); break;
				          case (Num)R1C: ccEmitOpcode(vmLDI11C); break;
				          default : assert(!"Unsuported field LDI $1 ?reg? imm"); } break;
				case (Num)R2 : switch ((Num)field2) {
				          case (Num)R0 : ccEmitOpcode(vmLDI20); break;
				          case (Num)R2 : ccEmitOpcode(vmLDI22); break;
				          default : assert(!"Unsuported field LDI $2 ?reg? imm"); } break;
				case (Num)R5 : switch ((Num)field2) {
				          case (Num)R0 : ccEmitOpcode(vmLDI50); break;
				          default : assert(!"Unsuported field LDI $5 ?reg? imm"); } break;
				case (Num)R1C: switch ((Num)field2) {
				          case (Num)R0 : ccEmitOpcode(vmLDI1C0); break;
				          default : assert(!"Unsuported field LDI $1C ?reg? imm"); } break;
				default : assert(!"Unsuported field LDI ?reg? reg imm"); }
			ccEmitOpcode(field4);
			break;
		case (Num)LD:
			field1 = asmICodeField(r0, 1);
			field2 = asmICodeField(r0, 2);
			field3 = asmICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				               case (Num)R1 : switch ((Num)field3) {
				                              case (Num)R2 : ccEmitOpcode(vmLD012); break;
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
				               case (Num)R1 : ccEmitOpcode(vmSTI01); break;
				               case (Num)R1C: ccEmitOpcode(vmSTI01C); break;
				               default : assert(!"Unsuported field STI $0 ?reg? imm"); } break;
				case (Num)R2 : switch ((Num)field2) {
				               case (Num)R0 : ccEmitOpcode(vmSTI20); break;
				               case (Num)R1 : ccEmitOpcode(vmSTI21); break;
				               default : assert(!"Unsuported field STI $2 ?reg? imm"); } break;
				case (Num)R3 : switch ((Num)field2) {
				               case (Num)R0 : ccEmitOpcode(vmSTI30); break;
				               default : assert(!"Unsuported field STI $3 ?reg? imm"); } break;
				case (Num)R5 : switch ((Num)field2) {
				               case (Num)R0 : ccEmitOpcode(vmSTI50); break;
				               default : assert(!"Unsuported field STI $5 ?reg? imm"); } break;
				default : assert(!"Unsuported field STI ?reg? reg imm"); }
			ccEmitOpcode(field4);
			break;
		case (Num)ST:
			field1 = asmICodeField(r0, 1);
			field2 = asmICodeField(r0, 2);
			field3 = asmICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				               case (Num)R1 : switch ((Num)field3) {
				                              case (Num)R2 :  ccEmitOpcode(vmST012); break;
				                              default : assert(!"Unsuported field ST $0 $1 ?reg?"); } break;
				               default : assert(!"Unsuported field ST $0 ?reg? reg"); } break;
				default : assert(!"Unsuported field ST ?reg? reg reg"); }
			break;
		case (Num)PUSH:
			field1 = asmICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmPUSH0); break;
				case (Num)R1 : ccEmitOpcode(vmPUSH1); break;
				case (Num)R2 : ccEmitOpcode(vmPUSH2); break;
				case (Num)R4 : ccEmitOpcode(vmPUSH4); break;
				case (Num)R7 : ccEmitOpcode(vmPUSH7); break;
				case (Num)R19: ccEmitOpcode(vmPUSH19); break;
				case (Num)R1A: ccEmitOpcode(vmPUSH1A); break;
				case (Num)R1B: ccEmitOpcode(vmPUSH1B); break;
				default : assert(!"Unsuported field PUSH ?reg?"); }
			break;
		case (Num)POP:
			field1 = asmICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmPOP0); break;
				case (Num)R1 : ccEmitOpcode(vmPOP1); break;
				case (Num)R2 : ccEmitOpcode(vmPOP2); break;
				case (Num)R3 : ccEmitOpcode(vmPOP3); break;
				case (Num)R4 : ccEmitOpcode(vmPOP4); break;
				case (Num)R7 : ccEmitOpcode(vmPOP7); break;
				case (Num)R19: ccEmitOpcode(vmPOP19); break;
				case (Num)R1A: ccEmitOpcode(vmPOP1A); break;
				case (Num)R1B: ccEmitOpcode(vmPOP1B); break;
				default : assert(!"Unsuported field POP ?reg?"); }
			break;
		case (Num)ADDI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmADDI0); break;
				case (Num)R1 : ccEmitOpcode(vmADDI1); break;
				case (Num)R2 : ccEmitOpcode(vmADDI2); break;
				default : assert(!"Unsuported field ADDI ?reg? imm"); }
			ccEmitOpcode(field4);
			break;
		case (Num)BLTI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			//field5 = asmICodeField(r0, 5);
			switch ((Num)field1) {
				case (Num)R1 : ccEmitOpcode(vmBLTI1); break;
				default : assert(!"Unsuported field BLTI ?reg? imm offset"); }
			ccEmitOpcode(field4);
			ccEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BEQI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			//field5 = asmICodeField(r0, 5);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmBEQI0); break;
				case (Num)R1 : ccEmitOpcode(vmBEQI1); break;
				case (Num)R7 : ccEmitOpcode(vmBEQI7); break;
				default : assert(!"Unsuported field BEQI ?reg? imm offset"); }
			ccEmitOpcode(field4);
			ccEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BNEI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			//field5 = asmICodeField(r0, 5);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmBNEI0); break;
				case (Num)R1 : ccEmitOpcode(vmBNEI1); break;
				case (Num)R2 : ccEmitOpcode(vmBNEI2); break;
				case (Num)R5 : ccEmitOpcode(vmBNEI5); break;
				default : assert(!"Unsuported field BNEI ?reg? imm offset"); }
			ccEmitOpcode(field4);
			ccEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BRTI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			//fieldj = asmICodeField(r0, 5);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmBRTI0); break;
				default : assert(!"Unsuported field BRTI ?reg? imm offset"); }
			ccEmitOpcode(field4);
			ccEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BNTI:
			field1 = asmICodeField(r0, 1);
			field4 = asmICodeField(r0, 4);
			//field5 = asmICodeField(r0, 5);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmBNTI0); break;
				default : assert(!"Unsuported field BNTI ?reg? imm offset"); }
			ccEmitOpcode(field4);
			ccEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BRA :
			/* Emitted by parent logic */
			break;
		case (Num)JMP :
			field1 = asmICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmJ0); break;
				case (Num)R2 : ccEmitOpcode(vmJ2); break;
				default : assert(!"Unsuported field JMP ?reg?"); }
			break;
		case (Num)JAL :
			field1 = asmICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmJAL0); break;
				case (Num)R2 : ccEmitOpcode(vmJAL2); break;
				default : assert(!"Unsuported field JAL ?reg?"); }
			break;
		case (Num)RET :
			ccEmitOpcode(vmRET);
			break;
		case (Num)SYS :
			field1 = asmICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmSYS0); break;
				default : assert(!"Unsuported field SYS ?imm?"); }
			break;
		case (Num)SYSI:
			field4 = asmICodeField(r0, 4);
			ccEmitOpcode2(vmSYSI, field4);
			break;
		case (Num)QUIT:
			ccEmitOpcode(vmQUIT);
			break;
		case (Num)NOP:
			//ccEmitOpcode(vmNOP);
			break;
		default:
			fprintf(stderr, "\nCan't assemble opcode ");
			objDump(r3, stderr);
			assert(!"Unsuported opcode");
		}
	}

	DBEND();
}

/* Given an iblock ID, return the next valid iblock based on
   incrementing ID numbers.
*/
Obj asmNextValidIBlock (Num id) {
 Obj ib;
	while (++id < IBlockCount) {
		ib = ccIBlock(id);
		if (true == ccIBlockTag(ib)) return ib;
	}
	return false;
}

/* Emit a jump opcode if the iblock's default iblock will not be emitted after this
   one.  Also set the default/conditional branch field code-block offsets in the
   iblock structure.
*/
void ccPrepareIBlockBranches (void) {
 Obj defBlock, condBlock;
 Num codeBlockOffset, id;
 Obj nextib;
	DBBEG("  iblock="NUM, ccIBlockID(riblock));
	/* If no default block is set then verify no conditional block either as it's
	   probably the final "quit" iblock. */
	if (false == ccIBlockDefaultTag(riblock)) {
		assert(false == ccIBlockConditionalTag(riblock));
		goto ret;
	}

	/* If the iblock has a conditional iblock, cache the branch opcode's offset-field location and
	   set the field value to the target iblock temporarily */
	condBlock = ccIBlockConditionalTag(riblock);
	if (false != condBlock) {
		assert(ccIsObjectTypeIBlock(condBlock));
		codeBlockOffset = pccode - 1;
		memVectorSet(rcodenew, codeBlockOffset, condBlock);
		ccIBlockConditionalTagSet(riblock, (Obj)codeBlockOffset);
	}

	/* A default iblock exists.  If the iblock is the last iblock in the igraph vector
	   or the next iblock to emit is not my default, emit a jump opcode. */
	id = ccIBlockID(riblock);
	nextib = asmNextValidIBlock(id); /* Consider next iblock to be emitted */
	defBlock = ccIBlockDefaultTag(riblock);
	if ((id == IBlockCount - 1) || (defBlock != nextib)) {
		assert(ccIsObjectTypeIBlock(defBlock));
		ccEmitOpcode2(vmBRA, false); /* Emit jump opcode */
		codeBlockOffset = pccode - 1;
		memVectorSet(rcodenew, codeBlockOffset, defBlock);
		ccIBlockDefaultTagSet(riblock, (Obj)codeBlockOffset);
	}
ret:
	DBEND();
}

/* For every iblock in the igraph that has been tagged #t, icode is emitted
   to the code object.  The iblock is tagged with its address in the code
   block.  The branch field for branch instruction are also stored so when
   the offset can be determined, the branch opcode's field can be set quickly.

   riblock/r16  <= current iblock
   rcodenew/r10    <= code emitted to
   pccode       <= C var code object index
*/
void ccPlaceAllIBlocks (void) {
 Num i;
	DBBEG();
	for (i=iblockOffset; i < IBlockCount; ++i) {
		riblock = ccIBlock(i); /* Consider iblock from vector of all iblocks */

		/* This means the iblock is not connected to the igraph as it wasn't
		   recursively found when counting the igraph fields */
		if (false != ccIBlockTag(riblock)) {

			/* Translate and emit the icodes as virtual machine opcodes */
			ccEmitIblockOpcodes();

			/* Emit a jump, if the default block does not follow immediatley after.
		   	Set up the iblock to resolve the target default and conditional branch offsets by
		   	moving the iblock from the default/conditional tag to the opcode's offset field
		   	and resetting the tag to the opcode's offset field offset in the code block. */
			ccPrepareIBlockBranches();
		}
	}
	DBEND();
}


/* Resolve current iblock's jump-opcode offset field
*/
void ccResolveDefault (void) {
 Obj defBlock;
 Obj defBlockAddr;
 Num opcodeFieldAddr;
	DBBEG("      iblock="NUM, ccIBlockID(riblock));
	/* Only resolve if the default opcode index is an immediate integer.
	   False means no jump was required as its default iblock was
	   emitted after this block. */
	opcodeFieldAddr = (Num)ccIBlockDefaultTag(riblock);

	if (!memIsObjectValid((Obj)opcodeFieldAddr)) {
		/* Consider default block and it's address in the code block*/
		defBlock = memVectorObject(rcodenew, opcodeFieldAddr);
		assert(false != defBlock);
		defBlockAddr = ccIBlockTag(defBlock);
		assert(true != defBlockAddr); /* If it wasn't placed, it would be tagged #t */
 		/* Set the jump-opcode's offset */
		memVectorSet(rcodenew, opcodeFieldAddr, (Obj)(((Int)defBlockAddr-(Int)opcodeFieldAddr-1)*8));
		ccIBlockDefaultTagSet(riblock, defBlock); /* Set default tag back to target iblock */
	}
	DBEND();
}

/* Resolve current iblock's branch-opcode offset field
*/
void ccResolveConditional (void) {
 Obj condBlock;
 Obj condBlockAddr;
 Num opcodeFieldAddr;
	DBBEG("  iblock="NUM, ccIBlockID(riblock));
	/* Only resolve if the conditional opcode index is an immediate integer.
	   False means no final branch op at the end of the block */
	opcodeFieldAddr = (Num)ccIBlockConditionalTag(riblock);

	if (!memIsObjectValid((Obj)opcodeFieldAddr)) {
		/* Consider default block and it's address in the code block*/
		condBlock = memVectorObject(rcodenew, opcodeFieldAddr);
		assert(false != condBlock);
		condBlockAddr = ccIBlockTag(condBlock);
		assert(true != condBlockAddr); /* If it wasn't placed, it would be tagged #t */
 		/* Set the jump-opcode's offset */
		memVectorSet(rcodenew, opcodeFieldAddr, (Obj)(((Int)condBlockAddr-(Int)opcodeFieldAddr-1)*8));
		ccIBlockConditionalTagSet(riblock, condBlock); /* Set conditional tag back to target iblock */
	}
	DBEND();
}

/* Assumes default and conditional tags are code block offsets to
   branch instructions which temporarily have the branch field set
   to the target iblock. */
void ccResolveBranchOpcodeAddresses (void) {
 Num i;
	DBBEG();
	/* Resolve branch offsets for the current iblock segment */
	for (i=iblockOffset; i < IBlockCount; ++i) {
		riblock = ccIBlock(i); /* Consider iblock from vector of all iblocks */

		/* This means the iblock is not connected to the igraph as it wasn't
		   recursively found when counting the igraph fields */
		if (false != ccIBlockTag(riblock)) {
			/* Resolve my branch opcode offsets */
			ccResolveDefault();
			ccResolveConditional();
		}
	}
//ccDumpIBlock(riblock);
//vmDebugDumpCode(rcodenew, stderr);
	DBEND();
}


/* Set the default/conditional tags to an actual iblock.
   Also add this iblock to the child's incoming list.
*/
void ccIBlockLinkDefault (Num IDparent, Num IDchild) {
 Obj parentib, childib;

	parentib = ccIBlock(IDparent);
	childib = ccIBlock(IDchild);
	assert(ccIsObjectTypeIBlock(parentib));
	assert(ccIsObjectTypeIBlock(childib));

	ccIBlockDefaultTagSet(parentib, childib);
	ccIBlockIncomingListAdd(childib, parentib);
}

void ccIBlockLinkConditional (Num IDparent, Num IDchild) {
 Obj parentib, childib;

 	parentib = ccIBlock(IDparent);
 	childib = ccIBlock(IDchild);
	assert(ccIsObjectTypeIBlock(parentib));
	assert(ccIsObjectTypeIBlock(childib));

	ccIBlockConditionalTagSet(parentib, childib);
	ccIBlockIncomingListAdd(childib, parentib);
}

/* Initialize iblock default and conditional tags with their child iblocks (if it has them)
    r4 = temp 
*/
void ccInitIBlockBranchTagsToIBlocks (Obj ib) {
 Obj tag;
	DBBEG();
	DBE ccDumpIBlock(ib);

	r4 = ib; /* TODO this iblock being passed around in C land is incorrect.  Use a register instead. */
	tag = ccIBlockDefaultTag(r4);
	if (false != tag) {
		if (true == tag) {
			/* Default block is via 'next logical' */
			ccIBlockLinkDefault(ccIBlockID(r4), 1 + ccIBlockID(r4)); 
		} else if (!ccIsObjectTypeIBlock(tag)) { /* Could already be connected if non ASM flow */
			/* Default block is via 'labeled block' */
			ccIBlockLinkDefault(ccIBlockID(r4), ccLabels((Num)tag));
		}
	}

	tag = ccIBlockConditionalTag(r4);
	if (false != tag) {
		/* Conditional block is a labeled block */
		if (!ccIsObjectTypeIBlock(tag)) { /* Could already be connected if non ASM flow */
			ccIBlockLinkConditional(ccIBlockID(r4), ccLabels((Num)tag)); 
		}
	}
	DBEND();
}


void compSetIBlockICodeToNOP (Obj ib, Num icidx) {
 Obj ic;
	assert(ccIsObjectTypeIBlock(ib));

	ic = ccIBlockICode(riblock, icidx);
	assert(ccIsObjectTypeICode(ic));

	memVectorSet(ic, 0, NOP);
}

/* riblock <= iblock
     start <= index
       reg <= register field
 Look for "push reg" icode in iblock in riblock starting at icode start,
 skipping nop and instructions that don't use the same register.
*/
Num compOptimizePeepHolePopPushFindMatchingPush(Num start, Obj reg) {
 Obj ic;
 Num i;
 Obj field0, field1, field2, field3;

	for (i=start; i<ccIBlockICodeLength(riblock); ++i) {

		/* Consider next instruction and its fields */
		ic = ccIBlockICode(riblock, i);
		field0 = asmICodeField(ic, 0);
		field1 = asmICodeField(ic, 1);
		field2 = asmICodeField(ic, 2);
		field3 = asmICodeField(ic, 3);

		/* Found a matching push so stop looking and succeed passing index back */
		if ((PUSH == field0) && (reg == field1))
			return i;

		/* Found an instruction that alters the stack so stop looking and fail*/
		if ((PUSH == field0) || (POP == field0))
			return 0;

		/* Found instruction that requires register, stop looking and fail */
		if ((reg == field1) || (reg == field2) || (reg == field3))
			return 0;
	}

	return 0;
}

/* riblock <= iblock to optimize
   return => optimization performed
*/
void compOptimizePeepHolePopPush(void) {
 Obj ic;
 Num idx, i, j;
 Obj field0, field1;
	DBBEG();
	/* Consider every live iblock  TODO make iteration on igraph cleaner (and everywhere else) */
	for (i = iblockOffset; i < IBlockCount; ++i) {
		riblock = ccIBlock(i);
		if (true == ccIBlockTag(riblock)) do {
			idx = 0;
			/* Over every POP instruction */
			for (j=0; !idx && j<ccIBlockICodeLength(riblock); ++j) {
				ic = ccIBlockICode(riblock, j);
				field0 = asmICodeField(ic, 0); /* opcode */
				field1 = asmICodeField(ic, 1); /* reg 0 */
				if (POP == field0) {
					/* Find a matching push that cancels this pop */
					idx = compOptimizePeepHolePopPushFindMatchingPush(j+1, field1);
					if (idx) {
						DB("Omitting "HEX" and "HEX, j, idx);
						DBE ccDumpIBlock(riblock);
						compSetIBlockICodeToNOP(riblock, j);
						compSetIBlockICodeToNOP(riblock, idx);
					}
				}
			}
		} while (idx);
	}
	DBEND();
}

/* Incoming connections moved to outoing connections,
   outgoing connection removed, iblock invalidated.
    riblock <= Empty iblock to remove from igraph
*/
void compOptimizeEmptyIBlock(void) {
 Obj mydef, lst, inib;
	if ((true == ccIBlockTag(riblock)) && (0 == ccIBlockICodeLength(riblock))) {
		DB("Found empty iblock:");

		assert(false == ccIBlockConditionalTag(riblock)); /* Verify my empty conditional iblock */
		/* Consider default iblock */
		mydef = ccIBlockDefaultTag(riblock);

		/* Make sure my default tag is an iblock and not myself */
		assert(ccIsObjectTypeIBlock(mydef));

		if (riblock == mydef) {
			DB("Skipping empty blocks that jump to themselves");
			goto ret;
		}
		DBE compDumpIBlockParentAndChildren(riblock);

		/* Remove myself from default iblock */
		ccIBlockIncomingListDel(mydef, riblock);
		/* Set incoming blocks' default and/or conditional block to my default */
		lst = ccIBlockIncomingList(riblock);
		assert(objIsPair(lst)); /* It's guaranteed to have an incoming list otherwise it wouldn't be tagged #t */
		while (null != lst) {
			inib = car(lst); /* Consider an incoming block */
			if (riblock == ccIBlockDefaultTag(inib)) ccIBlockLinkDefault(ccIBlockID(inib), ccIBlockID(mydef));
			if (riblock == ccIBlockConditionalTag(inib)) ccIBlockLinkConditional(ccIBlockID(inib), ccIBlockID(mydef));
			lst = cdr(lst);
		}
		ccIBlockTagSet(riblock, false); /* Now invalidate this now unused iblock */

		DB("The result:");
		DBE compDumpIBlockParentAndChildren(riblock);
	}
ret:
	return;
}

void compOptimizeEmptyIBlocks(void) {
 Num i;
	DBBEG();
	/* Consider every live iblock  TODO make iteration on igraph cleaner (and everywhere else) */
	for (i = iblockOffset; i < IBlockCount; ++i) {
		riblock = ccIBlock(i);
		compOptimizeEmptyIBlock();
	}
	DBEND();
}

/* riblock = temp
*/
void asmPeepHoleOptimization (void) {
	DBBEG();
	compOptimizePeepHolePopPush();
	compOptimizeEmptyIBlocks();
	DBEND();
}


/* Recursively traverse the igraph's iblocks.  Tag each with #t.
   Also resolve default and conditional branch tags.
*/
void asmPrepareIGraph (Obj ib) {
	/* Base case.  Not an iblock or the iblock has been traversed already (tagged with #t) */
	if (!ccIsObjectTypeIBlock(ib) || true == ccIBlockTag(ib))
		return;

	ccIBlockTagSet(ib, true); /* Tag iblock #t */

	vmPush(ib);
	ccInitIBlockBranchTagsToIBlocks(ib);
	ib = vmPop();

	vmPush(ib);
	asmPrepareIGraph(ccIBlockDefaultTag(ib));
	ib = vmPop();
	asmPrepareIGraph(ccIBlockConditionalTag(ib));
}


Num asmCountIGraphFields (void) {
 Obj ib, dib, icode, nextib;
 Num i, j, len=0;

	for (i=iblockOffset; i < IBlockCount; ++i) {
		ib = ccIBlock(i); /* Consider iblock from vector of all iblocks */
		/* This means the iblock is not connected to the igraph as it wasn't
		   recursively found when counting the igraph fields */
		if (true == ccIBlockTag(ib)) {
			/* Count the number of fields in each icode in this iblock */
			for (j=0; j<ccIBlockICodeLength(ib); ++j) {
				icode = ccIBlockICode(ib, j);
				len += asmICodeOpcodeSize(icode);
			}
			/* Include room for a branch if the default iblock does not come after this iblock */
			dib = ccIBlockDefaultTag(ib);
			nextib = asmNextValidIBlock(ccIBlockID(ib)); /* Consider next iblock to be emitted */
			if (ccIsObjectTypeIBlock(dib) && dib != nextib)
				len += 2;
		}
	}
	return len;
}

void asmOptimizeIGraph (void) {
	asmPeepHoleOptimization();
}

/* The IGraph's iblocks are found in riblocks.  The icode found in each iblock
   and the links between icodes, are assembled into a VM code object object
   rcodenew/r10 which can be run in the VM.

	Also restores previous ASM context
*/
void asmAsmIGraph (void) {
 Num len;
	DBBEG("  ICodeCount="NUM"   iblockOffset="NUM, ICodeCount, iblockOffset);

	/* Might have to create one more last iblock with the remaining new icodes */
	if (icodeOffset < ICodeCount) ccGenerateIBlockWithPushedIcodes();

	assert((0 < IBlockCount) && "There are no iblocks to assemble");

	/* Create the code block object which all iblocks are compile to */
	asmPrepareIGraph(ccIBlock(iblockOffset));
	//ccDumpIBlocks();
	asmOptimizeIGraph();
	//ccDumpIBlocks();

	if (rdebug) ccDumpIBlocks();

	len = asmCountIGraphFields();
	if (len) {
		rcodenew = memNewVector(TCODE, len);
		pccode = 0;
		ccPlaceAllIBlocks();

		ccResolveBranchOpcodeAddresses();

		r0 = rcodenew;
		//objDump(rlabels,stdout);
		if (rdebug) vmDebugDumpCode(rcodenew, stderr);
	} else {
		r0 = false;
	}

	ccEnd();

	DBEND();
}



/*******************************************************************************
 Debugging
*******************************************************************************/
void ccDumpICodeFields (Obj ic) {
 Obj f;
	if ((Obj)NA != (f = asmICodeField(ic, 1))) { assert(f<=R1F); fprintf(stderr, " $"HEX, f); }
	if ((Obj)NA != (f = asmICodeField(ic, 2))) { assert(f<=R1F); fprintf(stderr, " $"HEX, f); }
	if ((Obj)NA != (f = asmICodeField(ic, 3))) { assert(f<=R1F); fprintf(stderr, " $"HEX, f); }
	if ((Obj)NA != (f = asmICodeField(ic, 4))) { fprintf(stderr, " "); objDump(f, stderr); fflush(stderr); }
	if ((Obj)NA != (f = asmICodeField(ic, 5))) { fprintf(stderr, " "); objDump(f, stderr); fflush(stderr); }
}
void ccDumpICode (Obj ic) {
	if (memIsObjectValid(ic)) {
		switch ((Num)asmICodeField(ic, 0)) {
			case (Num)MV  : fprintf(stderr, "mv  "); ccDumpICodeFields(ic); break;
			case (Num)MVI : fprintf(stderr, "mvi "); ccDumpICodeFields(ic); break;
			case (Num)LDI : fprintf(stderr, "ldi "); ccDumpICodeFields(ic); break;
			case (Num)LD  : fprintf(stderr, "ld  "); ccDumpICodeFields(ic); break;
			case (Num)STI : fprintf(stderr, "sti "); ccDumpICodeFields(ic); break;
			case (Num)PUSH: fprintf(stderr, "push"); ccDumpICodeFields(ic); break;
			case (Num)POP : fprintf(stderr, "pop "); ccDumpICodeFields(ic); break;
			case (Num)ADDI: fprintf(stderr, "addi"); ccDumpICodeFields(ic); break;
			case (Num)BLTI: fprintf(stderr, "blti"); ccDumpICodeFields(ic); break;
			case (Num)BEQI: fprintf(stderr, "beqi"); ccDumpICodeFields(ic); break;
			case (Num)BNEI: fprintf(stderr, "bnei"); ccDumpICodeFields(ic); break;
			case (Num)BRTI: fprintf(stderr, "brti"); ccDumpICodeFields(ic); break;
			case (Num)BNTI: fprintf(stderr, "bnti"); ccDumpICodeFields(ic); break;
			case (Num)BRA : fprintf(stderr, "bra "); ccDumpICodeFields(ic); break;
			case (Num)JMP : fprintf(stderr, "jmp "); ccDumpICodeFields(ic); break;
			case (Num)JAL : fprintf(stderr, "jal "); ccDumpICodeFields(ic); break;
			case (Num)RET : fprintf(stderr, "ret");                          break;
			case (Num)SYS : fprintf(stderr, "sys "); ccDumpICodeFields(ic); break;
			case (Num)SYSI: fprintf(stderr, "sysi"); ccDumpICodeFields(ic); break;
			case (Num)NOP : fprintf(stderr, "nop");                          break;
			case (Num)QUIT: fprintf(stderr, "quit");                         break;
			default:
				fprintf(stderr, "**UNKNOWN OPCODE**");
				objDump(ic, stderr);
		}
	}
}

void ccDumpIBlock (Obj ib) {
 Num i;
 Obj o, block;
	assert(ccIsObjectTypeIBlock(ib));
	/* ID */
	fprintf(stderr, "\n#<"HEX03"  ", ccIBlockID(ib));

	/* Tag */
	o = ccIBlockTag(ib);
	if (false == o)
		fprintf(stderr, "---");
	else
		fprintf(stderr, HEX04, ccIBlockTag(ib));

	/* Default block */
	block = ccIBlockDefaultTag(ib);
	if (false==block)
		fprintf(stderr, "  [---]");
	else {
		fprintf (stderr, "  [");
		if (ccIsObjectTypeIBlock(block)) fprintf(stderr, HEX03, ccIBlockID(block));
		else objDump(block, stderr);
		fprintf (stderr, "]");
	}

	/* Conditional block */
	block = ccIBlockConditionalTag(ib);
	if (false==block)
		fprintf(stderr, "  [---]");
	else {
		fprintf (stderr, "  [");
		if (ccIsObjectTypeIBlock(block)) fprintf(stderr, HEX03, ccIBlockID(block));
		else objDump(block, stderr);
		fprintf (stderr, "]");
	}
	/* Incoming block IDs */
	fprintf(stderr, "  (");
	for (o = ccIBlockIncomingList(ib); null != o; ) {
		fprintf(stderr, HEX03, ccIBlockID(car(o)));
		o = cdr(o);
		if (null != o) fprintf(stderr, " ");
	}
	fprintf(stderr, ")");
	/* Code */
	for (i=0; i<ccIBlockICodeLength(ib); ++i) {
		fprintf(stderr, "\n  "HEX02"  ", i);
		ccDumpICode(ccIBlockICode(ib, i));
	}
	fprintf (stderr, ">");
}

void ccDumpIBlocks (void) {
 Num i;
 int fl;
	/* Temporarily enable blocking I/O */
	fl = fcntl(0, F_GETFL, 0);
	fcntl (0, F_SETFL, fl&~O_NONBLOCK);

	DBBEG();

	for (i=iblockOffset; i<IBlockCount; ++i)
		ccDumpIBlock(ccIBlock(i));

	DBEND();

	fcntl (0, F_SETFL, fl);
}

void compDumpIBlockParentAndChildren (Obj ib) {
 Obj lst, last, inib, dib, cib;
	lst = ccIBlockIncomingList(ib);
	last = null;
	while (null != lst) {
		inib = car(lst); /* Consider an incoming block */
		if (last != inib) ccDumpIBlock(inib);
		last = inib;
		lst = cdr(lst);
	}
	ccDumpIBlock(ib);
	dib = ccIBlockDefaultTag(ib);
	cib = ccIBlockConditionalTag(ib);
	if (ccIsObjectTypeIBlock(dib)) ccDumpIBlock(dib);
	if (ccIsObjectTypeIBlock(cib)) ccDumpIBlock(cib);
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
		objInitialize (); /* objInitialize -> vmInitialize -> memInitialize */

		DB("Registering types");
		memTypeRegisterString(TICODE, "icode");
		memTypeRegisterString(TIBLOCK, "iblock");

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
