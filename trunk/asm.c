#define DEBUG 0
#define DB_DESC "ASM "
#include "debug.h"
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
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


/* Register aliases
*/
#define riblocklast ra /* The last iblock considered when stitching igraphs together */
#define riblock     rb /* The current iblock or most recently created */
#define riblocks    rc /* Vector of all iblocks */
#define ricodes     rd /* Vector where new icodes are initially placed before being stuffed into a new iblock */
#define rlabels     re /* Vector of blocks indexed by its assembly label value */
#define rexpr       rf /* Expression being compiled.  See vm.h */
#define rcodenew    r11 /* Where a new code block is created and emitted to */

/* Object types used by compiler
*/
#define TICODE  0x87l
#define TIBLOCK 0x88l
Num ccIsObjectTypeICode  (Obj ic) { return memIsObjectType(ic, TICODE); }
Num ccIsObjectTypeIBlock (Obj ib) { return memIsObjectType(ib, TIBLOCK); }

/*******************************************************************************
 I_Code

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

Num ccICodeFieldLength (Obj ic) {
	assert(ccIsObjectTypeICode(ic));
	return memObjectLength(ic);
}

Obj ccICodeField(Obj ic, Num i) {
	assert(i < ccICodeFieldLength(ic));
	return memVectorObject(ic, i);
}
	

void ccICodePushNewMV (Obj rega, Obj regb) {
	r0 = memNewVector(TICODE, 3);
	memVectorSet(r0, 0, MV);
	memVectorSet(r0, 1, (Obj)rega);
	memVectorSet(r0, 2, (Obj)regb);
	ccICodePush(r0);
}

void ccICodePushNewMVI (Obj r, Obj i) {
	r0 = memNewVector(TICODE, 3);
	memVectorSet(r0, 0, MVI);
	memVectorSet(r0, 1, (Obj)r);
	memVectorSet(r0, 2, i);
	ccICodePush(r0);
}

void ccICodePushNewLDI (Obj rega, Obj regb, Obj i) {
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, LDI);
	memVectorSet(r0, 1, (Obj)rega);
	memVectorSet(r0, 2, (Obj)regb);
	memVectorSet(r0, 3, i);
	ccICodePush(r0);
}

void ccICodePushNewLD (Obj rega, Obj regb, Obj regc) {
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, LD);
	memVectorSet(r0, 1, (Obj)rega);
	memVectorSet(r0, 2, (Obj)regb);
	memVectorSet(r0, 3, (Obj)regc);
	ccICodePush(r0);
}

void ccICodePushNewSTI (Obj rega, Obj regb, Obj i) {
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, STI);
	memVectorSet(r0, 1, (Obj)rega);
	memVectorSet(r0, 2, (Obj)regb);
	memVectorSet(r0, 3, i);
	ccICodePush(r0);
}

void ccICodePushNewST (Obj rega, Obj regb, Obj regc) {
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, ST);
	memVectorSet(r0, 1, (Obj)rega);
	memVectorSet(r0, 2, (Obj)regb);
	memVectorSet(r0, 3, (Obj)regc);
	ccICodePush(r0);
}

void ccICodePushNewPUSH (Obj r) {
	r0 = memNewVector(TICODE, 2);
	memVectorSet(r0, 0, PUSH);
	memVectorSet(r0, 1, r);
	ccICodePush(r0);
}

void ccICodePushNewPOP (Obj r) {
	r0 = memNewVector(TICODE, 2);
	memVectorSet(r0, 0, POP);
	memVectorSet(r0, 1, r);
	ccICodePush(r0);
}

void ccICodePushNewADDI (Obj r, Obj i) {
	r0 = memNewVector(TICODE, 3);
	memVectorSet(r0, 0, ADDI);
	memVectorSet(r0, 1, (Obj)r);
	memVectorSet(r0, 2, i);
	ccICodePush(r0);
}

void ccICodePushNewBLTI (Obj r, Obj i, Obj l) {
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, BLTI);
	memVectorSet(r0, 1, r);
	memVectorSet(r0, 2, i);
	memVectorSet(r0, 3, l);
	ccICodePush(r0);
}

void ccICodePushNewBEQI (Obj rega, Obj i, Obj l) {
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, BEQI);
	memVectorSet(r0, 1, rega);
	memVectorSet(r0, 2, i);
	memVectorSet(r0, 3, l);
	ccICodePush(r0);
}

void ccICodePushNewBNEI (Obj r, Obj i, Obj l) {
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, BNEI);
	memVectorSet(r0, 1, (Obj)r);
	memVectorSet(r0, 2, i);
	memVectorSet(r0, 3, l);
	ccICodePush(r0);
}

void ccICodePushNewBRTI (Obj rega, Obj t, Obj l) {
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, BRTI);
	memVectorSet(r0, 1, (Obj)rega);
	memVectorSet(r0, 2, t);
	memVectorSet(r0, 3, l);
	ccICodePush(r0);
}

void ccICodePushNewBNTI (Obj rega, Obj t, Obj l) {
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, BNTI);
	memVectorSet(r0, 1, (Obj)rega);
	memVectorSet(r0, 2, t);
	memVectorSet(r0, 3, l);
	ccICodePush(r0);
}

void ccICodePushNewBRA (Obj l) {
	r0 = memNewVector(TICODE, 2);
	memVectorSet(r0, 0, BRA);
	memVectorSet(r0, 1, l);
	ccICodePush(r0);
}

void ccICodePushNewJ (Obj r) {
	r0 = memNewVector(TICODE, 2);
	memVectorSet(r0, 0, JMP);
	memVectorSet(r0, 1, r);
	ccICodePush(r0);
}

void ccICodePushNewJAL (Obj r) {
	r0 = memNewVector(TICODE, 2);
	memVectorSet(r0, 0, JAL);
	memVectorSet(r0, 1, r);
	ccICodePush(r0);
}

void ccICodePushNewRET () {
	r0 = memNewVector(TICODE, 1);
	memVectorSet(r0, 0, RET);
	ccICodePush(r0);
}

void ccICodePushNewSYS (Obj r) {
	r0 = memNewVector(TICODE, 2);
	memVectorSet(r0, 0, SYS);
	memVectorSet(r0, 1, r);
	ccICodePush(r0);
}

void ccICodePushNewSYSI (Obj o) {
	vmPush(o);
	r0 = memNewVector(TICODE, 2);
	memVectorSet(r0, 0, SYSI);
	memVectorSet(r0, 1, vmPop());
	ccICodePush(r0);
}

void ccICodePushNewNOP (void) {
	r0 = memNewVector(TICODE, 1);
	memVectorSet(r0, 0, NOP);
	ccICodePush(r0);
}

void ccICodePushNewQUIT (void) {
	r0 = memNewVector(TICODE, 1);
	memVectorSet(r0, 0, QUIT);
	ccICodePush(r0);
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

Num ccIBlockID (Obj ib)                  { return (Num)memVectorObject(ib, IBLOCK_INDEX_ID); }
Obj ccIBlockTag (Obj ib)                 { return memVectorObject(ib, IBLOCK_INDEX_TAG); }
Obj ccIBlockGetDefaultBlock (Obj ib)     { return memVectorObject(ib, IBLOCK_INDEX_DEFAULT); }
Obj ccIBlockGetConditionalBlock (Obj ib) { return memVectorObject(ib, IBLOCK_INDEX_CONDITIONAL); }
Obj ccIBlockIncomingList (Obj ib)        { return memVectorObject(ib, IBLOCK_INDEX_INCOMING); }

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

void ccIBlockSetTag (Obj ib, Obj tag) {
	memVectorSet(ib, IBLOCK_INDEX_TAG, tag);
}

void ccIBlockSetDefaultTag (Obj ib, Obj tag) {
	memVectorSet(ib, IBLOCK_INDEX_DEFAULT, tag);
}

void ccIBlockSetConditionalTag (Obj ib, Obj tag) {
	memVectorSet(ib, IBLOCK_INDEX_CONDITIONAL, tag);
}

void ccIBlockAddIncoming (Obj ib, Obj in) {
	r1 = in;
	r2 = memVectorObject(ib, IBLOCK_INDEX_INCOMING);
	objCons12();
	memVectorSet(r1, IBLOCK_INDEX_INCOMING, r0);
}

void ccIBlockSetICode (Num offset, Obj op) {
	assert(offset < memObjectLength(riblock));
	memVectorSet(riblock, IBLOCK_INDEX_ICODE+offset, op);
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

	ccIBlockSetDefaultTag(parentib, childib);
	ccIBlockAddIncoming (childib, parentib);
}

void ccIBlockLinkConditional (Num IDparent, Num IDchild) {
 Obj parentib, childib;

 	parentib = ccIBlock(IDparent);
 	childib = ccIBlock(IDchild);
	assert(ccIsObjectTypeIBlock(parentib));
	assert(ccIsObjectTypeIBlock(childib));

	ccIBlockSetConditionalTag(parentib, childib);
	ccIBlockAddIncoming(childib, parentib);
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
Num icodeOffset=0;
Num iblockOffset=0;

/* Begin a new ASM context saving the current context state on the stack
*/
void asmStart (void) {
	vmPush((Obj)icodeOffset);
	vmPush((Obj)iblockOffset);
	vmPush((Obj)riblock);
	vmPush((Obj)riblocklast);

	riblocklast = 0;
	riblock = 0;
	icodeOffset = ICodeCount;
	iblockOffset = IBlockCount;
}

/* Reset ASM context from stack along with some sanity assertions
*/
void ccEnd (void) {
	IBlockCount = iblockOffset;

	riblocklast = vmPop();
	assert(riblocklast == 0 || ccIsObjectTypeIBlock(riblocklast));

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
	riblocklast = 0;

	IBlockCount = 0;

	assert(0 == ICodeCount); /* Check we're not initializing the assembler in the middle of assembly */
	ICodeCount = 0;

	LabelsCount = 0;

	asmStart();

	DBEND();
}


/* Create a new iblock containing the most recent set of icodes created.
   Called by ccAsmAsm()
*/
void ccGenerateIBlockWithPushedIcodes () {
 Obj ic;
	/* Create new empty iblock in riblock and 'pop' icodes from stack into it */
	ccGenerateNewIBlock(ICodeCount - icodeOffset);
	while (icodeOffset < ICodeCount) {
		ic = ccICodePop(); /* This decrement ICodeCount */
		ccIBlockSetICode(ICodeCount - icodeOffset, ic);
	}

	/* Connect the 'first' iblock generated by the current ASM call to
	   to the last iblock generated by the last call to ASM */
	if (riblocklast
	    && ccIBlockGetDefaultBlock(riblocklast) == false) {
		ccIBlockSetDefaultTag(riblocklast, riblock);
	}

	riblocklast = riblock;
}

/* Generates icode objects and continually adds to a set which are
   eventually added to the next new iblock.
*/
void asmAsmInternal (Obj f, ...) {
 va_list ap;
 Obj obj, r, rr, rrr, i, o, l; /* r,rr=reg  i=index  o=obj  l=label */
	DBBEG("  ICodeCount "NUM"   IBlockCount "NUM, ICodeCount, IBlockCount);

	/* Parse the opcode fields and create then push an icode object */
	for (va_start(ap, f), obj = f; (obj != END); obj = va_arg(ap, Obj)) {
		if        (MV == obj) {
			r  = va_arg(ap, Obj);
			rr = va_arg(ap, Obj);
			DB("mv ["HEX" "HEX" "HEX"]", obj, r, rr);
			ccICodePushNewMV(r, rr);
		} else if (MVI == obj) {
			r = va_arg(ap, Obj);
			o = va_arg(ap, Obj);
			DB("mvi ["HEX" "HEX" "HEX"]", obj, r, o);
			ccICodePushNewMVI(r, o);
		} else if (LDI == obj) {
			r = va_arg(ap, Obj);
			rr = va_arg(ap, Obj);
			o = va_arg(ap, Obj);
			DB("ldi ["HEX" "HEX" "HEX"]", r, rr, o);
			ccICodePushNewLDI(r, rr, o);
		} else if (LD == obj) {
			r = va_arg(ap, Obj);
			rr = va_arg(ap, Obj);
			rrr = va_arg(ap, Obj);
			DB("ld ["HEX" "HEX" "HEX"]", r, rr, rrr);
			ccICodePushNewLD(r, rr, rrr);
		} else if (STI == obj) {
			r = va_arg(ap, Obj);
			rr = va_arg(ap, Obj);
			o = va_arg(ap, Obj);
			DB("sti ["HEX" "HEX" "HEX"]", r, rr, o);
			ccICodePushNewSTI(r, rr, o);
		} else if (ST == obj) {
			r = va_arg(ap, Obj);
			rr = va_arg(ap, Obj);
			rrr = va_arg(ap, Obj);
			DB("st ["HEX" "HEX" "HEX"]", r, rr, rrr);
			ccICodePushNewST(r, rr, rrr);
		} else if (PUSH == obj) {
			o = va_arg(ap, Obj);
			DB("push["HEX"]", o);
			ccICodePushNewPUSH(o);
		} else if (POP == obj) {
			o = va_arg(ap, Obj);
			DB("pop ["HEX"]", o);
			ccICodePushNewPOP(o);
		} else if (ADDI == obj) {
			r = va_arg(ap, Obj);
			o = va_arg(ap, Obj);
			DB("addi["HEX" "HEX"]", r, o);
			ccICodePushNewADDI(r, o);
		} else if (BLTI == obj) {
			r = va_arg(ap, Obj);
			i = va_arg(ap, Obj);
			l = va_arg(ap, Obj);
			DB("blti["HEX" "HEX" "HEX"]", r, i, l);
			ccICodePushNewBLTI(r, i, l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */
			ccIBlockSetConditionalTag(riblock, l);  /* signal this block conditional is a label */
		} else if (BEQI == obj) {
			r = va_arg(ap, Obj);
			i = va_arg(ap, Obj);
			l = va_arg(ap, Obj);
			DB("beqi["HEX" "HEX" "HEX"]", r, i, l);
			ccICodePushNewBEQI(r, i, l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */
			ccIBlockSetConditionalTag(riblock, l);  /* signal this block conditional is a label */
		} else if (BNEI == obj) {
			r = va_arg(ap, Obj);
			i = va_arg(ap, Obj);
			l = va_arg(ap, Obj);
			DB("bnei["HEX" "HEX" "HEX"]", r, i, l);
			ccICodePushNewBNEI(r, i, l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */
			ccIBlockSetConditionalTag(riblock, l);  /* signal this block conditional is a label */
		} else if (BRTI == obj) {
			r = va_arg(ap, Obj);
			i = va_arg(ap, Obj);
			l = va_arg(ap, Obj);
			DB("brti["HEX" "HEX" "HEX"]", r, i, l);
			ccICodePushNewBRTI(r, i, l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */
			ccIBlockSetConditionalTag(riblock, l);  /* signal this block conditional is a label */
		} else if (BNTI == obj) {
			r = va_arg(ap, Obj);
			i = va_arg(ap, Obj);
			l = va_arg(ap, Obj);
			DB("bnti["HEX" "HEX" "HEX"]", r, i, l);
			ccICodePushNewBNTI(r, i, l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */
			ccIBlockSetConditionalTag(riblock, l);  /* signal this block conditional is a label */
		} else if (BRA == obj) {
			l = va_arg(ap, Obj);
			DB("bra ["HEX"]", l);
			ccICodePushNewBRA(l);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockSetDefaultTag(riblock, l);  /* signal this block's default is a label */
		} else if (JMP == obj) {
			r = va_arg(ap, Obj);
			DB("j   ["HEX"]", r);
			ccICodePushNewJ(r);
			ccGenerateIBlockWithPushedIcodes();
			// no default block after a jump
		} else if (JAL == obj) {
			r = va_arg(ap, Obj);
			DB("jal ["HEX"]", r);
			ccICodePushNewJAL(r);
			ccGenerateIBlockWithPushedIcodes();
			ccIBlockSetDefaultTag(riblock, true);  /* default block is next */
		} else if (RET == obj) {
			DB("ret []");
			ccICodePushNewRET();
			ccGenerateIBlockWithPushedIcodes();
			// no default block after a ret
		} else if (SYS == obj) {
			r = va_arg(ap, Obj);
			DB("sys ["HEX"]",r);
			ccICodePushNewSYS(r);
		} else if (SYSI == obj) {
			o = va_arg(ap, Obj);
			DB("sysi["HEX"]", o);
			ccICodePushNewSYSI(o);
		} else if (NOP == obj) {
			DB("nop[]");
			ccICodePushNewNOP();
		} else if (QUIT == obj) {
			DB("quit[]");
			ccICodePushNewQUIT();
		} else if (LABEL == obj) {
			l = va_arg(ap, Obj);
			DB("label["HEX"]", l);
			if (0 < ICodeCount) {
				ccGenerateIBlockWithPushedIcodes();
				ccIBlockSetDefaultTag(riblock, true);  /* default block is next */
			}
			/* Set the next block's ID in the label/iblockID table */
			ccLabelsSet((Num)l, IBlockCount);
		} else {
			DB("["HEX"]", obj);
			assert(!"Unhandled asm opcode");
		}
	}
	va_end(ap); /* stdarg */

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
	memVectorSet(rcodenew, pccode++, op);
}

void ccEmitOpcode2 (Obj op1, Obj op2) {
	memVectorSet(rcodenew, pccode++, op1);
	memVectorSet(rcodenew, pccode++, op2);
}

void ccEmitIblockOpcodes (void) {
 Num i;
 Obj field0, field1, field2, field3;
	DBBEG("      iblock="NUM, ccIBlockID(riblock));

	/* Re-tag the iblock with its initial location in the code block */
	ccIBlockSetTag (riblock, (Obj)pccode);

	for (i=0; i<ccIBlockICodeLength(riblock); ++i) {
		r0 = ccIBlockICode(riblock, i); /* Consider icode object in r0 */
		field0 = ccICodeField(r0, 0);
		DB("field0 = "HEX, field0);
		switch ((Num)field0) {
		case (Num)MV:
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				               case (Num)R1 : ccEmitOpcode(vmMV01); break;
				               case (Num)R3 : ccEmitOpcode(vmMV03); break;
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
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmMVI0); break;
				case (Num)R1 : ccEmitOpcode(vmMVI1); break;
				case (Num)R2 : ccEmitOpcode(vmMVI2); break;
				case (Num)R3 : ccEmitOpcode(vmMVI3); break;
				case (Num)R4 : ccEmitOpcode(vmMVI4); break;
				case (Num)R6 : ccEmitOpcode(vmMVI6); break;
				case (Num)R7 : ccEmitOpcode(vmMVI7); break;
				default : assert(!"Unsuported field MVI ?reg? imm"); }
			ccEmitOpcode(field2);
			break;
		case (Num)LDI:
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			field3 = ccICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				          case (Num)R0 : ccEmitOpcode(vmLDI00); break;
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
			ccEmitOpcode(field3);
			break;
		case (Num)LD:
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			field3 = ccICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				               case (Num)R1 : switch ((Num)field3) {
				                              case (Num)R2 : ccEmitOpcode(vmLD012); break;
				                              default : assert(!"Unsuported field LD $0 $1 ?reg?"); } break;
				               default : assert(!"Unsuported field LD $0 ?reg? reg"); } break;
				default : assert(!"Unsuported field LD ?reg? reg reg"); }
			break;
		case (Num)STI:
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			field3 = ccICodeField(r0, 3);
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
			ccEmitOpcode(field3);
			break;
		case (Num)ST:
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			field3 = ccICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R0 : switch ((Num)field2) {
				               case (Num)R1 : switch ((Num)field3) {
				                              case (Num)R2 :  ccEmitOpcode(vmST012); break;
				                              default : assert(!"Unsuported field ST $0 $1 ?reg?"); } break;
				               default : assert(!"Unsuported field ST $0 ?reg? reg"); } break;
				default : assert(!"Unsuported field ST ?reg? reg reg"); }
			break;
		case (Num)PUSH:
			field1 = ccICodeField(r0, 1);
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
			field1 = ccICodeField(r0, 1);
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
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmADDI0); break;
				case (Num)R1 : ccEmitOpcode(vmADDI1); break;
				case (Num)R2 : ccEmitOpcode(vmADDI2); break;
				default : assert(!"Unsuported field ADDI ?reg? imm"); }
			ccEmitOpcode(field2);
			break;
		case (Num)BLTI:
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			//field3 = ccICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R1 : ccEmitOpcode(vmBLTI1); break;
				default : assert(!"Unsuported field BLTI ?reg? imm offset"); }
			ccEmitOpcode(field2);
			ccEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BEQI:
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			//field3 = ccICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmBEQI0); break;
				case (Num)R1 : ccEmitOpcode(vmBEQI1); break;
				case (Num)R7 : ccEmitOpcode(vmBEQI7); break;
				default : assert(!"Unsuported field BEQI ?reg? imm offset"); }
			ccEmitOpcode(field2);
			ccEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BNEI:
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			//field3 = ccICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmBNEI0); break;
				case (Num)R1 : ccEmitOpcode(vmBNEI1); break;
				case (Num)R2 : ccEmitOpcode(vmBNEI2); break;
				case (Num)R5 : ccEmitOpcode(vmBNEI5); break;
				default : assert(!"Unsuported field BNEI ?reg? imm offset"); }
			ccEmitOpcode(field2);
			ccEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BRTI:
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			//field3 = ccICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmBRTI0); break;
				default : assert(!"Unsuported field BRTI ?reg? imm offset"); }
			ccEmitOpcode(field2);
			ccEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BNTI:
			field1 = ccICodeField(r0, 1);
			field2 = ccICodeField(r0, 2);
			//field3 = ccICodeField(r0, 3);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmBNTI0); break;
				default : assert(!"Unsuported field BNTI ?reg? imm offset"); }
			ccEmitOpcode(field2);
			ccEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BRA :
			/* Emitted by parent logic */
			break;
		case (Num)JMP :
			field1 = ccICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmJ0); break;
				case (Num)R2 : ccEmitOpcode(vmJ2); break;
				default : assert(!"Unsuported field JMP ?reg?"); }
			break;
		case (Num)JAL :
			field1 = ccICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmJAL0); break;
				case (Num)R2 : ccEmitOpcode(vmJAL2); break;
				default : assert(!"Unsuported field JAL ?reg?"); }
			break;
		case (Num)RET :
			ccEmitOpcode(vmRET);
			break;
		case (Num)SYS :
			field1 = ccICodeField(r0, 1);
			switch ((Num)field1) {
				case (Num)R0 : ccEmitOpcode(vmSYS0); break;
				default : assert(!"Unsuported field SYS ?imm?"); }
			break;
		case (Num)SYSI:
			field1 = ccICodeField(r0, 1);
			ccEmitOpcode2(vmSYSI, field1);
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

/* Emit a jump opcode if the iblock's default iblock will not be emitted after this
   one.  Also set the default/conditional branch field code-block offsets in the
   iblock structure.
*/
void ccPrepareIBlockBranches (void) {
 Obj defBlock, condBlock;
 Num codeBlockOffset, id;
	DBBEG("  iblock="NUM, ccIBlockID(riblock));
	/* If no default block is set then verify no conditional block either as it's
	   probably the final "quit" iblock. */
	if (false == ccIBlockGetDefaultBlock(riblock)) {
		assert(false == ccIBlockGetConditionalBlock(riblock));
		goto ret;
	}

	/* If the iblock has a conditional iblock, cache the branch opcode's offset-field location and
	   set the field value to the target iblock temporarily */
	condBlock = ccIBlockGetConditionalBlock(riblock);
	if (false != condBlock) {
		assert(ccIsObjectTypeIBlock(condBlock));
		codeBlockOffset = pccode - 1;
		memVectorSet(rcodenew, codeBlockOffset, condBlock);
		ccIBlockSetConditionalTag(riblock, (Obj)codeBlockOffset);
	}

	/* if the iblock is the last iblock in the igraph vector or the next iblock is not my default */
	id = ccIBlockID(riblock);
	defBlock = ccIBlockGetDefaultBlock(riblock);
	if ((id == IBlockCount - 1) || (defBlock != ccIBlock(id + 1))) {
		assert(ccIsObjectTypeIBlock(defBlock));
		ccEmitOpcode2(vmBRA, false); /* Emit jump opcode */
		codeBlockOffset = pccode - 1;
		memVectorSet(rcodenew, codeBlockOffset, defBlock);
		ccIBlockSetDefaultTag(riblock, (Obj)codeBlockOffset);
	}
ret:
	DBEND();
}

/* For every iblock in the igraph, icode is emitted to the code object.
   The iblock is tagged with itss address in the code block.  The branch
   field for branch instruction are also stored so when the offset can be
   determined, the branch opcode's field can be set quickly.

   riblock/r16  <= current iblock
   rcodenew/r11    <= code emitted to
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
	opcodeFieldAddr = (Num)ccIBlockGetDefaultBlock(riblock);

	if (!memIsObjectValid((Obj)opcodeFieldAddr)) {
		/* Consider default block and it's address in the code block*/
		defBlock = memVectorObject(rcodenew, opcodeFieldAddr);
		assert(false != defBlock);
		defBlockAddr = ccIBlockTag(defBlock);
		assert(true != defBlockAddr); /* If it wasn't placed, it would be tagged #t */
 		/* Set the jump-opcode's offset */
		memVectorSet(rcodenew, opcodeFieldAddr, (Obj)(((Int)defBlockAddr-(Int)opcodeFieldAddr-1)*8));
		ccIBlockSetDefaultTag(riblock, defBlock); /* Set default tag back to target iblock */
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
	opcodeFieldAddr = (Num)ccIBlockGetConditionalBlock(riblock);

	if (!memIsObjectValid((Obj)opcodeFieldAddr)) {
		/* Consider default block and it's address in the code block*/
		condBlock = memVectorObject(rcodenew, opcodeFieldAddr);
		assert(false != condBlock);
		condBlockAddr = ccIBlockTag(condBlock);
		assert(true != condBlockAddr); /* If it wasn't placed, it would be tagged #t */
 		/* Set the jump-opcode's offset */
		memVectorSet(rcodenew, opcodeFieldAddr, (Obj)(((Int)condBlockAddr-(Int)opcodeFieldAddr-1)*8));
		ccIBlockSetConditionalTag(riblock, condBlock); /* Set conditional tag back to target iblock */
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
		/* Resolve my branch opcode offsets */
		ccResolveDefault();
		ccResolveConditional();
	}
//ccDumpIBlock(riblock);
//vmDebugDumpCode(rcodenew, stderr);
	DBEND();
}


/* Initialize iblock default and conditional tags with their child iblocks (if it has them)
*/
void ccInitIBlockBranchTagsToIBlocks (Obj ib) {
 Obj tag;

	r3 = ib; /* TODO this iblock being passed around in C land is incorrect.  Use a register instead. */
	tag = ccIBlockGetDefaultBlock(r3);
	if (false != tag) {
		if (true == tag) {
			/* Default block is via 'next logical' */
			ccIBlockLinkDefault(ccIBlockID(r3), 1 + ccIBlockID(r3)); 
		} else if (!ccIsObjectTypeIBlock(tag)) { /* Could already be connected if non ASM flow */
			/* Default block is via 'labeled block' */
			ccIBlockLinkDefault(ccIBlockID(r3), ccLabels((Num)tag));
		}
	}

	tag = ccIBlockGetConditionalBlock(r3);
	if (false != tag) {
		/* Conditional block is a labeled block */
		if (!ccIsObjectTypeIBlock(tag)) { /* Could already be connected if non ASM flow */
			ccIBlockLinkConditional(ccIBlockID(r3), ccLabels((Num)tag)); 
		}
	}
}

/* Recursively traverse the igraph's iblocks.  Tag each with #t.
   Also resolve default and conditional branch tags.
*/
Num ccCountIGraphFields (Obj ib) {
 Num i, len=0;

	/* Base case.  Not an iblock or the iblock has been traversed already (tagged with #t) */
	if (!ccIsObjectTypeIBlock(ib) || true == ccIBlockTag(ib)) return 0;

	ccIBlockSetTag(ib, true);

	vmPush(ib);
	ccInitIBlockBranchTagsToIBlocks(ib);
	ib = vmPop();

	/* Count the number of fields in each icode in this iblock */
	for (i=IBLOCK_INDEX_ICODE; i<memObjectLength(ib); ++i)
		len += memObjectLength(memVectorObject(ib, i));

	vmPush(ib);
	len += ccCountIGraphFields(ccIBlockGetDefaultBlock(ib));
	ib = vmPop();
	len += ccCountIGraphFields(ccIBlockGetConditionalBlock(ib));
	return len;
}


/* The IGraph's iblocks are found in riblocks.  The icode found in each iblock
   and the links between icodes, are assembled into a VM code object object
   rcodenew/r11 which can be run in the VM.

	Also restores previous ASM context
*/
void asmAsmIGraph (void) {
 Num len;
	DBBEG();

	/* Might have to create an iblock with remaining icodes on stack */
	DB("ICodeCount="NUM"   iblockOffset="NUM, ICodeCount, iblockOffset);
	if (icodeOffset < ICodeCount) ccGenerateIBlockWithPushedIcodes();

	assert(0 < IBlockCount && "There are no iblocks to assemble");

	//ccDumpIBlocks();

	/* Create the code block object which all iblocks are compile to */
	len = ccCountIGraphFields(ccIBlock(iblockOffset));
	rcodenew = memNewVector(TCODE, len);
	pccode = 0;

	ccPlaceAllIBlocks();

	ccResolveBranchOpcodeAddresses();

	r0 = rcodenew;
//ccDumpIBlocks();
	ccEnd();
//objDump(rlabels,stdout);
//vmDebugDumpCode(rcodenew, stderr);
	DBEND();
}



/*******************************************************************************
 Debugging
*******************************************************************************/
void ccDumpICodeFieldsImm (Obj c) {
 Obj r;
	r = ccICodeField(c, 1);
	objDump(r, stderr);
}

void ccDumpICodeFieldsReg (Obj c) {
 Num r;
	r = (Num)ccICodeField(c, 1);
	assert(r <= (Num)R1F);
	fprintf(stderr, "$"HEX, r);
}

void ccDumpICodeFieldsRegReg (Obj c) {
 Num r;
	r = (Num)ccICodeField(c, 1);
	assert(r <= (Num)R1F);
	fprintf(stderr, "$"HEX" ", r);

	r = (Num)ccICodeField(c, 2);
	assert(r <= (Num)R1F);
	fprintf(stderr, "$"HEX, r);
}

void ccDumpICodeFieldsRegRegReg (Obj c) {
 Num r;
	r = (Num)ccICodeField(c, 1);
	assert(r <= (Num)R1F);
	fprintf(stderr, "$"HEX" ", r);

	r = (Num)ccICodeField(c, 2);
	assert(r <= (Num)R1F);
	fprintf(stderr, "$"HEX" ", r);

	r = (Num)ccICodeField(c, 3);
	assert(r <= (Num)R1F);
	fprintf(stderr, "$"HEX, r);
}

void ccDumpICodeFieldsRegImm (Obj c) {
 Obj r;
	r = ccICodeField(c, 1);
	assert(r <= R1F);
	fprintf(stderr, "$"HEX" ", r);

	r = ccICodeField(c, 2);
	objDump(r, stderr);
}

void ccDumpICodeFieldsRegImmImm (Obj c) {
 Obj r;
	r = ccICodeField(c, 1);
	assert(r <= R1F);
	fprintf(stderr, "$"HEX" ", r);

	r = ccICodeField(c, 2);
	objDump(r, stderr);

	fprintf(stderr, " ");
	r = ccICodeField(c, 3);
	objDump(r, stderr);
}

void ccDumpICodeFieldsRegRegImm (Obj c) {
 Num r;
	r = (Num)ccICodeField(c, 1);
	assert(r <= (Num)R1F);
	fprintf(stderr, "$"HEX" ", r);

	r = (Num)ccICodeField(c, 2);
	assert(r <= (Num)R1F);
	fprintf(stderr, "$"HEX" ", r);

	r = (Num)ccICodeField(c, 3);
	fprintf(stderr, HEX, r);
}

void ccDumpICode (Obj ic) {
	if (memIsObjectValid(ic)) {
		switch ((Num)ccICodeField(ic, 0)) {
			case (Num)MV  : fprintf(stderr, "mv   "); ccDumpICodeFieldsRegReg(ic);    break;
			case (Num)MVI : fprintf(stderr, "mvi  "); ccDumpICodeFieldsRegImm(ic);    break;
			case (Num)LDI : fprintf(stderr, "ldi  "); ccDumpICodeFieldsRegRegImm(ic); break;
			case (Num)LD  : fprintf(stderr, "ld   "); ccDumpICodeFieldsRegRegReg(ic); break;
			case (Num)STI : fprintf(stderr, "sti  "); ccDumpICodeFieldsRegRegImm(ic); break;
			case (Num)PUSH: fprintf(stderr, "push "); ccDumpICodeFieldsReg(ic);       break;
			case (Num)POP : fprintf(stderr, "pop  "); ccDumpICodeFieldsReg(ic);       break;
			case (Num)ADDI: fprintf(stderr, "addi "); ccDumpICodeFieldsRegImm(ic);    break;
			case (Num)BLTI: fprintf(stderr, "blti "); ccDumpICodeFieldsRegImmImm(ic); break;
			case (Num)BEQI: fprintf(stderr, "beqi "); ccDumpICodeFieldsRegImmImm(ic); break;
			case (Num)BNEI: fprintf(stderr, "bnei "); ccDumpICodeFieldsRegImmImm(ic); break;
			case (Num)BRTI: fprintf(stderr, "brti "); ccDumpICodeFieldsRegImmImm(ic); break;
			case (Num)BNTI: fprintf(stderr, "bnti "); ccDumpICodeFieldsRegImmImm(ic); break;
			case (Num)BRA : fprintf(stderr, "bra  "); ccDumpICodeFieldsImm(ic);       break;
			case (Num)JMP : fprintf(stderr, "j    "); ccDumpICodeFieldsReg(ic);       break;
			case (Num)JAL : fprintf(stderr, "jal  "); ccDumpICodeFieldsReg(ic);       break;
			case (Num)RET : fprintf(stderr, "ret");                             break;
			case (Num)SYS : fprintf(stderr, "sys  "); ccDumpICodeFieldsReg(ic);       break;
			case (Num)SYSI: fprintf(stderr, "sysi "); ccDumpICodeFieldsImm(ic);       break;
			case (Num)NOP : fprintf(stderr, "nop");                             break;
			case (Num)QUIT: fprintf(stderr, "quit");                            break;
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
	block = ccIBlockGetDefaultBlock(ib);
	if (false==block)
		fprintf(stderr, "  [---]");
	else {
		fprintf (stderr, "  [");
		if (ccIsObjectTypeIBlock(block)) fprintf(stderr, HEX03, ccIBlockID(block));
		else objDump(block, stderr);
		fprintf (stderr, "]");
	}

	/* Conditional block */
	block = ccIBlockGetConditionalBlock(ib);
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
	DBBEG();
	for (i=iblockOffset; i<IBlockCount; ++i)
		ccDumpIBlock(ccIBlock(i));
	DBEND();
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
		objNewVector(IBLOCK_VECTOR_SIZE);
		riblocks = r0;
		objNewVector(ICODE_VECTOR_SIZE);
		ricodes = r0;
		objNewVector(LABELS_DB_SIZE);
		rlabels = r0;
	} else {
		DB("Module already activated");
	}
	DBEND();
}



#undef DB_DESC
#undef DEBUG
