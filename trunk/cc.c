#define DEBUG 0
#define DB_DESC "CC"
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "cc.h"
#include "os.h"
#include "sys.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"

/*
TABLE OF CONTENTS
 I_Code
 Igraph_and_iblocks
 Labels
 ASM
 Assemble
 Compiler
 Debugging
 Init

TERMS
  I-Graph   Intermediate graph composed of I-blocks
  I-Block   I-graph node composed of a list of incoming iblocks, outgoing iblocks and icode statements
  I-Code    I-block statement composed of multiple code fields

DESIGN
   Expression to compile assigned to rexpr/r15
   Flow keeps track of pseudo environment in renv/r1c and used registers in flags
*/

void ccDumpICode (Obj ic);
void ccDumpIBlock (Obj ib);
void ccCompileExpr (Num flags);
void ccInitialize (void);

/* Register aliases overriding aliases in vm module
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

/* compiler flags
*/
static const Num CCTAILCALL  = (Num)0x00010000;
static const Num CCNODEFINES = (Num)0x00020000;



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
Obj ccIBlockGetDefaultBlock (Obj ib)     { return car(memVectorObject(ib, IBLOCK_INDEX_DEFAULT)); }
Obj ccIBlockGetConditionalBlock (Obj ib) { return car(memVectorObject(ib, IBLOCK_INDEX_CONDITIONAL)); }
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
 Obj condPair;
	condPair = memVectorObject(ib, IBLOCK_INDEX_DEFAULT);
	memVectorSet(condPair, 0, tag);
}

void ccIBlockSetConditionalTag (Obj ib, Obj tag) {
 Obj condPair;
	condPair = memVectorObject(ib, IBLOCK_INDEX_CONDITIONAL);
	memVectorSet(condPair, 0, tag);
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


/* Create new igraph block defaults to #(#f () #(#f #f) ...)
       r0 =  temp
       r1 =  temp
       r2 =  temp
   iblock => iblock object
   return => iblock ID
   Increments IBlockCount which is also used as the block's ID.
*/
Num ccGenerateNewIBlock (Num icodeSize) {
	riblock = memNewVector(TIBLOCK, icodeSize + IBLOCK_INDEX_ICODE);
	/* Append to igraph vector */
	memVectorSet(riblocks, IBlockCount, riblock);
	/* Tag defaults to #f */
	memVectorSet(riblock, IBLOCK_INDEX_TAG, false);
	/* Name can be anything */
	memVectorSet(riblock, IBLOCK_INDEX_ID, (Obj)IBlockCount);
	/* Incoming iblock list defaults to null */
	memVectorSet(riblock, IBLOCK_INDEX_INCOMING, null);
	/* Outgoing iblock pair defaults to #((#f.#f) (#f.#f)) */
	r1 = r2 = false;  objCons12();
	memVectorSet(riblock, IBLOCK_INDEX_DEFAULT, r0);
	r1 = r2 = false;  objCons12();
	memVectorSet(riblock, IBLOCK_INDEX_CONDITIONAL, r0);

	return IBlockCount++;
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
Obj ccNewLabel() {
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
void ccStart (void) {
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
void ccAsmInit (void) {
	DBBEG();

	riblock = 0;
	riblocklast = 0;

	IBlockCount = 0;

	assert(0 == ICodeCount); /* Check we're not initializing the assembler in the middle of assembly */
	ICodeCount = 0;

	LabelsCount = 0;

	ccStart();

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
void ccAsmAsm (Obj f, ...) {
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
	DBEND ();
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
				               default : assert(!"Unsuported icode MV $0 ??"); } break;
				case (Num)R1 : switch ((Num)field2) {
				               case (Num)R0 : ccEmitOpcode(vmMV10); break;
				               default : assert(!"Unsuported icode MV $1 ??"); } break;
				case (Num)R2 : switch ((Num)field2) {
				               case (Num)R0 : ccEmitOpcode(vmMV20); break;
				               default : assert(!"Unsuported icode MV $2 ??"); } break;
				case (Num)R3 : switch ((Num)field2) {
				               case (Num)R0 : ccEmitOpcode(vmMV30); break;
				               default : assert(!"Unsuported icode MV $3 ??"); } break;
				case (Num)R5 : switch ((Num)field2) {
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
void ccAsmIGraph (void) {
 Num len;
	DBBEG();

	/* Might have to create an iblock with remaining icodes on stack */
	DB("ICodeCount="NUM"   iblockOffset="NUM, ICodeCount, iblockOffset);
	if (icodeOffset < ICodeCount) ccGenerateIBlockWithPushedIcodes();

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
//sysDisplay(rlabels,stdout);
//vmDebugDumpCode(rcodenew, stderr);
	DBEND();
}



/*******************************************************************************
 Compiler

 Compile a scheme expression into a VM code block.  Calls ASM and Assemble
 functions in this module.
*******************************************************************************/

void ccError (void) {
	fprintf(stderr, "ccError: "STR, r0);
	while (r1--) {
		fprintf (stderr, " ");
		sysDisplay(vmPop(), stderr);
	}
	//vmDebugDumpCode(rcodenew, stderr);
	exit(-1);
}


/* Run time symbol lookup syscall.  If a symbol in r1 found in TGE mutate code
   to just reference the binding's value rather than make this syscall.
*/
void ccTGELookup (void) {
	DBBEG();
	sysTGEFind();
	if (r0 == null) {
		printf ("ERROR: Unbound symbol:");
		objDump(r1, stdout);
		r0 = r1;
		/* TODO  Kill thread, stop machine, return to monitor/shell? */
	} else {
		DB("SYS    found in tge @ opcode %x", (Num)rip-4);
		/* Specialization optimization.  Muate code that originally called
		   this function into a code that references the binding's value. */
		memVectorSet(rcode, (Num)rip-4, vmMVI0);  memVectorSet(rcode, (Num)rip-3, r0);
		memVectorSet(rcode, (Num)rip-2, vmLDI00); memVectorSet(rcode, (Num)rip-1, 0);
		/* Force virtual machine to run this code. */
		rip -= 4;
	}
	DBEND();
}

/* Run time symbol mutate syscall.  If a symbol in r1 found in TGE mutate code
   to just reference the binding's and mutate binding's value with r0.
*/
void ccTGEMutate (void) {
	DBBEG();
	r2=r0; /* Since a syscall, save value we're trying to set!. */
	sysTGEFind();
	if (r0 == null) {
		printf ("ERROR: Unbound symbol '");
		objDump(r1, stdout);
		printf ("'\n");
		r0 = r2; /* Return value. TODO  Kill thread, stop machine, return to monitor/shell? */
	} else {
		DB("SYS    found in tge at opcode %0x", (Int)rip-4);
		/* Specialization optimization.  Muate code that originally called
		   this function into a code that references the binding's value. */
		memVectorSet(rcode, (Num)rip-4, vmMVI1);  memVectorSet(rcode, (Num)rip-3, r0);
		memVectorSet(rcode, (Num)rip-2, vmSTI01); memVectorSet(rcode, (Num)rip-1, 0);
		r0 = r2; /* Restore value we're trying to set!. */
		/* Force virtual machine to run this code. */
		rip -= 4;
	}
	DBEND();
}

/* Generate assembly which looks up value of symbol in a local or
   global environment and assigns to r0
*/
void ccSymbol (Num flags) {
 Num d, ret, depth, offset;
	DBBEG();
	DBE objDump(rexpr, stderr);

	/* Scan local environments.  Returned is a 16 bit number, the high 8 bits
	   is the environment chain depth, the low 8 bits the binding offset. The
	   offset will be 2 or greater if a variable is found in any environment
	   excluding the global environment. */
	r1 = rexpr;
	ret = sysEnvFind();

	if (ret) {
		depth = ret >> 8;
		offset = ret & 0xff;
		DB("   found in a local environment depth:"NUM" offset:"NUM, depth, offset);
		/* Emit code that traverses the environment chain and references the proper binding. */
		if (depth == 0) {
			ccAsm(LDI, R0, R1C, (Obj)offset);
		} else {
			ccAsm(LDI, R0, R1C, 0l); /* Parent env */
			for (d=1; d < depth; d++) ccAsm(LDI, R0, R0, 0l); /* It's parent env */
			ccAsm(LDI, R0, R0, (Obj)offset); /* Local symbol offset */
		}
	} else {
		/* Scan tge... */
		sysTGEFind();
		if (null == r0) {
			DB("   can't find in TGE...maybe at runtime");
			ccAsm(
				MVI, R1, rexpr,
				SYSI, ccTGELookup);
		} else {
			DB("   found in TGE");
			r3 = r0; /* Keep track of the symbol */
			ccAsm(
				MVI, R0, r3,
				LDI, R0, R0, 0l);
		}
	}
	DBEND();
}


void ccSetB (Num flags) {
 Num ret, d,depth, offset;
	DBBEG();
	rexpr = cdr(rexpr); /* Consider set! parameter list (S E) */
	vmPush(car(rexpr)); /* Save S */
	/* Emit code that evaluates the expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	/* Scan local environments.  Returned is a 16 bit number, the high 8 bits
	   is the environment chain depth, the low 8 bits the binding offset. The
	   offset will be 1 or greater if a variable is found in any environment
	   excluding the global environment. */
	r1 = vmPop(); /* Restore S */
	ret = sysEnvFind();

	if (ret) {
		depth = ret >> 8;
		offset = ret & 0xff;
		DB("   found in a local environment depth:"NUM" offset:"NUM, depth, offset);
		/* Emit code that traverses the environment chain and references the proper binding. */
		if (depth == 0) {
			ccAsm(STI, R0, R1C, (Obj)offset);
		} else {
			ccAsm(LDI, R1, R1C, 0l); /* Parent env */
			for (d=1; d < depth; d++) ccAsm(LDI, R1, R1, 0l); /* It's parent env */
			ccAsm(STI, R0, R1, (Obj)offset); /* Local symbol offset */
		}
	} else {
		/* Scan tge... */
		sysTGEFind();
		if (r0 == null) {
			DB("   can't find in TGE...maybe at runtime");
			ccAsm(
				MVI, R1, rexpr,
				SYSI, ccTGEMutate);
		} else {
			DB("   found in TGE");
			r3 = r0; /* Keep track of the symbol */
			ccAsm(
				MVI, R1, r3,
				STI, R0, R1, 0l);
		}
	}

	DBEND();
}

void ccIf (Num flags) {
 Obj L1, L2;
	DBBEG();

	rexpr = cdr(rexpr); /* Consider if expression's parameter list (TEST CONSEQUENT ALTERNATE)*/
	assert("If expression missing TEST operand" && memIsObjectType(rexpr, TPAIR));

	/* [TEST] */
	r0 = cdr(rexpr); /* Make sure at least the CONSEQUENT operand exists */
	assert("If expression missing consequent" && memIsObjectType(r0, TPAIR));
	vmPush(r0); /* Push (CONSEQUENT ALTERNATE) */

	rexpr = car(rexpr); /* Compile TEST expression */
	ccCompileExpr(flags & ~CCTAILCALL);
	rexpr = vmPop(); /* Restore (CONSEQUENT ALTERNATE) */

	/* [TEST]---[BRANCH] */
 	L1 = ccNewLabel();
	ccAsm(BEQI, R0, false, L1);

	/* [TEST]---[BRANCH]---[CONSEQUENT] */
	vmPush(cdr(rexpr)); /* Push (ALTERNATE) */
	rexpr = car(rexpr); /* Compile CONSEQUENT expression */
	ccCompileExpr(flags);
	rexpr = vmPop(); /* Restore (ALTERNATE) */

	if (null == rexpr) {
		/* [TEST]---[BRANCH]---[CONSEQUENT]--[END] */
		ccAsm(LABEL, L1);
	} else {
 		L2 = ccNewLabel();
		/* [TEST]---[BRANCH]---[CONSEQUENT]--[JUMP]--[ALTERNATE]--[END] */
		ccAsm(BRA, L2);
		ccAsm(LABEL, L1);
		rexpr = car(rexpr); /* Consider alternate */
		ccCompileExpr(flags); /* Compile ALTERNATE expression.  It becomes the leading alternate block's default */
		ccAsm(LABEL, L2);
	}

	DBEND();
}

void ccCons (Num flags) {
	DBBEG();

	rexpr = cdr(rexpr); /* Consider cons's parameter list (A B)*/
	assert(memIsObjectType(rexpr, TPAIR)); /* TODO Ugly output Assertion memIsObjectType(r15, 0x80l) */

	vmPush(cdr(rexpr)); /* Save (B) */

	rexpr = car(rexpr); /* Consider and compile A */
	ccCompileExpr(flags & ~CCTAILCALL);

	ccAsm(
		PUSH, R0
	);

	rexpr = vmPop(); /* Restore (B) */
	assert(memIsObjectType(rexpr, TPAIR));
	assert(memIsObjectType(cdr(rexpr), TNULL));

	rexpr = car(rexpr); /* Consider and compile B */
	ccCompileExpr(flags & ~CCTAILCALL);

	ccAsm(
		POP, R1,
		SYSI, objCons10
	);

	DBEND();
}


/* Parse the form (? *) placing * into r1
   Return: 0 success  -1 error
*/
Int ccParseUnary (void) {
	r0 = cdr(rexpr); /* Consider arguments */
	if (!objIsPair(r0)) return -1; /* No arguments */
	r1 = car(r0);
	if (cdr(r0) != null) return -1; /* More than one argument */
	return 0;
}

void ccCar (Num flags) {
 Obj Lok;
	DBBEG();

	vmPush(rexpr); /* Save expression. */

	if (ccParseUnary()) {
		r0 = "Compiler error";
		r1 = (Obj)1;
		ccError();
	}

	rexpr = r1;  /* Consider and compile expression parsed */

	ccCompileExpr(flags & ~CCTAILCALL);
	rexpr = vmPop(); /* Restore expression and use in runtime error message */
	Lok = ccNewLabel();
	ccAsm(
		BRTI, R0, TPAIR, Lok,
		MVI, R0, rexpr,
		PUSH, R0,
		MVI, R1, 1l,
		MVI, R0, "runtime error",
		SYSI, ccError,
	 LABEL, Lok,
		LDI, R0, R0, 0l /* Perform car */
	);
	DBEND();
}

void ccCdr (Num flags) {
 Obj Lok;
	DBBEG();

	vmPush(rexpr); /* Save expression. */

	if (ccParseUnary()) {
		r0 = "Compiler error";
		r1 = (Obj)1;
		ccError();
	}

	rexpr = r1;  /* Consider and compile expression parsed */

	ccCompileExpr(flags & ~CCTAILCALL);
	rexpr = vmPop(); /* Restore expression and use in runtime error message */
	Lok = ccNewLabel();
	ccAsm(
		BRTI, R0, TPAIR, Lok,
		MVI, R0, rexpr,
		PUSH, R0,
		MVI, R1, 1l,
		MVI, R0, "runtime error",
		SYSI, ccError,
	 LABEL, Lok,
		LDI, R0, R0, 1l /* Perform cdr */
	);
	DBEND();
}

void ccSetCarB (Num flags) {
	DBBEG();
	rexpr = cdr(rexpr); /* Skip set-car! symbol. */
	if (rexpr == null) {
		printf ("ERROR: set-car! illegal pair expression: ");
		objDump (rexpr, stdout);
		goto ret;
	}
	vmPush(car(rexpr)); /* Save pair expression. */
	rexpr = cdr(rexpr);
	if (rexpr == null) {
		printf ("ERROR: set-car! illegal object expression: ");
		objDump (rexpr, stdout);
		goto ret;
	}
	rexpr = car(rexpr);/* Consider and compile object expression. */
	ccCompileExpr(flags & ~CCTAILCALL);
	ccAsm(PUSH, R0);
	rexpr = vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	ccAsm(
		POP, R2,
		STI, R2, R0, 0l);
ret:
	DBEND();
}

void ccSetCdrB (Num flags) {
	DBBEG();
	rexpr = cdr(rexpr); /* Skip set-cdr! symbol. */
	if (rexpr == null) {
		printf ("ERROR: set-cdr! illegal pair expression: ");
		objDump (rexpr, stdout);
		goto ret;
	}
	vmPush(car(rexpr)); /* Save pair expression. */
	rexpr = cdr(rexpr);
	if (rexpr == null) {
		printf ("ERROR: set-cdr! illegal object expression: ");
		objDump (rexpr, stdout);
		goto ret;
	}
	rexpr = car(rexpr);/* Consider and compile object expression. */
	ccCompileExpr(flags & ~CCTAILCALL);
	ccAsm(PUSH, R0);
	rexpr = vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	ccAsm(
		POP, R2,
		STI, R2, R0, 1l);
ret:
	DBEND();
}

void ccVerifyVectorRef (void) {
	if (memObjectLength(r1) <= (Num)r2) {
		vmPush(r2);
		vmPush(r1);
		r0 = "vector-ref out of bounds";
		r1 = (Obj)2;
		ccError();
	}
}

void ccVerifyVectorSetB (void) {
	if (memObjectLength(r1) <= (Num)r2) {
		vmPush(r0);
		vmPush(r2);
		vmPush(r1);
		r0 = "vector-set! out of bounds";
		r1 = (Obj)3;
		ccError();
	}
}

void ccVectorRef (Num flags) {
	DBBEG();
	vmPush(car(cddr(rexpr))); /* Save index expression. */
	rexpr = cadr(rexpr);       /* Compile Vector expression. */
	ccCompileExpr(flags & ~CCTAILCALL);
	rexpr = vmPop();            /* Compile index expression. */
	if (TINTEGER == memObjectType(rexpr)) {
		/* Load static integer value into register */
		ccAsm(
			MV, R1, R0, /* Move object to r1 */
			MVI, R2, *(Num*)rexpr,
			SYSI, ccVerifyVectorRef,
			LD, R0, R1, R2);
	} else {
		ccAsm(PUSH, R0);
		ccCompileExpr(flags & ~CCTAILCALL);
		ccAsm(
			POP, R1,
			/* Load object's integer value into register. */
			LDI, R2, R0, 0l, /* This fails runtime type check */
			SYSI, ccVerifyVectorRef,
			LD, R0, R1, R2);
	}
	DBEND();
}

void ccVectorSetB (Num flags) {
	DBBEG();
	rexpr=cdr(rexpr); /* Skip 'vector-set!. */
	vmPush(car(cddr(rexpr))); /* Save new-value expression. */
	vmPush(cadr(rexpr));      /* Save index expression. */
	/* Consider and compile Vector expression. */
	rexpr = car(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);
	ccAsm(PUSH, R0);           /* Save vector object. */
	/* Pop and compile index expression. */
	rexpr=vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	ccAsm(PUSH, R0);           /* Save offset object. */
	/* Pop and compile new-value expression. */
	rexpr=vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	ccAsm (
		POP, R2,       /* Pop offset object. */
		LDI, R2, R2, 0l,   /* Load offset object's integer value into register. */
		POP, R1,       /* Pop vector object. */
		SYSI, ccVerifyVectorSetB,
		ST, R0, R1, R2       /* Store new-value object in vector. */
	);
	DBEND();
}


/* Transform expr:((fn formals) body) into the form
   r0:(fn (lambda formals body)).  No syntic error checking is performed
   yet.  Would rather implement a macro transformation facility.
*/
void ccTransformDefineFunction (void) {
	DBBEG();
	r5 = cdr(rexpr);  /* Function's body. */
	rexpr = car(rexpr);
	r3 = car(rexpr); /* Function's name. */
	r4 = cdr(rexpr); /* Function's formal parameters. */

	r1=r4;      r2=r5;   objCons12(); /* (formals body) */
	r1=slambda; r2=r0;   objCons12(); /* (lambda formals body) */
	r1=r0;      r2=null; objCons12(); /* ((lambda formals body)) */
	r1=r3;      r2=r0;   objCons12(); /* (fn (lambda formals body)) */
	
	DBEND("  =>  ");
	DBE objDump(rexpr, stderr);
}


/* Transform expr:((define x q) (define y r) body)
       => r0:((lambda (x y) (set! x q) (set! y r) body) () ())
*/
void ccTransformInternalDefinitions(void) {
 Int definitionsCount=0;
	DBBEG();

	/* Save lambda body. */
	while (objIsPair(rexpr) && objIsPair(car(rexpr)) && sdefine == caar(rexpr)) {
		vmPush(cdr(rexpr));
		rexpr = cdar(rexpr); // Consider next expression and skip 'define.
		if (objIsPair(car(rexpr))) {
			ccTransformDefineFunction(); // Returns (fn (lambda formals body))
		} else {
			r0=rexpr;
		}
		rexpr = vmPop();
		vmPush(r0);
		definitionsCount++;
	}

	/* expr is now pointing at body of function.  If there were any internal
	   definitions, form an equivalent letrec expression. */
	if (definitionsCount) {
		r4=null; /* Local variable list.  Start out empty. */
		r5=rexpr; /* Set! expressions and body list. Start out with body. */
		r6=null; /* Null arguments list. */
		while (definitionsCount--) {
			r3=vmPop();/* Considered saved transformed define expression. */
			/* Prepend formal argument to list. */
			r1=car(r3); r2=r4; objCons12(); r4=r0;
			/* Prepend set! expression to list. */
			r1=ssetb;   r2=r3; objCons12();  /* Create (set! var ...) */
			r1=r0;      r2=r5; objCons12(); r5=r0;
			/* Prepend another null to argument list. */
			r1=null;    r2=r6; objCons12(); r6=r0;
		}
		r1=r4;      r2=r5;  objCons12();
		r1=slambda; r2=r0;  objCons12();
		r1=r0;      r2=r6;  objCons12();
		/* Create list consisting of this new expression. */
		r1=r0;      r2=null; objCons12();
		rexpr=r0;
	}
	
	DBEND("  =>  ");
	DBE objDump(rexpr, stdout);
}


/* Given (args body) in expr (r18) create a new code block that basically
   handles a call to a closures function.  The code assumes the closure is
   in r0.  A closure is a pair (code . environment) containing the code itself
   and the closures instantiated environment.

   Expr assumed to be of the form (args body) where args is currently of the
   form: (sym+).  Emit code that keeps track of the current environment

	Emitted code assumes the caller's code sets up the stack with all evaluated
   arguments pushed with r1 containing the arg count.  The count includes
   arguments to be grouped into the dotted formal argument list.

   Create an extended environment given:
   wscmExtendEnvironment moved from system call to inlined assembly.
    r1   - arg count
    r2   - lexical environment
    r3   - symbol list
    r1f (stack) - arguments on the stack.

  IE: ==> #( #<PARENT-ENV> (x y z rest) 1 2 3 (4 5 6))

 A local environment is of the form #(parent-env (x y . z) 1 2 (3 4 5))

    #( * (a . ()))
        \
         #( * (x y . z))
             \
              (TGE (square . #<closure>) (x . 5) (y . 9))

 A symbol lookup will be:
  (1) Compiled either as direct reference to a global environment binding
  (2) Compiled into a series of parent environment references and one
      local environment reference.
  (3) A syscall that attempts to locate the named binding which will then
      code modify itslef into case (1).
*/
void ccLambdaBody (Num flags) {
 Obj Lcomment, Lexpectednoargs, LkeepLexicalEnvironment, LnotEnoughArguments, LnormalFormals, LbuildRestList;
	DBBEG();
	DBE objDump(rexpr, stdout);

	/* Since we're creating a new code object, create a new ASM context */
	ccStart();

	/* The first opcode emitted is a branch past a pointer to the original
	   expression being compiled.  This is a quick and dirty debugging aid. */
	Lcomment = ccNewLabel();
	//ccAsm(
	//	BRA, Lcomment,
	//	rexpr,
	//	LABEL, Lcomment
	//);

	/* Emit code that extends stack-stored arguments (Free variables can be
	   statically compiled?  r2 is assumed to hold the environment to be
	   extended, r1 the argument count, r3 the formal arguments.) */
	if (null == car(rexpr)) {
		/* Since a lambda with empty formals list, emit code which doesn't extend
		   the environment but instead sets env to the containing closure's env
		   or TGE if this is a top level definition (which will always be for 'lambda
		   and not 'macro) */
		if (renv == rtge) ccAsm(MV, R1C, R18); /* env = tge */
		else ccAsm(LDI, R1C, R0, 1l);

		Lexpectednoargs = ccNewLabel();
		ccAsm (
			BEQI, R1, 0, Lexpectednoargs,
			MVI, R0, rexpr, /* Error situation.  Add expression to stack */
			PUSH, R0,
			ADDI, R1, 1l,
			MVI, R0, "Too many arguments to closure",
			SYSI, ccError, /* Error correction */
			LABEL, Lexpectednoargs
		);
	} else {
		/* Emit code that extends the environment.  Pops the top most arguments
		   into a list for the 'rest' formal parameter  (lambda (a b . rest)...).
		   R3 contains the non-dotted formal parameter length (via the Normalize
		   function above). */

		/* Temporarily save lexical environment, from closure in r0, or tge, to r5.
		   The stored environment might be null in which case keep track of
		   the current/dynamic environment instead of the stored lexical.  Use
		   TGE when a top level definition.  See also the similar situation in this
		   if block's true clause with the empty formals case. */
		if (car(renv) == rtge) ccAsm(MV, R5, R18);
		else ccAsm(LDI, R5, R0, 1l);

		LkeepLexicalEnvironment = ccNewLabel();
		LnotEnoughArguments = ccNewLabel();
		LnormalFormals = ccNewLabel();
		ccAsm (
			BNEI, R5, null, LkeepLexicalEnvironment,
			MV, R5, R1C,
		 LABEL, LkeepLexicalEnvironment,
			MVI, R0, null, /* Initial formal argument 'rest' value (empty list). */
			/* r3 is non-dotted formal argument length. */
			BLTI, R1, r3, LnotEnoughArguments,
			BEQI, R1, r3, LnormalFormals
		);

		/* Emit code for functions lacking a dotted formal argument.  This code
		   will be reached if there are more values passed to the function than
		   there are formal arguments.  Otherwise it will just continue to build
		   the dotted formal list. */
		if (r4 == 0) {
			ccAsm (
				MVI, R0, rexpr, /* Add expression to stack */
				PUSH, R0,
				ADDI, R1, 1l,
				MVI, R0, "Too many arguments to function",
				SYSI, ccError /* Error correction */
			);
		}

		LbuildRestList = ccNewLabel();
		ccAsm (
		LABEL, LbuildRestList,
			MV, R3, R0,
			POP, R2,
			SYSI, objCons23,
			ADDI, R1, -1l,
			BNEI, R1, r3, LbuildRestList,
			BRA, LnormalFormals,
		LABEL, LnotEnoughArguments,
			MVI, R0, rexpr, /* Add expression to stack */
			PUSH, R0,
			ADDI, R1, 1l,
			MVI, R0, "Not enough arguments to closure",
			SYSI, ccError, /* Error correction */
			PUSH, R0,
			ADDI, R1, 1l,
			BNEI, R1, r3, LnotEnoughArguments,
		LABEL, LnormalFormals,
			PUSH, R0,
			/* Create the local environment. r1 is the length of the vector.
			   3 is added to account for the parent env, formal argument list
			   and rest formal argument. */
			ADDI, R1, 3l,
			SYSI,  objNewVector1, /* New vector in r0 of size imm:r1. */
			STI, R5, R0, 0l, /* Set parent link. */
			/* Set the environment's normalized formal argument list which was
			   created before the call to this C function. */
			MVI, R3, cdr(renv),
			STI, R3, R0, 1l
		);

		/* Emit code that pops arguments off stack and stores into proper
		   local environment locations.  */
		r3++;
		while (r3--) {
			ccAsm (
				POP, R2,
				STI, R2, R0, r3+2l
			);
		}
		/* Set env register to the newly extended environment. */
		ccAsm(MV, R1C, R0);
	}

	/* Skip lambda's formal argument list. */
	rexpr = cdr(rexpr);

	/* Compile expressions in lambda block (all but the last).  If the lambda
	   body is empty, emit code that returns null.  This is not to r5rs
	   specification which requires body contain a sequence of one or more
	   expressions. */
	if (rexpr == null) {
		ccAsm(MVI, R0, null);
		DB("   Empty function body.");
	} else {
		/* Transform internal definitions, if any, and body into equivalent
		   expanded letrec and body, ie:(((lambda (f) (set! f ...) body) () () ...)).*/
		ccTransformInternalDefinitions();
		while (cdr(rexpr) != null) {
			DB("   Lambda non-tail optimization");
			vmPush(cdr(rexpr)); /* Push next expr. */
			rexpr = car(rexpr);
			ccCompileExpr((flags & ~CCTAILCALL) | CCNODEFINES);
			rexpr = vmPop();
		}
		DB("   Lambda tail optimization");
		rexpr = car(rexpr);
		ccCompileExpr(flags | CCTAILCALL | CCNODEFINES);
	}

	ccAsm(RET);

	/* Assemble igraph and restore previous ASM context */
//ccDumpIBlocks();
	ccAsmIGraph();

	DBE vmDebugDumpCode(rcodenew, stderr);
	DBEND();
}

/* Normalize a scheme formals list into an internal normalized formals
   environment list.  A proper list with a symbol or null as the "rest"
   formal.

   (x)       ->  (x ())
   (x y)     ->  (x y ())
   (x . r)   ->  (x r)
   (x y . r) ->  (x y r)
   r         ->  (r)
   ()        ->  (())

   Given   r0   A lambda expression's formals list

   Uses    r1   List creation

   Return  r0   Normalized formal parameter list
           r3   Number of non-dotted formal parameters
           r4   0 or 1 dotted formals
*/
void ccNormalizeFormals(void) {
 Num i;
	r3=0; /* Keep track of non-dotted formal count. */

	/* Push formals onto stack. */
	while (objIsPair(r0)) {
		r3++;
		vmPush(car(r0));
		r0=cdr(r0);
	}

	/* Keep track of the existence of a dotter formal */
	r4 = (r0==null) ? (Obj)0 : (Obj)1;

	/* Pop formals from stack creating list of args starting
      with (()) or (dotted-formal) */
	r1=r0;  r2=null;  objCons12();
	i=(Num)r3;
	while (i--) { r2=r0;  r1=vmPop();  objCons12(); }
}

void ccLambda (Num flags) {
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'lambda. */

	vmPush(renv); /* Save env. */

	/* Extend pseudo environment only if the formals list is not empty to
	   mimic the runtime optimized environment chain.   A pseudo environment
	   is just the pair (parent-environment . formals-list)*/
	if (car(rexpr) != null) {
		r0=car(rexpr);
		ccNormalizeFormals(); /* Create normalized list in r0, length in r3, dotted-formal bool in r4. */
		r1=renv;  r2=r0;  objCons12();  renv=r0;
	}

	/* Create closures code block in r0. */
	ccLambdaBody(flags);

	renv = vmPop(); /* Restore env. */

	/* Generate code that generates a closure.  Closure returned in r0 created
	   from r1 (code) and r1c (current environment). */
	ccAsm(
		MVI, R1, r0, /* Load r1 with code just generated */
		SYSI, sysNewClosure1Env /* Create closure from r1 and env (r1c) */
	);

	DBEND();
}


void ccBegin (Num flags) {
	DBBEG();
	rexpr = cdr(rexpr); /* Skip symbol 'begin. */

	if (rexpr == null) {
		ccAsm( MVI, R0, null);
	} else {
		while (cdr(rexpr) != null) {
			DB("begin block's non-tail expression");
			vmPush(cdr(rexpr)); /* Push rest of operands */
			rexpr = car(rexpr); /* Consider next operand */
			ccCompileExpr(flags & ~CCTAILCALL);
			rexpr = vmPop(); /* Pop rest of expression */
		}
		DB("begin block's tail expression");
		rexpr = car(rexpr);
		ccCompileExpr(flags);
	}
	DBEND();
}


void ccDefine (Num flags) {
	DBBEG();
	if (flags & CCNODEFINES) {
		//CompError = 1;
		fprintf(stderr, "ERROR: compDefine(): Define not allowed here");
		objDump(rexpr, stderr);
	} else {
		rexpr = cdr(rexpr); /* Skip 'define symbol. */

		vmPush(renv);
		renv = rtge;

		/* If the expression is of the form ((...) body) transform. */
		if (objIsPair(car(rexpr))) {
			ccTransformDefineFunction();
			rexpr = r0;
		}

		if (TSYMBOL == memObjectType(r1=car(rexpr))) {
			/* Bind (if not already bound) the symbol and get its binding. */
			sysTGEBind();
			/* Emit code to set the binding's value. */
			rexpr = cdr(rexpr);
			if (objIsPair(rexpr)) {
				vmPush(r0); /* Save binding. */
				rexpr = car(rexpr); /* Consider this definition's expression and compile. */
				ccCompileExpr((Num)flags & ~CCTAILCALL);
				ccAsm(
					MVI, R1, vmPop(), /* Load r1 with saved binding. */
					STI, R0, R1, 0L);    /* Set binding's value. */
			} else {
				fprintf(stderr, "ERROR: compDefine(): Missing expression.");
			}
		} else  {
			fprintf(stderr, "ERROR: compDefine(): Not a symbol:"); objDump(r1, stderr);
		}

		renv = vmPop();
	}
	DBEND();
}


void ccNot (Num flags) {
 Obj L1, L2;
	DBBEG();
	rexpr = cadr(rexpr);           /* Compile this expression */
	ccCompileExpr(flags & ~CCTAILCALL);
 	L1 = ccNewLabel();
 	L2 = ccNewLabel();
	ccAsm (
		BEQI, R0, false, L1,
		MVI, R0, false,
		BRA, L2,
		LABEL, L1,
		MVI, R0, true,
		LABEL, L2
	);
	DBEND();
}

/* Compiles expressions of the form (or exp ...) into:
		exp
		branch if not false to end
*/
void ccOr (Num flags) {
 Obj Lend;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'or. */

	/* Empty or expression returns #f. */
	if (null == rexpr) {
		ccAsm (MVI, R0, false);
	} else {
		Lend = ccNewLabel();
		while (objIsPair(rexpr)) {
			vmPush (cdr(rexpr)); /* Push rest. */
			/* Is this the last expression?  If so it's tail optimized. */
			if (!objIsPair(cdr(rexpr))) {
				rexpr = car(rexpr); /* Consider next expression. */
				ccCompileExpr(flags);
			} else {
				rexpr = car(rexpr); /* Consider next expression. */
				ccCompileExpr(flags & ~CCTAILCALL);
				ccAsm(BNTI, R0, TFALSE, Lend);
			}
			rexpr = vmPop();
		}
		ccAsm (LABEL, Lend);
	}
	DBEND();
}

/* Compiles expressions of the form (and exp ...) into:
		exp
		branch if false to end
*/
void ccAnd (Num flags) {
 Obj Lend;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'and. */

	/* Empty or expression returns #t. */
	if (null == rexpr) {
		ccAsm (MVI, R0, true);
	} else {
		Lend = ccNewLabel();
		while (objIsPair(rexpr)) {
			vmPush (cdr(rexpr)); /* Push rest. */
			/* Is this the last expression?  If so it's tail optimized. */
			if (!objIsPair(cdr(rexpr))) {
				rexpr = car(rexpr); /* Consider next expression. */
				ccCompileExpr(flags);
			} else {
				rexpr = car(rexpr); /* Consider next expression. */
				ccCompileExpr(flags & ~CCTAILCALL);
				ccAsm(BRTI, R0, TFALSE, Lend);
			}
			rexpr = vmPop();
		}
		ccAsm (LABEL, Lend);
	}
	DBEND();
}


void ccAsmTailCall () {
 Obj Lsyscall, Lclosure;
	DBBEG();
	Lsyscall = ccNewLabel();
	Lclosure = ccNewLabel();
	ccAsm (
		BRTI, R0, TSYSCALL, Lsyscall,
		BRTI, R0, TCLOSURE, Lclosure,
		/* Illegal operator section.  For now just dump the arguments.  */
		MVI, R0, "Illegal Operator Type",
		SYSI, ccError,
		RET, /* Since tail call, return. */
		/*  Reference the syscall address then make the system call.  */
	 LABEL, Lsyscall,
		LDI, R0, R0, 0l,
		SYS, R0,
		RET, /* Since a tail call, return. */
		/* Closure operator section.  Load jump address into r2.  R1 is
		   argument count and r0 is the closure (which is needed as it
		   holds the lexical environment).
		*/
	 LABEL, Lclosure,
		LDI, R2, R0, 0l,
		JMP, R2
	);
	DBEND();
}

void ccAsmNonTailCall () {
 Obj Lsyscall, Lclosure, Lend;
	DBBEG();
 	Lsyscall = ccNewLabel();
 	Lclosure = ccNewLabel();
 	Lend = ccNewLabel();
	ccAsm (
		BRTI, R0,  TSYSCALL, Lsyscall,
		BRTI, R0,  TCLOSURE, Lclosure,
		/* Illegal operator section.  For now just dump the arguments. */
		MVI, R0, "Illegal Operator Type",
		SYSI, ccError,
	//	SYSI, ccIllegalOperator,
		BRA,  Lend,
		/* Syscall operator section.  Reference the syscall address, set the
	   	operand count then make the system call.  */
	LABEL, Lsyscall,
		LDI, R0, R0, 0l,
		SYS, R0,
		BRA,  Lend,
		/* Closure operator section.  */
	LABEL, Lclosure,
		LDI, R2, R0, 0l, /* load r2 with code and jump. */
		JAL, R2,
		/* End of block.  */
	LABEL, Lend,
		POP, R19, /* Restores previous environment, ip and code registers. */
		POP, R1B,
		POP, R1A
	);
	DBEND();
}


/* Compiles expression of the form (if testExpr (consequentExpr {value of testExpr}) alternateExpr)
*/
void ccAIf (Num flags) {
 Obj LfalseBraAddr, LtrueContAddr;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip 'aif symbol. */
	vmPush (cddr(rexpr)); /* Push alternate expressions list.  Will be NULL or a list containing the alternate expression. */
	vmPush (cadr(rexpr));  /* Push consequent expressions. */

	DB("compiling test");
	rexpr = car(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	DB("compiling test logic");
	LfalseBraAddr = ccNewLabel();
	ccAsm (
		BEQI, R0, false, LfalseBraAddr
	);

	DB("compiling consequent");
	/* Save execution state, possibly, since the following is the equivalent of ccCombination */
	if (!((Num)flags & CCTAILCALL)) {
		ccAsm (
			PUSH, R1A,
			PUSH, R1B,
			PUSH, R19);
	}

	ccAsm(
		PUSH, R0 /* Push result of test expression on the stack.  Becomes argument to consequent. */
	);

	rexpr = vmPop(); /* Compile consequent. */
	ccCompileExpr(flags & ~CCTAILCALL);

	ccAsm(
		MVI, R1, 1l  /* Set the argument count to 1.  Argument already on the stack. */
	);
	
	if ((Num)flags & CCTAILCALL) ccAsmTailCall();
	else ccAsmNonTailCall();

	DB("compiling end of consequent and beginning of alternate");
	LtrueContAddr = ccNewLabel();
	ccAsm(
		BRA, LtrueContAddr,
	 LABEL, LfalseBraAddr
	);

	DB("compiling alternate");
	rexpr = vmPop();
	if (objIsPair(rexpr)) {
		 /* Compile alternate expression. */
		rexpr = car(rexpr);
		ccCompileExpr(flags);
	} else {
		/* No alternate expression so return #f which is in reg 0 from the test condition */
	}

	ccAsm(
	 LABEL, LtrueContAddr
	);
	DBEND();
}

/* Transforms then compiles the cond special form
   (cond <clause> ...)
     clause := (<test>    <expr> ...)
               (<test> => <expr>)
               (<test>)
               (else      <expr> ...)
*/
void ccCond (Num flags) {
 Num clauses=0;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip symbol 'cond */

	/* Push clauses, checking for non-lists and verifying the else clause is last */
	while (objIsPair(rexpr)) { /* Over all clauses  expr = (<clause> ....) */
		r5 = car(rexpr); /* Consider next clause  r5 = <clause>  */
		if (!objIsPair(r5)) { /* Error if clause is not a list */
			fprintf (stderr, "\nWARNING: compCond: Malformed cond clause ");
			objDump(r5, stderr);
		} else {
			DB("Pushing clause");
			DBE objDump(r5, stderr);
			clauses++;
			vmPush(r5);
			rexpr = cdr(rexpr); /* Consider next clause for this loop */
			if (selse == car(r5)) {
				/* Else clause matched, so stop pushing clauses and give warning if more clauses follow */
				if (rexpr != null) {
					fprintf (stderr, "\nWARNING: compCond: cond's else clause followed by more clauses ");
					objDump(rexpr, stderr);
				}
				rexpr = null;
			}
		}
	}

	/* Pop clauses building the if/or/begin tree bottom-up into r0 */
	DB (" Creating nested if/or/begin expression");
	r0 = null;
	while (clauses--) {
		r5 = vmPop(); /* Consider clause r5 = <clause> = (r4 . r3) */
		r4 = car(r5); /* First expr */
		r3 = cdr(r5) ; /* Rest expr */
		if (selse == r4) {
			assert(null == r0); /* This better be the first clause popped or not at all */
			r1=sbegin; r2=r3; objCons12();          /* (begin <expr> ...) */
		} else if (!objIsPair(r3)) {
			r1=r0;  r2=null; objCons12();           /* (translated) */
			r1=r4;  r2=r0; objCons12();             /* (<test> (translated)) */
			r1=sor; r2=r0; objCons12();             /* (or <test> (translated)) */
		} else if (saif == car(r3)) {
			r3 = cdr(r3); /* Consider (r4 => . r3 */
			r1=r0;  r2=null; objCons12();           /* (translated) */
			if (objIsPair(cdr(r3))) { /* Give warning if else clause followed by more clauses */
				fprintf (stderr, "\nWARNING: compCond: cond's => expr not a single expression ");
				objDump(r5, stderr);
			}
			r1=car(r3); r2=r0; objCons12();         /* (<expr> translated) */
			r1=r4;  r2=r0; objCons12();             /* (<test> <expr> translated) */
			r1=saif; r2=r0; objCons12();            /* (if <test> <expr> translated) */
		} else {
			r1=r0;  r2=null; objCons12(); vmPush(r0); /* (translated) */
			r1=sbegin; r2=r3; objCons12();          /* (begin <expr> ...) */
			r1=r0; r2=vmPop(); objCons12();           /* ((begin <expr> ...) translated) */
			r1=r4;  r2=r0; objCons12();             /* (<test> (begin <expr> ...) translated) */
			r1=sif; r2=r0; objCons12();             /* (if <test> (begin <expr> ...) translated) */
		}
	}
	DB ("compCond translated ");
	DBE objDump(r0, stdout);
	rexpr = r0;
	ccCompileExpr(flags);
	DBEND();
}

void ccProcedureP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (procedure?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = ccNewLabel();
	Ltrue = ccNewLabel();

	ccAsm(
		BRTI, R0, TCLOSURE, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}


void ccNullP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (null?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = ccNewLabel();
	Ltrue = ccNewLabel();

	ccAsm(
		BEQI, R0, null, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccPairP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (pair?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = ccNewLabel();
	Ltrue = ccNewLabel();

	ccAsm(
		BRTI, R0, TPAIR, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccVectorP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (vector?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = ccNewLabel();
	Ltrue = ccNewLabel();

	ccAsm(
		BRTI, R0, TVECTOR, Ltrue,
		BRTI, R0, TNULLVEC, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccStringP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (string?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = ccNewLabel();
	Ltrue = ccNewLabel();

	ccAsm(
		BRTI, R0, TSTRING, Ltrue,
		BRTI, R0, TNULLSTR, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccIntegerP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (integer?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = ccNewLabel();
	Ltrue = ccNewLabel();

	ccAsm(
		BRTI, R0, TINTEGER, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccSymbolP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (integer?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = ccNewLabel();
	Ltrue = ccNewLabel();

	ccAsm(
		BRTI, R0, TSYMBOL, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccPortP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (integer?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = ccNewLabel();
	Ltrue = ccNewLabel();

	ccAsm(
		BRTI, R0, TPORT, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}

void ccEOFObjectP (Num flags) {
 Obj Lend, Ltrue;
	DBBEG();
	if (!objIsPair(cdr(rexpr))) {
		fprintf (stderr, "ERROR: (integer?) illegal operand count: ");
		objDump (rexpr, stdout);
	}

	/* Consider and compile expression. */
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);

	Lend = ccNewLabel();
	Ltrue = ccNewLabel();

	ccAsm(
		BRTI, R0, TEOF, Ltrue,
		MVI, R0, false,
		BRA, Lend,
	 LABEL, Ltrue,
		MVI, R0, true,
	 LABEL, Lend);

	DBEND();
}


void ccTransformLet (void) {
 Num bindingLen, i;
	DBBEG();
	r4=car(rexpr);     /* Consider the let bindings. */
	r5 = cdr(rexpr);   /* Consider the let body. */

	/* Create (val ...) */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		vmPush(car(cdar(r6)));
		r6=cdr(r6);
	}
	r2=null;
	for (i=0; i<bindingLen; i++) {
		r1=vmPop();
		objCons12();
		r2=r0;
	}
	vmPush(r2);

	/* Create (var...) */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		vmPush(caar(r6));
		r6=cdr(r6);
	}
	r2=null;
	for (i=0; i<bindingLen; i++) {
		r1=vmPop();
		objCons12();
		r2=r0;
	}

	/* Create ((var...)body) */
	r1=r2;  r2=r5;  objCons12();

	/* Create (lambda (var...)body) */
	r1=slambda;r2=r0;  objCons12();

	/* Create ((lambda (var...) body) val...) */
	r1=r0;  r2=vmPop();  objCons12();

	/* Return transformed expression. */
	rexpr=r0;

	DBEND("  =>  ");
	DBE objDump(rexpr, stdout);
}

void ccTransformNamedLet (void) {
 Num bindingLen, i;
	DBBEG();
	r3=car(rexpr);   /* Consider the named-let name symbol. */
	rexpr = cdr(rexpr);
	r4=car(rexpr);   /* Consider the named-let bindings. */
	r5=cdr(rexpr);   /* Consider the named-let body. */

	/* Create ((name val ...)) */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		vmPush(car(cdar(r6)));
		r6=cdr(r6);
	}
	r2=null;
	for (i=0; i<bindingLen; i++) {
		r1=vmPop();
		objCons12();
		r2=r0;
	}
	r1=r3;  objCons12();
	r1=r0;  r2=null;  objCons12();
	vmPush(r0);

	/* Create (set! name (lambda (var...) body)). */
	r6=r4;
	bindingLen=objListLength(r4);
	for (i=0; i<bindingLen; i++) {
		vmPush(caar(r6));
		r6=cdr(r6);
	}
	r2=null;
	for (i=0; i<bindingLen; i++) {
		r1=vmPop();
		objCons12();
		r2=r0;
	}
	r1=r2;     r2=r5;  objCons12();
	r1=slambda;r2=r0;  objCons12();
	r1=r0;     r2=null;objCons12();
	r1=r3;     r2=r0;  objCons12();
	r1=ssetb;  r2=r0;  objCons12();

	/* Merge them into new-body. */
	r1=r0;  r2=vmPop();  objCons12();
	vmPush(r0);

	/* Create (lambda name new-body) */
	r1=r3;  r2=vmPop();  objCons12();
	r1=slambda; r2=r0;  objCons12();
	vmPush(r0);

	/* Create ((lambda name newbody)) and we're done. */
	r1=vmPop();  r2=null;  objCons12();

	/* Return transformed expression. */
	rexpr=r0;

	DBEND("  =>  ");
	DBE objDump(rexpr, stdout);
}

void ccLet (Num flags) {
	DBBEG();
	rexpr=cdr(rexpr); /* Skip 'let. */

	/* Transform named-let form (let symbol ...). */
	if (memObjectType(car(rexpr)) == TSYMBOL)
		ccTransformNamedLet();
	/* Transform let form (let (...) ...). */
	else
		ccTransformLet();

	/* Now compile the transformed form. */
	ccCompileExpr(flags);

	DBEND();
}

/* Transform:
   (letrec ((v exp)...) body)  =>  (let ((v ())...) (set! v exp)... body)
   Why not:  ((lambda (v ...) (set! v exp) ... body) () ...)
*/
void ccTransformLetrec (void) {
 Num len;
	DBBEG();
	rexpr=cdr(rexpr); /* Skip letrec. */

   if (!objIsPair(car(rexpr))) {
		fprintf (stderr, "letrec malformed: ");
		objDump(rexpr, stderr);
	}

	/* Push and count letrec binding expressions. */
	for (r3=car(rexpr), len=0;  r3!=null; r3=cdr(r3), len++) vmPush(car(r3));

	/* Create (()) in r4. */
	r1=null;  r2=null;  objCons12();
	r4=r0;
	/* Create ((x ())...) in r3 from bindings on stack so start it with null. */
	r3=null;
	while(len--) {
		r1=car(vmPop());  r2=r4;  objCons12(); /* Form (x ()). */
		r1=r0;          r2=r3;  objCons12(); /* Form ((x ()) ...). */
		r3=r0;
	}
	vmPush(r3); /* Save transformed bindings to stack. */

	/* Push and count letrec binding expressions (again). */
	for (r3=car(rexpr), len=0;  r3!=null; r3=cdr(r3), len++) vmPush(car(r3));
	/* Create (((x ())...) (set! x rexpr) ... body). */
	r3=cdr(rexpr); /* Consider (body). */
	while(len--) {
		r1=ssetb;   r2=vmPop();  objCons12();
		r1=r0;      r2=r3;     objCons12();
		r3=r0;
	}

	/* Create (bindings (set! ...) body). */
	r1=vmPop();  r2=r3;  objCons12();

	/* Create (let ...). */
	r1=slet; r2=r0;  objCons12();

	DBEND("  =>  ");
	DBE objDump(r0, stdout);
}

void ccLetrec (Num flags) {
	DBBEG();
	ccTransformLetrec();
	rexpr = r0;
	ccCompileExpr(flags);
	DBEND();
}

/* Given <qq template> in rexpr, create cons tree in r0.
*/
void ccTransformQuasiquote (int depth) {
 int isUnquote, isQuasiquote;
	DBBEG();
	if (objIsPair(rexpr)) { /* Is this (unquote ...) */
		isUnquote    = (car(rexpr)==sunquote);
		isQuasiquote = (car(rexpr)==squasiquote);
		if (isUnquote && depth==0) {
			/* (unquote atom) => atom */
			r0 = cadr(rexpr);
		} else if (objIsPair(car(rexpr))
		           && caar(rexpr) == sunquotesplicing
		           && depth==0) {
			/* ((unquote-splicing template) . b) */
			vmPush(car(cdar(rexpr))); /* Save template */
			rexpr=cdr(rexpr);  /* Consider b */
			ccTransformQuasiquote(depth); /* => b' */
			/* (append template b') */
			r1=r0;     r2=null;  objCons12(); /* => (b') */
			r1=vmPop();  r2=r0;    objCons12(); /* => (template b') */
			r1=sappend;  r2=r0;    objCons12(); /* => (append template b') */
		} else { /* Transform (a . b) => (cons a' b') */
			vmPush(cdr(rexpr)); /* Save b */
			rexpr=car(rexpr);  /* Consider a */
			ccTransformQuasiquote(depth); /* => a' */
			rexpr=vmPop();      /* Restore b */
			vmPush(r0);        /* Save a' */
			ccTransformQuasiquote(depth - isUnquote + isQuasiquote); /* => b' */
			r1=r0;     r2=null;  objCons12(); /* => (b') */
			r1=vmPop();  r2=r0;    objCons12(); /* => (a' b') */
			r1=scons;  r2=r0;    objCons12(); /* => (cons a' b') */
		}
	/* Transform atom into (quote atom) */
	} else {
		r1=rexpr;   r2=null;  objCons12(); // atom   => (atom)
		r1=squote; r2=r0;    objCons12(); // (atom) => (quote atom)
	}
	DBEND();
}

void ccQuasiquote (Num flags) {
	DBBEG();
	rexpr = cadr(rexpr); // Given (quasiquote <qq template>) pass <qq template>
	ccTransformQuasiquote(0);
	rexpr = r0;
	DB("quasiquote transformation => ");
	DBE objDump (rexpr, stderr);
	ccCompileExpr(flags);
	DBEND();
}


void ccQuote (void) {
	DBBEG();
	ccAsm (
		MVI, R0,
		 cadr(rexpr));
	DBEND();
}


/* Compile the form (apply fn argument-list).  This should be similar to
   a combination expression. */
void ccApply (Num flags) {
 Num operandCount=0;
 Obj Largcount, Largcountdone;
	DBBEG();

	rexpr = cdr(rexpr); /* Skip over 'apply symbol */

	/* Is this a tail call?  if not save state. */
	if (!((Num)flags & CCTAILCALL)) {
		ccAsm (
			PUSH, R1A,
			PUSH, R1B,
			PUSH, R19);
	}

	vmPush(car(rexpr)); /* Save operator parameter. */

	/* Compile operand expressions the last of which hopefully evaluates to a list of args.
	   The resulting arguments will be pushed onto the stack and passed to the function.  */
	rexpr = cdr(rexpr);
	while (objIsPair(rexpr)) {
		vmPush (cdr(rexpr)); /* Push rest */
		rexpr = car(rexpr); /* Consider expression  */
		ccCompileExpr(flags & ~CCTAILCALL);
		ccAsm(PUSH, R0);
		operandCount++;
		rexpr = vmPop();
	}

	/* Restore and compile operator expression. */
	rexpr=vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	ccAsm(MV, R3, R0); /* Save operator in r3 */

	/* At this point stack has the arguments, the argument-list and r3 has function.
	   Want to transfers the argument-list items from list to the stack with r1 ending up
	   with the argument count.  Initially the argument count is the number of initial
	   non-list arguments to apply.
	*/
	Largcount = ccNewLabel();
	Largcountdone = ccNewLabel();
	ccAsm (
		MVI, R1, (Obj)(operandCount-1), /* Initialize operand count in r1 to number of initial arguments to apply. */
		POP, R0,    /* Pop argument-list. */
	 LABEL, Largcount,
		BRTI, R0, TNULL, Largcountdone,
		ADDI, R1, 1l, /* Inc argument count in r1. */
		LDI, R2, R0, 0l, /* Push the car. */
		PUSH, R2,
		LDI, R0, R0, 1l, /* Consider cdr. */
		BRA, Largcount,
	 LABEL, Largcountdone,
		MV, R0, R3     /* Operator back to r0 */
	);

	/* Need to asm code that handles operators of type syscall, closure,
	   continuation and the like.  For now it just assumes a syscall.  Perhaps
	   a special syscall that handles this all in C for now?
	   Emit code to check the object type and either SYS the TSYSCALL type
	   or JAL the TCODE type.  */
	/* Emit code to that applys args to function/code tail optimized or not. */

	if ((unsigned)flags & CCTAILCALL) ccAsmTailCall();
	else ccAsmNonTailCall();

	DBEND();
}

/* Compiles s-expression in r0 into code block in r0.  Probably messes up
   a bunch of registers.
	TODO: does this mangle r19/1a/1b retenv/retip/retcode?
*/
void ccSysCompile (void) {
	DBBEG();

	vmPush(renv); /* Save env vars */
	vmPush(rcode);

	//CompError = 0;
	//asmAsm (BRA, 8, rexpr); /* Keep track of original expression for debugging. */
	rexpr = r0;
	ccAsmInit();
	ccCompileExpr(0);
	//if (CompError)
	//{
	//	r0 = "compSysCompile: ccCompileExpr failed";
	//	compError();
	//	goto ret;
	//}
	ccAsm(RET);

	ccAsmIGraph();
	//r0 = rcode; /* Move new code block to r0 */

	rcode = vmPop(); /* Restore env vars */
	renv = vmPop();

	DBE vmDebugDumpCode(r0, stderr); // Dump the code block after compiling code during runtime.
	DBEND("  =>  ");
	DBE objDump (r0, stderr);
}

void ccEval (Num flags) {
	DBBEG();
	rexpr = cadr(rexpr);
	ccCompileExpr(flags & ~CCTAILCALL);
	ccAsm(SYSI, (Obj)ccSysCompile);
	if (flags & CCTAILCALL) {
		ccAsm(JMP, R0);
	} else {
		ccAsm(
			PUSH, R1A,
			PUSH, R1B,
			PUSH, R19,
			JAL, R0,
			POP, R19,
			POP, R1B,
			POP, R1A);
	}
	DBEND();
}

void ccMacro (Num flags) {
	DBBEG();

	ccStart();

	/* Transform (macro ... ...) => (lambda .. ...) assigned to r0 */
	r1=slambda;  r2 = cdr(rexpr);  objCons12();

	ccAsm(
		PUSH, R1,
		MVI, R0, r0,
		SYSI, ccSysCompile,
		PUSH, R1A, PUSH, R1B, PUSH, R19,
		JAL, R0,
		POP, R19, POP, R1B, POP, R1A,
		LDI, R2, R0, 0l, /* load r2 with code block and call it.  it will return a closure.  */
		POP, R1,
		JMP, R2);

	ccAsmIGraph();
	//r0 = rcode; /* Move new code block to r0 */

	/* Generate code that generates a closure.  Closure returned in r0 created
	   from r1 (code) and r1c (current environment). */
	ccAsm(
		MVI, R1, r0, /* Load r1 with code block just compiled. */
		SYSI, sysNewClosure1Env, /* Create closure from r1 and env (r1c) */
		MVI, R2, null, /* Replace stored lexical environment with null so dynamic environment is used when applied */
		STI, R2, R0, 1l);

	DBEND();
}

/* Stored stack expected in r3.
*/
void ccSysReinstateContinuation (void) {
 Num length;
	DBBEG();

	if ((Int)r1==1) r0=vmPop();
	else {
		fprintf (stderr, "ERROR: compReinstateContinuation() bad argument count %d.\n", (Int)r1);
		exit (-1);
	}

	/* Reinstate stack and registers.
	*/
	length = memObjectLength(r3); /* The stored stack is in r3. */
	memcpy(rstack+8, r3, length*8); /* Copy objects into stack vector. */
	*(Obj*)rstack = rstack+length*8; /* Set the stack object pointer. */
	rcode = vmPop();
	rretcode = vmPop();
	rip = vmPop();
	rretip = vmPop();
	renv = vmPop();
	rretenv = vmPop();
	r1 = (Obj)1l; /* Let contnuation code know this is a call to the continuation */

	DBEND();
}

void ccSysCreateContinuation (void) {
 Num length;
	DBBEG();
	vmPush(rretenv);
	vmPush(renv);
	vmPush(rretip);
	vmPush(rip);
	vmPush(rretcode);
	vmPush(rcode);
	length = memStackLength(rstack);
	objNewVector(length);
	memcpy(r0, rstack+ObjSize, length*ObjSize);
	vmPop(); vmPop(); vmPop(); vmPop(); vmPop(); vmPop();

	/* Need to protect register rcode/r1e since we are creating
	   a new code block at runtime. [TODO ccAsmIGraph should not clobber rcode] */
	vmPush(rcode);

	ccStart();
	ccAsm(
		MVI, R3, r0,  /* Copy of stack moved to r3 at runtime */
		SYSI, (Obj)ccSysReinstateContinuation);
	ccAsmIGraph();
	r1 = r0; /* Move new code block to r0 */

	rcode = vmPop();

	sysNewClosure1Env();
	memVectorSet(r0, 1, rtge); /* Set to TGE just in case. */

	r1 = 0l; /* Let continuation code know this is a call to capture the continuation and to pass it to fn argument  */

	DBEND();
}

/* At this point evaluating (fn).  Want to pass it code that when
   called will revert to this continuation point passing back
   whatever parameter it was given.

   Emit code that stores continuation.  Tricky.  What should the IP
   register value be?  The code register is obvious.  It'll probably
   be a static offset.

   Emit code that calls the function with the continuation.  This might
   be in a tail context.
*/
void ccDumpHeapHeaders (void) {
	memDebugDumpHeapHeaders(stderr);
}
void ccCallcc (Num flags) {
 Obj Lcontinuationcall;
	DBBEG();
	rexpr = cdr(rexpr); /* Skip over 'call/cc symbol in (call/cc fn)*/

	Lcontinuationcall = ccNewLabel();

	ccAsm(
		SYSI, ccSysCreateContinuation,
		BEQI, R1, 1l, Lcontinuationcall
	);

	/* Is this a tail call?  if not save state. */
	if (!((Num)flags & CCTAILCALL))
		ccAsm (
			PUSH, R1A,
			PUSH, R1B,
			PUSH, R19);

	ccAsm(
		/* Push the continuation just create via ccSysCreateContinuation.  This is the argument to the function */
		PUSH, R0
	);

	rexpr = car(rexpr); /* Consider and compile fn. */
	ccCompileExpr(flags & ~CCTAILCALL);

	/* Setup application to fn */
	ccAsm(
		MVI, R1, 1l
	);

	if ((unsigned)flags & CCTAILCALL) ccAsmTailCall();
	else ccAsmNonTailCall();

	ccAsm(
		LABEL, Lcontinuationcall
	);

	DBEND();
}


void osUnthread (void); /* Refactor this call or ccThread */
void osNewThread (void); /* Refactor this call or ccThread */

void ccThread (void) {
	DBBEG();

	ccStart(); /* Enter a new assembly context */

	/* Compile parameters passed to thread as a begin block emitting the unthread syscall as the last opcode. */
	ccBegin(0);

	ccAsm(
		SYSI, osUnthread
	);
	ccAsmIGraph();

	ccAsm(
		MVI, R0, r0,
		SYSI, osNewThread /* the osNewThread syscall returns thread ID integer object */
	);

	DBEND("  => ");
	DBE objDump(r0, stderr);
}



void ccCombination (Num flags) {
 Int operandCount=0;
	DBBEG();

	if (!((Num)flags & CCTAILCALL)) {
		ccAsm (
			PUSH, R1A, // ip
			PUSH, R1B, // code
			PUSH, R19  // env
		);
	}

	vmPush(car(rexpr)); /* Save expression's operator parameter */

	/* Compile operand expressions. */
	rexpr = cdr(rexpr);
	while (objIsPair(rexpr)) {
		operandCount++;
		vmPush(cdr(rexpr));
		rexpr = car(rexpr);
		ccCompileExpr(flags & ~CCTAILCALL);
		//if (CompError) goto ret;
		ccAsm(PUSH, R0);
		rexpr = vmPop();
	}

	/* Restore and compile operator expression. */
	rexpr = vmPop();
	ccCompileExpr(flags & ~CCTAILCALL);
	//if (CompError) goto ret;

	/* Emit code that applys args to function/code (hopefully) */
	ccAsm (MVI, R1, operandCount);
	if (flags & CCTAILCALL)
		ccAsmTailCall();
	else
		ccAsmNonTailCall();

	DBEND();
}


void ccSelfEvaluating (Num flags) {
	DBBEG();
	ccAsm(MVI, R0, rexpr);
	DBEND();
}


/* Recursive scheme expression compiler.  Translates an expression in
   expr/r15 onto the end of the igraph in igraph/rf.
*/
void ccCompileExpr (Num flags) {
	DBBEG();
	DBE sysDisplay(rexpr, stderr);

	switch (memObjectType(rexpr)) {
		case TSYMBOL: ccSymbol(flags); break;
		case TPAIR  : if      (ssetb      == car(rexpr)) ccSetB(flags);
			           else if (sif        == car(rexpr)) ccIf(flags);
			           else if (scons      == car(rexpr)) ccCons(flags);
			           else if (scar       == car(rexpr)) ccCar(flags);
			           else if (scdr       == car(rexpr)) ccCdr(flags);
			           else if (ssetcarb   == car(rexpr)) ccSetCarB(flags);
			           else if (ssetcdrb   == car(rexpr)) ccSetCdrB(flags);
			           else if (svectorref == car(rexpr)) ccVectorRef(flags);
			           else if (svectorsetb== car(rexpr)) ccVectorSetB(flags);
			           else if (slambda    == car(rexpr)) ccLambda(flags);
			           else if (sbegin     == car(rexpr)) ccBegin(flags);
			           else if (sdefine    == car(rexpr)) ccDefine(flags);
			           else if (snot       == car(rexpr)) ccNot(flags);
			           else if (sor        == car(rexpr)) ccOr(flags);
			           else if (sand       == car(rexpr)) ccAnd(flags);
			           else if (saif       == car(rexpr)) ccAIf(flags);
			           else if (scond      == car(rexpr)) ccCond(flags);
			           else if (sprocedurep== car(rexpr)) ccProcedureP(flags);
			           else if (snullp     == car(rexpr)) ccNullP(flags);
			           else if (spairp     == car(rexpr)) ccPairP(flags);
			           else if (svectorp   == car(rexpr)) ccVectorP(flags);
			           else if (sstringp   == car(rexpr)) ccStringP(flags);
			           else if (sintegerp  == car(rexpr)) ccIntegerP(flags);
			           else if (ssymbolp   == car(rexpr)) ccSymbolP(flags);
			           else if (sportp     == car(rexpr)) ccPortP(flags);
			           else if (seofobjectp== car(rexpr)) ccEOFObjectP(flags);
			           else if (slet       == car(rexpr)) ccLet(flags);
			           else if (sletrec    == car(rexpr)) ccLetrec(flags);
			           else if (squasiquote== car(rexpr)) ccQuasiquote(flags);
			           else if (squote     == car(rexpr)) ccQuote();
			           else if (sapply     == car(rexpr)) ccApply(flags);
			           else if (seval      == car(rexpr)) ccEval(flags);
			           else if (smacro     == car(rexpr)) ccMacro(flags);
			           else if (scallcc    == car(rexpr)) ccCallcc(flags);
			           else if (sthread    == car(rexpr)) ccThread();
			           else if (srem       == car(rexpr)) ;
			           else ccCombination(flags);
			           break;
		default     : ccSelfEvaluating(flags);
	}

	DBEND();
}



/* Compiles the expression in r0 expr/r15 into an intermediate graph then
   asembles the igraph into a VM code object.
           r0 <= scheme expression to compile
      rexpr/rf = temp
  rcodenew/r11 = temp
            r0 => VM code block
*/
void ccCompile (void) {
	DBBEG();

	rexpr = r0;

	ccAsmInit();
	ccCompileExpr(0);
	/* Finalize the iblock with an iblock containing the VM quit op TODO this will become a RET for
	   the real version */
	ccAsm(RET);
	ccAsmIGraph();
//ccDumpIBlocks();

	DBE vmDebugDumpCode(rcodenew, stderr);
	DBEND();
}



/*******************************************************************************
 Debugging
*******************************************************************************/
void ccDumpICodeFieldsImm (Obj c) {
 Obj r;
	r = ccICodeField(c, 1);
	sysDisplay(r, stderr);
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
	sysDisplay(r, stderr);
}

void ccDumpICodeFieldsRegImmImm (Obj c) {
 Obj r;
	r = ccICodeField(c, 1);
	assert(r <= R1F);
	fprintf(stderr, "$"HEX" ", r);

	r = ccICodeField(c, 2);
	sysDisplay(r, stderr);

	fprintf(stderr, " ");
	r = ccICodeField(c, 3);
	sysDisplay(r, stderr);
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
				sysWrite(ic, stderr);
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
		else sysDisplay(block, stderr);
		fprintf (stderr, "]");
	}

	/* Conditional block */
	block = ccIBlockGetConditionalBlock(ib);
	if (false==block)
		fprintf(stderr, "  [---]");
	else {
		fprintf (stderr, "  [");
		if (ccIsObjectTypeIBlock(block)) fprintf(stderr, HEX03, ccIBlockID(block));
		else sysDisplay(block, stderr);
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
	for (i=iblockOffset; i<IBlockCount; ++i)
		ccDumpIBlock(ccIBlock(i));
}



/*******************************************************************************
 Init
*******************************************************************************/
void MysyscallDisplay (void) {
	while (r1--) sysDisplay(vmPop(), stderr);
}

void ccInitialize (void) {
 static Num shouldInitialize=1;
	DBBEG();
	if (shouldInitialize) {
		DB("Activating module");
		shouldInitialize=0;
		DB("Initializing submodules");
		sysInitialize (); /* objInitialize -> vmInitialize -> memInitialize */
		DB("Registering types");
		memTypeRegisterString(TICODE, "icode");
		memTypeRegisterString(TIBLOCK, "iblock");
		DB("Registering static pointer description strings");
		memPointerRegister(objCons10);
		memPointerRegister(sysNewClosure1Env); 
		memPointerRegister(ccSysCompile); 
		memPointerRegister(ccSysReinstateContinuation); 
		memPointerRegister(ccSysCreateContinuation); 
		memPointerRegister(osNewThread); 
		memPointerRegister("Too many arguments to closure"); 
		memPointerRegister(ccError);

		DB("Creating the global environment with predefined symbols 'x' and 'y'");
		objNewSymbol((Str)"~TGE~", 5);
		r1=r0;  r2=null;  objCons12();  renv=rtge=r0;

		sysDefineSyscall(MysyscallDisplay, "display");

		DB("Extending faux local environment");
		r1=renv; /* Keep track of parent env */
		objNewVector(4);
		renv = r0;
		memVectorSet(renv, 0, r1);
		r1 = null;
		objNewSymbol((Str)"yyy", (Num)3); objCons01(); r1 = r0;
		objNewSymbol((Str)"xxx", (Num)3); objCons01();
		memVectorSet(renv, 1, r0);
		objNewInt(420);
		memVectorSet(renv, 2, r0);
		objNewInt(690);
		memVectorSet(renv, 3, r0);

		DB("Extending faux local environment");
		r1=renv; /* Keep track of parent env */
		objNewVector(4);
		renv = r0;
		memVectorSet(renv, 0, r1);
		r1 = null;
		objNewSymbol((Str)"yy", (Num)2); objCons01(); r1 = r0;
		objNewSymbol((Str)"xx", (Num)2); objCons01();
		memVectorSet(renv, 1, r0);
		objNewInt(42);
		memVectorSet(renv, 2, r0);
		objNewInt(69);
		memVectorSet(renv, 3, r0);

		DB("Extending faux local environment");
		r1=renv; /* Keep track of parent env */
		objNewVector(4);
		renv = r0;
		memVectorSet(renv, 0, r1);
		r1 = null;
		objNewSymbol((Str)"y", (Num)1); objCons01(); r1 = r0;
		objNewSymbol((Str)"x", (Num)1); objCons01();
		memVectorSet(renv, 1, r0);
		objNewInt(4);
		memVectorSet(renv, 2, r0);
		objNewInt(6);
		memVectorSet(renv, 3, r0);

		objNewInt(4242); sysDefine ("X"); /* It's always nice to have x and y defined with useful values */
		objNewInt(6969); sysDefine ("Y"); /* It's always nice to have x and y defined with useful values */
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
