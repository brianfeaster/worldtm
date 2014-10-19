#define DEBUG 0
#define DB_DESC "ASM "
#include "debug.h"
#include <stdarg.h>
#include <fcntl.h>
#include <assert.h>
#include "mem.h"
#include "vm.h"
#include "obj.h"
#include "asm.h"

#define NOT_GARBAGE 1 \\ #undefine LOLZ = 69 (((((;

/*
TABLE OF CONTENTS
 Registers
 I_Code
 Igraph_and_iblocks
 Labels
 ASM
 Assemble
 Register_Allocation
 Debugging
 Init

TERMS
  I-Graph   Intermediate graph composed of I-blocks
  I-Block   I-graph node composed of a list of incoming iblocks, outgoing iblocks and icode statements
  I-Code    I-block statement composed of multiple code fields
  I-I       "Do you really have an I-I?" --Fuzzy   "I do now."--Shrewm
*/


/* Rootset objects
*/
Obj ropcodes, riblock, riblocks, ricodes, rlabels, rexpr, rcodenew;


/* Local static scheme symbol used to delimit assembly opcodes */
Obj sasmend, sasmna;


/* Object types used by compiler
*/
#define TIBLOCK  0x87l
#define TEXTENT  0x88l
#define TICODE   0x89l
Num asmIsObjectTypeICode  (Obj ic) { return memIsObjectType(ic, TICODE); }
Num asmIsObjectTypeIBlock (Obj ib) { return memIsObjectType(ib, TIBLOCK); }


/* Debuging flags
*/
#define REVERSE_REGS 0x01
#define FORWARD_REGS 0x02
void asmDumpIBlocksFlags (Num flags);

/* Forward declaration
*/
void asmDumpIBlocks (void);
void asmDumpICode (Obj ic);
void asmPrintIblock (Obj ib);
void asmDumpIBlockParentAndChildren (Obj ib);



/*******************************************************************************
 Registers

 DB of the 4096 register indices.  
   256 Hard register  (16 object $0-$f and 16 immediate %0-%f in current virtual machine)
  3840 Pseudo registers.  (1792 object $000-$6ff.   2048 immediate %000-%7ff)
*******************************************************************************/
#define HARD_OREGISTER_FIRST_VALID 0x000
#define HARD_OREGISTER_LAST_VALID  0x00f

#define HARD_IREGISTER_FIRST_VALID 0x010
#define HARD_IREGISTER_LAST_VALID  0x01f

#define OREGISTER_FIRST_VALID 0x100
#define OREGISTER_LAST_VALID  0x7ff

#define IREGISTER_FIRST_VALID 0x800
#define IREGISTER_LAST_VALID  0xfff
Num OregisterCount;
Num IregisterCount;

/* Generate new pseudo object or intermediate register
*/
Num asmNewOregister() {
	assert(OregisterCount < OREGISTER_LAST_VALID);
	return OregisterCount++;
}
Num asmNewIregister() {
	assert(IregisterCount < IREGISTER_LAST_VALID);
	return IregisterCount++;
}

Num asmRegIsHardOrPseudo (Obj o) { return (Num)o <= IREGISTER_LAST_VALID; }
Num asmRegIsHard         (Obj o) { return ((Num)o < OREGISTER_FIRST_VALID); }
Num asmRegIsHardObj      (Obj o) { return ((Num)o <=  0xf); }
Num asmRegIsHardInt      (Obj o) { return (0x10 <= (Num)o) && ((Num)o <= 0x1f); }
Num asmRegIsPseudo       (Obj r) { return (OREGISTER_FIRST_VALID <= (Num)r) && ((Num)r <= IREGISTER_LAST_VALID); }
Num asmRegIsPseudoObj    (Obj r) { return (OREGISTER_FIRST_VALID <= (Num)r) && ((Num)r <= OREGISTER_LAST_VALID); }
Num asmRegIsPseudoInt    (Obj o) { return (IREGISTER_FIRST_VALID <= (Num)o) && ((Num)o <= IREGISTER_LAST_VALID); }

void asmRegToString (char *buff, Obj r) {
	if      (asmRegIsPseudoObj(r)) sprintf(buff, "$"HEX03, r - OREGISTER_FIRST_VALID);
	else if (asmRegIsPseudoInt(r)) sprintf(buff, "%%"HEX03, r - IREGISTER_FIRST_VALID);
	else if (asmRegIsHardObj(r))   sprintf(buff, "$"HEX, r);
	else if (asmRegIsHardInt(r))   sprintf(buff, "%%"HEX, r - HARD_IREGISTER_FIRST_VALID);
	else *buff = 0;
}



/*******************************************************************************
 Register_Extents

 Lists of register extents.  (initial live-set final-set)

*******************************************************************************/
#define REG_EXTENT_INITIAL 0
#define REG_EXTENT_LIVE    1
#define REG_EXTENT_FINAL   2
#define  REG_EXTENT_SIZE  3

Obj asmRegExtentNew (void) {
 Obj o = memNewVector(TEXTENT, REG_EXTENT_SIZE);
	memVectorSet(o, REG_EXTENT_INITIAL, onull);
	memVectorSet(o, REG_EXTENT_LIVE,    onull);
	memVectorSet(o, REG_EXTENT_FINAL,   onull);
	return o;
}
void asmDisplayTypeExtent (Obj o, FILE *stream) {
 Obj oo;
 char buff[8], c='(';
	asmRegToString(buff, memVectorObject(o, 0));
	fprintf(stream, " %s ", *buff?buff:"#f");

	oo = memVectorObject(o, 1);
	if (onull != oo) {
		while (onull != oo)  {
			asmRegToString(buff, car(oo));
			fprintf(stream, "%c%s", c, buff);
			c = ' ';
			oo = cdr(oo);
		}
		fprintf(stream, ") ");
	} else fprintf(stream, "() ");

	c = '(';
	oo = memVectorObject(o, 2);
	if (onull != oo) {
		while (onull != oo)  {
			asmRegToString(buff, car(oo));
			fprintf(stream, "%c%s", c, buff);
			c = ' ';
			oo = cdr(oo);
		}
		fprintf(stream, ")");
	} else fprintf(stream, "()");

}

Obj asmRegExtentInitial (Obj e) { return memVectorObject(e, REG_EXTENT_INITIAL); }
Obj asmRegExtentLive    (Obj e) { return memVectorObject(e, REG_EXTENT_LIVE); }
Obj asmRegExtentFinal   (Obj e) { return memVectorObject(e, REG_EXTENT_FINAL); }

void asmRegExtentInitialSet (Obj e, Obj o) { memVectorSet(e, REG_EXTENT_INITIAL, o); }
void asmRegExtentLiveSet    (Obj e, Obj o) { memVectorSet(e, REG_EXTENT_LIVE, o); }
void asmRegExtentFinalSet   (Obj e, Obj o) { memVectorSet(e, REG_EXTENT_FINAL, o); }



/*******************************************************************************
 I_Code

 Vector containing: an opcode, 0-3 registers, possible immediate and
 possible branch field.

 New icode objects are pushed to a vector with the count in ICodeCount.  They are
 eventually popped off the vector and added to new iblocks.
*******************************************************************************/
#define ICODE_INDEX_OPCODE           0
#define ICODE_INDEX_REGISTER_A       1
#define ICODE_INDEX_REGISTER_B       2
#define ICODE_INDEX_REGISTER_C       3
#define ICODE_INDEX_IMMEDIATE        4
#define ICODE_INDEX_BRANCH           5
#define ICODE_INDEX_OBJREGSET        6
#define ICODE_INDEX_INTREGSET        7
#define ICODE_INDEX_HARD_REGISTERS   8
#define ICODE_INDEX_PSEUDO_EXTENT    9 // Also the temporary forward reg set
#define  ICODE_OBJECT_SIZE         10

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

Num asmICodeFieldLength (Obj ic) {
	assert(asmIsObjectTypeICode(ic));
	return memObjectLength(ic);
}

Obj asmICodeField (Obj ic, Num i) {
	assert(i < asmICodeFieldLength(ic));
	return memVectorObject(ic, i);
}

Obj asmIcodeFieldOpcode          (Obj ic) { return asmICodeField(ic, ICODE_INDEX_OPCODE); }
Obj asmIcodeFieldRegA            (Obj ic) { return asmICodeField(ic, ICODE_INDEX_REGISTER_A); }
Obj asmIcodeFieldRegB            (Obj ic) { return asmICodeField(ic, ICODE_INDEX_REGISTER_B); }
Obj asmIcodeFieldRegC            (Obj ic) { return asmICodeField(ic, ICODE_INDEX_REGISTER_C); }
Obj asmIcodeFieldImmediate       (Obj ic) { return asmICodeField(ic, ICODE_INDEX_IMMEDIATE); }
Obj asmIcodeFieldBranch          (Obj ic) { return asmICodeField(ic, ICODE_INDEX_BRANCH); }
Obj asmIcodeFieldHardRegisters   (Obj ic) { return asmICodeField(ic, ICODE_INDEX_HARD_REGISTERS); }
Obj asmIcodeFieldPseudoExtent    (Obj ic) { return asmICodeField(ic, ICODE_INDEX_PSEUDO_EXTENT); }

Obj asmIcodeFieldPseudoExtentInitial (Obj ic) { return asmRegExtentInitial(asmICodeField(ic, ICODE_INDEX_PSEUDO_EXTENT)); }
Obj asmIcodeFieldPseudoExtentLive    (Obj ic) { return asmRegExtentLive(asmICodeField(ic, ICODE_INDEX_PSEUDO_EXTENT)); }
Obj asmIcodeFieldPseudoExtentFinal   (Obj ic) { return asmRegExtentFinal(asmICodeField(ic, ICODE_INDEX_PSEUDO_EXTENT)); }

void asmICodeFieldSet (Obj ic, Num i, Obj o) {
	assert(i < asmICodeFieldLength(ic));
	return memVectorSet(ic, i, o);
}

void asmIcodeFieldPseudoExtentInitialSet (Obj ic, Obj o) { asmRegExtentInitialSet(asmICodeField(ic, ICODE_INDEX_PSEUDO_EXTENT), o); }

void asmIcodeFieldHardRegistersSet    (Obj ic, Num i, Obj o) { memVectorSet(asmICodeField(ic, ICODE_INDEX_HARD_REGISTERS), i, o); }
void asmIcodeFieldPseudoExtentLiveSet (Obj ic, Obj o)        { asmRegExtentLiveSet(asmICodeField(ic, ICODE_INDEX_PSEUDO_EXTENT), o); }
void asmIcodeFieldPseudoExtentFinalSet(Obj ic, Obj o)        { asmRegExtentFinalSet(asmICodeField(ic, ICODE_INDEX_PSEUDO_EXTENT), o); }


void asmICodeSetField (Obj icode, Num idx, Obj o) {
	assert(asmIsObjectTypeICode(icode));
	memVectorSet(icode, idx, o);
}

/* Return the hard register's state
*/
Obj asmIcodeHardRegistersState (Obj ic, Obj reg) {
	if (asmRegIsHardObj(reg))
		return memVectorObject(asmIcodeFieldHardRegisters(ic), (Num)reg);
	else
		return memVectorObject(asmIcodeFieldHardRegisters(ic), 5 + (Num)reg - HARD_IREGISTER_FIRST_VALID);
}


void asmNewICode (Obj op, Obj r, Obj s, Obj t, Obj i, Obj b) {
	memRootSetAddressRegister(&i); // Immediate value might be a scheme object and moved during a GC within memNewVector()

	vmPush(asmRegExtentNew());

	objNewVector(10); // The hard registers state vector
	vmPush(r00);

	objNewVector(2); // Forward intermediate register structure
	memVectorSet(r00, 0, onull);
	memVectorSet(r00, 1, onull);
	vmPush(r00);

	objNewVector(2); // Reverse intermediate register structure
	memVectorSet(r00, 0, onull);
	memVectorSet(r00, 1, onull);
	vmPush(r00);

	r00 = memNewVector(TICODE, ICODE_OBJECT_SIZE);
	memVectorSet(r00, ICODE_INDEX_OPCODE,      op);/* Opcode */
	memVectorSet(r00, ICODE_INDEX_REGISTER_A,  r); /* Register 0 */
	memVectorSet(r00, ICODE_INDEX_REGISTER_B,  s); /* Register 1 */
	memVectorSet(r00, ICODE_INDEX_REGISTER_C,  t); /* Register 2 */
	memVectorSet(r00, ICODE_INDEX_IMMEDIATE,   i); /* Immediate value */
	memVectorSet(r00, ICODE_INDEX_BRANCH,      b); /* Branch offset */
	memVectorSet(r00, ICODE_INDEX_OBJREGSET, vmPop()); /* Vector of object register related data */
	memVectorSet(r00, ICODE_INDEX_INTREGSET, vmPop()); /* Vector of integer register realted data */
	memVectorSet(r00, ICODE_INDEX_HARD_REGISTERS, vmPop()); /* Vector of integer register realted data */
	memVectorSet(r00, ICODE_INDEX_PSEUDO_EXTENT, vmPop()); /* Vector of integer register realted data */

	asmICodePush(r00);

	memRootSetAddressUnRegister(&i);
}
	
void asmICodePushNewANDI (Obj r, Obj i)       { asmNewICode(ANDI, r, NA, NA,  i, NA); }
void asmICodePushNewLSLI (Obj r, Obj i)       { asmNewICode(LSLI, r, NA, NA,  i, NA); }
void asmICodePushNewLSRI (Obj r, Obj i)       { asmNewICode(LSRI, r, NA, NA,  i, NA); }
void asmICodePushNewADD  (Obj r, Obj s, Obj t){ asmNewICode(ADD,  r,  s,  t, NA, NA); }
void asmICodePushNewADDI (Obj r, Obj s, Obj i){ asmNewICode(ADDI, r,  s, NA,  i, NA); }
void asmICodePushNewMUL  (Obj r, Obj s, Obj t){ asmNewICode(MUL,  r,  s,  t, NA, NA); }
void asmICodePushNewMULI (Obj r, Obj s, Obj i){ asmNewICode(MULI, r,  s, NA,  i, NA); }
void asmICodePushNewMVI  (Obj r, Obj i)       { asmNewICode(MVI,  r, NA, NA,  i, NA); }
void asmICodePushNewMV   (Obj r, Obj s)       { asmNewICode(MV,   r,  s, NA, NA, NA); }
void asmICodePushNewLDI  (Obj r, Obj s, Obj i){ asmNewICode(LDI,  r,  s, NA,  i, NA); }
void asmICodePushNewLD   (Obj r, Obj s, Obj t){ asmNewICode(LD,   r,  s,  t, NA, NA); }
void asmICodePushNewPOP  (Obj r)              { asmNewICode(POP,  r, NA, NA, NA, NA); }
void asmICodePushNewPUSH (Obj r)              { asmNewICode(PUSH, r, NA, NA, NA, NA); }
void asmICodePushNewSTI  (Obj r, Obj s, Obj i){ asmNewICode(STI,  r,  s, NA,  i, NA); }
void asmICodePushNewST   (Obj r, Obj s, Obj t){ asmNewICode(ST,   r,  s,  t, NA, NA); }
void asmICodePushNewBLTI (Obj r, Obj i, Obj l){ asmNewICode(BLTI, r, NA, NA,  i,  l); }
void asmICodePushNewBGTI (Obj r, Obj i, Obj l){ asmNewICode(BGTI, r, NA, NA,  i,  l); }
void asmICodePushNewBGT  (Obj r, Obj s, Obj l){ asmNewICode(BGT,  r,  s, NA, NA,  l); }
void asmICodePushNewBEQI (Obj r, Obj i, Obj l){ asmNewICode(BEQI, r, NA, NA,  i,  l); }
void asmICodePushNewBNEI (Obj r, Obj i, Obj l){ asmNewICode(BNEI, r, NA, NA,  i,  l); }
void asmICodePushNewBRA  (Obj l)              { asmNewICode(BRA, NA, NA, NA, NA,  l); }
void asmICodePushNewJMP  (Obj r)              { asmNewICode(JMP,  r, NA, NA, NA, NA); }
void asmICodePushNewJAL  (Obj r)              { asmNewICode(JAL,  r, NA, NA, NA, NA); }
void asmICodePushNewRET  (void)               { asmNewICode(RET, NA, NA, NA, NA, NA); }
void asmICodePushNewSYS  (Obj r)              { asmNewICode(SYS,  r, NA, NA, NA, NA); }
void asmICodePushNewSYSI (Obj i)              { asmNewICode(SYSI,NA, NA, NA,  i, NA); }
void asmICodePushNewNOP  (void)               { asmNewICode(NOP, NA, NA, NA, NA, NA); }
void asmICodePushNewQUIT (void)               { asmNewICode(QUIT,NA, NA, NA, NA, NA); }

/* Return number of words this icode requires after assembly.
*/
Num asmICodeOpcodeSize (Obj icode) {
	assert(asmIsObjectTypeICode(icode));
	if (NA == asmIcodeFieldOpcode(icode)) return 0; /* Ignore NAs since they're not emitted */
	return 1 + (Num)(NA != asmIcodeFieldImmediate(icode)) + (Num)(NA != asmIcodeFieldBranch(icode));
}

Obj asmIcodeRegisterAssigned (Obj icode) {
	return asmIcodeFieldOpcode(icode) < PUSH ? asmIcodeFieldRegA(icode) : NA;
}

/* Is the register assigned in this icode?  Could be referenced.
*/
Num asmIcodeIsRegisterAssigned (Obj icode, Obj reg) {
	return (asmIcodeFieldOpcode(icode) < PUSH) && (asmIcodeFieldRegA(icode) == reg);
}


/* Is the register referenced in this icode?  Could be assigned.
*/
Num asmIcodeIsRegUsed (Obj icode, Obj reg) {
	return (asmIcodeFieldRegA(icode) == reg && (PUSH <= asmIcodeFieldOpcode(icode))) ||
	       asmIcodeFieldRegB(icode) == reg ||
	       asmIcodeFieldRegC(icode) == reg;
}

Obj asmIcodeObjRegSetIRegList (Obj icode) { return memVectorObject(memVectorObject(icode, ICODE_INDEX_OBJREGSET), 0); }
Obj asmIcodeObjRegSetRegList (Obj icode)  { return memVectorObject(memVectorObject(icode, ICODE_INDEX_OBJREGSET), 1); }
Obj asmIcodeIntRegSetIRegList (Obj icode) { return memVectorObject(memVectorObject(icode, ICODE_INDEX_INTREGSET), 0); }
Obj asmIcodeIntRegSetRegList (Obj icode)  { return memVectorObject(memVectorObject(icode, ICODE_INDEX_INTREGSET), 1); }

void asmIcodeObjRegSetIRegListSet (Obj icode, Obj lst) { memVectorSet(memVectorObject(icode, ICODE_INDEX_OBJREGSET), 0, lst); }
void asmIcodeObjRegSetRegListSet (Obj icode, Obj lst)  { memVectorSet(memVectorObject(icode, ICODE_INDEX_OBJREGSET), 1, lst); }
void asmIcodeIntRegSetIRegListSet (Obj icode, Obj lst) { memVectorSet(memVectorObject(icode, ICODE_INDEX_INTREGSET), 0, lst); }
void asmIcodeIntRegSetRegListSet (Obj icode, Obj lst)  { memVectorSet(memVectorObject(icode, ICODE_INDEX_INTREGSET), 1, lst); }

/* For this icode, set the hard register's assignment or reference state.
*/
void asmHardRegStateSetAssigned(Obj icode, Obj reg, Obj ass) {
 Obj stateVec = asmIcodeFieldHardRegisters(icode);
 Obj p;
 Num idx = asmRegIsHardObj(reg) ? (Num)reg : 5 + (Num)reg - HARD_IREGISTER_FIRST_VALID;
	p = memVectorObject(stateVec, idx);
	if (0 == p) {
		p = objCons(ofalse, ofalse);
		memVectorSet(stateVec, idx, p);
	}
	memVectorSet(p, 0, ass);
}
void asmHardRegStateSetReferenced(Obj icode, Obj reg, Obj ass) {
 Obj stateVec = asmIcodeFieldHardRegisters(icode);
 Obj p;
 Num idx = asmRegIsHardObj(reg) ? (Num)reg : 5 + (Num)reg - HARD_IREGISTER_FIRST_VALID;
	p = memVectorObject(stateVec, idx);
	if (0 == p) {
		p = objCons(ofalse, ofalse);
		memVectorSet(stateVec, idx, p);
	}
	memVectorSet(p, 1, ass);
}

Str asmHardRegStateToString (Obj state) {
 static char buff[16];
	if (0 == state) {
		sprintf(buff, "   \\   ");
	} else {
		asmRegToString(buff+8, car(state));
		asmRegToString(buff+12, cdr(state));
		sprintf(buff, "%3s\\%-3s", buff+8, buff+12);
	}
	return (Str)buff;
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

#define IBLOCK_INDEX_ID           0
#define IBLOCK_INDEX_TAG          1 // Used to "color" the igraph when traversing around it.
#define IBLOCK_INDEX_INCOMING     2
#define IBLOCK_INDEX_ICODES       3
#define IBLOCK_INDEX_DEFAULT      4
#define IBLOCK_INDEX_CONDITIONAL  5
#define IBLOCK_INDEX_FORWARD_REGS 6
#define IBLOCK_INDEX_REVERSE_REGS 7
#define IBLOCK_OBJECT_SIZE 8

Num asmIBlockID               (Obj ib) { return (Num)memVectorObject(ib, IBLOCK_INDEX_ID); }
Obj asmIBlockTag              (Obj ib) { return memVectorObject(ib, IBLOCK_INDEX_TAG); }
Obj asmIBlockIncomingList     (Obj ib) { return memVectorObject(ib, IBLOCK_INDEX_INCOMING); }
Obj asmIBlockIcodes           (Obj ib) { return memVectorObject(ib, IBLOCK_INDEX_ICODES); }
Obj asmIBlockDefaultTag       (Obj ib) { return memVectorObject(ib, IBLOCK_INDEX_DEFAULT); }
Obj asmIBlockConditionalTag   (Obj ib) { return memVectorObject(ib, IBLOCK_INDEX_CONDITIONAL); }
Obj asmIblockForwardRegisters (Obj ib) { return memVectorObject(ib, IBLOCK_INDEX_FORWARD_REGS); }
Obj asmIblockReverseRegisters (Obj ib) { return memVectorObject(ib, IBLOCK_INDEX_REVERSE_REGS); }

Num asmIBlockICodeLength (Obj ib) {
	assert(asmIsObjectTypeIBlock(ib));
	return memObjectLength(asmIBlockIcodes(ib));
}

/* Get ith icode object in iblock 
*/
Obj asmIBlockICode (Obj ib, Num i) {
	return (i < asmIBlockICodeLength(ib)) ? memVectorObject(asmIBlockIcodes(ib), i) : ofalse;
}

/* Get last icode object in iblock .  #f if 0 length iblock
  DEPRECATED?
*/
Obj asmIBlockLastICode (Obj ib) {
	return asmIBlockICode(ib, asmIBlockICodeLength(ib) - 1);
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

/* Given an iblock ID, return the next valid live iblock based on
   incrementing ID numbers.
*/
Obj asmIBlockNextValidIblock (Obj ib) {
 Num id = asmIBlockID(ib);
	while (++id < IBlockCount) {
		ib = asmIBlock(id);
		if (otrue == asmIBlockTag(ib)) return ib;
	}
	return ofalse;
}

/* Set iblock's various tag values
*/

void asmIBlockTagSet (Obj ib, Obj tag) { memVectorSet(ib, IBLOCK_INDEX_TAG, tag); }
void asmIBlockDefaultTagSet (Obj ib, Obj tag) { memVectorSet(ib, IBLOCK_INDEX_DEFAULT, tag); }
void asmIBlockConditionalTagSet (Obj ib, Obj tag) { memVectorSet(ib, IBLOCK_INDEX_CONDITIONAL, tag); }

/*   r01 = temp
     r02 = temp
     r03 = temp
*/
void asmIBlockIncomingListAdd (Obj ib, Obj o) {
	r03 = ib;
	r01 = o;
	r02 = memVectorObject(r03, IBLOCK_INDEX_INCOMING);
	objCons012();
	memVectorSet(r03, IBLOCK_INDEX_INCOMING, r00);
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
	assert(offset < asmIBlockICodeLength(riblock));
	memVectorSet(asmIBlockIcodes(riblock), offset, op);
}

void asmIblockForwardRegistersSet (Obj ib, Obj regs) { memVectorSet(ib, IBLOCK_INDEX_FORWARD_REGS, regs); }
void asmIblockReverseRegistersSet (Obj ib, Obj regs) { memVectorSet(ib, IBLOCK_INDEX_REVERSE_REGS, regs); }


/* Create new igraph block. Increments IBlockCount which is
   also used as the block's ID.
    iblock => iblock object
*/
void asmGenerateNewIBlock (Num icodeSize) {
	// Create and push icode vector which might actually be empty so explicitly use the empty vector.
	if (icodeSize) {
		objNewVector(icodeSize);
		vmPush(r00);
	} else {
		vmPush(onullvec);
	}

	riblock = memNewVector(TIBLOCK, IBLOCK_OBJECT_SIZE);

	/* Unique ID number set automatically */
	memVectorSet(riblock, IBLOCK_INDEX_ID, (Obj)IBlockCount);

	/* Tag defaults to #f */
	memVectorSet(riblock, IBLOCK_INDEX_TAG, ofalse);

	/* Incoming iblock list defaults to empty list */
	memVectorSet(riblock, IBLOCK_INDEX_INCOMING, onull);

	/* Set the currently empty icode vector */
	memVectorSet(riblock, IBLOCK_INDEX_ICODES, vmPop());

	/* Outgoing default and conditional branch iblocks */
	memVectorSet(riblock, IBLOCK_INDEX_DEFAULT, ofalse);
	memVectorSet(riblock, IBLOCK_INDEX_CONDITIONAL, ofalse);

	/* Outgoing lists of intermediate (object and integer)
	   registers.  Initially empty */
	asmIblockForwardRegistersSet (riblock, ofalse);
	asmIblockReverseRegistersSet (riblock, ofalse);

	/* Append to igraph vector */
	memVectorSet(riblocks, IBlockCount, riblock);

	++IBlockCount;
}



/*******************************************************************************
 Labels

 DB of iblock numbers.  Used to associate a label, just a number, with an iblock
 index, a number.  The label index is set by the assembler to an iblock.
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
	if (LabelsCount <= i) return (Num)-1; /* Invalid label */

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
		__builtin_trap(); //*(int*)0=0;
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
	rip = 0;
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

	/* Reset object/intermediate register counter */
   OregisterCount = OREGISTER_FIRST_VALID;
   IregisterCount = IREGISTER_FIRST_VALID;

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

   The macro asmAsm(...) is generally used.
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
		DB ("["HEX03  HEX"]", OpcodesCount, obj);
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
		} else if (ANDI == obj) {
			r = asmOpcodesNext();
			o = asmOpcodesNext();
			DB("andi["HEX" "HEX"]", r, o);
			asmICodePushNewANDI(r, o);
		} else if (LSLI == obj) {
			r = asmOpcodesNext();
			o = asmOpcodesNext();
			DB("lsli["HEX" "HEX"]", r, o);
			asmICodePushNewLSLI(r, o);
		} else if (LSRI == obj) {
			r = asmOpcodesNext();
			o = asmOpcodesNext();
			DB("lsri["HEX" "HEX"]", r, o);
			asmICodePushNewLSRI(r, o);
		} else if (ADD == obj) {
			r = asmOpcodesNext();
			rr = asmOpcodesNext();
			rrr = asmOpcodesNext();
			DB("add["HEX" "HEX" "HEX"]", r, rr, rrr);
			asmICodePushNewADD(r, rr, rrr);
		} else if (ADDI == obj) {
			r  = asmOpcodesNext();
			rr = asmOpcodesNext();
			i  = asmOpcodesNext();
			DB("addi["HEX" "HEX" "HEX"]", r, rr, i);
			asmICodePushNewADDI(r, rr, i);
		} else if (MUL == obj) {
			r = asmOpcodesNext();
			rr = asmOpcodesNext();
			rrr = asmOpcodesNext();
			DB("mul["HEX" "HEX" "HEX"]", r, rr, rrr);
			asmICodePushNewMUL(r, rr, rrr);
		} else if (MULI == obj) {
			r  = asmOpcodesNext();
			rr = asmOpcodesNext();
			i  = asmOpcodesNext();
			DB("muli["HEX" "HEX" "HEX"]", r, rr, i);
			asmICodePushNewMULI(r, rr, i);
		} else if (BLTI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("blti["HEX" "HEX" "HEX"]", r, i, l);
			asmICodePushNewBLTI(r, i, l);
			asmGenerateIBlockWithPushedIcodes();
			asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */
			asmIBlockConditionalTagSet(riblock, l); /* signal this block conditional is a label */
		} else if (BGTI == obj) {
			r = asmOpcodesNext();
			i = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("bgti["HEX" "HEX" "HEX"]", r, i, l);
			asmICodePushNewBGTI(r, i, l);
			asmGenerateIBlockWithPushedIcodes();
			asmIBlockDefaultTagSet(riblock, otrue); /* signal this block's default is the next one */
			asmIBlockConditionalTagSet(riblock, l); /* signal this block conditional is a label */
		} else if (BGT == obj) {
			r = asmOpcodesNext();
			rr = asmOpcodesNext();
			l = asmOpcodesNext();
			DB("bgt["HEX" "HEX" "HEX"]", r, rr, l);
			asmICodePushNewBGT(r, rr, l);
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
 Num i, error=0;
 Obj field0, field1, field2, field3, field4;
	DBBEG("      iblock="NUM, asmIBlockID(riblock)); 
	/* Re-tag the iblock with its initial location in the code block */
	assert(otrue == asmIBlockTag(riblock));
	asmIBlockTagSet (riblock, (Obj)pccode);

	for (i=0; (!error && (i < asmIBlockICodeLength(riblock))); !error && i++) {
		// Consider next icode and its fields
		r00 = asmIBlockICode(riblock, i);
		field0 = asmICodeField(r00, 0);
		field1 = asmICodeField(r00, 1);
		field2 = asmICodeField(r00, 2);
		field3 = asmICodeField(r00, 3);
		field4 = asmICodeField(r00, 4);

		DB("fields [" HEX SP HEX SP HEX SP HEX SP HEX "]", field0, field1, field2, field3, field4);

		switch ((Num)field0) {
		case (Num)MV:
			switch ((Num)field1) {
				case (Num)R00 : switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmMV_R00_R00); break; // TODO temporary
				               case (Num)R01 : asmEmitOpcode(vmMV_R00_R01); break;
				               case (Num)R02 : asmEmitOpcode(vmMV_R00_R02); break;
				               case (Num)R03 : asmEmitOpcode(vmMV_R00_R03); break;
				               case (Num)R04 : asmEmitOpcode(vmMV_R00_R04); break;
				               case (Num)R0D : asmEmitOpcode(vmMV_R00_R0D); break;
				               case (Num)R0E : asmEmitOpcode(vmMV_R00_R0E); break;
				               case (Num)R10 : asmEmitOpcode(vmMV_R00_R10); break;
				               case (Num)R11 : asmEmitOpcode(vmMV_R00_R11); break;
				               default : error=1; } break;
				case (Num)R01 : switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmMV_R01_R00); break;
				               case (Num)R02 : asmEmitOpcode(vmMV_R01_R02); break;
				               case (Num)R03 : asmEmitOpcode(vmMV_R01_R03); break;
				               default : error=1; }break;
				case (Num)R02 : switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmMV_R02_R00); break;
				               case (Num)R01 : asmEmitOpcode(vmMV_R02_R01); break;
				               case (Num)R03 : asmEmitOpcode(vmMV_R02_R03); break;
				               default : error=1; } break;
				case (Num)R03 : switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmMV_R03_R00); break;
				               case (Num)R01 : asmEmitOpcode(vmMV_R03_R01); break;
				               case (Num)R02 : asmEmitOpcode(vmMV_R03_R02); break;
				               case (Num)R04 : asmEmitOpcode(vmMV_R03_R04); break;
				               default : error=1; } break;
				case (Num)R04 : switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmMV_R04_R00); break;
				               default : error=1; } break;
				case (Num)R05 : switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmMV_R05_R00); break;
				               case (Num)R08 : asmEmitOpcode(vmMV_R05_R08); break;
				               case (Num)R09 : asmEmitOpcode(vmMV_R05_R09); break;
				               case (Num)R0B : asmEmitOpcode(vmMV_R05_R0B); break;
				               case (Num)R0C : asmEmitOpcode(vmMV_R05_R0C); break;
				               default : error=1; } break;
				case (Num)R08: switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmMV_R08_R00); break;
				               default : error=1; } break;
				case (Num)R0B: switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmMV_R0B_R00); break;
				               case (Num)R05 : asmEmitOpcode(vmMV_R0B_R05); break;
				               case (Num)R09 : asmEmitOpcode(vmMV_R0B_R09); break;
				               default : error=1; } break;
				case (Num)R0C: switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmMV_R0C_R00); break;
				               case (Num)R05 : asmEmitOpcode(vmMV_R0C_R05); break;
				               case (Num)R08 : asmEmitOpcode(vmMV_R0C_R08); break;
				               default : error=1; } break;
				case (Num)R10: switch ((Num)field2) {
				               case (Num)R02 : asmEmitOpcode(vmMV_R10_R02); break;
				               case (Num)R11 : asmEmitOpcode(vmMV_R10_R11); break;
				               case (Num)R12 : asmEmitOpcode(vmMV_R10_R12); break;
				               case (Num)R13 : asmEmitOpcode(vmMV_R10_R13); break;
				               default : error=1; } break;
				case (Num)R11: switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmMV_R11_R00); break;
				               case (Num)R01 : asmEmitOpcode(vmMV_R11_R01); break;
				               case (Num)R10 : asmEmitOpcode(vmMV_R11_R10); break;
				               default : error=1; } break;
				case (Num)R12: switch ((Num)field2) {
				               case (Num)R02 : asmEmitOpcode(vmMV_R12_R02); break;
				               case (Num)R10 : asmEmitOpcode(vmMV_R12_R10); break;
				               default : error=1; } break;
				default :error=1; }
			break;
		case (Num)MVI:
			switch ((Num)field1) {
				case (Num)R00 : asmEmitOpcode(vmMV_R00_I); break;
				case (Num)R01 : asmEmitOpcode(vmMV_R01_I); break;
				case (Num)R02 : asmEmitOpcode(vmMV_R02_I); break;
				case (Num)R03 : asmEmitOpcode(vmMV_R03_I); break;
				case (Num)R04 : asmEmitOpcode(vmMV_R04_I); break;
				case (Num)R05 : asmEmitOpcode(vmMV_R05_I); break;
				case (Num)R06 : asmEmitOpcode(vmMV_R06_I); break;
				case (Num)R07 : asmEmitOpcode(vmMV_R07_I); break;
				case (Num)R10 : asmEmitOpcode(vmMV_R10_I); break;
				case (Num)R11 : asmEmitOpcode(vmMV_R11_I); break;
				case (Num)R12 : asmEmitOpcode(vmMV_R12_I); break;
				case (Num)R13 : asmEmitOpcode(vmMV_R13_I); break;
				default :error=1; }
			asmEmitOpcode((Obj)field4);
			break;
		case (Num)LDI:
			switch ((Num)field1) {
				case (Num)R00 : switch ((Num)field2) {
				          case (Num)R00 : asmEmitOpcode(vmLD_R00_R00_I); break;
				          case (Num)R01 : asmEmitOpcode(vmLD_R00_R01_I); break;
				          case (Num)R02 : asmEmitOpcode(vmLD_R00_R02_I); break;
				          case (Num)R0C : asmEmitOpcode(vmLD_R00_R0C_I); break;
				          case (Num)R0B : asmEmitOpcode(vmLD_R00_R0B_I); break;
				          case (Num)R1F : asmEmitOpcode(vmLD_R00_R1F_I); break;
				          default : error=1; } break;
				case (Num)R01 : switch ((Num)field2) {
				          case (Num)R00 : asmEmitOpcode(vmLD_R01_R00_I); break;
				          case (Num)R01 : asmEmitOpcode(vmLD_R01_R01_I); break;
				          case (Num)R0B: asmEmitOpcode(vmLD_R01_R0B_I); break;
				          case (Num)R0C: asmEmitOpcode(vmLD_R01_R0C_I); break;
				          default : error=1; } break;
				case (Num)R02 : switch ((Num)field2) {
				          case (Num)R00 : asmEmitOpcode(vmLD_R02_R00_I); break;
				          case (Num)R01 : asmEmitOpcode(vmLD_R02_R01_I); break;
				          case (Num)R02 : asmEmitOpcode(vmLD_R02_R02_I); break;
				          default : error=1; } break;
				case (Num)R05 : switch ((Num)field2) {
				          case (Num)R00 : asmEmitOpcode(vmLD_R05_R00_I); break;
				          default : error=1; } break;
				case (Num)R0B: switch ((Num)field2) {
				          case (Num)R00 : asmEmitOpcode(vmLD_R0B_R00_I); break;
				          default : error=1; } break;
				case (Num)R0C: switch ((Num)field2) {
				          case (Num)R00 : asmEmitOpcode(vmLD_R0C_R00_I); break;
				          default : error=1; } break;
				case (Num)R10: switch ((Num)field2) {
				          case (Num)R00 : asmEmitOpcode(vmLD_R10_R00_I); break;
				          case (Num)R02 : asmEmitOpcode(vmLD_R10_R02_I); break;
				          case (Num)R01 : asmEmitOpcode(vmLD_R10_R01_I); break;
				          default : error=1; } break;
				case (Num)R11: switch ((Num)field2) {
				          case (Num)R00 : asmEmitOpcode(vmLD_R11_R00_I); break;
				          case (Num)R01 : asmEmitOpcode(vmLD_R11_R01_I); break;
				          case (Num)R02 : asmEmitOpcode(vmLD_R11_R02_I); break;
				          default : error=1; } break;
				case (Num)R12: switch ((Num)field2) {
				          case (Num)R00 : asmEmitOpcode(vmLD_R12_R00_I); break;
				          case (Num)R01 : asmEmitOpcode(vmLD_R12_R01_I); break;
				          default : error=1; } break;
				default :error=1; }
			asmEmitOpcode((Obj)field4);
			break;
		case (Num)LD:
			if      (R00==(Obj)field1 && r00==field2 && R11==field3) asmEmitOpcode(vmLD_R00_R00_R11);
			else if (R00==(Obj)field1 && r01==field2 && R02==field3) asmEmitOpcode(vmLD_R00_R01_R02);
			else if (R01==(Obj)field1 && r01==field2 && R11==field3) asmEmitOpcode(vmLD_R01_R01_R11);
			else error=1;
			break;
		case (Num)STI:
			switch ((Num)field1) {
				case (Num)R00 : switch ((Num)field2) {
				               case (Num)R01 : asmEmitOpcode(vmST_R00_R01_I); break;
				               case (Num)R05: asmEmitOpcode(vmST_R00_R05_I); break;
				               case (Num)R0B: asmEmitOpcode(vmST_R00_R0B_I); break;
				               case (Num)R0C: asmEmitOpcode(vmST_R00_R0C_I); break;
				               case (Num)R1F: asmEmitOpcode(vmST_R00_R1F_I); break;
				               default : error=1; } break;
				case (Num)R01 : switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmST_R01_R00_I); break;
				               case (Num)R02 : asmEmitOpcode(vmST_R01_R02_I); break;
				               default : error=1; } break;
				case (Num)R02 : switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmST_R02_R00_I); break;
				               case (Num)R01 : asmEmitOpcode(vmST_R02_R01_I); break;
				               default : error=1; } break;
				case (Num)R03 : switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmST_R03_R00_I); break;
				               default : error=1; } break;
				case (Num)R05 : switch ((Num)field2) {
				               case (Num)R00 : asmEmitOpcode(vmST_R05_R00_I); break;
				               default : error=1; } break;
				default :error=1; }
			asmEmitOpcode((Obj)field4);
			break;
		case (Num)ST:
			switch ((Num)field1) {
				case (Num)R00 : switch ((Num)field2) {
				               case (Num)R01 : switch ((Num)field3) {
				                              case (Num)R02 :  asmEmitOpcode(vmST_R00_R01_R02); break;
				                              default : error=1; } break;
				               default : error=1; } break;
				default :error=1; } break;
		case (Num)PUSH:
			switch ((Num)field1) {
				case (Num)R00 : asmEmitOpcode(vmPUSH_R00); break;
				case (Num)R01 : asmEmitOpcode(vmPUSH_R01); break;
				case (Num)R02 : asmEmitOpcode(vmPUSH_R02); break;
				case (Num)R03 : asmEmitOpcode(vmPUSH_R03); break;
				case (Num)R04 : asmEmitOpcode(vmPUSH_R04); break;
				case (Num)R05 : asmEmitOpcode(vmPUSH_R05); break;
				case (Num)R07 : asmEmitOpcode(vmPUSH_R07); break;
				case (Num)R09: asmEmitOpcode(vmPUSH_R09); break;
				case (Num)R0A: asmEmitOpcode(vmPUSH_R0A); break;
				case (Num)R0B: asmEmitOpcode(vmPUSH_R0B); break;
				case (Num)R0C: asmEmitOpcode(vmPUSH_R0C); break;
				case (Num)R1C: asmEmitOpcode(vmPUSH_R1C); break;
				default :error=1; } break;
		case (Num)POP:
			switch ((Num)field1) {
				case (Num)R00 : asmEmitOpcode(vmPOP_R00); break;
				case (Num)R01 : asmEmitOpcode(vmPOP_R01); break;
				case (Num)R02 : asmEmitOpcode(vmPOP_R02); break;
				case (Num)R03 : asmEmitOpcode(vmPOP_R03); break;
				case (Num)R04 : asmEmitOpcode(vmPOP_R04); break;
				case (Num)R05 : asmEmitOpcode(vmPOP_R05); break;
				case (Num)R07 : asmEmitOpcode(vmPOP_R07); break;
				case (Num)R09: asmEmitOpcode(vmPOP_R09); break;
				case (Num)R0A: asmEmitOpcode(vmPOP_R0A); break;
				case (Num)R0B: asmEmitOpcode(vmPOP_R0B); break;
				case (Num)R0C: asmEmitOpcode(vmPOP_R0C); break;
				case (Num)R1C: asmEmitOpcode(vmPOP_R1C); break;
				default :error=1; } break;
		case (Num)ANDI:
			switch ((Num)field1) {
				case (Num)R10 : asmEmitOpcode(vmAND_R10_I); break;
				default :error=1; }
			asmEmitOpcode((Obj)field4);
			break;
		case (Num)LSLI:
			switch ((Num)field1) {
				case (Num)R02 : asmEmitOpcode(vmLSL_R02_I); break;
				case (Num)R10 : asmEmitOpcode(vmLSL_R10_I); break;
				case (Num)R11 : asmEmitOpcode(vmLSL_R11_I); break;
				default :error=1; }
			asmEmitOpcode((Obj)field4);
			break;
		case (Num)LSRI:
			switch ((Num)field1) {
				case (Num)R10 : asmEmitOpcode(vmLSR_R10_I); break;
				default :error=1; }
			asmEmitOpcode((Obj)field4);
			break;
		case (Num)ADD:
			if      (R00==field1 && R00==field2 && R01==field3) asmEmitOpcode(vmADD_R00_R00_R01);
			else if (R00==field1 && R00==field2 && R02==field3) asmEmitOpcode(vmADD_R00_R00_R02);

			else if (R01==field1 && R01==field2 && R11==field3) asmEmitOpcode(vmADD_R01_R01_R00);
			else if (R01==field1 && R01==field2 && R11==field3) asmEmitOpcode(vmADD_R01_R01_R02);

			else if (R02==field1 && R02==field2 && R00==field3) asmEmitOpcode(vmADD_R02_R02_R00);
			else if (R02==field1 && R02==field2 && R01==field3) asmEmitOpcode(vmADD_R02_R02_R01);
			else if (R02==field1 && R02==field2 && R03==field3) asmEmitOpcode(vmADD_R02_R02_R03);

			else if (R03==field1 && R03==field2 && R02==field3) asmEmitOpcode(vmADD_R03_R03_R02);

			else if (R10==field1 && R10==field2 && R00==field3) asmEmitOpcode(vmADD_R10_R10_R00);
			else if (R10==field1 && R10==field2 && R11==field3) asmEmitOpcode(vmADD_R10_R10_R11);
			else if (R10==field1 && R10==field2 && R13==field3) asmEmitOpcode(vmADD_R10_R10_R13);

			else if (R11==field1 && R11==field2 && R00==field3) asmEmitOpcode(vmADD_R11_R11_R00);
			else if (R11==field1 && R11==field2 && R11==field3) asmEmitOpcode(vmADD_R11_R11_R10);
			else if (R11==field1 && R11==field2 && R13==field3) asmEmitOpcode(vmADD_R11_R11_R12);

			else if (R12==field1 && R12==field2 && R00==field3) asmEmitOpcode(vmADD_R12_R12_R00);
			else if (R12==field1 && R12==field2 && R10==field3) asmEmitOpcode(vmADD_R12_R12_R10);
			else if (R12==field1 && R12==field2 && R11==field3) asmEmitOpcode(vmADD_R12_R12_R11);

			else if (R13==field1 && R13==field2 && R00==field3) asmEmitOpcode(vmADD_R13_R13_R00);
			else if (R13==field1 && R13==field2 && R10==field3) asmEmitOpcode(vmADD_R13_R13_R10);

			else error=1;
			break;
		case (Num)ADDI:
			if      (R00==field1 && R00==field2) asmEmitOpcode(vmADD_R00_R00_I);
			else if (R01==field1 && R01==field2) asmEmitOpcode(vmADD_R01_R01_I);
			else if (R02==field1 && R02==field2) asmEmitOpcode(vmADD_R02_R02_I);
			else if (R03==field1 && R03==field2) asmEmitOpcode(vmADD_R03_R03_I);
			else if (R10==field1 && R10==field2) asmEmitOpcode(vmADD_R10_R10_I);
			else if (R11==field1 && R11==field2) asmEmitOpcode(vmADD_R11_R11_I);
			else if (R12==field1 && R12==field2) asmEmitOpcode(vmADD_R12_R12_I);
			else if (R1F==field1 && R1F==field2) asmEmitOpcode(vmADD_R1F_R1F_I);
			else error=1;
			asmEmitOpcode(field4);
			break;
		case (Num)MUL:
			if      (R11==field1 && R11==field2 && R10==field3) asmEmitOpcode(vmMUL_R11_R11_R10);
			else if (R12==field1 && R12==field2 && R10==field3) asmEmitOpcode(vmMUL_R12_R12_R10);
			else if (R13==field1 && R13==field2 && R10==field3) asmEmitOpcode(vmMUL_R13_R13_R10);
			else if (R14==field1 && R14==field2 && R10==field3) asmEmitOpcode(vmMUL_R14_R14_R10);
			else error=1;
			break;
		case (Num)BLTI:
			switch ((Num)field1) {
				case (Num)R01 : asmEmitOpcode(vmBLT_R01_I); break;
				case (Num)R10 : asmEmitOpcode(vmBLT_R10_I); break;
				default :error=1; }
			asmEmitOpcode(field4);
			asmEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BGTI:
			switch ((Num)field1) {
				case (Num)R10 : asmEmitOpcode(vmBGT_R10_I); break;
				default :error=1; }
			asmEmitOpcode(field4);
			asmEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BGT:
			switch ((Num)field1) {
				case (Num)R10 : switch((Num)field2) {
				                   case (Num)R11 : asmEmitOpcode(vmBGT_R10_R11); break;
				                   default : error=1; } break;
				default :error=1; }
			asmEmitOpcode((Obj)(-2*8)); /* Branch address left unresolved */
			break;
		case (Num)BEQI:
			switch ((Num)field1) {
				case (Num)R00 : asmEmitOpcode(vmBEQ_R00_I); break;
				case (Num)R01 : asmEmitOpcode(vmBEQ_R01_I); break;
				case (Num)R02 : asmEmitOpcode(vmBEQ_R02_I); break;
				case (Num)R03 : asmEmitOpcode(vmBEQ_R03_I); break;
				case (Num)R07 : asmEmitOpcode(vmBEQ_R07_I); break;
				case (Num)R10 : asmEmitOpcode(vmBEQ_R10_I); break;
				default :error=1; }
			asmEmitOpcode(field4);
			asmEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BNEI:
			switch ((Num)field1) {
				case (Num)R00 : asmEmitOpcode(vmBNE_R00_I); break;
				case (Num)R01 : asmEmitOpcode(vmBNE_R01_I); break;
				case (Num)R02 : asmEmitOpcode(vmBNE_R02_I); break;
				case (Num)R03 : asmEmitOpcode(vmBNE_R03_I); break;
				case (Num)R05 : asmEmitOpcode(vmBNE_R05_I); break;
				default :error=1; }
			asmEmitOpcode(field4);
			asmEmitOpcode((Obj)(-3*8)); /* Branch address left unresolved */
			break;
		case (Num)BRA :
			/* Emitted by parent logic */
			break;
		case (Num)JMP :
			switch ((Num)field1) {
				case (Num)R00 : asmEmitOpcode(vmJMP_R00); break;
				case (Num)R02 : asmEmitOpcode(vmJMP_R02); break;
				default :error=1; } break;
		case (Num)JAL :
			switch ((Num)field1) {
				case (Num)R00 : asmEmitOpcode(vmJAL_R00); break;
				case (Num)R02 : asmEmitOpcode(vmJAL_R02); break;
				default :error=1; } break;
		case (Num)RET :
			asmEmitOpcode(vmRET);
			break;
		case (Num)SYS :
			switch ((Num)field1) {
				case (Num)R00 : asmEmitOpcode(vmSYS_R00); break;
				default :error=1;  } break;
		case (Num)SYSI:
			asmEmitOpcode2(vmSYS_I, field4);
			break;
		case (Num)QUIT:
			asmEmitOpcode(vmQUIT);
			break;
		case (Num)NOP:
			asmEmitOpcode(vmNOP);
			break;
		default:
			if (field0 == NA) break; /* The NA fake opcode is OK.  It's an opcode removed during optimization. */
			else {
				fprintf(stderr, "\nCan't assemble opcode ");
				error=1;
			}
		} // switch field0
	} //for asmIBlockICodeLength

	if (1==error) {
		asmDumpIBlocks();
		fprintf(stderr, "\nUnable to assemble icode "HEX, i);
		asmPrintIblock(riblock);
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
	DBE asmDumpIBlocks();
	DBE objDisplay(rcodenew, stderr);
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
		memVectorSet(rcodenew, opcodeFieldAddr, (Obj)(((Int)defBlockAddr-(Int)opcodeFieldAddr-1+2)*8));
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
 Int adjust;
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
		/* If the branch opcode has no immediate field, adjust the jump offset by -1 */
		adjust = (asmICodeField(asmIBlockLastICode(riblock), ICODE_INDEX_IMMEDIATE) == NA) ? -1 : 0;
 		/* Set the jump-opcode's offset */
		memVectorSet(rcodenew, opcodeFieldAddr, (Obj)(((Int)condBlockAddr-(Int)opcodeFieldAddr-1+3+adjust)*8));
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
//	asmPrintIblock(riblock);
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
	DBE asmPrintIblock(ib);

	r04 = ib; /* Protect object reference from garbage collector */
	tag = asmIBlockDefaultTag(r04);
	if (ofalse != tag) {
		if (otrue == tag) {
			/* Default block is via 'next logical' */
			asmIBlockLinkDefault(asmIBlockID(r04), 1 + asmIBlockID(r04)); 
		} else if (!asmIsObjectTypeIBlock(tag)) { /* Could already be connected if non ASM flow */
			/* Default block is via 'labeled block' */
			asmIBlockLinkDefault(asmIBlockID(r04), asmLabels((Num)tag));
		}
	}

	tag = asmIBlockConditionalTag(r04);
	if (ofalse != tag) {
		/* Conditional block is a labeled block */
		if (!asmIsObjectTypeIBlock(tag)) { /* Could already be connected if non ASM flow */
			asmIBlockLinkConditional(asmIBlockID(r04), asmLabels((Num)tag)); 
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
						DBE asmPrintIblock(riblock);
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
		memRootSetAddressRegister(&mydef); //MEM_ADDRESS_REGISTER(&mydef);
		memRootSetAddressRegister(&lst); //MEM_ADDRESS_REGISTER(&lst);
		memRootSetAddressRegister(&inib); //MEM_ADDRESS_REGISTER(&inib);
		while (onull != lst) {
			inib = car(lst); /* Consider an incoming block */
			if (riblock == asmIBlockDefaultTag(inib)) asmIBlockLinkDefault(asmIBlockID(inib), asmIBlockID(mydef));
			if (riblock == asmIBlockConditionalTag(inib)) asmIBlockLinkConditional(asmIBlockID(inib), asmIBlockID(mydef));
			lst = cdr(lst);
		}
		/* Unregister local C variables */
		memRootSetAddressUnRegister(&inib);
		memRootSetAddressUnRegister(&lst);
		memRootSetAddressUnRegister(&mydef);
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

void asmIdentityOptimization (void) {
 Num changed=0;
 Obj ic;
 Num i, j;
 Obj field0, field1, field2;
	DBBEG();
	/* Consider every live iblock */
	for (i=0; (i < asmIBlockFrameCount()); ++i) {
		riblock = asmIBlockFrame(i);
		if (otrue == asmIBlockTag(riblock)) {
			/* Over every instruction */
			for (j=0; (j < asmIBlockICodeLength(riblock)); ++j) {
				ic = asmIBlockICode(riblock, j);
				field0 = asmICodeField(ic, 0); /* opcode */
				field1 = asmICodeField(ic, 1); /* reg 0 */
				field2 = asmICodeField(ic, 2); /* reg 1 */
				if (MV == field0 && field1 == field2) {
					/* Find a matching push that cancels this pop */
					DB("Omitting MV opcode in block "HEX" and icode "HEX, i, j);
					changed = 1;
					DBE asmPrintIblock(riblock);
					asmIBlockDeleteICode(riblock, j);
				}
			}
		}
	}
	DBEND(" => ", changed);
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
	//fprintf(stderr, "\nWARNING:  Disabled asmPeepHoleOptimization()");
	asmPeepHoleOptimization();
	asmIdentityOptimization();
	asmOptimizeEmptyIBlocks();
}



/*******************************************************************************
 Register_Allocation
*******************************************************************************/

/* Collect this iblock's local (does not consider reverse incoming sets) reverse register extent set.
   r00  --  clobbered
*/
Obj asmGatherIblockLocalRegisterSetReverse (Obj riblock, Obj revInSet) {
 Num i;
 Obj icode, field;
 memRootSetPush(&riblock);
 memRootSetPush(&icode);

	r00 = revInSet; // Empty ordered sets.
	// Over all icodes in reverse
	for (i=1; (i <= asmIBlockICodeLength(riblock)); ++i) {
		icode = asmIBlockICode(riblock, asmIBlockICodeLength(riblock)-i);
		// Over the three reg fields #(-, REG1, REG2, REG3, -, -)
		field = asmIcodeFieldRegA(icode);
		if (asmRegIsHardOrPseudo(field)) {
			if (asmIcodeIsRegisterAssigned(icode, field)) {
 				r00 = objOrderedSetSub0(r00, field);
			} else {
 				r00 = objOrderedSetAdd0(r00, field);
			}
		}

		field = asmIcodeFieldRegB(icode);
		if (asmRegIsHardOrPseudo(field)) { r00 = objOrderedSetAdd0(r00, field); }

		field = asmIcodeFieldRegC(icode);
		if (asmRegIsHardOrPseudo(field)) { r00 = objOrderedSetAdd0(r00, field); }
	}

 memRootSetPop(&icode);
 memRootSetPop(&riblock);
	return r00;
}


/* Scan through all iblocks for the iblock 'ib' placeholder and union its outgoing sets with
   the placeholder iblock's reverse incoming set.
*/
void asmRegisterExtentsReverseUpdatePlaceholder (Obj ib, Obj setObjReg) {
 Obj iblock, regset;
	memRootSetAddressRegister(&ib);
	memRootSetAddressRegister(&setObjReg);
	memRootSetAddressRegister(&iblock);
	memRootSetAddressRegister(&regset);

	for (iblock = asmIBlockFrame(0); // Over all iblocks.  TODO implement asmIBlockFirst() frame 0 might not be the 1st.
	     iblock != ofalse;
	     iblock = asmIBlockNextValidIblock(iblock)) { // TODO when iblock hits ib, stop (since every pass is linear starting at 0)

		// If this iblock has a matching iblock placeholder, remove it and union the two sets
		regset = asmIblockReverseRegisters(iblock);
		if (objOrderedSetIsMember(regset, ib)) {
			regset = objOrderedSetUnion0(regset, setObjReg); regset = objOrderedSetSub0(regset, ib);
			asmIblockReverseRegistersSet(iblock, regset);
		}
	}

	memRootSetAddressUnRegister(&regset);
	memRootSetAddressUnRegister(&iblock);
	memRootSetAddressUnRegister(&setObjReg);
	memRootSetAddressUnRegister(&ib);
}

Num asmRegisterReverseExtentsSweepForPlaceholders (void) {
 Obj iblock;
 Obj forwardSet;
 Obj o;
 Num found=0;
	memRootSetPush(&iblock);
	memRootSetPush(&forwardSet);
	memRootSetPush(&o);

	for (iblock = asmIBlockFrame(0); // Over all iblocks
	     iblock != ofalse;
	     iblock = asmIBlockNextValidIblock(iblock)) {
		forwardSet = asmIblockReverseRegisters(iblock);
		if (onull != forwardSet) {
			o = car(forwardSet);
			if (!asmRegIsHardOrPseudo(o)) {
				found = 1;
				asmRegisterExtentsReverseUpdatePlaceholder(o, asmIblockReverseRegisters(o));
			}
		}
	}

	memRootSetPop(&o);
	memRootSetPop(&forwardSet);
	memRootSetPop(&iblock);
	return found;
}


void asmRegisterExtentsReversePass (void) {
 Obj iblock, reverseSet, inib;
 memRootSetPush(&iblock);
 memRootSetPush(&reverseSet);
 memRootSetPush(&inib);

	for (iblock = asmIBlockFrame(0); // Over all iblocks
	     iblock != ofalse;
	     iblock = asmIBlockNextValidIblock(iblock)) {

		// Add reverse incoming extent set from outgoing blocks back to mine.
		// Reverse incoming iblock's set will be either #f or a list of iregisters and/or iblock placeholders.
		// If a reverse incoming list is #f, then use the reverse incoming iblock as a placeholder.  TODO:  Initialize the set with the iblock itself?
		reverseSet = onull;
		inib = asmIBlockDefaultTag(iblock);
		if ((ofalse != inib) && (inib != iblock)) { // Ignore myself (blocks that branch to themselves)
			r00 = asmIblockReverseRegisters(inib);
			reverseSet = (ofalse == r00)
			          ? objOrderedSetAdd0(reverseSet, inib)   // Add iblock to set as a temporary placeholder
			          : objOrderedSetUnion0(reverseSet, r00); // Union sets
		}

		inib = asmIBlockConditionalTag(iblock);
		if ((ofalse != inib) && (inib != iblock)) { // Ignore myself (blocks that branch to themselves)
			r00 = asmIblockReverseRegisters(inib);
			reverseSet = (ofalse == r00)
			          ? objOrderedSetAdd0(reverseSet, inib)   // Add iblock to set as a temporary placeholder
			          : objOrderedSetUnion0(reverseSet, r00); // Union sets
		}

		reverseSet = asmGatherIblockLocalRegisterSetReverse(iblock, reverseSet); // Consider this iblock's local registers only

		// Set forward register set (might include unprocessed iblocks)
		asmIblockReverseRegistersSet(iblock, reverseSet);

		// If a reverse incoming set includes this block as a place holder, then we can go back and add my outgoing sets to the iblocks with my placeholder (including myself).
		if (objOrderedSetIsMember(reverseSet, iblock)) {
			asmRegisterExtentsReverseUpdatePlaceholder(iblock, reverseSet);
		}

	} // for iblocks


	// Last sweep for forward set placeholders.
	while (asmRegisterReverseExtentsSweepForPlaceholders());

 memRootSetPop(&inib);
 memRootSetPop(&reverseSet);
 memRootSetPop(&iblock);
}



Obj asmGatherIblockLocalRegisterSetForward (Obj riblock) {
 Num i, f;
 Obj icode, field;
 memRootSetPush(&riblock);
 memRootSetPush(&icode);

	r00 = onull; // Empty ordered sets.
	// Over all icodes
	for (i=0; (i < asmIBlockICodeLength(riblock)); ++i) {
		icode = asmIBlockICode(riblock, i);
		// Over the three reg fields #(-, REG1, REG2, REG3, -, -)
		for (f = 1; (f <= 3); ++f) {
			field = asmICodeField(icode, f);
			if (asmRegIsHardOrPseudo(field)) { r00 = objOrderedSetAdd0(r00, field); }
		}
	}

 memRootSetPop(&icode);
 memRootSetPop(&riblock);
	return r00;
}


/* Scan through all iblocks for the iblock 'ib' placeholder and union its outgoing sets with
   the placeholder iblock's outgoing set.
*/
void asmRegisterExtentsForwardUpdatePlaceholder (Obj ib, Obj setObjReg) {
 Obj iblock, regset;
	memRootSetAddressRegister(&ib);
	memRootSetAddressRegister(&setObjReg);
	memRootSetAddressRegister(&iblock);
	memRootSetAddressRegister(&regset);

	for (iblock = asmIBlockFrame(0); // Over all iblocks.  TODO implement asmIBlockFirst() frame 0 might not be the 1st.
	     iblock != ofalse;
	     iblock = asmIBlockNextValidIblock(iblock)) { // TODO when iblock hits ib, stop (since every pass is linear starting at 0)

		// If this iblock has a matching iblock placeholder, remove it and union the two sets
		regset = asmIblockForwardRegisters(iblock);
		if (objOrderedSetIsMember(regset, ib)) {
			regset = objOrderedSetUnion0(regset, setObjReg);
			regset = objOrderedSetSub0(regset, ib);
			asmIblockForwardRegistersSet(iblock, regset);
		}
	}

	memRootSetAddressUnRegister(&regset);
	memRootSetAddressUnRegister(&iblock);
	memRootSetAddressUnRegister(&setObjReg);
	memRootSetAddressUnRegister(&ib);
}

Num asmRegisterExtentsSweepForPlaceholders (void) {
 Obj iblock;
 Obj forwardSet;
 Obj o;
 Num found=0;
	memRootSetPush(&iblock);
	memRootSetPush(&forwardSet);
	memRootSetPush(&o);

	for (iblock = asmIBlockFrame(0); // Over all iblocks
	     iblock != ofalse;
	     iblock = asmIBlockNextValidIblock(iblock)) {
		forwardSet = asmIblockForwardRegisters(iblock);
		if (onull != forwardSet) {
			o = car(forwardSet);
			if (!asmRegIsHardOrPseudo(o)) {
				found = 1;
				asmRegisterExtentsForwardUpdatePlaceholder(o, asmIblockForwardRegisters(o));
			}
		}
	}

	memRootSetPop(&o);
	memRootSetPop(&forwardSet);
	memRootSetPop(&iblock);
	return found;
}

void asmRegisterExtentsForwardPass (void) { // !!! BRAND NEW !!!
 Obj iblock, forwardSet, inib;

	memRootSetPush(&iblock);
	memRootSetPush(&forwardSet);
	memRootSetPush(&inib);

	for (iblock = asmIBlockFrame(0); // Over all iblocks
	     iblock != ofalse;
	     iblock = asmIBlockNextValidIblock(iblock)) {
		// Consider this iblock's local registers only
		forwardSet = asmGatherIblockLocalRegisterSetForward(iblock);

		// Add forward extent set from incoming blocks to mine.
		// Incoming iblock's set will be either #f or a list of iregisters and/or iblock placeholders.
		// If an incoming list is #f, then use the incoming iblock as a placeholder.  TODO:  Initialize the set with the iblock itself?
		for (r01 = asmIBlockIncomingList(iblock); // Over all incoming iblocks
		     r01 != onull;
		     r01 = cdr(r01)) {
			inib = car(r01);
			if (inib == iblock) continue; // Ignore myself (blocks that branch to themselves)
			// Incoming iblock's outgoing iregister list
			r00 = asmIblockForwardRegisters(inib);
			forwardSet = (ofalse == r00)
			          ? objOrderedSetAdd0(forwardSet, inib)   // Add iblock to set as a temporary placeholder
			          : objOrderedSetUnion0(forwardSet, r00); // Union sets
		}

		// Set forward register set (might include unprocessed iblocks)
		asmIblockForwardRegistersSet(iblock, forwardSet);

		// If an incoming set includes this block as a place holder, then we can go back and add my outgoing sets to the iblocks with my placeholder (including myself).
		if (objOrderedSetIsMember(forwardSet, iblock)) {
			asmRegisterExtentsForwardUpdatePlaceholder(iblock, forwardSet);
		}

	} // for iblocks

	// Last sweep for forward set placeholders.
	while (asmRegisterExtentsSweepForPlaceholders());

	memRootSetPop(&inib);
	memRootSetPop(&forwardSet);
	memRootSetPop(&iblock);
}


/* Set each icode's forward iregister extent set based on incoming iregister
   extent sets and the iregistered used by the icode.
*/
void asmIregisterExtentsUpdateIcodes (void) {
 Obj iblock, inIblock, registers=onull, finalRegs, icode, reg, h;
 Num i, f, c, preLen, postLen, oneOrBoth;
	memRootSetAddressRegister(&iblock);
	memRootSetAddressRegister(&inIblock);
	memRootSetAddressRegister(&registers);
	memRootSetAddressRegister(&icode);

	for (iblock = asmIBlockFrame(0); // Over all iblocks.
	     iblock != ofalse;
	     iblock = asmIBlockNextValidIblock(iblock)) {

		if (asmIBlockTag(iblock) == ofalse) continue; // IBlock might have been optimized away.

		// Forward iblock pass

		// Start with union of all incoming iregister extent sets from the parent iblocks
		for (r01 = asmIBlockIncomingList(iblock); // Over all incoming iblocks
		     r01 != onull;
		     r01 = cdr(r01)) {
			inIblock = car(r01);
			registers = objOrderedSetUnion0(registers, asmIblockForwardRegisters(inIblock));
		}

		// Add each icode's iregisters to the extent sets and assign the set to the icode
		for (i=0; (i < asmIBlockICodeLength(iblock)); ++i) { // Over all icodes
			icode = asmIBlockICode(iblock, i);
			for (f = 1; (f <= 3); ++f) { // Over the three reg fields
				// Consider register field of icode #(-, REG1, REG2, REG3, -, -) of iblock
				reg = asmICodeField(icode, f);
				// Add reg to either the object or integer set
				if (asmRegIsHardOrPseudo(reg)) { registers = objOrderedSetAdd0(registers, reg); }
			}

			// Set icode's initial register extent
			reg = asmIcodeRegisterAssigned(icode);
			if (NA != reg) asmIcodeFieldPseudoExtentInitialSet(icode, reg);

			// Set icode's live extent sets
			asmIcodeFieldPseudoExtentLiveSet(icode, registers);
		} // for icodes

		// Reverse iblock pass

		registers = onull;
		inIblock = asmIBlockDefaultTag(iblock); // Gather default block's reverse outgoing set
		if (inIblock != ofalse) {
			registers = objOrderedSetUnion0(registers, asmIblockReverseRegisters(inIblock));
		}

		inIblock = asmIBlockConditionalTag(iblock); // Gather conditional block's reverse outgoing set
		if (inIblock != ofalse) {
			registers = objOrderedSetUnion0(registers, asmIblockReverseRegisters(inIblock));
		}

		// Add each icode's iregisters to the extent sets and assign the set to the icode
		for (i=1; (i <= asmIBlockICodeLength(iblock)); ++i) {
			icode = asmIBlockICode(iblock, asmIBlockICodeLength(iblock) - i);
			for (f = 1; (f <= 3); ++f) { // Over the three reg fields
				reg = asmICodeField(icode, f); // Consider register
				// Add reg to reverse set (remove if assigned in this opcode)
				if (asmRegIsHardOrPseudo(reg)) {
					if ((1 == f) && asmIcodeIsRegisterAssigned(icode, reg)) {
						registers = objOrderedSetSub0(registers, reg);
					} else {
						preLen = objListLength(registers);
						registers = objOrderedSetAdd0(registers, reg);
						postLen = objListLength(registers);
						if (preLen != postLen) {
							finalRegs = asmIcodeFieldPseudoExtentFinal(icode);
							finalRegs = objOrderedSetAdd0(finalRegs, reg);
							asmIcodeFieldPseudoExtentFinalSet(icode, finalRegs);
						}
					}
				}
			}

			// Set icode's register extent sets via intersection of forward and reverse sets.
			registers = objOrderedSetIntersection0(registers, asmIcodeFieldPseudoExtentLive(icode));
			if (NA != (reg = asmIcodeRegisterAssigned (icode))) {
				// Since I'm removing assigned registers from the extent set, add it in just for this icode.
				asmIcodeFieldPseudoExtentLiveSet(icode, objOrderedSetAdd0(registers, reg));
			} else {
				asmIcodeFieldPseudoExtentLiveSet(icode, registers);
			}
		} // for icodes

      // Extract hard registers into the hard register vector while marking their specific state(s) at each icode.
		// Only supports object register $0..$4 and integer registers %0..%4.
		for (i=0; (i < asmIBlockICodeLength(iblock)); ++i) {
			icode = asmIBlockICode(iblock, i);
			registers = asmIcodeFieldPseudoExtentLive(icode);
			c = 0;
			while (onull != registers) {
				h = car(registers);

				if (asmRegIsHard(h)) {
					oneOrBoth = 0;
					if (onull != objMemq(h, asmIcodeFieldPseudoExtentFinal(icode))) { oneOrBoth=1; asmHardRegStateSetReferenced(icode, h, h); } // Last usage, so set reference
					if (h == asmIcodeFieldPseudoExtentInitial(icode)) {
						oneOrBoth = 1;
						asmHardRegStateSetAssigned(icode, h, h);  // It's also first usage
					}

					if (!oneOrBoth)
					{
						asmHardRegStateSetAssigned(icode, h, h); // Not first, so must be both.
						asmHardRegStateSetReferenced(icode, h, h);
					}
				} else {
					++c;
					//vmPush(car(registers)); // Push the pseudo registers (reassembled later) so as to get rid of the hard registers in the pseudo register list.
				}

				registers = cdr(registers);
			}
			//objCountStackToList0(c); // Reassemble list of pseudo registers.
			//asmIcodeFieldPseudoExtentLiveSet(icode, r00);
		}

	} // for iblocks

	memRootSetAddressUnRegister(&icode);
	memRootSetAddressUnRegister(&registers);
	memRootSetAddressUnRegister(&inIblock);
	memRootSetAddressUnRegister(&iblock);
}


/* Consider each pseudo register and verify the first available hard register is available through its extent.
   asmIBlockTagSet
*/
void asmAssignRegisters (void) {
 Num i, f;
 Obj iregField;
	// Over all iblocks
	for (riblock = asmIBlockFrame(0);
	     riblock != ofalse;
	     riblock = asmIBlockNextValidIblock(riblock)) {
		if (otrue == asmIBlockTag(riblock)) { // If it's alive
		for (i=0; (i < asmIBlockICodeLength(riblock)); ++i) { // Over all icodes
			r02 = asmIBlockICode(riblock, i); /* Consider icode #(ADDI, r, NA, NA, i, NA)  r02 */
			for (f=1; f<=3; ++f) { // Over the three icode register fields  #(-, REG1, REG2, REG3, -, -)
				iregField = asmICodeField(r02, f);
				if (NA != (Obj)iregField) {
					if (asmRegIsPseudoObj(iregField)) {
						r01 = objAssq(iregField, asmIcodeObjRegSetIRegList(r02));
						//objDisplay(r01, stdout);
						asmICodeSetField(r02, f, cdr(r01));
					} else if (asmRegIsPseudoInt(iregField)) {
						r01 = objAssq(iregField, asmIcodeIntRegSetIRegList(r02));
						//objDisplay(r01, stdout);
						asmICodeSetField(r02, f, cdr(r01)); 
					} else if (asmRegIsHard(iregField)) {
						//objDisplay(iregField, stdout);
					} // if else if else if
				} // if
			} // for
		} // for
		} // if
	} // for
} // asmAssignRegisters()


/* Dedicated to Professor Mackey.

	Allocate either type=0 object register or type=1 integer register
*/
void asmAllocateRegisters (Num type) {
 Num i, count;
 Obj lst;
 Obj listOfAvailableRegisters=onull;
 Obj listOfCurrentIregisterAssignments=onull;
 Obj icode;

	memRootSetAddressRegister(&listOfAvailableRegisters);
	memRootSetAddressRegister(&listOfCurrentIregisterAssignments);
	memRootSetAddressRegister(&icode);

	// Over all iblocks
	for (riblock = asmIBlockFrame(0); // TODO implement asmIBlockFirst()
	     (riblock != ofalse);
	     riblock = asmIBlockNextValidIblock(riblock)) {

		/* Consider all available registers from each of the last icodes in the parent iblocks */
		lst = asmIBlockIncomingList(riblock);
		if (lst == onull) { // Default Set of available registers (objects 00-r08  integers r10-r1b) if there are no parent iblocks
			if (0==type) {
				r00 = onull;
				r01 = (Obj)0x04; objCons010();
				r01 = (Obj)0x03; objCons010();
				r01 = (Obj)0x02; objCons010();
				r01 = (Obj)0x01; objCons010();
			} else {
				r00 = onull;
				r01 = (Obj)0x14; objCons010();
				r01 = (Obj)0x13; objCons010();
				r01 = (Obj)0x12; objCons010();
				r01 = (Obj)0x11; objCons010();
			}
			listOfAvailableRegisters = r00;
		} else { // Verify all parent register lists are the same.
			// Use parent iblocks' last icode as current set of iregister extents.  Initialize with null.
			r00 = onull;
			for (r02 = asmIBlockIncomingList(riblock); // List of incoming blocks
			     (r02 != onull);
			     r02 = cdr(r02)) {// next incoming iblock
				r01 = asmIBlockLastICode(car(r02)); // Consider last icode of each parent's iblock
				if (ofalse != r01) {
					r01 = (type == 0) ? asmIcodeObjRegSetRegList(r01)  // Consider set of intermediate registers (from either the object or integer sets)
					                  : asmIcodeIntRegSetRegList(r01);
					r00 = r01; //objOrderedSetUnion001(); // TODO don't untion the parent available register sets. instead just verify they're equivalent
				}
			}
			listOfAvailableRegisters = r00;
		}

		for (i=0; (i < asmIBlockICodeLength(riblock)); ++i) { // Over all icodes
			/* Considering: listOfCurrentIregisterAssignments
			              : listOfAvailableRegisters
			*/

			icode = asmIBlockICode(riblock, i); /* Consider icode #(ADDI, r, NA, NA, i, NA)  */
			r03 = (type == 0) ? asmIcodeObjRegSetIRegList(icode)  /* Consider iregister list               r03*/
			                  : asmIcodeIntRegSetIRegList(icode);

			/* Create new free register list based on previous register list while freeing those that are not in the current iregister list.
			   Free register if out of iregister's range
 			*/
			r00 = listOfAvailableRegisters;
			count=0;
			while (r00 != onull) {
				r01 = car(r00); // Consider each register assignment (if not a pair, not assigned)
				if (objIsPair(r01)) {
					r01 = objMemq(car(r01), r03);
					if (r01 == onull) { // Iregister not required anylonger so free register
						vmPush(cdar(r00));
					} else {
						vmPush(car(r00));
					}
				} else {
					vmPush(r01);
				}
				count++;
				r00 = cdr(r00);
			}
			r00 = onull;
			while(count--) { r01 = vmPop(); objCons010(); }
			listOfAvailableRegisters = r00;

			// For each unassigned iregister...assign an available register...if none then OH NOES!
			count=0;
			while (r03 != onull) {
				r01 = car(r03); // Consider each assumed unassigned iregister
				r02 = objAssq(r01, listOfAvailableRegisters);

				if (r02 != onull) { // Found existing register assignment
					vmPush(r02); //memVectorSet(r03, 0, r02);
				} else {
					r02 = listOfAvailableRegisters;
					while (objIsPair(car(r02))) r02 = cdr(r02); // Look for first available register
					r00 = car(r02);
					objCons010(); // (ireg . reg)
					memVectorSet(r02, 0, r00);
					vmPush(r00); // memVectorSet(r03, 0, r00);
				}
				count++;
				r03 = cdr(r03); // Next unassigned iregister
			}

			r00 = onull;
			while(count--) {
				r01 = vmPop();
				objCons010();
			}

			// Look for placed register in available register list

			if (type == 0) {
				asmIcodeObjRegSetIRegListSet(icode, r00);
				asmIcodeObjRegSetRegListSet(icode, listOfAvailableRegisters);
			} else {
				asmIcodeIntRegSetIRegListSet(icode, r00);
				asmIcodeIntRegSetRegListSet(icode, listOfAvailableRegisters);
			}
		}
	}

	memRootSetAddressUnRegister(&icode);
	memRootSetAddressUnRegister(&listOfCurrentIregisterAssignments);
	memRootSetAddressUnRegister(&listOfAvailableRegisters);
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

	asmPrepareIGraph(asmIBlock(iblockOffset));

	//asmOptimizeIGraph();

	// Register allocation.  Will this ever be done?

	asmRegisterExtentsForwardPass(); // Determine iblock forward and reverse register extents (both hard and pseudo registers).
	asmRegisterExtentsReversePass();
	asmIregisterExtentsUpdateIcodes(); // Set each icode's register extent sets since we only determined extent between iblocks.

if (ofalse != odebug) {
	fprintf(stderr, "\nasmTagRegisterExtents() forward/reverse pseudo register extent:");
	asmDumpIBlocksFlags(!REVERSE_REGS | !FORWARD_REGS);
}

	asmAllocateRegisters(0);
	asmAllocateRegisters(1);
	asmAssignRegisters();
	asmOptimizeIGraph();

	/* Create the code block object which all iblocks are compiled to */
	len = asmCountIGraphFields();
	if (len) {
		rcodenew = memNewVector(TCODE, len);
		pccode = 0;
		asmPlaceAllIBlocks();

		asmResolveBranchOpcodeAddresses();

		r00 = rcodenew;

		if (odebug == otrue) {
			fprintf(stderr, "\n::%s", __FUNCTION__);
			//objDisplay(rlabels, stdout);
			objDisplay(rcodenew, stderr);
		}
	} else {
		r00 = ofalse;
	}

	asmEnd();

	DBEND();
}



/*******************************************************************************
 Debugging
*******************************************************************************/
Num _asmPrintIregisterAssignments (Obj l, FILE *fp, char *sep) {
 Obj o;
 char buff[80];
	if (!objIsPair(l)) return 0;
	o = car(l); // consider car
	if (objIsPair(o)) {
		fprintf(fp, STR HEX03 ":$%x", sep, car(o), cdr(o));
	} else {
		asmRegToString(buff, o);
		fprintf(fp, STR STR, sep, buff);
	}
	return 1 + _asmPrintIregisterAssignments(cdr(l), fp, " "); // recurse on cdr
}
Num asmPrintIregisterAssignments (Obj l, FILE *fp) {
	return (l!=onull) ? _asmPrintIregisterAssignments(l, fp, "") : 0;
}

void _asmPrtinRegisterAssignmentsHeader (FILE *fp) {
	fprintf(fp, "\t  $0     $1     $2     $3     $4     %%0     %%1     %%2     %%3     %%4");
}

Num _asmPrintRegisterAssignments (Obj l, FILE *fp, char *sep) {
 Obj o;
	if (!objIsPair(l)) return 0;
	o = car(l); // consider car
	if (objIsPair(o)) {
		fprintf(fp, STR HEX04, sep, car(o)); //fprintf(fp, STR "$"HEX"(" HEX03 ")", sep, cdr(o), car(o));
	} else {
		fprintf(fp, STR " -- ", sep);  // fprintf(fp, STR "$" HEX "(   )", sep, o);
	}
	return 1 + _asmPrintRegisterAssignments(cdr(l), fp, " "); // recurse on cdr
}
Num asmPrintRegisterAssignments (Obj l, FILE *fp) {
	//if (l==onull) fprintf(fp, " --   --   --   --  ");
	return (l!=onull) ? _asmPrintRegisterAssignments(l, fp, "") : 0;
}

/* Print fields of an icode (icode arguments) including assembler work structures
*/
void asmPrintICodeFields (Obj ic) {
 Obj f, o;
 char buff[80];
 Num count;
	// Field 1 reg
	if ((Obj)NA != (f = asmICodeField(ic, 1))) {
		asmRegToString(buff, f);
		fprintf(stderr, " %4s", buff);
	}

	// Field 2 reg
	if ((Obj)NA != (f = asmICodeField(ic, 2))) {
		asmRegToString(buff, f);
		fprintf(stderr, " %4s", buff);
	}

	// Field 3 reg
	if ((Obj)NA != (f = asmICodeField(ic, 3))) {
		asmRegToString(buff, f);
		fprintf(stderr, " %4s", buff);
	}

	// Field 4 immediate
	if ((Obj)NA != (f = asmICodeField(ic, 4))) { fprintf(stderr, " "); objDisplay(f, stderr); }

	// Field 5 offset
	if ((Obj)NA != (f = asmICodeField(ic, 5))) { fprintf(stderr, " ["HEX03"]", asmLabels((Num)f)); }

	fprintf(stderr, "\t"); // Indent in preparation for hard register extent vectors.

	// Hard registers extents
	//objDisplay(asmIcodeFieldHardRegisters(ic), stderr);
	o = asmIcodeFieldHardRegisters(ic);
	for (count=0; count<memObjectLength(o); ++count) { fprintf(stderr, STR3, asmHardRegStateToString(memVectorObject(o, count))); }
	fprintf(stderr, "]");

	// Pseudo register extents
	f = asmIcodeFieldPseudoExtent(ic);
	objDisplay(f, stderr);
/*
	f = asmIcodeFieldPseudoExtentLive(ic);
	if (!objIsPair(f)) {
		fprintf(stderr, " ");
		objDisplay(f, stderr);
	} else {
		asmRegToString(buff, car(f));
		fprintf(stderr, " %s", buff);
		f = cdr(f);
		while (f != onull) {
			o = car(f);
			asmRegToString(buff, o);
			fprintf(stderr, " %s", buff);
			f = cdr(f);
		}
	}
*/

	// Field 6 intermediate object-register extents

	if ((Obj)NA != (f = asmICodeField(ic, ICODE_INDEX_OBJREGSET))) {
		count = asmPrintRegisterAssignments(memVectorObject(f, 1), stderr); // Field 5 Reverse object-integer extent set
		if (count) { fprintf(stderr, " "); }
	}
	if ((Obj)NA != (f = asmICodeField(ic, ICODE_INDEX_INTREGSET))) {
		asmPrintRegisterAssignments(memVectorObject(f, 1), stderr); // Field 6 Reverse integer-register extent set
	}

	fprintf(stderr, " ");
	if ((Obj)NA != (f = asmICodeField(ic, ICODE_INDEX_OBJREGSET))) {
		count = asmPrintIregisterAssignments(memVectorObject(f, 0), stderr); // Field 5 Forward object-integer extent set
		if (count) { fprintf(stderr, " "); }
	}
	if ((Obj)NA != (f = asmICodeField(ic, ICODE_INDEX_INTREGSET))) {
		asmPrintIregisterAssignments(memVectorObject(f, 0), stderr); // Field 6 Forward integer-register extent set
	}

	fflush(stderr);
}

void asmPrintICode (Obj ic) {
 Obj field;
	if (memIsObjectValid(ic)) {
		assert(asmIsObjectTypeICode(ic));
		field = asmICodeField(ic, 0);
		switch ((Num)field) {
			case (Num)MV  : fprintf(stderr, "mv  "); asmPrintICodeFields(ic); break;
			case (Num)MVI : fprintf(stderr, "mvi "); asmPrintICodeFields(ic); break;
			case (Num)LDI : fprintf(stderr, "ldi "); asmPrintICodeFields(ic); break;
			case (Num)LD  : fprintf(stderr, "ld  "); asmPrintICodeFields(ic); break;
			case (Num)STI : fprintf(stderr, "sti "); asmPrintICodeFields(ic); break;
			case (Num)ST  : fprintf(stderr, "st  "); asmPrintICodeFields(ic); break;
			case (Num)PUSH: fprintf(stderr, "push"); asmPrintICodeFields(ic); break;
			case (Num)POP : fprintf(stderr, "pop "); asmPrintICodeFields(ic); break;
			case (Num)ANDI: fprintf(stderr, "andi"); asmPrintICodeFields(ic); break;
			case (Num)LSLI: fprintf(stderr, "lsli"); asmPrintICodeFields(ic); break;
			case (Num)LSRI: fprintf(stderr, "lsri"); asmPrintICodeFields(ic); break;
			case (Num)ADD : fprintf(stderr, "add "); asmPrintICodeFields(ic); break;
			case (Num)ADDI: fprintf(stderr, "addi"); asmPrintICodeFields(ic); break;
			case (Num)MUL : fprintf(stderr, "mul");  asmPrintICodeFields(ic); break;
			case (Num)MULI: fprintf(stderr, "muli"); asmPrintICodeFields(ic); break;
			case (Num)BLTI: fprintf(stderr, "blti"); asmPrintICodeFields(ic); break;
			case (Num)BGTI: fprintf(stderr, "bgti"); asmPrintICodeFields(ic); break;
			case (Num)BGT : fprintf(stderr, "bgt");  asmPrintICodeFields(ic); break;
			case (Num)BEQI: fprintf(stderr, "beqi"); asmPrintICodeFields(ic); break;
			case (Num)BNEI: fprintf(stderr, "bnei"); asmPrintICodeFields(ic); break;
			case (Num)BRA : fprintf(stderr, "bra "); asmPrintICodeFields(ic); break;
			case (Num)JMP : fprintf(stderr, "jmp "); asmPrintICodeFields(ic); break;
			case (Num)JAL : fprintf(stderr, "jal "); asmPrintICodeFields(ic); break;
			case (Num)RET : fprintf(stderr, "ret");  asmPrintICodeFields(ic); break;
			case (Num)SYS : fprintf(stderr, "sys "); asmPrintICodeFields(ic); break;
			case (Num)SYSI: fprintf(stderr, "sysi"); asmPrintICodeFields(ic); break;
			case (Num)NOP : fprintf(stderr, "nop");  asmPrintICodeFields(ic); break;
			case (Num)QUIT: fprintf(stderr, "quit"); asmPrintICodeFields(ic); break;
			default:
				if (field == NA) { fprintf(stderr, "---"); break; }
				fprintf(stderr, "**UNKNOWN OPCODE**");
				objDisplay(ic, stderr);
		}
	}
}

void asmPrintIblockFlags (Obj ib, Num flags) {
 Num i;
 Obj o, block;
 char buff[80];
	assert(asmIsObjectTypeIBlock(ib));
	// ID
	fprintf(stderr, "\n\n#<"HEX03 " " OBJ " ", asmIBlockID(ib), ib);

	// Tag
	objDisplay(asmIBlockTag(ib), stderr);

	fprintf(stderr, " ");

	// Incoming block IDs
	o = asmIBlockIncomingList(ib);
	if (onull == o) {
		fprintf(stderr, "(---)");
	} else {
		fprintf(stderr, "(");
		while (onull != o) {
			fprintf(stderr, HEX03, asmIBlockID(car(o)));
			o = cdr(o);
			if (onull != o) fprintf(stderr, " ");
		}
		fprintf(stderr, ")");
	}

	_asmPrtinRegisterAssignmentsHeader(stderr);

	/* Code */
	for (i=0; i<asmIBlockICodeLength(ib); ++i) {
		fprintf(stderr, "\n "HEX02"  ", i);
		asmPrintICode(asmIBlockICode(ib, i));
	}

	/* Reverse register set */
	if (flags & REVERSE_REGS) {
		fprintf(stderr, "\n  ");
		block = asmIblockReverseRegisters(ib);
		if (!objIsPair(block)) {
			objDisplay(block, stderr);
		} else while (block != onull) {
			o = car(block);
			if (!asmRegIsHardOrPseudo(o)) fprintf(stderr, "["HEX03"]", asmIBlockID(o));
			else {
				asmRegToString(buff, o);
				fprintf(stderr, " %s", buff);
			}
			block = cdr(block);
		}
	}

		/* Forward register set */
	if (flags & FORWARD_REGS) {
		fprintf(stderr, "\n  ");

		block = asmIblockForwardRegisters(ib);
		if (!objIsPair(block)) {
			objDisplay(block, stderr);
		} else while (block != onull) {
			o = car(block);
			if (!asmRegIsHardOrPseudo(o)) fprintf(stderr, "["HEX03"]", asmIBlockID(o));
			else {
				asmRegToString(buff, o);
				fprintf(stderr, " %s", buff);
			}
			block = cdr(block);
		}
	}

	fprintf(stderr, "\n ");

	/* Default block */
	block = asmIBlockDefaultTag(ib);
	if (ofalse==block)
		fprintf(stderr, "[---]");
	else {
		fprintf (stderr, "[");
		if (asmIsObjectTypeIBlock(block)) fprintf(stderr, HEX03, asmIBlockID(block));
		else objDisplay(block, stderr);
		fprintf (stderr, "]");
	}

	/* Conditional block */
	block = asmIBlockConditionalTag(ib);
	if (ofalse==block)
		fprintf(stderr, " [---]");
	else {
		fprintf (stderr, " [");
		if (asmIsObjectTypeIBlock(block)) fprintf(stderr, HEX03, asmIBlockID(block));
		else objDisplay(block, stderr);
		fprintf (stderr, "]");
	}


	fprintf (stderr, " >");
}

void asmPrintIblock(Obj ib) {
	asmPrintIblockFlags(ib, (Num)-1);
}


void asmDumpIBlocksFlags (Num flags) {
 Num i;
 int fl;
	/* Temporarily enable blocking I/O */
	fl = fcntl(0, F_GETFL, 0);
	fcntl (0, F_SETFL, fl&~O_NONBLOCK);

	DBBEG();

	for (i=iblockOffset; (i < IBlockCount); ++i) {
		asmPrintIblockFlags(asmIBlock(i), flags);
	}

	fprintf(stderr, "\n");

	DBEND();

	fcntl (0, F_SETFL, fl);
}

void asmDumpIBlocks (void) {
	asmDumpIBlocksFlags((Num)-1);
}

void asmDumpIBlockParentAndChildren (Obj ib) {
 Obj lst, last, inib, dib, cib;
	lst = asmIBlockIncomingList(ib);
	last = onull;
	while (onull != lst) {
		inib = car(lst); /* Consider an incoming block */
		if (last != inib) asmPrintIblock(inib);
		last = inib;
		lst = cdr(lst);
	}
	asmPrintIblock(ib);
	dib = asmIBlockDefaultTag(ib);
	cib = asmIBlockConditionalTag(ib);
	if (asmIsObjectTypeIBlock(dib)) asmPrintIblock(dib);
	if (asmIsObjectTypeIBlock(cib)) asmPrintIblock(cib);
}



/*******************************************************************************
 Init
*******************************************************************************/
void asmInitialize (void) {
 static Num shouldInitialize=1;
	DBBEG();

	// TODO set tabstop for icode debugging
	printf("\e[3g                                   \eH                   \eH                 \eH                \eH           \eH          \eH        \eH        \eH        \eH      \eH\r");

	if (shouldInitialize) {
		DB("Activating module");
		shouldInitialize=0;

		DB("Initializing submodules");
		objInitialize(); /* objInitialize -> vmInitialize -> memInitialize */

		DB("Registering rootset objects");
		memRootSetAddressRegister(&ropcodes); MEM_ADDRESS_REGISTER(&ropcodes);
		memRootSetAddressRegister(&riblock); MEM_ADDRESS_REGISTER(&riblock);
		memRootSetAddressRegister(&riblocks); MEM_ADDRESS_REGISTER(&riblocks);
		memRootSetAddressRegister(&ricodes); MEM_ADDRESS_REGISTER(&ricodes);
		memRootSetAddressRegister(&rlabels); MEM_ADDRESS_REGISTER(&rlabels);
		memRootSetAddressRegister(&rcodenew); MEM_ADDRESS_REGISTER(&rcodenew);

		DB("Registering types");
		memTypeStringRegister(TICODE, (Str)"icode");
		memTypeStringRegister(TEXTENT, (Str)"extent");
		memTypeStringRegister(TIBLOCK, (Str)"iblock");

		DB("Registering serializers");
		objDisplayTypeRegister(TEXTENT, asmDisplayTypeExtent);

		DB("Initializing compiler related objects and symbols");
		objNewSymbolStatic("sasmend"); sasmend = r00;
		objNewSymbolStatic("sasmna"); sasmna = r00;
		objNewVector(IBLOCK_VECTOR_SIZE);
		riblocks = r00;
		objNewVector(ICODE_VECTOR_SIZE);
		ricodes = r00;
		objNewVector(LABELS_DB_SIZE);
		rlabels = r00;
		objNewVector(OPCODES_VEC_SIZE);
		ropcodes = r00;
	} else {
		DB("Module already activated");
	}
	DBEND();

}



#undef DB_DESC
#undef DEBUG
