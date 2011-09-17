#define DEBUG 1
#define DB_DESC "CC"
#include "debug.h"
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "cc.h"
#include "sys.h"
#include "obj.h"
#include "asm.h"
#include "vm.h"
#include "mem.h"

/*
TABLE OF CONTENTS
 Igraph_and_iblocks
 Debugging
 Assemble
 ASM
 Compiler
 REPL
 Init

TERMS
  I-Graph   Intermediate graph composed of I-blocks
  I-Block   I-graph node composed of a list of incoming iblocks, outgoing iblocks and icode statements
  I-Code    I-block statement composed of multiple code fields

DESIGN
   Expression to compile assigned to rexpr/r15
   Flow keeps track of pseudo environment in renv/r1c and used registers in flags
*/

void ccDumpICode (Obj c);
void ccDumpIBlock (Obj ib);
void ccCompileExpr (Num flags);
void ccInitialize (void);

/* Register aliases overriding aliases in vm module
*/
#define riblocklast re  /* The last iblock considered when building an igraph */
#define rigraph rf  /* The igraph which is a vector of iblocks */
//#define rexpr r15 /* Expression being compiled.  See vm.h */
#define riblock r16 /* The current iblock where new icode is emitted and new iblocks are attached to */

/* Object types used by compiler
*/
#define TICODE  0x87l
#define TIBLOCK 0x88l

/* Icode symbols.  Since they are less generalized I can't
   use the VM objects */
Obj smv, smvi;
Obj sldi;
Obj spush, spop;
Obj saddi;
Obj sbnei, sbeqi, sbrti, sbra;
Obj ssysi, snop, squit;

/* Assembly opcode symbols */
Obj R0 =(Obj)0x00; Obj R1 =(Obj)0x01; Obj R2 =(Obj)0x02; Obj R3 =(Obj)0x03;
Obj R4 =(Obj)0x04; Obj R5 =(Obj)0x05; Obj R6 =(Obj)0x06; Obj R7 =(Obj)0x07;
Obj R1C=(Obj)0x1c;

Obj MV  = (Obj)0x20;
Obj MVI = (Obj)0x21;
Obj LDI = (Obj)0x22;
Obj PUSH = (Obj)0x23;
Obj POP = (Obj)0x24;;
Obj ADDI = (Obj)0x25;
Obj BNEI = (Obj)0x26;
Obj BEQI = (Obj)0x27;
Obj BRTI = (Obj)0x28;
Obj BRA = (Obj)0x29;
Obj SYSI = (Obj)0x2a;
Obj NOP = (Obj)0x2b;
Obj QUIT = (Obj)0x2c;
Obj LABEL = (Obj)0xef;
Obj END = (Obj)0xff;




/*******************************************************************************
 Igraph_and_iblocks

 An intermediate graph is a graph of intermediate blocks.  Each iblock can have
 any number of incoming iblocks and up to two outgoing: the default iblock and
 the conditional iblock.  If the iblock represents a conditional statement, its
 last icode will be a conditional branch code and it's target block will be the
 "conditional" block.  All iblocks will generally have a "default" block which
 represents the continued flow of instructions.  Iblocks contain icode
 instructions optimized and eventually assembled into a VM code block object.
*******************************************************************************/
#define IBLOCK_INDEX_TAG         0
#define IBLOCK_INDEX_ID          1
#define IBLOCK_INDEX_INCOMING    2
#define IBLOCK_INDEX_DEFAULT     3
#define IBLOCK_INDEX_CONDITIONAL 4
#define IBLOCK_INDEX_ICODE       5
Num iblockCount;
Num pciblock;

Num ccIBlockID (Obj ib)                     { return (Num)memVectorObject(ib, IBLOCK_INDEX_ID); }
Obj ccIBlockTag (Obj ib)                         { return memVectorObject(ib, IBLOCK_INDEX_TAG); }
Obj ccIBlockIncomingList (Obj ib)                { return memVectorObject(ib, IBLOCK_INDEX_INCOMING); }
Obj ccIBlockGetDefaultBlock (Obj ib)     { return car(memVectorObject(ib, IBLOCK_INDEX_DEFAULT)); }
Obj ccIBlockGetConditionalBlock (Obj ib) { return car(memVectorObject(ib, IBLOCK_INDEX_CONDITIONAL)); }

/* Lookup iblock by ID
*/
Obj ccIBlock (Num id) {
 Obj ib;
	ib = memVectorObject(rigraph, id);
	assert(TIBLOCK == memObjectType(ib));
	assert(ccIBlockID(ib) == id);
	return ib;
}

/* Get the iblock's tag object
*/
void ccIBlockSetTag (Obj ib, Obj tag) {
	memVectorSet(ib, IBLOCK_INDEX_TAG, tag);
}

/* Set the iblock's default and conditional tag.  It's also considered the
   destination block at times.  See similar functions below.
*/
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


/* Add an iblock object to the iblock's list of incoming blocks.
   Automatically called when adding default/conditional blocks.
*/
void ccIBlockAddIncoming (Obj ib, Obj in) {
	r1 = in;
	r2 = memVectorObject(ib, IBLOCK_INDEX_INCOMING);
	objCons12();
	memVectorSet(ib, IBLOCK_INDEX_INCOMING, r0);
}


/* Set the default iblock for this iblock.
*/
void ccIBlockSetDefault (Num parentid, Num childid) {
 Obj parentib, childib, condPair;

	parentib = ccIBlock(parentid);
	condPair = memVectorObject(parentib, IBLOCK_INDEX_DEFAULT);
	//assert(false == car(condPair));
	childib = ccIBlock(childid);

	memVectorSet(condPair, 0, childib);
	ccIBlockAddIncoming (childib, parentib);
}

/* Set the conditional iblock for this iblock.
*/
void ccIBlockSetConditional (Num parentid, Num childid) {
 Obj condPair, parentib, childib;

 	parentib = ccIBlock(parentid);
 	condPair = memVectorObject(parentib, IBLOCK_INDEX_CONDITIONAL);
	//assert(false == car(condPair));

 	childib = ccIBlock(childid);

	memVectorSet(condPair, 0, childib);
	ccIBlockAddIncoming(childib, parentib);
}

void ccIBlockSetICode (Num offset, Obj op) {
	memVectorSet(riblock, IBLOCK_INDEX_ICODE+offset, op);
}

/* Create new igraph block defaults to #(#f () #(#f #f) ...)
       r0 =  temp
       r1 =  temp
       r2 =  temp
   iblock => iblock object
   return => iblock ID
   Increments iblockCount which is also used as the block's ID.
*/
Num ccGenerateNewIBlock (Num icodeSize) {
	riblock = memNewVector(TIBLOCK, icodeSize + IBLOCK_INDEX_ICODE);
	/* Append to igraph vector */
	memVectorSet(rigraph, iblockCount, riblock);
	/* Reset the iblock's icode 'next field' pointer.  Used by opcode emitter. */
	pciblock = IBLOCK_INDEX_ICODE;
	/* Tag defaults to #f */
	memVectorSet(riblock, IBLOCK_INDEX_TAG, false);
	/* Name can be anything */
	memVectorSet(riblock, IBLOCK_INDEX_ID, (Obj)iblockCount);
	/* Incoming iblock list defaults to null */
	memVectorSet(riblock, IBLOCK_INDEX_INCOMING, null);
	/* Outgoing iblock pair defaults to #((#f.#f) (#f.#f)) */
	r1 = r2 = false;  objCons12();
	memVectorSet(riblock, IBLOCK_INDEX_DEFAULT, r0);
	r1 = r2 = false;  objCons12();
	memVectorSet(riblock, IBLOCK_INDEX_CONDITIONAL, r0);

	return iblockCount++;
}

Num ccNewDefaultIBlock (Num parentID, Num icodeSize) {
 Num id;
	id = ccGenerateNewIBlock(icodeSize);
	ccIBlockSetDefault(parentID, id);
	return id;
}

Num ccNewConditionalIBlock (Num parentID, Num icodeSize) {
 Num id;
	id = ccGenerateNewIBlock(icodeSize);
	ccIBlockSetConditional (parentID, id);
	return id;
}


void ccNewICodeMV (Obj ra, Obj rb) {
	r0 = memNewVector(TICODE, 3);
	memVectorSet(r0, 0, smv);
	memVectorSet(r0, 1, (Obj)ra);
	memVectorSet(r0, 2, (Obj)rb);
}

void ccNewICodeMVI (Obj r, Obj o) {
	vmPush(o);
	r0 = memNewVector(TICODE, 3);
	memVectorSet(r0, 0, smvi);
	memVectorSet(r0, 1, (Obj)r);
	memVectorSet(r0, 2, vmPop());
}

void ccNewICodeLDI (Obj ra, Obj rb, Obj o) {
	vmPush(o);
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, sldi);
	memVectorSet(r0, 1, (Obj)ra);
	memVectorSet(r0, 2, (Obj)rb);
	memVectorSet(r0, 3, vmPop());
}

void ccNewICodePUSH (Obj o) {
	vmPush(o);
	r0 = memNewVector(TICODE, 2);
	memVectorSet(r0, 0, spush);
	memVectorSet(r0, 1, vmPop());
}

void ccNewICodePOP (Obj r) {
	r0 = memNewVector(TICODE, 2);
	memVectorSet(r0, 0, spop);
	memVectorSet(r0, 1, r);
}
void ccNewICodeADDI (Obj r, Obj o) {
	vmPush(o);
	r0 = memNewVector(TICODE, 3);
	memVectorSet(r0, 0, saddi);
	memVectorSet(r0, 1, (Obj)r);
	memVectorSet(r0, 2, vmPop());
}

void ccNewICodeBNEI (Obj r, Obj i, Obj o) {
	vmPush(o);
	vmPush(i);
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, sbnei);
	memVectorSet(r0, 1, (Obj)r);
	memVectorSet(r0, 2, vmPop());
	memVectorSet(r0, 3, vmPop());
}

void ccNewICodeBEQI (Obj ra, Obj imm, Obj o) {
	vmPush(o);
	vmPush(imm);
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, sbeqi);
	memVectorSet(r0, 1, (Obj)ra);
	memVectorSet(r0, 2, vmPop());
	memVectorSet(r0, 3, vmPop());
}

void ccNewICodeBRTI (Obj ra, Obj imm, Obj o) {
	vmPush(o);
	vmPush(imm);
	r0 = memNewVector(TICODE, 4);
	memVectorSet(r0, 0, sbrti);
	memVectorSet(r0, 1, (Obj)ra);
	memVectorSet(r0, 2, vmPop());
	memVectorSet(r0, 3, vmPop());
}

void ccNewICodeBRA (Obj o) {
	vmPush(o);
	r0 = memNewVector(TICODE, 2);
	memVectorSet(r0, 0, sbra);
	memVectorSet(r0, 1, vmPop());
}

void ccNewICodeSYSI (Obj o) {
	vmPush(o);
	r0 = memNewVector(TICODE, 2);
	memVectorSet(r0, 0, ssysi);
	memVectorSet(r0, 1, vmPop());
}

void ccNewICodeNOP (void) {
	r0 = memNewVector(TICODE, 1);
	memVectorSet(r0, 0, snop);
}

void ccNewICodeQUIT (void) {
	r0 = memNewVector(TICODE, 1);
	memVectorSet(r0, 0, squit);
}
/* Emit a new icode object to the current iblock.  Not called directly.
*/
void ccEmitICode (Obj c) {
	assert(pciblock < memObjectLength(riblock));
	memVectorSet(riblock, pciblock++, c);
}

void ccMV  (Obj ra, Obj rb)         { ccNewICodeMV(ra, rb);     ccEmitICode(r0); }
void ccMVI (Obj r, Obj o)           { ccNewICodeMVI(r, o);     ccEmitICode(r0); }
void ccLDI (Obj ra, Obj rb, Obj o)  { ccNewICodeLDI(ra, rb, o); ccEmitICode(r0); }
void ccPUSH(Obj o)                  { ccNewICodePUSH(o);        ccEmitICode(r0); }
void ccPOP (Obj r)                  { ccNewICodePOP(r);         ccEmitICode(r0); }
void ccADDI (Obj r, Obj o)          { ccNewICodeADDI(r, o);    ccEmitICode(r0); }
void ccBNEI (Obj r, Obj i, Obj o)   { ccNewICodeBNEI(r, i, o); ccEmitICode(r0); }
void ccBEQI (Obj ra, Obj imm, Obj o){ ccNewICodeBEQI(ra, imm, o); ccEmitICode(r0); }
void ccBRTI (Obj ra, Obj imm, Obj o){ ccNewICodeBRTI(ra, imm, o); ccEmitICode(r0); }
void ccBRA (Obj o)                  { ccNewICodeBRA(o); ccEmitICode(r0); }
void ccSYSI (Obj o)                 { ccNewICodeSYSI(o); ccEmitICode(r0); }
void ccNOP (void)                   { ccNewICodeNOP(); ccEmitICode(r0); }
void ccQUIT (void)                  { ccNewICodeQUIT(); ccEmitICode(r0); }



/* Reset the "igraph" and related symbols for iblock creation and connecting.
*/
void ccResetIGraph (void) {
	/* Reset the vector if iblocks */
	iblockCount = 0;
}



/*******************************************************************************
 Debugging
*******************************************************************************/
void ccDumpICode (Obj c) {
 Num i;
	if (memIsObjectValid(c)) {
		for (i=0; i<memObjectLength(c); ++i) {
			if (0<i) fprintf(stderr, " ");
			sysDisplay(memVectorObject(c, i), stderr);
		}
	} else {
		sysWrite(c, stderr);
	}
}

void ccDumpIBlock (Obj ib) {
 Num i;
 Obj o, block;
	assert(TIBLOCK == memObjectType(ib));
	/* ID */
	fprintf(stderr, "\n#<"HEX03" "OBJ"  ", ccIBlockID(ib), ib);
	/* Tag */
	sysDisplay(ccIBlockTag(ib), stderr);
	/* Incoming block IDs */
	fprintf(stderr, "  (");
	for (o = ccIBlockIncomingList(ib); null != o; ) {
		fprintf(stderr, HEX03, ccIBlockID(car(o)));
		o = cdr(o);
		if (null != o) fprintf(stderr, " ");
	}
	fprintf(stderr, ")");
	/* Code */
	for (i=IBLOCK_INDEX_ICODE; i<memObjectLength(ib); ++i) {
		fprintf(stderr, "\n  "HEX02"  ", i-IBLOCK_INDEX_ICODE);
		ccDumpICode(memVectorObject(ib, i));
	}
	fprintf (stderr, "\n");
	/* Default block */
	block = ccIBlockGetDefaultBlock(ib);
	if (false==block)
		fprintf(stderr, "  [---]");
	else {
		fprintf (stderr, "  [");
		if (memIsObjectType(block, TIBLOCK)) fprintf(stderr, HEX03, ccIBlockID(block));
		else if (!memIsObjectValid(block)) fprintf(stderr, HEX03, ccIBlockID(memVectorObject(rcode, (Num)block)));
		else sysDisplay(block, stderr);
		fprintf (stderr, "]");
	}

	/* Conditional block */
	block = ccIBlockGetConditionalBlock(ib);
	if (false==block)
		fprintf(stderr, "  [---]");
	else {
		fprintf (stderr, "  [");
		if (memIsObjectType(block, TIBLOCK)) fprintf(stderr, HEX03, ccIBlockID(block));
		else if (!memIsObjectValid(block)) fprintf(stderr, HEX03, ccIBlockID(memVectorObject(rcode, (Num)block)));
		else sysDisplay(block, stderr);
		fprintf (stderr, "]");
	}
	fprintf (stderr, ">");
}

void ccDumpIBlocks (void) {
 Num i;
	for (i=0; i<iblockCount; ++i)
		ccDumpIBlock(ccIBlock(i));
}



/*******************************************************************************
 Assemble

 Assemble the iblocks in an igraph into a VM runable code block object
*******************************************************************************/
Num pccode = 0;  /* Pointer into code block */

void ccEmitOpcode (Obj op) {
	memVectorSet(rcode, pccode++, op);
}

void ccEmitOpcode2 (Obj op1, Obj op2) {
	memVectorSet(rcode, pccode++, op1);
	memVectorSet(rcode, pccode++, op2);
}

void ccEmitIblockOpcodes (void) {
 Num i;
	DBBEG("      iblock="NUM, ccIBlockID(riblock));
	/* Re-tag the iblock with its initial location in the code block */
	ccIBlockSetTag (riblock, (Obj)pccode);

	for (i=IBLOCK_INDEX_ICODE; i < memObjectLength(riblock); ++i) {
		/* r2 = icode object, vector of fields */
		r2 = memVectorObject(riblock, i);
		/* r3 = icode field 0, icode's 'opcode' */
		r3 = memVectorObject(r2, 0);

		/* MV */
		if (smv == r3) {
			r3 = memVectorObject(r2, 1); /* icode's 2nd field: reg */
			if (R0 == r3) {
				r3 = memVectorObject(r2, 2); /* icode's 3rd field: reg */
				if (R1 == r3) ccEmitOpcode(vmMV01);
				else assert(!"Unsuported icode MV $0 ??");
			} else if (R1 == r3) {
				r3 = memVectorObject(r2, 2); /* icode's 3rd field: reg */
				if (R0 == r3) ccEmitOpcode(vmMV10);
				else assert(!"Unsuported icode MV $1 ??");
			} else if (R2 == r3) {
				r3 = memVectorObject(r2, 2); /* icode's 3rd field: reg */
				if (R0 == r3) ccEmitOpcode(vmMV20);
				else assert(!"Unsuported icode MV $2 ??");
			} else assert(!"Unsuported icode MV ?? reg");

		/* MVI */
		} else if (smvi == r3) {
			r3 = memVectorObject(r2, 1); /* icode's 2nd field: reg */
			if (R0 == r3) {
				ccEmitOpcode(vmMVI0);
			} else if (R1 == r3) {
				ccEmitOpcode(vmMVI1);
			} else if (R2 == r3) {
				ccEmitOpcode(vmMVI2);
			} else
				assert(!"Unsuported icode MVI ?? imm");
			ccEmitOpcode(memVectorObject(r2, 2)); /* icode's 3rd field: immediate */
		/* LDI */
		} else if (sldi == r3) {
			r3 = memVectorObject(r2, 1); /* icode's 2nd field: reg */
			if (R0 == r3) {
				r3 = memVectorObject(r2, 2); /* icode's 3rd field: reg */
				if (R0 == r3) ccEmitOpcode(vmLDI00);
				else if (R1C == r3) ccEmitOpcode(vmLDI01C);
				else assert(!"Unsuported icode LDI $0 ?? imm");
				ccEmitOpcode(memVectorObject(r2, 3));
			} else if (R1 == r3) {
				r3 = memVectorObject(r2, 2); /* icode's 3rd field: reg */
				if (R1 == r3) ccEmitOpcode(vmLDI11);
				else assert(!"Unsuported icode LDI $1 ?? imm");
				ccEmitOpcode(memVectorObject(r2, 3));
			} else assert(!"Unsuported icode LDI reg reg imm");

		/* PUSH */
		} else if (spush == r3) {
			r3 = memVectorObject(r2, 1); /* icode's 2nd field: reg */
			if (R0 == r3) {
				ccEmitOpcode(vmPUSH0);
			} else if (R1 == R3) {
				ccEmitOpcode(vmPUSH1);
			} else
				assert(!"Unsuported icode PUSH ??");

		/* POP */
		} else if (spop == r3) {
			r3 = memVectorObject(r2, 1); /* icode's 2nd field: reg */
			if (R0 == r3) {
				ccEmitOpcode(vmPOP0);
			} else if (R1 == r3) {
				ccEmitOpcode(vmPOP1);
			} else if (R2 == r3) {
				ccEmitOpcode(vmPOP2);
			} else
				assert(!"Unsuported icode POP ??");

		/* ADDI */
		} else if (saddi == r3) {
			r3 = memVectorObject(r2, 1); /* icode's 2nd field: reg */
			if (R0 == r3) {
				ccEmitOpcode(vmADDI0);
			} else if (R1 == r3) {
				ccEmitOpcode(vmADDI1);
			} else if (R2 == r3) {
				ccEmitOpcode(vmADDI2);
			} else
				assert(!"Unsuported icode ADDI ?? imm");
			ccEmitOpcode(memVectorObject(r2, 2)); /* icode's 3rd field: immediate */

		/* BNEI */
		} else if (sbnei == r3) {
			r3 = memVectorObject(r2, 1); /* icode's 2nd field: reg */
			if (R0 == r3) {
				ccEmitOpcode(vmBNEI0);
			} else if (R1 == r3) {
				ccEmitOpcode(vmBNEI1);
			} else if (R2 == r3) {
				ccEmitOpcode(vmBNEI2);
			} else
				assert(!"Unsuported icode BNEI ?? offset");
			ccEmitOpcode(memVectorObject(r2, 2)); /* icode's 3rd field: immediate */
			ccEmitOpcode((Obj)(-3*8)); /* icode's 4th field: bra address */

		/* BEQI */
		} else if (sbeqi == r3) {
			r3 = memVectorObject(r2, 1); /* icode's 2nd field: reg */
			if (R0 == r3) {
				ccEmitOpcode(vmBEQI0);
			} else if (R1 == r3) {
				ccEmitOpcode(vmBEQI1);
			} else
				assert(!"Unsuported icode BEQI ?? offset");
			ccEmitOpcode(memVectorObject(r2, 2)); /* icode's 3rd field: immediate */
			ccEmitOpcode((Obj)(-3*8)); /* icode's 4th field: bra address */

		/* BRTI */
		} else if (sbrti == r3) {
			r3 = memVectorObject(r2, 1); /* icode's 2nd field: reg */
			if (R0 == r3) {
				ccEmitOpcode(vmBRTI0);
			} else
				assert(!"Unsuported icode BNEI ?? offset");
			ccEmitOpcode(memVectorObject(r2, 2)); /* icode's 3rd field: immediate type */
			ccEmitOpcode((Obj)(-3*8)); /* opcode's bra address */

		/* BRTI */
		} else if (sbra == r3) {
			//ccEmitOpcode(vmBRA);
			//ccEmitOpcode(memVectorObject(r2, 1)); /* icode's 2nd field: branch address (ignored when in igraph form */

		/* SYSI */
		} else if (ssysi == r3) {
			ccEmitOpcode2(vmSYSI, memVectorObject(r2, 1));

		/* QUIT */
		} else if (squit == r3) {
			ccEmitOpcode(vmQUIT);

		/* NOP */
		} else if (snop == r3) {
			//ccEmitOpcode(vmNOP);

		/* UNSUPPORTED ICODE */
		} else {
			fprintf(stderr, "\nCan't assemble opcode ");
			objDump(r3, stderr);
			fprintf(stderr, ".");
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
 Num id;
	DBBEG("  iblock="NUM, ccIBlockID(riblock));
	/* If no default block is set then verify no conditional block either as it's
	   probably the final "quit" iblock. */
	if (false == ccIBlockGetDefaultBlock(riblock)) {
		assert(false == ccIBlockGetConditionalBlock(riblock));
		goto ret;
	}

	/* If the iblock has a conditional iblock, cache the branch opcode's offset-field location and
	   set the field value to the target iblock temporarily */
	//ccIBlockSetConditionalAddr(riblock, (Obj)(pccode-1)); // (ib, child)
	condBlock = ccIBlockGetConditionalBlock(riblock);
	if (false != condBlock) {
		assert(memIsObjectType(condBlock, TIBLOCK));
		memVectorSet(rcode, pccode-1, condBlock);
		ccIBlockSetConditionalTag(riblock, (Obj)(pccode-1));
	}

	/* if the iblock is the last iblock in the igraph vector or the next iblock is not my default */
	id = ccIBlockID(riblock);
	defBlock = ccIBlockGetDefaultBlock(riblock);
	if ((id == iblockCount - 1) || (defBlock != ccIBlock(id + 1))) {
		ccEmitOpcode2(vmBRA, false); /* Emit jump opcode */
		/* Cache jump opcode's offset-field location */
		//ccIBlockSetDefaultAddr(riblock, (Obj)(pccode-1));
		assert(false != defBlock);
		assert(memIsObjectType(defBlock, TIBLOCK));
		memVectorSet(rcode, pccode-1, defBlock);
		ccIBlockSetDefaultTag(riblock, (Obj)(pccode-1));
	}
ret:
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
		defBlock = memVectorObject(rcode, opcodeFieldAddr);
		assert(false != defBlock);
		defBlockAddr = ccIBlockTag(defBlock);
		assert(true != defBlockAddr); /* If it wasn't placed, it would be tagged #t */
 		/* Set the jump-opcode's offset */
		memVectorSet(rcode, opcodeFieldAddr, (Obj)(((Int)defBlockAddr-(Int)opcodeFieldAddr-1)*8));
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
		condBlock = memVectorObject(rcode, opcodeFieldAddr);
		assert(false != condBlock);
		condBlockAddr = ccIBlockTag(condBlock);
		assert(true != condBlockAddr); /* If it wasn't placed, it would be tagged #t */
 		/* Set the jump-opcode's offset */
		memVectorSet(rcode, opcodeFieldAddr, (Obj)(((Int)condBlockAddr-(Int)opcodeFieldAddr-1)*8));
	}
	DBEND();
}


/* For every iblock in the igraph, icode is emitted to the code object.
   The block's address in the code block are stored.  The branch field
   for branch instruction are also stored so when the offset can be
   determined, the branch opcode's field can be set quickly.

   riblock/r16  <= current iblock
   rcode/r1e    <= code emitted to
   pccode       <= C var code object index
*/
void ccAssembleAllIBlocks (void) {
 Num i;
	DBBEG();
	/* Place the blocks */
	for (i=0; i < iblockCount; ++i) {
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

void ccResolveBranchOpcodeAddresses (void) {
 Num i;
	DBBEG();
	/* Resolve branch offsets */
	for (i=0; i < iblockCount; ++i) {
		riblock = ccIBlock(i); /* Consider iblock from vector of all iblocks */
		/* Resolve my branch opcode offsets */
		ccResolveDefault();
		ccResolveConditional();
	}
//ccDumpIBlock(riblock);
//vmDebugDumpCode(rcode, stderr);
	DBEND();
}


/* Initialize iblock default and conditional tags with
   block ID immediate numbers
*/
void ccInitIBlockBranches (Obj ib) {
 Obj tag;

	tag = ccIBlockGetDefaultBlock(ib);
	if (true == tag) {
		/* Default block is via 'next logical' */
		ccIBlockSetDefault(ccIBlockID(ib), 1+ccIBlockID(ib)); 
	} else if (false != tag && !memIsObjectType(tag, TIBLOCK))
		/* Default block is via 'labeled block' */
		ccIBlockSetDefault(ccIBlockID(ib), (Num)memVectorObject(r3, (Num)tag));

	tag = ccIBlockGetConditionalBlock(ib);
	if (false != tag && !memIsObjectType(tag, TIBLOCK))
		/* Conditional block is a labeled block */
		ccIBlockSetConditional(ccIBlockID(ib), (Num)memVectorObject(r3, (Num)tag)); 
}

/* Recursively traverse the igraph's iblocks.  Tag each with #t.
   Also resolve default and conditional branch links.
*/
Num ccCountIGraphFields (Obj ib) {
 Num i, len=0;

	/* Base case.  Not an iblock or the iblock has been traversed already (tagged with #t) */
	if (!memIsObjectType(ib, TIBLOCK) || true == ccIBlockTag(ib)) return 0;

	ccIBlockSetTag(ib, true);
	ccInitIBlockBranches(ib);

	for (i=IBLOCK_INDEX_ICODE; i<memObjectLength(ib); ++i)
		len += memObjectLength(memVectorObject(ib, i));

	return len + ccCountIGraphFields(ccIBlockGetDefaultBlock(ib))
	           + ccCountIGraphFields(ccIBlockGetConditionalBlock(ib));
}


/* The IGraph's iblocks are found in rigraph/rf.  The icode found in each iblock
   and the links between icodes, are assembled into a VM code object object
   rcode/r1e which can be run in the VM.

         r0 <= head iblock object representing entire igraph
          r2 = icode
          r3 = field
   rcode/r1e => code object
*/
void ccAssembleIGraph (void) {
 Num len;
	DBBEG();

	/* Create the code block object which all iblocks are compile to */
	len = ccCountIGraphFields(ccIBlock(0));
	rcode = memNewVector(TCODE, len);
	pccode = 0;

	DB(NUM" opcode fields in igraph", len);

//sysDisplay(r3,stdout);

//ccDumpIBlocks();
	ccAssembleAllIBlocks();


	ccResolveBranchOpcodeAddresses();

//vmDebugDumpCode(rcode, stderr);
	DBEND();
}



/*******************************************************************************
 ASM

 Accept opcode and opcode fields and compile into an igraph.
*******************************************************************************/
Num IBlockCount = 0;
Num LabelIndex = 0;
void ccAsmLabelsReset() {
	LabelIndex = 0;
}

/* Generate a label for the label opcode and branch opcode address fields
*/
Num ccAsmLabelNew() {
	return LabelIndex++;
}

/* Prepare ASM for multiple calls to ccAsm()
*/
void ccAsmInit (void) {
	DBBEG();
	IBlockCount = 0;
	ccResetIGraph();
	riblocklast = false;

	objNewVector(LabelIndex);
	r3 = r0;
	DBEND();
}

/* Replace count icodes on stack with a new iblock containing the icodes.
*/
void ccGenerateIBlockWithPushedIcodes (Num count) {
 Num n;
	/* Create new empty iblock in riblock and pop icodes from stack into it */
	ccGenerateNewIBlock(count);
	for (n=1; n <= count; ++n)
		ccIBlockSetICode(count-n, vmPop());

	/* Connect the 'first' iblock generated by the current ASM call to
	   to the last iblock generated by the last call to ASM */
	if ((0 == IBlockCount) && (false != riblocklast)) {
		ccIBlockSetDefaultTag(riblocklast, riblock);  // TODO this fails when performed after direct iblock creation
	}

	/* Keep track of the last iblock generated.  See above statement. */
	riblocklast = riblock;

	++IBlockCount;
}


void ccAsmAsm (Obj f, ...) {
 va_list ap;
 Num ICodeCount = 0;
 Obj obj, r, rr, i, o, l;
	DBBEG ();

	IBlockCount = 0;

	/* Parse the opcode fields and create then push an icode object */
	for (va_start(ap, f), obj = f; (obj != END); obj = va_arg(ap, Obj)) {
		++ICodeCount;
		if        (MV == obj) {
			r = va_arg(ap, Obj);
			rr = va_arg(ap, Obj);
			ccNewICodeMV(r, rr);  vmPush(r0);
		} else if (MVI == obj) {
			r = va_arg(ap, Obj);
			o = va_arg(ap, Obj);
			ccNewICodeMVI(r, o);  vmPush(r0);
		} else if (ADDI == obj) {
			r = va_arg(ap, Obj);
			o = va_arg(ap, Obj);
			ccNewICodeADDI(r, o);  vmPush(r0);
		} else if (SYSI == obj) {
			o = va_arg(ap, Obj);
			ccNewICodeSYSI(o);  vmPush(r0);
		} else if (BNEI == obj) {
			r = va_arg(ap, Obj);
			i = va_arg(ap, Obj);
			o = va_arg(ap, Obj);
			ccNewICodeBNEI(r, i, o);  vmPush(r0);
			ccGenerateIBlockWithPushedIcodes(ICodeCount);
			ICodeCount = 0;
			ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */
			ccIBlockSetConditionalTag(riblock, o);  /* signal this block conditional is a label */
		} else if (BEQI == obj) {
			r = va_arg(ap, Obj);
			i = va_arg(ap, Obj);
			o = va_arg(ap, Obj);
			ccNewICodeBEQI(r, i, o);  vmPush(r0);
			ccGenerateIBlockWithPushedIcodes(ICodeCount);
			ICodeCount = 0;
			ccIBlockSetDefaultTag(riblock, true); /* signal this block's default is the next one */
			ccIBlockSetConditionalTag(riblock, o);  /* signal this block conditional is a label */
		} else if (BRA == obj) {
			o = va_arg(ap, Obj);
			ccNewICodeBRA(o);  vmPush(r0);
			ccGenerateIBlockWithPushedIcodes(ICodeCount);
			ICodeCount = 0;
			ccIBlockSetDefaultTag(riblock, o);  /* signal this block's default is a label */
		} else if (LABEL == obj) {
			l = va_arg(ap, Obj);
			/* Since labels aren't opcodes, undo the assumed icode counter at the beginning of this 'if block */
			--ICodeCount;
			if (0 < ICodeCount) {
				ccGenerateIBlockWithPushedIcodes(ICodeCount);
				ICodeCount = 0;
				ccIBlockSetDefaultTag(riblock, true);  /* default block is next */
			}
			/* Set the next block's ID in the label/iblockID table */
			memVectorSet(r3, (Num)l, (Obj)(iblockCount));
		} else if (QUIT == obj) {
			ccNewICodeQUIT();  vmPush(r0);
		} else {
			assert(!"Unhandled asm opcode");
		}
	}
	va_end(ap); /* stdarg */

	/* Might have to create an iblock with remaining icodes on stack */
	if (ICodeCount) ccGenerateIBlockWithPushedIcodes(ICodeCount);

//ccDumpIBlocks();
//sysWrite(r3, stdout); printf("\n");
	DBEND ();
}



/*******************************************************************************
 Compiler

 Compile a scheme expression into a VM code block.  Calls ASM and Assemble
 functions in this module.
*******************************************************************************/

/* Emit i-graph which looks up value of symbol in a local or global environment
   and assigns to r0
   expr <= symbol expression compiled
      r0 = temp
      r1 = temp
      r3 = temp
*/
void ccSymbol (Num flags) {
 Num ret, depth, offset;
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
			ccNewDefaultIBlock(ccIBlockID(riblock), 1);
				ccLDI(R0, R1C, (Obj)offset);
		} else {
			//asm(LDI01C); asm(0l);
			//for (d=1; d < depth; d++) asm(LDI00); asm(0l);
			//asm(LDI00); asm(offset);
		}
	} else {
		/* Scan tge... */
		sysTGEFind();
		if (null == r0) {
			DB("   can't find in TGE...maybe at runtime");
			//asm(MVI1); asm(rexpr);
			//asm(SYSI); asm(compTGELookup);
		} else {
			DB("   found in TGE");
			r3 = r0; /* Keep track of the symbol */
			ccNewDefaultIBlock(ccIBlockID(riblock), 2);
				ccMVI(R0, r3);
				ccLDI(R0, R0, 0);
		}
	}
	DBEND();
}

void ccCons (Num flags) {
	DBBEG();
	rexpr = cdr(rexpr); /* Consider cons expression's parameter list (A B)*/
	/* [A] */
	vmPush(cdr(rexpr)); /* Push (B) */
	rexpr = car(rexpr); /* Compile A expression */
	ccCompileExpr(flags);
	rexpr = vmPop(); /* Restore (B) */

	ccNewDefaultIBlock(ccIBlockID(riblock), 1);
	ccPUSH(R0);

	rexpr = car(rexpr); /* Compile B expression */
	ccCompileExpr(flags);

	ccNewDefaultIBlock(ccIBlockID(riblock), 2);
	ccPOP(R1);
	ccSYSI(objCons10);

	DBEND();
}

void ccAdd (Num flags) {
	DBBEG();
	DBEND();
}

void ccIf (Num flags) {
 Num branchid, endid, altid;
	DBBEG();
	rexpr = cdr(rexpr); /* Consider if expression's parameter list (TEST CONSEQUENT ALTERNATE)*/

	/* [TEST] */
	vmPush(cdr(rexpr)); /* Push (CONSEQUENT ALTERNATE) */
	rexpr = car(rexpr); /* Compile TEST expression */
	ccCompileExpr(flags);
	rexpr = vmPop(); /* Restore (CONSEQUENT ALTERNATE) */

	/* [TEST]---[BRANCH] */
	branchid = ccNewDefaultIBlock(ccIBlockID(riblock), 1);
	ccBRTI(R0, TFALSE, 0);

	/* [TEST]---[BRANCH]---[CONSEQUENT] */
	vmPush(cdr(rexpr)); /* Push (ALTERNATE) */
	rexpr = car(rexpr); /* Compile CONSEQUENT expression */
	ccCompileExpr(flags);
	rexpr = vmPop(); /* Restore (ALTERNATE) */

	/* [TEST]---[BRANCH]---[CONSEQUENT]---[END] */
	endid = ccNewDefaultIBlock(ccIBlockID(riblock), 1);
	ccNOP();

	if (null == rexpr) {
		/*                  .----------------.
		   [TEST]---[BRANCH]---[CONSEQUENT]---[END]
		   No alternate clause so just default to end block */
		ccIBlockSetConditional(branchid, endid);
	} else {
		/*                 .--[NOP]-[ALTERNATE]--.
		   [TEST]---[BRANCH]-----[CONSEQUENT]----[END] */
		/* Consider alternate */
		rexpr = car(rexpr);
		/* Create a leading alternate block since there's currently no way
		   to tell ccCompileExpr to attach to the branch block's alternate. */
		altid = ccNewConditionalIBlock(branchid, 1);
		ccNOP();
		/* Compile ALTERNATE expression.  It becomes the leading alternate block's default */
		ccCompileExpr(flags);
		/* Force the newly compiled alternate expression's default block to the already created end block */
		ccIBlockSetDefault(ccIBlockID(riblock), endid);

		riblock = ccIBlock(endid); /* Done block is now the last iblock */
	}

ccDumpIBlocks();
	DBEND();
}

void ccCombination (Num flags) {
	DBBEG();
	DBEND();
}

void ccSelfEvaluating (Num flags) {
	DBBEG();
	//ccNewDefaultIBlock(ccIBlockID(riblock), 1);
		//ccMVI(R0, rexpr);
	ccAsm (
		MVI, R0, rexpr
	);
	DBEND();
}

/* Recursive scheme expression compiler.  Translates an expression in
   expr/r15 onto the end of the igraph in igraph/rf.
*/
void ccCompileExpr (Num flags) {
	DBBEG("Create intermediate graph data structure");

	switch (memObjectType(rexpr)) {
		case TSYMBOL :
			ccSymbol(flags);
			break;
		case TPAIR :
			if      (scons      == car(rexpr)) ccCons(flags);
			else if (sadd       == car(rexpr)) ccAdd(flags);
			else if (sif        == car(rexpr)) ccIf(flags);
			else ccCombination(flags);
			break;
		default:
			ccSelfEvaluating(flags);
			break;
	}

	DBEND();
}

/* Compiles the expression in expr/r15 into an intermediate graph then
   asembles the igraph into a VM code object.
   rexpr/r15 <= scheme expression to compile
   rcode/r1e  => VM code block
*/
void ccCompile () {

	ccAsmInit ();

	/* Head iblock of the igraph */
	ccGenerateNewIBlock(1);
	ccNOP();
	riblocklast = riblock;
	ccCompileExpr(0);

	/* Finalize the iblock with an iblock containing the VM quit op */
	ccNewDefaultIBlock(ccIBlockID(riblock), 1);
	ccQUIT();

	ccAssembleIGraph();
}



/*******************************************************************************
 REPL
 Read a scheme expression, evaluate and print the results
*******************************************************************************/
void repl (void) {
	DBBEG();
	yyrestart(0);   /* Tell scanner to use stdin/0 as input. */
	while (1) {
		renv = rtge; /* Evaluate in TGE */
		fprintf(stderr, "\n== Read and parse ===============\nWSCM>");
		yyparse();/* Expr read into r0. */

		if (eof == r0) break;

		fprintf(stderr, "\n== Compile ======================\n");
		sysWrite(r0, stderr);
		rexpr = r0;
		ccCompile(0);
		rip = 0;
		ccDumpIBlocks ();
		vmDebugDumpCode(rcode, stderr);

		fprintf(stderr, "== Execute and return value =====\n");
		vmRun();
		sysDisplay(r0, stderr);
		//DBE fprintf(stderr, "== Debug =======================");
		//DBE memDebugDumpHeapHeaders(stderr);
		//DBE sysWrite(rstack, stderr);
		//DBE for (i=memStackLength(rstack); 0<i; --i) sysWrite(memStackObject(rstack, i-1), stdout);
	}
	fprintf (stderr, "WEL loop done\n");
	DBEND();
}



/*******************************************************************************
 Init
*******************************************************************************/
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
		DB("Creating static symbols");
		objNewSymbolStatic("mv"); smv = r0;
		objNewSymbolStatic("mvi"); smvi = r0;
		objNewSymbolStatic("ldi"); sldi = r0;
		objNewSymbolStatic("bnei"); sbnei = r0;
		objNewSymbolStatic("beqi"); sbeqi = r0;
		objNewSymbolStatic("brti"); sbrti = r0;
		objNewSymbolStatic("bra"); sbra = r0;
		objNewSymbolStatic("sysi"); ssysi = r0;
		objNewSymbolStatic("push"); spush = r0;
		objNewSymbolStatic("pop"); spop = r0;
		objNewSymbolStatic("addi"); saddi = r0;
		objNewSymbolStatic("quit"); squit = r0;
		objNewSymbolStatic("nop"); snop = r0;
		DB("Creating the global environment with predefined symbols 'x' and 'y'");
		objNewSymbol((Str)"T.G.E.", 3);
		r1=r0;  r2=null;  objCons12();  renv=rtge=r0;
		objNewInt(42); sysDefine ("x"); /* It's always nice to have x and y defined with useful values */
		objNewInt(69); sysDefine ("y"); /* It's always nice to have x and y defined with useful values */
		DB("Initializing compiler related objects");
		objNewVector(0x1000);
		rigraph = r0;
	} else {
		DB("Module already activated");
	}
	DBEND();
}

int mmain (int argc, char *argv[]) {
	ccInitialize();
	repl();
	return 0;
}


#undef DB_DESC
#undef DEBUG
