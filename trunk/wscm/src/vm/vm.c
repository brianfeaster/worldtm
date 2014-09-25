#define DEBUG 0
#define DB_DESC "VM "
#define VALIDATE 0
#include "debug.h"
#include <stdio.h>
#include <assert.h>
#include "vm.h"
#include "mem.h"
/*
 Virtual_Machine
 Serializer
 Debugging
 Init

 About vm.c

   While the virtual machine is running, register rip/r1d, which is the opcode
   index, is transformed into a pointer into the rcode/re vector object.  This
   transformation is undone and rip reverted to an immediate index value when
   a SYS opcode is performed or the interrupt handler called. 
*/



void vmPush (Obj o)    {        memVecStackPush(rstack, o); } 
Obj  vmPop (void)      { return memVecStackPop(rstack); }
void vmAryPush (Obj o) {        memAryStackPush(rdstack, o); }
Obj  vmAryPop (void)   { return memAryStackPop(rdstack); }


void vmVecPushInternal (Obj o) { *(Obj*)rstackp=o; rstackp+=ObjSize; }
Obj  vmVecPopInternal  (void)  { rstackp-=8; return *(Obj*)rstackp; }
void vmAryPushInternal (Obj o) { *(Obj*)rdstackp=o; rdstackp+=ObjSize; }
Obj  vmAryPopInternal  (void)  { rdstackp-=8; return *(Obj*)rdstackp; }

int vmRunCount=0;

void vmRunRestore (void) {
	DBBEG("  ip:"OBJ"  code:"OBJ, rip, rcode);
	assert(1 == vmRunCount--); // Verify non-reentrant
	// Set the stack's internal pointer to the correct offset.
	*(Obj*)rstack = rstackp = (Obj)((rstackp - rstack));
	*(Obj*)rdstack = rdstackp = (Obj)((rdstackp - rdstack));
	rip = (Obj)(rip - rcode);
	DBEND("  ip:"OBJ"  code:"OBJ, rip, rcode);
}

void vmRunSetup (void) {
	DBBEG("  ip:"OBJ"  code:"OBJ, rip, rcode);
	assert(0 == vmRunCount++); // Verify non-reentrant
	rstackp = (Obj)(rstack + *(Num*)rstack);
	rdstackp = (Obj)(rdstack + *(Num*)rdstack);
	rip = (Obj)(rcode + (Num)rip);
	DBEND("  ip:"OBJ"  code:"OBJ, rip, rcode);
}


/* This flag causes the virtual machine to make a call to the interrupt handler.
   It is set in this module by the timer signal handler.  It is also set
   in the wscm module by wscmSignalHandler() the generic signaler handling mechanism.
*/
Int vmInterrupt = 0;
Func vmInterruptHandler = NULL;

void vmProcessInterrupt (void) {
	DBBEG("  rcode:"OBJ" rip:"OBJ, rcode, rip);
	assert (vmInterrupt);
	assert (vmInterruptHandler);
	vmInterrupt = 0;
	vmRunRestore();
	vmInterruptHandler();
	vmRunSetup();
	DBEND("  rcode:"OBJ" rip:"OBJ, rcode, rip);
}



/*******************************************************************************
 Virtual_Machine
*******************************************************************************/

/* Registers r00 through r0f should be C and World Scheme Mem module objects.
   r10 through r1f can be any value but must not represent Mem objects since
   the garbage collector would try and relocate them.
 */
Obj r00, r01, r02, r03, r04, r05, r06, r07, r08, r09, r0a, r0b, r0c, r0d, r0e, r0f;
Obj r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r1a, r1b, r1c, r1d, r1e, r1f;


/* Opcodes.  Create all the global opcode goto label pointers.
    VMOP's parameter o is ignored as it is only required for the serializer.
    void *NOP;
    void *MV_R00_I;
    void *PUSH_R01;
    ...
*/
#define _
#define VMOP(op,d,n,i,o) VMOP_(op, _##d, _##n, _##i)
#define VMOP_(op,d,n,i) VMOP__(op, d, n, i)
#define VMOP__(op,d,n,i) VMOP___(vm##op##d##n##i)
#define VMOP___(op) void *op;
// Load and transform the opcode definitions
#include "op.h"
// Cleanup the unneeded macros
#undef VMOP___
#undef VMOP__
#undef VMOP_
#undef VMOP
#undef _


void vmVm (void) {
 static Num NeedToInitialized = 1;
	if (NeedToInitialized) {
		NeedToInitialized = 0;
		DBBEG("  Initializing opcodes' addresses and strings");

		/* Assign C jump label addresses of each opcode implementation to each opcode object symbol.  Tricky
		   macros are used along with a shared opcode definitions file.  Each opcode definition in the file
		   is transformed into:

		   NOP        = &&NOP;        MEM_ADDRESS_REGISTER(NOP);
		   MV_R00_R01 = &&MV_R00_R01; MEM_ADDRESS_REGISTER(MV_R00_R01);
		   PUSH_R00   = &&PUSH_R00;   MEM_ADDRESS_REGISTER(PUSH_R00);
		   ...

		   Associate an opcode symbol with a goto address (for the virtual machine) and a string (for
         debug dumps) Prepare for opcode definition transformation.

			VMOP's parameter o (branch offset) is ignored as it is only required for the serializer.
		*/
		#define _
		#define VMOP(op,d,n,i,o) VMOP_(op, _##d, _##n, _##i)
		#define VMOP_(op,d,n,i) VMOP__(op, d, n, i)
		#define VMOP__(op,d,n,i) VMOP___(vm##op##d##n##i)
		#define VMOP___(op) op=&&op; MEM_ADDRESS_REGISTER(op);
		// Load and transform the opcode definitions
		#include "op.h"
		// Cleanup the unneeded macros
		#undef VMOP___
		#undef VMOP__
		#undef VMOP_
		#undef VMOP
		#undef _

		DBEND();
		return;
	}

	/* Convert the normalized ip and stack pointers to actual addresses.  This must be undone whenever
	   the VM is existed or interrupted either via an actual interrupt or syscall.
	*/
	vmRunSetup();

	// Jump to the instruction register's (rip==r1d) goto label address.  First honor any interrupts.
	if (vmInterrupt) vmProcessInterrupt();

	#define GOTOIP goto **(void**)rip

	// Run machine code execution by "goto'ing" the instruction pointer (rip==r1d pointing inside of rcode==r0d)
	DBBEG("  Starting VM:  ip="HEX"  code="HEX"  *ip="HEX, rip, rcode, *(void**)rip);
	GOTOIP;

	// Opcode arguments for a register or immediate
	#define arg
	#define argR00 r00
	#define argR01 r01
	#define argR02 r02
	#define argR03 r03
	#define argR04 r04
	#define argR05 r05
	#define argR06 r06
	#define argR07 r07
	#define argR08 r08
	#define argR09 r09
	#define argR0A r0a
	#define argR0B r0b
	#define argR0C r0c
	#define argR0D r0d
	#define argR0E r0e
	#define argR0F r0f
	#define argR10 r10
	#define argR11 r11
	#define argR12 r12
	#define argR13 r13
	#define argR14 r14
	#define argR15 r15
	#define argR16 r16
	#define argR17 r17
	#define argR18 r18
	#define argR19 r19
	#define argR1A r1a
	#define argR1B r1b
	#define argR1C r1c
	#define argR1D r1d
	#define argR1E r1e
	#define argR1F r1f
	#define argI   *(Obj*)(rip+1*ObjSize)

	#define imm
	#define immR02 + (Num)r02
	#define immR10 + (Num)r10
	#define immR11 + (Num)r11
	#define immI   + *(Num*)(rip+1*ObjSize)

	/* Expression for incrementing the instruction pointer via the offset
	   field whos location depends on the existence of an immediate field.
	*/
	#define IPINCO       rip+=*(Num*)(rip+1*ObjSize)
	#define IPINCOI      rip+=*(Num*)(rip+2*ObjSize)

	/* Expression for incrementing the instruction pointer based on the
      last opcode argument.
	*/
	#define IPINC  rip+=1*ObjSize
	#define IPINC2 rip+=2*ObjSize
	#define IPINCR00 IPINC
	#define IPINCR01 IPINC
	#define IPINCR02 IPINC
	#define IPINCR03 IPINC
	#define IPINCR04 IPINC
	#define IPINCR05 IPINC
	#define IPINCR07 IPINC
	#define IPINCR08 IPINC
	#define IPINCR0B IPINC
	#define IPINCR0C IPINC
	#define IPINCR0D IPINC
	#define IPINCR0E IPINC
	#define IPINCR10 IPINC
	#define IPINCR11 IPINC
	#define IPINCR12 IPINC
	#define IPINCR13 IPINC
	#define IPINCR00R11 IPINC
	#define IPINCR01R02 IPINC
	#define IPINCR01R11 IPINC
	#define IPINCR00I IPINC2
	#define IPINCR01I IPINC2
	#define IPINCR02I IPINC2
	#define IPINCR03I IPINC2
	#define IPINCR05I IPINC2
	#define IPINCR07I IPINC2
	#define IPINCR0BI IPINC2
	#define IPINCR0CI IPINC2
	#define IPINCR1FI IPINC2
	#define IPINCI    IPINC2

	/* NOP
	   NOP: OPDB("NOP");  goto **(void**)(rip += ObjSize);
	*/
	#define VMOP_NOP(d,n,i) \
	   IPINC; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* MV Rd [Rn | I]
	   MV_R00_I: OPDB("MV_R00_I");    r00 = *(Obj*)(rip+8);  goto **(void**)(rip+=2*ObjSize);
	   MV_R00_I: OPDB("MV_R00_R01");  r00 = r01;             goto **(void**)(rip+=ObjSize);
	*/
	#define VMOP_MV(argd,argn,argi) \
	   arg##argd = arg##argn arg##argi; \
	   IPINC##argi; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* LD Rd Rn [Rm | I]
	   LD_R00_R01:     OPDB("LD_R00_R01");    r00 = *(Obj*)r01;                   goto **(void**)(rip+=ObjSize);
	   LD_R00_I:       OPDB("LD_R00_I");      r00 = *(Obj*)(*(Obj*)(rip+8));      goto **(void**)(rip+=ObjSize); TODO this should proably be *(Obj*)( *(Obj*)(rip+8) )  But since an equivalent ST op wouldn't make sense, maybe this shouldn't exist?  (Unless I want self modifying code?)
	   LD_R00_R01_I:   OPDB("LD_R00_R01_I");  r00 = *(Obj*)(r01 + *(Num*)(rip+8));goto **(void**)(rip+=ObjSize);
	   LD_R00_R01_R02: OPDB("LD_R00_R01_R02");r00 = *(Obj*)(r01 + (Num)r02);      goto **(void**)(rip+=ObjSize);
	*/
	#define VMOP_LD(argd,argn,argi) \
	   arg##argd = *(Obj*)(arg##argn imm##argi); \
	   IPINC##argn##argi; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* ST Rd Rn (Rm | I)
	   ST_R00_R01:   OPDB("ST_R00_R01");  *(Obj*)r01                  = r00;  goto **(void**)(rip+=ObjSize);
	   ST_R00_I:     OPDB("ST_R00_I");    *(Obj*)*(*(Obj)(rip+8))     = r00;  goto **(void**)(rip+=ObjSize);
	   ST_R00_R01_I: OPDB("ST_R00_R01");  *(Obj*)(r01 *(Num*)(rip+8)) = r00;  goto **(void**)(rip+=ObjSize);
	   ST_R00_I:     OPDB("ST_R00_I");    *(Obj*)(rip+8)              = r00;  goto **(void**)(rip+=ObjSize);
	*/
	#define VMOP_ST(argd,argn,argi) \
	   *(Obj*)(arg##argn imm##argi) = arg##argd; \
	   IPINC##argn##argi; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* PUSH Rd
	   PUSH_R00: OPDB("PUSH_R00");  vmVecPushInternal(r00);  goto **(void**)(rip+=ObjSize);
	*/
	#define VMOP_PUSH(argd,argn,argi) \
	   vmVecPushInternal(arg##argd); \
	   IPINC; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* POP Rd
	   POP_R00: OPDB("POP_R00");  r00 = vmVecPopInternal();  goto **(void**)(rip+=ObjSize);
	*/
	#define VMOP_POP(argd,argn,argi) \
	   arg##argd=vmVecPopInternal(); \
	   IPINC; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* ADD Rd [Rn | I]
	   ADD_R00_I:   OPDB("ADD_R00_I");    r00 += *(Num*)(rip+8);  goto **(void**)(rip+=2*ObjSize);
	   ADD_R00_R01: OPDB("ADD_R00_R01");  r00 += (Num)R01;        goto **(void**)(rip+=ObjSize);
	*/
	#define VMOP_ADD(argd,argn,argi) \
	   arg##argd = (Obj)((Int)arg##argd + (Int)(arg##argn imm##argi)); \
	   IPINC##argn##argi; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* MUL Rd [Rn | I]
	   MUL_R00_I:   OPDB("MUL_R00_I");    r00 *= *(Num*)(rip+8);  goto **(void**)(rip+=2*ObjSize);
	   MUL_R00_R01: OPDB("MUL_R00_R01");  r00 *= (Num)R01;        goto **(void**)(rip+=ObjSize);
	*/
	#define VMOP_MUL(argd,argn,argi) \
	   arg##argd = (Obj)((Int)arg##argd * (Int)(arg##argn imm##argi)); \
	   IPINC##argn##argi; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* AND Rd I
	*/
	#define VMOP_AND(argd,argn,argi) \
	   arg##argd = (Obj)((Num)arg##argd & (Num)arg##argn imm##argi); \
	   IPINC##argi; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* LSL Rd I
	*/
	#define VMOP_LSL(argd,argn,argi) \
	   arg##argd = (Obj)((Num)arg##argd << (Num)arg##argn imm##argi); \
	   IPINC##argi; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* LSR Rd I
	*/
	#define VMOP_LSR(argd,argn,argi) \
	   arg##argd = (Obj)((Num)arg##argd >> (Num)arg##argn imm##argi); \
	   IPINC##argi; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* BEQ Rd I O
	*/
	#define VMOP_BEQ(argd,argn,argi) \
	   if (arg##argd == arg##argn arg##argi) { \
	      rip += *(Num*)(rip + 2*ObjSize); \
	   } else { \
	      rip += 3 * ObjSize;\
	   } \
	   if (vmInterrupt) vmProcessInterrupt()

	/* BNE [Rd | I]
	*/
	#define VMOP_BNE(argd,argn,argi) \
	   if (arg##argd != arg##argn arg##argi) { \
	      rip += *(Num*)(rip + 2*ObjSize); \
	   } else { \
	      rip += 3*ObjSize;\
	   } \
	   if (vmInterrupt) vmProcessInterrupt()

	/* BLT [Rd | I]
	*/
	#define VMOP_BLT(argd,argn,argi) \
	   if (arg##argd < arg##argn arg##argi) { \
	      rip += *(Num*)(rip + 2*ObjSize); \
	   } else { \
	      rip += 3*ObjSize;\
	   } \
	   if (vmInterrupt) vmProcessInterrupt()

	/* BGT [Rd | I]
	*/
	#define VMOP_BGT(argd,argn,argi) \
	   if (arg##argd > arg##argn arg##argi) { \
	      IPINCO##argi; \
	   } else { \
	      IPINC; IPINC##argi; \
	   } \
	   if (vmInterrupt) vmProcessInterrupt()

	/* BRA I
	*/
	#define VMOP_BRA(argd,argn,argi) \
	   rip += *(Num*)(rip + 1*ObjSize); \
	   if (vmInterrupt) vmProcessInterrupt()

	/* JMP [Rd | I]
	*/
	#define VMOP_JMP(argd,argn,argi) \
	   rip = rcode = arg##argd; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* JAL Rd
	    Link block/offset then jump to first instruction in block in acc.
	    JAL_R00: OPDB("JAL_R00");

	    TODO handle immediate
	*/
	#define VMOP_JAL(argd,argn,argi) \
	   riplink   = (Obj)(rip - rcode + 1*ObjSize); \
	   rcodelink = rcode; \
	   renvlink  = renv; \
	   rip = rcode = arg##argd; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* RET
	    Return from a JAL call.  Copy link register back to their parent registers.
	*/
	#define VMOP_RET(argd,argn,argi) \
	   renv  = renvlink; \
	   rcode = rcodelink; \
	   rip   = rcode + (Int)riplink; \
	   if (vmInterrupt) vmProcessInterrupt()

	/* SYS [Rd | I]
	    Syscall to C function in either a register or an immediate. Sets
	    the rip to next instruction first so that the syscall runs with
	    the IP at the next instruction.
	*/ 
	#define ADDR
	#define ADDRR00 r00
	#define ADDRI   *(Obj*)(rcode + (Num)rip - 1*ObjSize)
	#define VMOP_SYS(argd,argn,argi) \
	   IPINC##argi; \
	   vmRunRestore(); \
	   ( (Func) ADDR##argd ADDR##argi ) (); \
	   vmRunSetup(); \
	   if (vmInterrupt) vmProcessInterrupt()

	/* QUIT
	    Exits the virtual machine
	*/
	#define VMOP_QUIT(argd,argn,argi) \
	   vmRunRestore(); \
	   DBEND(); \
	   return


	/* Creates the opcode implementation of the form:
	    {C goto label} : {debug message displaying the code, ip, and opcode during runtime};
	    {one of the opcode implementations defined above};
	    {call to jump to the next opcode pointed to by the instruction pointer ip=r1d};
      VMOP's parameter o is ignored as it is only required for the serializer.
	*/

	/* Debug dump the current opcode */
	#if DEBUG == 1
	 #define OPDB(s,...) DBE fprintf(stderr,"\n"OBJ":"HEX" " s, rcode, rip-rcode, ##__VA_ARGS__);
	#else
	 #define OPDB(s,...)
	#endif

	#define _
	#define VMOP(op,d,n,i,o) VMOP_(op, _##d, _##n, _##i, d, n, i)
	#define VMOP_(op,dd,nn,ii,d,n,i) VMOP__(op, dd, nn, ii, d, n, i)
	#define VMOP__(op,dd,nn,ii,d,n,i) \
	   vm##op##dd##nn##ii: \
	   OPDB("vm"#op#dd#nn#ii) \
	   VMOP_##op(d, n, i); \
	   GOTOIP;

	// Load and transform the opcode definitions into actual opcode implementations
	#include "op.h"

	// Cleanup the unneeded macros
	#undef VMOP__
	#undef VMOP_
	#undef VMOP
	#undef _
}


/* Starts virtual machine using the program code (rc) starting at instruction
   offset in immediate rip (r1b).
*/
void vmRun (void) {
	DBBEG("  code:"OBJ  " ip:"INT, rcode, rip);
	assert(memIsObjectType(rcode, TCODE) && "Not a code type");
	vmVm();
	DBEND();
}



/*******************************************************************************
 Serializer
*******************************************************************************/
/* Default Object serializer and its mutable callback pointer.
 */
void vmObjectDumperDefault (Obj o, FILE *stream) {
 static Str p;
	fprintf (stream, "#<"HEX, o);
	if ((p = memAddressString(o))) fprintf (stream, ":%s", p);
	fprintf (stream, ">");
}

void (*vmObjectDumper)(Obj o, FILE *tream) = vmObjectDumperDefault;


/* Convert an instruction pointer (Obj) to an instruction address
   given the code object pointer (Obj) it is pointing into.
*/
Int vmOffsetToPosition (Obj codeBlock, Int lineNumber, Obj *instPtr) {
 Int pos = lineNumber + (Int)*instPtr;
	return (memObjectLength(codeBlock)*ObjSize < pos) ? (Int)*instPtr : pos;
}

void vmDisplayTypeCode (Obj c, FILE *stream) {
 Int idx=0, lineNumber;
 static char *buff="                                ";
 static int indent=-1;
 char *ind;

	DBBEG ("  "OBJ"  rcode:"OBJ"  rip:"OBJ, c, rcode, rip);
	assert(stream);
	assert(memIsObjectType(c, TCODE));

	memRootSetAddressRegister(&c); // Protect pointer in case of a GC

	++indent;
	if (indent < 32) ind = buff+32-indent;
	else ind = buff;

	/* idx is in ObjSize bytes
	*/
	while (idx < memObjectLength(c)) {
		lineNumber = idx;

		fprintf (stream, NL STR OBJ STR HEX04" ",
			ind,
			c, //c+idx*ObjSize
			((rip - c) == idx*ObjSize) || ((Num)rip == idx*ObjSize) ? "*" : " ", // Indicate current instruction pointer if register "rip" is an address within the code object or object byte offset to this instruction
			idx*ObjSize);

		/* Setup macro aliases for opcode serializer generation to produce the following:
		    if (*idx == vmNOP) {fprintf(stream, "%-4s" "    " "    ", "NOP"); ; ; } else
		    if (*idx == vmMV_R00_I) {fprintf(stream, "%-4s" " $00" "    ", "MV"); fprintf(stream," ");vmObjectDumper(*++idx, stream); ; } else
		    if (*idx == vmBEQ_R00_I) {fprintf(stream, "%-4s" " $00" "    ", "BEQ"); fprintf(stream," ");vmObjectDumper(*++idx, stream); fprintf(stream," ""%04lx", vmOffsetToPosition(c, lineNumber, ++idx));; } else
		    ...
		*/
		#define PRINTREG    "    "
		#define PRINTREGI
		#define PRINTREGR00 " $00"
		#define PRINTREGR01 " $01"
		#define PRINTREGR02 " $02"
		#define PRINTREGR03 " $03"
		#define PRINTREGR04 " $04"
		#define PRINTREGR05 " $05"
		#define PRINTREGR06 " $06"
		#define PRINTREGR07 " $07"
		#define PRINTREGR08 " $08"
		#define PRINTREGR09 " $09"
		#define PRINTREGR0A " $0a"
		#define PRINTREGR0B " $0b"
		#define PRINTREGR0C " $0c"
		#define PRINTREGR0D " $0d"
		#define PRINTREGR0E " $0e"
		#define PRINTREGR0F " $0f"
		#define PRINTREGR10 " $10"
		#define PRINTREGR11 " $11"
		#define PRINTREGR12 " $12"
		#define PRINTREGR13 " $13"
		#define PRINTREGR14 " $14"
		#define PRINTREGR1C " $1c"
		#define PRINTREGR1D " $1d"
		#define PRINTREGR1E " $1e"
		#define PRINTREGR1F " $1F"

		#define PRINT
		// This is expanded for the opcode with three registers so the immediate field is not rendered.  [LD $00 $01 $02] is the only opcode for now
		#define PRINTR02
		#define PRINTR11
		#define PRINTIO ++idx; fprintf(stream, " #<"OBJ0"> ", *(Obj*)(c+idx*ObjSize)); ++idx; fprintf(stream,    HEX04" ", vmOffsetToPosition(c, lineNumber*ObjSize, c+idx*ObjSize));  vmObjectDumper(*(Obj*)(c+(idx-1)*ObjSize), stream); 
		#define PRINTI  ++idx; fprintf(stream, " #<"OBJ0"> ", *(Obj*)(c+idx*ObjSize));        fprintf(stream, "     ");                                                               vmObjectDumper(*(Obj*)(c+(idx)*ObjSize), stream); 
		#define PRINTO  ++idx; fprintf(stream, ""HEX04,    vmOffsetToPosition(c, lineNumber*ObjSize, c+idx*ObjSize));

		#define _
		#define VMOP(op,d,n,i,o) VMOP_(op, _##d, _##n, _##i, d, n, i, o)
		#define VMOP_(op,dd,nn,ii,d,n,i,o) VMOP__(op, dd, nn, ii, d, n, i, o)
		#define VMOP__(op,dd,nn,ii,d,n,i,o) \
		   if (*(Obj*)(c+idx*ObjSize) == vm##op##dd##nn##ii) {fprintf(stream, STR4 PRINTREG##d PRINTREG##n PRINTREG##i, #op); PRINT##i##o } else

		// Load and transform the opcode definitions into serializing logic
		#include "op.h"

		// Cleanup the unneeded macros
		#undef VMOP__
		#undef VMOP_
		#undef VMOP
		#undef _
      // else clause -- the default opcode serializer case
		{
			fprintf(stream, "??? ");
			vmObjectDumper(*(Obj*)(c+idx*1*ObjSize), stream);
		}

		++idx;
		fflush(stdout);
	}

	printf (NL);

	--indent;
	memRootSetAddressUnRegister(&c);

	DBEND ();
}



/*******************************************************************************
 Debugging
*******************************************************************************/
void vmPrintRegisters (FILE *stream)
{
	fprintf(stream, "\n-- vmPrintRegisters ----");
	fprintf(stream, "\n       $00 "OBJ"       $10 "OBJ0,  r00, r10);
	fprintf(stream, "\n       $01 "OBJ"       $11 "OBJ0,  r01, r11);
	fprintf(stream, "\n       $02 "OBJ"       $12 "OBJ0,  r02, r12);
	fprintf(stream, "\n       $03 "OBJ"       $13 "OBJ0,  r03, r13);
	fprintf(stream, "\n       $04 "OBJ"       $14 "OBJ0,  r04, r14);
	fprintf(stream, "\n       $05 "OBJ"       $15 "OBJ0,  r05, r15);
	fprintf(stream, "\n       $06 "OBJ"       $16 "OBJ0,  r06, r16);
	fprintf(stream, "\n       $07 "OBJ"       $17 "OBJ0,  r07, r17);
	fprintf(stream, "\n       $08 "OBJ"       $18 "OBJ0,  r08, r18);
	fprintf(stream, "\n tge   $09 "OBJ"       $19 "OBJ0,  r09, r19);
	fprintf(stream, "\n envl  $0a "OBJ"       $1a "OBJ0,  r0a, r1a);
	fprintf(stream, "\n env   $0b "OBJ"       $1b "OBJ0,  r0b, r1b);
	fprintf(stream, "\n codel $0c "OBJ" ipl   $1c "OBJ0,  r0c, r1c);
	fprintf(stream, "\n code  $0d "OBJ" ip    $1d "OBJ0,  r0d, r1d);
	fprintf(stream, "\n istk  $0e "OBJ" istkp $1e "OBJ0,  r0e, r1e);
	fprintf(stream, "\n ostk  $0f "OBJ" ostkp $1f "OBJ0,  r0f, r1f);
}



/*******************************************************************************
 Init
*******************************************************************************/
void vmInitialize (Func interruptHandler, Func2ObjFile vmObjDumper) {
 static Num shouldInitialize=1;
	DBBEG();
	if (shouldInitialize) {
		DB("Activating module");
		shouldInitialize = 0;
		memInitialize(0, 0, 0);

		DB("Registering rootset objects pointers r00 through r0f");
		#define REGISTER_ROOT_OBJECT(o) {memRootSetAddressRegister(&o); MEM_ADDRESS_REGISTER(&o);}
		REGISTER_ROOT_OBJECT(r00);  REGISTER_ROOT_OBJECT(r01);  REGISTER_ROOT_OBJECT(r02);  REGISTER_ROOT_OBJECT(r03);
		REGISTER_ROOT_OBJECT(r04);  REGISTER_ROOT_OBJECT(r05);  REGISTER_ROOT_OBJECT(r06);  REGISTER_ROOT_OBJECT(r07);
		REGISTER_ROOT_OBJECT(r08);  REGISTER_ROOT_OBJECT(r09);  REGISTER_ROOT_OBJECT(r0a);  REGISTER_ROOT_OBJECT(r0b);
		REGISTER_ROOT_OBJECT(r0c);  REGISTER_ROOT_OBJECT(r0d);  REGISTER_ROOT_OBJECT(r0e);  REGISTER_ROOT_OBJECT(r0f);

		DB("Registering rootset objects pointers r10 through r1f");
		#define REGISTER_DATA_REG(v) MEM_ADDRESS_REGISTER(&v);
		REGISTER_DATA_REG(r10);  REGISTER_DATA_REG(r11);  REGISTER_DATA_REG(r12);  REGISTER_DATA_REG(r13);
		REGISTER_DATA_REG(r14);  REGISTER_DATA_REG(r15);  REGISTER_DATA_REG(r16);  REGISTER_DATA_REG(r17);
		REGISTER_DATA_REG(r18);  REGISTER_DATA_REG(r19);  REGISTER_DATA_REG(r1a);  REGISTER_DATA_REG(r1b);
		REGISTER_DATA_REG(r1c);  REGISTER_DATA_REG(r1d);  REGISTER_DATA_REG(r1e);  REGISTER_DATA_REG(r1f);

		DB("Create the array stack");
		rdstack = memNewAryStack();

		DB("Create the vector stack");
		rstack = memNewVecStack();

		DB("Register the internal object types");
		memTypeStringRegister(TCODE, (Str)"code");

		DB("Initialize opcode values");
		vmVm(); /* The first call to vmVm() initializes opcode values */
	} else {
		DB("Module already activated");
	}

	if (interruptHandler) {
		DB("Setting interrupt handler callback function");
		assert(!vmInterruptHandler);
		vmInterruptHandler = interruptHandler;
	}
	if (vmObjDumper) {
		DB("Setting vmObjDumper callback function");
		assert(vmObjectDumperDefault == vmObjectDumper); /* Verify the object dump callback is changed once */
		vmObjectDumper = vmObjDumper;
	}
	DBEND();
}


#undef DB_DESC
#undef DEBUG
