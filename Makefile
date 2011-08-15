# Makefile for Wscheme.  It's simple.

CFLAGS = -Wall -ggdb -Wno-format -Wno-trigraphs -march=native -Wconversion -O3
#CFLAGS = -D_GNU_SOURCE -march=pentium -Wall -Wno-format -Wno-trigraphs -ggdb -Wconversion -O3
# -Wall            -- Many warnings.
# -Wno-format      -- Disable the annoying printf warnings.
# -Wno-parentheses -- If it's valid C why bitch about it?
# -ggdb -- Debugging.
# -pg   -- Profiling.
# -O3   -- Implies:  -malign-loops=2 -malign-jumps=2 -malign-functions=2
#                    -malign-double -mwide-multiply
# -Werror -- Warnings prevent compilation.

LDFLAGS = -lm
# -lm       Math library (probably trig functions).
# -lcrypto  Crypto library for md5sum hash function.

MEMTOBJS  = memt.o                                     mem.o
VMTOBJS   = vmt.o                                 vm.o mem.o
ASMTOBJS  = asmt.o                          asm.o vm.o mem.o
OBJTOBJS  = objt.o                    obj.o            mem.o
SYSTOBJS  = syst.o              sys.o obj.o            mem.o
OSTOBJS   =  ost.o         os.o sys.o obj.o            mem.o
COMPTOBJS = compt.o comp.o os.o sys.o obj.o asm.o vm.o mem.o
WSCMTOBJS = wscmt.o comp.o      sys.o obj.o asm.o vm.o mem.o

WSCMOBJS  =  wscm.o comp.o os.o sys.o obj.o asm.o vm.o mem.o

wscm: $(WSCMOBJS)

mem.o:                                     mem.h globals.h debug.h

vm.o:                                 vm.h mem.h globals.h debug.h

asm.o:                          asm.h vm.h mem.h globals.h debug.h

obj.o:                    obj.h            mem.h globals.h debug.h

sys.o:              sys.h obj.h            mem.h globals.h debug.h

os.o:          os.h sys.h obj.h            mem.h globals.h debug.h

comp.o: comp.h os.h sys.h obj.h asm.h vm.h mem.h globals.h debug.h

wscm.o: comp.h os.h sys.h obj.h asm.h vm.h mem.h globals.h debug.h

build: globals.h debug.h mem.h vm.h asm.h obj.h sys.h os.h comp.h mem.c vm.c asm.c obj.c sys.c os.c comp.c wscm.c
	cat globals.h debug.h mem.h vm.h asm.h obj.h sys.h os.h comp.h mem.c vm.c asm.c obj.c sys.c os.c comp.c wscm.c > build.c ; gcc $(CFLAGS) $(LDFLAGS) build.c -o wscm

memt: $(MEMTOBJS)

vmt: $(VMTOBJS)

asmt: $(ASMTOBJS)

objt: $(OBJTOBJS)

syst: $(SYSTOBJS)

ost: $(OSTOBJS)

compt: $(COMPTOBJS)

wscmt: $(WSCMTOBJS)

memtest: memt
	./memt

vmtest: vmt
	./vmt

asmtest: asmt
	./asmt

objtest: objt
	./objt

systest: syst
	./syst

ostest: ost
	./ost

comptest: compt
	./compt

wscmtest: wscmt
	./wscmt

worldscheme: wscm
	./wscm scmt.scm

tests: memt vmt asmt objt syst ost compt wscmt

test: memtest vmtest asmtest objtest systest ostest comptest wscmtest worldscheme

linecount:
	wc Makefile globals.h debug.h mem.h vm.h asm.h obj.h sys.h os.h comp.h mem.c vm.c asm.c obj.c sys.c os.c comp.c wscm.c memt.c vmt.c asmt.c objt.c syst.c ost.c compt.c wscmt.c

clean:
	rm $(MEMTOBJS) $(VMTOBJS) $(ASMTOBJS) $(OBJTOBJS) $(SYSTOBJS) $(OSTOBJS) $(COMPTOBJS) $(WSCMTOBJS) $(WSCMOBJS) memt vmt asmt objt syst ost compt wscmt wscm build.c build.o


