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

MEMTOBJS  =  memt.o                                         mem.o
VMTOBJS   =   vmt.o                                    vm.o mem.o
OBJTOBJS  =  objt.o                        obj.o       vm.o mem.o
ASMTOBJS  =  asmt.o                        obj.o asm.o vm.o mem.o
SYSTOBJS  =  syst.o                  sys.o obj.o       vm.o mem.o
OSTOBJS   =   ost.o             os.o sys.o obj.o       vm.o mem.o
COMPTOBJS = compt.o      comp.o os.o sys.o obj.o asm.o vm.o mem.o
WSCMTOBJS = wscmt.o      comp.o os.o sys.o obj.o asm.o vm.o mem.o
CCTOBJS   =   cct.o cc.o        os.o sys.o obj.o       vm.o mem.o

WSCMOBJS  =  wscm.o cc.o comp.o os.o sys.o obj.o asm.o vm.o mem.o

wscm: $(WSCMOBJS)

mem.o:                                               mem.h globals.h debug.h

vm.o:                                           vm.h mem.h globals.h debug.h

obj.o:                              obj.h       vm.h mem.h globals.h debug.h

asm.o:                                    asm.h vm.h mem.h globals.h debug.h

sys.o:                        sys.h obj.h       vm.h mem.h globals.h debug.h sys.c

os.o:                    os.h sys.h obj.h       vm.h mem.h globals.h debug.h

comp.o:           comp.h os.h sys.h obj.h asm.h vm.h mem.h globals.h debug.h

wscm.o:           comp.h os.h sys.h obj.h asm.h vm.h mem.h globals.h debug.h

cc.o:   cc.h cc.h        os.h sys.h obj.h       vm.h mem.h globals.h debug.h

cct.o:  cc.c cc.h        os.h sys.h obj.h       vm.h mem.h globals.h debug.h

build: globals.h debug.h mem.h vm.h obj.h asm.h sys.h os.h cc.h comp.h mem.c vm.c obj.c asm.c sys.c os.c cc.c comp.c wscm.c
	cat globals.h debug.h mem.h vm.h obj.h asm.h sys.h os.h cc.h comp.h mem.c vm.c obj.c asm.c sys.c os.c cc.c comp.c wscm.c > build.c ; gcc $(CFLAGS) $(LDFLAGS) build.c -o wscm

memt: $(MEMTOBJS)

vmt: $(VMTOBJS)

objt: $(OBJTOBJS)

asmt: $(ASMTOBJS)

syst: $(SYSTOBJS)

ost: $(OSTOBJS)

cct: $(CCTOBJS)

compt: $(COMPTOBJS)

wscmt: $(WSCMTOBJS)

memtest: memt
	./memt

vmtest: vmt
	./vmt

objtest: objt
	./objt

asmtest: asmt
	./asmt

systest: syst
	./syst

ostest: ost
	./ost

cctest: cct
	sleep 1
	./cct

comptest: compt
	./compt

wscmtest: wscmt
	./wscmt

worldscheme: wscm
	./wscm scmt.scm

tests: memt vmt objt asmt syst ost cct compt wscmt

test: memtest vmtest objtest asmtest systest ostest cctest comptest wscmtest worldscheme

linecount:
	wc Makefile globals.h debug.h mem.h vm.h obj.h asm.h sys.h os.h cc.h comp.h mem.c vm.c obj.c asm.c sys.c os.c cc.c comp.c wscm.c memt.c vmt.c objt.c asmt.c syst.c ost.c cct.c compt.c wscmt.c

clean:
	rm $(MEMTOBJS) $(VMTOBJS) $(ASMTOBJS) $(OBJTOBJS) $(SYSTOBJS) $(OSTOBJS) $(COMPTOBJS) $(CCTOBJS) $(WSCMTOBJS) $(WSCMOBJS) memt vmt objt asmt syst ost cct compt wscmt wscm build.c build.o


