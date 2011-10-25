# Makefile for Wscheme.  It's simple.

CFLAGS = -Wall -ggdb -march=native -Wconversion -Wno-trigraphs -Wno-format
# -march=native     GCC can figure out the best optimization switches
# -Wall             Many warnings.
# -Wno-format       Disable the annoying printf warnings.
# -Wno-parentheses  If it's valid C why complain?
# -ggdb             Debugging
# -pg               Profiling.
# -O3               -malign-loops=2 -malign-jumps=2 -malign-functions=2 -malign-double -mwide-multiply
# -Werror           Warnings prevent compilation.

LDFLAGS = -lm
# -lm       Math library (probably trig functions).
# -lcrypto  Crypto library for md5sum hash function.

MEMTOBJS  =  memt.o                                         mem.o
VMTOBJS   =   vmt.o                                    vm.o mem.o
OBJTOBJS  =  objt.o                        obj.o       vm.o mem.o
SYSTOBJS  =  syst.o                  sys.o obj.o       vm.o mem.o
OSTOBJS   =   ost.o             os.o sys.o obj.o       vm.o mem.o
ASMTOBJS  =  asmt.o                        obj.o asm.o vm.o mem.o
COMPTOBJS = compt.o      comp.o os.o sys.o obj.o asm.o vm.o mem.o
WSCMTOBJS = wscmt.o      comp.o os.o sys.o obj.o asm.o vm.o mem.o

CCTOBJS   =   cct.o cc.o        os.o sys.o obj.o       vm.o mem.o

WSCMOBJS  =  wscm.o cc.o comp.o sys.o os.o obj.o asm.o vm.o mem.o

wscm: $(WSCMOBJS)

mem.o:                                               mem.h globals.h debug.h

vm.o:                                           vm.h mem.h globals.h debug.h

obj.o:                              obj.h       vm.h mem.h globals.h debug.h

sys.o:                   os.h sys.h obj.h       vm.h mem.h globals.h debug.h

os.o:                    os.h       obj.h       vm.h mem.h globals.h debug.h

asm.o:                              obj.h asm.h vm.h mem.h globals.h debug.h

comp.o:           comp.h os.h sys.h obj.h asm.h vm.h mem.h globals.h debug.h

wscm.o:           comp.h os.h sys.h obj.h asm.h vm.h mem.h globals.h debug.h

cc.o:   cc.h cc.h        os.h sys.h obj.h       vm.h mem.h globals.h debug.h

build: globals.h debug.h mem.h vm.h obj.h asm.h os.h sys.h cc.h comp.h mem.c vm.c obj.c asm.c os.c sys.c cc.c comp.c wscm.c
	cat globals.h debug.h mem.h vm.h obj.h asm.h os.h sys.h cc.h comp.h mem.c vm.c obj.c asm.c os.c sys.c cc.c comp.c wscm.c > build.c ; gcc $(CFLAGS) $(LDFLAGS) build.c -o wscm
	rm build.c

memt: $(MEMTOBJS)

vmt: $(VMTOBJS)

objt: $(OBJTOBJS)

syst: $(SYSTOBJS)

ost: $(OSTOBJS)

asmt: $(ASMTOBJS)

compt: $(COMPTOBJS)

wscmt: $(WSCMTOBJS)

cct: $(CCTOBJS)

memtest: memt
	./memt

vmtest: vmt
	./vmt

objtest: objt
	./objt

systest: syst
	./syst

ostest: ost
	./ost

cctest: cct
	sleep 1
	./cct

asmtest: asmt
	./asmt

comptest: compt
	./compt

wscmtest: wscmt
	./wscmt

worldscheme: wscm
	./wscm scmt.scm

tests: memt vmt objt syst ost asmt compt wscmt cct

test: memtest vmtest objtest systest ostest cctest asmtest comptest wscmtest worldscheme

linecount:
	wc Makefile globals.h debug.h mem.h vm.h obj.h sys.h os.h cc.h asm.h comp.h mem.c vm.c obj.c sys.c os.c cc.c asm.c comp.c wscm.c memt.c vmt.c objt.c asmt.c syst.c ost.c compt.c wscmt.c cct.c 

clean:
	rm $(MEMTOBJS) $(VMTOBJS) $(ASMTOBJS) $(OBJTOBJS) $(OSTOBJS) $(SYSTOBJS) $(COMPTOBJS) $(CCTOBJS) $(WSCMTOBJS) $(WSCMOBJS) memt vmt objt syst ost cct asmt compt wscmt wscm build.c build.o


