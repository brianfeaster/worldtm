# Makefile for Wscheme.  It's simple.

# Tell make where to find files
VPATH = src/h src/mem src/vm src/obj src/sys src/os src/asm src/comp src/wscm

CFLAGS = -I. -Isrc/h -Wall -ggdb -march=native -Wconversion -Wno-trigraphs -Wno-format -O3 -Werror
# -I                Include this directory for h file searches
# -Wall             Many warnings.
# -ggdb             Debugging
# -march=native     GCC can figure out the best optimization switches
# -Wno-trigraphs    Don't honor trigraphs
# -Wno-format       Disable the annoying printf warnings.
# -Wno-parentheses  If it's valid C why complain?
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

WSCMOBJS  =  wscm.o comp.o sys.o os.o obj.o asm.o vm.o mem.o

wscm: $(WSCMOBJS)

mem.o:                                               mem.h globals.h debug.h

vm.o:                                           vm.h mem.h globals.h debug.h

obj.o:                              obj.h       vm.h mem.h globals.h debug.h

sys.o:                   os.h sys.h obj.h       vm.h mem.h globals.h debug.h

os.o:                    os.h       obj.h       vm.h mem.h globals.h debug.h

asm.o:                              obj.h asm.h vm.h mem.h globals.h debug.h

comp.o:           comp.h os.h sys.h obj.h asm.h vm.h mem.h globals.h debug.h

wscm.o:           comp.h os.h sys.h obj.h asm.h vm.h mem.h globals.h debug.h scmlib.h

build: globals.h debug.h mem.h vm.h obj.h asm.h os.h sys.h comp.h mem.c vm.c obj.c asm.c os.c sys.c comp.c wscm.c | scmlib.h
	cat $^ > build.c
	gcc $(CFLAGS) $(LDFLAGS) build.c -o wscm
	rm build.c scmlib.h

scmlib.h: lib/scm.scm
	(echo -n \" ; sed 's/\\/\\\\/g;s/"/\\"/g;s/\\/\\\\/g;s/"/\\"/g;s/$$/\\n\\/g' < $< ; echo \") > scmlib.h

memt: $(MEMTOBJS)

vmt: $(VMTOBJS)

objt: $(OBJTOBJS)

syst: $(SYSTOBJS)

ost: $(OSTOBJS)

asmt: $(ASMTOBJS)

compt: $(COMPTOBJS)

wscmt: $(WSCMTOBJS)

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

asmtest: asmt
	./asmt

comptest: compt
	./compt

wscmtest: wscmt
	./wscmt

worldscheme: wscm
	./wscm lib/scmt.scm

tests: memt vmt objt syst ost asmt compt wscmt

test: memtest vmtest objtest systest ostest asmtest comptest wscmtest worldscheme

linecount:
	wc Makefile globals.h debug.h mem.h vm.h obj.h sys.h os.h asm.h comp.h mem.c vm.c obj.c sys.c os.c asm.c comp.c wscm.c memt.c vmt.c objt.c asmt.c syst.c ost.c compt.c wscmt.c

links:
	ln -s src/h/globals.h  globals.h
	ln -s src/h/debug.h    debug.h
	ln -s src/h/mem.h      mem.h
	ln -s src/h/vm.h       vm.h
	ln -s src/h/obj.h      obj.h
	ln -s src/h/sys.h      sys.h
	ln -s src/h/os.h       os.h
	ln -s src/h/asm.h      asm.h
	ln -s src/h/comp.h     comp.h
	ln -s src/h/test.h     test.h
	ln -s src/mem/mem.c   mem.c
	ln -s src/vm/vm.c     vm.c
	ln -s src/obj/obj.c   obj.c
	ln -s src/sys/sys.c   sys.c
	ln -s src/os/os.c     os.c
	ln -s src/asm/asm.c   asm.c
	ln -s src/comp/comp.c comp.c
	ln -s src/wscm/wscm.c wscm.c
	ln -s lib/scm.scm      scm.scm

linksclean:
	rm -f globals.h debug.h mem.h vm.h obj.h sys.h os.h asm.h comp.h test.h mem.c vm.c obj.c sys.c os.c asm.c comp.c wscm.c scm.scm


clean: linksclean
	rm $(MEMTOBJS) $(VMTOBJS) $(ASMTOBJS) $(OBJTOBJS) $(OSTOBJS) $(SYSTOBJS) $(COMPTOBJS) $(WSCMTOBJS) $(WSCMOBJS) scmlib.h memt vmt objt syst ost asmt compt wscmt wscm build.c build.o
