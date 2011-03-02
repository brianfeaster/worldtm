# Makefile for Wscheme.  It's simple.

CFLAGS = -Wall -ggdb -Wno-format -Wno-trigraphs -march=native -Wconversion -O3
#CFLAGS = -D_GNU_SOURCE -march=pentium -Wall -Wno-format -Wno-trigraphs -ggdb -O3
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

MEMTOBJS  = memt.o        mem.o
OBJTOBJS  = objt.o        mem.o obj.o vm.o asm.o
VMTOBJS   = vmt.o         mem.o       vm.o
ASMTOBJS  = asmt.o        mem.o obj.o vm.o asm.o
WSCMTOBJS = wscmt.o sys.o mem.o obj.o vm.o asm.o comp.o scanner.o
WSCMOBJS  = wscm.o  sys.o mem.o obj.o vm.o asm.o comp.o scanner.o

wscm: $(WSCMOBJS)

wscm.o: sys.h comp.h obj.h asm.h vm.h mem.h globals.h debug.h scanner.h

comp.o: comp.h obj.h asm.h vm.h mem.h globals.h debug.h

obj.o:         obj.h asm.h vm.h mem.h globals.h debug.h

asm.o:               asm.h vm.h mem.h globals.h debug.h

vm.o:                      vm.h mem.h globals.h debug.h

mem.o:                          mem.h globals.h debug.h

scanner.o:     obj.h            mem.h globals.h         scanner.h

build: globals.h debug.h mem.h vm.h asm.h obj.h comp.h sys.h scanner.h mem.c vm.c asm.c obj.c comp.c sys.c wscm.c scanner.c
	cat globals.h debug.h mem.h vm.h asm.h obj.h comp.h sys.h scanner.h mem.c vm.c asm.c obj.c comp.c sys.c wscm.c scanner.c > build.c ; gcc $(CFLAGS) $(LDFLAGS) build.c -o wscm

memt: $(MEMTOBJS)

objt: $(OBJTOBJS)

vmt: $(VMTOBJS)

asmt: $(ASMTOBJS)

wscmt: $(WSCMTOBJS)

memtest: memt
	./memt

objtest: objt
	./objt

vmtest: vmt
	./vmt

asmtest: asmt
	./asmt

wscmtest: wscmt
	./wscmt

worldscheme: wscm
	./wscm scmt.scm

tests: memt objt vmt asmt wscmt

test: memtest objtest vmtest asmtest wscmtest worldscheme

linecount:
	wc Makefile asm.c asm.h asmt.c comp.c comp.h debug.h globals.h mem.c mem.h memt.c obj.c obj.h objt.c scanner.c scanner.h vm.c vm.h vmt.c sys.h sys.c wscm.c wscmt.c

clean:
	rm $(WSCMOBJS) $(WSCMTOBJS) $(MEMTOBJS) $(OBJTOBJS) $(VMTOBJS) $(ASMTOBJS) wscm wscmt asmt vmt objt memt build.o build.c


