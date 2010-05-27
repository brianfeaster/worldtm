# Makefile for Wscheme.  It's simple.

CFLAGS = -Wall -ggdb -Wno-format -Wno-trigraphs
#CFLAGS = -D_GNU_SOURCE -march=pentium -Wall -Wno-format -Wno-trigraphs -ggdb
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

MEMTOBJS  = memt.o         mem.o
OBJTOBJS  = objt.o         mem.o obj.o vm.o asm.o
VMTOBJS   = vmt.o          mem.o       vm.o
ASMTOBJS  = asmt.o         mem.o obj.o vm.o asm.o
WSCMTOBJS = wscmt.o wscm.o mem.o obj.o vm.o asm.o comp.o scanner.o
WSCMOBJS  =         wscm.o mem.o obj.o vm.o asm.o comp.o scanner.o

wscm: $(WSCMOBJS)

wscm.o: comp.h obj.h asm.h vm.h mem.h globals.h debug.h scanner.h

comp.o: comp.h obj.h asm.h vm.h mem.h globals.h debug.h

obj.o:         obj.h asm.h vm.h mem.h globals.h debug.h

asm.o:               asm.h vm.h mem.h globals.h debug.h

vm.o:                      vm.h mem.h globals.h debug.h

mem.o:                          mem.h globals.h debug.h

scanner.o:     obj.h            mem.h globals.h         scanner.h

build: globals.h debug.h mem.h vm.h asm.h obj.h comp.h scanner.h mem.c vm.c obj.c comp.c wscm.c scanner.c
	cat globals.h debug.h mem.h vm.h asm.h obj.h comp.h scanner.h mem.c vm.c asm.c obj.c comp.c wscm.c scanner.c > f.c ; gcc $(CFLAGS) -O6 $(LDFLAGS) f.c -o wscm

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

test: memtest objtest vmtest asmtest wscmtest

linecount:
	wc Makefile asm.c asm.h asmt.c comp.c comp.h debug.h globals.h mem.c mem.h memt.c obj.c obj.h objt.c scanner.c scanner.h vm.c vm.h vmt.c wscm.c wscmt.c

upload-wscheme:
	scp Makefile globals.h debug.h mem.h vm.h asm.h obj.h comp.h scanner.h mem.c vm.c asm.c obj.c comp.c wscm.c scanner.c worlda@dv8.org:v5

upload-scm:
	scp world.scm window.scm scm.scm worlda@dv8.org:v5

clean:
	rm $(WSCMOBJS) wscm wscmt asmt vmt objt memt
