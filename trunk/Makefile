# Makefile for Wscheme.  It's simple.

CFLAGS = -D_GNU_SOURCE -march=pentium -Wall -Wno-format -Wno-trigraphs -ggdb
# -Wall            -- Many warnings.
# -Wno-format      -- Disable the annoying printf warnings.
# -Wno-parentheses -- If it's valid C why bitch about it?
# -ggdb -- Debugging.
# -pg   -- Profiling.
# -O3   -- Implies:  -malign-loops=2 -malign-jumps=2 -malign-functions=2
#                    -malign-double -mwide-multiply
# -Werror -- Warnings prevent compilation.

LDFLAGS = -lm -lcrypto
# -lm       Math library (probably trig functions).
# -lcrypto  Crypto library for md5sum hash function.

OBJS = scanner.o wscm.o comp.o obj.o asm.o vm.o mem.o

wscm: $(OBJS)

wscm.o: comp.h obj.h asm.h vm.h mem.h globals.h debug.h scanner.h

comp.o: comp.h obj.h asm.h vm.h mem.h globals.h debug.h

obj.o:         obj.h asm.h vm.h mem.h globals.h debug.h

asm.o:               asm.h vm.h mem.h globals.h debug.h

vm.o:                      vm.h mem.h globals.h debug.h

mem.o:                          mem.h globals.h debug.h

scanner.o:   obj.h mem.h globals.h

build: globals.h debug.h mem.h vm.h asm.h obj.h comp.h scanner.h mem.c vm.c obj.c comp.c wscm.c scanner.c
	cat globals.h debug.h mem.h vm.h asm.h obj.h comp.h scanner.h mem.c vm.c asm.c obj.c comp.c wscm.c scanner.c > f.c ; gcc $(CFLAGS) -O6 $(LDFLAGS) f.c -o wscm

linecount:
	wc Makefile globals.h debug.h mem.h vm.h asm.h obj.h comp.h scanner.h mem.c vm.c asm.c obj.c comp.c wscm.c scanner.c world.scm

upload-wscheme:
	scp Makefile globals.h debug.h mem.h vm.h asm.h obj.h comp.h scanner.h mem.c vm.c asm.c obj.c comp.c wscm.c scanner.c worlda@dv8.org:v5

upload-scm:
	scp world.scm window.scm scm.scm worlda@dv8.org:v5
clean:
	rm $(OBJS) wscm
