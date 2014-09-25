# Makefile for Wscheme.  It's getting complicated.

# Path of the source files and include files
CC         = clang
SRC_PATH   = src
H_DIR      = h
TEST_DIR   = test
H_PATH     = src/$(H_DIR)
SRC_DIRS   = $(wildcard $(SRC_PATH)/*)

# The individual modules proper that make up World Scheme (ignoring h and test dir): mem vm obj sys os asm comp wscm
MODULES = $(filter-out $(H_DIR) $(TEST_DIR), $(patsubst src/%,%,$(SRC_DIRS)))

CFLAGS = -I$(H_PATH) -Wall -march=native -Wconversion -Wno-trigraphs -Wno-format -Werror -g -O0
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
# -falign-labels=8  Align emitted machine code on 8 byte boundaries.

LDFLAGS = -lm
# -lm       Math library (probably trig functions).
# -lcrypto  Crypto library for md5sum hash function.

# Paths of all required files including h files.  Tests are located in their module directory.  (makefile variable)
VPATH = $(SRC_DIRS)

# Modules as .o files
OBJS := $(addsuffix .o, $(MODULES))

# Default target.  Start here!  Creates the World Scheme binary 'wscm'
wscm: $(OBJS)

# Creates and compile full binary from a monolithic source file via files src/h/*.h and src/module/*.c
build: $(H_PATH)/*.h $(addsuffix .c,$(MODULES))
	echo '/*' $^ > build.c '*/'
	cat $^ >> build.c
	$(CC) $(CFLAGS) $(LDFLAGS) build.c -o wscm
	rm build.c

# Module dependencies 
mem.o:                                     mem.h globals.h debug.h
vm.o:                                 vm.h mem.h globals.h debug.h op.h
obj.o:                          obj.h vm.h mem.h globals.h debug.h op.h
sys.o:                    sys.h obj.h vm.h mem.h globals.h debug.h op.h
os.o:                os.h sys.h obj.h vm.h mem.h globals.h debug.h op.h
asm.o:         asm.h            obj.h vm.h mem.h globals.h debug.h op.h
comp.o: comp.h asm.h os.h sys.h obj.h vm.h mem.h globals.h debug.h op.h
wscm.o: comp.h asm.h os.h sys.h obj.h vm.h mem.h globals.h debug.h op.h

# C test names one-to-one with MODULES
TESTS_C := $(addsuffix test, $(MODULES))

# Scheme test names
TESTS_SCM := $(addsuffix test, scm adt)

# Unit test object file dependencies
MEMTOBJS  :=                                    mem.o
VMTOBJS   :=                               vm.o mem.o
OBJTOBJS  :=                         obj.o vm.o mem.o
SYSTOBJS  :=                   sys.o obj.o vm.o mem.o
OSTOBJS   :=              os.o sys.o obj.o vm.o mem.o
ASMTOBJS  :=        asm.o            obj.o vm.o mem.o
COMPTOBJS := comp.o asm.o os.o sys.o obj.o vm.o mem.o
WSCMTOBJS := comp.o asm.o os.o sys.o obj.o vm.o mem.o


memt.o:  mem.o      test.h
vmt.o:   mem.o vm.o test.h
objt.o:             test.h
syst.o:             test.h
ost.o:              test.h
asmt.o:             test.h
compt.o:            test.h
wscmt.o:            test.h

# Create the unit test binaries
memt:  $(MEMTOBJS)
vmt:   $(VMTOBJS)
objt:  $(OBJTOBJS)
syst:  $(SYSTOBJS)
ost:   $(OSTOBJS)
asmt:  $(ASMTOBJS)
compt: $(COMPTOBJS)
wscmt: $(WSCMTOBJS)

# The TESTS_C rules.  These run the individual module unit tests.
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

# Run the scheme unit tests.  The target 'MODtest' becomes prerequisite 'MODt.scm'
$(TESTS_SCM): wscm
	./wscm lib/$(@:%test=%t).scm

testc: $(TESTS_C)

testscm:  $(TESTS_SCM)

test: testc testscm

linecount:
	wc Makefile $(H_PATH)/*.h $(foreach m, $(MODULES), $(SRC_PATH)/$(m)/$(m).c) $(foreach m, $(MODULES), $(SRC_PATH)/$(m)/$(m)t.c) lib/*.scm

links:
	ln -s src/h/globals.h  globals.h
	ln -s src/h/debug.h    debug.h
	ln -s src/h/mem.h      mem.h
	ln -s src/h/vm.h       vm.h
	ln -s src/h/obj.h      obj.h
	ln -s src/h/op.h       op.h
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
	ln -s lib/scm.scm       scm.scm
	ln -s src/mem/memt.c   memt.c
	ln -s src/vm/vmt.c     vmt.c
	ln -s src/obj/objt.c   objt.c
	ln -s src/sys/syst.c   syst.c
	ln -s src/os/ost.c     ost.c
	ln -s src/asm/asmt.c   asmt.c
	ln -s src/comp/compt.c compt.c
	ln -s src/wscm/wscmt.c wscmt.c

.PHONY : linksclean clean

linksclean:
	rm -f globals.h debug.h mem.h vm.h obj.h op.h sys.h os.h asm.h comp.h test.h mem.c vm.c obj.c sys.c os.c asm.c comp.c wscm.c scm.scm memt.c vmt.c objt.c syst.c ost.c asmt.c compt.c wscmt.c

clean: linksclean
	rm -f $(MEMTOBJS) $(VMTOBJS) $(ASMTOBJS) $(OBJTOBJS) $(OSTOBJS) $(SYSTOBJS) $(COMPTOBJS) $(WSCMTOBJS) $(WSCMOBJS) $(OBJS) memt vmt objt syst ost asmt compt wscmt wscm build.c build.o
