#define DEBUG 0
#define DB_DESC "CC"
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "cc.h"
#include "os.h"
#include "sys.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"

void empty (void) {
	DBBEG();
	DBEND();
};

#undef DB_DESC
#undef DEBUG
