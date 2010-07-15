#define DEBUG 0
#define DB_MODULE "WSCM "
#include "debug.h"

#include <stdio.h>
#include "sys.h"

/* Weird hack that implements a read-eval-print loop as a syscall.  The strange
   thing about it is that the entire process is started from this syscall
   rather than starting everything with a call to the virtual machine vmRun.
   It uses the QUIT opcode which will probably be phased out.  Carefull
   attention must be made when calling wscmSchedule so that this thread is
   guaranteed to be the current running thread.  STDIN must be in blocking
   mode.
*/
void wscmCReadEvalPrintLoop (void) {
	/* Read Eval Print Loop.  stdin must be blocking to work.
	*/
	yyrestart(0);     /* Tell scanner to use stdin as input. */
	wscmNewThread();  /* Create a new thread. */
	wscmSchedule();   /* Prepare it for the VM. */
	/* Blocking thread that reads input and evalautes in the global
	   environment. */
	while (r0 != eof) {
		env = tge;
		DB("MAIN Parsing======================");
		//DBE wscmDumpEnv(env);
		DBE wscmWrite(stack, stderr), write (2, "\n", 1);
		DBE wscmWrite(ready, stderr), write (2, "\n", 1);
		DBE wscmWrite(sleeping, stderr), write (2, "\n", 1);
		DBE wscmWrite(blocked, stderr);
		write (1, "\nC>", 3);
		yyparse();/* Expr read into r0. */
		DB("MAIN Compiling==================== <= ");
		DBE wscmWrite(r0, stderr);
		compCompile();   /* Expr in r0 compiled into VM runable code in r0. */
		code=r0; ip=0;
		DB("MAIN Running======================");
		DBE wscmWrite(code, stderr);DBE write (2, "\n", 1);
		vmDebugDumpCode(code, stderr);
		vmRun();
		//wscmDisplay(stack, 0, 2);
		//DBE memDebugDumpHeapStructures ();
		DB("MAIN Output======================");
		wscmDisplay(r0, 0, 1);
	}
	sysUnthread();
	printf ("WEL loop done\n");
}

/* Uses legacy C-based parsing code to parse a string.  It's then compiled
   into a thread and the virtual machine started up.
*/
void wscmStringReadEvalPrintLoop (void) {
	DB("::wscmSringReadEvalPrintLoop  calling vmRun()");

	yy_scan_string ((Str)
"(let ~ ((FILE:SCM.SCM (open \"scm.scm\")))\
    (if (eof-object? (eval (read FILE:SCM.SCM)))\
        (send \"\r\nbye.\r\n\" stdout)\
        (~ FILE:SCM.SCM)))");
	yyparse();
	compCompile();

	//DB("  calling vmDebugDump()\n");
	//vmDebugDump(); /* THIS IS CRASHING */

	wscmNewThread();
	wscmSchedule();
	DB("  calling vmRun()");
	vmRun();
	DB("  --wscmSringReadEvalPrintLoop  calling vmRun()");
}

/* Bind wscheme's command line arguments to the vector 'argv
*/
void wscmBindArgs (int argc, char *argv[]) {
 Int i=0;
	objNewVector(argc); r1=r0;
	for (i=0; i<argc; i++) {
		objNewString((u8*)argv[i], strlen(argv[i]));
		memVectorSet(r1, i, r0);
	}
	r0=r1; wscmDefine ("argv"); 
}

int main (int argc, char *argv[]) {
	wscmInitialize();
	setbuf(stdout, NULL);
	signal(SIGPIPE, SIG_IGN);
	fcntl (0, F_SETFL, fcntl(0, F_GETFL)|O_NONBLOCK);
	srandom(time(NULL));
	wscmBindArgs(argc, argv);

	/* Three ways of firing up a repl. */

	/* REPL in C. */
	//wscmCReadEvalPrintLoop(); return 0;

	/* REPL as inlined scheme. */
	wscmStringReadEvalPrintLoop(); return 0;

	/* Bind symbol 'in and assign the stdin port or the filename passed as arg
	   1 to wscm. */
	if (argc==2) {
		/* Create port object, push the argument, set arg count to 1 then
		   make the syscall. */
		objNewSymbol ((Str)argv[1], strlen(argv[1]));
		push(r0);  r1=(Obj)1;  sysOpen();
		/* Assign port to existing binding. */
		if ((r3=r0) != false) {
			objNewSymbol ((Str)"in", 2);  r1=r0;  wscmTGEFind();
			memVectorSet(r0, 0, r3);
		}
	}

	objNewSymbol ((Str)"repl2", 5);r1=r0;
	wscmTGEFind(); r0=caar(r0);

	wscmWrite(r0, stderr);
	wscmNewThread();
	wscmSchedule();
	vmRun();

	return 0;
}

#undef DB_MODULE
