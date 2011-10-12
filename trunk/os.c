#define DEBUG_ALL 0
#include "debug.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include "os.h"
#include "sys.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"
/*
 Scheduling
*/



/*******************************************************************************
 Scheduling
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "OS_SCHED"

/* Semaphores are just immediate numbers.
*/
void osOpenSemaphore (void) {
	r0 = memNewSemaphore();
	memVectorSet(r0, 0, (Obj)*(Int*)vmPop()); /* Initialize the semaphore's counter */
}

void osNewThreadDescriptor (void) {
	objNewVector(3);
}

Obj osThreadDescStack (Obj d) { return memVectorObject(d, 0); }
Obj osThreadDescId    (Obj d) { return memVectorObject(d, 1); }
Obj osThreadDescState (Obj d) { return memVectorObject(d, 2); }

Obj osThreadDescriptor (Obj t) { return car(t); }

Obj osThreadStack (Obj t) { return osThreadDescStack (osThreadDescriptor(t)); }
Obj osThreadId    (Obj t) { return osThreadDescId    (osThreadDescriptor(t)); }
Obj osThreadState (Obj t) { return osThreadDescState (osThreadDescriptor(t)); }

Int osIsQueueEmpty (Obj q) { return cdr(q)==q; }

/* Symbol 'thread' is a vector of thread objects indexed by their thread ID (tid).
   The first object is the thread count as an immediate integer which implies
	TID=0 is illegal.*/
Obj osThreadCount (void) {
	return memVectorObject(rthreads, 0);
}

void osRemoveThread (Obj t) {
 Obj p, n;
	DBBEG();
	if (t!=rready && t!=rsleeping && t!=rblocked) {
		p = objDoublyLinkedListPrev(t); /* Previous doubly linked list entry. */
		n = objDoublyLinkedListNext(t); /* Next doubly linked list entry. */
		memVectorSet(p, 1, n); /* Set previous' next to next. */
		memVectorSet(n, 2, p); /* Set next's previous to previous. */
		/* Keep running pointer on the ready queue. */
		if (t==rrunning) rrunning=p;
	} else {
		fprintf (stderr, "WARNING: %s: Attempting to remove thread queue head.\r\n", __func__);
	}
	DBEND();
}

/* Creates a new thread object which is inserted into the running queue.

	Requires: r0 code block
	Mutates: r0 r1 r2
	Returns: r1 thread descriptor
            r0 thread id
*/
void osNewThread (void) {
 Num tid;
	DBBEG();

	/* Find next available thread id.  Thread table's first entry
	   is the count so index range is 1 to MAX_THREADS inclusive. */
	for (tid=1; memVectorObject(rthreads, tid) != null; ++tid)
		if (MAX_THREADS <= tid) {
			fprintf (stderr, "WARNING: osNewThread: Too many rthreads.");
			r0 = false;
			goto done;
		}

	DB("tid="NUM, tid);

	/* Create thread's stack (in an un-running state). */
	r1=r0; /* Code block */
	r2 = memNewStack();
	memStackPush(r2, renv); /* Initial environment. */
	memStackPush(r2, 0);   /* Initial ip. */
	memStackPush(r2, r1);  /* Initial code. */
	memStackPush(r2, 0);   /* Initial retenv. */
	memStackPush(r2, 0);   /* Initial retip. */
	memStackPush(r2, 0);   /* Initial retcode. */
	memStackPush(r2, 0);   /* Initial r7. */
	memStackPush(r2, 0);   /* Initial r6. */
	memStackPush(r2, 0);   /* Initial r5. */
	memStackPush(r2, 0);   /* Initial r4. */
	memStackPush(r2, 0);   /* Initial r3. */
	memStackPush(r2, 0);   /* Initial r2. */
	memStackPush(r2, 0);   /* Initial r1. */
	memStackPush(r2, 0);   /* Initial r0. */

	objNewInt((Int)tid); r1 = r0; /* ID as an integer object */
	/* Create thread descriptor #( #<stack> tid 'state) and add to thread table vector */
	osNewThreadDescriptor();
	memVectorSet(r0, 0, r2);      /* Set stack */
	memVectorSet(r0, 1, r1); /* Set id */
	memVectorSet(r0, 2, sready);  /* Set 'ready' state */

	/* Add thread descriptor to thread table vector and increment count */
	memVectorSet(rthreads, tid, r0);
	memVectorSet(rthreads, 0, osThreadCount()+1);

	/* Create new doubly linked list queue element for this thread descriptor and
	   insert into the ready queue before the current running thread */
	r2 = r1; /* Thread ID */
	r1 = r0; /* Thread descriptor */
	objNewDoublyLinkedListNode();
	memVectorSet(r0, 0, r1);
	objDoublyLinkedListInsert (rrunning, r0); /* Insert new thread before current thread */

	r0 = r2;
	/* Return thread ID in r0 and thread descriptor in r1. */
done:
	DBEND("  =>  tid:"NUM, tid);
} /* osNewThread */


/* Deal with sleeping threads that need to wake up.  If nothing in ready queue
   nor blocked queue then wait for topmost thread to wakeup. */
void osScheduleSleeping (void) {
 Obj sleepingThreadDescriptor;
 Int wakeupTime;
	DBBEG();
	sleepingThreadDescriptor = osThreadDescriptor(objDoublyLinkedListNext(rsleeping));

	/* Only sleeping threads exist so wait for next one to wakeup.
	   Next thread's wakeup time on top of its stack. */
	wakeupTime = *(s64*)memStackObject(osThreadDescStack(sleepingThreadDescriptor),0) - sysTime();
	if (wakeupTime > 0
	    && osIsQueueEmpty(rready)
	    && osIsQueueEmpty(rblocked)) {
		DB("Waiting "INT"ms", wakeupTime);
		/* Disable VM's interrupt timer as it'll interrupt our
		   sleeping.  It will be reactivated by the VM module upon return.

		   BF TODO does this even matter?  The only way this scheduler is
		   called is after the timer interrupts the VM.  Oh but what if the
		   code block ends or unthread is called?  */
		ualarm(0,0);
		usleep((useconds_t)wakeupTime*1000);

		/* Consider wakeup time again.  An interrupt might have prematurely interrupted usleep() */
		wakeupTime = *(s64*)memStackObject(osThreadDescStack(sleepingThreadDescriptor),0) - sysTime();
		DB("Remaining "INT"ms", wakeupTime);
	}

	/* If next sleeping thread is ready to be woken up, insert into ready queue. */
	if (wakeupTime <= 0) {
		DB("Waking next sleeping thread");
		/* Pop wake-time from stack. */
		memStackPop(osThreadDescStack(sleepingThreadDescriptor));
		osMoveToQueue(objDoublyLinkedListNext(rsleeping), rready, sready);
	}

	DBEND();
}


/* Move all blocked threads that can and have read a chracter from their
   descriptor to the ready queue.
   Have yet to block threads on a remote connection...blocks the
   entire process on an (open-socket "remote" port). FIXED.

	use r1 r5
*/
void osScheduleBlocked (void) {
 Num timedOut;
	DBBEG("  blocked:"NUM"  sleeping:"NUM,
	   objDoublyLinkedListLength(rblocked)-1,
	   objDoublyLinkedListLength(rsleeping)-1);
	/* For each doubly linked list node of blocked threads... */
	r5=objDoublyLinkedListNext(rblocked);
	while (r5!=rblocked) {
	DB("considering blocked thread "NUM, *(Num*)osThreadId(r5));
		/* Consider status in the descriptor of this thread. */
		r1 = osThreadState(r5);

		if (r1 == sreadblocked) {
			DB("dealing with a read blocked thread");
			r1 = memStackObject(osThreadStack(r5), 1l); /* port */
			r2 = memStackObject(osThreadStack(r5), 2l); /* timeout */
			r3 = memStackObject(osThreadStack(r5), 3l); /* buffer */
			r4 = memStackObject(osThreadStack(r5), 4l); /* count */
			sysRecv();
			timedOut = (r2!=false) && (*(Int*)r2 <= sysTime());
			if (r0 != false || timedOut) {
				/* If timed out but partial read, return the partial string */
				if (r0==false && timedOut && 0<(Num)r4) {
					objNewString(NULL, (Num)r4);
   				memcpy(r0, r3, (Num)r4);
				}
				/* Set thread's return value (r0 register top of stack) with
			   	newly-read string or #f if it has timed out. */
				memStackSet(osThreadStack(r5), 0, r0);
				r1=objDoublyLinkedListNext(r5);
				osMoveToQueue(r5, rready, sready);
				r5=r1;
			/* Store back registers into thread keeping it blocked. */
			} else {
				memStackSet(osThreadStack(r5), 1l, r1);
				memStackSet(osThreadStack(r5), 2l, r2);
				memStackSet(osThreadStack(r5), 3l, r3);
				memStackSet(osThreadStack(r5), 4l, r4);
				r5=objDoublyLinkedListNext(r5);
			}
		}
		else if (r1 == swriteblocked) {
			DB("dealing with a write blocked thread");
			r1 = memStackObject(osThreadStack(r5), 1);
			r2 = memStackObject(osThreadStack(r5), 2);
			r3 = memStackObject(osThreadStack(r5), 3);
			sysSend();
			if (r0 != false) {
				DB(" unblocking thread");
				/* Set thread's return value (r0 register top of stack) with
			   	sent string. */
				memStackSet(osThreadStack(r5), 0, r2);
				r1=objDoublyLinkedListNext(r5);
				osMoveToQueue(r5, rready, sready);
				r5=r1;
			/* Store back registers into thread since sysSend more than likely
			   changed them and keep this thread blocked. */
			} else {
				DB(" not unblocking thread");
				memStackSet(osThreadStack(r5), 1, r1);
				memStackSet(osThreadStack(r5), 2, r2);
				memStackSet(osThreadStack(r5), 3, r3);
				r5=objDoublyLinkedListNext(r5);
			}
		} else if (r1 == sopenblocked) {
			DB("dealing with a open-blocked thread");
			/* Snag port from sleeping thread (r1). */
			r1 = memStackObject(osThreadStack(r5),1);
			/* If a connection is made on the port and set to a non-accepting
			   state, set the threads return value (r0) to the port and move the
			   thread to the ready queue. */
			if (memVectorObject(r1, 3) == saccepting) {
				DB(" dealing with a new incomming stream connection thread");
				vmPush(r5); /* Since the following clobbers r5. */
				sysAcceptLocalStream(); r1=r0;
				r5=vmPop();
				if (memVectorObject(r1, 3) != saccepting) {
					memStackSet(osThreadStack(r5), 0, r1);
					r1=objDoublyLinkedListNext(r5);
					osMoveToQueue(r5, rready, sready);
					r5=r1;
				} else {
					r5=objDoublyLinkedListNext(r5);
				}
			} else if (memVectorObject(r1, 3) == sconnecting) {
				DB(" dealing with a new remote stream connection thread");
				sysAcceptRemoteStream();
				if (r1==eof || memVectorObject(r1, 3) != sconnecting) {
					memStackSet(osThreadStack(r5), 0, r1);
					r1=objDoublyLinkedListNext(r5);
					osMoveToQueue(r5, rready, sready);
					r5=r1;
				} else {
					r5=objDoublyLinkedListNext(r5);
				}
			} else if (memVectorObject(r1, 3) == sclosed) {
				memStackSet(osThreadStack(r5), 0, eof);
				r1=objDoublyLinkedListNext(r5);
				osMoveToQueue(r5, rready, sready);
				r5=r1;
			} else { /* Must be in a connecting state. */
				r5=objDoublyLinkedListNext(r5);
			}
		} else if (r1 == ssemaphore) {
			/* Skip semaphore blocked threads. */
			r5=objDoublyLinkedListNext(r5);
		} else {
			fprintf (stderr, "ERROR: osScheduleBlocked: unknown thread state");
			r5=objDoublyLinkedListNext(r5);
		}
	}
	DBEND();
}

/* Make running thread ready for the VM.  Pop all the saved registers. */
void osRun (void) {
	DBBEG(" tid="INT, *(Num*)osThreadId(rrunning));
	if (osThreadState(rrunning) != sready) {
		fprintf (stderr, "WARNING: osRun: Should be 'ready' thread but is ");
		sysDisplay(osThreadState(rrunning), stderr);
	} else {
		/* Get stack from descriptor. */
		rstack = osThreadStack(rrunning);
		r0 = vmPop();
		r1 = vmPop();
		r2 = vmPop();
		r3 = vmPop();
		r4 = vmPop();
		r5 = vmPop();
		r6 = vmPop();
		r7 = vmPop();
		rretcode=vmPop();
		rretip=vmPop();
		rretenv=vmPop();
		rcode=vmPop();
		rip=vmPop();
		renv=vmPop();
		memVectorSet(osThreadDescriptor(rrunning), 2, srunning);
	}
	DBEND(" => ");
	DBE sysWrite(osThreadDescriptor(rrunning), stderr);
}


void osScheduler (void) {
	DBBEG();
//	DBE osDebugDumpThreadInfo();
	if (!osIsQueueEmpty(rsleeping)) osScheduleSleeping();
	if (!osIsQueueEmpty(rblocked)) osScheduleBlocked();
	while (osIsQueueEmpty(rready)) {
		/* A signal might have occured during the scheduler
		   requiring a signal handler thread to be spawned */
		if (signalFlag) { osSpawnSignalHandler(); }

		/* No more threads so shutdown. */
		if (osIsQueueEmpty(rsleeping) && osIsQueueEmpty(rblocked)) {
			DB("No more threads.  So long and thanks for all the fish.");
			exit(0);
		}
		if (!osIsQueueEmpty(rsleeping)) osScheduleSleeping();
		if (!osIsQueueEmpty(rblocked))  osScheduleBlocked();
		/* Since sleeping threads have been dealt with and there are no ready
		   theads but we need to wait for blocked threads, sleep a bit and try
		   again later. */
		if (osIsQueueEmpty(rready)  && !osIsQueueEmpty(rblocked)) {
			ualarm(0,0);       /* Disable scheduler's interrupt timer. */
			usleep(50*1000);  /* Sleep 50 milliseconds and try again. */
		}
	}

	/* Switch to another thread.  Round robin scheme.  Just go to next thread. */
	rrunning = objDoublyLinkedListNext(rrunning);

	/* Can this happen? */
	if (rrunning==rready) rrunning=objDoublyLinkedListNext(rready);
	if (rrunning==rready) fprintf (stderr, "ERROR: deal with this!");

	osRun();

	DBEND("  =>"NUM, *(Num*)osThreadId(rrunning));
} /* osScheduler */


void osUnthread (void) {
	DBBEG("  thread ID "NUM, *(Num*)osThreadId(rrunning));
	/* Remove from thread vector table. */
	memVectorSet(rthreads, *(Num*)osThreadId(rrunning), null);
	/* Decrement thread count. */
	memVectorSet(rthreads, 0, osThreadCount()-1); /* TODO Race condition? */
	osRemoveThread(rrunning);
	osScheduler();
	DBEND();
}


/* Move thread from its current queue to specified queue.
	State one of: sready srunning ssleeping ssemaphore sopenblocked sreadblocked swriteblocked
*/
void osMoveToQueue (Obj thread, Obj queue, Obj state) {
	DBBEG();
	/* To keep round robin scheduler happy we need to trick it by moving
	   the 'running' object back a thread. */
	memVectorSet(osThreadDescriptor(thread), 2, state); /* Set thread's new state. */
	osRemoveThread(thread);
	/* If inserting into ready queue, insert behind the running thread,
	   otherwise insert at end of passed queue.  The ternary expression
	   performs this logic. */
	objDoublyLinkedListInsert (queue==rready?rrunning:queue, thread);
	DBEND();
}

/* Make the running thread (the stack) ready to be stashed away.
*/
void osUnRun (void) {
 Obj threadDescriptor;
	DBBEG();

	/* Verify thread is in runningstate then change to ready state. */
	threadDescriptor = osThreadDescriptor(rrunning);
	assert(osThreadDescState(threadDescriptor) == srunning);
	memVectorSet(threadDescriptor, 2, sready);

	vmPush(renv);
	vmPush(rip);
	vmPush(rcode);
	vmPush(rretenv);
	vmPush(rretip);
	vmPush(rretcode);
	vmPush(r7);
	vmPush(r6);
	vmPush(r5);
	vmPush(r4);
	vmPush(r3);
	vmPush(r2);
	vmPush(r1);
	vmPush(r0);

	DBEND();
}


/* Put thread on sleep list.  Stack contains the millisecond count to sleep.
*/
void osSleepThread (void) {
 s64 wakeupTime;
	DBBEG();
	r0 = vmPop(); /* The call to sleep returns the argument passed to it. */
	osUnRun();
	wakeupTime = sysTime() + *(Int*)r0;
	DB("wakeupTime %lldms = %lld-%lld", wakeupTime-sysTime(), wakeupTime, sysTime());
	/* Wakeup time (u64) goes on top of stack. */
	objNewInt(wakeupTime);
	vmPush(r0);

	/* Put this thread in order of wakup time in the sleeping list.  */
	r3=objDoublyLinkedListNext(rsleeping);
	while (r3 != rsleeping) {
		if (*(s64*)memStackObject(osThreadStack(r3),0) > wakeupTime)
			break;
		r3 = objDoublyLinkedListNext(r3);
	}
	osMoveToQueue(rrunning, r3, ssleeping); /* Insert thread into list. */

	/* Go setup another thread to start running. */
	osScheduler();
	DBEND();
}

/* Called by syscallSemaphoreUp, not the scheduler.  all==1 means the semaphore is
   now closed and all threads blocked on this should unblock. */
void osUnblockSemaphoreBlocked (Obj sem, Num all) {
 Obj semaphore, next;
 Int found=0;
	DBBEG();
	r4=objDoublyLinkedListNext(rblocked); /* For each thread r4 in blocked queue... */
	while (!found && r4!=rblocked) {
		next=objDoublyLinkedListNext(r4);
		/* Check thread status in its descriptor. */
		if (osThreadState(r4) == ssemaphore) {
			/* Look at thread's r1 register stored on its stack. */
			semaphore = memStackObject(osThreadStack(r4),1);
			DB("considering blocked thread with sem:");
			DBE sysWrite(sem, stderr);
			DBE sysWrite(semaphore, stderr);
			if (semaphore == sem) {
				DB("unblocking thread tid:"NUM, *(Num*)osThreadId(r4));
				/* Set thread's return value (r0 register which is found at the top of the thread's stack)
				   to #t if another thread down'ed the semaphore and #f if close-semaphore called. */
				memStackSet(osThreadStack(r4), 0, all?false:true);
				osMoveToQueue(r4, rready, sready);
				if (!all) found=1;
			}
		}
		r4=next;
	}
	if (!all && !found) {
		fprintf (stderr, "ERROR: Couldn't find thread blocked on semaphore:");
		sysWrite(sem, stderr);
		exit (-1);
	}
	DBEND();
}

Int signalFlag=0;
Int caughtSignals[MAX_SIGNAL_VALUE]={0};

/* Spawn a new signal handler thread.  Function exist
   in global signal-handler vector.
*/
void osSpawnSignalHandler(void) {
 Num i;
	DBBEG();
	DBE for (i=0; i<MAX_SIGNAL_VALUE; ++i) { fprintf (stderr, " "NUM, caughtSignals[i]); }
	signalFlag = 0;
	for (i=0; i<MAX_SIGNAL_VALUE; ++i) {
		if (caughtSignals[i]) {
			DB("caughtSignals["NUM"]="NUM, i, caughtSignals[i]);
			caughtSignals[i] = 0;
			vmPush(r0); /* Save state */
			vmPush(r1);
			vmPush(r2);
			vmPush(r3);
				r1 = signalhandlers;  sysTGEFind(); r0=car(r0); /* Consider vector of signalhandlers. */
				r3 = memVectorObject(r0, i); /* Consider the closure */
				r0 = car(r3);
				osNewThread();
				/* Set the new thread's r0 register to the closure object as
				   this is expected state during a procedure application */
				memStackSet(osThreadDescStack(r1), 0, r3);
			r3=vmPop();
			r2=vmPop();
			r1=vmPop();
			r0=vmPop();
		}
	}
	DBEND();
}

void osDebugDumpThreadInfo (void) {
 Obj node;
	DBBEG();
	fprintf (stderr, "\n-- THREAD INFO --------");
	for(node = objDoublyLinkedListNext(rblocked); node != rblocked; node=objDoublyLinkedListNext(node)) {
		fprintf (stderr, "\nblocked "NUM, *(Num*)osThreadId(node));
	}
	for(node = objDoublyLinkedListNext(rsleeping); node != rsleeping; node=objDoublyLinkedListNext(node)) {
		fprintf (stderr, "\nsleeping "NUM, *(Num*)osThreadId(node));
	}
	for(node = objDoublyLinkedListNext(rready); node != rready; node=objDoublyLinkedListNext(node)) {
		fprintf (stderr, "\nready "NUM" %s", *(Num*)osThreadId(node), rrunning==rready?"running":"");
	}
	fprintf (stderr, "\n-- thread info --------");
	DBEND();
}


#undef DB_DESC
#undef DEBUG
