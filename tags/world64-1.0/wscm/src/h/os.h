#ifndef _OS_H
#define _OS_H

#include "globals.h"

#define MAX_THREADS 1024
#define MAX_SIGNAL_VALUE 32
extern Int signalFlag;
extern Int caughtSignals[MAX_SIGNAL_VALUE];

/* Rootset objects */
extern Obj rdebug, rblocked, rthreads, rsleeping, rrunning, rready;

/* Useful
*/
void osException (Obj str);

/* IO
*/
void osRecv (void);
void osSend (void);

/* Scheduling
*/
void osOpenSemaphore (void);
Obj osThreadId (Obj t);
Int osIsQueueEmpty (Obj q);
Obj osThreadCount (void);
void osNewThread (void);
void osScheduler (void);
void osUnthread (void);
void osMoveToQueue (Obj thread, Obj queue, Obj state);
void osUnRun (void);
void osSleepThread (void);
void osUnblockSemaphoreBlocked (Obj sem, Num all);
void osSpawnSignalHandler (void);
void osCatchSignal (int sig);

/* IO
*/
void osRecvBlock (void);

/* Initialization
*/
void osInitialize(Func exceptionHandler);

#endif
