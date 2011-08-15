#ifndef _OS_H
#define _OS_H

#include "globals.h"

#define MAX_THREADS 1024
#define MAX_SIGNAL_VALUE 32
extern Int signalFlag;
extern Int caughtSignals[MAX_SIGNAL_VALUE];

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
void osSpawnSignalHandler(void);
void osCatchSignal (int sig);

#endif
