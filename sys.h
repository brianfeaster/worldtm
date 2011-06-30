#ifndef _SYS_H
#define _SYS_H

#include <stdio.h>
#include <math.h>   /* sqrt() */
#include <unistd.h>
#include <limits.h>
#include <fcntl.h>  /* fcntl() */
#include <stdlib.h> /* random() */
#include <unistd.h> /* write() */
#include <string.h> /* strlen() */
#include <sys/time.h> /* gettimeofday() */
#include <errno.h> /* for errno */
#include <sys/types.h>  /* for socket() */
#include <sys/socket.h>
#include <netinet/in.h> /* for htons() */
#include <netdb.h>      /* for gethostbyname() */
#include <arpa/inet.h>  /* for inet_ntoa() */
#include <time.h>  /* for time() */
#include <sys/poll.h>

#include <signal.h>     /* for signal() */
#include <sys/ioctl.h>  /* for ioctl() and struct winsize*/

#include "scanner.h"
#include "obj.h"
#include "comp.h"

Int wscmWriteR (Obj a, Num islist, FILE *stream, Int max);
Num wscmWrite (Obj a, FILE *stream);
Num wscmWriteMax (Obj a, FILE *stream, Int max);
void wscmDisplayR (Obj a, Num islist, FILE *stream);
void wscmDisplay (Obj a, FILE *stream);
void sysDumpEnv (Obj env);
void wscmTGEFind (void);
void wscmInsertThread (Obj t, Obj q);
void wscmRemoveThread (Obj t);
void wscmMoveToQueue (Obj thread, Obj queue, Obj state);
void wscmNewThread (void);
void wscmUnRun (void);
void wscmRun (void);
void wscmSleepThread (void);
void wscmScheduleSleeping (void);
void wscmScheduleBlocked (void);
void wscmSchedule (void);
void sysError (void);
void sysUnthread (void);
void sysOpenFile (void);
void sysOpenSemaphore (void);
void sysSchedule (void);
void catchSignal (int sig);
void sysSignal (void);
void wscmDefine (char* sym);
void wscmInitialize (void);
void sysDumpCallStackCode (void);
void sysDebugger (void);

#endif
