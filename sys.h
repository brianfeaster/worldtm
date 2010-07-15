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

Int wscmWriteR (Obj a, long islist, FILE *stream, Int max);
Int wscmWrite (Obj a, FILE *stream);
Int wscmWriteMax (Obj a, FILE *stream, Int max);
void wscmDisplay (Obj a, long islist, int fd);
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
void wscmError (void);
void sysUnthread (void);
void sysOpen (void);
void sysSchedule (void);
void catchSignal (int sig);
void sysSignal (void);
void wscmDefine (char* sym);
void wscmInitialize (void);

#endif
