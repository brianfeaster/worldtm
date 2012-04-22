#ifndef _DEBUG_H
#define _DEBUG_H
/* Include this module and #define DEBUG as 1 and #DB_DESC as a string for debug messages.
*/
#include <stdio.h>

#define NOR "\e[0m"
#define BLK  "\e[0;30m"
#define RED  "\e[0;31m"
#define GRN  "\e[0;32m"
#define YEL  "\e[0;33m"
#define BLU  "\e[0;34m"
#define PUR  "\e[0;35m"
#define CYN  "\e[0;36m"
#define WHT  "\e[0;37m"
#define BBLK  "\e[0;1;30m"
#define BRED  "\e[0;1;31m"
#define BGRN  "\e[0;1;32m"
#define BYEL  "\e[0;1;33m"
#define BBLU  "\e[0;1;34m"
#define BPUR  "\e[0;1;35m"
#define BCYN  "\e[0;1;36m"
#define BWHT  "\e[0;1;37m"

static unsigned DBT=0;

#define DBBEG(FMT,...) if(DEBUG)fprintf(stderr,"\n"DB_DESC"%*s::%s"FMT,DBT++*2,"",__func__,##__VA_ARGS__)
#define    DB(FMT,...) if(DEBUG)fprintf(stderr,"\n"DB_DESC"%*s"    FMT,  DBT*2,"",         ##__VA_ARGS__)
#define DBEND(FMT,...) if(DEBUG)fprintf(stderr,"\n"DB_DESC"%*s--%s"FMT,DBT--*2,"",__func__,##__VA_ARGS__)

#define DBE if(DEBUG)

#endif
