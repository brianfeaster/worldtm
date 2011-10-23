#define DEBUG_ALL 0
#include "debug.h"
#include <stdio.h>
#include <unistd.h>
#include <termios.h> /* tcgetattr */
#include <stdlib.h>
#include <ctype.h> /* isxdigit() */
#include <limits.h>
#include <fcntl.h>  /* fcntl() */
#include <string.h> /* strlen() */
#include <sys/types.h>  /* socket() */
#include <netdb.h>      /* gethostbyname() */
#include <errno.h> /* for errno */
#include <sys/time.h> /* gettimeofday() */
#include <sys/poll.h> /* poll() */
#include <sys/ioctl.h>  /* for ioctl() and struct winsize*/
#include <arpa/inet.h>  /* sai inet_ntoa() */
#include <assert.h>
#include "sys.h"
#include "obj.h"
#include "vm.h"
#include "mem.h"
/*
 Useful
 Scanning_parsing
 Environment
 Serializers_internal
 Ports
 Initialization
*/

#if 0
#define rstack     rf /* Global stack */
#endif


/*******************************************************************************
 Useful
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "SYS_SYS"

/* Return number of milliseconds since epoch as a 64bit unsigned.
*/
s64 sysTime (void) {
 struct timeval tv;
	gettimeofday(&tv, NULL);
	return (s64)tv.tv_sec*1000 + (s64)tv.tv_usec/1000;
}

/* Create new closure in r0 which is (r1=<code> . rc=<environment>)
*/
void sysNewClosure1Env (void) {
   r0=memNewVector(TCLOSURE, 2);
	memVectorSet(r0, 0, r1);
	memVectorSet(r0, 1, renv); /* rc */
}

/* Pop stack operand arguments into a new list.
    r1 <= operand count
    r2  = temporary
    r0  => new list
*/
void sysStackToList (void) {
 Num count = (Num)r1;
	assert(count <= 256); /* Don't expect more than 256 args */
	r0 = null;
	while (count--) { r1=vmPop();  r2=r0;  objCons12(); }
}

/* Push list objects to stack, last to first.
     r0 <= list
  rstack => new elements added
*/
void sysListToStack (void) {
	if (null == r0) return;
	vmPush(car(r0));
	r0 = cdr(r0);
	sysListToStack();
}

void sysDumpCallStackCode (void) {
 Num i, isValid;
 Obj o; // l=NULL;

	sysDumpEnv(rc);
	sysDumpEnv(r9);
	for (i=0; i<memStackLength(rstack); ++i) {
		o = memStackObject(rstack, i);
		isValid = memIsObjectValid(o);
		printf ("Stack "INT" "HEX016" "OBJ STR NL, i, isValid?memObjectDescriptor(o):0, o, (isValid&&memObjectType(o)==TCODE)?"code":"");
	}

#if 0
	for (i=0; i<memStackLength(rstack); ++i) {
		o = memStackObject(rstack, i);
		if (memIsObjectValid(o)) {
			if (memObjectType(o)==TCODE) {
				printf (NL "Stack "INT" "HEX016" "OBJ NL, i, memObjectDescriptor(o), o);
				sysWrite(memVectorObject(o, 2), stdout);
				if (l!=NULL) sysDumpEnv(l);
				else printf (NL);
			}
		}
		l = o;
	}
#endif
}

void syscallDebugger (void) {
 struct winsize win;
 int done=0, cmd, fl;
 struct termios tios, tios_orig;

	fflush(stdout);

	/* Force cooked mode. */
	tcgetattr(1, &tios_orig); /* Backup termios */
	tcgetattr(1, &tios);
	tios.c_iflag |= (ICRNL | IXON | IXOFF | IXANY);
	tios.c_oflag |= OPOST;
	tios.c_lflag |= (ECHO | ICANON | ISIG | IEXTEN);
	tcsetattr(1, TCSANOW, &tios);

	/* Force blocking input */
	fl=fcntl(0, F_GETFL, 0);
	fcntl (0, F_SETFL, fl&~O_NONBLOCK);

	ioctl(1, TIOCGWINSZ, &win); printf ("\e[0m\e[%dH\n", win.ws_row); /* Move cursor to bottom of screen */
	while (!done) {
		printf ("\n\e[1m-------------------------------");
		printf ("\nc    vmDebugDumpCode(code, stderr)");
		printf ("\nC a  vmDebugDumpCode(a, stderr)");
		printf ("\nu    sysDumpCallStackCode()");
		printf ("\ne    sysDumpEnv(env)");
		printf ("\ns    sysWrite(stack, stderr)");
		printf ("\ny    sysWrite(symbols, stderr)");
		printf ("\nh    memDebugDumpHeapHeaders(stderr)"); 
		printf ("\n3    memDebugDumpYoungHeap(stderr)"); 
		printf ("\nw a  sysWrite(a1, stderr)");
		printf ("\nR    return");
		printf ("\nX    crash");
		printf ("\nQ    exit(-1)");
		printf ("\n-------------------------------\e[0m");
		printf ("\nwscmdbg>");
		while (strchr("\r\n \t", cmd=getchar())); /* Skip whitespace */
		if (cmd=='c') vmDebugDumpCode(rcode, stderr);
	   if (cmd=='C') {
			Obj arg;
			scanf("%lx", &arg);
			vmDebugDumpCode(arg, stderr);
		}
		if (cmd=='u') sysDumpCallStackCode();
		if (cmd=='e') sysDumpEnv(renv);
		if (cmd=='s') sysWrite(rstack, stderr);
		if (cmd=='y') sysWrite(rsymbols, stderr);
	   if (cmd=='h') memDebugDumpHeapHeaders(stderr);
	   if (cmd=='3') memDebugDumpYoungHeap (stderr);
	   if (cmd=='w') {
			Obj arg;
			scanf("%lx", &arg);
			if (memPointerString(arg)) printf ((char*)memPointerString(arg));
			else sysWrite(arg, stderr);
		}
		if (cmd=='R') done=1;
		if (cmd=='X') *(Int*)0=0;
		if (cmd=='Q') exit(-1);
	}

	/* Restore terminal and IO */
	tcsetattr(1, TCSANOW, &tios_orig);
	fcntl (0, F_SETFL, fl);
	r0 = true;
}


#undef DB_DESC
#undef DEBUG



/*******************************************************************************
 Scanning_parsing
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "SYS_SCAN"

/* Non-final states.  These must be in numerical order starting at 0 as they
   represent the index of a table of vectors. */
#define STARTSTATE 0x00
#define A STARTSTATE // Initial state
#define B 0x01 // ;comment
#define C 0x02 // #
#define D 0x03 // symbol
#define E 0x04 // .
#define F 0x05 // + -
#define G 0x06 // integer number
#define H 0x07 /* "str\ */
#define I 0x08 // "str
#define J 0x09 // float number
#define K 0x0a // binary number
#define L 0x0b // number or symbol
#define N 0x0c /* #\ */
#define O 0x0d // hexidecimal number
#define P 0x0e // , or ,@
#define Q MAXSTATE // octal number
#define MAXSTATE 0x0f

/* Final states */
#define a SOPENPAREN
#define c SSTRING
#define d SVECTOR
#define e SQUOTE
#define f SQUASIQUOTE
#define g SUNQUOTESPLICING
#define h SCLOSEPAREN
#define i STRUE
#define j SFALSE
#define k SCHARACTER
#define l SEOF

/* Final state with character push-back */
#define s SOCT
#define t SUNQUOTE
#define u SBINARY
#define v SSYMBOL
#define w SHEX
#define x SDOT
#define y SREAL
#define z SINTEGER

Num transition (Num ch, Num state) {
 static Num table[] = {
	/* A Initial lexical state.  From here we begin the state machine walk.  This
	 * has the most transitions obviously.  White space is handled here by just
	 * returning to this state.  A few single character tokens ) ] \} , ' ` are
	 * handled by the scanning logic and are just states back to A for no reason.
	 */
	// \a\b\t\n\v\f\r
	   A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   A,D,I,C,D,D,D,e,a,h,D,F,P,F,E,D,G,G,G,G,G,G,G,G,G,G,D,B,D,D,D,D,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,a,D,h,D,D,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   f,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,a,D,h,D,A,
	//��������������������������������
	   A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   A,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,l,

	/* B Comment state.  */
	// \a\b\t\n\v\f\r
	   B,B,B,B,B,B,B,B,B,B,A,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,
	//��������������������������������
	   B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,l,

	/* C State to handle # tokens.  */
	// \a\b\t\n\v\f\r
	   v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   v,B,D,D,D,D,D,D,d,v,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,v,D,D,D,D,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   D,D,K,D,G,D,j,D,D,D,D,D,D,D,D,Q,D,D,D,D,i,D,D,D,O,D,D,d,N,v,D,D,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   D,D,K,D,G,D,j,D,D,D,D,D,D,D,D,Q,D,D,D,D,i,D,D,D,O,D,D,d,D,v,D,v,
	//��������������������������������
	   v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   v,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,v,

	/* D Symbol state */
	// \a\b\t\n\v\f\r
	   v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   v,D,v,D,D,D,D,v,v,v,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,v,D,D,D,D,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,v,D,v,D,D,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   v,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,v,D,v,D,v,
	//��������������������������������
	   v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   v,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,v,

	/* E Dot or real initial state */
	// \a\b\t\n\v\f\r
	   x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,J,J,J,J,J,J,J,J,J,J,x,x,x,x,x,x,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
	//��������������������������������
	   x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,

	/* F Initial Possible Number or Symbol -/+ state.  */
	// \a\b\t\n\v\f\r
	   v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   v,D,D,D,D,D,D,D,v,v,D,D,D,D,J,D,L,L,L,L,L,L,L,L,L,L,D,D,D,D,D,D,  // Changed G to L
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,v,D,v,D,D,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,v,D,v,D,v,
	//��������������������������������
	   v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   v,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,v,

	/* G Base 10 number state.  */
	// \a\b\t\n\v\f\r
	   z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   z,z,z,z,z,z,z,z,z,z,z,z,z,z,J,z,G,G,G,G,G,G,G,G,G,G,z,z,z,z,z,z,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,
	//��������������������������������
	   z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,

	/* H Escaped character string state.  */
	// \a\b\t\n\v\f\r
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	//��������������������������������
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,l,

	/* I String state.  */
	// \a\b\t\n\v\f\r
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   I,I,c,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,H,I,I,I,
	// ` a b c d e f g h i c k l m n o p q r s t u v w x y z { | } ~
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	//��������������������������������
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,l,

	/* J Real state.  */
	// \a\b\t\n\v\f\r
	   y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,J,J,J,J,J,J,J,J,J,J,y,y,y,y,y,y,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,
	//��������������������������������
	   y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,

	/* K #b binary number state */
	// \a\b\t\n\v\f\r
	   u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,K,K,w,w,w,w,w,w,w,w,u,u,u,u,u,u,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   u,w,w,w,w,w,w,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   u,w,w,w,w,w,w,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,
	//��������������������������������
	   u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,

	/* L number or symbol state.  */
	// \a\b\t\n\v\f\r
	   z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   z,D,z,D,D,D,D,z,z,z,D,D,D,D,D,D,L,L,L,L,L,L,L,L,L,L,D,z,D,D,D,D,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,z,D,z,D,D,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   z,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,z,D,z,D,z,
	//��������������������������������
	   z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,z,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   z,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,z,

	/* N #\ state */
	// \a\b\t\n\v\f\r
	   k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,
	//��������������������������������
	   k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,k,l,

	/* O #x hex number state */
	// \a\b\t\n\v\f\r
	   w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,O,O,O,O,O,O,O,O,O,O,w,w,w,w,w,w,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   w,O,O,O,O,O,O,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   w,O,O,O,O,O,O,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,
	//��������������������������������
	   w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,

	/* P , state */
	// \a\b\t\n\v\f\r
	   t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   g,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,
	//��������������������������������
	   t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,

	/* Q #o oct number state */
	// \a\b\t\n\v\f\r
	   s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,
	//   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ?
	   s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,Q,Q,Q,Q,Q,Q,Q,Q,s,s,s,s,s,s,s,s,
	// @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _
	   s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,
	//��������������������������������
	   s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s
 };

	DBBEG ("ch:"NUM"  state:"NUM, ch, state);

	if (MAXSTATE<state || 257<ch) {
		fprintf (stderr, "\nERROR: transition: illegal char or state ch:"HEX2" state:"HEX2"", ch, state);
		exit(0);
	}

	DBEND ("Next state"NUM, table[state*257+ch]);

	return table[state*257+ch];
} /* transition */

#undef A
#undef B
#undef C
#undef D
#undef E
#undef F
#undef G
#undef H
#undef I
#undef J
#undef K
#undef L
#undef N
#undef O
#undef P
#undef Q
#undef a
#undef c
#undef d
#undef e
#undef f
#undef g
#undef h
#undef i
#undef j
#undef k
#undef l
#undef s
#undef t
#undef u
#undef v
#undef w
#undef x
#undef y
#undef z


#define STRING     0
#define DISCRIPTOR 1
#define EOFCHAR    256
Chr buff[65536], *bp, *bpLast;
int fd=0;
Num mode=DISCRIPTOR, ungetchar=EOFCHAR;

/* Reset file descriptor
 */
void yyrestart(int fd0) {
	mode=DISCRIPTOR;
	fd=fd0;
	ungetchar=EOFCHAR;
}

/* Reset buffer
 */
void yy_scan_string(Str buff0) {
 Num len = strlen((char*)buff0);
	mode=STRING;
	if (len>65535) {
		printf("WARNING: yy_scan_string: string length > 65535");
		len=65535;
	}
	memcpy(buff, buff0, len);
	bpLast = (bp=buff)+len;
	ungetchar=EOFCHAR;
}

void yy_scan_bytes(Str buff0, Num len) {
	mode=STRING;
	if (len>65535) {
		printf("WARNING: yy_scan_bytes: len > 65535");
		len=65535;
	}
	memcpy(buff, buff0, len);
	bpLast = (bp=buff)+len;
	ungetchar=EOFCHAR;
}

/* -1 returned implies eof or null
 */
Num readChar (void) {
 Num ch=0, ret;
	DBBEG();
	if (EOFCHAR != ungetchar) {
		DB("using ungetchar "HEX02" "CHR, ungetchar, ungetchar);
		ch = ungetchar;
		ungetchar = EOFCHAR;
		ret = ch;
	} else if (mode==STRING) {
		if (bp == bpLast) ret = EOFCHAR;
		else {
			ch = *bp++;
			ret = ch;
		}
		DB("from string "HEX02" "CHR, ret, ret);
	} else if (0 >= read(fd, &ch, 1))
		ret = EOFCHAR;
	else
		ret = ch;
	DBEND();
	return ret;
}

void unreadChar (Num ch) {
	ungetchar = ch;
}


/* Return value of an ASCII hex digit
*/
int hexToNum (Str sp) { return 9*(*sp>>6) + (0xf & *sp); }


/* Replace escape chars with actual char in string squishing the string in
   the process.  Used after a call to yylex.  Also tweaks yyleng.
*/
Num parseString (Str str) {
 Str p=str++; /* Start p right after the first quote. */
	yyleng = 0;
	while (*str!='"') {
		if (*str == '\\')
			switch(*++str) {
				case '0': *p++ = '\0'; break; /*   0 */
				case 'a': *p++ = '\a'; break; /*   7 */
				case 'b': *p++ = '\b'; break; /*   8 */
				case 't': *p++ = '\t'; break; /*   9 */
				case 'n': *p++ = '\n'; break; /*  10 */
				case 'v': *p++ = '\v'; break; /*  11 */
				case 'f': *p++ = '\f'; break; /*  12 */
				case 'r': *p++ = '\r'; break; /*  13 */
				case 'e': *p++ = '\e'; break; /*  27 */
				case '"': *p++ = '"';  break; /*  34 */
				case '\\':*p++ = '\\'; break; /*  92 */
				case 'X':                     /*  88 */
				case 'x':                     /* 120 */
					/* Parse a \xff hex digit */
					if (isxdigit(*(str+1)) && isxdigit(*(str+2))) {
						*p++ = (Chr)(16*hexToNum(str+1) + hexToNum(str+2));
						str+=2;
					} else {
						*p++ = *str;
					}
					break;
				case 'c': *p++ = 0x9b; break; /* 155 */
				default: *p++ = *str;
			}
		else
			*p++ = *str;
		yyleng++;
		str++;
	}
	return yyleng;
}


/* Kernel based blocking parse function.
*/
Chr yytext[65536];
Str yyp;
Num yyleng;

Num yylex (void) {
 Num ch, state;

	/* Initial state and scanned-token buffer. */
	state=0;
 	yyp=yytext;

	/* Continue state transtions over non-final states. */
	while (!(state & FINALSTATE)) {
		*yyp++ = (Chr)(ch = readChar());
		state = transition(ch, state);
		/* Re-initialize yytext whenever we re-enter initial state. */
		if (state==STARTSTATE) yyp=yytext;
	}
	/* Handle character push-back states. */
	if (state & PUSHBACK) {
		unreadChar(ch);
		yyp--;
	}

	/* Calculate scanned token length. */
	yyleng = (Num)(yyp-yytext);

	/* Delimit scanned string with NULL for C string compatibility. */
	yytext[yyleng]=0;

	return state;
}


/* C implementation.  Look in wscm.c for the VM assembly version. */
void parse (long islist) {
 Num state;

	state = yylex();
	DB ("parse   state:"HEX"  yytext:["STR"]", state, yytext);
   switch (state) {
      case SEOF :        r0=eof; return;
      case SCLOSEPAREN : r0=(null); return;
      case SFALSE :      r0=(false); break;
      case STRUE :       r0=(true); break;
      case SOPENPAREN :  parse(1); break;
		/* Silently ignore anything scanned after the 2nd obj in a malformed
		   dotted list IE: (a . b ignore these things) => (a . b) */
		case SDOT :        parse(1); r0=car(r0); return;
      case SINTEGER:     objNewInt(strtol((char *)yytext+(*yytext=='#'?2:0), 0, 10)); break;
      case SREAL   :     objNewReal(strtof((char *)yytext, 0)); break;
      case SHEX :        objNewInt(strtol((char *)yytext+2,0,16)); break;
      case SOCT :        objNewInt(strtol((char *)yytext+2,0,8)); break;
      case SBINARY :     objNewInt(strtol((char *)yytext+2,0, 2)); break;
      case SSTRING :     parseString(yytext);
		                       if (yyleng) objNewString((Str)yytext, yyleng);
		                       else r0=nullstr; break;
      case SCHARACTER :  r0=memVectorObject(scharacters, yytext[2]); break;
      case SSYMBOL :     objNewSymbol((Str)yytext, yyleng); break;
      case SVECTOR :     parse(1); objListToVector(); break;
      case SQUOTE :
			parse(0);
			r1=r0; r2=null; objCons12();
			r1=squote; r2=r0; objCons12();
			break;
      case SUNQUOTE :
			parse(0);
			r1=r0; r2=null; objCons12();
			r1=sunquote; r2=r0; objCons12();
			break;
      case SUNQUOTESPLICING :
			parse(0);
			r1=r0; r2=null; objCons12();
			r1=sunquotesplicing; r2=r0; objCons12();
			break;
      case SQUASIQUOTE :
			parse(0);
			r1=r0; r2=null; objCons12();
			r1=squasiquote; r2=r0; objCons12();
			break;
		default :
			fprintf (stderr, "ERROR: parse: unknown state="NUM, state);
   }
	/* If constructing a list, cons this object with the rest of the list.  */
   if (islist) {
		vmPush(r0);
      parse(1);
		r1=vmPop();
		if (r0==eof) return;
		r2=r0;  objCons12();
   }
}


void yyparse (void) {
	DBBEG();
	parse (0);
	DBEND();
}

#undef DB_DESC
#undef DEBUG



/*******************************************************************************
 Environment
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "SYS_ENV"

/* Look for the symbol in r1 in the global environment.  Return in r0 the
   binding (value . symbol) or null if not found.
	 r1 <= Symbol to find
	  r0 => (symbol . value) pair
*/
void sysTGEFind (void) {
	DBBEG(" r1 <= ");
	DBE sysWrite(r1, stderr);
	/* Scan over the list of (value . symbol) pairs. */
	for (r0=cdr(rtge); r0!=null; r0=cdr(r0)) {
		if (cdar(r0) == r1) {
			r0 = car(r0);
			break;
		}
	}
	DBEND(" r0 => ");
	DBE sysWrite(r0, stderr);
}

/* Given a symbol in r1, bind it to a new location if it doesn't already
   exist in the global environment.  The 'binding' is returned in r0. */
void sysTGEBind (void) {
	DBBEG("");
	/* Look for symbol r1 in TGE. Binding returned in r0. */
	sysTGEFind();
	if (null == r0) {
		/* Create new empty binding. */
		r2=r1;  r1=null;  objCons12();  /* (() . sym) */
		/* Insert binding after the first in the global environment list. */
		r1=r0;  r2=cdr(rtge);  objCons12();   /* ((val . sym) . ...) */
		memVectorSet(rtge, 1, r0);
		r0 = r1; /* return binding. */
		DB("ENV    Added binding ");
		DBE sysWrite(cdr(r0), stderr);
	}
	DBEND();
}

/* Looks for first occurance of symbol in r1 in a chain of pseudo environments.
   Returns env link depth and local environment binding vector offset in the
   2nd and 1st byte fields respectively.  The global environment is not searched.
   r1 <= symbol to search for
   r0 = temp
*/
Num sysEnvFind (void) {
 Num ret, depth=0, offset;
	DBBEG(" r1 <= ");
	DBE sysWrite(r1, stderr);
	vmPush(renv);
	while (renv != rtge) {
		DB("  Examining env: ");
		DBE sysWrite(cdr(renv), stderr);
		/* Start at 2 since local environment values start at offset 2. */
		offset=2;
		for (r0=cdr(renv); objIsPair(r0); r0=cdr(r0)) {
			DB("  Looking at:");
			DBE sysWrite(car(r0), stderr);
			if (car(r0) == r1) { /* Pseudo env ( ^ a b c), so just check car */
				ret = (depth<<8) | offset;
				DB("  Found in local environment");
				goto ret;
			}
			offset++;
		}
		renv = memVectorObject(renv, 0);
		depth++;
	}
	ret = 0;
 ret:
	renv = vmPop();
	DBEND("  => "INT, ret);
	return ret;
}

/* Resolve the symbol value in current envionrment chain.
   env: of the form #(parent-env (z y x) 1 2 3)
        where tge is 
   r1 : symbol in question
   Clobbers: r2
   Return: r0
*/
void sysEnvGet (void) {
 Num offset;
	DBBEG();
	vmPush(renv); /* Save environment. */
	while (renv != rtge) {
		/* Start at 2 since local environment values start at offset 2. */
		offset=2;
		r0=memVectorObject(renv, 1);
		while (objIsPair(r0)) {
			if (car(r0) == r1) {
				r0 = memVectorObject(renv, offset); /* Found in a local env. */
				goto ret;
			}
			offset++;
			r0=cdr(r0);
		}
		renv = memVectorObject(renv, 0);
	}
	sysTGEFind();

	if (r0==null) {
		/* Symbol nonexistant.  Display error and return symbol. */
		printf ("ERROR: Unbound symbol '");
		sysWrite(r1, stdout);
		printf ("'\n");
		r0 = r1;
	} else {
		r0 = car(r0);
	}
 ret:
	renv=vmPop(); /* Restore environment. */
	DBEND();
}

/* Bind symbol, created from 'sym', in TGE and assign object in r0 to the location.
	Mutates: r1 r2
	Returns: r0
*/
void sysDefine (char* sym) {
	DBBEG();
	r1 = r0;
	objNewSymbol((Str)sym, strlen(sym)); r2=r0;
	objCons12();
	/* Insert binding into TGE list:  (TGE (val . sym) . ...) */
	r1=r0;  r2=cdr(rtge);  objCons12();
	memVectorSet(rtge, 1, r0);
	DBEND();
	DBE sysWrite (r1, stderr);
}


/* Bind 'symbol in global environment and assign a syscall object
   created from 'function.  This should be called via the macro
   in sys.h
*/
void sysDefineSyscallString (Func function, Str functionStr, char* symbol) {
	DBBEG("  "OBJ" "STR, function, symbol);
	/* Create binding: (val . sym) */
	objNewSyscall(function); r1=r0;
	objNewSymbol((Str)symbol, strlen(symbol)); r2=r0;
	objCons12();
	/* Insert binding into TGE list:  (TGE (newVal . newSym) ...) */
	r1=r0;
	r2=cdr(rtge);
	objCons12();
	memVectorSet(rtge, 1, r0);

	/* Register internal pointer with memory module. */
	memPointerRegisterString(function, functionStr);

	DBEND("  =>  ");
	DBE sysWrite (r1, stderr);
}


#undef DB_DESC
#undef DEBUG



/*******************************************************************************
 Serializers_internal
 Internal object serializers and output function.
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "SYS_SERIALIZER "

/* Given a signed integer and base, create a new string object in r0
   representing the number's external signed representation.
   IE: sysSerializeInteger(-256 16) => "-100"

	In C on this machine, the sign of the base doesn't matter but I don't
	expect the desired base of a number to be negative, as interesting as
	negative base numbers are.
*/
void sysSerializeInteger (Int theint, Num base) {
 u8 buff[64], /* Big enough to contain signed 64bit binary ascii number. */
    *ptr=buff+64;
 Num  signBit, dividend, glyph;
	DBBEG();
	assert(base<32);
	// Special cases which the general algorithm doesn't handle gracefully.
	if (theint == LLONG_MIN) {
		if (10==base) objNewString((u8*)"-9223372036854775808", 20);
		else if (16==base) objNewString((u8*)"-8000000000000000",17);
		else if (2==base) objNewString((u8*)"-1000000000000000000000000000000000000000000000000000000000000000",65);
		else {
			fprintf(stderr, "Can't convert LLONG_MIN to base "NUM, base);
			exit(-1);
		}
	} else if (theint == 0) objNewString((u8*)"0", 1);
	else {
		if (theint<0) {    /* Consider sign and normalize dividen into a positive number. */
			signBit=1;
			dividend = -(Num)theint;
		} else {
			signBit=0;
			dividend = (Num)theint;
		}

		while (dividend) { /* Construct the string in reverse. */
			glyph = dividend % base; /* Both dividend and divisor positive here */
			*--ptr = (u8)(glyph + (glyph<10 ? '0' : 'a'-10));
			dividend /= base;
		}

		if (signBit) *--ptr = '-'; /* Here's the sign. */
		objNewString(ptr, (Num)(buff+64-ptr));
	}
	DBEND();
}

Int sysWriteR (Obj a, Num islist, FILE *stream, Int max) {
 Num i;
 Int count=0;

	if (NULL==stream) stream=stderr;

	if (max <= 0) count += fprintf (stream, "...");
	else if (!memIsObjectValid(a)) count += fprintf(stream, "#<"HEX">", a);
	else switch (memObjectType(a)) {
		case TFALSE:
			count += fprintf(stream, "#f");
			break;
		case TTRUE:
			count += fprintf(stream, "#t");
			break;
		case TNULL:
			count += fprintf(stream, "()");
			break;
		case TNULLVEC:
			count += fprintf(stream, "#()");
			break;
		case TNULLSTR:
			count += fprintf(stream, "\"\"");
			break;
		case TEOF:
			count += fprintf(stream, "#eof");
			break;
		case TCHAR:
			 switch (*(char*)a) {
				case '\a'   : count += fprintf(stream, "\\bell");  break;
				case '\033' : count += fprintf(stream, "\\esc");  break;
				case '\233' : count += fprintf(stream, "\\csi");  break; /* CSI character.  ESC + 8th bit*/
				case '\n'   : count += fprintf(stream, "\\newline");  break;
				case '\r'   : count += fprintf(stream, "\\return");  break;
				case '\t'   : count += fprintf(stream, "\\tab");  break;
				case '\v'   : count += fprintf(stream, "\\vtab");  break;
				case ' '   : count += fprintf(stream, "\\space");  break;
				default     : count += fprintf(stream, "#\\%c", *(char*)a);
			}
			break;
		case TSTRING:
			count += fprintf(stream, "\"");
			for (i=0; i<memObjectLength(a); i++)
			 switch (((char*)a)[i]) {
				case '\\'   : count += fprintf(stream, "\\\\"); break;
				case '\"'   : count += fprintf(stream, "\\\""); break;
				case '\a'   : count += fprintf(stream, "\\a");  break;
				case '\033' : count += fprintf(stream, "\\e");  break;
				case '\233' : count += fprintf(stream, "\\c");  break; /* CSI character.  ESC + 8th bit*/
				case '\n'   : count += fprintf(stream, "\\n");  break;
				case '\r'   : count += fprintf(stream, "\\r");  break;
				case '\t'   : count += fprintf(stream, "\\t");  break;
				case '\v'   : count += fprintf(stream, "\\v");  break;
				default     : count += fprintf(stream, "%c", ((char*)a)[i]);
			 }
			count += fprintf(stream, "\"");
			break;
		case TSYMBOL:
			count += (Int)fwrite(a, 1, memObjectLength(a), stream);
			break;
		case TINTEGER:
			count += fprintf(stream, INT, *(Int*)a);
			break;
		case TREAL: 
			count += fprintf(stream, REAL, *(Real*)a);
			break;
		case TPAIR:
			if (!islist) count += fprintf(stream, "(");
			count += sysWriteR(car(a), 0, stream, max-count);
			if (objIsPair(cdr(a))) {
				count += fprintf(stream, " ");
				count += sysWriteR(cdr(a), 1, stream, max-count);
			} else {
				if (cdr(a)!=null) {
					count += fprintf(stream, " . ");
					count += sysWriteR(cdr(a), 0, stream, max-count);
				}
			}
			if (!islist) count += fprintf(stream, ")");
			break;
		case TVECTOR:
			count += fprintf(stream, "#(");
			for (i=0; i<memObjectLength(a) && count < max; i++) {
				if (i) count += fprintf(stream, " ");
				count += sysWriteR(memVectorObject(a, i), 0, stream, max-count);
			}
			if (count > max) count += fprintf(stream, "...");
			count += fprintf(stream, ")");
			break;
		case TCLOSURE:
			count += fprintf(stream, "#CLOSURE<CODE "HEX"  ENV "HEX">", car(a), cdr(a));
			break;
		case TCONTINUATION:
			count += fprintf(stream, "#CONTINUATION<"HEX">", a);
			break;
//		case TCODE:
//			count += fprintf(stream, "#CODE<"HEX">", a);
//			break;
		case TPORT:
		case TSOCKET:
			count += fprintf(stream, "#SOCKET<DESC:");
			count += sysWriteR(memVectorObject(a, 0), 0, stream, max);
			count += fprintf(stream, " ADDR:");
			count += sysWriteR(memVectorObject(a, 1), 0, stream, max);
			count += fprintf(stream, " PORT:");
			count += sysWriteR(memVectorObject(a, 2), 0, stream, max);
			count += fprintf(stream, " STATE:");
			count += sysWriteR(objPortState(a), 0, stream, max);
			count += fprintf(stream, " NEXT:");
			count += sysWriteR(memVectorObject(a, 4), 0, stream, max);
			count += fprintf(stream, " FINALIZER:");
			count += sysWriteR(memVectorObject(a, 5), 0, stream, max);
			count += fprintf(stream, ">");
			break;
//		case TSEMAPHORE:
//			count += fprintf(stream, "#SEMAPHORE<count:"NUM">", *(Num*)a);
//			break;
//		case TFINALIZER:
//			count += fprintf(stream, "#FINALIZER<"NUM" "OBJ">", *(Num*)a, ((Obj*)a)[1]);
//			break;
		case TSYSCALL:
			count += fprintf(stream, "#SYSCALL<"OBJ">", a);
			break;
		case TSTACK :
			count += fprintf(stream, "#[%x |", memStackLength(a));
			for (i=0; i<memStackLength(a); i++) {
				count += fprintf(stream, " "HEX, memStackObject(a, memStackLength(a)-i-1));
			}
			count += fprintf(stream, "]");
			break;
		default:
			count += fprintf(stream, "#"STR":"HEX, memTypeString(memObjectType(a)), a);
			if (memIsObjectBaseArray(a)) {
				for (i=0; i<memObjectLength(a); i++) {
					count += fprintf (stream, STR HEX02, i==0?"(":" ", ((u8*)a)[i]);
				}
				count += fprintf (stream, ")");
			} else {
				for (i=0; i<memObjectLength(a); i++) {
					count += fprintf (stream, STR OBJ, i==0?"<":" ", ((Obj*)a)[i]);
				}
				count += fprintf (stream, ">");
			}
	}

	return count;
}

Num sysWrite (Obj a, FILE *stream) {
	return (Num)sysWriteR (a, 0, stream, LONG_MAX);
}

Num sysWriteMax (Obj a, FILE *stream, Int max)
{
	return (Num)sysWriteR (a, 0, stream, max);
}

void sysDisplayR (Obj a, Num islist, FILE *stream) {
 Num i;
 Str desc;

	if (-0x430000 < (Int)a && (Int)a < 0x430000) {
		fprintf(stream, "#<");
		if ((Int)a < 0l)
			fprintf(stream, "-"HEX, labs((Int)a));
		else
			fprintf(stream, HEX, a);
		if ((desc = memPointerString(a))) fprintf(stream, ":"STR, desc);
		fprintf(stream, ">");
	} else switch (memObjectType(a)) {
		case TFALSE:
			fprintf(stream, "#f");
			break;
		case TTRUE:
			fprintf(stream, "#t");
			break;
		case TNULL:
			fprintf(stream, "()");
			break;
		case TNULLVEC:
			fprintf(stream, "#()");
			break;
		case TNULLSTR:
			break;
		case TEOF:
			fprintf(stream, "#eof");
			break;
		case TCHAR:
		case TSTRING:
		case TSYMBOL:
			fwrite(a, 1, memObjectLength(a), stream);
			break;
		case TINTEGER:
			fprintf(stream, INT, *(Int*)a);
			break;
		case TREAL: 
			fprintf(stream, REAL, *(Real*)a);
			break;
		case TPAIR:
			if (!islist) fprintf(stream, "(");
			sysDisplayR(car(a), 0, stream);
			if (!memIsObjectValid(cdr(a))) {
				fprintf(stream, " . ");
				sysDisplayR(cdr(a), 0, stream);
			} else if (objIsPair(cdr(a))) {
				fprintf(stream, " ");
				sysDisplayR(cdr(a), 1, stream);
			} else {
				if (cdr(a)!=null) {
					fprintf(stream, " . ");
					sysDisplayR(cdr(a), 0, stream);
				}
			}
			if (!islist) fprintf(stream, ")");
			break;
		case TVECTOR:
			fprintf(stream, "#(");
			for (i=0; i<memObjectLength(a); i++) {
				if (i) fprintf(stream, " ");
				 sysDisplayR(memVectorObject(a, i), 0, stream);
			}
			fprintf(stream, ")");
			break;
		case TCLOSURE:
			fprintf(stream, "#CLOSURE<CODE "OBJ"  ENV "OBJ">", car(a), cdr(a));
			break;
		case TCONTINUATION:
			fprintf(stream, "#CONTINUATION<"OBJ">", a);
			break;
//		case TCODE:
//			fprintf(stream, "#CODE<"OBJ">", a);
//			break;
		case TPORT:
		case TSOCKET:
			fprintf(stream, "#SOCKET<");
			sysDisplayR (memVectorObject(a, 0), 0, stream); fprintf(stream, " ");
			sysDisplayR (memVectorObject(a, 1), 0, stream); fprintf(stream, " ");
			sysDisplayR (memVectorObject(a, 2), 0, stream); fprintf(stream, " ");
			sysDisplayR (objPortState(a), 0, stream); fprintf(stream, " ");
			sysDisplayR (memVectorObject(a, 4), 0, stream); fprintf(stream, " ");
			sysDisplayR (memVectorObject(a, 5), 0, stream);
			fprintf(stream, ">");
			break;
		case TSYSCALL:
			fprintf(stream, "#SYSCALL<"STR":"OBJ">", memPointerString(*(Obj*)a), a);
			break;
//		case TSTACK :
//			fprintf(stream, "#[%x |", memStackLength(a));
//			for (i=0; i<memStackLength(a); i++) {
//				fprintf(stream, " "HEX, memStackObject(a, memStackLength(a)-i-1));
//			}
//			fprintf(stream, "]");
//			break;
		default:
			fprintf(stream, "#"HEX02"<"OBJ">", memObjectType(a), a);
	}
}

void sysDisplay (Obj a, FILE *stream) {
	sysDisplayR (a, 0, stream);
}

void sysDumpTGE (void) {
 Obj o;
 Num len;
	o = cdr(rtge);
	fprintf (stderr, "\n----TGE "OBJ"------------------------------------", o);
	while (o != null) {
		fprintf (stderr, "\n"OBJ" ", caar(o));
		len=sysWrite (cdar(o), stderr);
		fprintf(stderr, "                "+((len<17)?len-1:15));
		sysWriteMax (caar(o), stderr, 80);
		o=cdr(o);
	}
	fprintf (stderr, "\n\\------------------------------------------------------\n");
}

void sysDumpEnv (Obj e) {
 Obj formals;
 Num len, i;

	e = renv;
	if (e==rtge) fprintf (stderr, "\n----TGE "OBJ"------------------------------------\n...", e);

	while (e!=rtge) {
		formals = memVectorObject(e, 1); /* Consider local environment formals list */
		fprintf (stderr, "\n----ENV "OBJ"------------------------------------", e);
		for (i=2; objIsPair(formals); i++) {
			//fprintf (stderr, "\n"OBJ" ", memVectorObject(e, i));
			fprintf (stderr, NL);
			len = sysWrite (car(formals), stderr);
			fprintf(stderr, "                "+((len<17)?len-1:15));
			sysWriteMax (memVectorObject(e, i), stderr, 80);
			formals = cdr(formals);
		}
		e=memVectorObject(e, 0); /* Consider parent env */
	}
	fprintf (stderr, "\n\\------------------------------------------------------\n");
}

#undef DB_DESC
#undef DEBUG



/*******************************************************************************
 Ports
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "SYS_NET"

/* Open a local listener socket on port number passed in r1
   r1 <= IP port number
   r2 <= host string
   r0 => socket port object
*/
void sysOpenRemoteSocket (void) {
 char  hostname[128]={0};
 int fd;
 struct hostent *he;
 struct sockaddr_in sai;
	DBBEG();
	r3 = r1; /* consider port number */
	fd = socket(PF_INET, SOCK_STREAM, 0);
	if (fd == -1) {
		fprintf (stderr, "ERROR: socket(PF_INET, SOCK_STREAM, 0) => -1\r\n");
		r0 = false;
		goto ret;
	}

	/* Disable blocking on file descriptor. */
	fcntl(fd, F_SETFL, fcntl (fd, F_GETFL) | O_NONBLOCK);

	strncat(hostname, r2, memObjectLength(r2));
	he = gethostbyname(hostname);
	if (he == NULL) {
		fprintf (stderr, "ERROR sysOpenRemoteSocket() gethostbyname(\"%s\")==NULL %d::%s\r\n", hostname, errno, strerror(errno));
		r0 = false;
		goto ret;
	}
	sai.sin_family      = AF_INET;
	sai.sin_port        = htons(*(u16*)r3);
	sai.sin_addr.s_addr = *((in_addr_t*)he->h_addr);
	if (-1 == connect (fd, (struct sockaddr*)&sai, sizeof(sai))) {
		r4 = sconnecting;
	} else {
		r4 = sopen;
	}
	/* Force the fd to a long int to avoid "warning: cast to pointer from integer of different size" */
	r1 = (Obj)(0l + fd);
	objNewPort();
	
ret:
	DBEND();
} /* sysOpenRemoteSocket */


/* Open a local listener socket on port number passed in r1
   r1 <= IP port number
   r0 => socket port object
*/
void sysOpenLocalSocket (void) {
 int ld, on=1;
 struct sockaddr_in sai;
	DBBEG();

	DB("attempting to open local port "NUM, *(Num*)r1);

	ld = socket(PF_INET, SOCK_STREAM, 0);
	if (-1 == ld) {
		//printf ("ERROR: socket(PF_INET, SOCK_STREAM, 0)");
		r0 = false;
		goto ret;
	}

	if (-1 == setsockopt (ld, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(int))) {
		//printf ("ERROR: setsockopt()");
		r0 = false;
		goto ret;
	}

	sai.sin_family = AF_INET;
	sai.sin_port = htons(*(u16*)r1);
	sai.sin_addr.s_addr = htons(INADDR_ANY);
	if (-1 == bind (ld, (struct sockaddr*)&sai, sizeof(sai))) {
		//printf ("ERROR: bind() port="NUM" [%s]\r\n", *(Num*)r1, strerror(errno));
		r0 = false;
		goto ret;
	}

	if (-1 == listen(ld, 128)) {
		//printf("ERROR: listen()\r\n");
		r0 = false;
		goto ret;
	}

	fcntl (ld, F_SETFL, fcntl (ld, F_GETFL) | O_NONBLOCK);

	r1=(Obj)(Int)ld;
	r2=r3;         /* Put port number in host field for the hell of it. */
	r4=saccepting; /* State object starts out in accepting state. */
	objNewPort();

ret:
	DBEND();
}


/* Expect listening socket port in r1
*/
void sysAcceptRemoteStream (void) {
 Int ret;
 struct pollfd fds={(int)(Int)car(r1), POLLOUT, 0}; /* Cast pointer to integer of same size, then smaller size */
	DBBEG("  <=  r1:");
	DBE objDump(r1, stderr);
	ret = poll(&fds, 1, 0);
	if (ret == 1) {
		DB("poll() =>"INT, ret);
		if (fds.revents & POLLOUT) {
			memVectorSet(r1, 3, sopen);
			r0 = r1;
		} else {
			r0 = r1 = false;
		}
	} else if (ret == -1) {
		r0 = r1 = false;
	}
	DBEND("  =>  ");
	DBE objDump(r1, stderr);
}


/* IN:  r1:port object
  OUT:  r0:port object, possibly new
   Accept blocks.  Need to implement a blocking mechanism with the
   scheduler.  Hopefully it can coexist with the current fd blocked
   mechanism.  Expect a listening socket in r1.

   This should be called by read and the thread blocked if no connection
   has been made.
*/
void sysAcceptLocalStream (void) {
 struct sockaddr sa;
 socklen_t salen;
 int ld, fd2;
 char *name;
	DBBEG("  <=  r1:");
	DBE objDump(r1, stderr);
	ld = (int)(Int)memVectorObject(r1, 0);
	DB ("ld="NUM, ld);
	salen = sizeof(struct sockaddr);
	if (-1 == (fd2=accept(ld, &sa, &salen))) {
		DB ("accept returns -1 errno="NUM"="STR, errno, strerror(errno));
		r0=r1; /* Return the original socket port signaling that a stream port couldn't be created */
		goto ret;
	}

	/* Disable C level blocking on the new descriptor. */
	fcntl (fd2, F_SETFL, fcntl (fd2, F_GETFL) | O_NONBLOCK);

	/* The file descriptor. */
	r1 = (Obj)(0l + fd2);

	/* The host IP address string. */
	name=inet_ntoa(((struct sockaddr_in*)&sa)->sin_addr);
	objNewString((Str)name, strlen(name));
	r2=r0;

	objNewInt(ntohl(((struct sockaddr_in*)&sa)->sin_port));
	r3=r0;

	/* Port state to open "ready to read and write". */
	r4=sopen;

	objNewPort();

ret:
	DBEND(" => ");
	DBE objDump(r0, stderr);
}

/* In  r1 Port object.
       r2 Timeout integer or #f
       r3 Thing to read:  "..." specific length  "" any length including nothing  () one char
       r4 Bytes currently read already as an immediate integer

   Out r0 ""     No data ready yet
          #eof   No data will ever be ready
          "blah" All the read characters
          #f     No characters ready yet

	Blocks until timeout is reached (unless timeout is #f) or all the specified characters
	have been successfully read.
*/
void sysRecv (void) {
 Chr buffer[0x2000];
 Int ret;
 Num len;
	DBBEG("");
	/* Port is open and data is flowing. */
	if (objPortState(r1) == sopen) {
		DB("reading from file descriptor");
		/* Consider push-back character, will be #f if none there. */
		r0 = memVectorObject(r1, 4);

		/* Deal with 'any length' read request. */
		if (r3 == nullstr) {
			DB("  reading any length");
			/* If char in push-back, deal with it. */
			if (r0 != false) {
				*buffer = *(Chr*)r0;
				ret = read(*(int*)r1, buffer+1, 0xfff);
				if (ret<=0) ret=1;
				else ret++;
				memVectorSet(r1, 4, false); /* Clear push-back character. */
			} else
				ret = read(*(int*)r1, buffer, 0x1000);
			if (ret>0) objNewString(buffer, (Num)ret); /* Return new string. */
			else if (ret==0) r0 = eof;          /* File Descriptor closed. */
			else r0=false;                      /* No bytes available yet. */
		/* Deal with character read and return. */
		} else if (r3==null) {
			DB("  reading single character");
			/* Character already in push-back buffer. */
			if (r0 != false) {
				memVectorSet(r1, 4, false); /* Clear push-back character. */
			} else {
				DB("    before           *buffer="HEX02" errno="INT" %s", *buffer, errno, strerror(errno));
				ret = read(*(int*)r1, buffer, 1);
				DB("    after  ret="INT" *buffer="HEX02" errno="INT" %s", ret, *buffer, errno, strerror(errno));
				if (ret == 1) r0=memVectorObject(scharacters, *buffer);
				else if (ret==0) r0 = eof;
				else r0 = false;
			}
		/* Deal with fixed length read request. */
		} else {
			DB("  dealing with fixed length read request.  r3="HEX" Buffer = ", r3);
			len = memObjectLength(r3);
			DB("  reading fixed length "NUM"/"INT, r4, len);
			if (r0 != false) {
				*((char*)r3+(Int)r4) = *(char*)r0;
				ret = read(*(int*)r1, r3+(Int)r4+1, len-(Num)r4-1);
				if (ret<=0) ret = 1;
				else ret++;
				memVectorSet(r1, 4, false); /* Clear push-back character. */
			} else
				ret = read(*(int*)r1, r3+(Int)r4, len-(Num)r4);
			if (ret>0) {
				r4+=ret;
				if ((Int)r4 == len) r0=r3; /* Return the string buffer obj. */
				else r0 = false;           /* Not ready yet ret false and block. */
			} else if (ret<0) r0=false;   /* Nothing read so keep blocking. */
			else { /* ret==0 File descriptor closed...see if anything was read. */
				if (r4 == 0) r0 = eof;
				else {
					objNewString(r3, (Num)r4);
   				memcpy(r0, r3, (Num)r4);
				}
			}
		}
	} else { /* State of the port must be 'closed'. */
		assert(sopen == objPortState(r1));
		r0 = eof;
	}

	DBEND(" =>  r0="HEX, r0);
} /* syRecv */

/* Given port object in r1, string in r2 and sent count in r3, send string to
   port object.
*/
void sysSend (void) {
 Num len=0;
 Int ret;
	DBBEG();

	if (r2 == nullstr) r0=nullstr;
	/* Port is open and data is flowing. */
	else if (objPortState(r1) == sopen) {
		len = memObjectLength(r2);
		DB("sent so far "INT"/"INT"", r3, len);
		ret = write(*(int*)r1, r2+(Int)r3, len-(Num)r3);
		if (ret>0) {
			r3 = (Obj)(ret + (Int)r3);
			/* Return true. Can't return the string in case the 'string' is #f.
			   which implies nothing sent and that the thread should block (bug).*/
			r0 = (len == (Int)r3) ? true : false;
		} else if (ret<=0) {
			if(errno == EAGAIN) {
				r0=false;/* Nothing sent so block. */
			} else {
				DB("ERROR: Unknown return value from system send [%s]", strerror(errno));
				r0=eof; /* ret==0 closed fd so return eof. */
			}
		}
	} else
		r0 = eof;

	DBEND("sent so far "NUM"/"NUM, r3, len);
}

/* Create a file object used for file I/O.
   r1 <= filename object
   r0 => port object or #eof
	r2 => normalized filename object with null
*/
void sysOpenFile (int oflag, mode_t mode, Num silent) {
 Num i, filenameLen;
	DBBEG();

	filenameLen = memObjectLength(r1);
   r2 = memNewArray(TSTRING, filenameLen+1);
   memcpy(r2, r1, filenameLen);

	/* Filter out directory paths.  Replace all '/' with '!' */
	for (i=0; i<filenameLen; ++i) if (((Chr*)r2)[i]=='/') ((Chr*)r2)[i]='!';
	DB ("Filename ["STR"]", r2);

	r1 = (Obj)(Int)open((char*)r2, oflag, mode);

	if (-1 == (Int)r1) {
		r0 = false;
	} else {
		/* Create the port object passing the fd/r1, normalized filename/r2,
		   descriptor flags/r3 and object state/r4 */
		objNewInt(oflag);  r3=r0;
		r4 = sopen;
		objNewPort();
	}
	DBEND();
}


/* r1 <= semaphore
   r0 => false if already closed or closed by another thread
         true  if successfully downed (which happens when incremented by another and its thread awoken)
         the semaphore if decremented past 0 signalling that it should be blocked
*/
void sysSemaphoreDown (void) {
	if (false == memVectorObject(r1,0)) { /* Semaphore is closed so do nothing */
		r0 = false;
	} else if (--*(Int*)r1 < 0) /* Decrement the semaphore's counter */
		r0=r1;
	else
		r0 = true;
}

/* r1 <= semaphore
   r0 => false if already closed
         true if greater than 0
         the semaphore if less than 1 signalling that a blocked thread can be awoken
*/
void sysSemaphoreUp (void) {
	if (false == memVectorObject(r1,0)) {
		/* Semaphore is closed so do nothing */
		r0 = false;
	} else if (0 < ++*(Int*)r1) {
		r0 = true;
	} else
		r0 = r1;
}

#undef DB_DESC
#undef DEBUG



/*******************************************************************************
 Initialization
*******************************************************************************/
#define DEBUG DEBUG_ALL|0
#define DB_DESC "SYS_INIT "

void sysInitialize (void) {
 static Num shouldInitialize=1;
 Num i;
	DBBEG();
	if (shouldInitialize) {
		DB("Activating module...");
		shouldInitialize=0;
		objInitialize(); /* vm mem */

		DB("Registering static pointer description strings");
		memPointerString(sysNewClosure1Env);

		DB("Create empty The Global Environment");
		objNewSymbol((Str)"TGE", 3);
		r1=r0;  r2=null;  objCons12();
		renv = rtge = r0;

		/* Create the standard I/O port object symbols in TGE */
		DB("Create I/O port symbols and objects");
		r1=(Obj)0; /* Descriptor */
		objNewSymbol ((Str)"stdin", 5);  r2=r3=r0; /* Address and port */
		r4 = sopen; /* State */
		objNewPort(); sysDefine("stdin");

		r1=(Obj)1;
		objNewSymbol ((Str)"stdout", 6);  r2=r3=r0;
		r4 = sopen;
		objNewPort ();  sysDefine("stdout");

		r1=(Obj)2;
		objNewSymbol ((Str)"stderr", 6);
		r2=r3=r0;
		r4 = sopen;
		objNewPort(); sysDefine("stderr");

		// TODO clean this mechanism up
		DB("Creating signalhandlers symbol and vector");
		i=32;
		objNewVector(i);
		while (i--) { memVectorSet(r0, i, null); }
		sysDefine("SIGNALHANDLERS");

	} else {
		DB("Module already activated");
	}
	DBEND();
}

#undef DB_DESC
#undef DEBUG
#undef DEBUG_ALL
