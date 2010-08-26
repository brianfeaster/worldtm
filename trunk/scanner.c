#define DEBUG 0
#define DB_MODULE "PARSE "
#include "debug.h"

#include <stdio.h>
#include <stdlib.h> /* strtol() */
#include <unistd.h>
#include <string.h>
#include "obj.h"
#include "scanner.h"

/* Non-final states.  These must be in numerical order starting at 0 as they
   represent the index of a table of vectors. */
#define A 0x00 // Initial state
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
#define N 0x0b /* #\ */
#define O 0x0c // hexidecimal number
#define P 0x0d // , or ,@
#define Q 0x0e // octal number
#define MAXSTATE Q

/* Final states. */
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

/* Final state with character push-back.
*/
#define s SOCT
#define t SUNQUOTE
#define u SBINARY
#define v SSYMBOL
#define w SHEX
#define x SDOT
#define y SREAL
#define z SINTEGER

#define STRING     0
#define DISCRIPTOR 1
#define EOFCHAR    256
char buff[65536], *bp, *bpLast;
int fd=0, mode=DISCRIPTOR, ungetchar=EOFCHAR;



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
 int len = strlen((char*)buff0);
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
int readChar (void) {
 char ch;
	if (EOFCHAR != ungetchar) {
		ch = ungetchar;
		ungetchar = EOFCHAR;
		return ch;
	}
	if (mode==STRING) {
		if (bp == bpLast) return EOFCHAR;
		ch = *bp++;
		return ch;
	}
	if (0 >= read(fd, &ch, 1)) return EOFCHAR;
	return ch;
}

void unreadChar (char ch) {
	ungetchar = ch;
}



Num transition (Num ch, Num state) {
 static int table[] = {
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
	   D,D,K,D,D,D,j,D,D,D,D,D,D,D,D,Q,D,D,D,D,i,D,D,D,O,D,D,d,N,v,D,D,
	// ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~
	   D,D,K,D,D,D,j,D,D,D,D,D,D,D,D,Q,D,D,D,D,i,D,D,D,O,D,D,d,D,v,D,v,
	//��������������������������������
	   v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,
	//� � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   v,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � �
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,
	// � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � � EOF
	   D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,v,

	/* D Symbol state.  For now we loop around until symbol scaned. */
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
	   v,D,D,D,D,D,D,D,v,v,D,D,D,D,J,D,G,G,G,G,G,G,G,G,G,G,D,D,D,D,D,D,
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

	//printf ("\r\ntransition(ch %x, state %x)", ch, state);
	if (MAXSTATE<state || 257<ch) {
		printf ("ERROR: transition: illegal char or state %03x %02x\n", ch, state);
		exit(0);
	}
	return table[state*257+ch];
}

/* Replace escape chars with actual char in string squishing the string in
   the process.  Used after a call to yylex.  Also tweaks yyleng.
*/
Num parseString (Str str) {
 Str p=str++; /* Start p right after the first quote. */
	yyleng = 0;
	while (*str!='"') {
		if (*str == '\\')
			switch(*++str) {
				case '"': *p++ = '"';  break;
				case '\\':*p++ = '\\'; break;
				case 'a': *p++ = '\a'; break;
				case 'b': *p++ = '\b'; break;
				case 'c': *p++ = 0x9b; break;
				case 'e': *p++ = '\e'; break;
				case 'f': *p++ = '\f'; break;
				case 'n': *p++ = '\n'; break;
				case 'r': *p++ = '\r'; break;
				case 't': *p++ = '\t'; break;
				case 'v': *p++ = '\v'; break;
				default: *p++ = *str;
			}
		else
			*p++ = *str;
		yyleng++;
		str++;
	}
	return yyleng;
}

/* Kernel based blocking parse function.  This code will be ported to wscheme
   vm.
*/
Chr yytext[65536];
Str yyp;
Num yyleng;

Num yylex (void) {
 u32 ch, state;

	/* Initial state and scanned-token buffer. */
	state=0;
 	yyp=yytext;

	/* Continue state transtions over non-final states. */
	while (!(state & FINALSTATE)) {
		*yyp++ = ch = readChar();
		state = transition(ch, state);
		/* Re-initialize yytext whenever we re-enter initial state. */
		if (state==A) yyp=yytext;
	}
	/* Handle character push-back states. */
	if (state & PUSHBACK) {
		unreadChar(ch);
		yyp--;
	}

	/* Calculate scanned token length. */
	yyleng = yyp-yytext;

	/* Delimit scanned string with NULL for C string compatibility. */
	yytext[yyleng]=0;

	return state;
}

/* C implementation.  Look in wscm.c for the VM assembly version. */
void parse (long islist) {
 Num state;

	state = yylex();
	DB ("parse %08x [%s]", state, yytext);
   switch (state) {
      case SEOF :        r0=eof; return;
      case SCLOSEPAREN : r0=(null); return;
      case SFALSE :      r0=(false); break;
      case STRUE :       r0=(true); break;
      case SOPENPAREN :  parse(1); break;
		/* Silently ignore anything scanned after the 2nd obj in a malformed
		   dotted list IE: (a . b ignore these things) => (a . b) */
		case SDOT :        parse(1); r0=car(r0); return;
      case SINTEGER:     objNewInt(strtol((char *)yytext, 0, 10)); break;
      case SREAL   :     objNewReal(strtof((char *)yytext, 0)); break;
      case SHEX :        objNewInt(strtol((char *)yytext+2,0,16)); break;
      case SOCT :        objNewInt(strtol((char *)yytext+2,0,8)); break;
      case SBINARY :     objNewInt(strtol((char *)yytext+2,0, 2)); break;
      case SSTRING :     parseString(yytext);
		                       if (yyleng) objNewString((Str)yytext, yyleng);
		                       else r0=nullstr; break;
      case SCHARACTER :  r0=memVectorObject(characters, yytext[2]); break;
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
   }
	/* If constructing a list, cons this object with the rest of the list.  */
   if (islist) {
		push(r0);   
      parse(1);
		r1=pop();
		if (r0==eof) return;
		r2=r0;  objCons12();
   }
}

void yyparse (void) {
	DB("-->yyparse()");
	parse (0);
	DB("<--yyparse()");
}


int scannermain (void) {
 int ret;
	while((ret=yylex())) {
		hashpjw(yytext, yyleng);
		printf ("->ret=%08x [%s]\n", ret, yytext);
	}
	return 0;
}

#undef DB_MODULE
