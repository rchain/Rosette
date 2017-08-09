grammar Rbl;

/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/

program : expr* EOF ;

/* Order matters */
expr : method
     | rmethod
     | quote
     | free
     | gotoexpr
     | set
     | label
     | string
     | tuple
     | block
     | seq
     | let
     | letrec
     | ifexpr
     | proc
     | constant
     | token
     | id
     | request
     | send ;

/* Distinguish between method/rmethod expressions and method/rmethod as used in defActor and similar */

mpattern: expr* ']'
        | expr* '&' expr ']' ;

/* Method */

method : METHOD mpattern expr+ CP ;

/* Reflective method */

rmethod : RMETHOD mpattern expr+ CP ;

/* Quote */

quote : '\'' expr | '\'\\' constant;

/* Miscellaneous forms */

free : FREE '[' id* ']' expr+ CP ;

gotoexpr : GOTO id CP ;

set : SET id expr CP ;

label : LABEL id expr+ CP ;

/* String */

string : STRING ;

/* Tuple */

tuple : '[' expr* ']'
      | '[' expr* '&' expr ']' ;

/* Block */

block : BLOCK expr+ CP ;

/* Seq */

seq : SEQ expr+ CP ;

/* Let */

let : LET '[' ('[' id expr ']' | '[' pattern expr ']')* ']' expr+ CP ;

letrec : LETREC '[' ('[' id expr ']')* ']' expr+ CP ;

/* If */

ifexpr : IF expr expr CP
       | IF expr expr expr CP ;

/* Proc */

proc : PROC pattern expr CP ;

/* Constant */

constant : rboolean
         | rfixnum
         | rfloat
         | rchar
         | rescape
         | rabsent
         | reof
         | rniv
         | readerror
         | incompleteio ;

rboolean : '#t' | '#f' ;

rfixnum : FIXNUM ;

rfloat : FLOAT ;

rchar : CHAR ;

rescape : ESCAPE ;

rabsent : '#absent' ;

reof : '#eof' ;

rniv : '#niv' ;

readerror : '#read-error' ; // undocumented

incompleteio: '#incomplete-io' ; // undocumented

/* Token */

token : ATOM ;

/* Identifier */

id : ATOM ;

/* Request */

request : OP expr clause CP ;

send : SEND expr clause CP ;

clause : expr*
       | expr* '&' expr ;

/* Message pattern */

pattern : '[' expr* ']'
        | '[' expr* '&' expr ']' ;

/*------------------------------------------------------------------
 * LEXER RULES (order matters)
 *------------------------------------------------------------------*/

METHOD : '(' ' '* 'method ' ' '* '[' ;

RMETHOD : '(' ' '* 'rmethod ' ' '* '[' ;

SEND : '(' ' '* 'send ' ;

LET : '(' ' '* 'let ' ;

LETREC : '(' ' '* 'letrec ' ;

BLOCK : '(' ' '* 'block ' ;

SEQ : '(' ' '* 'seq ' ;

IF : '(' ' '* 'if ' ;

PROC : '(' ' '* 'proc ' ;

FREE : '(' ' '* 'free ' ;

GOTO : '(' ' '* 'goto ' ;

SET : '(' ' '* 'set! ' ;

LABEL : '(' ' '* 'label ' ;

OP : '(' ;
CP : ')' ;

FLOAT : DIGIT+ '.' DIGIT* | DIGIT* '.' DIGIT+ ;

FIXNUM : DIGIT+ ;

STRING : '"' ( '\\"' | . )*? '"' ;

ESCAPE : '#\\\\' ('n' | 'r' | 't' | 'f' | HEX | '\\') ;

fragment HEX : 'x' [\p{Hex_Digit}] [\p{Hex_Digit}] ;

// includes '#\ '
CHAR : '#\\' . ;

DIGIT : '0'..'9';

WHITESPACE : [ \r\n\t] + -> channel (HIDDEN);

COMMENT : ';' .*? '\n' -> skip ;

ATOM : (LETTER | DIGIT | EXTENDED) (LETTER | DIGIT | EXTENDED | '\'')* ;

fragment LETTER : LOWER | UPPER ;

fragment EXTENDED : ('+' | '-' | '*' | '/' | '<' | '=' | '>' | '!' | '?' | '$' | '%' | '_' | '~' | '^' | '&' | ':' | '\\' | '.' | '@' | ',' | '`' ) ;

fragment LOWER : ('a'..'z') ;
fragment UPPER : ('A'..'Z') ;