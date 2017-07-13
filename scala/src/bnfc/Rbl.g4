grammar Rbl;

/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/

program : expr* EOF ;

/* Order matters */
expr : method
     | request
     | send
     | quote
     | token
     | string
     | tuple
     | id
     | block
     | seq
     | let
     | letrec
     | ifexpr
     | method
     | proc
     | free
     | gotoexpr
     | set
     | label
     | constant ;

/* Request */

request : OP (expr | 'method') clause CP ;

send : OP 'send' (expr | 'method') clause CP ;

clause : expr*
       | expr* '&' expr ;

/* Quote */

quote : '\'' expr ;

/* Token */

token : TOKEN ;

/* String */

string : STRING ;

/* Tuple */

tuple : '[' expr* ']'
      | '[' expr* '&' expr ']' ;

/* Identifier */

id : TOKEN ;

/* Block */

block : OP 'block' expr+ CP ;

/* Seq */

seq : OP 'seq' expr+ CP ;

/* Message pattern */

pattern : '[' expr* ']'
        | '[' expr* '&' expr ']' ;

/* Let */

let : OP 'let' '[' ('[' id expr ']' | '[' pattern expr ']')* ']' expr+ CP ;

letrec : OP 'letrec' '[' ('[' id expr ']')* ']' expr+ CP ;

/* If */

ifexpr : OP 'if' expr expr CP
       | OP 'if' expr expr expr CP ;

/* Method */

method : OP 'method' pattern expr+ CP ;

/* Proc */

proc : OP 'proc' pattern expr CP ;

/* Reflective method */

/* ??? */

/* Null expression */

/* Miscellaneous forms */

free : OP 'free' '[' id* ']' expr+ CP ;

gotoexpr : OP 'goto' id CP ;

set : OP 'set!' id expr CP ;

label : OP 'label' id expr CP ;

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

/*------------------------------------------------------------------
 * LEXER RULES (order matters)
 *------------------------------------------------------------------*/

OP : '(';
CP : ')';

FLOAT : DIGIT+ '.' DIGIT* | DIGIT* '.' DIGIT+ ;

FIXNUM : DIGIT+ ;

CHAR : '#\\' . ;

STRING : '"' ( '\\"' | . )*? '"' ;

ESCAPE : '#\\\\' ('n' | 'r' | 't' | 'f' | 'x' | '\\') ;

TOKEN : (LETTER | DIGIT | SPECIAL_CHAR)+ ;

WHITESPACE : [ \r\n\t] + -> channel (HIDDEN);

DIGIT : '0'..'9';

LETTER : LOWER | UPPER ;

SPECIAL_CHAR : ('+' | '-' | '*' | '/' | '<' | '=' | '>' | '!' | '?' | '$' | '%' | '_' | '~' | '^' | '\'' | '&' | ':' | '\\' | '.' | '@' | ',' | '`' ) ;

LOWER : ('a'..'z') ;
UPPER : ('A'..'Z') ;

COMMENT : ';' .*? '\n' -> skip ;
