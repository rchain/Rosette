grammar Rbl;

/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/

program : expr* EOF ;

expr : request
     | literal
     | token
     | string
     | tuple
     | id
     | block
     | let
     | letstar
     | letrec
     | ifexpr
     | cond
     | iterate
     | doexpr
     | dostar
     | constant ;

/* Request */

request : OP expr clause CP
        | OP 'send' expr clause CP ;

clause : expr*
       | expr* '&' expr ;

/* Literal Expression */

literal : LITERAL ('(' | ')' )* ;

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

block : OP 'block' expr+ CP
      | OP 'seq' expr+ CP ;

/* Let */

let : OP 'let' '[' ('[' id expr ']' | '[' pattern expr ']')* ']' expr+ CP ;

letstar : OP 'let*' '[' ('[' id expr ']' | '[' pattern expr ']')* ']' expr+ CP ;

letrec : OP 'letrec' '[' ('[' id expr ']')* ']' expr+ CP ;

pattern : '[' expr* ']'
        | '[' expr* '&' expr ']' ;

/* Conditional */

ifexpr : OP 'if' expr expr CP
   | OP 'if' expr expr expr CP ;

cond : OP 'cond' ( '(' expr expr ')' )+ CP
     | OP 'cond' ( '(' expr expr ')' )+ '(' 'else' expr ')' CP ;

/* Iteration */

iterate : OP 'iterate' id '[' ( '[' id expr ']' | '[' expr* ']' )* ']' expr* CP ;

doexpr : OP 'do' '[' ('[' id expr expr ']')* ']' '[' ('(' expr expr ')')* ']' expr? CP ;

dostar : OP 'do*' '[' ('[' id expr expr ']')* ']' '[' ('(' expr expr ')')* ']' expr? CP ;

/* Constant */

constant : rboolean
         | rfixnum
         | rfloat
         | rchar
         | rescape
         | rabsent
         | reof
         | rniv ;

rboolean : '#t' | '#f' ;

rfixnum : FIXNUM ;

rfloat : FLOAT ;

rchar : CHAR ;

rescape : ESCAPE ;

rabsent : '#absent' ;

reof : '#eof' ;

rniv : '#niv' ;

/*------------------------------------------------------------------
 * LEXER RULES (order matters)
 *------------------------------------------------------------------*/

OP : '(';
CP : ')';

FLOAT : DIGIT+ '.' DIGIT* | DIGIT* '.' DIGIT+ ;

FIXNUM : DIGIT+ ;

LITERAL : '\'' [()]* (LETTER | DIGIT | WHITESPACE)+ ;

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
