(*  COMP 321 Homework 3:  Lexer and parser for a fragment of C.
*
*   ml-ulex specification.
*   
*   By: Ping-Jung Liu, Fabien Bessez
*   Fall 2016
*)

(*  The name of the generated structure must be CPPLexer.
*)
%name CPPLexer ;


%let digit = [0-9] ;
%let num = {digit}+ ;
%let double = {num}\.{num} ;
%let bool = true|false;
%let ws = [\n\t\ ] ;
%let alpha = [a-zA-Z];
%let id = {alpha}({alpha} | {digit} | _)* ;
%let type = int|double|string|bool|void ;
%let comment = ("//"|"#")(.)*\n? | "/*"(.)*(\n\t)*"*/" ;
%let string = \"([^"] | "\\\"")*\" ;

(*  You might want to add some definitions, but these should be useful. *)
%defs (
  structure T = CPPGrmTokens
  type lex_result = T.token
  fun eof() = T.EOF
  exception lex_error of string
) ;

"+"		=> ( T.PLUS ) ;
"-"		=> ( T.MINUS ) ;
"/"		=> ( T.DIVIDE ) ;
"%"		=> ( T.MOD ) ;
"?"		=> ( T.Q ) ;
"&&"		=> ( T.CONJUNCTION ) ;
"||"		=> ( T.DISJUNCTION ) ;
"<<"		=> ( T.LSHIFT ) ;
">>"		=> ( T.RSHIFT ) ;
"++"		=> ( T.INCREMENT );
"--"		=> ( T.DECREMENT ) ;
"!"		=> ( T.NEGATION ) ; 
"<"		=> ( T.LT ) ;
">"		=> ( T.GT ) ;
"*"		=> ( T.TIMES ) ;
"="		=> ( T.EQ ) ;
";"		=> ( T.SEMI ) ;
":"		=> ( T.COLON ) ;
","		=> ( T.C ) ;
"{"		=> ( T.LC ) ;
"}"		=> ( T.RC ) ;
"("		=> ( T.LPAREN );
")"		=> ( T.RPAREN );
"printf"	=> ( T.PRINT );
"if"		=> ( T.IF );
"else"		=> ( T.ELSE ) ;
"for"		=> ( T.FOR ) ;
"do"		=> ( T.DO ) ;
"while"		=> ( T.WHILE ) ;
"return"	=> ( T.RETURN ) ;
{string}	=> ( T.STRING yytext );
{comment}	=> ( skip() ) ;
{bool}		=> ( T.BOOL yytext );
{double}	=> ( T.DOUBLE (valOf (Real.fromString yytext))) ;
{type}		=> ( T.TYPE yytext) ;
{num}		=> ( T.NUM ( valOf (Int.fromString yytext)) ) ;
{id}		=> ( T.ID yytext ) ;
{ws}		=> ( skip() );
.		=> ( raise Fail ( "Unexpected character: " ^ yytext ))
; 
