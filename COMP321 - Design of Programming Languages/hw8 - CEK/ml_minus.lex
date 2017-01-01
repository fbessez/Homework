%name MLMinusLexer ;

%let digit = [0-9] ;
%let num = {digit}+ ;
%let alpha = [a-zA-Z] ;
%let id = {alpha}({alpha} | {digit} | "_")* ;
%let ws = [\n\ \t] ;
%let dquote_str = \"([^"] | "\\\"")*\" ;
%let blockcomment = "(*" ~(.*"*)".*) "*)" ;

%defs (
  structure T = MLMinusTokens
  type lex_result = T.token
  fun eof() = T.EOF
  exception lex_error of string

  fun trimFirstLast (s : string) : string =
    String.substring(s, 1, String.size s - 2)
) ;

{num}           => ( T.NUM (valOf (Int.fromString yytext)) ) ;
"("             => ( T.LP ) ;
")"             => ( T.RP ) ;
"["             => ( T.LVERT ) ;
"]"             => ( T.RVERT ) ;
"+"             => ( T.PLUS ) ;
"-"             => ( T.MINUS ) ;
"*"             => ( T.TIMES ) ;
"^"             => ( T.CAT ) ;
"="             => ( T.EQ ) ;
"<>"            => ( T.NE ) ;
"<"             => ( T.LT ) ;
"<="            => ( T.LE ) ;
">"             => ( T.GT ) ;
">="            => ( T.GE ) ;
"/"             => ( T.DIV ) ;
"fn"            => ( T.FN ) ;
"=>"            => ( T.DARROW ) ;
"let"           => ( T.LET ) ;
"let rec"       => ( T.LETREC ) ;
"be"           => ( T.BE ) ;
"in"           => ( T.IN ) ;
"end"           => ( T.END ) ;
"true"          => ( T.TRUE ) ;
"false"         => ( T.FALSE ) ;
"orelse"         => ( T.ORELSE ) ;
"andalso"         => ( T.ANDALSO ) ;
"not"         => ( T.NOT ) ;
"if"         => ( T.IF ) ;
"then"         => ( T.THEN ) ;
"else"         => ( T.ELSE ) ;
"fi"         => ( T.ENDIF ) ;
"#1"            => ( T.SEL1 ) ;
"#2"            => ( T.SEL2 ) ;
","             => ( T.COMMA ) ;
"val"           => ( T.VAL ) ;
"val rec"       => ( T.VALREC ) ;
{dquote_str}  => ( T.STRING (trimFirstLast yytext) ) ;
{id}            => ( T.ID yytext ) ;
{ws}            => ( continue() ) ;
{blockcomment}  => ( skip() ) ;
.               => ( raise (lex_error (concat ["Illegal character: ", yytext]))) ;

