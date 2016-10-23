(*  COMP 321 Homework 2:  ML-Ulex specification.
*   
*   Driver program that parses a string by lexing it with the lexer
*   defined in NumLex.
*
*   You can use the driver by compiling it in the REPL with
*   - CM.make "nums.cm" ;
*   Then if s is any string, Nums.parse s returns the integer n such that
*   the lexer yields NumsTokens.Num(n) when tokenizing s.  If s cannot be
*   tokenized, Nums.parse s raises Fail.
*
*   You can also compile the driver to an executable with
*   $ make nums
*   Then the shell command
*   $ ./nums s
*   prints the integer n such that the lexer yields NumsTokens.Num(n) when
*   tokenizing s.  If s cannot be tokenized, an error message is printed.
*
*   Fall 2016
*)

structure Nums =
struct

  structure Lex = NumLex
  structure T = NumsTokens

  (*  parse s = n, where n is the integer represented by s.  s is assumed
  *   to be a string of digits written in groups of three separated by 
  *   commas with no leading zeros.
  *
  *   If s does not have the correct form, an error is raised with a message
  *   indicating the initial segment of s that could be lexed.
  *)
  fun parse(s : string) : int =
  let
    val sm = AntlrStreamPos.mkSourcemap()
    val lex = Lex.lex sm

    val strm = Lex.streamifyInstream (TextIO.openString s)

    val (T.Num n, _, strm) = lex strm
    val (tok, _, _) = lex strm

  in

    case tok of
         T.EOF => n
       | _ => 
             let
               val msg = String.concat [
                 "Parsed ",
                 Int.toString n,
                 ", but then had extra tokens."
               ]
             in
               raise (Fail msg)
             end

  end

  (*  A main function for the executable.
  *)
  fun main(arg0 : string, argv : string list) : int =
  let
    val s :: _ = argv
  in
    (print ((Int.toString (parse s)) ^ "\n") ; 0)
    handle Fail msg => (print (msg ^ "\n") ; 1 )
  end


end
