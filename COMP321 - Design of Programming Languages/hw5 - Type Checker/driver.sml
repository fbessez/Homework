(*  COMP 321 homework 3:  CPP parser driver.
*   
*   Upon building this driver, there are several ways of executing it from
*   the shell.  
*
*   - To parse a program that is in file f and print the resulting
*     Ast.program value to the screen (using Ast.programToString):
*
*       $ ./driver f
*
*   - To parse an expression that is in file f and print the resulting
*     Ast.expr value to the screen (using Ast.expToString):
*
*       $ ./driver --expr f
*
*   - To print the results of just lexing the contents of file f:
*
*       $ ./driver --lex f
*
*   - To parse an expression e that you give on the command line instead of in a
*     file:
*
*       $ ./driver --expr --arg e
*
*     Note that if e has spaces, you should put quotes around it.
*
*   
*
*   N. Danner
*)

structure Driver =
struct

  structure Lex = CPPLexer
  structure T = CPPGrmTokens
  structure P = CPPGrmParseFn(Lex)

  (*  printnl s = ().
  *
  *   As a side-effect, s will be printed to the terminal followed by a newline.
  *)
  fun printnl(s : string) : unit =
    print (String.concat [s, "\n"])

  fun printssnl(ss : string list) : unit =
    printnl (String.concatWith " " ss)

  fun tokensToString strm =
  let
    val sm = AntlrStreamPos.mkSourcemap()
    val lex = Lex.lex sm

    fun tokensToList (strm : Lex.strm) =
      case lex strm of
           (T.EOF, _, _) => [T.EOF]
         | (t, _, strm) => t :: tokensToList strm
  in
    String.concatWith " " (map T.toString (tokensToList strm))
  end

  (*  parse parser strm = ().  Parses strm using the function parse,
  *   which ought to be either P.parse or P.parseE, where E is an entry point
  *   to the parser specifed in an %entry directive.
  *)
  fun parse parser strm =
  let
    val sm = AntlrStreamPos.mkSourcemap()
    val lex = Lex.lex sm

    (*  e : Ast.exp option is the expression that could be parsed from strm.
    *   strm : Lex.strm is the rest of the stream after the parse.
    *   repairs is the list of repairs to the stream made by the parser
    *     in order to successfully parse.
    *
    *   For us:  a parse is only successful if e = SOME e' and repairs = [].
    *)
    val (e, strm, repairs) = parser lex strm
  in
    case (repairs, lex strm, e) of
         ([], (T.EOF, _, _), SOME e') => e'
       | ([], (T.EOF, _, _), NONE) =>
           raise Fail "Parse result:  NONE."
       | ([], (_, _, _), _) =>
           raise Fail "Extra tokens!"
       | (_, _, _) =>
           raise Fail (String.concatWith "\n" [
             "********* Parser reports errors *********",
             String.concatWith "\n" (
               map (AntlrRepair.repairToString T.toString sm) repairs
             ),
             "*****************************************"
           ])
  end

  fun main(arg0 : string, argv : string list) : int =
  let
    val () = SMLofNJ.Internals.TDP.mode := true

    val doExp = ref false

    val streamFromFile = (Lex.streamifyInstream o TextIO.openIn)
    val streamFromString = (Lex.streamifyInstream o TextIO.openString)
    val stream = ref (streamFromFile)

    val parsePgm = (parse P.parse)
    val parseExp = (parse P.parseexp)

    val checkPgm = ref (Typing.checkPgm)

    val handler = 
      ref (AnnAst.programToString o Typing.checkPgm o parsePgm)

    val usage = String.concatWith "\n" [
      "driver [--lex] [--parse] [--expr] [--arg] s",
      "",
      "Parse (default) or lex the contents of file s.",
      "Options:",
      "\t--lex:   lex only",
      "\t--parse: lex and parse only",
      "\t--expr:  s specifies an expression, not a program; --lex ignored",
      "\t--arg:   lex/parse s itself; i.e., s does not name a file to read",
      "\n"
    ]

    exception doUsage

    (*  handleOpt : handle a single option by setting stream or parser
    *   appropriately.
    *
    *   Pre-condition:  oa = "--" ^ oa'.
    *)
    fun handleOpt (oa : string) : unit =
    let
    in
      case String.substring(oa, 2, String.size oa - 2) of
           "arg" => stream := streamFromString
         | "expr" => (
             doExp := true ;
             handler := 
               (AnnAst.expToString o Typing.inferExpNoEnv o parseExp)
           )
         | "lex" => (
             handler := tokensToString
           )
         | "parse" => if !doExp then (
             handler := (Ast.expToString o parseExp)
           ) else (
             handler := (Ast.programToString o parsePgm)
           )
         | _ => raise doUsage
    end

    (*  handleOpts : handle all options by calling handleOpt o for each option o
    *   on the command line.
    *)
    fun handleOpts (optsargs : string list) : string list =
    let
    in
      case optsargs of
           [] => []
         | oa :: oas =>
             if String.isPrefix "--" oa then (handleOpt oa ; handleOpts oas)
             else oa :: oas
    end

    (*  handleArgs : handle all arguments on the command line.  args must be
    *   a string of length exactly 1.
    *)
    fun handleArgs (args : string list) : unit =
    let
    in
      case args of
           [s] => printnl ((!handler o !stream) s)
         | _ => raise doUsage
    end

    val handleOptsArgs = handleArgs o handleOpts

  in
    (handleOptsArgs argv ; 0)
    handle doUsage => (print usage ; 1)
      | Fail msg => (printnl msg ; 1)
      | Typing.UndeclaredError id => 
          (printssnl ["Exception: UndeclaredError for ", id] ; 1)
      | e => (printnl (String.concatWith " " ["Exception: ", exnMessage e]) ; 1)
  end

end
