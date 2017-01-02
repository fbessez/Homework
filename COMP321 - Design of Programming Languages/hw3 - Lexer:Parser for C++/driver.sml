(*  COMP 321 homework 3:  CPP parser driver.
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

  (*  lex strm = ().  Lexes stream to produce a token list, which is then
  *   converted to a string with T.toString.
  *)
  fun lex (strm : Lex.strm) : unit =
  let
    val sm = AntlrStreamPos.mkSourcemap()
    val lex = Lex.lex sm

    fun lex' (strm : Lex.strm, toks : T.token list) : T.token list =
      case lex strm of
           (T.EOF, _, _) => T.EOF :: toks
         | (t, _, strm) => lex' (strm, t :: toks)

    fun printToks (strm : Lex.strm) : unit =
      case lex strm of
           (T.EOF, _, _) => printnl (T.toString T.EOF)
         | (t, _, strm) => 
             let
               val () = print (String.concat [T.toString t, " "])
             in
               printToks strm
             end
  in
    printToks strm
  end

  (*  parse parser toString strm = ().  Parses strm using the function parse,
  *   which ought to be either P.parse or P.parseE, where E is an entry point
  *   to the parser specifed in an %entry directive.  The parse is the
  *   converted to a string with toString, and the results printed to standard
  *   out.
  *)
  fun parse parser toString (strm : Lex.strm) : unit =
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

    val repair_msg =
      case repairs of
           [] => ""
         | _ => String.concatWith "\n" [
             "********* Parser reports errors *********",
             String.concatWith "\n" (
               map (AntlrRepair.repairToString T.toString sm) repairs
             ),
             "*****************************************"
           ]

    val parse_res_msg =
      case e of
           NONE => "Parse result:  NONE."
         | SOME e' => "Parse result: " ^ (toString e')
  in
    print (String.concatWith "\n" [repair_msg, parse_res_msg, ""])
  end

  fun main(arg0 : string, argv : string list) : int =
  let
    val stream = ref (Lex.streamifyInstream o TextIO.openIn)

    val parser = ref (parse P.parse Ast.programToString)

    val usage = String.concatWith "\n" [
      "driver [--lex] [--expr] [--arg] s",
      "",
      "Parse (default) or lex the contents of file s.",
      "Options:",
      "\t--lex:   lex only",
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
           "arg" => stream := (Lex.streamifyInstream o TextIO.openString)
         | "expr" => parser := (parse P.parseexp Ast.expToString)
         | "lex" => parser := lex
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
           [s] => (!parser o !stream) s
         | _ => raise doUsage
    end

    val handleOptsArgs = handleArgs o handleOpts

  in
    (handleOptsArgs argv ; 0)
    handle doUsage => (print usage ; 1)
      | e => (printnl (String.concatWith " " ["Exception: ", exnMessage e]) ; 1)
  end

end
