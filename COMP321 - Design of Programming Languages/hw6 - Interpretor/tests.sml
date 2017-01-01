(*  Tests for COMP 321 Homework 6:  interpreter for a fragment of C
*
*   N. Danner
*   Fall 2016
*)

structure Tests =
struct

  structure U = UnitTest
  structure TR = TestRunner

  structure CPPParser = CPPGrmParseFn(CPPLexer)
  structure Tok = CPPGrmTokens

  structure T = TextIO

  exception BadParse of string

  fun parse filename =
  let
    val sm = AntlrStreamPos.mkSourcemap()
    val lex = CPPLexer.lex sm
    val ins = TextIO.openIn filename
    val strm = CPPLexer.streamifyInstream ins
    val (e, strm, repairs) = CPPParser.parse lex strm
  in
    case (repairs, lex strm, e) before TextIO.closeIn ins of
         ([], (Tok.EOF, _, _), SOME e') => e'
       | ([], (Tok.EOF, _, _), NONE) => raise BadParse "parse returns NONE"
       | ([], (_, _, _), _) => raise BadParse "extra tokens after parse"
       | (_, _, _) => raise BadParse "parse makes repairs"
  end

  (*
  (*  checkFileGood(filename) = t, where t is a test that succeeds if the
  *   program in filename parses successfully.
  *)
  fun checkFileGood parser checker toString (filename : string) : U.test =
    U.assertNothing(filename, fn () => checker(parse parser filename))

  (*  CheckFileBad(filename) = t, where t is a test that succeeds if the
  *   program in filename does not parse successfully.
  *)
  fun checkFileBad parser checker excep filename : U.test =
    U.assertExn(filename, fn () => checker(parse parser filename), excep)
  *)

  (*  checkExecPgm f = t, where t is a test that succeeds if 
  *   Interp.exec p produces the output in f ^ ".output" when it is
  *   given input  in f ^ ".input".  
  *
  *   Side-effect:  output of p is in f ^ ".results".
  *
  *   See assignment description for details.
  *)
  fun checkExecPgm (filename : string) : U.test =
  let
    val fileIn = filename ^ ".input"
    val fileOut = filename ^ ".results"
    val fileExp = filename ^ ".output"

    fun readFile(filename : string) : string =
    let
      val ins = T.openIn filename
      val s = Substring.string (
        Substring.dropr Char.isSpace (Substring.full (T.inputAll ins))
      )
      val () = T.closeIn ins
    in
      s
    end

    fun test() : string =
    let
      val ins = T.openIn fileIn
      val outs = T.openOut fileOut

      val () = IoBase.setIO (ins, outs) 
      val ret = Interp.exec(Typing.checkPgm(parse(filename)))
      val () = T.closeIn ins
      val () = T.closeOut outs

      val res = readFile fileOut
    in
      res
    end

    val exp = readFile fileExp
  in
    U.assertEqStr( filename, test, exp)
  end

  (*  assertExecError e f = t, where t is a test that succeeds if
  *   Interp.exec p raises e when the program reads input from
  *   f ^ ".input".
  *
  *   Side-effect:  output of p is in f ^ ".results".
  *
  *   See assignment description for details.
  *)
  fun assertExecError (e : exn) (filename : string) : U.test =
  let
    val name = filename

    val fileIn = filename ^ ".input"
    val fileOut = filename ^ ".results"
    val fileExp = filename ^ ".output"

    fun readFile(filename : string) : string =
    let
      val ins = T.openIn filename
      val s = T.inputAll ins
      val () = T.closeIn ins
    in
      s
    end

    fun test() : string =
    let
      val ins = T.openIn fileIn
      val outs = T.openOut fileOut

      val () = IoBase.setIO (ins, outs) 
      val _ = Interp.exec(Typing.checkPgm(parse(filename)))
      val () = T.closeIn ins
      val () = T.closeOut outs

      val res = readFile fileOut
    in
      res
    end
  in
    U.assertExn (
      name,
      test,
      e
    )
  end

  (*  cFiles d = a list of all files with suffix .cc contained in
  *   any recursive subdirectory of d.
  *)
  fun cFiles (d : string)  : string list =
  let
    val ds : OS.FileSys.dirstream = OS.FileSys.openDir d

    fun files (curdir : string) (ds : OS.FileSys.dirstream) : string list =
    let
    in
      case OS.FileSys.readDir ds of
           NONE => []
         | SOME f =>
             let
               val full_f = curdir ^ "/" ^ f
             in
               if OS.FileSys.isDir full_f 
               then (cFiles full_f) @ (files curdir ds)
               else full_f :: (files curdir ds)
             end
    end
  in
    List.filter (String.isSuffix ".cc") (files d ds)
  end
  handle SysErr => []

  fun checkGoodProgramTests () =
    ("Check program execution", 
    map 
      checkExecPgm (cFiles "testfiles/good"))

  fun checkNoMainTests () =
    ("Check NoMainError",
    map (assertExecError Interp.NoMainError) (cFiles "testfiles/bad/nomain"))

  fun checkNoRetTests () =
    ("Check NoReturnError",
    map (assertExecError Interp.NoReturnError) (cFiles "testfiles/bad/noret"))

  fun checkUninitTests () =
    ("Check UninitializedError",
    map 
      (assertExecError Interp.UninitializedError) 
      (cFiles "testfiles/bad/uninit"))

      (*
  fun checkUndecProgramTests () =
    (
      "Check UndeclaredError",
      map (
        checkFileBad 
        CPPParser.parse 
        Typing.checkPgm
        (Typing.UndeclaredError "")
      ) (cFiles "testfiles/programs/bad/undec")
    )

  fun checkMultProgramTests () =
    (
      "Check MultiplyDeclaredError",
      map (
        checkFileBad 
        CPPParser.parse 
        Typing.checkPgm
        (Typing.MultiplyDeclaredError "")
      ) (cFiles "testfiles/programs/bad/mult")
    )

  fun checkReturnTypeProgramTests () =
    (
      "Check ReturnTypeError",
      map (
        checkFileBad 
        CPPParser.parse 
        Typing.checkPgm
        Typing.ReturnTypeError
      ) (cFiles "testfiles/programs/bad/ret")
    )

  fun checkTypeProgramTests () =
    (
      "Check TypeError",
      map (
        checkFileBad 
        CPPParser.parse 
        Typing.checkPgm
        Typing.TypeError
      ) (cFiles "testfiles/programs/bad/type")
    )
    *)

  fun allTests () = [
    checkGoodProgramTests(),
    checkNoMainTests(),
    checkNoRetTests(),
    checkUninitTests()
  ]

  fun main(arg0 : string, argv : string list) : int =
  let
    val _ = TR.runTimedTestSuites (allTests (), 60, true)
  in
    0
  end


  fun runTests() = main("", [])

end
