(*  Tests for COMP 321 Homework 5:  type-checking a fragment of C
*
*   N. Danner
*   Fall 2016
*)

structure Tests =
struct

  structure U = UnitTest
  structure TR = TestRunner

  structure CPPParser = CPPGrmParseFn(CPPLexer)
  structure T = CPPGrmTokens

  exception BadParse of string

  fun parse parser filename =
  let
    val sm = AntlrStreamPos.mkSourcemap()
    val lex = CPPLexer.lex sm
    val ins = TextIO.openIn filename
    val strm = CPPLexer.streamifyInstream ins
    val (e, strm, repairs) = parser lex strm
  in
    case (repairs, lex strm, e) before TextIO.closeIn ins of
         ([], (T.EOF, _, _), SOME e') => e'
       | ([], (T.EOF, _, _), NONE) => raise BadParse "parse returns NONE"
       | ([], (_, _, _), _) => raise BadParse "extra tokens after parse"
       | (_, _, _) => raise BadParse "parse makes repairs"
  end

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

  fun checkGoodExprTests() =
    ("Check expression typeability",
     map (
       checkFileGood 
       CPPParser.parseexp 
       Typing.inferExpNoEnv 
       AnnAst.expToString
     ) (cFiles "testfiles/expr/good")
    )

  fun checkGoodProgramTests () =
    ("Check program typeability", 
    map 
      (checkFileGood CPPParser.parse Typing.checkPgm AnnAst.programToString)
      (cFiles "testfiles/programs/good"))

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

  fun allTests () = [
    checkGoodExprTests(),
    checkGoodProgramTests(),
    checkMultProgramTests(),
    checkUndecProgramTests(),
    checkReturnTypeProgramTests(),
    checkTypeProgramTests()
  ]

  fun main(arg0 : string, argv : string list) : int =
  let
    val _ = TR.runTimedTestSuites (allTests (), 60, true)
  in
    0
  end


  fun runTests() = main("", [])

end
