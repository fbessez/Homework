(*  Tests for COMP 321 Homework 3
*)

structure Tests =
struct

  structure U = UnitTest
  structure TR = TestRunner

  structure CPPParser = CPPGrmParseFn(CPPLexer)

  (*  checkFileGood(filename) = t, where t is a test that succeeds if the
  *   program in filename parses successfully.
  *)
  fun checkFileGood parser toString (filename : string) : U.test =
  let
    val name = filename

    val sm = AntlrStreamPos.mkSourcemap()
    val lex = CPPLexer.lex sm

    fun parse() =
    let
      val ins = TextIO.openIn filename
      val strm = CPPLexer.streamifyInstream ins
    in
      parser lex strm before TextIO.closeIn ins
    end
  in
    U.seqTest(name, [
      (*  Error string must be non-empty.
      *)
      U.assertNil(name ^ " (no errors)",
        fn () => #3(parse()),
        (ListFormat.listToString String.toString) o 
          (map (AntlrRepair.repairToString CPPGrmTokens.toString sm))),

      (*  Parse must not return NONE.
      *)
      U.assertSomeOption(name ^ " (parses)",
        fn () => let val (x, _, _) = parse() in x end,
        toString)
      ])
  end

  (*  CheckFileBad(filename) = t, where t is a test that succeeds if the
  *   program in filename does not parse successfully.
  *)
  fun checkFileBad parser (filename : string) : U.test =
  let
    val name = filename

    val sm = AntlrStreamPos.mkSourcemap()
    val lex = CPPLexer.lex sm

    fun parse() =
    let
      val ins = TextIO.openIn filename
      val strm = CPPLexer.streamifyInstream ins
    in
      parser lex strm before TextIO.closeIn ins
    end
  in
    U.orTest(name, [
      (* Get some exception (probably from lexing).
      *)
      U.assertExn(
        name ^ " (parsing raises exception)", 
        fn () => parse(),
        Fail ""
      ),

      (* Error string is non-empty.
      *)
      U.assertNotNil(name ^ " (has errors)", 
        fn () => let val (_, _, errs) = parse() in errs end),

      (*  Parse returns NONE.
      *)
      U.assertNoneOption(name ^ " (does not parse)", 
        fn () => let val (x, _, _) = parse() in x end)
    ])
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

  fun checkGoodExprTests() =
    ("Check expression parseability",
     map (checkFileGood CPPParser.parseexp Ast.expToString)
         (cFiles "testfiles/expr/good")
    )

  fun checkBadExprTests() =
    ("Check expression non-parseability",
     map (checkFileBad CPPParser.parseexp)
         (cFiles "testfiles/expr/bad")
    )

  fun checkGoodProgramTests () =
    ("Check parseability", 
    map (checkFileGood CPPParser.parse Ast.programToString) 
        (cFiles "testfiles/programs/good"))

  fun checkBadProgramTests() =
    ("Check non-parseability", 
    map (checkFileBad CPPParser.parse) (cFiles "testfiles/programs/bad"))

  fun allTests () = [
    checkGoodExprTests(),
    checkBadExprTests(),
    checkGoodProgramTests(),
    checkBadProgramTests()
  ]

  fun main(arg0 : string, argv : string list) : int =
    TR.runTimedTestSuites (allTests (), 60, true)

  fun runTests() = main("", [])

end
