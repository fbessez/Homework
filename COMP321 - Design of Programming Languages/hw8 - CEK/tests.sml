(*  Tests for COMP 212 Homework 8: Interpreter for a higher-order language
*
*   N. Danner
*)

structure Tests =
struct

  structure U = UnitTest
  structure TR = TestRunner

  structure E = Interp

  structure Ast = Ast
  structure MLMinusParser = MLMinusParseFn(MLMinusLexer)

  exception parse_error of string

   
  (*  Test whether test() = exp : Ast.t, where 
  *   test() = value2ast (eval_expr ast) and
  *   ast is the AST corresponding to expr.
  *)
  fun assertEqVal(expr, exp) =
  let
    val name = expr

    fun test () = 
    let
      val sm = AntlrStreamPos.mkSourcemap()
      val expr_lex = MLMinusLexer.lex sm
      val expr_strm = MLMinusLexer.streamifyInstream (TextIO.openString expr)
      val (expr_ast, _, errs) = MLMinusParser.parseexp expr_lex expr_strm
    in
      case (expr_ast, errs) of
           (SOME e, []) => Interp.value2ast (Interp.evalExp e)
         | _ =>
             raise Fail ("Unable to parse: " ^
               (ListFormat.listToString 
                 (AntlrRepair.repairToString MLMinusTokens.toString sm)
                 errs))
    end

    val exp_sm = AntlrStreamPos.mkSourcemap()
    val exp_lex = MLMinusLexer.lex exp_sm
    val exp_strm = MLMinusLexer.streamifyInstream (TextIO.openString exp)
    val (exp_val, _, _) = MLMinusParser.parseexp exp_lex exp_strm
  in
    U.assertEq(name, test, valOf exp_val, Ast.expToString)
  end

  val exprTests = ("Expression evaluation",
    [
      assertEqVal("3 = 3", "true"),
      assertEqVal("3 <> 3", "false"),
      assertEqVal("3 < 5", "true"),
      assertEqVal("3 <= 3", "true"),
      assertEqVal("3 > 5", "false"),
      assertEqVal("3 >= 1", "true"),
      assertEqVal("(5 + 2) * 4", "28"),
      assertEqVal("((5 + 2) * 4) - 3", "25"),

      assertEqVal("true andalso false", "false"),
      assertEqVal("true orelse false", "true"),
      assertEqVal("not true", "false"),

      assertEqVal("if true then 0 else 1 fi", "0"),
      assertEqVal("if 3 > 1 then true else false fi", "true"),
      assertEqVal("if 3 < 1 then true else false fi", "false"),
      assertEqVal("if 3 > 1 then 2 else 3 + 4 fi", "2"),
      assertEqVal("let x = 3 in (fn z => 2 + z end) x end", "5"),

      assertEqVal("let rec f = fn x => x end in f 3 end", "3"),
      assertEqVal("let rec f = fn x => if x = 0 then 0 else x + f (x-1) fi end in f 4 end", "10"),
      assertEqVal("(3, 5)", "(3, 5)"),
      assertEqVal("#1(3, 5)", "3"),
      assertEqVal("#2(3, 5)", "5"),
      assertEqVal("let x = 3 in x + 2 end", "5"), 
      assertEqVal("(fn z => #1(z) + #2(z) end)(3, 5)", "8"),
      assertEqVal("(fn x => 3 end) 0", "3")

    ]
  )

  fun assertEqValPgm(pgm, exp) =
  let
    val name = String.concatWith " " ["Eval program", pgm]
    fun test() =
    let
      val pgm = "testfiles/" ^ pgm ^ ".mlm"
      val sm = AntlrStreamPos.mkSourcemap()
      val expr_lex = MLMinusLexer.lex sm
      val expr_strm = MLMinusLexer.streamifyInstream (TextIO.openIn pgm)
      val (expr_ast, _, errs) = MLMinusParser.parse expr_lex expr_strm
    in
      case (expr_ast, errs) of
           (SOME e, []) => Interp.value2ast (Interp.evalPgm (valOf expr_ast))
         | _ =>
             raise Fail ("Unable to parse: " ^
               (ListFormat.listToString 
                 (AntlrRepair.repairToString MLMinusTokens.toString sm)
                 errs))
    end

    val exp_sm = AntlrStreamPos.mkSourcemap()
    val exp_lex = MLMinusLexer.lex exp_sm
    val exp_strm = MLMinusLexer.streamifyInstream (TextIO.openString exp)
    val (exp_val, _, _) = MLMinusParser.parseexp exp_lex exp_strm
  in
    U.assertEq(name, test, valOf exp_val, Ast.expToString)
  end

  val pgmTests = ("Program evaluation", [
    assertEqValPgm("t5", "144")
  ])


  val all_tests = [exprTests, pgmTests]

  fun main(arg0 : string, argv : string list) : int =
    TR.runTimedTestSuites (all_tests, 30, true)
    handle e => (print ((exnMessage e) ^ "\n") ; 1)


end
