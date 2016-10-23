(*  COMP 321 Homework 1:  Tests.
*
*   Fabien Bessez
*)

structure Tests =
struct

  structure U = UnitTest
  
  structure TR = TestRunner

  (*  An implementation of PRI_KEY where the key type is string, and
  *   strings are ordered by length.
  *)
  structure SK : PRI_KEY =
  struct
    type key = string
    val pri = String.size
  end

  (*  A structure for priority queues over strings, where the priority
  *   of a string is its length.
  *)
  structure Q = ListPQFn(SK)

  (*  Convenience abbreviation.
  *)
  type key = SK.key


  (*  insertMany (q, [y_0,...,y_{n-1}]) =
  *     Q.insert(...Q.insert(Q.insert(q, y_0), y_1)...,y_{n-1}).
  *
  *   In other words, insertMany(q, ys) is the result of inserting the
  *   elements of ys into q in order.
  *)
  fun insertMany(q : Q.pq, ss : string list) : Q.pq =
    case ss of
         [] => q
       | s :: ss => insertMany(Q.insert(q, s), ss)

  (*  insertAll ss = insertMany (Q.empty, ss).
  *)
  fun insertAll(ss : string list) : Q.pq =
    insertMany(Q.empty, ss)

  (*  testQ(ss, exp) is a test that succeeds if insertAll ss = exp.
  *)
  fun testQ(ss : string list, exp : string list) : U.test =
    U.assertEq(
      ListFormat.listToString String.toString ss,
      fn () => insertAll ss,
      exp,
      ListFormat.listToString String.toString
    )

  fun isIn (x, nil) = false 
    | isIn (x, y::ys) = x=y orelse isIn (x,ys)

  (*  tests is the list of test suites that will be run.
  string * (UnitTest.t list) list
  *)
  val tests = [
    ("insert, unique priorities", [
      testQ(["a"], ["a"]),
      testQ(["abc", "ab", "a"], ["a", "ab", "abc"])
    ]),    
    ("insert, duplicate priorities", [
      U.assertPred(
        ListFormat.listToString String.toString ["zz", "x", "yy", "w"],
        fn () => insertAll(["zz", "x", "yy", "w"]),
        fn pq => isIn(pq, [["w", "x", "yy", "zz"], 
                          ["x", "w", "yy", "zz"],
                          ["x", "w", "zz", "yy"],
                          ["w", "x", "zz", "yy"]]),
        ListFormat.listToString String.toString)]),
    ("peek, unique priorities", [
      U.assertEq(
        ListFormat.listToString String.toString ["a", "bc", "def"],
        fn () => Q.peek(insertAll(["a", "bc", "def"])),
        "a",
        String.toString)]),
    ("peek, duplicate priorities", [
      U.assertPred(
      ListFormat.listToString String.toString ["x", "w", "yy", "zz"],
      fn () => Q.peek(["x", "w", "yy", "zz"]),
      fn exp => isIn(exp, ["x", "w"]),
      String.toString)]),
    ("delmin, unique priorities", [
      U.assertPred("delMin unique priorities",
        fn () => Q.delMin (insertAll(["x", "yy"])),
        fn (min, pq) => isIn(pq, [["yy"]]),
        fn pq => "error" )
        ]),
    ("delmin, duplicate priorities", [
      U.assertPred("duplicate priorities",
        fn () => Q.delMin (insertAll(["x", "y", "zz"])),
        fn (min, pq) => isIn((min, pq), [("x", ["y", "zz"]),
                                          ("y", ["x", "zz"])]),
        fn pq => "error")]),
    ("isEmpty", [
      U.assertPred("isEmpty on empty list",
        fn () => Q.isEmpty([]), 
        fn (bool) => if bool = true then true else false,
        fn (bool) => "error")]),
    ("isEmpty", [
      U.assertPred("isEmpt on non-empty list", 
        fn () => Q.isEmpty(["x"]), 
        fn (bool) => if bool = false then true else false,
        fn (bool) => "error" )])]


  (*  Do not change this function definition.
  *)
  fun main(arg0 : string, argv : string list) : int =
    TR.runTimedTestSuites (tests, 60, true)
end
