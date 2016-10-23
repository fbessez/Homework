(*  COMP 321 Homework 2:  ML-ulex specification.
*   
*   Token definition.
*
*   Fall 2016
*)

structure NumsTokens =
struct

  (*  The type of tokens.
  *)
  datatype token = Num of int | EOF

end
