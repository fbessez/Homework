(*  COMP 321 Homework 1:  Functors.
*   
*  
*)

(*  A structure for keys with a priority function.
*)
signature PRI_KEY =
sig

  (*  The type of keys in a priority queue.
  *)
  type key

  (*  pri(x) = the priority of x.
  *)
  val pri : key -> int

end

