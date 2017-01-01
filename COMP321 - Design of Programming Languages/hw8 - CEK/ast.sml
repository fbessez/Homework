(* Abstract syntax trees for simple applicative language.
*)
structure Ast =
struct

  type ident = string
  datatype exp = Ident of ident | Num of int | Str of string | Bool of bool
    | Plus of exp*exp | Minus of exp*exp | Times of exp*exp | Div of exp*exp
    | Lt of exp*exp | Le of exp*exp | Gt of exp*exp | Ge of exp*exp
    | Eq of exp*exp | Ne of exp*exp 
    | Orelse of exp*exp | Andalso of exp*exp | Not of exp
    | Cat of exp*exp | Len of exp
    | Cond of exp*exp*exp
    | Proj1 of exp | Proj2 of exp | Pair of exp*exp
    | App of exp*exp | Lambda of ident*exp
    | Let of ident*exp*exp
    | Letrec of ident*exp*exp

  (*  The type of program declarations.
  *)
  datatype dec = Val of ident*exp
                 | Valrec of ident*exp

  (*  The type of programs.
  *)
  datatype pgm = Program of (dec list)

    
  (* expToString : t -> string
  *  expToString e is the string representation of ASTs. 
  *)
  local
    fun wrap s = concat ["(", s, ")"]
  in
    fun binop s e1 e2 = concat [expToString e1, s, expToString e2]
    and expToString(Ident(x)) = x
      | expToString(Num n) = Int.toString n
      | expToString(Str s) = concat ["\"", s, "\""]
      | expToString(Bool b) = Bool.toString b
      | expToString(App(rator, rand)) =
          concat ["(", expToString rator, "@", expToString rand, ")"]
      | expToString(Lambda(x, e)) =
          concat ["( fn ", x, " => ", expToString e, ")"]
      | expToString(Let(x, e1, e2)) =
          concat ["( let ", x, " be ", expToString e1, " in ", expToString e2, 
                  " end)"]
      | expToString(Plus(e1, e2)) =
          concat ["(", expToString e1, " + ", expToString e2, ")"]
      | expToString(Minus(e1, e2)) =
          concat ["(", expToString e1, " - ", expToString e2, ")"]
      | expToString(Times(e1, e2)) =
          concat ["(", expToString e1, "*", expToString e2, ")"]
      | expToString(Div(e1, e2)) =
          concat ["(", expToString e1, "/", expToString e2, ")"]
      | expToString(Eq(e1, e2)) = wrap (binop "=" e1 e2)
      | expToString(Ne(e1, e2)) = wrap (binop "<>" e1 e2)
      | expToString(Lt(e1, e2)) = wrap (binop "<" e1 e2)
      | expToString(Le(e1, e2)) = wrap (binop "<=" e1 e2)
      | expToString(Gt(e1, e2)) = wrap (binop ">" e1 e2)
      | expToString(Ge(e1, e2)) = wrap (binop ">=" e1 e2)
      | expToString(Cat(e1, e2)) =
          concat ["(", expToString e1, "^", expToString e2, ")"]
      | expToString(Andalso(e1, e2)) =
          concat ["(", expToString e1, " andalso ", expToString e2, ")"]
      | expToString(Orelse(e1, e2)) =
          concat ["(", expToString e1, " orelse ", expToString e2, ")"]
      | expToString(Not(e)) = concat ["not(", expToString e, ")"]
      | expToString(Len(e)) =
          concat ["[", expToString e, "]"]
      | expToString(Cond(e1, e2, e3)) =
          String.concatWith " " 
          ["if", expToString e1, "then", expToString e2, "else", expToString e3, "fi"]
      | expToString(Pair(e1, e2)) = 
          concat ["(", expToString e1, ", ", expToString e2, ")"]
      | expToString(Proj1(e)) = concat ["#1(", expToString e, ")"]
      | expToString(Proj2(e)) = concat ["#2(", expToString e, ")"]
  end

  (*  declToString d = s, where s is a string representation of the
  *   declaration d.
  *)
  fun declToString (Val(x, e)) =
        String.concatWith " " ["val", x, "=", expToString e]
    | declToString (Valrec(x, e)) =
        String.concatWith " " ["val rec", x, "=", expToString e]

  (*  toString pgm = s, where s is a string representation of the program pgm.
  *)
  fun pgmToString (Program(ds)) =
    String.concatWith ";" (map declToString ds)

end
