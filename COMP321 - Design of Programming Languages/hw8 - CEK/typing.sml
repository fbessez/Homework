structure Type =
struct

  datatype t = Num | Str

  fun toString Num = "num"
    | toString Str = "str"

  structure Ast = Ast

  (*  A type_context is a partial map from identifiers to types.
  *   Partiality is implemented by having range type t option;
  *   Gamma(x) = NONE means that Gamma is undefined on x.
  *)
  type type_context = Ast.ident -> t option

  (*  empty represents the empty type context.
  *)
  val empty : type_context = fn _ => NONE

  (*  update G x sigma = G', where
  *     G' y = sigma, if y = x
  *          = G y,   if y <> x
  *)
  fun update G x sigma =
    fn y => if x = y then SOME sigma else G y

  (*  infer t = NONE if no type can be assigned to t.
  *             SOME sigma if _ |- t : sigma.
  *)
  local

    (*  infer' G t = NONE if no type can be assigned to t.
    *                SOME(sigma) if G |- t : sigma.
    *   Pre-condition:  fv(t) is contained in dom(G).
    *)
    fun infer' G (Ast.Ident x) =  G x
      | infer' G (Ast.Num m) = SOME Num
      | infer' G (Ast.Str s) = SOME Str
      | infer' G (Ast.Len e) =
        (
        case infer' G e of
             SOME Str => SOME Num
           | _ => NONE
        )
      | infer' G (Ast.Plus (e1, e2) | Ast.Minus(e1, e2) | 
                  Ast.Times(e1, e2) | Ast.Div (e1, e2) ) =
        (
        case (infer' G e1, infer' G e2) of
             (SOME Num, SOME Num) => SOME Num
           | _ => NONE
        )
      | infer' G (Ast.Cat (e1, e2)) =
        (
        case (infer' G e1, infer' G e2) of
             (SOME Str, SOME Str) => SOME Str
           | _ => NONE
        )
      | infer' G (Ast.Let(x, e1, e2)) =
        let
          val tau1 = infer' G e1
        in
          case tau1 of
               SOME sigma => infer' (update G x sigma) e2
             | NONE => NONE
        end

  in

    fun infer e = infer' empty e

  end

end
