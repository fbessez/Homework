(*  COMP 321 Homework 5:  Type-checking for a fragment of C.
*   
*   Samuel Zhu
*   Fabien Bessez
*)

structure AnnAst =
struct

  (* The type of identifiers *)
  type id = string

  (*  You will need a type for the source language types.  Do not
  *   use the TNone constructor.
  *)
  datatype typ = Tbool | Tint | Tdouble | Tstring | Tvoid

  (*  The annotated type of expressions.
  *)
  datatype exp = EInt of int 
               | EDouble of real 
               | EString of string
               | ETrue | EFalse 
               | EId of id*typ
               | ECall of (id*(exp list))*typ
               | EPostIncr of id*typ 
               | EPostDecr of id*typ
               | ENot of exp*typ
               | EPreIncr of id*typ
               | EPreDecr of id*typ 
               | EMul of (exp*exp)*typ
               | EDiv of (exp*exp)*typ
               | EMod of (exp*exp)*typ
               | EAdd of (exp*exp)*typ
               | ESub of (exp*exp)*typ
               | ELShift of (exp*exp)*typ
               | ERShift of (exp*exp)*typ
               | ELt of (exp*exp)*typ 
               | EGt of (exp*exp)*typ
               | ELe of (exp*exp)*typ
               | EGe of (exp*exp)*typ
               | EEq of (exp*exp)*typ
               | ENeq of (exp*exp)*typ
               | EAnd of (exp*exp)*typ
               | EOr of (exp*exp)*typ
               | EAsst of (id*exp)*typ
               | ECond of (exp*exp*exp)*typ

  (*  The type of statements.
  *)
  datatype stm = SExp of exp 
               | SDecl of typ*(id list)
               | SInit of typ*((id*exp) list)
               | SReturn of exp 
               | SDoWhile of stm*exp
               | SWhile of exp*stm
               | SFor of (typ*id*exp)*exp*exp*stm
               | SBlock of stm list
               | SIf of exp*stm
               | SIfElse of exp*stm*stm

  (*  Parameters and prototypes.
  *)
  type paramdecl = typ*id
  type prototype = typ

  (*  The type of definitions.
  *)
  datatype def = DFun of typ*id*(paramdecl list)*(stm list)
               | DFunProt of typ*id*(prototype list)

  (*  The type of programs.
  *)
  datatype program = PDefs of def list

  (* Returns the typ of e the expression
  *)
  fun getType(e:exp):typ = 
    case e of 
      EInt i => Tint
      | EDouble(r) => Tdouble
      | EString(s) => Tstring
      | ETrue => Tbool
      | EFalse => Tbool
      | (EId(_, typ)| 
        ECall((_,_),typ)|
        EPostIncr(_, typ) | EPostDecr(_, typ) |
        ENot(_, typ) | EPreIncr(_, typ) | EPreDecr(_, typ) | 
        EMul(_, typ) | EDiv(_, typ) | EMod(_, typ) | EAdd(_, typ) |
        ESub(_, typ) | ELShift(_, typ) | ERShift(_, typ) | ELt(_, typ) | 
        EGt(_, typ) | ELe(_, typ) | EGe(_, typ) | EEq(_, typ) |
        ENeq(_, typ) | EAnd(_, typ) | EOr(_, typ) | EAsst(_, typ) |
        ECond(_, typ)) => typ

  (*  indentStr indent s = s', where s' is obtained from s by prefixing
  *     indent many spaces at the beginning.
  *)
  fun indentStr (indent : int) (s : string) : string =
    (implode (List.tabulate (indent, fn _ => #" ") )) ^ s


 (*  typToString t = a string representation of t *)
  fun typToString (t : typ) : string =
    case t of
         Tbool => "TBool"
       | Tint => "Tint"
       | Tdouble => "Tdouble"
       | Tvoid => "Tvoid"
       | Tstring => "Tstring"


  (*  A function to convert (annotated) expressions to
  *   strings for the driver program.
  *)
  fun expToString (indent : int) (e : exp) : string =
    let
    fun unToStr(con : string, (e, t): exp* typ) : string =
      indentStr indent (String.concat [
        con, "(", expToString 0 e, ", ", typToString t, ")"
      ])
    fun unIdToStr(con : string, e : id, t : typ) : string =
      indentStr indent (String.concat [
        con, "(", e, ", ", typToString t, ")"
      ])
    fun binToStr(con : string, e : exp, e' : exp, t : typ) : string =
      indentStr indent (String.concat [
        con, "(", expToString 0 e, ", ", expToString 0 e', ", ", typToString t, ")"
      ])
    fun binIdToStr(con : string, e : id, e' : exp, t : typ) : string =
      indentStr indent (String.concat [
        con, "(", e, ", ", expToString 0 e', ", ", typToString t, ")"
      ])
  in
    case e of
         EInt x => indentStr indent ("Eint(" ^ (Int.toString x) ^ ")")
       | EDouble x => indentStr indent ("EDouble(" ^ (Real.toString x) ^ ")")
       | EString s => indentStr indent ("EString(" ^ s ^ ")")
       (* | EChar c => indentStr indent ("EChar(" ^ (Char.toString c) ^ ")") *)
       | ETrue => indentStr indent "ETrue"
       | EFalse => indentStr indent "EFalse"
       | EId (id, t) =>
           indentStr indent ("EId(" ^ id ^ ")")
       | ECall((e, es), t) => indentStr indent (String.concat [
           "ECall(", 
           e, 
           ListFormat.listToString (expToString 0) es, 
           ")"
         ])
       | EPostIncr (e, t) => unIdToStr("EPostIncr", e, t)
       | EPostDecr (e, t) => unIdToStr("EPostDecr", e, t)
       | ENot (e, t) => unToStr ("ENot", (e, t))
       | EPreIncr (e, t) => unIdToStr("EPreIncr", e, t)
       | EPreDecr (e, t) => unIdToStr("EPDecr", e, t)
       | EMul((e, e'), t) => binToStr("EMul", e, e', t)
       | EDiv((e, e'), t) => binToStr("EDiv", e, e', t)
       | EMod((e, e'), t) => binToStr("EMod", e, e', t)
       | EAdd((e, e'), t) => binToStr("EAdd", e, e', t)
       | ESub((e, e'), t) => binToStr("ESub", e, e', t)
       | ELShift((e, e'), t) => binToStr("ELShift", e, e', t)
       | ERShift((e, e'), t) => binToStr("ELShift", e, e', t)
       | ELt ((e, e'), t) => binToStr("ELt", e, e', t)
       | EGt ((e, e'), t) => binToStr("EGt", e, e', t)
       | ELe ((e, e'), t) => binToStr("ELe", e, e', t)
       | EGe ((e, e'), t) => binToStr("EGe", e, e', t)
       | EEq((e, e'), t) => binToStr("EEq", e, e', t)
       | ENeq((e, e'), t) => binToStr("ENeq", e, e', t)
       | EAnd((e, e'), t) => binToStr("EAnd", e, e', t)
       | EOr((e, e'), t) => binToStr("EOr", e, e', t)
       | EAsst((e, e'), t) => binIdToStr("EAsst", e, e', t)
       | ECond((e, e0, e1), t) => indentStr indent (String.concat [
           "ECond(",
           expToString 0 e,
           ", ",
           expToString 0 e0,
           ", ",
           expToString 0 e1,
           ")"
         ])  end

  (*  stmToString indent s = a string representation of s, indented by
  *     indent-many spaces.
  *)
  fun stmToString (indent : int) (s : stm) : string =
    case s of
         SExp exp =>
           String.concatWith "\n" [
             indentStr indent "SExp(",
             expToString (indent+2) exp,
             indentStr indent ")"
           ]
       | SDecl (qty, ids) =>
           indentStr indent (String.concat [
             "SDecl(", typToString qty, ", ",
             ListFormat.listToString String.toString ids, ")"
           ])
       | SInit(qty, assns) =>
           indentStr indent (String.concat [
             "SInit(", typToString qty, ", ",
             ListFormat.listToString 
               (fn (i, e) => "(" ^ i ^ ", " ^ (expToString 0 e) ^ ")")
               assns,
             ")"
           ])
       | SReturn exp =>
           String.concatWith "\n" [
             indentStr indent "SReturn(",
             expToString (indent+2) exp,
             indentStr indent ")"
           ]
       | SDoWhile(s, e) =>
           String.concatWith "\n" [
             indentStr indent "SDoWhile(",
             stmToString (indent+2) s,
             ",",
             expToString (indent+2) e,
             ")"
           ]
       | SWhile(e, s) =>
           String.concatWith "\n" [
             indentStr indent ("SWhile(" ^ (expToString 0 e) ^ ","),
             stmToString (indent+2) s,
             ")"
           ]
       | SFor((ty,id,e), e0, e1, s) =>
           String.concatWith "\n" [
             indentStr indent (
               "SFor(" ^ (typToString ty) ^ " " ^ id ^ "=" ^ 
               (expToString 0 e) ^ "; " ^
               (expToString 0 e0) ^ "; " ^
               (expToString 0 e1) ^ ","
             ),
             stmToString (indent+2) s,
             ")"
           ]
       | SBlock(stms) =>
           String.concatWith "\n" ([
             indentStr indent "SBlock("
           ] @ (map (stmToString (indent+2)) stms) @ [indentStr indent ")"])
       | SIf (exp, stm) =>
           String.concatWith "\n" [
             indentStr indent ("SIf(" ^ (expToString 0 exp) ^ ", "),
             stmToString (indent+2) stm,
             indentStr indent ")"
           ]
       | SIfElse (exp, stm, stm') =>
           String.concatWith "\n" [
             indentStr indent ("SIfElse(" ^ (expToString 0 exp) ^ ", "),
             stmToString (indent+2) stm,
             indentStr indent "-",
             stmToString (indent+2) stm',
             indentStr indent ")"
           ]

  fun paramDeclToString (indent : int) ((ty, id) : paramdecl) : string =
    indentStr indent (
      (typToString ty) ^ ", " ^ id
    )

  (*  defToString indent defn = a string representation of defn indented by
  *     indent-many spaces.
  *)
  fun defToString (indent : int) (defn : def) : string =
    case defn of
         DFun(t, i, argdecls, stms) =>
           String.concatWith "\n" [
             indentStr indent "DFun(",
             indentStr (indent+2) (typToString t),
             indentStr (indent+2) i,
             String.concatWith "\n" (map (paramDeclToString (indent+2)) argdecls),
             String.concatWith "\n" (map (stmToString (indent+2)) stms),
             indentStr indent ")"
           ]
       | DFunProt(t, i, tys) =>
           String.concatWith "\n" [
             indentStr indent "DFunProt(",
             indentStr (indent+2) (typToString t),
             indentStr (indent+2) i,
             String.concatWith "\n" (map typToString tys),
           
             indentStr indent ")"
            ]

    val expToString = expToString 0

    (*  programToString defs = a string representation of defs.
    *)
    fun programToString(PDefs defs : program) : string =
      String.concatWith "\n" (map (defToString 0) defs)


end
