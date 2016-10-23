(*  COMP 321 Homework 3:  Lexer and parser for a fragment of C.
*
*   Abstract syntax definitions.
*   
*   By: Ping-Jung Liu, Fabien Bessez
*
*  
*   Fall 2016
*)

structure Ast =
struct

  (*  The type of expressions.
  *   
  *   You must have this type in your solution.  Do not use the ENone
  *   constructor; it is here solely so that this skeleton file compiles.
  *)
  datatype exp = ENum of int
               | EDouble of real
               | EBool of string
               | EPlus of exp*exp
               | ETimes of exp*exp
               | ELess of exp*exp
               | EGreat of exp*exp
               | EEqual of exp*exp
               | EId of string
               | EString of string
               | EMinus of exp*exp
               | EDivide of exp*exp
               | EMod of exp*exp
               | EDisjunction of exp*exp
               | EConjunction of exp*exp
               | ERShift of exp*exp
               | ELShift of exp*exp
               | ENegation of exp
               | EDecrement of exp
               | EIncrement of exp
               | ETernary of exp*exp*exp

  (*  The type of programs.
  *   
  *   You must have this type in your solution.  Do not use the PNone
  *   constructor; it is here solely so that this skeleton file compiles.
  *)
  type id = string

  type ty = string

  datatype statement = SInit of ty * (id * exp) list
                     | SDecl of ty * id list
                     | SReg  of id * exp
                     | SDW   of statement * exp
                     | SFor  of statement * statement * exp * exp
                     | SIf   of statement * exp
                     | SIfEl of statement * statement * exp
                     | SPrint of exp
                     | SReturn of exp

  datatype decl =  DInit of ty * (id * exp) list
                 | DDecl of ty * id list
                 | FDefi of ty * id * (ty * id) list * statement list
                 | FPro  of ty * id * (ty * id) list
                 | FProo of ty * id * ty list
  
  datatype program =  PPro of decl list



  (*  ********
  *   'a -> string conversion functions.
  *)

  (*  expToString e = a string representation of e.
  *   
  *   This function is used by the driver program to print the result of
  *   parsing an expression.
  *)
  fun expToString (e : exp) : string =
    case e of
         ENum(n) =>
                "ENum(" ^ (Int.toString n) ^ ")"
       | EPlus(e0, e1) =>
                "EPlus(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ") "
       | ETimes(e0, e1) =>
                "ETimes(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ") "
       | EId(s) =>
                "EId(" ^ s ^ ") "        
       | ELess(e0, e1) => 
                "ELess(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ") "
       | EGreat(e0, e1) =>
                "EGreat(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ") "
       | EEqual(e0, e1) => 
                "EEqual(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ") "
       | EDouble(n) => 
                "EDouble(" ^ (Real.toString n) ^ ")"
       | EBool(b) => 
                "EBool(" ^ (b) ^ ")"
       | EString(s) => 
                "EString(" ^ (s) ^ ")"
       | EMinus(e0, e1) =>
                "EMinus(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ") "
       | EDivide(e0, e1) =>
                "EDivide(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ") "
       | EConjunction(e0, e1) => 
                "EConjunction(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^
                ") "
       | EDisjunction(e0, e1) =>
                "EDisjunction(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^
                ") "
       | ELShift(e0, e1) => 
                "ELShift(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ") "
       | ERShift(e0, e1) => 
                "ERShift(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ") "
       | EMod(e0, e1) =>
                "EMod(" ^ (expToString e0) ^ ", " ^ (expToString e1) ^ ") "
       | EIncrement(e0) => 
                "EIncrement(" ^ (expToString e0) ^ ") "
       | EDecrement(e0) =>
                "EDecrement(" ^ (expToString e0) ^ ") "
       | ENegation(e0) => 
                "ENegation(" ^ (expToString e0) ^ ") "
       | ETernary(e0, e1, e2) => 
                "ETernary(" ^ (expToString e0) ^ "?" ^ (expToString e1) ^ ":" ^
                (expToString e2) ^ ") "

  (*  programToString p = a string representation of p.
  *   
  *   This function is used by the driver program to print the result of
  *   parsing a program.
  *)
  fun initToString (initl : ty*(id*exp) list) : string =
  let val (ty, lll) = initl in 
    case lll of
         [] => ""
       | x::xs => let val (id, exp) = x in 
                        ty ^ " " ^ id ^ " = " ^ (expToString exp) ^ " " ^
                        initToString(ty, xs)
                  end
  end
  fun declToString (idl: ty * id list) : string =
  let val (ty, lll) = idl in
    case lll of
         [] => ""       
       | x::xs => let val id = x in
                        ty ^ " " ^ id ^ " " ^ declToString(ty, xs) 
                  end
  end

  fun finputToString (inputl: (ty * id) list) : string = 
    case inputl of
         [] => ""
       | x::xs => let val(ty, id) = x in
                        ty ^ " " ^ id ^ " " ^ finputToString(xs)
                  end
  
  fun typelistToString (tyl : ty list) : string =
    case tyl of
         [] => ""
       | x::xs => x ^ " " ^ typelistToString(xs)
                  
  fun statementToString (s : statement) : string =
                        case s of
                                SDecl(id) => declToString(id) ^ "\n" 
                              | SInit(initl) =>
                                  initToString(initl) ^ "\n"
                              | SDW(s, condi) => 
                                  "DO " ^ statementToString(s) ^ "\n" ^ "WHILE " ^ (expToString condi) ^ "\n" 
                              | SFor(s, init, condi,step) =>
                                  "FOR(" ^ (statementToString init) ^ (expToString
                                  condi) ^ (expToString step) ^ "\n" ^
                                  (statementToString s) ^ "\n"
                              | SIf(s, exp) => 
                                  "IF(" ^ (expToString exp) ^ ")\n" ^
                                  (statementToString s) ^ "\n"
                              | SPrint(exp) => 
                                  "Print: " ^ (expToString exp) ^ "\n"
                              | SReturn(exp) =>
                                  "Return: " ^ (expToString exp) ^ "\n"
                              | SIfEl(s1, s2, exp) => 
                                  "IF(" ^ (expToString exp) ^ "\n" ^
                                  (statementToString s1) ^ "\n" ^ "ELSE" ^ "\n"
                                  ^ (statementToString s2) ^ "\n"
                              | SReg(id, exp) =>
                                  id ^ " = " ^ (expToString exp) ^ "\n"  

  fun statementsToString (s : statement list) : string =
    case s of
          [] => ""
        | x :: xs => case x of
                                SDecl(id) => declToString(id) ^ "\n" ^
                                (statementsToString
                                xs)
                              | SInit(initl) =>
                                  initToString(initl) ^ "\n" ^
                                  (statementsToString xs)
                              | SDW(s, condi) => 
                                  "DO " ^ statementToString(s) ^ "\n" ^ "WHILE "
                                  ^ (expToString condi) ^ "\n" ^
                                  (statementsToString xs) ^ "\n" 
                              | SFor(s, init, condi,step) =>
                                  "FOR(" ^ (statementToString init) ^ (expToString
                                  condi) ^ (expToString step) ^ "\n" ^
                                  (statementToString s) ^ "\n" ^
                                  (statementsToString xs)
                              | SIf(s, exp) => 
                                  "IF(" ^ (expToString exp) ^ ")\n" ^
                                  (statementToString s) ^ "\n" ^
                                  (statementsToString xs) ^ "\n"
                              | SPrint(exp) => 
                                  "Print: " ^ (expToString exp) ^ "\n" ^
                                  (statementsToString xs) ^ "\n"
                              | SReturn(exp) =>
                                  "Return: " ^ (expToString exp) ^ "\n" ^
                                  (statementsToString xs) ^ "\n"
                              | SIfEl(s1, s2, exp) => 
                                  "IF(" ^ (expToString exp) ^ "\n" ^
                                  (statementToString s1) ^ "\n" ^ "ELSE" ^ "\n"
                                  ^ (statementToString s2) ^ "\n" ^
                                  (statementsToString xs) ^ "\n"
                              | SReg(id, exp) =>
                                  id ^ " = " ^ (expToString exp) ^ "\n" ^
                                  (statementsToString xs) ^ "\n"  


  fun programToString (p : program) : string =
    case p of
          PPro([]) => ""
        | PPro(x :: xs) => case x of
                                DDecl(id) => "\n" ^ declToString(id) ^ "\n" ^ (programToString
                                (PPro(xs)))
                              | DInit(initl) =>
                                  "\n" ^ initToString(initl) ^ "\n" ^ (programToString (PPro(xs)))
                              | FDefi(defi) => 
                                  let val (ty, id, inputl, statel) = defi in
                                    "\n" ^ ty ^ " " ^ id ^ "(" ^ finputToString(inputl) 
                                    ^ ")" ^ ":\n" ^ (statementsToString statel)
                                    ^ "\n" ^ (programToString (PPro(xs)))
                                  end  
                              | FPro(pro) =>
                                  let val (ty, id, inputl) = pro in
                                    "\n" ^ ty ^ " " ^ id ^ "(" ^ finputToString(inputl)
                                    ^ ")" ^ "\n" ^ (programToString (PPro(xs)))
                                  end
                              | FProo(ty, id, tyl) => 
                                  "\n" ^ ty ^ "" ^ id ^ "(" ^
                                  typelistToString(tyl) ^ ")" ^ "\n" ^
                                  (programToString (PPro(xs)))

end
