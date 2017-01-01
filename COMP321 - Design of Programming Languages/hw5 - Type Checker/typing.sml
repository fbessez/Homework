(*  COMP 321 Homework 5:  CPP typing engine.
*   
*   Samuel Zhu
*   Fabien Bessez
*)

structure Typing =
struct

  exception TypeError
  exception UndeclaredError of Ast.id
  exception MultiplyDeclaredError of Ast.id
  exception ReturnTypeError

  structure Ann = AnnAst

  fun astToAnnTyp t = case t of
                    Ast.Tbool => Ann.Tbool
                  | Ast.Tint => Ann.Tint
                  | Ast.Tdouble => Ann.Tdouble
                  | Ast.Tstring => Ann.Tstring
                  | Ast.Tvoid => Ann.Tvoid

(*Environment structure of type SplayMapFn*)
  structure E = SplayMapFn(
                        struct
                           type ord_key = string
                           val compare = String.compare
                        end
                           )
(* From Textbook:
  emptyEnv  :: env
  newBlock  :: env -> env
  removeBlock :: env -> env
  lookVar   :: env -> id -> Ann.typ
  updateVar :: env -> id -> Ann.typ -> env
  varExistsCurrentBlock :: env -> id -> bool 
  lookFun   :: env -> id -> Ast.def
  updateFun :: env -> Ast.def -> env
*)

  type env = Ann.typ E.map list * Ast.def E.map

  (* The empty environment. The first projection is the queue of Ann.typ maps
  * The second projection is a Ast.def map intended to store function 
  * definitions and prototypes.
  * use ([],E.empty) if you don't want global variable declarations and initializations
  * use ([E.empty],E.empty) as the default value if you do
  *)
  val emptyEnv:env = ([], E.empty)

  (* Adds a map to the variable environment queue
  *)
  fun newBlock (env as (varenv, funenv):env):env = 
    (E.empty :: varenv, funenv)

  (* Removes a variable environment from the queue
  *)
  fun removeBlock (env as (ve::ves, funenv):env):env = (ves, funenv)
    | removeBlock _ = raise TypeError

  (* Returns the typ corresponding to the id by searching in env.
  *  The typ closest to the top of the queue is returned if there's more than 1
  *  Raises UndeclaredError if the id isn't present in the variable environment
  *)
  fun lookVar (env as (ve::ves,funenv):env)(id:Ann.id): Ann.typ =
        (case E.find(ve, id) of 
            NONE => lookVar (ves,funenv) id
          | SOME typ => typ)
      | lookVar _ id = raise UndeclaredError id

  (* Updates env with the idtyp entry.
  * Returns the updated environment
  * Fails if no variable environment exists in the queue.
  *)
  fun updateVar (env as (ve::ves,funenv):env)(idtyp:Ann.id * Ann.typ): env =
    (E.insert'(idtyp, ve)::ves, funenv)
    | updateVar _ _ = raise TypeError

  (* Checks whether the topmost variable environment contains id.
  * Returns true if it does, false otherwise
  * Fails if there is no topmost variable environment in the stack.
  *)
  fun varExistsCurrentBlock ((ve::_,funenv):env)(id:Ann.id):bool =
        (case E.find(ve,id) of 
            NONE => false
          | _ => true)
    | varExistsCurrentBlock _ _ = raise TypeError

  (* Checks to see if an ast definition with id already exists in env
  *  Raises UndeclaredError if the id isn't present in the function environment
  *)
  fun lookFun (env as (_,funenv):env)(id:Ast.id): Ast.def =
    case E.find(funenv, id) of 
           NONE => raise UndeclaredError id
         | SOME def => def
    
  (* Removes ids from an Ast parameter list.
  *)
  fun rmIds (prmdecls : Ast.paramdecl list): Ast.prototype list =
      List.map (fn (typ,_) => typ) prmdecls

 (* Updates env with newfun
  * Returns the updated environment
  * Fails if prototype and definition conflict.
  *)
  fun updateFun (env as (varenv,funenv):env)
    (newfun as (Ast.DFunProt(_,id,_)|Ast.DFun(_,id,_,_)):Ast.def): env =
       (case (E.find(funenv, id), newfun) of
         (NONE, _) => (varenv, E.insert'((id,newfun),funenv))
       | (SOME (Ast.DFunProt(t0,id0,pts)), Ast.DFun(t1,id1,prms,_)) =>
          if (t0 = t1) andalso (pts = rmIds prms)
          then (varenv, E.insert'((id,newfun),funenv))
          else raise MultiplyDeclaredError id
       | (_,_) => raise MultiplyDeclaredError id
       )
   | updateFun _ _ = raise TypeError


  (*  helper functions for inferExp
  *)

(*Helps infer increment expressions: PreDecr, PostDecr, PreIncr, PostIncr *)
  fun inferIncrements(env: env, id: Ast.id, ann_exp: Ann.id*Ann.typ -> Ann.exp) : Ann.exp = 
    let val t_id = lookVar env id
    in 
      case t_id of 
        Ann.Tint => ann_exp(id, t_id)
        | _ => raise TypeError
      end
(*Helps infer arithmetic expressions: Div, Mul, Sub, Add *)
  fun inferDivMulSubAdd(env: env, t0: Ann.exp , t1: Ann.exp, t00: Ann.typ, t11: Ann.typ,
    ann_exp: (Ann.exp*Ann.exp)*Ann.typ -> Ann.exp) : Ann.exp = 
    if t00 = t11 then 
      case t00 of
        Ann.Tint => ann_exp((t0, t1), Ann.Tint)
        | Ann.Tdouble => ann_exp((t0, t1), Ann.Tdouble)
        | _ => raise TypeError
    else raise TypeError

(*Helps infer expressions that require input of type int and return type int:
  Mod, LShift, RShift*)
  fun inferModShifts(env: env, t0: Ann.exp , t1: Ann.exp, t00: Ann.typ, t11: Ann.typ,
    ann_exp: (Ann.exp*Ann.exp)*Ann.typ -> Ann.exp) : Ann.exp = 
    if t00 = t11 then
      case t00 of
        Ann.Tint => ann_exp((t0, t1), Ann.Tint)
        | _ => raise TypeError
    else raise TypeError

(*Helps infer comparison expressions: Lt, Gt, Le, Ge *)
  fun inferComparisons(env: env, t0: Ann.exp , t1: Ann.exp, t00: Ann.typ, t11: Ann.typ,
    ann_exp: (Ann.exp*Ann.exp)*Ann.typ -> Ann.exp) : Ann.exp =
    if t00 = t11 then
      case t00 of
        Ann.Tint => ann_exp((t0, t1), Ann.Tbool)
        | Ann.Tdouble => ann_exp((t0, t1), Ann.Tbool)
        | _ => raise TypeError
    else raise TypeError

(*Helps infer equality expressions: Eq, NEq*)
  fun inferEquality(env: env, t0: Ann.exp , t1: Ann.exp, t00: Ann.typ, t11: Ann.typ,
    ann_exp: (Ann.exp*Ann.exp)*Ann.typ -> Ann.exp): Ann.exp =
    if t00 = t11 then
      ann_exp((t0, t1), Ann.Tbool)
    else raise TypeError

(*Helps infer expressions that take input of type bool and return type bool:
  And, Or*)
  fun inferAndOr(env: env, t0: Ann.exp , t1: Ann.exp, t00: Ann.typ, t11: Ann.typ,
    ann_exp: (Ann.exp*Ann.exp)*Ann.typ -> Ann.exp): Ann.exp = 
    if t00 = t11 then
      case t00 of
        Ann.Tbool => ann_exp((t0, t1), Ann.Tbool)
        | _ => raise TypeError
    else raise TypeError

(*Helps infer the Not expression (!) *)
  fun inferNot(env: env, t_id: Ann.exp , ann_exp: Ann.exp * Ann.typ -> Ann.exp): Ann.exp = 
    if Ann.getType(t_id) = Ann.Tbool then ann_exp(t_id, Ann.Tbool)
    else raise TypeError

  (* this helper function uses polyequal. not sure if we can directly compare
   * 2 lists *)
  fun inferCall(env: env, id: Ann.id, es) :Ann.exp = 
    let val func = lookFun env id
        val es = List.map (fn e => inferExp (env, e)) es
        val estypes = List.map Ann.getType es
        fun sameTypes (etlist0, etlist1, typf) =
          if etlist0 = etlist1 then typf
          else raise TypeError
          (*
          case (etlist0, etlist1) of
              ([], []) => typf
            | (e0::e0s, e1::e1s) => (
                case e0 = e1 of 
                  true => sameTypes (e0s, e1s, typf)
                | false => raise TypeError
                )
            | (_, _) => raise TypeError
            *)
    in 
      case func of 
        Ast.DFunProt(typf, _, plist) => Ann.ECall((id,es),
        (sameTypes(List.map astToAnnTyp plist, estypes, astToAnnTyp typf)))
      | Ast.DFun(typf, _, prms, _) => Ann.ECall((id,es),
        (sameTypes(List.map astToAnnTyp (rmIds prms), estypes, astToAnnTyp typf)))
      | _ => raise TypeError
      end

  (* infers the Ann.exp from the env and the Ast.exp
   *)
  and inferExp(env:env, exp: Ast.exp): Ann.exp =
    case exp of 
        Ast.EInt i => Ann.EInt i 
      | Ast.EDouble r  => Ann.EDouble r 
      | Ast.EString s  => Ann.EString s 
      | Ast.ETrue => Ann.ETrue
      | Ast.EFalse => Ann.EFalse
      | Ast.EId var  => Ann.EId(var, lookVar env var)
      | Ast.ECall(id, es) => inferCall(env, id, es)
      | Ast.EPostIncr id => inferIncrements(env, id, Ann.EPostIncr)
      | Ast.EPostDecr id => inferIncrements(env, id, Ann.EPostDecr)
      | Ast.ENot id => inferNot(env, inferExp(env, id), Ann.ENot)
      | Ast.EPreIncr id => inferIncrements(env, id, Ann.EPreIncr)
      | Ast.EPreDecr id => inferIncrements(env, id, Ann.EPreDecr)
      | (Ast.EMul(e0,e1) | Ast.EDiv(e0,e1) | Ast.EMod(e0,e1)|
         Ast.EAdd(e0, e1) | Ast.ESub(e0, e1) |
         Ast.ELShift(e0, e1) | Ast.ERShift(e0, e1) |
         Ast.ELt(e0, e1) | Ast.EGt(e0, e1) |
         Ast.ELe(e0, e1) | Ast.EGe(e0, e1) | 
         Ast.EEq(e0, e1) | Ast.ENeq(e0, e1) |
         Ast.EAnd(e0, e1) | 
         Ast.EOr(e0, e1)) =>
        let
          val t0 = inferExp(env, e0)
          val t1 = inferExp(env, e1)
          val t00 = Ann.getType t0
          val t11 = Ann.getType t1
        in
          case exp of
            Ast.EMul(e0, e1) => inferDivMulSubAdd(env, t0, t1, t00, t11, Ann.EMul)
            | Ast.EDiv(e0, e1) => inferDivMulSubAdd(env, t0, t1, t00, t11, Ann.EDiv)
            | Ast.EMod(e0, e1) => inferModShifts(env, t0, t1, t00, t11, Ann.EMod)
            | Ast.EAdd(e0, e1) => inferDivMulSubAdd(env, t0, t1, t00, t11, Ann.EAdd)
            | Ast.ESub(e0, e1) => inferDivMulSubAdd(env, t0, t1, t00, t11, Ann.ESub)
            | Ast.ELShift(e0, e1) => inferModShifts(env, t0, t1, t00, t11, Ann.ELShift)
            | Ast.ERShift(e0, e1) => inferModShifts(env, t0, t1, t00, t11, Ann.ERShift)
            | Ast.ELt(e0, e1) => inferComparisons(env, t0, t1, t00, t11, Ann.ELt)
            | Ast.EGt(e0, e1) => inferComparisons(env, t0, t1, t00, t11, Ann.EGt)
            | Ast.ELe(e0, e1) => inferComparisons(env, t0, t1, t00, t11, Ann.ELe)
            | Ast.EGe(e0, e1) => inferComparisons(env, t0, t1, t00, t11, Ann.EGe)
            | Ast.EEq(e0, e1) => inferEquality(env, t0, t1, t00, t11, Ann.EEq)
            | Ast.ENeq(e0, e1) => inferEquality(env, t0, t1, t00, t11, Ann.ENeq)
            | Ast.EAnd(e0, e1) => inferAndOr(env, t0, t1, t00, t11, Ann.EAnd)
            | Ast.EOr(e0, e1) => inferAndOr(env, t0, t1, t00, t11, Ann.EOr)
            | _ => raise TypeError
          end
      | Ast.EAsst(id, e1) => 
        let 
          val t0 = lookVar env id
          val t1 = inferExp(env, e1)
          val t11 = Ann.getType(t1)
        in 
          if t0 = t11 then
            Ann.EAsst((id, t1), t11)
          else raise TypeError
          end
      | Ast.ECond(e1, e2, e3) => 
      let 
        val t1 = inferExp(env, e1)
        val t2 = inferExp(env, e2)
        val t3 = inferExp(env, e3)
        val t11 = Ann.getType(t1)
        val t22 = Ann.getType(t2)
        val t33 = Ann.getType(t3)
      in
        if t11 = Ann.Tbool andalso t22 = t33
        then Ann.ECond((t1, t2, t3), t33)
        else raise TypeError
      end

 (* Infers e on the empty env
  * Potentially throws an error
  *)
  fun inferExpNoEnv (e : Ast.exp) : AnnAst.exp =
    inferExp(emptyEnv, e)

  (* type checks statements
  * env is the environment
  * typ is the return typ of the function inside which we find stm the statement
  * returns an updated environment and the corresponding Ann stm
   *)
  fun typeCheckStatements(env:env, typ: Ann.typ, statement: Ast.stm):env * Ann.stm =
  let fun sdecl (env:env)(typ:Ann.typ)((id::ids):Ast.id list):env =
        (
        case varExistsCurrentBlock env id of
             true => raise MultiplyDeclaredError id
           | false => sdecl(updateVar env (id, typ))typ ids
           )
         | sdecl env t [] = env
      fun sinit (env:env)(typ:Ann.typ)
        ((id,exp)::rest:(Ast.id*Ast.exp) list):env*(Ann.id * Ann.exp) list =
        (let val exp = inferExp(env,exp)
         val exptyp = Ann.getType exp
        in
         case (exptyp = typ, varExistsCurrentBlock env id) of
           (false, _) => raise TypeError
         | (true, true) => raise MultiplyDeclaredError id
         | (true, false) => (let val (env,rest) = sinit(updateVar env (id, typ)) typ rest
                            in (env,(id,exp)::rest) end)
        end
         )
    | sinit env t [] = (env,[])
    fun boolTypeExpStm(env:env)(exp:Ast.exp)(stm:Ast.stm):env*Ann.exp*Ann.stm = 
         let val exp = inferExp (env,exp)
          in
          if Ann.Tbool = Ann.getType exp
          then let val (env,stm) = typeCheckStatements(env,typ,stm)
                 in
                   (env, exp, stm)
                 end
          else raise TypeError
         end
  in
    case statement of
       Ast.SExp exp => (env, Ann.SExp(inferExp(env, exp)))
     | Ast.SDecl (typ, ids) => let val typ = astToAnnTyp typ
                               in (sdecl env typ ids, Ann.SDecl(typ,ids)) 
                               end
     | Ast.SInit (typ, idsexps) => let val typ = astToAnnTyp typ
                                       val (env, idsexps) = sinit env typ idsexps
                               in (env, Ann.SInit(typ, idsexps))
                               end
     | Ast.SReturn exp => let 
         val exp = inferExp (env, exp) in
          if (Ann.getType exp = typ) then (env, Ann.SReturn exp) 
          else raise ReturnTypeError
         end
     | Ast.SDoWhile (stm,exp) => let 
         val (_, exp, stm) = boolTypeExpStm env exp stm
           in (env, Ann.SDoWhile(stm, exp))
         end
     | Ast.SWhile (exp,stm) => let 
         val (_, exp, stm) = boolTypeExpStm env exp stm
           in (env, Ann.SWhile(exp, stm))
         end
     | Ast.SIf (exp,stm) => let 
         val (_, exp, stm) = boolTypeExpStm env exp stm
           in (env, Ann.SIf(exp,stm))
         end
     | Ast.SIfElse (exp,stm0,stm1) => let
         val (_, exp, stm0) = boolTypeExpStm env exp stm0
         val (_, stm1) = typeCheckStatements(env,typ,stm1)
           in (env, Ann.SIfElse(exp, stm0, stm1))
         end
     | Ast.SFor ((typx,id,exp0),exp1,exp2,stm) =>
         let  
             val exp0 = inferExp (env,exp0)
             val exp0typ = Ann.getType exp0
           in
         case (varExistsCurrentBlock env id, exp0typ = astToAnnTyp typx)
           of
              (true,_) => raise MultiplyDeclaredError id
            | (false, false) => raise TypeError
            | (false, true) => (
              let 
                val env = updateVar (newBlock env) (id, exp0typ)
                val (exp1, exp2) = (inferExp (env,exp1),inferExp(env,exp2))
                val (env, stm) = 
                 typeCheckStatements(env, typ, stm)
                in
                 (removeBlock env, Ann.SFor((exp0typ, id, exp0),exp1,exp2,stm))
              end
              )
             end
     | Ast.SBlock stmlist => 
         let val (env, stmlist) = statementCascade(env,typ,stmlist)
            in
              (env, Ann.SBlock stmlist)
            end
  end

  (* Deals with a Ast statement list, updating the env
  * after each statement list is dealt with
  *)
  and statementCascade (env:env, ty:Ann.typ, ss:Ast.stm list):env * Ann.stm list = 
     case ss of 
       s :: ss => (
          let 
            val (env, ann_stm) = typeCheckStatements (env, ty, s)
            val (env, sslist) = statementCascade(env,ty,ss)
           in
              (env,ann_stm::sslist) 
           end)
        | [] => (env, [])


  (*  checkPgm p = p', where p' is the annotated program corresponding to p'.
  *)
  fun checkPgm (Ast.PDefs(defs) : Ast.program) : Ann.program =
    let 
      (* The 6 default functions prototypes *)
      val (f0, f1, f2, f3, f4, f5) =
        (
      Ast.DFunProt (Ast.Tint, "readInt", []), 
      Ast.DFunProt (Ast.Tdouble, "readDouble", []), 
      Ast.DFunProt (Ast.Tbool, "readBool", []), 
      Ast.DFunProt (Ast.Tvoid, "printInt", [Ast.Tint]),
      Ast.DFunProt (Ast.Tvoid, "printDouble", [Ast.Tdouble]), 
      Ast.DFunProt (Ast.Tvoid, "printBool", [Ast.Tbool]) 
        )
      val env = updateFun emptyEnv f0
      val env = updateFun env f1
      val env = updateFun env f2
      val env = updateFun env f3
      val env = updateFun env f4
      val envWFuns = updateFun env f5
      
      fun processParams (env:env)(pmlist:Ann.paramdecl list) : env =  
                case pmlist of 
                     [] => env
                   | (t,id)::pms => if varExistsCurrentBlock env id
                                    then raise MultiplyDeclaredError id
                                    else processParams (updateVar env (id, t)) pms
      fun checkPgmHelper(dflist : Ast.def list, env:env) : Ann.def list=
        case dflist of
             (def as (Ast.DFunProt(ty,id,ptlist)))::defs => 
             Ann.DFunProt(astToAnnTyp ty, id, List.map astToAnnTyp ptlist)
             :: checkPgmHelper (defs, updateFun env def)
           | (def as (Ast.DFun(ty,id,pmlist,ss))) :: defs =>
               let 
                 val (ty,pmlist) = (astToAnnTyp ty,List.map(fn (ty, id) =>
                 ((astToAnnTyp ty), id)) pmlist)
                 val env = processParams(newBlock env) pmlist
                 val (env, ann_ss) = statementCascade(updateFun env def, ty, ss)
               in
                 Ann.DFun(ty, id,pmlist,ann_ss)::checkPgmHelper(defs,
                 removeBlock env)
               end
           | [] => []
           | _ => raise TypeError
      in
        Ann.PDefs (checkPgmHelper (defs, envWFuns))
      end
end
