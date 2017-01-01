(*  COMP 321:  Interpreter for a higher-order language.
*   
*   N. Danner
*   Fall 2016
*)

structure Interp =
struct

  structure A = Ast

  exception BadResult of A.exp

  exception Unimplemented
  exception Undefined

  type exp = A.exp
  type pgm = A.pgm

  (*  You will need to fill the rest of this in.
  *)
  type id = A.ident
  datatype value = Vint of int | Vstring of string | Vbool of bool | Vpair of value * value | Vc of id * exp * value Frame.frame
  type env = value Frame.frame
  datatype control = Cexp of exp | Cval of value | Null 
  datatype k = Let1 of id * exp | Let2 of value | Plus1 of exp * env | Plus2 of value | Minus1 of exp * env |
  Minus2 of value | Times1 of exp * env | Times2 of value | Div1 of exp * env |
  Div2 of value | Eq1 of exp * env | Eq2 of value | Ne1 of exp * env | Ne2 of
  value | Lt1 of exp * env | Lt2 of value | Le1 of exp * env | Le2 of value |
  Gt1 of exp * env | Gt2 of value | Ge1 of exp * env | Ge2 of value | App1 of
     exp * env | App2 of value | AndAlso1 of exp * env | AndAlso2 of value |
     OrElse1 of exp * env | OrElse2 of value | Not1 | Cat1 of exp * env | Cat2
     of value | Len1 | Cond1 of exp * exp * env | Cond2 | Cond3 | Proj11 |
     Proj12 | Proj21 | Proj22 | Pair1 of exp * env | Pair2 of value
  type state = control * env * k list


  fun value2ast (v : value) : exp =
    case v of
      Vint(i) => A.Num(i)
    | Vbool(b) => A.Bool(b)
    | Vstring(s) => A.Str(s)
    | Vpair(v1, v2) => A.Pair(value2ast(v1), value2ast(v2))
    | Vc(id, e, env) => A.Lambda(id, e)

  fun stateTrans(s : state) : state =
    case s of
      (Cval(v), env, []) => (Cval(v), env, [])  
    | (Cexp(A.Plus(e1, e2)), en, kl) => stateTrans((Cexp(e1), en, Plus1(e2,
      en)::kl))
    | (Cexp(e1), en, Plus1(e2, en')::kl) => stateTrans(Cval(evalEnvExp(e1, (Cexp(e1), en, kl))), en, Plus1(e2, en)::kl)
    | (Cval(Vint(i)), en, Plus1(e2, en')::kl) => stateTrans((Cexp(e2), en',
    Plus2(Vint(i))::kl))
    | (Cexp(e2), en, Plus2(Vint(i))::kl) => stateTrans(Cval(evalEnvExp(e2, (Cexp(e2), en, kl))), en, Plus2(Vint(i))::kl)
    | (Cval(Vint(i2)), en, Plus2(Vint(i1)):: kl) => stateTrans(Cval(Vint(i1 + i2)), en, kl)

    | (Cexp(A.Minus(e1, e2)), en, kl) => stateTrans((Cexp(e1), en, Minus1(e2, en)::kl))
    | (Cexp(e1), en, Minus1(e2, en')::kl) => stateTrans((Cval(evalEnvExp(e1,
    (Cexp(e1), en, kl))), en, Minus1(e2, en)::kl))
    | (Cval(Vint(i)), en, Minus1(e2, en')::kl) => stateTrans((Cexp(e2), en,
    Minus2(Vint(i))::kl))
    | (Cexp(e2), en, Minus2(Vint(i))::kl) => stateTrans((Cval(evalEnvExp(e2,
    (Cexp(e2), en, kl))), en, Minus2(Vint(i))::kl))
    | (Cval(Vint(i2)), en, Minus2(Vint(i1)):: kl) => stateTrans(Cval(Vint(i1 - i2)), en, kl)

    | (Cexp(A.Times(e1, e2)), en, kl) => stateTrans((Cexp(e1), en, Times1(e2, en)::kl))
    | (Cexp(e1), en, Times1(e2, en')::kl) => stateTrans(Cval(evalEnvExp(e1,
    (Cexp(e1), en, kl))), en, Times1(e2, en)::kl)
    | (Cval(Vint(i)), en, Times1(e2, en')::kl) => stateTrans((Cexp(e2), en,
    Times2(Vint(i))::kl))
    | (Cexp(e2), en, Times2(Vint(i))::kl) => stateTrans((Cval(evalEnvExp(e2,
    (Cexp(e2), en, kl))), en, Times2(Vint(i))::kl))
    | (Cval(Vint(i2)), en, Times2(Vint(i1)):: kl) => stateTrans(Cval(Vint(i1 * i2)), en, kl)

    | (Cexp(A.Div(e1, e2)), en, kl) => stateTrans((Cexp(e1), en, Div1(e2, en)::kl))
    | (Cexp(e1), en, Div1(e2, en')::kl) => stateTrans((Cval(evalEnvExp(e1,
    (Cexp(e1), en, kl))), en, Div1(e2, en)::kl))
    | (Cval(Vint(i)), en, Div1(e2, en')::kl) => stateTrans((Cexp(e2), en,
    Div2(Vint(i))::kl))
    | (Cexp(e2), en, Div2(Vint(i))::kl) => stateTrans((Cval(evalEnvExp(e2,
    (Cexp(e2), en, kl))), en, Div2(Vint(i))::kl))
    | (Cval(Vint(i2)), en, Div2(Vint(i1)):: kl) => stateTrans(Cval(Vint(i1 div i2)), en, kl)

    | (Cexp(A.Cat(e1, e2)), en, kl) => stateTrans((Cexp(e1), en, Cat1(e2, en)::kl))
    | (Cexp(e1), en, Cat1(e2, en')::kl) => stateTrans((Cval(evalEnvExp(e1,
    (Cexp(e1), en, kl))), en, Cat1(e2, en)::kl))
    | (Cval(Vstring(i)), en, Cat1(e2, en')::kl) => stateTrans((Cexp(e2), en,
    Cat2(Vstring(i))::kl))
    | (Cexp(e2), en, Cat2(Vstring(i))::kl) => stateTrans((Cval(evalEnvExp(e2,
    (Cexp(e2), en, kl))), en, Cat2(Vstring(i))::kl))
    | (Cval(Vstring(i2)), en, Cat2(Vstring(i1)):: kl) => stateTrans(Cval(Vstring(i1 ^ i2)), en, kl)


    | (Cexp(A.Eq(e0, e1)), env, kl) => stateTrans(Cexp(e0), env, Eq1(e1, env) ::
    kl)
    | (Cexp(e0), env, Eq1(e1, env1) :: kl) => stateTrans(Cval(evalEnvExp(e0,
    (Cexp(e0), env, kl))), env, Eq1(e1, env) :: kl)
    | (Cval(Vint(i)), env, Eq1(e1, env1) :: kl) => stateTrans(Cexp(e1), env1,
    Eq2(Vint(i)) :: kl)
    | (Cexp(e1), env, Eq2(Vint(v0)) :: kl) => stateTrans(Cval(evalEnvExp(e1,
    (Cexp(e1), env, kl))), env,  Eq2(Vint(v0)) :: kl)
    | (Cval(Vint(m1)), env, Eq2(Vint(m0)) :: kl) => (case m0 = m1 of
                                                        true =>
                                                        stateTrans(Cval(Vbool(true)), env,
                                                        kl)
                                                      | false =>
                                                          stateTrans(Cval(Vbool(false)),
                                                          env, kl))

    | (Cexp(A.Ne(e0, e1)), env, kl) => stateTrans(Cexp(e0), env, Ne1(e1, env) ::
    kl)
    | (Cexp(e0), env, Ne1(e1, env1) :: kl) => stateTrans(Cval(evalEnvExp(e0,
    (Cexp(e0), env, kl))), env, Ne1(e1, env) :: kl)
    | (Cval(Vint(i)), env, Ne1(e1, env1) :: kl) => stateTrans(Cexp(e1), env1,
    Ne2(Vint(i)) :: kl)
    | (Cexp(e1), env, Ne2(Vint(v0)) :: kl) => stateTrans(Cval(evalEnvExp(e1,
    (Cexp(e1), env, kl))), env,  Ne2(Vint(v0)) :: kl)
    | (Cval(Vint(m1)), env, Ne2(Vint(m0)) :: kl) => (case m0 <> m1 of
                                                        true =>
                                                        stateTrans(Cval(Vbool(true)), env,
                                                        kl)
                                                      | false =>
                                                          stateTrans(Cval(Vbool(false)),
                                                          env, kl))

    | (Cexp(A.Lt(e0, e1)), env, kl) => stateTrans(Cexp(e0), env, Lt1(e1, env) ::
    kl)
    | (Cexp(e0), env, Lt1(e1, env1) :: kl) => stateTrans(Cval(evalEnvExp(e0,
    (Cexp(e0), env, kl))), env, Lt1(e1, env) :: kl)
    | (Cval(Vint(i)), env, Lt1(e1, env1) :: kl) => stateTrans(Cexp(e1), env1,
    Lt2(Vint(i)) :: kl)
    | (Cexp(e1), env, Lt2(Vint(v0)) :: kl) => stateTrans(Cval(evalEnvExp(e1,
    (Cexp(e1), env, kl))), env,  Lt2(Vint(v0)) :: kl)
    | (Cval(Vint(m1)), env, Lt2(Vint(m0)) :: kl) => (case m0 < m1 of
                                                        true =>
                                                        stateTrans(Cval(Vbool(true)), env,
                                                        kl)
                                                      | false =>
                                                          stateTrans(Cval(Vbool(false)),
                                                          env, kl))

    | (Cexp(A.Gt(e0, e1)), env, kl) => stateTrans(Cexp(e0), env, Gt1(e1, env) ::
    kl)
    | (Cexp(e0), env, Gt1(e1, env1) :: kl) => stateTrans(Cval(evalEnvExp(e0,
    (Cexp(e0), env, kl))), env, Gt1(e1, env) :: kl)
    | (Cval(Vint(i)), env, Gt1(e1, env1) :: kl) => stateTrans(Cexp(e1), env1,
    Gt2(Vint(i)) :: kl)
    | (Cexp(e1), env, Gt2(Vint(v0)) :: kl) => stateTrans(Cval(evalEnvExp(e1,
    (Cexp(e1), env, kl))), env,  Gt2(Vint(v0)) :: kl)
    | (Cval(Vint(m1)), env, Gt2(Vint(m0)) :: kl) => (case m0 > m1 of
                                                        true =>
                                                        stateTrans(Cval(Vbool(true)), env,
                                                        kl)
                                                      | false =>
                                                          stateTrans(Cval(Vbool(false)),
                                                          env, kl))

    | (Cexp(A.Le(e0, e1)), env, kl) => stateTrans(Cexp(e0), env, Le1(e1, env) ::
    kl)
    | (Cexp(e0), env, Le1(e1, env1) :: kl) => stateTrans(Cval(evalEnvExp(e0,
    (Cexp(e0), env, kl))), env, Le1(e1, env) :: kl)
    | (Cval(Vint(i)), env, Le1(e1, env1) :: kl) => stateTrans(Cexp(e1), env1,
    Le2(Vint(i)) :: kl)
    | (Cexp(e1), env, Le2(Vint(v0)) :: kl) => stateTrans(Cval(evalEnvExp(e1,
    (Cexp(e1), env, kl))), env,  Le2(Vint(v0)) :: kl)
    | (Cval(Vint(m1)), env, Le2(Vint(m0)) :: kl) => (case m0 <= m1 of
                                                        true =>
                                                        stateTrans(Cval(Vbool(true)), env,
                                                        kl)
                                                      | false =>
                                                          stateTrans(Cval(Vbool(false)),
                                                          env, kl))
    
    | (Cexp(A.Ge(e0, e1)), env, kl) => stateTrans(Cexp(e0), env, Ge1(e1, env) ::
    kl)
    | (Cexp(e0), env, Ge1(e1, env1) :: kl) => stateTrans(Cval(evalEnvExp(e0,
    (Cexp(e0), env, kl))), env, Ge1(e1, env) :: kl)
    | (Cval(Vint(i)), env, Ge1(e1, env1) :: kl) => stateTrans(Cexp(e1), env1,
    Ge2(Vint(i)) :: kl)
    | (Cexp(e1), env, Ge2(Vint(v0)) :: kl) => stateTrans(Cval(evalEnvExp(e1,
    (Cexp(e1), env, kl))), env,  Ge2(Vint(v0)) :: kl)
    | (Cval(Vint(m1)), env, Ge2(Vint(m0)) :: kl) => (case m0 < m1 of
                                                        true =>
                                                        stateTrans(Cval(Vbool(true)), env,
                                                        kl)
                                                      | false =>
                                                          stateTrans(Cval(Vbool(false)),
                                                          env, kl))

    
    | (Cexp(A.Orelse(e0, e1)), env, kl) => stateTrans(Cexp(e0), env, OrElse1(e1,
    env) :: kl)
    | (Cexp(e0), env, OrElse1(e1, env1) :: kl) =>
            stateTrans(Cval(evalEnvExp(e0, (Cexp(e0), env, kl))), env, OrElse1(e1, env1) :: kl)
    | (Cval(Vbool(b0)), env, OrElse1(e1, env1) :: kl) => if b0 = true 
        														then 
        															stateTrans(Cval(Vbool(true)),
                                                                                                                                env,
                                                                                                                                kl)
        														else
        															stateTrans(Cexp(e1),
                                                                                                                                env1,
                                                                                                                                OrElse2(Vbool(b0))
                                                                                                                                ::
                                                                                                                                kl)
    | (Cexp(e1), env, OrElse2(Vbool(b0)) :: kl) =>
            stateTrans(Cval(evalEnvExp(e1, (Cexp(e1), env, kl))), env, OrElse2(Vbool(b0)) :: kl)
    | (Cval(Vbool(b1)), env, OrElse2(Vbool(b0)) :: kl) => if b1 = true
        															then
        																stateTrans(Cval(Vbool(true)),
                                                                                                                                        env,
                                                                                                                                        kl)
        															else
        																stateTrans(Cval(Vbool(false)),
                                                                                                                                        env,
                                                                                                                                        kl)

            														                                                      

    | (Cexp(A.Andalso(e0, e1)), env, kl) => stateTrans(Cexp(e0), env,
    AndAlso1(e1, env) :: kl)
    | (Cexp(e0), env, AndAlso1(e1, env1) :: kl) =>
        stateTrans(Cval(evalEnvExp(e0, (Cexp(e0), env, kl))), env, AndAlso1(e1, env1) :: kl)
    | (Cval(Vbool(b0)), env, AndAlso1(e1, env1) :: kl) => if b0 = true 
        														then 
        															stateTrans(Cexp(e1),
                                                                                                                                env1,
                                                                                                                                AndAlso2(Vbool(b0))
                                                                                                                                ::
                                                                                                                                kl)
        														else
        															stateTrans(Cval(Vbool(false)),
                                                                                                                                env,
                                                                                                                                kl)

    | (Cexp(e1), env, AndAlso2(Vbool(b0)) :: kl) =>
        stateTrans(Cval(evalEnvExp(e1, (Cexp(e1), env, kl))), env, AndAlso2(Vbool(b0)) :: kl)
    | (Cval(Vbool(b1)), env, AndAlso2(Vbool(b0)) :: kl) => if b1 = true
        															then
        																stateTrans(Cval(Vbool(true)),
                                                                                                                                        env,
                                                                                                                                        kl)
        															else
        																stateTrans(Cval(Vbool(false)),
                                                                                                                                        env,
                                                                                                                                        kl)



    | (Cexp(A.Not(e1)), env, kl) => stateTrans(Cexp(e1), env, Not1 :: kl)
    | (Cexp(e1), env, Not1 :: kl) => stateTrans(Cval(evalEnvExp(e1, (Cexp(e1),
    env, kl))), env,
    Not1 :: kl)
    | (Cval(Vbool(v)), env, Not1 :: kl) => stateTrans(Cval(Vbool(not v)), env, kl)

    | (Cexp(A.Len(str)), env, kl) => stateTrans(Cval(evalEnvExp(str,
    (Cexp(A.Len(str)), env, kl))), env, Len1 :: kl)
    | (Cval(Vstring(str)), env, Len1 :: kl) => stateTrans(Cval(Vint(size str)), env, kl)

    | (Cexp(A.Cond(e0, e1, e2)), env, kl) => stateTrans(Cexp(e0), env, Cond1(e1, e2, env) :: kl)
    | (Cexp(e0), env, Cond1(e1, e2, env1) :: kl) =>
        stateTrans(Cval(evalEnvExp(e0, (Cexp(e0), env, kl))), env, Cond1(e1, e2, env1) :: kl)
    | (Cval(Vbool(b0)), env, Cond1(e1, e2, env1) :: kl) => if b0 = true then
       		                                             stateTrans(Cexp(e1), env1, Cond2 :: kl)
       			                                   else
       							     stateTrans(Cexp(e2),
                                                             env1, Cond3 :: kl)
                                                           
    | (Cexp(e1), env, Cond2 :: kl) => stateTrans(Cval(evalEnvExp(e1, (Cexp(e1),
    env, kl))),
        env, Cond2 :: kl)
    | (Cval(v1), env, Cond2 :: kl) => stateTrans(Cval(v1), env, kl)
    | (Cexp(e2), env, Cond3 :: kl) => stateTrans(Cval(evalEnvExp(e2, (Cexp(e2),
    env, kl))),
        env, Cond3 :: kl)
    | (Cval(v1), env, Cond3 :: kl) => stateTrans(Cval(v1), env, kl)

    | (Cexp(A.Proj1(e)), env, kl) => stateTrans(Cval(evalEnvExp(e, s)), env, Proj11
    :: kl)
    | (Cval(Vpair(e0, e1)), env, Proj11 :: kl) => stateTrans(Cval(e0), env, kl)

    | (Cexp(A.Proj2(e)), env, kl) => stateTrans(Cval(evalEnvExp(e, s)), env, Proj21
    :: kl)
    | (Cval(Vpair(e0, e1)), env, Proj21 :: kl) => stateTrans(Cval(e1), env, kl)

    | (Cexp(A.Pair(e0,e1)), env, kl) => stateTrans(Cexp(e0), env, Pair1(e1, env) :: kl)
    | (Cexp(e0), env, Pair1(e1, env1) :: kl) => stateTrans(Cval(evalEnvExp(e0,
    (Cexp(e0), env, kl))), env, Pair1(e1, env1) :: kl)
    | (Cval(v0), env, Pair1(e1, env1) :: kl) => stateTrans(Cexp(e1), env,
    Pair2(v0) :: kl)
    | (Cexp(e1), env, Pair2(v0) :: kl) => stateTrans(Cval(evalEnvExp(e1,
    (Cexp(e1), env, kl))),
    env, Pair2(v0) :: kl)
    | (Cval(v1), env, Pair2(v0) :: kl) => stateTrans(Cval(Vpair(v0, v1)), env, kl)


    | (Cexp(A.Let(id, e0, e1)), env, kl) => stateTrans(Cexp(e0), env, Let1(id,
    e1) :: kl)
    | (Cexp(e0), env, Let1(id, e1) :: kl) => stateTrans(Cval(evalEnvExp(e0,
    (Cexp(e0), env, kl))), env, Let1(id, e1) :: kl)
    | (Cval(v0), env, Let1(id, e1) :: kl) => stateTrans(Cexp(e1),
    Frame.update(env, id, v0), Let2(v0) :: kl)
    | (Cexp(e1), env , Let2(v0) :: kl) => stateTrans(Cval(evalEnvExp(e1,
    (Cexp(e1), env, kl))),
    env, Let2(v0) :: kl)
    | (Cval(v1), env, Let2(v1') :: kl) => stateTrans(Cval(v1), env, kl)


    | (Cexp(A.Lambda(id, e)), env, kl) => stateTrans(Cval(Vc(id, e, env)), env, kl)


    | (Cexp(A.App(e0, e1)), env, kl) => stateTrans(Cexp(e0), env, App1(e1, env) :: kl)
    | (Cexp(A.Ident(id)), env, App1(e1, env') :: kl) => let val Vc(id'', exp'', env'') = evalEnvExp(A.Ident(id), (Null, env, kl))
                                                        in
           stateTrans(Cval(Vc(id'', exp'', Frame.update(env'', id, Vc(id'', exp'', env'')))), env, App1(e1, env') :: kl)
                                                        end
        
    | (Cval(Vc(id, exp, env)), env', App1(e1, env'') :: kl) => stateTrans(Cexp(e1), env'', App2(Vc(id, exp, env)) :: kl)
    | (Cexp(e1), env, App2(Vc(id, exp, env')) :: kl) => stateTrans(Cval(evalEnvExp(e1, (Cexp(e1), env, kl))), env, App2(Vc(id, exp, env')) :: kl)
    | (Cval(v), env, App2(Vc(id, exp, env')) :: kl) => stateTrans(Cval(evalEnvExp(exp, (Null, Frame.update(env', id, v), kl))), env, kl)
        
  and evalEnvExp (e : exp, s : state) : value =
    let val (c, en, kl) = s
    in
      case e of
        A.Ident(id) => (case Frame.lookup(en, id) of
                            SOME v => v
                          | NONE => raise Undefined)
      | A.Num(i) => Vint(i)
      | A.Str(s) => Vstring(s)
      | A.Bool(b) => Vbool(b)
      | A.Plus(e1, e2) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Plus(e1, e2)), en, kl))
                          in
                            v'
                          end
      | A.Minus(e1, e2) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Minus(e1, e2)), en, kl))
                          in
                            v'
                          end
      | A.Times(e1, e2) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Times(e1, e2)), en, kl))
                          in
                            v'
                          end
      | A.Div(e1, e2) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Div(e1, e2)), en, kl))
                          in
                            v'
                          end
      | A.Eq(e1, e2) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Eq(e1, e2)), en, kl))
                          in
                            v'
                          end
      | A.Ne(e1, e2) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Ne(e1, e2)), en, kl))
                          in
                            v'
                          end
      | A.Lt(e1, e2) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Lt(e1, e2)), en, kl))
                          in
                            v'
                          end
      | A.Gt(e1, e2) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Gt(e1, e2)), en, kl))
                          in
                            v'
                          end
      | A.Le(e1, e2) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Le(e1, e2)), en, kl))
                          in
                            v'
                          end
      | A.Ge(e1, e2) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Ge(e1, e2)), en, kl))
                          in
                            v'
                          end

      | A.Orelse(e1, e2) => let val (Cval(v'), en', kl') =
      stateTrans((Cexp(A.Orelse(e1, e2)), en, kl))
                          in
                            v'
                          end
                    
      | A.Andalso(e1, e2) => let val (Cval(v'), en', kl') =
      stateTrans((Cexp(A.Andalso(e1, e2)), en, kl))
                          in
                            v'
                          end
      | A.Not(e1) => let val (Cval(v'), en', kl') =
      stateTrans((Cexp(A.Not(e1)), en, kl))
                          in
                            v'
                          end

      | A.Cat(e1, e2) => let val (Cval(v'), en', kl') =
      stateTrans((Cexp(A.Cat(e1, e2)), en, kl))
                          in
                            v'
                          end

      | A.Len(e1) => let val (Cval(v'), en', kl') =
      stateTrans((Cexp(A.Len(e1)), en, kl))
                          in
                            v'
                          end

      | A.Proj1(e1) => let val (Cval(v'), en', kl') =
      stateTrans((Cexp(A.Proj1(e1)), en, kl))
                          in
                            v'
                          end

      | A.Proj2(e1) => let val (Cval(v'), en', kl') =
      stateTrans((Cexp(A.Proj2(e1)), en, kl))
                          in
                            v'
                          end

      | A.Pair(e1, e2) => let val (Cval(v'), en', kl') =
      stateTrans((Cexp(A.Pair(e1, e2)), en, kl))
                          in
                            v'
                          end


      | A.Cond(e1, e2, e3) => let val (Cval(v'), en', kl') =
      stateTrans(Cexp(A.Cond(e1, e2, e3)), en,  kl)
                          in
                            v'
                          end


      | A.Let(id, e1, e2) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Let(id, e1, e2)), en, kl))
                          in
                            v'
                          end

      | A.Letrec(id, e1, e2) => let val (Cval(v'), en', kl') =
      stateTrans((Cexp(A.Let(id, e1, e2)), en, kl))
                          in
                            v'
                          end



      | A.App(e1, e2) => let val(Cval(v'), en', kl') = stateTrans((Cexp(A.App(e1, e2)), en, kl))
                          in
                            v'
                          end


      | A.Lambda(id, e) => let val (Cval(v'), en', kl') = stateTrans((Cexp(A.Lambda(id, e)), en, kl))
                          in
                            v'
                          end







    end


  fun evalExp (e : exp) : value =
    evalEnvExp(e, (Null, Frame.empty, []))

  (*  evalPgm pgm = v, where v is the value of the RHS of the last declaration
  *   in pgm.
  *)
  fun evalDec (dec : Ast.dec, s : state) : value * env =
    let val(c, en, kl) = s
    in
      case dec of
        A.Val(id, exp) => (evalEnvExp(exp, s), Frame.extend(en, id, evalEnvExp(exp, s)))
      | A.Valrec(id, exp) => let val Vc(id', exp', en') = evalEnvExp(exp, s)
                             in
                               (Vc(id', exp', Frame.empty), Frame.extend(en, id, evalEnvExp(exp, s)))
                             end
    end

  fun evalDecl (decl : Ast.dec list, s : state, prev : value) : value =
    case decl of
      [] => prev
    | x::xs => let val (v', en') = evalDec(x, s)
               in
                 evalDecl(xs, (Null, en', []), v')
               end

  fun evalPgm (pgm : Ast.pgm) : value = 
    let val A.Program(decl) = pgm
        val s = (Null, Frame.empty, [])
    in
      evalDecl (decl, s, Vint(0))
    end

end
