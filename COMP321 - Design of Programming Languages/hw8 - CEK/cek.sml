(*
COMP 321
HW8: CEK Interp
Fabien Bessez
  *)

structure Interp =
struct

  structure A = Ast

  exception BadResult of A.exp

  exception Unimplemented

  type exp = A.exp
  type pgm = A.pgm

  type id = A.ident
  datatype value = VInt of int | VString of string | VBool of bool 
  | VPair of value * value | VClosure of A.ident * exp * value Frame.frame
  type env = value Frame.frame 
  datatype control = CExp of exp | CVal of value | Null
  datatype k = Num1 | Str1 | Bool1
  | Plus1 of exp * env | Plus2 of value | Minus1 of exp * env | Minus2 of value 
  | Mult1 of exp * env | Mult2 of value | Div1 of exp * env | Div2 of value
  | KEq1 of exp * env | KEq2 of value | KNeq1 of exp * env | KNeq2 of value 
  | Lt1 of exp * env | Lt2 of value | Le1 of exp * env | Le2 of value 
  | Gt1 of exp * env | Gt2 of value | Ge1 of exp * env | Ge2 of value
  | AndAlso1 of exp * env | AndAlso2 of value 
  | OrElse1 of exp * env | OrElse2 of value 
  | Not1
  | Cat1 of exp * env | Cat2 of value 
  | Len1
  | Cond1 of exp * exp * env | Cond2 | Cond3
  | Proj11 | Proj12 | Proj21 | Proj22 | Pair1 of exp * env | Pair2 of value
  | Let1 of id * exp  | Let2 of value
  | App1 of exp * env | App2 of value| Lambda1 of id
  type state = control * env * k list

  fun value2ast(v : value) : exp =
  	case v of
  		VInt(i) => A.Num(i)
  		| VBool(b) => A.Bool(b)
  		| VString(s) => A.Str(s)
  		| VPair(e0, e1) => A.Pair(value2ast(e0), value2ast(e1))
      | VClosure(id, exp, env) => A.Lambda(id, exp)

  fun trans(s: state) : state =
  	case s of
      (CVal(x), env, []) => (CVal(x), env, [])
      | (CExp(A.Andalso(e0, e1)), env, cont) => 
      trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, AndAlso1(e1, env) :: cont)
      | (CVal(VBool(b0)), env, AndAlso1(e1, env1) :: cont) => 
        if b0 = true 
          then 
            trans(CVal(evalEwSt8(e1, (CExp(e1), env, cont))), env, AndAlso2(VBool(b0)) :: cont)
          else
            trans(CVal(VBool(false)), env, cont)
      | (CVal(VBool(b1)), env, AndAlso2(VBool(b0)) :: cont) => 
        if b1 = true
          then
            trans(CVal(VBool(true)), env, cont)
          else
            trans(CVal(VBool(false)), env, cont)

      | (CExp(A.Orelse(e0, e1)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, OrElse1(e1, env) :: cont)
      | (CVal(VBool(b0)), env, OrElse1(e1, env1) :: cont) => 
        if b0 = true 
          then 
            trans(CVal(VBool(true)), env, cont)
          else
            trans(CVal(evalEwSt8(e1, (CExp(e1), env, cont))), env, OrElse2(VBool(b0)) :: cont)
      | (CVal(VBool(b1)), env, OrElse2(VBool(b0)) :: cont) => 
        if b1 = true
          then
            trans(CVal(VBool(true)), env, cont)
          else
            trans(CVal(VBool(false)), env, cont)

      | (CExp(A.Eq(e0, e1)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, KEq1(e1, env) :: cont)
      | (CVal(v0), env, KEq1(e1, env1) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env, cont))), env,  KEq2(v0) :: cont)
      | (CVal(v1), env, KEq2(v0) :: cont) => 
        (if v0 = v1
          then 
            trans(CVal(VBool(true)), env,  cont)
          else 
            trans(CVal(VBool(false)), env, cont)
          )

      | (CExp(A.Ne(e0, e1)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, KNeq1(e1, env) :: cont)
      | (CVal(v0), env, KNeq1(e1, env1) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env, cont))), env,  KNeq2(v0) :: cont)
      | (CVal(v1), env, KNeq2(v0) :: cont) => 
        (if v0 <> v1 
            then 
              trans(CVal(VBool(true)), env,  cont)
            else 
              trans(CVal(VBool(false)), env, cont)
            )

      | (CExp(A.Lt(e0, e1)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, Lt1(e1, env) :: cont)
      | (CVal(VInt(v0)), env, Lt1(e1, env1) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env1, cont))), env1,  Lt2(VInt(v0)) :: cont)
      | (CVal(VInt(v1)), env1, Lt2(VInt(v0)) :: cont) => 
        (case v0 < v1 of 
          true =>  
            trans(CVal(VBool(true)), env1,  cont)
        | false =>  
            trans(CVal(VBool(false)), env1, cont)
        )

      | (CExp(A.Le(e0, e1)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, Le1(e1, env) :: cont)
      | (CVal(VInt(i)), env, Le1(e1, env1) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env1, cont))), env1,  Le2(VInt(i)) :: cont)
      | (CVal(VInt(m1)), env1, Le2(VInt(m0)) :: cont) => 
        (case m0 <= m1 of 
          true =>  
            trans(CVal(VBool(true)), env1,  cont)
          | false =>  
            trans(CVal(VBool(false)), env1, cont)
          )

      | (CExp(A.Gt(e0, e1)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, Gt1(e1, env) :: cont)
      | (CVal(VInt(i)), env, Gt1(e1, env1) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env1, cont))), env1,  Gt2(VInt(i)) :: cont)
      | (CVal(VInt(m1)), env1, Gt2(VInt(m0)) :: cont) => 
        (case m0 > m1 of 
          true =>  
            trans(CVal(VBool(true)), env1,  cont)
          | false =>  
            trans(CVal(VBool(false)), env1, cont)
          )

      | (CExp(A.Ge(e0, e1)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, Ge1(e1, env) :: cont)
      | (CVal(VInt(i)), env, Ge1(e1, env1) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env1, cont))), env1,  Ge2(VInt(i)) :: cont)
      | (CVal(VInt(m1)), env1, Ge2(VInt(m0)) :: cont) => 
        (case m0 >= m1 of 
            true =>  
            trans(CVal(VBool(true)), env1,  cont)
          | false =>  
            trans(CVal(VBool(false)), env1, cont)
          )

  		| (CExp(A.Plus(e0, e1)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, Plus1(e1, env) :: cont)
  		| (CVal(VInt(v0)), env, Plus1(e1, env1) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env1, cont))), env, Plus2(VInt(v0)) :: cont)
  		| (CVal(VInt(v1)), env, Plus2(VInt(v0)) :: cont) => 
        trans(CVal(VInt(v0 + v1)), env, cont)

  		| (CExp(A.Minus(e0, e1)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, Minus1(e1, env) :: cont)
  		| (CVal(VInt(v0)), env, Minus1(e1, env1) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env1, cont))), env, Minus2(VInt(v0)) :: cont)
  		| (CVal(VInt(v1)), env, Minus2(VInt(v0)) :: cont) => 
        trans(CVal(VInt(v0 - v1)), env, cont)

      | (CExp(A.Cat(s1, s2)), env, cont) => 
        trans(CVal(evalEwSt8(s1, (CExp(s1), env, cont))), env, Cat1(s2, env) :: cont)
      | (CVal(VString(v1)), env, Cat1(s2, env1) :: cont) =>  
        trans(CVal(evalEwSt8(s2, (CExp(s2), env1, cont))), env, Cat2(VString(v1)) :: cont)
      | (CVal(VString(v2)), env, Cat2(VString(v1)) :: cont) => 
        trans(CVal(VString(v1 ^ v2)), env, cont)

  		| (CExp(A.Times(e0, e1)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, Mult1(e1, env) :: cont)
  		| (CVal(VInt(v0)), env, Mult1(e1, env1) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env1, cont))), env, Mult2(VInt(v0)) :: cont)
  		| (CVal(VInt(v1)), env, Mult2(VInt(v0)) :: cont) => 
        trans(CVal(VInt(v0 * v1)), env, cont)

  		| (CExp(A.Div(e0, e1)), env, cont) =>
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, Div1(e1, env) :: cont)
  		| (CVal(VInt(v0)), env, Div1(e1, env1) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env1, cont))), env, Div2(VInt(v0)) :: cont)
  		| (CVal(VInt(v1)), env, Div2(VInt(v0)) :: cont) => 
        trans(CVal(VInt(v0 div v1)), env, cont)

      | (CExp(A.Not(e0)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, Not1 :: cont)
      | (CVal(VBool(v0)), env, Not1 :: cont) => 
        trans(CVal(VBool(not v0)), env, cont)

      | (CExp(A.Proj1(e)), env, cont) => 
        trans(CVal(evalEwSt8(e, (CExp(e), env, cont))), env, Proj11 :: cont)
      | (CVal(VPair(e0, e1)), env, Proj11 :: cont) => 
        trans(CVal(e0), env, cont)

      | (CExp(A.Proj2(e)), env, cont) => 
        trans(CVal(evalEwSt8(e, (CExp(e), env, cont))), env, Proj21 :: cont)
      | (CVal(VPair(e0, e1)), env, Proj21 :: cont) => 
        trans(CVal(e1), env, cont)

      | (CExp(A.App(e0, e1)), env, cont) => 
        trans(CExp(e0), env, App1(e1, env) :: cont)
      | (CExp(A.Ident(id)), env, App1(e1, env') :: cont) => 
          let 
            val VClosure(id'', exp'', env'') = evalEwSt8(A.Ident(id), (Null, env, cont))
          in
            trans(CVal(VClosure(id'', exp'', Frame.update(env'', id, VClosure(id'', exp'', env'')))), env, App1(e1, env') :: cont)
          end
      | (CVal(VClosure(id, exp, env)), env', App1(e1, env'') :: cont) => 
        trans(CExp(e1), env'', App2(VClosure(id, exp, env)) :: cont)
      | (CExp(e1), env, App2(VClosure(id, exp, env')) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env, cont))), env, App2(VClosure(id, exp, env')) :: cont)
      | (CVal(v0), env, App2(VClosure(id, exp, env')) :: cont) => 
        trans(CVal(evalEwSt8(exp, (Null, Frame.update(env', id, v0), cont))), env, cont)

      | (CExp(A.Cond(e0, e1, e2)), env, kl) => 
        trans(CExp(e0), env, Cond1(e1, e2, env) :: kl)
      | (CExp(e0), env, Cond1(e1, e2, env1) :: kl) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, kl))), env, Cond1(e1, e2, env1) :: kl)
      | (CVal(VBool(b0)), env, Cond1(e1, e2, env1) :: kl) => 
        if b0 = true 
          then
            trans(CVal(evalEwSt8(e1, (CExp(e1), env, kl))), env, Cond2 :: kl)
          else
            trans(CVal(evalEwSt8(e2, (CExp(e2),env, kl))), env, Cond3 :: kl)                                                    
      | (CVal(v1), env, Cond2 :: kl) => 
        trans(CVal(v1), env, kl)
      | (CVal(v1), env, Cond3 :: kl) => 
        trans(CVal(v1), env, kl)

      | (CExp(A.Let(id, e0, e1)), env, cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, Let1(id, e1) :: cont)
      | (CVal(v0), env, Let1(id, e1) :: cont) => 
        trans(CExp(e1), Frame.update(env, id, v0), Let2(v0) :: cont)
      | (CExp(e1), env, Let2(v0) :: cont)  => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env, cont))), env, Let2(v0) :: cont)
      | (CVal(v1), env, Let2(v0) :: cont) => 
        trans(CVal(v1), env, cont)


      | (CExp(A.Lambda(id, e0)), env, cont) => 
        trans(CVal(VClosure(id, e0, env)), env, cont)

      | (CExp(A.Pair(e0,e1)), env, cont) => 
        trans(CExp(e0), env, Pair1(e1, env) :: cont)
      | (CExp(e0), env, Pair1(e1, env1) :: cont) => 
        trans(CVal(evalEwSt8(e0, (CExp(e0), env, cont))), env, Pair1(e1, env1) :: cont)
      | (CVal(v0), env, Pair1(e1, env1) :: cont) => 
        trans(CExp(e1), env, Pair2(v0) :: cont)
      | (CExp(e1), env, Pair2(v0) :: cont) => 
        trans(CVal(evalEwSt8(e1, (CExp(e1), env, cont))), env, Pair2(v0) :: cont)
      | (CVal(v1), env, Pair2(v0) :: cont) => 
        trans(CVal(VPair(v0, v1)), env, cont)

      | (CExp(A.Len(str)), env, cont) => 
        (CVal(evalEwSt8(str, (CExp(str), env, cont))), env, Len1 :: cont)
      | (CVal(VString(str)), env, Len1 :: cont) => 
        trans(CVal(VInt(size str)), env, cont)

      | (CExp(A.Num(i)), env, cont) => 
        trans(CVal(VInt(i)), env, cont)
      | (CExp(A.Str(str)), env, cont) => 
        trans(CVal(VString(str)), env, cont)
      | (CExp(A.Bool(b)), env, cont) => 
        trans(CVal(VBool(b)), env, cont)
      | (CExp(A.Ident(id)), env, cont) => 
        (case Frame.lookup(env, id) of
            SOME v => trans(CVal(v), env, cont)
            | NONE => raise Unimplemented)

  and evalEwSt8(e: exp, s : state) : value =
  	let 
  		val (control, env, cont) = s
  	in
  		case e of 
  			A.Num(i) => VInt(i)
  			| A.Str(s) => VString(s)
  			| A.Bool(b) => VBool(b)
  			| A.Plus(e0, e1) => 
  				let
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Plus(e0, e1)), env, cont))
  				in
  					v0
  				end
  			| A.Minus(e0, e1) => 
  				let
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Minus(e0, e1)), env, cont))
  				in
  					v0
  				end
 			| A.Times(e0, e1) => 
  				let
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Times(e0, e1)), env, cont))
  				in
  					v0
  				end
  			| A.Div(e0, e1) => 
  				let
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Div(e0, e1)), env, cont))
  				in
  					v0
  				end
  			| A.Eq(e0, e1) => 
  				let 
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Eq(e0, e1)), env, cont))
  				in
  					v0
  				end
  			| A.Ne(e0, e1) => 
  				let 
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Ne(e0, e1)), env, cont))
  				in
  					v0
  				end
  			| A.Lt(e0, e1) => 
  				let 
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Lt(e0, e1)), env, cont))
  				in
  					v0
  				end
  			| A.Gt(e0, e1) => 
  				let 
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Gt(e0, e1)), env, cont))
  				in
  					v0
  				end
  			| A.Le(e0, e1) => 
  				let 
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Le(e0, e1)), env, cont))
  				in
  					v0
  				end
  			| A.Ge(e0, e1) => 
  				let 
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Ge(e0, e1)), env, cont))
  				in
  					v0
  				end
  			| A.Andalso(e0, e1) => 
  				let 
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Andalso(e0, e1)), env, cont))
  				in
  					v0
  				end
  			| A.Orelse(e0, e1) => 
  				let 
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Orelse(e0, e1)), env, cont))
  				in
  					v0
  				end
  			| A.Not(e0) => 
  				let
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Not(e0)), env, cont))
  				in
  					v0
  				end
  			| A.Cat(s1, s2) => 
  				let 
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Cat(s1, s2)), env, cont))
  				in
  					v0
  				end
  			| A.Len(s1) => 
  				let 
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Len(s1)), env, cont))
  				in
  					v0
  				end
  			| A.Cond(e0, e1, e2) => 
  				let 
  					val (CVal(v0), env1, cont1) = trans((CExp(A.Cond(e0, e1, e2)), env, cont))
  				in
  					v0
  				end
        | A.Proj1(e) => 
          let 
            val (CVal(v0), env1, cont1) = trans((CExp(A.Proj1(e)), env, cont))
          in
            v0
          end
        | A.Proj2(e) => 
          let 
            val (CVal(v0), env1, cont1) = trans((CExp(A.Proj2(e)), env, cont))
          in
            v0
          end
        | A.Pair(e0, e1) =>
          let 
            val (CVal(v0), env1, cont1) = trans((CExp(A.Pair(e0, e1)), env, cont))
          in
            v0
          end
        | A.Let(id, e0, e1) => 
          let 
            val (CVal(v0), env1, cont1) = trans((CExp(A.Let(id, e0, e1)), env, cont))
          in
            v0
          end
        | A.Letrec(id, e1, e2) =>
          let 
            val (CVal(v0), env1, cont1) = trans((CExp(A.Let(id, e1, e2)), env, cont))
          in
            v0
          end
        | A.Ident(id) => 
          (case Frame.lookup(env, id) of
            SOME v => v
            | NONE => raise Unimplemented)
        | A.App(e0, e1) => 
          let 
            val (CVal(v0), env1, cont1) = trans((CExp(A.App(e0, e1)), env, cont))
          in
            v0
          end
        | A.Lambda(id, e0) =>
          let 
            val (CVal(v0), env1, cont1) = trans((CExp(A.Lambda(id, e0)), env, cont))
          in
            v0
          end
  	end

  fun evalExp(e: exp) : value =
    let val mtstate = (Null, Frame.empty, [])
    in
      evalEwSt8(e, mtstate)
    end
    
  fun evalDec(dec: A.dec, s: state) : value * env =
  	let 
  		val (control, env, cont) = s
  	in
  		case dec of
  			A.Val(id, exp) => 
          (evalEwSt8(exp, s), Frame.extend(env, id, evalEwSt8(exp, s))) 
        | A.Valrec(id, exp) =>  
        let 
          val VClosure(id', exp', env') = evalEwSt8(exp, s)
        in
          (VClosure(id', exp', Frame.empty), Frame.extend(env, id, evalEwSt8(exp, s)))
        end
    end

  fun evalDecL(decl : A.dec list, s: state, prev: value) : value =
  	case decl of
  		[] => prev
  		| d :: ds => 
  			let
  				val (v, env) = evalDec(d, s)
  			in
  				evalDecL(ds, (Null, env, []), v)
  			end

	fun evalPgm(pgm: A.pgm) : value =
		let
			val A.Program(decl) = pgm
			val mtState = (Null, Frame.empty, [])
		in
			evalDecL(decl, mtState, VInt(0))
		end

end
