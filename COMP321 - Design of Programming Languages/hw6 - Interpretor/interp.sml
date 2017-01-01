(*  COMP 321 HW 6:  Interpreter for a fragment of C.
 *   
 *   Grant Addis & Fabien Bessez
 *   Fall 2016
 *)

structure Interp =
struct

datatype value = VInt of int
			   | VDouble of real
			   | VString of string
			   | VBool of bool
			   | VFun of AnnAst.def
	   | VNone

type env = value Env.env
type frame = value Frame.frame

exception NoMainError
exception NoReturnError
exception UninitializedError
exception RuntimeTypeError

fun valueToString (v : value) : string =
  case v of
	  VInt i => Int.toString(i)
	| VDouble d => Real.toString(d)
	| VString s => s
	| VBool b => Bool.toString(b)

(* Evaluate an expression under an environment and return 
 * the value and the modified enviornment *)
fun eval (nv : env, e : AnnAst.exp) : value*env =
  let
	  fun extendWithInit (init : (AnnAst.id*AnnAst.exp), nv : env) : env =
		let val (i, e) = init in
			let val (v, nv') = eval(nv, e) in
				Env.extend nv' i v
			end
		end
			
	  (* Evaluate a list of statements that is expected to have at least one reachable return
	   * statement (eg, the list of statements in a function definition). It is assumed that
	   * any frame pushing and popping that needs to be done for the statement list is done
	   * by the caller. *)
	  fun evalFramedStatementsWithReturn (ss : AnnAst.stm list, nv : env) : env*value =
		let
			fun forLoopHelper (e1 : AnnAst.exp, e2 : AnnAst.exp, s : AnnAst.stm, nv : env) : env*value =
			  case eval(nv, e1) of
				  (VBool true, nv') => (case evalFramedStatementsWithReturn([s], Env.pushFrame nv' Frame.empty) of
											(nv'', VNone) => let val (_, nv''') = eval(Env.popFrame nv'', e2) in
																 forLoopHelper(e1, e2, s, nv''')
															 end
										  | (nv'', v) => (Env.popFrame(nv''), v))
				| (VBool false, nv') => (Env.popFrame(nv'), VNone)
			fun blockHelper (ss : AnnAst.stm list, nv : env) : env*value =
			  case ss of
				  [] => (nv, VNone)
				| s::sss => (case evalFramedStatementsWithReturn([s], nv) of
								 (nv', VNone) => blockHelper(sss, nv')
							   | (nv', v) => (nv', v))
		in
			case ss of
				[] => (nv, VNone)
			  | s::sss => (case s of
							   AnnAst.SExp e => let val (_, nv') = eval(nv, e) in evalFramedStatementsWithReturn(sss, nv') end
							 | AnnAst.SDecl (_, is) =>  evalFramedStatementsWithReturn(sss, Env.extends nv is VNone)
							 | AnnAst.SInit (_, inits) => evalFramedStatementsWithReturn(sss, foldl extendWithInit nv inits)
							 | AnnAst.SDoWhile (sdw, e) =>
							   (case evalFramedStatementsWithReturn([sdw],
																	Env.pushFrame nv Frame.empty) of
									(nv', VNone) => let val (ve, nv'') = eval(Env.popFrame nv', e) in
														(case ve of
															 VBool true => evalFramedStatementsWithReturn(ss, nv'')
														   | VBool false => evalFramedStatementsWithReturn(sss, nv''))
													end
								  | (nv', v) => (Env.popFrame nv', v))
							 | AnnAst.SWhile (e, sw) =>
							   (let val (ve, nv') = eval(nv, e) in
									(case ve of
										 VBool true => (case evalFramedStatementsWithReturn([sw],
																							Env.pushFrame nv' Frame.empty) of
															(nv'', VNone) => evalFramedStatementsWithReturn(ss, Env.popFrame nv'')
														  | (nv'', v) => (Env.popFrame nv'', v))
									   | VBool false => evalFramedStatementsWithReturn(sss, nv'))
								end)
							 | AnnAst.SFor ((_, i, e0), e1, e2, sf) =>
							   (let val (v, nv') = eval(Env.pushFrame nv Frame.empty, e0) in
									let val nv'' = Env.extend nv' i v in
										(case forLoopHelper(e1, e2, sf, nv'') of
											 (nv''', VNone) => evalFramedStatementsWithReturn(sss, nv''')
										   | (nv''', v') => (nv''', v'))
									end
								end)
							 | AnnAst.SIf (e, si) =>
							   (case eval(nv, e) of
									(VBool true, nv') => (case evalFramedStatementsWithReturn([si],
																							  Env.pushFrame nv' Frame.empty) of
															  (nv'', VNone) => evalFramedStatementsWithReturn(sss, Env.popFrame nv')
															| (nv'', v) => (nv'', v))
								  | (VBool false, nv') => evalFramedStatementsWithReturn(sss, nv'))
							 | AnnAst.SIfElse (e, s0, s1) =>
							   (case eval(nv, e) of
									(VBool true, nv') => (case evalFramedStatementsWithReturn([s0], Env.pushFrame nv' Frame.empty) of
															 (nv'', VNone) => evalFramedStatementsWithReturn(sss, Env.popFrame nv'')
														   | (nv'', v) => (Env.popFrame nv'', v))
								  | (VBool false, nv') => (case evalFramedStatementsWithReturn([s1], Env.pushFrame nv' Frame.empty) of
															   (nv'', VNone) => evalFramedStatementsWithReturn(sss, Env.popFrame nv'')
															 | (nv'', v) => (Env.popFrame nv'', v)))
							 | AnnAst.SReturn e => let val (v, nv') = eval(nv, e) in (nv', v) end
							 | AnnAst.SBlock ss => (case blockHelper(ss, Env.pushFrame nv Frame.empty) of
														(nv', VNone) => evalFramedStatementsWithReturn(sss, Env.popFrame nv')
													  | (nv', v) => (Env.popFrame nv', v)))
		end
	  fun getBinInts (nv : env, e0 : AnnAst.exp, e1 : AnnAst.exp) : (int*int)*env =
		let val (VInt v0, nv') = eval(nv, e0) in
  			let val (VInt v1, nv'') = eval(nv', e1) in
  				((v0, v1), nv'')
  			end
  		end
	  fun getBinDoubles (nv : env, e0 : AnnAst.exp, e1 : AnnAst.exp) : (real*real)*env =
		let val (VDouble v0, nv') = eval(nv, e0) in
  			let val (VDouble v1, nv'') = eval(nv', e1) in
  				((v0, v1), nv'')
  			end
  		end
	  fun getBinBools (nv : env, e0 : AnnAst.exp, e1 : AnnAst.exp) : (bool*bool)*env =
		let val (VBool v0, nv') = eval(nv, e0) in
  			let val (VBool v1, nv'') = eval(nv', e1) in
  				((v0, v1), nv'')
  			end
  		end
	  fun applyBinArithOp (nv : env, e0 : AnnAst.exp,
						   e1 : AnnAst.exp, t : AnnAst.typ,
						   fi : (int*int -> int),
						   fd : (real*real -> real)) : value*env =
		case t of
			AnnAst.Tint => let val ((i0, i1), nv') = getBinInts(nv, e0, e1) in
							   (VInt(fi(i0, i1)), nv')
						   end
		  | AnnAst.Tdouble => let val ((d0, d1), nv') = getBinDoubles(nv, e0, e1) in
								  (VDouble(fd(d0, d1)), nv')
							  end
		  | _ => raise RuntimeTypeError
	  fun applyBinArithOpBool (nv : env, e0 : AnnAst.exp,
							   e1 : AnnAst.exp, t : AnnAst.typ,
							   fi : (int*int -> bool),
							   fd : (real*real -> bool)) : value*env =
		case AnnAst.typeOf(e0) of
			AnnAst.Tint => let val ((i0, i1), nv') = getBinInts(nv, e0, e1) in
							   (VBool(fi(i0, i1)), nv')
						   end
		  | AnnAst.Tdouble => let val ((d0, d1), nv') = getBinDoubles(nv, e0, e1) in
								  (VBool(fd(d0, d1)), nv')
							  end
		  | _ => raise RuntimeTypeError
	  fun prepFunctionFrame (((_, i), e) : AnnAst.paramdecl*AnnAst.exp, (nv, frm) : env*frame) : env*frame =
		let val (v, nv') = eval(nv, e) in (nv', Frame.extend(frm, i, v)) end
  in
	  case e of
		  AnnAst.EInt i => (VInt i, nv)
		| AnnAst.EDouble d => (VDouble d, nv)
		| AnnAst.EString s => (VString s, nv)
		| AnnAst.ETrue => (VBool true, nv)
		| AnnAst.EFalse => (VBool false, nv)
		| AnnAst.EId (i, _) => (case Env.lookup nv i of
									VNone => raise UninitializedError
								  | v => (v, nv))
		| AnnAst.ECall ((i, es), t) => (case i of
											"readInt" => (VInt(IoBase.readInt()), nv)
										  | "printInt" => let val (VInt(pi), nv') = eval(nv, List.hd(es)) in
															  IoBase.printInt(pi);
															  (VInt(pi), nv')
														  end
										  | "readDouble" => (VDouble(IoBase.readDouble()), nv)
										  | "printDouble" => let val (VDouble(pd), nv') = eval(nv, List.hd(es)) in
																 IoBase.printDouble(pd);
																 (VDouble(pd), nv')
															 end
										  | "readBool" => (VBool(IoBase.readBool()), nv)
										  | "printBool" => let val (VBool(pb), nv') = eval(nv, List.hd(es)) in
															   IoBase.printBool(pb);
															   (VBool(pb), nv')
														   end
										  | "readString" => (VString(IoBase.readString()), nv)
										  | "printString" => let val (VString(ps), nv') = eval(nv, List.hd(es)) in
																 IoBase.printString(ps);
																 (VString(ps), nv')
															 end
										  | _ => let val VFun (AnnAst.DFun (t, i, pds, ss)) = Env.lookup nv i in
													 let val (nv', f) = (foldl (prepFunctionFrame) (nv, Frame.empty) (ListPair.zip(pds, es))) in
														 let val (nv'', v) = evalFramedStatementsWithReturn(ss, Env.pushFrame nv' f) in
															 (case v of
																  VNone => raise NoReturnError
																| _ => (v, Env.popFrame nv''))
														 end
													 end
												 end)
		| AnnAst.EPostIncr (i, _) => let val VInt v = Env.lookup nv i in
										 (VInt v, Env.update nv i (VInt(v + 1)))
									 end
		| AnnAst.EPostDecr (i, _) => let val VInt v = Env.lookup nv i in
										 (VInt v, Env.update nv i (VInt(v - 1)))
									 end
		| AnnAst.ENot (e, t) => (case eval(nv, e) of
									 (VBool true, nv') => (VBool false, nv')
								   | (VBool false, nv') => (VBool true, nv'))
		| AnnAst.EPreIncr (i, _) => let val VInt v = Env.lookup nv i in
										(VInt(v+1), Env.update nv i (VInt(v+1)))
									end
		| AnnAst.EPreDecr (i, _) => let val VInt v = Env.lookup nv i in
										(VInt(v-1), Env.update nv i (VInt(v-1)))
									end
		| AnnAst.EMul ((e0, e1), t) => applyBinArithOp(nv, e0, e1, t, op*, op* )
		| AnnAst.EDiv ((e0, e1), t) => applyBinArithOp(nv, e0, e1, t, Int.div, op/)
		| AnnAst.EMod ((e0, e1), t) => let val ((i0, i1), nv') = getBinInts(nv, e0, e1) in
										   (VInt(i0 mod i1), nv')
									   end
		| AnnAst.EAdd ((e0, e1), t) => applyBinArithOp(nv, e0, e1, t, op+, op+)
		| AnnAst.ESub ((e0, e1), t) => applyBinArithOp(nv, e0, e1, t, op -, op -)
		| AnnAst.ELt ((e0, e1), t) => applyBinArithOpBool(nv, e0, e1, t, op<, op<)
		| AnnAst.EGt ((e0, e1), t) => applyBinArithOpBool(nv, e0, e1, t, op>, op>)
		| AnnAst.ELe ((e0, e1), t) => applyBinArithOpBool(nv, e0, e1, t, op<=, op<=)
		| AnnAst.EGe ((e0, e1), t) => applyBinArithOpBool(nv, e0, e1, t, op>=, op>=)
		| AnnAst.EEq ((e0, e1), t) => applyBinArithOpBool(nv, e0, e1, t, op=, Real.==)
		| AnnAst.ENeq ((e0, e1), t) => applyBinArithOpBool(nv, e0, e1, t, op<>, Real.!=)
		| AnnAst.EAnd ((e0, e1), t) => let val ((b0, b1), nv') = getBinBools(nv, e0, e1) in
										   (VBool(b0 andalso b1), nv')
									   end
		| AnnAst.EOr ((e0, e1), t) => let val ((b0, b1), nv') = getBinBools(nv, e0, e1) in
										  (VBool(b0 orelse b1), nv')
									  end
		| AnnAst.EAsst ((i, e), _) => let val (v, nv') = eval(nv, e) in
										  (v, Env.update nv' i v)
									  end
		| AnnAst.ECond ((e, e0, e1), t) => (case eval(nv, e) of
												(VBool true, nv') => eval(nv, e0)
											  | (VBool false, nv') => eval(nv, e1))
  end
	  
fun evalNoEnv (e : AnnAst.exp) : value =
  let val (v, _) = eval(Env.empty, e) in
	  v
  end

fun addDefToEnv (d : AnnAst.def, nv : env) : env =
  case d of
	  AnnAst.DFun (_, i, pds, ss) => Env.extend nv i (VFun(d))
	| _ => nv

fun checkDefsForMain (defs : AnnAst.def list) : bool =
  case defs of
	  [] => false
	| (AnnAst.DFun (_, i, _, _))::xs => (i = "main") orelse checkDefsForMain(xs)
	| _::xs => checkDefsForMain(xs)
	
fun exec(p : AnnAst.program) : int =
  let val AnnAst.PDefs(ds) = p in
	  case checkDefsForMain(ds) of
		  true => let val nv = foldl (addDefToEnv) (Env.pushFrame Env.empty Frame.empty) ds in
					  let val (VInt i, _) = eval(nv, AnnAst.ECall(("main", []), AnnAst.Tint)) in
						  i
					  end
				  end
		| false => raise NoMainError
  end
	  
end
