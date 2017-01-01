structure 
MLMinusTokens = struct

    datatype token = EOF
      | VALREC
      | VAL
      | COMMA
      | SEL2
      | SEL1
      | ENDIF
      | ELSE
      | THEN
      | IF
      | END
      | IN
      | BE
      | LETREC
      | LET
      | DARROW
      | FN
      | RVERT
      | LVERT
      | RP
      | LP
      | DIV
      | CAT
      | GE
      | GT
      | LE
      | LT
      | NE
      | EQ
      | TIMES
      | MINUS
      | PLUS
      | NOT
      | ANDALSO
      | ORELSE
      | FALSE
      | TRUE
      | STRING of string
      | NUM of int
      | ID of Ast.ident

    val allToks = [EOF, VALREC, VAL, COMMA, SEL2, SEL1, ENDIF, ELSE, THEN, IF, END, IN, BE, LETREC, LET, DARROW, FN, RVERT, LVERT, RP, LP, DIV, CAT, GE, GT, LE, LT, NE, EQ, TIMES, MINUS, PLUS, NOT, ANDALSO, ORELSE, FALSE, TRUE]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (VALREC) => "val rec"
  | (VAL) => "val"
  | (COMMA) => ","
  | (SEL2) => "#2"
  | (SEL1) => "#1"
  | (ENDIF) => "fi"
  | (ELSE) => "else"
  | (THEN) => "then"
  | (IF) => "if"
  | (END) => "end"
  | (IN) => "in"
  | (BE) => "BE"
  | (LETREC) => "let rec"
  | (LET) => "let"
  | (DARROW) => "=>"
  | (FN) => "fn"
  | (RVERT) => "]"
  | (LVERT) => "["
  | (RP) => ")"
  | (LP) => "("
  | (DIV) => "/"
  | (CAT) => "^"
  | (GE) => ">="
  | (GT) => ">"
  | (LE) => "<="
  | (LT) => "<"
  | (NE) => "<>"
  | (EQ) => "="
  | (TIMES) => "*"
  | (MINUS) => "-"
  | (PLUS) => "+"
  | (NOT) => "not"
  | (ANDALSO) => "andalso"
  | (ORELSE) => "orelse"
  | (FALSE) => "FALSE"
  | (TRUE) => "TRUE"
  | (STRING(_)) => "STRING"
  | (NUM(_)) => "NUM"
  | (ID(_)) => "ID"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (VALREC) => false
  | (VAL) => false
  | (COMMA) => false
  | (SEL2) => false
  | (SEL1) => false
  | (ENDIF) => false
  | (ELSE) => false
  | (THEN) => false
  | (IF) => false
  | (END) => false
  | (IN) => false
  | (BE) => false
  | (LETREC) => false
  | (LET) => false
  | (DARROW) => false
  | (FN) => false
  | (RVERT) => false
  | (LVERT) => false
  | (RP) => false
  | (LP) => false
  | (DIV) => false
  | (CAT) => false
  | (GE) => false
  | (GT) => false
  | (LE) => false
  | (LT) => false
  | (NE) => false
  | (EQ) => false
  | (TIMES) => false
  | (MINUS) => false
  | (PLUS) => false
  | (NOT) => false
  | (ANDALSO) => false
  | (ORELSE) => false
  | (FALSE) => false
  | (TRUE) => false
  | (STRING(_)) => false
  | (NUM(_)) => false
  | (ID(_)) => false
(* end case *))

  fun isEOF EOF = true
    | isEOF _ = false

end

functor MLMinusParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
MLMinusTokens
    structure UserCode =
      struct

  fun multi_unop rator 1 t = Ast.Not(t)
    | multi_unop rator n t = Ast.Not(multi_unop rator (n-1) t)

  fun multi_unop' rators t =
      case rators of
           [] => t
         | r :: rs => r(multi_unop' rs t)

  fun multi_binop_right rator ([t]) = t
    | multi_binop_right rator (t :: ts) = rator(t, multi_binop_right rator ts)

  fun multi_binop_left' rator ([t]) = t
    | multi_binop_left' rator (t :: ts) = rator(multi_binop_left' rator ts, t)
  fun multi_binop_left rator ts = multi_binop_left' rator (rev ts)

  
  fun multi_binop_left2' init [] = init
    | multi_binop_left2' init ((rator, rand) :: rrs) =
        rator (multi_binop_left2' init rrs, rand)
  fun multi_binop_left2 init rrs = multi_binop_left2' init (rev rrs)


fun pgm_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (VAL, VAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Val)
fun pgm_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (VALREC, VALREC_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Valrec)
fun pgm_PROD_1_SUBRULE_1_PROD_1_ACT (EQ, ID, SR, exp, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SR(ID, exp))
fun pgm_PROD_1_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Program SR)
fun orelse_exp_PROD_1_ACT (SR, andalso_exp, SR_SPAN : (Lex.pos * Lex.pos), andalso_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (multi_binop_left Ast.Orelse (andalso_exp :: SR))
fun andalso_exp_PROD_1_ACT (SR, numrel_exp, SR_SPAN : (Lex.pos * Lex.pos), numrel_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (multi_binop_left Ast.Andalso (numrel_exp :: SR))
fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (EQ, plusminus_exp, EQ_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Eq)
fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (NE, plusminus_exp, NE_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Ne)
fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3_ACT (LT, plusminus_exp, LT_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Lt)
fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_4_ACT (LE, plusminus_exp, LE_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Le)
fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_5_ACT (GT, plusminus_exp, GT_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Gt)
fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_6_ACT (GE, plusminus_exp, GE_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Ge)
fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_ACT (SR, plusminus_exp, SR_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( fn x => SR(x, plusminus_exp))
fun numrel_exp_PROD_1_ACT (SR, plusminus_exp, SR_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR of NONE => plusminus_exp | SOME f => f plusminus_exp)
fun plusminus_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (PLUS, cat_exp, PLUS_SPAN : (Lex.pos * Lex.pos), cat_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Ast.Plus )
fun plusminus_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (MINUS, cat_exp, MINUS_SPAN : (Lex.pos * Lex.pos), cat_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Ast.Minus )
fun plusminus_exp_PROD_1_SUBRULE_1_PROD_1_ACT (SR, cat_exp, SR_SPAN : (Lex.pos * Lex.pos), cat_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( (SR, cat_exp) )
fun plusminus_exp_PROD_1_ACT (SR, cat_exp, SR_SPAN : (Lex.pos * Lex.pos), cat_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( multi_binop_left2 cat_exp SR )
fun cat_exp_PROD_1_ACT (SR, timesdiv_exp, SR_SPAN : (Lex.pos * Lex.pos), timesdiv_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (multi_binop_left Ast.Cat (timesdiv_exp :: SR))
fun timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (TIMES, not_exp, TIMES_SPAN : (Lex.pos * Lex.pos), not_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Ast.Times )
fun timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (DIV, not_exp, DIV_SPAN : (Lex.pos * Lex.pos), not_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Ast.Div )
fun timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_ACT (SR, not_exp, SR_SPAN : (Lex.pos * Lex.pos), not_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( (SR, not_exp) )
fun timesdiv_exp_PROD_1_ACT (SR, not_exp, SR_SPAN : (Lex.pos * Lex.pos), not_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( multi_binop_left2 not_exp SR )
fun not_exp_PROD_1_SUBRULE_1_PROD_2_SUBRULE_1_PROD_1_ACT (SR, sel_exp, SR_SPAN : (Lex.pos * Lex.pos), sel_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (multi_unop Ast.Not (length SR) sel_exp)
fun sel_exp_PROD_1_SUBRULE_1_PROD_1_ACT (SEL1, SEL1_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Proj1)
fun sel_exp_PROD_1_SUBRULE_1_PROD_2_ACT (SEL2, SEL2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Proj2)
fun sel_exp_PROD_1_ACT (SR, app_exp, SR_SPAN : (Lex.pos * Lex.pos), app_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (multi_unop' SR app_exp)
fun app_exp_PROD_1_ACT (cond_exp, cond_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (multi_binop_left Ast.App cond_exp)
fun cond_exp_PROD_1_ACT (IF, ELSE, THEN, exp1, exp2, exp3, ENDIF, IF_SPAN : (Lex.pos * Lex.pos), ELSE_SPAN : (Lex.pos * Lex.pos), THEN_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), exp3_SPAN : (Lex.pos * Lex.pos), ENDIF_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Cond(exp1, exp2, exp3))
fun let_exp_PROD_1_ACT (EQ, ID, IN, END, LET, exp1, exp2, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), IN_SPAN : (Lex.pos * Lex.pos), END_SPAN : (Lex.pos * Lex.pos), LET_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Ast.Let (ID, exp1, exp2) )
fun let_exp_PROD_2_ACT (EQ, ID, IN, END, exp, func, LETREC, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), IN_SPAN : (Lex.pos * Lex.pos), END_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), func_SPAN : (Lex.pos * Lex.pos), LETREC_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Letrec (ID, func, exp))
fun fn_exp_PROD_1_ACT (func, func_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (func)
fun pair_exp_PROD_1_ACT (LP, RP, SR, exp, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR of NONE => exp | SOME e2 => Ast.Pair(exp, e2))
fun len_exp_PROD_1_ACT (exp, LVERT, RVERT, exp_SPAN : (Lex.pos * Lex.pos), LVERT_SPAN : (Lex.pos * Lex.pos), RVERT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Ast.Len exp )
fun lit_id_exp_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Ident ID)
fun lit_id_exp_PROD_2_ACT (NUM, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Num NUM)
fun lit_id_exp_PROD_3_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Str STRING)
fun lit_id_exp_PROD_4_ACT (TRUE, TRUE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Bool true)
fun lit_id_exp_PROD_5_ACT (FALSE, FALSE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Bool false)
fun func_PROD_1_ACT (FN, ID, END, exp, DARROW, FN_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), END_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Ast.Lambda(ID, exp))
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) = 
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in 
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) = 
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) = 
	        (Err.whileDisabled eh (fn() => prod strm)) 
		handle Err.ParseError => try (prods)
          in try prods end
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchVALREC strm = (case (lex(strm))
 of (Tok.VALREC, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchVAL strm = (case (lex(strm))
 of (Tok.VAL, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEL2 strm = (case (lex(strm))
 of (Tok.SEL2, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEL1 strm = (case (lex(strm))
 of (Tok.SEL1, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchENDIF strm = (case (lex(strm))
 of (Tok.ENDIF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchELSE strm = (case (lex(strm))
 of (Tok.ELSE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTHEN strm = (case (lex(strm))
 of (Tok.THEN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchIF strm = (case (lex(strm))
 of (Tok.IF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEND strm = (case (lex(strm))
 of (Tok.END, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchIN strm = (case (lex(strm))
 of (Tok.IN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBE strm = (case (lex(strm))
 of (Tok.BE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLETREC strm = (case (lex(strm))
 of (Tok.LETREC, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLET strm = (case (lex(strm))
 of (Tok.LET, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDARROW strm = (case (lex(strm))
 of (Tok.DARROW, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFN strm = (case (lex(strm))
 of (Tok.FN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRVERT strm = (case (lex(strm))
 of (Tok.RVERT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLVERT strm = (case (lex(strm))
 of (Tok.LVERT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRP strm = (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLP strm = (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDIV strm = (case (lex(strm))
 of (Tok.DIV, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCAT strm = (case (lex(strm))
 of (Tok.CAT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchGE strm = (case (lex(strm))
 of (Tok.GE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchGT strm = (case (lex(strm))
 of (Tok.GT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLE strm = (case (lex(strm))
 of (Tok.LE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLT strm = (case (lex(strm))
 of (Tok.LT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNE strm = (case (lex(strm))
 of (Tok.NE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEQ strm = (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTIMES strm = (case (lex(strm))
 of (Tok.TIMES, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMINUS strm = (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPLUS strm = (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNOT strm = (case (lex(strm))
 of (Tok.NOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchANDALSO strm = (case (lex(strm))
 of (Tok.ANDALSO, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchORELSE strm = (case (lex(strm))
 of (Tok.ORELSE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFALSE strm = (case (lex(strm))
 of (Tok.FALSE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTRUE strm = (case (lex(strm))
 of (Tok.TRUE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSTRING strm = (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNUM strm = (case (lex(strm))
 of (Tok.NUM(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))

val (pgm_NT, exp_NT) = 
let
fun lit_id_exp_NT (strm) = let
      fun lit_id_exp_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.lit_id_exp_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun lit_id_exp_PROD_2 (strm) = let
            val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
            val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
            in
              (UserCode.lit_id_exp_PROD_2_ACT (NUM_RES, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun lit_id_exp_PROD_3 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.lit_id_exp_PROD_3_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun lit_id_exp_PROD_4 (strm) = let
            val (TRUE_RES, TRUE_SPAN, strm') = matchTRUE(strm)
            val FULL_SPAN = (#1(TRUE_SPAN), #2(TRUE_SPAN))
            in
              (UserCode.lit_id_exp_PROD_4_ACT (TRUE_RES, TRUE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun lit_id_exp_PROD_5 (strm) = let
            val (FALSE_RES, FALSE_SPAN, strm') = matchFALSE(strm)
            val FULL_SPAN = (#1(FALSE_SPAN), #2(FALSE_SPAN))
            in
              (UserCode.lit_id_exp_PROD_5_ACT (FALSE_RES, FALSE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.FALSE, _, strm') => lit_id_exp_PROD_5(strm)
          | (Tok.STRING(_), _, strm') => lit_id_exp_PROD_3(strm)
          | (Tok.ID(_), _, strm') => lit_id_exp_PROD_1(strm)
          | (Tok.NUM(_), _, strm') => lit_id_exp_PROD_2(strm)
          | (Tok.TRUE, _, strm') => lit_id_exp_PROD_4(strm)
          | _ => fail()
        (* end case *))
      end
fun exp_NT (strm) = let
      val (orelse_exp_RES, orelse_exp_SPAN, strm') = orelse_exp_NT(strm)
      val FULL_SPAN = (#1(orelse_exp_SPAN), #2(orelse_exp_SPAN))
      in
        ((orelse_exp_RES), FULL_SPAN, strm')
      end
and orelse_exp_NT (strm) = let
      val (andalso_exp_RES, andalso_exp_SPAN, strm') = andalso_exp_NT(strm)
      fun orelse_exp_PROD_1_SUBRULE_1_NT (strm) = let
            val (ORELSE_RES, ORELSE_SPAN, strm') = matchORELSE(strm)
            val (andalso_exp_RES, andalso_exp_SPAN, strm') = andalso_exp_NT(strm')
            val FULL_SPAN = (#1(ORELSE_SPAN), #2(andalso_exp_SPAN))
            in
              ((andalso_exp_RES), FULL_SPAN, strm')
            end
      fun orelse_exp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ORELSE, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(orelse_exp_PROD_1_SUBRULE_1_PRED, orelse_exp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(andalso_exp_SPAN), #2(SR_SPAN))
      in
        (UserCode.orelse_exp_PROD_1_ACT (SR_RES, andalso_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), andalso_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and andalso_exp_NT (strm) = let
      val (numrel_exp_RES, numrel_exp_SPAN, strm') = numrel_exp_NT(strm)
      fun andalso_exp_PROD_1_SUBRULE_1_NT (strm) = let
            val (ANDALSO_RES, ANDALSO_SPAN, strm') = matchANDALSO(strm)
            val (numrel_exp_RES, numrel_exp_SPAN, strm') = numrel_exp_NT(strm')
            val FULL_SPAN = (#1(ANDALSO_SPAN), #2(numrel_exp_SPAN))
            in
              ((numrel_exp_RES), FULL_SPAN, strm')
            end
      fun andalso_exp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ANDALSO, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(andalso_exp_PROD_1_SUBRULE_1_PRED, andalso_exp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(numrel_exp_SPAN), #2(SR_SPAN))
      in
        (UserCode.andalso_exp_PROD_1_ACT (SR_RES, numrel_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), numrel_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and numrel_exp_NT (strm) = let
      val (plusminus_exp_RES, plusminus_exp_SPAN, strm') = plusminus_exp_NT(strm)
      fun numrel_exp_PROD_1_SUBRULE_1_NT (strm) = let
            val (SR_RES, SR_SPAN, strm') = let
            fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                        val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm)
                        val FULL_SPAN = (#1(EQ_SPAN), #2(EQ_SPAN))
                        in
                          (UserCode.numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (EQ_RES, plusminus_exp_RES, EQ_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                        val (NE_RES, NE_SPAN, strm') = matchNE(strm)
                        val FULL_SPAN = (#1(NE_SPAN), #2(NE_SPAN))
                        in
                          (UserCode.numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (NE_RES, plusminus_exp_RES, NE_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                        val (LT_RES, LT_SPAN, strm') = matchLT(strm)
                        val FULL_SPAN = (#1(LT_SPAN), #2(LT_SPAN))
                        in
                          (UserCode.numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3_ACT (LT_RES, plusminus_exp_RES, LT_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_4 (strm) = let
                        val (LE_RES, LE_SPAN, strm') = matchLE(strm)
                        val FULL_SPAN = (#1(LE_SPAN), #2(LE_SPAN))
                        in
                          (UserCode.numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_4_ACT (LE_RES, plusminus_exp_RES, LE_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_5 (strm) = let
                        val (GT_RES, GT_SPAN, strm') = matchGT(strm)
                        val FULL_SPAN = (#1(GT_SPAN), #2(GT_SPAN))
                        in
                          (UserCode.numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_5_ACT (GT_RES, plusminus_exp_RES, GT_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_6 (strm) = let
                        val (GE_RES, GE_SPAN, strm') = matchGE(strm)
                        val FULL_SPAN = (#1(GE_SPAN), #2(GE_SPAN))
                        in
                          (UserCode.numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_6_ACT (GE_RES, plusminus_exp_RES, GE_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.GE, _, strm') =>
                          numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_6(strm)
                      | (Tok.LE, _, strm') =>
                          numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_4(strm)
                      | (Tok.NE, _, strm') =>
                          numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2(strm)
                      | (Tok.EQ, _, strm') =>
                          numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1(strm)
                      | (Tok.LT, _, strm') =>
                          numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3(strm)
                      | (Tok.GT, _, strm') =>
                          numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_5(strm)
                      | _ => fail()
                    (* end case *))
                  end
            in
              numrel_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT(strm)
            end
            val (plusminus_exp_RES, plusminus_exp_SPAN, strm') = plusminus_exp_NT(strm')
            val FULL_SPAN = (#1(SR_SPAN), #2(plusminus_exp_SPAN))
            in
              (UserCode.numrel_exp_PROD_1_SUBRULE_1_PROD_1_ACT (SR_RES, plusminus_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun numrel_exp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.EQ, _, strm') => true
              | (Tok.NE, _, strm') => true
              | (Tok.LT, _, strm') => true
              | (Tok.LE, _, strm') => true
              | (Tok.GT, _, strm') => true
              | (Tok.GE, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(numrel_exp_PROD_1_SUBRULE_1_PRED, numrel_exp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(plusminus_exp_SPAN), #2(SR_SPAN))
      in
        (UserCode.numrel_exp_PROD_1_ACT (SR_RES, plusminus_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), plusminus_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and plusminus_exp_NT (strm) = let
      val (cat_exp_RES, cat_exp_SPAN, strm') = cat_exp_NT(strm)
      fun plusminus_exp_PROD_1_SUBRULE_1_NT (strm) = let
            val (SR_RES, SR_SPAN, strm') = let
            fun plusminus_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  fun plusminus_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                        val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
                        val FULL_SPAN = (#1(PLUS_SPAN), #2(PLUS_SPAN))
                        in
                          (UserCode.plusminus_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (PLUS_RES, cat_exp_RES, PLUS_SPAN : (Lex.pos * Lex.pos), cat_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun plusminus_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                        val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
                        val FULL_SPAN = (#1(MINUS_SPAN), #2(MINUS_SPAN))
                        in
                          (UserCode.plusminus_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (MINUS_RES, cat_exp_RES, MINUS_SPAN : (Lex.pos * Lex.pos), cat_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.MINUS, _, strm') =>
                          plusminus_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2(strm)
                      | (Tok.PLUS, _, strm') =>
                          plusminus_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1(strm)
                      | _ => fail()
                    (* end case *))
                  end
            in
              plusminus_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT(strm)
            end
            val (cat_exp_RES, cat_exp_SPAN, strm') = cat_exp_NT(strm')
            val FULL_SPAN = (#1(SR_SPAN), #2(cat_exp_SPAN))
            in
              (UserCode.plusminus_exp_PROD_1_SUBRULE_1_PROD_1_ACT (SR_RES, cat_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), cat_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun plusminus_exp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.PLUS, _, strm') => true
              | (Tok.MINUS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(plusminus_exp_PROD_1_SUBRULE_1_PRED, plusminus_exp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(cat_exp_SPAN), #2(SR_SPAN))
      in
        (UserCode.plusminus_exp_PROD_1_ACT (SR_RES, cat_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), cat_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and cat_exp_NT (strm) = let
      val (timesdiv_exp_RES, timesdiv_exp_SPAN, strm') = timesdiv_exp_NT(strm)
      fun cat_exp_PROD_1_SUBRULE_1_NT (strm) = let
            val (CAT_RES, CAT_SPAN, strm') = matchCAT(strm)
            val (timesdiv_exp_RES, timesdiv_exp_SPAN, strm') = timesdiv_exp_NT(strm')
            val FULL_SPAN = (#1(CAT_SPAN), #2(timesdiv_exp_SPAN))
            in
              ((timesdiv_exp_RES), FULL_SPAN, strm')
            end
      fun cat_exp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.CAT, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(cat_exp_PROD_1_SUBRULE_1_PRED, cat_exp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(timesdiv_exp_SPAN), #2(SR_SPAN))
      in
        (UserCode.cat_exp_PROD_1_ACT (SR_RES, timesdiv_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), timesdiv_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and timesdiv_exp_NT (strm) = let
      val (not_exp_RES, not_exp_SPAN, strm') = not_exp_NT(strm)
      fun timesdiv_exp_PROD_1_SUBRULE_1_NT (strm) = let
            val (SR_RES, SR_SPAN, strm') = let
            fun timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  fun timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                        val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
                        val FULL_SPAN = (#1(TIMES_SPAN), #2(TIMES_SPAN))
                        in
                          (UserCode.timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (TIMES_RES, not_exp_RES, TIMES_SPAN : (Lex.pos * Lex.pos), not_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                        val (DIV_RES, DIV_SPAN, strm') = matchDIV(strm)
                        val FULL_SPAN = (#1(DIV_SPAN), #2(DIV_SPAN))
                        in
                          (UserCode.timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (DIV_RES, not_exp_RES, DIV_SPAN : (Lex.pos * Lex.pos), not_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.DIV, _, strm') =>
                          timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2(strm)
                      | (Tok.TIMES, _, strm') =>
                          timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1(strm)
                      | _ => fail()
                    (* end case *))
                  end
            in
              timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT(strm)
            end
            val (not_exp_RES, not_exp_SPAN, strm') = not_exp_NT(strm')
            val FULL_SPAN = (#1(SR_SPAN), #2(not_exp_SPAN))
            in
              (UserCode.timesdiv_exp_PROD_1_SUBRULE_1_PROD_1_ACT (SR_RES, not_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), not_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun timesdiv_exp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.TIMES, _, strm') => true
              | (Tok.DIV, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(timesdiv_exp_PROD_1_SUBRULE_1_PRED, timesdiv_exp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(not_exp_SPAN), #2(SR_SPAN))
      in
        (UserCode.timesdiv_exp_PROD_1_ACT (SR_RES, not_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), not_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and not_exp_NT (strm) = let
      val (SR_RES, SR_SPAN, strm') = let
      fun not_exp_PROD_1_SUBRULE_1_NT (strm) = let
            fun not_exp_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (sel_exp_RES, sel_exp_SPAN, strm') = sel_exp_NT(strm)
                  val FULL_SPAN = (#1(sel_exp_SPAN), #2(sel_exp_SPAN))
                  in
                    ((sel_exp_RES), FULL_SPAN, strm')
                  end
            fun not_exp_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (SR_RES, SR_SPAN, strm') = let
                  fun not_exp_PROD_1_SUBRULE_1_PROD_2_SUBRULE_1_NT (strm) = let
                        fun not_exp_PROD_1_SUBRULE_1_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                              val (NOT_RES, NOT_SPAN, strm') = matchNOT(strm)
                              val FULL_SPAN = (#1(NOT_SPAN), #2(NOT_SPAN))
                              in
                                ((), FULL_SPAN, strm')
                              end
                        fun not_exp_PROD_1_SUBRULE_1_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                               of (Tok.NOT, _, strm') => true
                                | _ => false
                              (* end case *))
                        val (SR_RES, SR_SPAN, strm') = EBNF.posclos(not_exp_PROD_1_SUBRULE_1_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_PRED, not_exp_PROD_1_SUBRULE_1_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm)
                        val (sel_exp_RES, sel_exp_SPAN, strm') = sel_exp_NT(strm')
                        val FULL_SPAN = (#1(SR_SPAN), #2(sel_exp_SPAN))
                        in
                          (UserCode.not_exp_PROD_1_SUBRULE_1_PROD_2_SUBRULE_1_PROD_1_ACT (SR_RES, sel_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), sel_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  in
                    not_exp_PROD_1_SUBRULE_1_PROD_2_SUBRULE_1_NT(strm)
                  end
                  val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
                  in
                    ((SR_RES), FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.NOT, _, strm') => not_exp_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.ID(_), _, strm') =>
                    not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.NUM(_), _, strm') =>
                    not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.STRING(_), _, strm') =>
                    not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.TRUE, _, strm') => not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.FALSE, _, strm') =>
                    not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.LP, _, strm') => not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.LVERT, _, strm') =>
                    not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.FN, _, strm') => not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.LET, _, strm') => not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.LETREC, _, strm') =>
                    not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.IF, _, strm') => not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.SEL1, _, strm') => not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.SEL2, _, strm') => not_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | _ => fail()
              (* end case *))
            end
      in
        not_exp_PROD_1_SUBRULE_1_NT(strm)
      end
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        ((SR_RES), FULL_SPAN, strm')
      end
and sel_exp_NT (strm) = let
      fun sel_exp_PROD_1_SUBRULE_1_NT (strm) = let
            fun sel_exp_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (SEL1_RES, SEL1_SPAN, strm') = matchSEL1(strm)
                  val FULL_SPAN = (#1(SEL1_SPAN), #2(SEL1_SPAN))
                  in
                    (UserCode.sel_exp_PROD_1_SUBRULE_1_PROD_1_ACT (SEL1_RES, SEL1_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun sel_exp_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (SEL2_RES, SEL2_SPAN, strm') = matchSEL2(strm)
                  val FULL_SPAN = (#1(SEL2_SPAN), #2(SEL2_SPAN))
                  in
                    (UserCode.sel_exp_PROD_1_SUBRULE_1_PROD_2_ACT (SEL2_RES, SEL2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.SEL2, _, strm') => sel_exp_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.SEL1, _, strm') => sel_exp_PROD_1_SUBRULE_1_PROD_1(strm)
                | _ => fail()
              (* end case *))
            end
      fun sel_exp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.SEL1, _, strm') => true
              | (Tok.SEL2, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(sel_exp_PROD_1_SUBRULE_1_PRED, sel_exp_PROD_1_SUBRULE_1_NT, strm)
      val (app_exp_RES, app_exp_SPAN, strm') = app_exp_NT(strm')
      val FULL_SPAN = (#1(SR_SPAN), #2(app_exp_SPAN))
      in
        (UserCode.sel_exp_PROD_1_ACT (SR_RES, app_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), app_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and app_exp_NT (strm) = let
      fun app_exp_PROD_1_SUBRULE_1_NT (strm) = let
            val (cond_exp_RES, cond_exp_SPAN, strm') = cond_exp_NT(strm)
            val FULL_SPAN = (#1(cond_exp_SPAN), #2(cond_exp_SPAN))
            in
              ((cond_exp_RES), FULL_SPAN, strm')
            end
      fun app_exp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | (Tok.NUM(_), _, strm') => true
              | (Tok.STRING(_), _, strm') => true
              | (Tok.TRUE, _, strm') => true
              | (Tok.FALSE, _, strm') => true
              | (Tok.LP, _, strm') => true
              | (Tok.LVERT, _, strm') => true
              | (Tok.FN, _, strm') => true
              | (Tok.LET, _, strm') => true
              | (Tok.LETREC, _, strm') => true
              | (Tok.IF, _, strm') => true
              | _ => false
            (* end case *))
      val (cond_exp_RES, cond_exp_SPAN, strm') = EBNF.posclos(app_exp_PROD_1_SUBRULE_1_PRED, app_exp_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(cond_exp_SPAN), #2(cond_exp_SPAN))
      in
        (UserCode.app_exp_PROD_1_ACT (cond_exp_RES, cond_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and cond_exp_NT (strm) = let
      fun cond_exp_PROD_1 (strm) = let
            val (IF_RES, IF_SPAN, strm') = matchIF(strm)
            val (exp1_RES, exp1_SPAN, strm') = exp_NT(strm')
            val (THEN_RES, THEN_SPAN, strm') = matchTHEN(strm')
            val (exp2_RES, exp2_SPAN, strm') = exp_NT(strm')
            val (ELSE_RES, ELSE_SPAN, strm') = matchELSE(strm')
            val (exp3_RES, exp3_SPAN, strm') = exp_NT(strm')
            val (ENDIF_RES, ENDIF_SPAN, strm') = matchENDIF(strm')
            val FULL_SPAN = (#1(IF_SPAN), #2(ENDIF_SPAN))
            in
              (UserCode.cond_exp_PROD_1_ACT (IF_RES, ELSE_RES, THEN_RES, exp1_RES, exp2_RES, exp3_RES, ENDIF_RES, IF_SPAN : (Lex.pos * Lex.pos), ELSE_SPAN : (Lex.pos * Lex.pos), THEN_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), exp3_SPAN : (Lex.pos * Lex.pos), ENDIF_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun cond_exp_PROD_2 (strm) = let
            val (let_exp_RES, let_exp_SPAN, strm') = let_exp_NT(strm)
            val FULL_SPAN = (#1(let_exp_SPAN), #2(let_exp_SPAN))
            in
              ((let_exp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => cond_exp_PROD_2(strm)
          | (Tok.NUM(_), _, strm') => cond_exp_PROD_2(strm)
          | (Tok.STRING(_), _, strm') => cond_exp_PROD_2(strm)
          | (Tok.TRUE, _, strm') => cond_exp_PROD_2(strm)
          | (Tok.FALSE, _, strm') => cond_exp_PROD_2(strm)
          | (Tok.LP, _, strm') => cond_exp_PROD_2(strm)
          | (Tok.LVERT, _, strm') => cond_exp_PROD_2(strm)
          | (Tok.FN, _, strm') => cond_exp_PROD_2(strm)
          | (Tok.LET, _, strm') => cond_exp_PROD_2(strm)
          | (Tok.LETREC, _, strm') => cond_exp_PROD_2(strm)
          | (Tok.IF, _, strm') => cond_exp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and let_exp_NT (strm) = let
      fun let_exp_PROD_1 (strm) = let
            val (LET_RES, LET_SPAN, strm') = matchLET(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (exp1_RES, exp1_SPAN, strm') = exp_NT(strm')
            val (IN_RES, IN_SPAN, strm') = matchIN(strm')
            val (exp2_RES, exp2_SPAN, strm') = exp_NT(strm')
            val (END_RES, END_SPAN, strm') = matchEND(strm')
            val FULL_SPAN = (#1(LET_SPAN), #2(END_SPAN))
            in
              (UserCode.let_exp_PROD_1_ACT (EQ_RES, ID_RES, IN_RES, END_RES, LET_RES, exp1_RES, exp2_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), IN_SPAN : (Lex.pos * Lex.pos), END_SPAN : (Lex.pos * Lex.pos), LET_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun let_exp_PROD_2 (strm) = let
            val (LETREC_RES, LETREC_SPAN, strm') = matchLETREC(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (func_RES, func_SPAN, strm') = func_NT(strm')
            val (IN_RES, IN_SPAN, strm') = matchIN(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (END_RES, END_SPAN, strm') = matchEND(strm')
            val FULL_SPAN = (#1(LETREC_SPAN), #2(END_SPAN))
            in
              (UserCode.let_exp_PROD_2_ACT (EQ_RES, ID_RES, IN_RES, END_RES, exp_RES, func_RES, LETREC_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), IN_SPAN : (Lex.pos * Lex.pos), END_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), func_SPAN : (Lex.pos * Lex.pos), LETREC_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun let_exp_PROD_3 (strm) = let
            val (fn_exp_RES, fn_exp_SPAN, strm') = fn_exp_NT(strm)
            val FULL_SPAN = (#1(fn_exp_SPAN), #2(fn_exp_SPAN))
            in
              ((fn_exp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => let_exp_PROD_3(strm)
          | (Tok.NUM(_), _, strm') => let_exp_PROD_3(strm)
          | (Tok.STRING(_), _, strm') => let_exp_PROD_3(strm)
          | (Tok.TRUE, _, strm') => let_exp_PROD_3(strm)
          | (Tok.FALSE, _, strm') => let_exp_PROD_3(strm)
          | (Tok.LP, _, strm') => let_exp_PROD_3(strm)
          | (Tok.LVERT, _, strm') => let_exp_PROD_3(strm)
          | (Tok.FN, _, strm') => let_exp_PROD_3(strm)
          | (Tok.LET, _, strm') => let_exp_PROD_1(strm)
          | (Tok.LETREC, _, strm') => let_exp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and fn_exp_NT (strm) = let
      fun fn_exp_PROD_1 (strm) = let
            val (func_RES, func_SPAN, strm') = func_NT(strm)
            val FULL_SPAN = (#1(func_SPAN), #2(func_SPAN))
            in
              (UserCode.fn_exp_PROD_1_ACT (func_RES, func_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun fn_exp_PROD_2 (strm) = let
            val (pair_exp_RES, pair_exp_SPAN, strm') = pair_exp_NT(strm)
            val FULL_SPAN = (#1(pair_exp_SPAN), #2(pair_exp_SPAN))
            in
              ((pair_exp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => fn_exp_PROD_2(strm)
          | (Tok.NUM(_), _, strm') => fn_exp_PROD_2(strm)
          | (Tok.STRING(_), _, strm') => fn_exp_PROD_2(strm)
          | (Tok.TRUE, _, strm') => fn_exp_PROD_2(strm)
          | (Tok.FALSE, _, strm') => fn_exp_PROD_2(strm)
          | (Tok.LP, _, strm') => fn_exp_PROD_2(strm)
          | (Tok.LVERT, _, strm') => fn_exp_PROD_2(strm)
          | (Tok.FN, _, strm') => fn_exp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and pair_exp_NT (strm) = let
      fun pair_exp_PROD_1 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            fun pair_exp_PROD_1_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(exp_SPAN))
                  in
                    ((exp_RES), FULL_SPAN, strm')
                  end
            fun pair_exp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(pair_exp_PROD_1_SUBRULE_1_PRED, pair_exp_PROD_1_SUBRULE_1_NT, strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.pair_exp_PROD_1_ACT (LP_RES, RP_RES, SR_RES, exp_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun pair_exp_PROD_2 (strm) = let
            val (len_exp_RES, len_exp_SPAN, strm') = len_exp_NT(strm)
            val FULL_SPAN = (#1(len_exp_SPAN), #2(len_exp_SPAN))
            in
              ((len_exp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => pair_exp_PROD_2(strm)
          | (Tok.NUM(_), _, strm') => pair_exp_PROD_2(strm)
          | (Tok.STRING(_), _, strm') => pair_exp_PROD_2(strm)
          | (Tok.TRUE, _, strm') => pair_exp_PROD_2(strm)
          | (Tok.FALSE, _, strm') => pair_exp_PROD_2(strm)
          | (Tok.LVERT, _, strm') => pair_exp_PROD_2(strm)
          | (Tok.LP, _, strm') => pair_exp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and len_exp_NT (strm) = let
      fun len_exp_PROD_1 (strm) = let
            val (LVERT_RES, LVERT_SPAN, strm') = matchLVERT(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (RVERT_RES, RVERT_SPAN, strm') = matchRVERT(strm')
            val FULL_SPAN = (#1(LVERT_SPAN), #2(RVERT_SPAN))
            in
              (UserCode.len_exp_PROD_1_ACT (exp_RES, LVERT_RES, RVERT_RES, exp_SPAN : (Lex.pos * Lex.pos), LVERT_SPAN : (Lex.pos * Lex.pos), RVERT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun len_exp_PROD_2 (strm) = let
            val (lit_id_exp_RES, lit_id_exp_SPAN, strm') = lit_id_exp_NT(strm)
            val FULL_SPAN = (#1(lit_id_exp_SPAN), #2(lit_id_exp_SPAN))
            in
              ((lit_id_exp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => len_exp_PROD_2(strm)
          | (Tok.NUM(_), _, strm') => len_exp_PROD_2(strm)
          | (Tok.STRING(_), _, strm') => len_exp_PROD_2(strm)
          | (Tok.TRUE, _, strm') => len_exp_PROD_2(strm)
          | (Tok.FALSE, _, strm') => len_exp_PROD_2(strm)
          | (Tok.LVERT, _, strm') => len_exp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and func_NT (strm) = let
      val (FN_RES, FN_SPAN, strm') = matchFN(strm)
      val (ID_RES, ID_SPAN, strm') = matchID(strm')
      val (DARROW_RES, DARROW_SPAN, strm') = matchDARROW(strm')
      val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
      val (END_RES, END_SPAN, strm') = matchEND(strm')
      val FULL_SPAN = (#1(FN_SPAN), #2(END_SPAN))
      in
        (UserCode.func_PROD_1_ACT (FN_RES, ID_RES, END_RES, exp_RES, DARROW_RES, FN_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), END_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun pgm_NT (strm) = let
      fun pgm_PROD_1_SUBRULE_1_NT (strm) = let
            val (SR_RES, SR_SPAN, strm') = let
            fun pgm_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  fun pgm_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                        val (VAL_RES, VAL_SPAN, strm') = matchVAL(strm)
                        val FULL_SPAN = (#1(VAL_SPAN), #2(VAL_SPAN))
                        in
                          (UserCode.pgm_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (VAL_RES, VAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun pgm_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                        val (VALREC_RES, VALREC_SPAN, strm') = matchVALREC(strm)
                        val FULL_SPAN = (#1(VALREC_SPAN), #2(VALREC_SPAN))
                        in
                          (UserCode.pgm_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (VALREC_RES, VALREC_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.VALREC, _, strm') =>
                          pgm_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2(strm)
                      | (Tok.VAL, _, strm') =>
                          pgm_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1(strm)
                      | _ => fail()
                    (* end case *))
                  end
            in
              pgm_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT(strm)
            end
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val FULL_SPAN = (#1(SR_SPAN), #2(exp_SPAN))
            in
              (UserCode.pgm_PROD_1_SUBRULE_1_PROD_1_ACT (EQ_RES, ID_RES, SR_RES, exp_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun pgm_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.VAL, _, strm') => true
              | (Tok.VALREC, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.posclos(pgm_PROD_1_SUBRULE_1_PRED, pgm_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        (UserCode.pgm_PROD_1_ACT (SR_RES, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
in
  (pgm_NT, exp_NT)
end
val pgm_NT =  fn s => unwrap (Err.launch (eh, lexFn, pgm_NT , true) s)
val exp_NT =  fn s => unwrap (Err.launch (eh, lexFn, exp_NT , false) s)

in (pgm_NT, exp_NT) end
  in
fun parse lexFn  s = let val (pgm_NT, exp_NT) = mk lexFn in pgm_NT s end

fun parseexp lexFn  s = let val (pgm_NT, exp_NT) = mk lexFn in exp_NT s end

  end

end
