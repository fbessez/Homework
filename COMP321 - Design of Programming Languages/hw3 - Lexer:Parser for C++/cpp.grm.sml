structure 
CPPGrmTokens = struct

    datatype token = EOF
      | RETURN
      | PRINT
      | COMMENT
      | ELSE
      | IF
      | FOR
      | WHILE
      | DO
      | RPAREN
      | LPAREN
      | RC
      | LC
      | C
      | SEMI
      | TYPE of string
      | STRING of string
      | ID of string
      | BOOL of string
      | GT
      | LT
      | EQ
      | TIMES
      | COLON
      | Q
      | NEGATION
      | DECREMENT
      | INCREMENT
      | RSHIFT
      | LSHIFT
      | DISJUNCTION
      | CONJUNCTION
      | MOD
      | DIVIDE
      | MINUS
      | PLUS
      | DOUBLE of real
      | NUM of int

    val allToks = [EOF, RETURN, PRINT, COMMENT, ELSE, IF, FOR, WHILE, DO, RPAREN, LPAREN, RC, LC, C, SEMI, GT, LT, EQ, TIMES, COLON, Q, NEGATION, DECREMENT, INCREMENT, RSHIFT, LSHIFT, DISJUNCTION, CONJUNCTION, MOD, DIVIDE, MINUS, PLUS]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (RETURN) => "RETURN"
  | (PRINT) => "PRINT"
  | (COMMENT) => "COMMENT"
  | (ELSE) => "ELSE"
  | (IF) => "IF"
  | (FOR) => "FOR"
  | (WHILE) => "WHILE"
  | (DO) => "DO"
  | (RPAREN) => "RPAREN"
  | (LPAREN) => "LPAREN"
  | (RC) => "RC"
  | (LC) => "LC"
  | (C) => "C"
  | (SEMI) => "SEMI"
  | (TYPE(_)) => "TYPE"
  | (STRING(_)) => "STRING"
  | (ID(_)) => "ID"
  | (BOOL(_)) => "BOOL"
  | (GT) => "GT"
  | (LT) => "LT"
  | (EQ) => "EQ"
  | (TIMES) => "TIMES"
  | (COLON) => "COLON"
  | (Q) => "Q"
  | (NEGATION) => "NEGATION"
  | (DECREMENT) => "DECREMENT"
  | (INCREMENT) => "INCREMENT"
  | (RSHIFT) => "RSHIFT"
  | (LSHIFT) => "LSHIFT"
  | (DISJUNCTION) => "DISJUNCTION"
  | (CONJUNCTION) => "CONJUNCTION"
  | (MOD) => "MOD"
  | (DIVIDE) => "DIVIDE"
  | (MINUS) => "MINUS"
  | (PLUS) => "PLUS"
  | (DOUBLE(_)) => "DOUBLE"
  | (NUM(_)) => "NUM"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (RETURN) => false
  | (PRINT) => false
  | (COMMENT) => false
  | (ELSE) => false
  | (IF) => false
  | (FOR) => false
  | (WHILE) => false
  | (DO) => false
  | (RPAREN) => false
  | (LPAREN) => false
  | (RC) => false
  | (LC) => false
  | (C) => false
  | (SEMI) => false
  | (TYPE(_)) => false
  | (STRING(_)) => false
  | (ID(_)) => false
  | (BOOL(_)) => false
  | (GT) => false
  | (LT) => false
  | (EQ) => false
  | (TIMES) => false
  | (COLON) => false
  | (Q) => false
  | (NEGATION) => false
  | (DECREMENT) => false
  | (INCREMENT) => false
  | (RSHIFT) => false
  | (LSHIFT) => false
  | (DISJUNCTION) => false
  | (CONJUNCTION) => false
  | (MOD) => false
  | (DIVIDE) => false
  | (MINUS) => false
  | (PLUS) => false
  | (DOUBLE(_)) => false
  | (NUM(_)) => false
(* end case *))

  fun isEOF EOF = true
    | isEOF _ = false

end

functor CPPGrmParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
CPPGrmTokens
    structure UserCode =
      struct

  open Ast
  fun leftAssoc (rator : exp*exp->exp) (e : exp) (es : exp list) : exp = 
	case es of
		[] => e
	      | e' :: es => leftAssoc rator (rator(e, e')) es	
  fun leftAssoc2 (e : exp) (ratorsAndRands : ((exp*exp -> exp)*exp) list) : exp = 
	case ratorsAndRands of
	[] => e
      | x::xs => let val (operator, expression) = x in leftAssoc2 (operator(e, expression)) (xs)
		end

fun pgm_PROD_1_ACT (decl, decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( PPro(decl) )
fun decl_PROD_1_ACT (EQ, ID, SR, exp, SEMI, TYPE, COMMENT, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), COMMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (DInit(TYPE, ((ID, exp) :: SR)))
fun decl_PROD_2_ACT (ID, SR, SEMI, TYPE, COMMENT, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), COMMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (DDecl(TYPE, (ID :: SR)))
fun decl_PROD_3_ACT (ID, SR, SEMI, RPAREN, TYPE1, TYPE2, LPAREN, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), TYPE1_SPAN : (Lex.pos * Lex.pos), TYPE2_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (FProo(TYPE1, ID, TYPE2::SR))
fun decl_PROD_4_ACT (ID, SR, TYPE, tyid, RPAREN, fun_rest, COMMENT, LPAREN, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), tyid_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), fun_rest_SPAN : (Lex.pos * Lex.pos), COMMENT_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case fun_rest of NONE => (FPro(TYPE, ID, tyid :: SR)) | SOME s => (FDefi(TYPE, ID, tyid :: SR, s)))
fun statement_PROD_1_ACT (EQ, ID, SR, exp, TYPE, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SInit(TYPE, ((ID, exp) :: SR)))
fun statement_PROD_2_ACT (ID, SR, TYPE, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SDecl(TYPE, (ID :: SR)))
fun statement_PROD_3_ACT (EQ, ID, exp, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SReg(ID, exp))
fun statement_PROD_4_ACT (DO, exp, SEMI, RPAREN, WHILE, LPAREN, statement, DO_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), WHILE_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SDW(statement, exp))
fun statement_PROD_5_ACT (exp, RPAREN, PRINT, LPAREN, exp_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), PRINT_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SPrint(exp))
fun statement_PROD_6_ACT (exp, RETURN, exp_SPAN : (Lex.pos * Lex.pos), RETURN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SReturn(exp))
fun statement_PROD_7_ACT (IF, exp, RPAREN, if_rest, LPAREN, statement, IF_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), if_rest_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case if_rest of NONE => SIf(statement, exp) | SOME s => SIfEl(statement, s, exp))
fun statement_PROD_8_ACT (FOR, exp1, exp2, RPAREN, statement1, statement2, SEMI1, SEMI2, LPAREN, FOR_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), statement1_SPAN : (Lex.pos * Lex.pos), statement2_SPAN : (Lex.pos * Lex.pos), SEMI1_SPAN : (Lex.pos * Lex.pos), SEMI2_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SFor(statement2, statement1, exp1, exp2))
fun fun_rest_PROD_1_ACT (SEMI, SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (NONE)
fun fun_rest_PROD_2_ACT (LC, RC, SR, LC_SPAN : (Lex.pos * Lex.pos), RC_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SOME SR)
fun if_rest_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (NONE)
fun if_rest_PROD_2_ACT (ELSE, SEMI, statement, ELSE_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SOME statement)
fun tyid_PROD_1_ACT (ID, TYPE, ID_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (TYPE, ID)
fun exp_PROD_1_ACT (e_mu, INCREMENT, e_mu_SPAN : (Lex.pos * Lex.pos), INCREMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EIncrement e_mu)
fun exp_PROD_2_ACT (e_mu, DECREMENT, e_mu_SPAN : (Lex.pos * Lex.pos), DECREMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EDecrement e_mu)
fun exp_PROD_3_ACT (e_at, INCREMENT, e_at_SPAN : (Lex.pos * Lex.pos), INCREMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EIncrement e_at)
fun exp_PROD_4_ACT (e_at, DECREMENT, e_at_SPAN : (Lex.pos * Lex.pos), DECREMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EDecrement e_at)
fun exp_PROD_5_SUBRULE_1_PROD_1_ACT (PLUS, e_mu, PLUS_SPAN : (Lex.pos * Lex.pos), e_mu_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EPlus, e_mu)
fun exp_PROD_5_SUBRULE_1_PROD_2_ACT (e_mu, MINUS, e_mu_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EMinus, e_mu)
fun exp_PROD_5_SUBRULE_1_PROD_3_ACT (e_mu, CONJUNCTION, e_mu_SPAN : (Lex.pos * Lex.pos), CONJUNCTION_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EConjunction, e_mu)
fun exp_PROD_5_SUBRULE_1_PROD_4_ACT (e_mu, DISJUNCTION, e_mu_SPAN : (Lex.pos * Lex.pos), DISJUNCTION_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EDisjunction, e_mu)
fun exp_PROD_5_SUBRULE_1_PROD_5_ACT (e_mu, LSHIFT, e_mu_SPAN : (Lex.pos * Lex.pos), LSHIFT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (ELShift, e_mu)
fun exp_PROD_5_SUBRULE_1_PROD_6_ACT (e_mu, RSHIFT, e_mu_SPAN : (Lex.pos * Lex.pos), RSHIFT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (ERShift, e_mu)
fun exp_PROD_5_SUBRULE_1_PROD_7_ACT (MOD, e_mu, MOD_SPAN : (Lex.pos * Lex.pos), e_mu_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EMod, e_mu)
fun exp_PROD_5_ACT (SR, e_mu, SR_SPAN : (Lex.pos * Lex.pos), e_mu_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (leftAssoc2 e_mu SR)
fun exp_PROD_6_ACT (SR, e_at, SR_SPAN : (Lex.pos * Lex.pos), e_at_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (ELess(e_at, SR))
fun exp_PROD_7_ACT (SR, e_at, SR_SPAN : (Lex.pos * Lex.pos), e_at_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EGreat(e_at, SR))
fun exp_PROD_8_ACT (EQ, SR, e_at, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), e_at_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EEqual(e_at, SR))
fun exp_PROD_9_ACT (e_mu, NEGATION, e_mu_SPAN : (Lex.pos * Lex.pos), NEGATION_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (ENegation e_mu)
fun exp_PROD_10_ACT (Q, e_at, COLON, e_mu1, e_mu2, Q_SPAN : (Lex.pos * Lex.pos), e_at_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), e_mu1_SPAN : (Lex.pos * Lex.pos), e_mu2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (ETernary (e_at, e_mu1, e_mu2))
fun e_mu_PROD_1_SUBRULE_1_PROD_1_ACT (e_at, TIMES, e_at_SPAN : (Lex.pos * Lex.pos), TIMES_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (ETimes, e_at)
fun e_mu_PROD_1_SUBRULE_1_PROD_2_ACT (e_at, DIVIDE, e_at_SPAN : (Lex.pos * Lex.pos), DIVIDE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EDivide, e_at)
fun e_mu_PROD_1_ACT (SR, e_at, SR_SPAN : (Lex.pos * Lex.pos), e_at_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (leftAssoc2 e_at SR)
fun e_at_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EId ID)
fun e_at_PROD_2_ACT (DOUBLE, DOUBLE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EDouble DOUBLE)
fun e_at_PROD_3_ACT (BOOL, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EBool BOOL)
fun e_at_PROD_4_ACT (NUM, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (ENum NUM)
fun e_at_PROD_5_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (EString STRING)
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
fun matchRETURN strm = (case (lex(strm))
 of (Tok.RETURN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPRINT strm = (case (lex(strm))
 of (Tok.PRINT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMENT strm = (case (lex(strm))
 of (Tok.COMMENT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchELSE strm = (case (lex(strm))
 of (Tok.ELSE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchIF strm = (case (lex(strm))
 of (Tok.IF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFOR strm = (case (lex(strm))
 of (Tok.FOR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchWHILE strm = (case (lex(strm))
 of (Tok.WHILE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDO strm = (case (lex(strm))
 of (Tok.DO, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRPAREN strm = (case (lex(strm))
 of (Tok.RPAREN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLPAREN strm = (case (lex(strm))
 of (Tok.LPAREN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRC strm = (case (lex(strm))
 of (Tok.RC, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLC strm = (case (lex(strm))
 of (Tok.LC, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchC strm = (case (lex(strm))
 of (Tok.C, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTYPE strm = (case (lex(strm))
 of (Tok.TYPE(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSTRING strm = (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchBOOL strm = (case (lex(strm))
 of (Tok.BOOL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchGT strm = (case (lex(strm))
 of (Tok.GT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLT strm = (case (lex(strm))
 of (Tok.LT, span, strm') => ((), span, strm')
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
fun matchCOLON strm = (case (lex(strm))
 of (Tok.COLON, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchQ strm = (case (lex(strm))
 of (Tok.Q, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNEGATION strm = (case (lex(strm))
 of (Tok.NEGATION, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDECREMENT strm = (case (lex(strm))
 of (Tok.DECREMENT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchINCREMENT strm = (case (lex(strm))
 of (Tok.INCREMENT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRSHIFT strm = (case (lex(strm))
 of (Tok.RSHIFT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLSHIFT strm = (case (lex(strm))
 of (Tok.LSHIFT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDISJUNCTION strm = (case (lex(strm))
 of (Tok.DISJUNCTION, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCONJUNCTION strm = (case (lex(strm))
 of (Tok.CONJUNCTION, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMOD strm = (case (lex(strm))
 of (Tok.MOD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDIVIDE strm = (case (lex(strm))
 of (Tok.DIVIDE, span, strm') => ((), span, strm')
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
fun matchDOUBLE strm = (case (lex(strm))
 of (Tok.DOUBLE(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNUM strm = (case (lex(strm))
 of (Tok.NUM(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))

val (pgm_NT, exp_NT) = 
let
fun e_at_NT (strm) = let
      fun e_at_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.e_at_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun e_at_PROD_2 (strm) = let
            val (DOUBLE_RES, DOUBLE_SPAN, strm') = matchDOUBLE(strm)
            val FULL_SPAN = (#1(DOUBLE_SPAN), #2(DOUBLE_SPAN))
            in
              (UserCode.e_at_PROD_2_ACT (DOUBLE_RES, DOUBLE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun e_at_PROD_3 (strm) = let
            val (BOOL_RES, BOOL_SPAN, strm') = matchBOOL(strm)
            val FULL_SPAN = (#1(BOOL_SPAN), #2(BOOL_SPAN))
            in
              (UserCode.e_at_PROD_3_ACT (BOOL_RES, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun e_at_PROD_4 (strm) = let
            val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
            val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
            in
              (UserCode.e_at_PROD_4_ACT (NUM_RES, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun e_at_PROD_5 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.e_at_PROD_5_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.STRING(_), _, strm') => e_at_PROD_5(strm)
          | (Tok.BOOL(_), _, strm') => e_at_PROD_3(strm)
          | (Tok.ID(_), _, strm') => e_at_PROD_1(strm)
          | (Tok.DOUBLE(_), _, strm') => e_at_PROD_2(strm)
          | (Tok.NUM(_), _, strm') => e_at_PROD_4(strm)
          | _ => fail()
        (* end case *))
      end
fun e_mu_NT (strm) = let
      val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm)
      fun e_mu_PROD_1_SUBRULE_1_NT (strm) = let
            fun e_mu_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
                  val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm')
                  val FULL_SPAN = (#1(TIMES_SPAN), #2(e_at_SPAN))
                  in
                    (UserCode.e_mu_PROD_1_SUBRULE_1_PROD_1_ACT (e_at_RES, TIMES_RES, e_at_SPAN : (Lex.pos * Lex.pos), TIMES_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun e_mu_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (DIVIDE_RES, DIVIDE_SPAN, strm') = matchDIVIDE(strm)
                  val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm')
                  val FULL_SPAN = (#1(DIVIDE_SPAN), #2(e_at_SPAN))
                  in
                    (UserCode.e_mu_PROD_1_SUBRULE_1_PROD_2_ACT (e_at_RES, DIVIDE_RES, e_at_SPAN : (Lex.pos * Lex.pos), DIVIDE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.DIVIDE, _, strm') => e_mu_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.TIMES, _, strm') => e_mu_PROD_1_SUBRULE_1_PROD_1(strm)
                | _ => fail()
              (* end case *))
            end
      fun e_mu_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.DIVIDE, _, strm') => true
              | (Tok.TIMES, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(e_mu_PROD_1_SUBRULE_1_PRED, e_mu_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(e_at_SPAN), #2(SR_SPAN))
      in
        (UserCode.e_mu_PROD_1_ACT (SR_RES, e_at_RES, SR_SPAN : (Lex.pos * Lex.pos), e_at_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun exp_NT (strm) = let
      fun exp_PROD_1 (strm) = let
            val (INCREMENT_RES, INCREMENT_SPAN, strm') = matchINCREMENT(strm)
            val (e_mu_RES, e_mu_SPAN, strm') = e_mu_NT(strm')
            val FULL_SPAN = (#1(INCREMENT_SPAN), #2(e_mu_SPAN))
            in
              (UserCode.exp_PROD_1_ACT (e_mu_RES, INCREMENT_RES, e_mu_SPAN : (Lex.pos * Lex.pos), INCREMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_2 (strm) = let
            val (DECREMENT_RES, DECREMENT_SPAN, strm') = matchDECREMENT(strm)
            val (e_mu_RES, e_mu_SPAN, strm') = e_mu_NT(strm')
            val FULL_SPAN = (#1(DECREMENT_SPAN), #2(e_mu_SPAN))
            in
              (UserCode.exp_PROD_2_ACT (e_mu_RES, DECREMENT_RES, e_mu_SPAN : (Lex.pos * Lex.pos), DECREMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_3 (strm) = let
            val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm)
            val (INCREMENT_RES, INCREMENT_SPAN, strm') = matchINCREMENT(strm')
            val FULL_SPAN = (#1(e_at_SPAN), #2(INCREMENT_SPAN))
            in
              (UserCode.exp_PROD_3_ACT (e_at_RES, INCREMENT_RES, e_at_SPAN : (Lex.pos * Lex.pos), INCREMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_4 (strm) = let
            val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm)
            val (DECREMENT_RES, DECREMENT_SPAN, strm') = matchDECREMENT(strm')
            val FULL_SPAN = (#1(e_at_SPAN), #2(DECREMENT_SPAN))
            in
              (UserCode.exp_PROD_4_ACT (e_at_RES, DECREMENT_RES, e_at_SPAN : (Lex.pos * Lex.pos), DECREMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_5 (strm) = let
            val (e_mu_RES, e_mu_SPAN, strm') = e_mu_NT(strm)
            fun exp_PROD_5_SUBRULE_1_NT (strm) = let
                  fun exp_PROD_5_SUBRULE_1_PROD_1 (strm) = let
                        val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
                        val (e_mu_RES, e_mu_SPAN, strm') = e_mu_NT(strm')
                        val FULL_SPAN = (#1(PLUS_SPAN), #2(e_mu_SPAN))
                        in
                          (UserCode.exp_PROD_5_SUBRULE_1_PROD_1_ACT (PLUS_RES, e_mu_RES, PLUS_SPAN : (Lex.pos * Lex.pos), e_mu_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun exp_PROD_5_SUBRULE_1_PROD_2 (strm) = let
                        val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
                        val (e_mu_RES, e_mu_SPAN, strm') = e_mu_NT(strm')
                        val FULL_SPAN = (#1(MINUS_SPAN), #2(e_mu_SPAN))
                        in
                          (UserCode.exp_PROD_5_SUBRULE_1_PROD_2_ACT (e_mu_RES, MINUS_RES, e_mu_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun exp_PROD_5_SUBRULE_1_PROD_3 (strm) = let
                        val (CONJUNCTION_RES, CONJUNCTION_SPAN, strm') = matchCONJUNCTION(strm)
                        val (e_mu_RES, e_mu_SPAN, strm') = e_mu_NT(strm')
                        val FULL_SPAN = (#1(CONJUNCTION_SPAN), #2(e_mu_SPAN))
                        in
                          (UserCode.exp_PROD_5_SUBRULE_1_PROD_3_ACT (e_mu_RES, CONJUNCTION_RES, e_mu_SPAN : (Lex.pos * Lex.pos), CONJUNCTION_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun exp_PROD_5_SUBRULE_1_PROD_4 (strm) = let
                        val (DISJUNCTION_RES, DISJUNCTION_SPAN, strm') = matchDISJUNCTION(strm)
                        val (e_mu_RES, e_mu_SPAN, strm') = e_mu_NT(strm')
                        val FULL_SPAN = (#1(DISJUNCTION_SPAN), #2(e_mu_SPAN))
                        in
                          (UserCode.exp_PROD_5_SUBRULE_1_PROD_4_ACT (e_mu_RES, DISJUNCTION_RES, e_mu_SPAN : (Lex.pos * Lex.pos), DISJUNCTION_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun exp_PROD_5_SUBRULE_1_PROD_5 (strm) = let
                        val (LSHIFT_RES, LSHIFT_SPAN, strm') = matchLSHIFT(strm)
                        val (e_mu_RES, e_mu_SPAN, strm') = e_mu_NT(strm')
                        val FULL_SPAN = (#1(LSHIFT_SPAN), #2(e_mu_SPAN))
                        in
                          (UserCode.exp_PROD_5_SUBRULE_1_PROD_5_ACT (e_mu_RES, LSHIFT_RES, e_mu_SPAN : (Lex.pos * Lex.pos), LSHIFT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun exp_PROD_5_SUBRULE_1_PROD_6 (strm) = let
                        val (RSHIFT_RES, RSHIFT_SPAN, strm') = matchRSHIFT(strm)
                        val (e_mu_RES, e_mu_SPAN, strm') = e_mu_NT(strm')
                        val FULL_SPAN = (#1(RSHIFT_SPAN), #2(e_mu_SPAN))
                        in
                          (UserCode.exp_PROD_5_SUBRULE_1_PROD_6_ACT (e_mu_RES, RSHIFT_RES, e_mu_SPAN : (Lex.pos * Lex.pos), RSHIFT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun exp_PROD_5_SUBRULE_1_PROD_7 (strm) = let
                        val (MOD_RES, MOD_SPAN, strm') = matchMOD(strm)
                        val (e_mu_RES, e_mu_SPAN, strm') = e_mu_NT(strm')
                        val FULL_SPAN = (#1(MOD_SPAN), #2(e_mu_SPAN))
                        in
                          (UserCode.exp_PROD_5_SUBRULE_1_PROD_7_ACT (MOD_RES, e_mu_RES, MOD_SPAN : (Lex.pos * Lex.pos), e_mu_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.MOD, _, strm') =>
                          exp_PROD_5_SUBRULE_1_PROD_7(strm)
                      | (Tok.LSHIFT, _, strm') =>
                          exp_PROD_5_SUBRULE_1_PROD_5(strm)
                      | (Tok.CONJUNCTION, _, strm') =>
                          exp_PROD_5_SUBRULE_1_PROD_3(strm)
                      | (Tok.PLUS, _, strm') =>
                          exp_PROD_5_SUBRULE_1_PROD_1(strm)
                      | (Tok.MINUS, _, strm') =>
                          exp_PROD_5_SUBRULE_1_PROD_2(strm)
                      | (Tok.DISJUNCTION, _, strm') =>
                          exp_PROD_5_SUBRULE_1_PROD_4(strm)
                      | (Tok.RSHIFT, _, strm') =>
                          exp_PROD_5_SUBRULE_1_PROD_6(strm)
                      | _ => fail()
                    (* end case *))
                  end
            fun exp_PROD_5_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.PLUS, _, strm') => true
                    | (Tok.MINUS, _, strm') => true
                    | (Tok.MOD, _, strm') => true
                    | (Tok.CONJUNCTION, _, strm') => true
                    | (Tok.DISJUNCTION, _, strm') => true
                    | (Tok.LSHIFT, _, strm') => true
                    | (Tok.RSHIFT, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(exp_PROD_5_SUBRULE_1_PRED, exp_PROD_5_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(e_mu_SPAN), #2(SR_SPAN))
            in
              (UserCode.exp_PROD_5_ACT (SR_RES, e_mu_RES, SR_SPAN : (Lex.pos * Lex.pos), e_mu_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_6 (strm) = let
            val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm)
            val (SR_RES, SR_SPAN, strm') = let
            fun exp_PROD_6_SUBRULE_1_NT (strm) = let
                  val (LT_RES, LT_SPAN, strm') = matchLT(strm)
                  val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm')
                  val FULL_SPAN = (#1(LT_SPAN), #2(e_at_SPAN))
                  in
                    ((e_at_RES), FULL_SPAN, strm')
                  end
            in
              exp_PROD_6_SUBRULE_1_NT(strm')
            end
            val FULL_SPAN = (#1(e_at_SPAN), #2(SR_SPAN))
            in
              (UserCode.exp_PROD_6_ACT (SR_RES, e_at_RES, SR_SPAN : (Lex.pos * Lex.pos), e_at_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_7 (strm) = let
            val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm)
            val (SR_RES, SR_SPAN, strm') = let
            fun exp_PROD_7_SUBRULE_1_NT (strm) = let
                  val (GT_RES, GT_SPAN, strm') = matchGT(strm)
                  val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm')
                  val FULL_SPAN = (#1(GT_SPAN), #2(e_at_SPAN))
                  in
                    ((e_at_RES), FULL_SPAN, strm')
                  end
            in
              exp_PROD_7_SUBRULE_1_NT(strm')
            end
            val FULL_SPAN = (#1(e_at_SPAN), #2(SR_SPAN))
            in
              (UserCode.exp_PROD_7_ACT (SR_RES, e_at_RES, SR_SPAN : (Lex.pos * Lex.pos), e_at_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_8 (strm) = let
            val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm)
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (SR_RES, SR_SPAN, strm') = let
            fun exp_PROD_8_SUBRULE_1_NT (strm) = let
                  val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm)
                  val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm')
                  val FULL_SPAN = (#1(EQ_SPAN), #2(e_at_SPAN))
                  in
                    ((e_at_RES), FULL_SPAN, strm')
                  end
            in
              exp_PROD_8_SUBRULE_1_NT(strm')
            end
            val FULL_SPAN = (#1(e_at_SPAN), #2(SR_SPAN))
            in
              (UserCode.exp_PROD_8_ACT (EQ_RES, SR_RES, e_at_RES, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), e_at_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_9 (strm) = let
            val (NEGATION_RES, NEGATION_SPAN, strm') = matchNEGATION(strm)
            val (e_mu_RES, e_mu_SPAN, strm') = e_mu_NT(strm')
            val FULL_SPAN = (#1(NEGATION_SPAN), #2(e_mu_SPAN))
            in
              (UserCode.exp_PROD_9_ACT (e_mu_RES, NEGATION_RES, e_mu_SPAN : (Lex.pos * Lex.pos), NEGATION_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_10 (strm) = let
            val (e_at_RES, e_at_SPAN, strm') = e_at_NT(strm)
            val (Q_RES, Q_SPAN, strm') = matchQ(strm')
            val (e_mu1_RES, e_mu1_SPAN, strm') = e_mu_NT(strm')
            val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm')
            val (e_mu2_RES, e_mu2_SPAN, strm') = e_mu_NT(strm')
            val FULL_SPAN = (#1(e_at_SPAN), #2(e_mu2_SPAN))
            in
              (UserCode.exp_PROD_10_ACT (Q_RES, e_at_RES, COLON_RES, e_mu1_RES, e_mu2_RES, Q_SPAN : (Lex.pos * Lex.pos), e_at_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), e_mu1_SPAN : (Lex.pos * Lex.pos), e_mu2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.DECREMENT, _, strm') => exp_PROD_2(strm)
          | (Tok.INCREMENT, _, strm') => exp_PROD_1(strm)
          | (Tok.NUM(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.INCREMENT, _, strm') => exp_PROD_3(strm)
                | (Tok.PLUS, _, strm') => exp_PROD_5(strm)
                | (Tok.MINUS, _, strm') => exp_PROD_5(strm)
                | (Tok.DIVIDE, _, strm') => exp_PROD_5(strm)
                | (Tok.MOD, _, strm') => exp_PROD_5(strm)
                | (Tok.CONJUNCTION, _, strm') => exp_PROD_5(strm)
                | (Tok.DISJUNCTION, _, strm') => exp_PROD_5(strm)
                | (Tok.LSHIFT, _, strm') => exp_PROD_5(strm)
                | (Tok.RSHIFT, _, strm') => exp_PROD_5(strm)
                | (Tok.TIMES, _, strm') => exp_PROD_5(strm)
                | (Tok.SEMI, _, strm') => exp_PROD_5(strm)
                | (Tok.C, _, strm') => exp_PROD_5(strm)
                | (Tok.RPAREN, _, strm') => exp_PROD_5(strm)
                | (Tok.GT, _, strm') => exp_PROD_7(strm)
                | (Tok.Q, _, strm') => exp_PROD_10(strm)
                | (Tok.EQ, _, strm') => exp_PROD_8(strm)
                | (Tok.LT, _, strm') => exp_PROD_6(strm)
                | (Tok.DECREMENT, _, strm') => exp_PROD_4(strm)
                | _ => fail()
              (* end case *))
          | (Tok.DOUBLE(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.INCREMENT, _, strm') => exp_PROD_3(strm)
                | (Tok.PLUS, _, strm') => exp_PROD_5(strm)
                | (Tok.MINUS, _, strm') => exp_PROD_5(strm)
                | (Tok.DIVIDE, _, strm') => exp_PROD_5(strm)
                | (Tok.MOD, _, strm') => exp_PROD_5(strm)
                | (Tok.CONJUNCTION, _, strm') => exp_PROD_5(strm)
                | (Tok.DISJUNCTION, _, strm') => exp_PROD_5(strm)
                | (Tok.LSHIFT, _, strm') => exp_PROD_5(strm)
                | (Tok.RSHIFT, _, strm') => exp_PROD_5(strm)
                | (Tok.TIMES, _, strm') => exp_PROD_5(strm)
                | (Tok.SEMI, _, strm') => exp_PROD_5(strm)
                | (Tok.C, _, strm') => exp_PROD_5(strm)
                | (Tok.RPAREN, _, strm') => exp_PROD_5(strm)
                | (Tok.GT, _, strm') => exp_PROD_7(strm)
                | (Tok.Q, _, strm') => exp_PROD_10(strm)
                | (Tok.EQ, _, strm') => exp_PROD_8(strm)
                | (Tok.LT, _, strm') => exp_PROD_6(strm)
                | (Tok.DECREMENT, _, strm') => exp_PROD_4(strm)
                | _ => fail()
              (* end case *))
          | (Tok.BOOL(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.INCREMENT, _, strm') => exp_PROD_3(strm)
                | (Tok.PLUS, _, strm') => exp_PROD_5(strm)
                | (Tok.MINUS, _, strm') => exp_PROD_5(strm)
                | (Tok.DIVIDE, _, strm') => exp_PROD_5(strm)
                | (Tok.MOD, _, strm') => exp_PROD_5(strm)
                | (Tok.CONJUNCTION, _, strm') => exp_PROD_5(strm)
                | (Tok.DISJUNCTION, _, strm') => exp_PROD_5(strm)
                | (Tok.LSHIFT, _, strm') => exp_PROD_5(strm)
                | (Tok.RSHIFT, _, strm') => exp_PROD_5(strm)
                | (Tok.TIMES, _, strm') => exp_PROD_5(strm)
                | (Tok.SEMI, _, strm') => exp_PROD_5(strm)
                | (Tok.C, _, strm') => exp_PROD_5(strm)
                | (Tok.RPAREN, _, strm') => exp_PROD_5(strm)
                | (Tok.GT, _, strm') => exp_PROD_7(strm)
                | (Tok.Q, _, strm') => exp_PROD_10(strm)
                | (Tok.EQ, _, strm') => exp_PROD_8(strm)
                | (Tok.LT, _, strm') => exp_PROD_6(strm)
                | (Tok.DECREMENT, _, strm') => exp_PROD_4(strm)
                | _ => fail()
              (* end case *))
          | (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.INCREMENT, _, strm') => exp_PROD_3(strm)
                | (Tok.PLUS, _, strm') => exp_PROD_5(strm)
                | (Tok.MINUS, _, strm') => exp_PROD_5(strm)
                | (Tok.DIVIDE, _, strm') => exp_PROD_5(strm)
                | (Tok.MOD, _, strm') => exp_PROD_5(strm)
                | (Tok.CONJUNCTION, _, strm') => exp_PROD_5(strm)
                | (Tok.DISJUNCTION, _, strm') => exp_PROD_5(strm)
                | (Tok.LSHIFT, _, strm') => exp_PROD_5(strm)
                | (Tok.RSHIFT, _, strm') => exp_PROD_5(strm)
                | (Tok.TIMES, _, strm') => exp_PROD_5(strm)
                | (Tok.SEMI, _, strm') => exp_PROD_5(strm)
                | (Tok.C, _, strm') => exp_PROD_5(strm)
                | (Tok.RPAREN, _, strm') => exp_PROD_5(strm)
                | (Tok.GT, _, strm') => exp_PROD_7(strm)
                | (Tok.Q, _, strm') => exp_PROD_10(strm)
                | (Tok.EQ, _, strm') => exp_PROD_8(strm)
                | (Tok.LT, _, strm') => exp_PROD_6(strm)
                | (Tok.DECREMENT, _, strm') => exp_PROD_4(strm)
                | _ => fail()
              (* end case *))
          | (Tok.STRING(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.INCREMENT, _, strm') => exp_PROD_3(strm)
                | (Tok.PLUS, _, strm') => exp_PROD_5(strm)
                | (Tok.MINUS, _, strm') => exp_PROD_5(strm)
                | (Tok.DIVIDE, _, strm') => exp_PROD_5(strm)
                | (Tok.MOD, _, strm') => exp_PROD_5(strm)
                | (Tok.CONJUNCTION, _, strm') => exp_PROD_5(strm)
                | (Tok.DISJUNCTION, _, strm') => exp_PROD_5(strm)
                | (Tok.LSHIFT, _, strm') => exp_PROD_5(strm)
                | (Tok.RSHIFT, _, strm') => exp_PROD_5(strm)
                | (Tok.TIMES, _, strm') => exp_PROD_5(strm)
                | (Tok.SEMI, _, strm') => exp_PROD_5(strm)
                | (Tok.C, _, strm') => exp_PROD_5(strm)
                | (Tok.RPAREN, _, strm') => exp_PROD_5(strm)
                | (Tok.GT, _, strm') => exp_PROD_7(strm)
                | (Tok.Q, _, strm') => exp_PROD_10(strm)
                | (Tok.EQ, _, strm') => exp_PROD_8(strm)
                | (Tok.LT, _, strm') => exp_PROD_6(strm)
                | (Tok.DECREMENT, _, strm') => exp_PROD_4(strm)
                | _ => fail()
              (* end case *))
          | (Tok.NEGATION, _, strm') => exp_PROD_9(strm)
          | _ => fail()
        (* end case *))
      end
fun statement_NT (strm) = let
      fun statement_PROD_1 (strm) = let
            val (TYPE_RES, TYPE_SPAN, strm') = matchTYPE(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            fun statement_PROD_1_SUBRULE_1_NT (strm) = let
                  val (C_RES, C_SPAN, strm') = matchC(strm)
                  val (ID_RES, ID_SPAN, strm') = matchID(strm')
                  val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
                  val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
                  val FULL_SPAN = (#1(C_SPAN), #2(exp_SPAN))
                  in
                    ((ID_RES, exp_RES), FULL_SPAN, strm')
                  end
            fun statement_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.C, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(statement_PROD_1_SUBRULE_1_PRED, statement_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(TYPE_SPAN), #2(SR_SPAN))
            in
              (UserCode.statement_PROD_1_ACT (EQ_RES, ID_RES, SR_RES, exp_RES, TYPE_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun statement_PROD_2 (strm) = let
            val (TYPE_RES, TYPE_SPAN, strm') = matchTYPE(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            fun statement_PROD_2_SUBRULE_1_NT (strm) = let
                  val (C_RES, C_SPAN, strm') = matchC(strm)
                  val (ID_RES, ID_SPAN, strm') = matchID(strm')
                  val FULL_SPAN = (#1(C_SPAN), #2(ID_SPAN))
                  in
                    ((ID_RES), FULL_SPAN, strm')
                  end
            fun statement_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.C, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(statement_PROD_2_SUBRULE_1_PRED, statement_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(TYPE_SPAN), #2(SR_SPAN))
            in
              (UserCode.statement_PROD_2_ACT (ID_RES, SR_RES, TYPE_RES, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun statement_PROD_3 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(exp_SPAN))
            in
              (UserCode.statement_PROD_3_ACT (EQ_RES, ID_RES, exp_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun statement_PROD_4 (strm) = let
            val (DO_RES, DO_SPAN, strm') = matchDO(strm)
            val (statement_RES, statement_SPAN, strm') = statement_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (WHILE_RES, WHILE_SPAN, strm') = matchWHILE(strm')
            val (LPAREN_RES, LPAREN_SPAN, strm') = matchLPAREN(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (RPAREN_RES, RPAREN_SPAN, strm') = matchRPAREN(strm')
            val FULL_SPAN = (#1(DO_SPAN), #2(RPAREN_SPAN))
            in
              (UserCode.statement_PROD_4_ACT (DO_RES, exp_RES, SEMI_RES, RPAREN_RES, WHILE_RES, LPAREN_RES, statement_RES, DO_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), WHILE_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun statement_PROD_5 (strm) = let
            val (PRINT_RES, PRINT_SPAN, strm') = matchPRINT(strm)
            val (LPAREN_RES, LPAREN_SPAN, strm') = matchLPAREN(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (RPAREN_RES, RPAREN_SPAN, strm') = matchRPAREN(strm')
            val FULL_SPAN = (#1(PRINT_SPAN), #2(RPAREN_SPAN))
            in
              (UserCode.statement_PROD_5_ACT (exp_RES, RPAREN_RES, PRINT_RES, LPAREN_RES, exp_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), PRINT_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun statement_PROD_6 (strm) = let
            val (RETURN_RES, RETURN_SPAN, strm') = matchRETURN(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val FULL_SPAN = (#1(RETURN_SPAN), #2(exp_SPAN))
            in
              (UserCode.statement_PROD_6_ACT (exp_RES, RETURN_RES, exp_SPAN : (Lex.pos * Lex.pos), RETURN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun statement_PROD_7 (strm) = let
            val (IF_RES, IF_SPAN, strm') = matchIF(strm)
            val (LPAREN_RES, LPAREN_SPAN, strm') = matchLPAREN(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (RPAREN_RES, RPAREN_SPAN, strm') = matchRPAREN(strm')
            val (statement_RES, statement_SPAN, strm') = statement_NT(strm')
            val (if_rest_RES, if_rest_SPAN, strm') = if_rest_NT(strm')
            val FULL_SPAN = (#1(IF_SPAN), #2(if_rest_SPAN))
            in
              (UserCode.statement_PROD_7_ACT (IF_RES, exp_RES, RPAREN_RES, if_rest_RES, LPAREN_RES, statement_RES, IF_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), if_rest_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun statement_PROD_8 (strm) = let
            val (FOR_RES, FOR_SPAN, strm') = matchFOR(strm)
            val (LPAREN_RES, LPAREN_SPAN, strm') = matchLPAREN(strm')
            val (statement1_RES, statement1_SPAN, strm') = statement_NT(strm')
            val (SEMI1_RES, SEMI1_SPAN, strm') = matchSEMI(strm')
            val (exp1_RES, exp1_SPAN, strm') = exp_NT(strm')
            val (SEMI2_RES, SEMI2_SPAN, strm') = matchSEMI(strm')
            val (exp2_RES, exp2_SPAN, strm') = exp_NT(strm')
            val (RPAREN_RES, RPAREN_SPAN, strm') = matchRPAREN(strm')
            val (statement2_RES, statement2_SPAN, strm') = statement_NT(strm')
            val FULL_SPAN = (#1(FOR_SPAN), #2(statement2_SPAN))
            in
              (UserCode.statement_PROD_8_ACT (FOR_RES, exp1_RES, exp2_RES, RPAREN_RES, statement1_RES, statement2_RES, SEMI1_RES, SEMI2_RES, LPAREN_RES, FOR_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), statement1_SPAN : (Lex.pos * Lex.pos), statement2_SPAN : (Lex.pos * Lex.pos), SEMI1_SPAN : (Lex.pos * Lex.pos), SEMI2_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.FOR, _, strm') => statement_PROD_8(strm)
          | (Tok.RETURN, _, strm') => statement_PROD_6(strm)
          | (Tok.DO, _, strm') => statement_PROD_4(strm)
          | (Tok.TYPE(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.ID(_), _, strm') =>
                    (case (lex(strm'))
                     of (Tok.SEMI, _, strm') => statement_PROD_2(strm)
                      | (Tok.C, _, strm') => statement_PROD_2(strm)
                      | (Tok.EQ, _, strm') => statement_PROD_1(strm)
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.ID(_), _, strm') => statement_PROD_3(strm)
          | (Tok.PRINT, _, strm') => statement_PROD_5(strm)
          | (Tok.IF, _, strm') => statement_PROD_7(strm)
          | _ => fail()
        (* end case *))
      end
and if_rest_NT (strm) = let
      fun if_rest_PROD_1 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.if_rest_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      fun if_rest_PROD_2 (strm) = let
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
            val (ELSE_RES, ELSE_SPAN, strm') = matchELSE(strm')
            val (statement_RES, statement_SPAN, strm') = statement_NT(strm')
            val FULL_SPAN = (#1(SEMI_SPAN), #2(statement_SPAN))
            in
              (UserCode.if_rest_PROD_2_ACT (ELSE_RES, SEMI_RES, statement_RES, ELSE_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SEMI, _, strm') =>
              (case (lex(strm'))
               of (Tok.WHILE, _, strm') => if_rest_PROD_1(strm)
                | (Tok.ELSE, _, strm') => if_rest_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun fun_rest_NT (strm) = let
      fun fun_rest_PROD_1 (strm) = let
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
            val FULL_SPAN = (#1(SEMI_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.fun_rest_PROD_1_ACT (SEMI_RES, SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun fun_rest_PROD_2 (strm) = let
            val (LC_RES, LC_SPAN, strm') = matchLC(strm)
            fun fun_rest_PROD_2_SUBRULE_1_NT (strm) = let
                  val (statement_RES, statement_SPAN, strm') = statement_NT(strm)
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
                  val FULL_SPAN = (#1(statement_SPAN), #2(SEMI_SPAN))
                  in
                    ((statement_RES), FULL_SPAN, strm')
                  end
            fun fun_rest_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.ID(_), _, strm') => true
                    | (Tok.TYPE(_), _, strm') => true
                    | (Tok.DO, _, strm') => true
                    | (Tok.FOR, _, strm') => true
                    | (Tok.IF, _, strm') => true
                    | (Tok.PRINT, _, strm') => true
                    | (Tok.RETURN, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(fun_rest_PROD_2_SUBRULE_1_PRED, fun_rest_PROD_2_SUBRULE_1_NT, strm')
            val (RC_RES, RC_SPAN, strm') = matchRC(strm')
            val FULL_SPAN = (#1(LC_SPAN), #2(RC_SPAN))
            in
              (UserCode.fun_rest_PROD_2_ACT (LC_RES, RC_RES, SR_RES, LC_SPAN : (Lex.pos * Lex.pos), RC_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LC, _, strm') => fun_rest_PROD_2(strm)
          | (Tok.SEMI, _, strm') => fun_rest_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun tyid_NT (strm) = let
      val (TYPE_RES, TYPE_SPAN, strm') = matchTYPE(strm)
      val (ID_RES, ID_SPAN, strm') = matchID(strm')
      val FULL_SPAN = (#1(TYPE_SPAN), #2(ID_SPAN))
      in
        (UserCode.tyid_PROD_1_ACT (ID_RES, TYPE_RES, ID_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun decl_NT (strm) = let
      fun decl_PROD_1 (strm) = let
            val (TYPE_RES, TYPE_SPAN, strm') = matchTYPE(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            fun decl_PROD_1_SUBRULE_1_NT (strm) = let
                  val (C_RES, C_SPAN, strm') = matchC(strm)
                  val (ID_RES, ID_SPAN, strm') = matchID(strm')
                  val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
                  val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
                  val FULL_SPAN = (#1(C_SPAN), #2(exp_SPAN))
                  in
                    ((ID_RES, exp_RES), FULL_SPAN, strm')
                  end
            fun decl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.C, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(decl_PROD_1_SUBRULE_1_PRED, decl_PROD_1_SUBRULE_1_NT, strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            fun decl_PROD_1_SUBRULE_2_NT (strm) = let
                  val (COMMENT_RES, COMMENT_SPAN, strm') = matchCOMMENT(strm)
                  val FULL_SPAN = (#1(COMMENT_SPAN), #2(COMMENT_SPAN))
                  in
                    ((), FULL_SPAN, strm')
                  end
            fun decl_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMENT, _, strm') => true
                    | _ => false
                  (* end case *))
            val (COMMENT_RES, COMMENT_SPAN, strm') = EBNF.closure(decl_PROD_1_SUBRULE_2_PRED, decl_PROD_1_SUBRULE_2_NT, strm')
            val FULL_SPAN = (#1(TYPE_SPAN), #2(COMMENT_SPAN))
            in
              (UserCode.decl_PROD_1_ACT (EQ_RES, ID_RES, SR_RES, exp_RES, SEMI_RES, TYPE_RES, COMMENT_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), COMMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun decl_PROD_2 (strm) = let
            val (TYPE_RES, TYPE_SPAN, strm') = matchTYPE(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            fun decl_PROD_2_SUBRULE_1_NT (strm) = let
                  val (C_RES, C_SPAN, strm') = matchC(strm)
                  val (ID_RES, ID_SPAN, strm') = matchID(strm')
                  val FULL_SPAN = (#1(C_SPAN), #2(ID_SPAN))
                  in
                    ((ID_RES), FULL_SPAN, strm')
                  end
            fun decl_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.C, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(decl_PROD_2_SUBRULE_1_PRED, decl_PROD_2_SUBRULE_1_NT, strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            fun decl_PROD_2_SUBRULE_2_NT (strm) = let
                  val (COMMENT_RES, COMMENT_SPAN, strm') = matchCOMMENT(strm)
                  val FULL_SPAN = (#1(COMMENT_SPAN), #2(COMMENT_SPAN))
                  in
                    ((), FULL_SPAN, strm')
                  end
            fun decl_PROD_2_SUBRULE_2_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMENT, _, strm') => true
                    | _ => false
                  (* end case *))
            val (COMMENT_RES, COMMENT_SPAN, strm') = EBNF.closure(decl_PROD_2_SUBRULE_2_PRED, decl_PROD_2_SUBRULE_2_NT, strm')
            val FULL_SPAN = (#1(TYPE_SPAN), #2(COMMENT_SPAN))
            in
              (UserCode.decl_PROD_2_ACT (ID_RES, SR_RES, SEMI_RES, TYPE_RES, COMMENT_RES, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), COMMENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun decl_PROD_3 (strm) = let
            val (TYPE1_RES, TYPE1_SPAN, strm') = matchTYPE(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (LPAREN_RES, LPAREN_SPAN, strm') = matchLPAREN(strm')
            val (TYPE2_RES, TYPE2_SPAN, strm') = matchTYPE(strm')
            fun decl_PROD_3_SUBRULE_1_NT (strm) = let
                  val (C_RES, C_SPAN, strm') = matchC(strm)
                  val (TYPE_RES, TYPE_SPAN, strm') = matchTYPE(strm')
                  val FULL_SPAN = (#1(C_SPAN), #2(TYPE_SPAN))
                  in
                    ((TYPE_RES), FULL_SPAN, strm')
                  end
            fun decl_PROD_3_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.C, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(decl_PROD_3_SUBRULE_1_PRED, decl_PROD_3_SUBRULE_1_NT, strm')
            val (RPAREN_RES, RPAREN_SPAN, strm') = matchRPAREN(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(TYPE1_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.decl_PROD_3_ACT (ID_RES, SR_RES, SEMI_RES, RPAREN_RES, TYPE1_RES, TYPE2_RES, LPAREN_RES, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), TYPE1_SPAN : (Lex.pos * Lex.pos), TYPE2_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun decl_PROD_4 (strm) = let
            val (TYPE_RES, TYPE_SPAN, strm') = matchTYPE(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (LPAREN_RES, LPAREN_SPAN, strm') = matchLPAREN(strm')
            val (tyid_RES, tyid_SPAN, strm') = tyid_NT(strm')
            fun decl_PROD_4_SUBRULE_1_NT (strm) = let
                  val (C_RES, C_SPAN, strm') = matchC(strm)
                  val (TYPE_RES, TYPE_SPAN, strm') = matchTYPE(strm')
                  val (ID_RES, ID_SPAN, strm') = matchID(strm')
                  val FULL_SPAN = (#1(C_SPAN), #2(ID_SPAN))
                  in
                    ((TYPE_RES, ID_RES), FULL_SPAN, strm')
                  end
            fun decl_PROD_4_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.C, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(decl_PROD_4_SUBRULE_1_PRED, decl_PROD_4_SUBRULE_1_NT, strm')
            val (RPAREN_RES, RPAREN_SPAN, strm') = matchRPAREN(strm')
            val (fun_rest_RES, fun_rest_SPAN, strm') = fun_rest_NT(strm')
            fun decl_PROD_4_SUBRULE_2_NT (strm) = let
                  val (COMMENT_RES, COMMENT_SPAN, strm') = matchCOMMENT(strm)
                  val FULL_SPAN = (#1(COMMENT_SPAN), #2(COMMENT_SPAN))
                  in
                    ((), FULL_SPAN, strm')
                  end
            fun decl_PROD_4_SUBRULE_2_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMENT, _, strm') => true
                    | _ => false
                  (* end case *))
            val (COMMENT_RES, COMMENT_SPAN, strm') = EBNF.closure(decl_PROD_4_SUBRULE_2_PRED, decl_PROD_4_SUBRULE_2_NT, strm')
            val FULL_SPAN = (#1(TYPE_SPAN), #2(COMMENT_SPAN))
            in
              (UserCode.decl_PROD_4_ACT (ID_RES, SR_RES, TYPE_RES, tyid_RES, RPAREN_RES, fun_rest_RES, COMMENT_RES, LPAREN_RES, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), TYPE_SPAN : (Lex.pos * Lex.pos), tyid_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), fun_rest_SPAN : (Lex.pos * Lex.pos), COMMENT_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.TYPE(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.ID(_), _, strm') =>
                    (case (lex(strm'))
                     of (Tok.SEMI, _, strm') => decl_PROD_2(strm)
                      | (Tok.C, _, strm') => decl_PROD_2(strm)
                      | (Tok.EQ, _, strm') => decl_PROD_1(strm)
                      | (Tok.LPAREN, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.TYPE(_), _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.ID(_), _, strm') => decl_PROD_4(strm)
                                  | (Tok.C, _, strm') => decl_PROD_3(strm)
                                  | (Tok.RPAREN, _, strm') => decl_PROD_3(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun pgm_NT (strm) = let
      fun pgm_PROD_1_SUBRULE_1_NT (strm) = let
            val (decl_RES, decl_SPAN, strm') = decl_NT(strm)
            val FULL_SPAN = (#1(decl_SPAN), #2(decl_SPAN))
            in
              ((decl_RES), FULL_SPAN, strm')
            end
      fun pgm_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.TYPE(_), _, strm') => true
              | _ => false
            (* end case *))
      val (decl_RES, decl_SPAN, strm') = EBNF.closure(pgm_PROD_1_SUBRULE_1_PRED, pgm_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(decl_SPAN), #2(decl_SPAN))
      in
        (UserCode.pgm_PROD_1_ACT (decl_RES, decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
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
