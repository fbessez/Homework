(*
Author: Fabien Bessez
Course: COMP321
Homework 2: Lexing
*)


%name NumLex ;

%let num = 0| [1-9]{1,3}(","[0-9]{3})* ;

%defs (

	structure T = NumsTokens

	type lex_result = T.token

	
	fun removeCommas(input: string, i: int, commalessList: string list): string list = 
		let 
			val numSize = (size input) - i - 1
			val dig = str(String.sub(input, numSize))
		in
			case (dig, numSize) of
				(_,0) => dig::commalessList
				| (",", _) => removeCommas(input, i + 1, commalessList)
				| (_,_) => removeCommas(input, i + 1, dig::commalessList)
		end
	fun strToIntWrapper (input: string): int = 
		let 
			val digitStringList = removeCommas(input, 0, [])
			val commasRemoved = concat digitStringList
		in
			valOf(Int.fromString commasRemoved)
		end
		
	(* Other option using char list instead of string list:

	fun removeCommas(listy: char list) : char list = 
		case listy of
			[] => [] 
			| x::xs => (
				case x of 
					#"," => removeCommas(xs)
					| _ => x::removeCommas(xs))
	fun strToIntWrapper (stringy: string) : int = 
		let
			val listy = String.explode stringy
		in
			valOf (Int.fromString (String.implode(removeCommas listy)))
		end
		*)


	fun eof() = T.EOF
	);

{num} => (T.Num (strToIntWrapper yytext));
. 	  => (raise Fail ( "Unexpected character: " ^ yytext));