(* 
	Author: Fabien Bessez
	Course: COMP 321 --> Design of Programming Languages
	Professor: 
	Homework: 1
 *)

functor ListPQFn(K: PRI_KEY) = 
struct

	type pq = K.key list 

	exception Empty

	val empty : pq = []

	fun isEmpty(q: pq) : bool =
		null q

	fun insert(q: pq, x: K.key) : pq = 
		case q of
			[] => x::q
			| y::ys => 
			if (K.pri x < K.pri y) then x::q else y::insert(ys, x)

	fun delMin(q: pq) : K.key * pq =
		case q of
			[] => raise Empty
			| x::xs => (x, xs)

	fun peek(q: pq) : K.key = 
		case q of
			[] => raise Empty
			| x::xs => x
end

