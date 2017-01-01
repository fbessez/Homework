structure Frame =
struct

  structure IdKey : ORD_KEY =
  struct
    type ord_key = Ast.ident
    val compare = String.compare
  end

  structure M = SplayMapFn(IdKey)

  (*  The type of frames, which are maps from Ast.ident to 'a.  We think
  *   of a frame as a function, and use function notation such as
  *   fr(x) to describe the value bound to the key x in fr.  We write
  *   fr{x |-> v} to denote the function fr', where
  *     fr'(y) = v,     y = x
  *              fr(y), y <> x.
  *)
  type 'a frame = 'a M.map

  (*  Raised by extend(fr, x, v) when x is in the domain of fr.
  *)
  exception DuplicateError

  (*  Raised by update(fr, x, v) when x is not in the domain of fr.
  *)
  exception DomainError

  (*  The empty frame.
  *)
  val empty : 'a frame = M.empty

  (*  loopkup(fr, x) = fr(x).
  *)
  fun lookup (fr : 'a frame, x : Ast.ident) : 'a option = M.find(fr, x)

  (*  extend(fr, x, v) = fr{x |-> v}.
  *   Raises DuplicateError if x is in the domain of fr.
  *)
  fun extend (fr : 'a frame, x : Ast.ident, v : 'a) : 'a frame =
    case M.find(fr, x) of
         NONE => M.insert(fr, x, v)
       | SOME _ => raise DuplicateError

  (*  update(fr, x, v) = fr{x |-> v}.
  *   Any existing binding to x is lost.
  *)
  val update = M.insert

end

structure Env =
struct

  (*  The type of envionments.  An environment is a non-empty stack of 
  *  'a frames.  We say that x is in the domain of Gamma if x is the domain of
  *   one of  the frames in Gamma.  If x is in the domain of Gamma, we 
  *   write Gamma(x) for fr(x), where fr is the topmost frame in Gamma such 
  *   that x is in the domain of fr.  If x is in the domain of Gamma and
  *   Gamma = [fr_0,...,fr_{i-1},...fr_{n-1}] with fr_{i-1} the topmost frame 
  *   whose domain contains x, then
  *   Gamma{x |-> v} = [fr_0,...,fr_{i-2},fr_{i-1}{x |-> v},fr_i,...,fr_{n-1}].
  *   If x is not in the domain of Gamma, then Gamma{x |-> v} is not 
  *   defined.
  *
  *   In the following documentation, when we write [f,...], we mean a
  *   stack with top element f.
  *)
  type 'a env = 'a Frame.frame list

  (*  Exception raised by extend Gamma x v if x is not in the domain of
  *   the top frame of Gamma.
  *)
  exception DuplicateError

  (*  Exception raised by update(Gamma, x, v) if x is not in the domain of
   *  Gamma.
   *)
  exception DomainError

  (*  The empty frame.
  *)
  val empty = []

  (*  lookup Gamma x = Gamma(x).
  *)
  fun lookup (Gamma : 'a env) (x : Ast.ident) : 'a =
    case Gamma of
         [] => raise DomainError
       | fr :: Gamma =>
           case Frame.lookup(fr, x) of
                NONE => lookup Gamma x
              | SOME v => v

  (*  extend Gamma x v = [fr{x |-> v},...], where Gamma = [fr,...].
  *   
  *   Raises DuplicateError if x is in the domain of fr.
  *)
  fun extend (fr :: Gamma : 'a env) (x : Ast.ident) (v : 'a) : 'a env =
    (Frame.extend(fr, x, v)) :: Gamma
    handle Frame.DuplicateError => raise DuplicateError

  (*  extends Gamma [x_0,...,x_{n-1}] v =
  *     extend (... extend (extend Gamma x_0 v) x_1 v...) x_{n-1} v.
  *
  *   Raises DuplicateError if x_i is in the domain of the top frame of
  *   Gamma for some i.
  *)
  fun extends (Gamma : 'a env) (xs : Ast.ident list) (v : 'a) : 'a env =
    case xs of
         [] => Gamma
       | x :: xs => extends (extend Gamma x v) xs v

  (*  update Gamma x v = Gamma{x |-> v}.
  *   
  *   Raises DomainError if x is not in the domain of Gamma.
  *)
  fun update (Gamma : 'a env) (x : Ast.ident) (v : 'a) : 'a env =
    case Gamma of
         [] => raise DomainError
       | fr :: Gamma =>
           Frame.update(fr, x, v) :: Gamma
           handle Frame.DomainError => fr :: (update Gamma x v)

  (*  pushFrame Gamma = [fr, f,...], where Gamma = [f,...].
  *)
  fun pushFrame (Gamma : 'a env) (fr : 'a Frame.frame) : 'a env =
    fr :: Gamma

  (*  popFrame Gamma = [f,...], where Gamma = [f', f,...].
  *)
  fun popFrame (Gamma : 'a env) : 'a env =
    tl Gamma

end
