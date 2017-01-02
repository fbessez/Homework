(*  COMP 321 Homework 7:  Compiler
*)

structure Compile =
struct

  structure A = AnnAst
  structure T = TextIO

  exception Unimplemented

  datatype jas_type = J_I | J_D | J_Z | J_V (*Don't we want strings?*)

  fun annToJasType(typ: A.typ) : char = 
  (*I chose chars, so we can use the 'implode' funtion on a list of 
    them to generate a typecode. If you need to change it back, keep and 
    rename this function.--G *)
    case typ of 
      A.Tint => #"I"
      | A.Tdouble => #"D"
      | A.Tbool => #"Z"
      | A.Tvoid => #"V"
      (*| A.Tstring => #"S"*)

  (*  compile(p, outs) = ().  As a side-effect, p is compiled to Jasmin
  *   assembly, with the resulting assembly code written to the output
  *   stream outs.  The client is responsible for opening outs before calling
  *   this function and closing it after this function returns.
  *)

(*  type env = ()
          idstore: int Env.env, mapping ids to store addresses
          nxtLabl: int, incrememnting the next label
          addresses: int list addresses used for stores w/ block statements
          ) *)
  type idstore = int Env.env
  type nxtLabl = int
  type addresses = int list
  type funloc = (A.paramdecl list * A.typ) Env.env
  type env = (idstore * nxtLabl * addresses * funloc)
  (*type env = (int Env.env * int * int list * (A.paramdecl list * A.typ) Env.env)*)
  val emptyEnv = (Env.pushFrame Env.empty Frame.empty, 0, [0], Env.pushFrame Env.empty Frame.empty) : env

(*given an env (idstore, nxtLabl, addresses)
  this returns (idstore, nxtLabl + 1, addresses along with 
    the env1) *)

    fun createLabel(env: env) : string * env =
      let 
        val (idstore, nxtLabl, addresses, funloc) = env
        val newLbl = "LBL" ^ Int.toString(nxtLabl)
        val env1 = (idstore, nxtLabl + 1, addresses, funloc)
      in 
        (newLbl, env1)
      end

(*This just looks up where the id is located based on
  the mapping in the environment*)
    fun idLocation(env: env, id: A.id): int = 
      let val (idstore, nxtLabl, addresses, funloc) = env in
        Env.lookup idstore id
      end

(*This function writes out IO *)
  fun emit(out: T.outstream, str: string) : unit = 
    let val str_with_line = str ^ "\n" in
      (T.output(out, str_with_line);
        TextIO.flushOut out)
      end

(*env * int where int is used for store allocation*)
  fun compileExp(out: T.outstream, (env, l): env*int, exp: A.exp): env*int = 
    case exp of
      A.EInt(i) => (emit(out, "ldc " ^ Int.toString(i)) ; (env, l + 1))
    | A.EDouble(d) => (emit(out, "ldc2_w " ^ Real.toString(d)) ; (env, l + 2))
    | A.EString(s) => (emit(out, "ldc " ^ s) ; (env, l + 1))
    | A.ETrue => (emit(out, "ldc 1") ; (env, l + 1))
    | A.EFalse => (emit(out, "ldc 0") ; (env, l + 1))
    | A.EId(id, typ) => 
      let val locID = idLocation(env, id) in 
        case typ of 
            A.Tint => (emit(out, "iload " ^ Int.toString(locID)) ; (env, l + 1))
            | A.Tdouble => (emit(out, "dload " ^ Int.toString(locID)); (env, l + 2))
            | A.Tbool => (emit(out, "iload " ^ Int.toString(locID)) ; (env, l + 1))
            | _ => raise Fail ""
          end
    | A.ECall((funcName, explist), typ) => 
        (case funcName of
            "readInt" => let val (env1, l1) = compileExps(out, (env, l), explist)
                             val () = emit(out, "invokestatic CSupport/readInt()I")
                           in 
                            (env1, l1)
                          end
            | "printInt" => let val env1 = compileExps(out, (env, l), explist)
                             val () = emit(out, "invokestatic CSupport/printInt(I)V")
                           in 
                            env1
                          end
            | "readBool" => let val env1 = compileExps(out, (env, l), explist)
                             val () = emit(out, "invokestatic CSupport/readBool()Z")
                           in 
                            env1
                          end
            | "printBool" => let val env1 = compileExps(out, (env, l), explist)
                             val () = emit(out, "invokestatic CSupport/printBool(Z)V")
                           in 
                            env1
                          end
            | "readDouble" => let val env1 = compileExps(out, (env, l), explist)
                             val () = emit(out, "invokestatic CSupport/readDouble()D")
                           in 
                            env1
                          end
            | "printDouble" => let val env1 = compileExps(out, (env, l), explist)
                             val () = emit(out, "invokestatic CSupport/printDouble(D)V")
                           in 
                            env1
                          end
            | _ => 
            let 
              val ((idstore, nxtLabl, addresses, funloc), l1) = compileExps(out, (env, l), explist)
              (*val (params, typ) = Env.lookup funloc funcName*)
              val argtyps=map(fn e=>annToJasType(A.typeOf(e))) explist
              val typcode=implode(argtyps)
              val () = emit(out, "invokestatic C/"^funcName^"("^typcode^")"^Char.toString(annToJasType(typ))) ;
              val env1 = (idstore, nxtLabl, addresses, funloc)
            in 
              (env1, l1 + 1)
              end)
    | A.EPostIncr(id, typ) =>  
      let val locID = Int.toString(idLocation(env, id)) in
        emit(out, "iload " ^ locID ^ "\n" ^ "dup" ^ "\n" ^ "ldc 1" ^"\n" ^ "iadd" ^ "\n" ^ "istore " ^ locID) ; (env, l + 3)
      end
    | A.EPostDecr(id, typ) => 
      let val locID = Int.toString(idLocation(env, id)) in
        (emit(out, "iload " ^ locID ^ "\n" ^ "dup" ^ "\n" ^ "ldc -1" ^"\n" ^ "iadd" ^ "\n" ^ "istore " ^ locID) ; (env, l + 3))
      end
    | A.ENot(exp, typ) =>
      let val (env, l) = compileExp(out, (env, l), exp) in
        emit(out, "ldc 1" ^ "\n" ^ "iadd" ^ "\n" ^ "ldc 2" ^ "\n" ^ "irem") ; (env, l + 1)
      end
    | A.EPreIncr(id, typ) =>
      let val locID = Int.toString(idLocation(env, id)) in
        (emit(out, "ldc 1" ^ "\n" ^ "iload " ^ locID ^ "\n" ^ "iadd" ^ "\n" ^ "dup" ^ "\n" ^ "istore " ^ locID) ; (env, l + 3))
      end
    | A.EPreDecr(id, typ) => 
      let val locID = Int.toString(idLocation(env, id)) in
        (emit(out, "ldc -1" ^ "\n" ^ "iload " ^ locID ^ "\n" ^ "iadd" ^ "\n" ^ "dup" ^ "\n" ^ "istore " ^ locID) ; (env, l + 3))
      end
    | A.EMul((e0, e1), typ) =>
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1) in
            case typ of 
              A.Tint => (emit(out, "imul ") ; (env2, line2))
              | A.Tdouble => (emit(out, "dmul ") ; (env2, line2))
              | _ => raise Fail ""
            end
    | A.EDiv((e0, e1), typ) => 
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1) in
            case typ of 
              A.Tint => (emit(out, "idiv ") ; (env2, line2))
              | A.Tdouble => (emit(out, "ddiv ") ; (env2, line2))
              | _ => raise Fail ""
            end
    | A.EMod((e0, e1), typ) => 
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1) in
            case typ of 
              A.Tint => (emit(out, "irem ") ; (env2, line2))
              | _ => raise Fail ""
            end
    | A.EAdd((e0, e1), typ) => 
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1) in
            case typ of 
              A.Tint => (emit(out, "iadd ") ; (env2, line2))
              | A.Tdouble => (emit(out, "dadd ") ; (env2, line2))
              | _ => raise Fail ""
            end
    | A.ESub((e0, e1), typ) => 
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1) in
            case typ of 
              A.Tint => (emit(out, "isub ") ; (env2, line2))
              | A.Tdouble => (emit(out, "dsub ") ; (env2, line2))
              | _ => raise Fail ""
            end
    | A.ELt((e0, e1), typ) =>
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1)
          val (labelline1, env3) = createLabel(env2)
          val (labelline2, env4) = createLabel(env3) in
            case A.typeOf(e0) of
              A.Tint => (emit(out, "if_icmplt " ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 2))
              | A.Tdouble => (emit(out, "dcmpg" ^ "\n" ^ "iflt" ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 3))
            end
    | A.EGt((e0, e1), typ) =>
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1)
          val (labelline1, env3) = createLabel(env2)
          val (labelline2, env4) = createLabel(env3) in
            case A.typeOf(e0) of
              A.Tint => (emit(out, "if_icmpgt " ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 2))
              | A.Tdouble => (emit(out, "dcmpg" ^ "\n" ^ "ifgt" ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 3))
            end
    | A.ELe((e0, e1), typ) =>
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1)
          val (labelline1, env3) = createLabel(env2)
          val (labelline2, env4) = createLabel(env3) in
            case A.typeOf(e0) of
              A.Tint => (emit(out, "if_icmple " ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 2))
              | A.Tdouble => (emit(out, "dcmpg" ^ "\n" ^ "ifle" ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 3))
            end
    | A.EGe((e0, e1), typ) =>
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1)
          val (labelline1, env3) = createLabel(env2)
          val (labelline2, env4) = createLabel(env3) in
            case A.typeOf(e0) of
              A.Tint => (emit(out, "if_icmpge " ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 2))
              | A.Tdouble => (emit(out, "dcmpg" ^ "\n" ^ "ifge" ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 3))
            end
    | A.EEq((e0, e1), typ) =>
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1)
          val (labelline1, env3) = createLabel(env2)
          val (labelline2, env4) = createLabel(env3) in
            case A.typeOf(e0) of
              A.Tint => (emit(out, "if_icmpeq " ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 2))
              | A.Tdouble => (emit(out, "dcmpg" ^ "\n" ^ "ifeq" ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 3))
              | A.Tbool => (emit(out, "if_icmpeq " ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 2))
              | _ => raise Fail ""
            end
    | A.ENeq((e0, e1), typ) =>
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1)
          val (labelline1, env3) = createLabel(env2)
          val (labelline2, env4) = createLabel(env3) in
            case A.typeOf(e0) of
              A.Tint => (emit(out, "if_icmpne " ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 2))
              | A.Tdouble => (emit(out, "dcmpg" ^ "\n" ^ "ifne" ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 3))
              | A.Tbool => (emit(out, "if_icmpne " ^ labelline1 ^ " \n" ^ "ldc 0" ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^ labelline1 ^": " ^ "\n" ^ "ldc 1" ^ "\n" ^ labelline2 ^ ": ") ; (env4, line2 + 2))
            end
    | A.EAnd((e0, e1), typ) => 
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1)
          val (labelline1, env3) = createLabel(env2)
          val (labelline2, env4) = createLabel(env3) in 
            emit(out, "iand\n"^"ifeq "^labelline1^"\n"^"ldc 1\n"^"goto"^
              labelline2^"\n"^labelline1^":"^"\n"^"ldc 0\n"^labelline2);
            (env4, line2+2) end
    | A.EOr((e0, e1), typ) => 
      let val (env1, line1) = compileExp(out, (env, l), e0)
          val (env2, line2) = compileExp(out, (env1, line1), e1)
          val (labelline1, env3) = createLabel(env2)
          val (labelline2, env4) = createLabel(env3) in 
            emit(out, "ior\n"^"ifeq "^labelline1^"\n"^"ldc 1\n"^"goto"^
              labelline2^"\n"^labelline1^":"^"\n"^"ldc 0\n"^labelline2);
            (env4, line2+2) end
    | A.EAsst((id, exp), typ) => let
      val loc = idLocation(env, id) in
        storeVal(out,(env,l), exp, loc) end
    | A.ECond((e0, e1, e2), typ) => 
      let val (labelline1, env1) = createLabel(env)
          val (labelline2, env2) = createLabel(env1)
          val (env3, line1) = compileExp(out, (env2, l), e0)
          val (env4, line2) = compileExp(out, (env3, line1 - 1), e1)
          val (env5, line3) = compileExp(out, (env4, line2), e2)
        in
          emit(out, "ifeq " ^ labelline1 ^ "\n" ^ "goto " ^ labelline2 ^ "\n" ^
            labelline1 ^ ": " ^ "\n" ^ labelline2 ^ ": ") ; 
          (env5, line3)
        end
  and storeVal(out: T.outstream, (env, l): env*int, exp: A.exp, loc: int) : env*int=
    (*FOR USE when exp has already been compiled and its value pushed onto the stack*)
    let
      val loc=Int.toString(loc)
      val (env', l') = compileExp(out, (env,l), exp) in
        (case A.typeOf(exp) of
          A.Tint=>(emit(out, "istore "^loc) ; (env, l+1))
        | A.Tdouble=>(emit(out, "dstore "^loc); (env,l+2))
        | A.Tbool=>(emit(out, "istore "^loc); (env,l+1))
        | _=> raise Fail "" 
        ) end

  and compileExps(out: T.outstream, (env, l): env * int, explist : A.exp list) : env * int = 
    case explist of
      [] => (env, l)
      | exp :: exps' => let val env1 = compileExp(out, (env, l), exp)
        in
          compileExps(out, env1, exps')
        end

  fun storeVals(out : T.outstream, (env,l): env*int, tids: ((A.typ*A.id) list)) : env*int=
      case tids of
        []=>(env,l)
      | (typ,id)::xs=> let
      val (idstore, nxtLabl, address :: addresses, funloc) = env
      val extendedAddressesStore = Env.extend idstore id address
      val newAddressesIntBool = (address + 1) :: addresses
      val newAddressesDouble = (address + 2) :: addresses
      val env2 = (extendedAddressesStore, nxtLabl, newAddressesIntBool, funloc)
      val env3 = (extendedAddressesStore, nxtLabl, newAddressesDouble, funloc)
      in 
      case typ of
        A.Tint => (emit(out, "istore " ^ Int.toString(address)) ; (env2, l - 1))
        | A.Tdouble => (emit(out, "dstore " ^ Int.toString(address)) ; (env3, l - 2))
        | A.Tbool => (emit(out, "istore " ^ Int.toString(address)) ; (env2, l - 1))
        | _ => raise Fail ""
      end

  fun storeID(env: env, typ: A.typ, id: A.id) : env =
    let 
      val (idstore, nxtLabl, address::addresses, funloc) = env
      val idLoc = address
      val extendedAddressesStore = Env.extend idstore id idLoc
      val newAddressesIntBool = (address + 1) :: addresses
      val newAddressesDouble = (address + 2) :: addresses
      val env2 = (extendedAddressesStore, nxtLabl, newAddressesIntBool, funloc)
      val env3 = (extendedAddressesStore, nxtLabl, newAddressesDouble, funloc)
      val str = Char.toString(annToJasType(typ))
    in 
      case typ of 
        A.Tint => env2
        | A.Tbool => env2
        | A.Tdouble => env3
        | A.Tvoid => env2
        | _ => raise Fail str
      end





      (*This function, when called by compileStm in the A.SFor case
      is used to update the environment given a typ, id, exp
      basically same as other except with diff inputs which stores the id at an address
        on extended environment*)
  and forStmHelper(out: T.outstream, (env, l) : env * int, typ: A.typ, id: A.id, exp: A.exp) : env * int = 
    let 
      val (env1, l1) = compileExp(out, (env, l), exp)
      val (idstore, nxtLabl, address :: addresses, funloc) = env1
      val extendedAddressesStore = Env.extend idstore id address
      val newAddressesIntBool = (address + 1) :: addresses
      val newAddressesDouble = (address + 2) :: addresses
      val env2 = (extendedAddressesStore, nxtLabl, newAddressesIntBool, funloc)
      val env3 = (extendedAddressesStore, nxtLabl, newAddressesDouble, funloc)
    in 
      case typ of
        A.Tint => (emit(out, "istore " ^ Int.toString(address)) ; (env2, l1 - 1))
        | A.Tdouble => (emit(out, "dstore " ^ Int.toString(address)) ; (env3, l1 - 2))
        | A.Tbool => (emit(out, "istore " ^ Int.toString(address)) ; (env2, l1 - 1))
        | _ => raise Fail ""
      end

      (*needed to do a recursion within the case statement
        this function makes it easier to accomplish*)
    fun stmInits(out: T.outstream, (env, l): env * int, typ: A.typ, idexplist: ((A.id * A.exp) list), returnt: A.typ option) : env * int =
      case idexplist of
        [] => (env, l)
        | (id, exp) :: idexps =>
          let val (env1, l1) = forStmHelper(out, (env, l), typ, id, exp)
          in
            stmInits(out, (env1,l1), typ, idexps, returnt)
          end

    fun stmDecls(env: env, typ : A.typ, idlist : A.id list, returnt: A.typ option) : env = 
      case idlist of
        [] => env
        | id1 :: ids =>
          let val env1 = storeID(env, typ, id1)
          in
            stmDecls(env1, typ, ids, returnt)
          end

  fun compileStm(out: T.outstream, (env, l): env * int, stm: A.stm, returnt: A.typ option) : env * int = 
    case stm of 
        A.SExp(e0) => 
          let val env1 = compileExp(out,(env, l),e0)
                   in
                     env1
                   end
      | A.SDecl(typ, idList) => 
        let val finalEnv = (stmDecls(env, typ, idList, returnt), l)
        in
          finalEnv
        end
      | A.SInit(typ, idexplist) => 
        let val finalEnv = stmInits(out, (env, l), typ, idexplist, returnt)
        in
          finalEnv
        end
      | A.SDoWhile(stm, exp) => 
        let 
          val (begin, lblline1env) = createLabel(env)
          val (next, lblline2env) = createLabel(lblline1env)
          val (envStm, l1) = compileStm(out, (lblline2env, l), stm, returnt)
          val finalEnv = compileExp(out, (envStm, l1), exp)
        in
          emit(out, begin ^ ": " ^ "\n" ^ "ifeq " ^ next ^ "\n" ^ "goto " ^ begin ^ "\n" ^ next ^ ": ") ;
          finalEnv
        end
      | A.SWhile(exp, stm) => let
          val (begin, lblline1env) = createLabel(env)
          val (next, lblline2env) = createLabel(lblline1env)
          val () = emit(out, begin ^ ": ")
          val (envExp, l1) = compileExp(out, (lblline2env, l), exp)
          val () = emit(out, "ifeq " ^ next)
          val finalEnv = compileStm(out, (lblline2env, l1), stm, returnt)
          val () = emit(out, "goto " ^ begin)
          val () = emit(out, next ^ ": ")
        in 
          (*emit(out, begin ^ ": " ^ "\n" ^ "ifeq " ^ next ^ "\n" ^ "goto " ^ begin ^ "\n" ^ next ^ ": ") ; *)
          finalEnv
        end
      | A.SFor((ty, var, e0), e1, e2, stm) => 
        let
          val (begin, env1) = createLabel(env)
          val (next, (idstore, nxtLabl, addresses, funloc)) = createLabel(env1)
          (*Pushed a new frame on to the idstore to match with the Interp file*)
          val new_idstore = Env.pushFrame idstore Frame.empty
          val env3 = (new_idstore, nxtLabl, (hd addresses) :: addresses, funloc)
          val (env4, l1) = forStmHelper(out, (env3, l), ty, var, e0)
          val () = emit(out, begin ^ ": ")
          val (env5, l2) = compileExp(out, (env4, l1), e1)
          val () = emit(out, "ifeq " ^next)
          val (env6, l3) = compileStm(out, (env5, l2), stm, returnt)
          val (env7, l4) = compileExp(out, (env6, l3), e2)
          val () = emit(out, "pop")
          val () = emit(out, "goto " ^ begin)
          val () = emit(out, next ^ ": ")
          val (idstore1, nxtLabl1, addresses1, funloc') = env7
          val env8 = (Env.popFrame idstore1, nxtLabl1, (tl addresses1), funloc')
        in
(*          emit(out, begin ^ ": " ^ "\n" ^ "ifeq " ^ next ^ "\n" ^ "pop " ^ "\n" ^ "goto " ^
               begin ^ "\n" ^ next ^ ": " ^ "\n") ; *)
          (env8, l4)
        end
      | A.SIf(exp, stm) => 
          let 
            val (begin, lbll1) = createLabel(env)
            val (next, lbll2) = createLabel(lbll1)
            val () = emit(out, begin ^ ": ")
            val (env3, l1) = compileExp(out, (lbll2, l), exp)
            val () = emit(out, "ifeq " ^ next)
            val (env4, l2) = compileStm(out, (env3, l1), stm, returnt)
            val () = emit(out, next ^ ": ")
          in 
            (env4, l2)
          end
(*          let 
            val env1 = compileExp(out, (env, l), exp)
            val (iffalse, labelline1) = createLabel(env)
            val () = emit(out, "ifeq " ^ iffalse)
            val env2 = compileStm(out, env1, stm, returnt)
            val () = emit(out, iffalse ^ ": ")
          in
            env2
          end*)
(*        let 
          val (begin, lblline1env) = createLabel(env)
          val (next, lblline2env) = createLabel(lblline1env)
          val (envExp, l1) = compileExp(out, (lblline2env, l), exp)
          val finalEnv = compileStm(out, (envExp, l1), stm, returnt)
        in
          emit(out, begin ^ ": " ^ "\n" ^ "ifeq " ^ next ^ "\n" ^ next ^ ": ") ; 
          finalEnv
        end*)
      | A.SIfElse(exp, stm1, stm2) =>
        let 
          val (elsebranch , lblline1env) = createLabel(env)
          val (endif, lblline2env) = createLabel(lblline1env)
          val (envExp, l1) = compileExp(out, (lblline2env, l), exp)
          val () = emit(out, "ifeq "^elsebranch)
          val (envStm, l2) = compileStm(out, (envExp, l1), stm1, returnt) 
          val () =  emit(out, "goto"^endif); 
          val () =  emit(out, elsebranch^":") 
          val finalEnv = compileStm(out, (envStm, l2), stm2, returnt)
          val () =  emit(out, endif^":") in 
          (finalEnv)
        end
      | A.SReturn(exp) => 
        (case returnt of
          NONE => let val () = emit(out, "return")
            in 
              (env, l)
            end
        | SOME(t) => 
            let
              val finalEnv = compileExp(out, (env, l), exp)
            in
          (case A.typeOf(exp) of
            A.Tint => (emit(out, "ireturn ") ; finalEnv)
            | A.Tdouble => (emit(out, "dreturn ") ; finalEnv)
            | A.Tbool => (emit(out, "ireturn ") ; finalEnv)
            | _ => raise Fail ""
            )
          end
          )
      | A.SVReturn => (emit(out, "return ") ; (env, l))
      | A.SBlock(stmlist) =>
          let
            val (idstore, nxtLabl, addresses, funloc) = env
            val pushedFrameStore = Env.pushFrame idstore Frame.empty
            val env1 = (pushedFrameStore, nxtLabl, (hd addresses) :: addresses, funloc)
            val ((idstore1, nxtLabl1, addresses1, funloc), l1) = compileStms(out, (env1, l), stmlist, returnt)
            val env2 = (Env.popFrame idstore1, nxtLabl1, addresses1, funloc)
          in
            (env2, l1)
          end

  (*This function simply takes an A.stm list and compiles it one at a time
        each time with the updated environment from the previous compiliation*)
  (*Uses the and so that they can be mutually recursive! otherwise they can't call one another*)
    and compileStms(out: T.outstream, (env,l): env*int, ss: A.stm list, returnt: A.typ option) : env*int=
      case ss of 
          nil => (env, l)
          | stmHd :: stmTl => 
            let 
              val (env1, l1) = compileStm(out, (env, l), stmHd, returnt)
            in 
              compileStms(out, (env1, l1), stmTl, returnt) 
            end
    (*repackage ss as SBlock and call compileStm?*)


  fun paramsToTypecode(tids: ((A.typ* A.id) list)) : string=
    case tids of
      [] => ""
      | _ => 
          let 
            val ids=map(fn (t,id)=>annToJasType(t)) tids
          in 
            implode(ids) end


  fun compileDef(out: T.outstream, (env, l): env * int, def: A.def) : env*int=
    case def of
      A.DFun(t,id,tids,ss)=> 
      let 
        val (idstore, nxtLabl, addresses, funloc) = env
        val env1 = storeVals(out, (env, l), tids)
        val funloc' = Env.extend funloc id (tids, t)
        val argtyps = paramsToTypecode(tids)
        val typcode = Char.toString(annToJasType(t))
        val () = 
          (case id of 
            "main" => emit(out, ".method public static main([Ljava/lang/String;)V")
            | _ => emit(out, ".method public static " ^ id ^ "(" ^ argtyps ^ ")" ^ typcode)
            )
        val () = emit(out, ".limit locals 1000")
        val () = emit(out, ".limit stack 1000")
        val ((idstore, nxtLabl', addresses, funloc''), l1) = 
          (case id of
            "main" => compileStms(out, ((idstore, nxtLabl, addresses, funloc'), l),ss, NONE)
            | _ => compileStms(out, ((idstore, nxtLabl, addresses, funloc'), l), ss, SOME(t)))
        val () = emit(out, "nop")
        val () = emit(out, ".end method")
      in
        ((Env.pushFrame Env.empty Frame.empty, nxtLabl, addresses, funloc''), l1)
      end
    | _ => (env, l)

    and compileDefs(out: T.outstream, (env, l) : env * int, deflist: A.def list) : env * int= 
      case deflist of
        [] => (env, l)
        | def :: defs =>
          let val (env1, l1) = compileDef(out, (env, l), def)
          in
            compileDefs(out, (env, l1), defs)
          end

      fun compile (p : A.program, outs : T.outstream) : unit =
    let 
        val A.PDefs ds = p 
        val () = emit(outs, ".class public C")
        val () = emit(outs, ".super java/lang/Object")
        val () = emit(outs, "")
        val () = emit(outs, ".method public <init>()V")
        val () = emit(outs, "aload_0")
        val () = emit(outs, "invokenonvirtual java/lang/Object/<init>()V")
        val () = emit(outs, "return")
        val () = emit(outs,".end method")
        val (env, l) = compileDefs(outs, (emptyEnv, 0), ds)
    in 
      ()
    end 

end

(*
-fix compile output (before syntax?)
-assignment
-calls
-...finish expressions



-research function defs 

-header for doc  ---> commented in but not sure where to implement
-header for calls
-figure out main output for jvm

*)
