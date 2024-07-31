structure Internal = struct
  (* Kinds *)
  datatype kind
    = KBase
    | KArrow of kind * kind

  (* Type constructors *)
  datatype tycon
    = TVar of Syntax.tvar
    | TApp of tycon * tycon
    | TArrow of tycon * tycon
    | TUnit
    | TUnif of uvar * unif ref

  and unif
    = Defined of tycon
    | Undefined

  (* Scheme and semantic signature *)
  type scheme = Syntax.tvar list * tycon

  datatype semsig
    = Struct of struct_
    | SigFun of (semsig * semsig) Quantified.t

  and struct_ = {
    m : semsig Syntax.ModuleIdent.Map.map,
    v : (path * scheme) Syntax.ValueIdent.Map.map,
    t : (tycon * kind) Syntax.TypeIdent.Map.map
  }

  (* Path and abstract signature *)
  datatype path = Path of tycon

  type asig = semsig Quantified.t

  (* Purity *)
  datatype purity = Pure | Impure

  (* Unification variable handling *)
  type uvar = int
  val uvar_counter = ref 0
  fun fresh_uvar () = 
    let val n = !uvar_counter in
      uvar_counter := n + 1;
      n
    end

  (* Quantified structure *)
  structure Quantified = struct
    type 'a t = Syntax.tvar list * 'a

    fun map f (tvs, x) = (tvs, f x)
    
    fun from_body x = ([], x)
    
    fun quantify tvs x = (tvs, x)
    
    fun apply f (tvs, x) = f (tvs, x)
    
    fun proj x = x
    
    fun get_body (_, x) = x

    fun all_alive _ = true  (* Simplified version *)
  end

  (* Purity structure *)
  structure Purity = struct
    type t = purity
    fun join Pure Pure = Pure
      | join _ _ = Impure
    fun show Pure = "Pure"
      | show Impure = "Impure"
  end

  (* Utility functions *)
  fun empty_struct () : struct_ = 
    {m = Syntax.ModuleIdent.Map.empty, 
     v = Syntax.ValueIdent.Map.empty, 
     t = Syntax.TypeIdent.Map.empty}

  fun insert_module id sig {m, v, t} =
    {m = Syntax.ModuleIdent.Map.insert (m, id, sig), v = v, t = t}

  fun insert_value id scheme {m, v, t} =
    {m = m, v = Syntax.ValueIdent.Map.insert (v, id, scheme), t = t}

  fun insert_type id ty_k {m, v, t} =
    {m = m, v = v, t = Syntax.TypeIdent.Map.insert (t, id, ty_k)}

  (* Type manipulation functions *)
  fun tycon_map f ty =
    case ty of
      TVar x => f (TVar x)
    | TApp (t1, t2) => TApp (tycon_map f t1, tycon_map f t2)
    | TArrow (t1, t2) => TArrow (tycon_map f t1, tycon_map f t2)
    | TUnit => TUnit
    | TUnif (u, r) =>
        case !r of
          Undefined => f (TUnif (u, r))
        | Defined t => tycon_map f t

  fun tycon_app_spine ty =
    case ty of
      TApp (t1, t2) =>
        let val (head, args) = tycon_app_spine t1
        in (head, args @ [t2])
        end
    | _ => (ty, [])

  (* Unification and substitution *)
  exception Unify of tycon * tycon

  fun occurs u ty =
    tycon_map (fn TUnif (u', _) => if u = u' then raise Unify (TUnif (u, ref Undefined), ty)
                                   else TUnif (u', ref Undefined)
                | t => t) ty

  fun unify (ty1, ty2) =
    case (ty1, ty2) of
      (TVar x, TVar y) => if x = y then () else raise Unify (ty1, ty2)
    | (TApp (t11, t12), TApp (t21, t22)) => (unify (t11, t21); unify (t12, t22))
    | (TArrow (t11, t12), TArrow (t21, t22)) => (unify (t11, t21); unify (t12, t22))
    | (TUnit, TUnit) => ()
    | (TUnif (u1, r1), TUnif (u2, r2)) =>
        if u1 = u2 then ()
        else (occurs u1 ty2; r1 := Defined ty2)
    | (TUnif (u, r), _) => (occurs u ty2; r := Defined ty2)
    | (_, TUnif (u, r)) => (occurs u ty1; r := Defined ty1)
    | _ => raise Unify (ty1, ty2)

  fun subst env ty =
    tycon_map (fn TVar x => (case Syntax.TvarMap.find (env, x) of
                               SOME t => t
                             | NONE => TVar x)
                | t => t) ty

  (* Pretty printing functions *)
  fun show_kind k =
    case k of
      KBase => "Type"
    | KArrow (k1, k2) => "(" ^ show_kind k1 ^ " -> " ^ show_kind k2 ^ ")"

  fun show_tycon ty =
    case ty of
      TVar x => "'" ^ x
    | TApp (t1, t2) => "(" ^ show_tycon t1 ^ " " ^ show_tycon t2 ^ ")"
    | TArrow (t1, t2) => "(" ^ show_tycon t1 ^ " -> " ^ show_tycon t2 ^ ")"
    | TUnit => "unit"
    | TUnif (u, r) =>
        case !r of
          Undefined => "?" ^ Int.toString u
        | Defined t => show_tycon t

  fun show_path (Path ty) = show_tycon ty

  fun show_semsig sig =
    case sig of
      Struct s => "struct ... end"
    | SigFun _ => "<functor>"

  fun show_asig (tvs, sig) =
    let
      val tvs_str = case tvs of
                      [] => ""
                    | _ => "forall " ^ String.concatWith " " tvs ^ ". "
    in
      tvs_str ^ show_semsig sig
    end

  (* Free variables and normalization *)
  structure FVar = struct
    type t = Syntax.tvar
    val compare = String.compare
    structure Set = RedBlackSetFn(type ord_key = t val compare = compare)
    structure Map = RedBlackMapFn(type ord_key = t val compare = compare)
  end

  fun fvs_tycon ty =
    let
      fun collect (TVar x, set) = FVar.Set.add(set, x)
        | collect (TApp(t1, t2), set) = collect(t2, collect(t1, set))
        | collect (TArrow(t1, t2), set) = collect(t2, collect(t1, set))
        | collect (TUnit, set) = set
        | collect (TUnif(_, ref (Defined t)), set) = collect(t, set)
        | collect (TUnif(_, ref Undefined), set) = set
    in
      collect(ty, FVar.Set.empty)
    end

  fun norm_tycon ty =
    case ty of
      TApp(t1, t2) => TApp(norm_tycon t1, norm_tycon t2)
    | TArrow(t1, t2) => TArrow(norm_tycon t1, norm_tycon t2)
    | TUnif(_, ref (Defined t)) => norm_tycon t
    | _ => ty

  (* Additional utility functions *)
  fun get_structure f =
    fn Struct s => s
     | s => raise f s

  fun get_functor f =
    fn SigFun u => (u, Impure)
     | s => raise f s

  fun proj_module s id =
    case Syntax.ModuleIdent.Map.find (#m (get_structure (fn _ => Fail "proj_module") s), id) of
      SOME sig => sig
    | NONE => raise Fail ("No such module: " ^ id)

  fun proj_value s id =
    case Syntax.ValueIdent.Map.find (#v (get_structure (fn _ => Fail "proj_value") s), id) of
      SOME x => x
    | NONE => raise Fail ("No such value: " ^ id)

  fun proj_type s id =
    case Syntax.TypeIdent.Map.find (#t (get_structure (fn _ => Fail "proj_type") s), id) of
      SOME x => x
    | NONE => raise Fail ("No such type: " ^ id)

  (* Functions for type checking and inference *)
  fun instantiate (tvs, ty) =
    let
      val subst = FVar.Map.fromList (List.map (fn tv => (tv, TUnif(fresh_uvar(), ref Undefined))) tvs)
    in
      subst ty
    end

  fun generalize env ty =
    let
      val fvs_ty = fvs_tycon ty
      val fvs_env = FVar.Set.unionList (List.map fvs_tycon (FVar.Map.listItems env))
      val gen_vars = FVar.Set.difference (fvs_ty, fvs_env)
    in
      (FVar.Set.listItems gen_vars, ty)
    end

end