structure Env = struct
  (* Environment type *)
  type t = {
    m : Internal.semsig Syntax.ModuleIdent.Map.map,
    v : Internal.tycon Syntax.ValueIdent.Map.map,
    t : (Internal.tycon * Internal.kind) Syntax.TypeIdent.Map.map,
    tv : Internal.tycon Syntax.TvarMap.map
  }

  (* Empty environment *)
  val empty : t = {
    m = Syntax.ModuleIdent.Map.empty,
    v = Syntax.ValueIdent.Map.empty,
    t = Syntax.TypeIdent.Map.empty,
    tv = Syntax.TvarMap.empty
  }

  (* Module operations *)
  structure Module = struct
    exception Unbound of Syntax.module_ident

    fun lookup (env : t) id =
      case Syntax.ModuleIdent.Map.find (#m env, id) of
        SOME sig => sig
      | NONE => raise Unbound id

    fun insert id sig env =
      {m = Syntax.ModuleIdent.Map.insert (#m env, id, sig),
       v = #v env,
       t = #t env,
       tv = #tv env}
  end

  (* Value operations *)
  structure Value = struct
    exception Unbound of Syntax.value_ident

    fun lookup (env : t) id =
      case Syntax.ValueIdent.Map.find (#v env, id) of
        SOME ty => ty
      | NONE => raise Unbound id

    fun insert id ty env =
      {m = #m env,
       v = Syntax.ValueIdent.Map.insert (#v env, id, ty),
       t = #t env,
       tv = #tv env}
  end

  (* Type operations *)
  structure Type = struct
    exception Unbound of Syntax.type_ident

    fun lookup (env : t) id =
      case Syntax.TypeIdent.Map.find (#t env, id) of
        SOME ty_k => ty_k
      | NONE => raise Unbound id

    fun insert id ty_k env =
      {m = #m env,
       v = #v env,
       t = Syntax.TypeIdent.Map.insert (#t env, id, ty_k),
       tv = #tv env}
  end

  (* Type variable operations *)
  structure TVar = struct
    exception Unbound of Syntax.tvar

    fun lookup (env : t) tv =
      case Syntax.TvarMap.find (#tv env, tv) of
        SOME ty => ty
      | NONE => raise Unbound tv

    fun insert tv ty env =
      {m = #m env,
       v = #v env,
       t = #t env,
       tv = Syntax.TvarMap.insert (#tv env, tv, ty)}
  end

  (* Utility functions *)
  fun merge (env1 : t) (env2 : t) : t = {
    m = Syntax.ModuleIdent.Map.unionWith #2 (#m env1, #m env2),
    v = Syntax.ValueIdent.Map.unionWith #2 (#v env1, #v env2),
    t = Syntax.TypeIdent.Map.unionWith #2 (#t env1, #t env2),
    tv = Syntax.TvarMap.unionWith #2 (#tv env1, #tv env2)
  }

  fun restrict (env : t) (ids : Syntax.value_ident list) : t = {
    m = #m env,
    v = Syntax.ValueIdent.Map.filteri (fn (id, _) => List.exists (fn x => x = id) ids) (#v env),
    t = #t env,
    tv = #tv env
  }

  fun map_tycons f (env : t) : t = {
    m = #m env,
    v = Syntax.ValueIdent.Map.map f (#v env),
    t = Syntax.TypeIdent.Map.map (fn (ty, k) => (f ty, k)) (#t env),
    tv = Syntax.TvarMap.map f (#tv env)
  }

  (* Functions for working with structures *)
  fun from_struct (s : Internal.struct_) : t = {
    m = #m s,
    v = Syntax.ValueIdent.Map.map (fn (Internal.Path ty, _) => ty) (#v s),
    t = #t s,
    tv = Syntax.TvarMap.empty
  }

  fun to_struct (env : t) : Internal.struct_ = {
    m = #m env,
    v = Syntax.ValueIdent.Map.map (fn ty => (Internal.Path ty, ([], ty))) (#v env),
    t = #t env
  }

  (* Pretty printing *)
  fun show_env (env : t) : string =
    let
      fun show_map show_val map =
        String.concatWith ", " (List.map (fn (k, v) => k ^ " -> " ^ show_val v) (Syntax.ValueIdent.Map.listItemsi map))
      
      val m_str = show_map Internal.show_semsig (#m env)
      val v_str = show_map Internal.show_tycon (#v env)
      val t_str = show_map (fn (ty, k) => Internal.show_tycon ty ^ " : " ^ Internal.show_kind k) (#t env)
      val tv_str = show_map Internal.show_tycon (#tv env)
    in
      "Modules: {" ^ m_str ^ "}\n" ^
      "Values: {" ^ v_str ^ "}\n" ^
      "Types: {" ^ t_str ^ "}\n" ^
      "Type variables: {" ^ tv_str ^ "}"
    end

  (* Functions for working with type constructors *)
  fun lookup_tycon (env : t) (ty : Internal.tycon) : Internal.tycon =
    case ty of
      Internal.TVar tv => 
        (case Syntax.TvarMap.find (#tv env, tv) of
           SOME ty' => ty'
         | NONE => ty)
    | Internal.TApp (ty1, ty2) => 
        Internal.TApp (lookup_tycon env ty1, lookup_tycon env ty2)
    | Internal.TArrow (ty1, ty2) => 
        Internal.TArrow (lookup_tycon env ty1, lookup_tycon env ty2)
    | _ => ty

  fun instantiate_scheme (env : t) ((tvs, ty) : Internal.scheme) : Internal.tycon =
    let
      val subst = List.foldl 
        (fn (tv, subst) => 
          Syntax.TvarMap.insert(subst, tv, Internal.TUnif(Internal.fresh_uvar(), ref Internal.Undefined)))
        Syntax.TvarMap.empty
        tvs
    in
      Internal.subst subst ty
    end

  (* Functions for working with kinds *)
  fun check_kind (env : t) (ty : Internal.tycon) (k : Internal.kind) : unit =
    let
      fun kind_of ty =
        case ty of
          Internal.TVar tv => 
            (case Syntax.TvarMap.find (#tv env, tv) of
               SOME _ => Internal.KBase
             | NONE => raise Type.Unbound tv)
        | Internal.TApp (ty1, ty2) =>
            (case kind_of ty1 of
               Internal.KArrow (k1, k2) => 
                 (check_kind env ty2 k1; k2)
             | _ => raise Fail "Type application with non-arrow kind")
        | Internal.TArrow _ => Internal.KBase
        | Internal.TUnit => Internal.KBase
        | Internal.TUnif _ => Internal.KBase
      
      val inferred_kind = kind_of ty
    in
      if inferred_kind = k then ()
      else raise Fail ("Kind mismatch: expected " ^ Internal.show_kind k ^ 
                       ", but got " ^ Internal.show_kind inferred_kind)
    end

  (* Function to add primitive types and values to the environment *)
  fun add_primitives (env : t) : t =
    let
      val env = Type.insert "int" (Internal.TUnif(Internal.fresh_uvar(), ref Internal.Undefined), Internal.KBase) env
      val env = Type.insert "bool" (Internal.TUnif(Internal.fresh_uvar(), ref Internal.Undefined), Internal.KBase) env
      val env = Type.insert "string" (Internal.TUnif(Internal.fresh_uvar(), ref Internal.Undefined), Internal.KBase) env
      
      val env = Value.insert "true" (Internal.TUnif(Internal.fresh_uvar(), ref Internal.Undefined)) env
      val env = Value.insert "false" (Internal.TUnif(Internal.fresh_uvar(), ref Internal.Undefined)) env
      
      (* Add more primitive types and values as needed *)
    in
      env
    end

  (* Initialize a standard environment with primitives *)
  val standard : t = add_primitives empty

end