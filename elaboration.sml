structure Elaboration = struct
  open Syntax
  open Internal

  exception ElaborationError of string

  fun error msg = raise ElaborationError msg

  (* Elaborate a module *)
  fun elaborate_module (env : Env.t) : module -> Internal.asig * Internal.purity =
    fn MPath p => elaborate_module_path env p
     | MStruct bs => elaborate_bindings env bs

  (* Elaborate a module path *)
  and elaborate_module_path env =
    fn PIdent id => 
         (Quantified.from_body (Env.Module.lookup env id), Purity.Pure)
     | PProj(m, id) =>
         let
           val (Quantified.Exist asig, p) = elaborate_module env m
           fun f s = proj_module s id
         in
           (Quantified.map f asig, p)
         end

  (* Elaborate bindings *)
  and elaborate_bindings env bs =
    let
      val (struct_, purity) = List.foldl elaborate_binding (empty_struct(), Purity.Pure) bs
    in
      (Quantified.from_body (Struct struct_), purity)
    end

  (* Elaborate a single binding *)
  and elaborate_binding (b, (struct_, purity)) =
    case b of
      BVal (id, e) =>
        let val (ty, p) = elaborate_expr env e
            val scheme = generalize env ty
        in 
          ({m = #m struct_,
            v = Syntax.ValueIdent.Map.insert (#v struct_, id, (Path ty, scheme)),
            t = #t struct_},
           Purity.join purity p)
        end
    | BType (id, t) =>
        let val (ty, k) = elaborate_type env t
        in 
          ({m = #m struct_,
            v = #v struct_,
            t = Syntax.TypeIdent.Map.insert (#t struct_, id, (ty, k))},
           purity)
        end
    | BModule (id, m) =>
        let val (asig, p) = elaborate_module env m
        in 
          ({m = Syntax.ModuleIdent.Map.insert (#m struct_, id, Quantified.get_body asig),
            v = #v struct_,
            t = #t struct_},
           Purity.join purity p)
        end

  (* Elaborate an expression *)
  and elaborate_expr env : expr -> Internal.tycon * Internal.purity =
    fn EPath p => (elaborate_value_path env p, Purity.Pure)
     | EVar x => (Env.Value.lookup env x, Purity.Pure)
     | EAbs (x, e) =>
        let val ty_x = TUnif(fresh_uvar(), ref Undefined)
            val env' = Env.Value.insert x ty_x env
            val (ty_e, p) = elaborate_expr env' e
        in (TArrow (ty_x, ty_e), p)
        end
     | EApp (e1, e2) =>
        let val (ty1, p1) = elaborate_expr env e1
            val (ty2, p2) = elaborate_expr env e2
            val ty_res = TUnif(fresh_uvar(), ref Undefined)
            val _ = unify ty1 (TArrow (ty2, ty_res))
        in (ty_res, Purity.join p1 p2)
        end
     | EUnit => (TUnit, Purity.Pure)
     | ELet (b, e) =>
        let
          val (struct_, p1) = elaborate_binding (b, (empty_struct(), Purity.Pure))
          val env' = Env.merge env (Env.from_struct struct_)
          val (ty, p2) = elaborate_expr env' e
        in
          (ty, Purity.join p1 p2)
        end

  (* Elaborate a value path *)
  and elaborate_value_path env =
    fn PIdent id => 
         Env.Value.lookup env id
     | PProj(m, id) =>
         let
           val (asig, _) = elaborate_module env m
           val (path, scheme) = proj_value (Quantified.get_body asig) id
         in
           instantiate_scheme env scheme
         end

  (* Elaborate a type *)
  and elaborate_type env : tycon -> Internal.tycon * Internal.kind = 
    fn TPath p => elaborate_type_path env p
     | TVar v => (TVar v, KBase)
     | TApp (t1, t2) => 
        let 
          val (ty1, k1) = elaborate_type env t1
          val (ty2, k2) = elaborate_type env t2
        in
          case k1 of
            KArrow (k_arg, k_res) => 
              if k_arg = k2 then (TApp (ty1, ty2), k_res)
              else error "Kind mismatch in type application"
          | _ => error "Expected arrow kind in type application"
        end
     | TArrow (t1, t2) =>
        let
          val (ty1, k1) = elaborate_type env t1
          val (ty2, k2) = elaborate_type env t2
          val _ = if k1 = KBase andalso k2 = KBase then () 
                  else error "Expected base kinds in arrow type"
        in
          (TArrow (ty1, ty2), KBase)
        end
     | TUnit => (TUnit, KBase)

  (* Elaborate a type path *)
  and elaborate_type_path env =
    fn PIdent id => 
         Env.Type.lookup env id
     | PProj(m, id) =>
         let
           val (asig, _) = elaborate_module env m
           val (ty, k) = proj_type (Quantified.get_body asig) id
         in
           (ty, k)
         end

  (* Elaborate a signature *)
  and elaborate_signature env : signature -> Internal.asig =
    fn SPath p => elaborate_signature_path env p
     | SStruct ds => 
         let
           val (struct_, _) = List.foldl elaborate_decl (empty_struct(), Purity.Pure) ds
         in
           Quantified.from_body (Struct struct_)
         end

  (* Elaborate a signature path *)
  and elaborate_signature_path env =
    fn PIdent id => 
         Env.Module.lookup env id
     | PProj(m, id) =>
         let
           val (asig, _) = elaborate_module env m
           val sig' = proj_module (Quantified.get_body asig) id
         in
           Quantified.from_body sig'
         end

  (* Elaborate a declaration *)
  and elaborate_decl (d, (struct_, purity)) =
    case d of
      DVal (id, ty) =>
        let val (ty', k) = elaborate_type (Env.from_struct struct_) ty
            val _ = Env.check_kind (Env.from_struct struct_) ty' KBase
        in
          ({m = #m struct_,
            v = Syntax.ValueIdent.Map.insert (#v struct_, id, (Path ty', ([], ty'))),
            t = #t struct_},
           purity)
        end
    | DType (id, k) =>
        let val k' = elaborate_kind k
            val ty = TUnif(fresh_uvar(), ref Undefined)
        in
          ({m = #m struct_,
            v = #v struct_,
            t = Syntax.TypeIdent.Map.insert (#t struct_, id, (ty, k'))},
           purity)
        end
    | DModule (id, sig') =>
        let val asig = elaborate_signature (Env.from_struct struct_) sig'
        in
          ({m = Syntax.ModuleIdent.Map.insert (#m struct_, id, Quantified.get_body asig),
            v = #v struct_,
            t = #t struct_},
           purity)
        end

  (* Elaborate a kind *)
  and elaborate_kind =
    fn KBase => KBase
     | KArrow (k1, k2) => KArrow (elaborate_kind k1, elaborate_kind k2)

  (* Generalize a type *)
  and generalize env ty =
    let
      val fvs_ty = Internal.fvs_tycon ty
      val fvs_env = FVar.Set.unionList (List.map Internal.fvs_tycon (FVar.Map.listItems env))
      val gen_vars = FVar.Set.difference (fvs_ty, fvs_env)
    in
      (FVar.Set.listItems gen_vars, ty)
    end

  (* Main elaboration function *)
  fun elaborate (m : Syntax.module) : Internal.asig * Internal.purity =
    elaborate_module Env.empty m
    handle ElaborationError msg =>
      (print ("Elaboration error: " ^ msg ^ "\n");
       raise Fail "Elaboration failed")

end