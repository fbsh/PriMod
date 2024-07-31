structure Syntax = struct
  (* Identifiers *)
  type module_ident = string
  type signature_ident = string
  type value_ident = string
  type type_ident = string
  type tvar = string

  (* Kinds *)
  datatype kind
    = KBase
    | KArrow of kind * kind

  (* Type constructors *)
  datatype tycon
    = TPath of type_ident path
    | TVar of tvar
    | TApp of tycon * tycon
    | TArrow of tycon * tycon
    | TUnit

  (* Expressions *)
  datatype expr
    = EPath of value_ident path
    | EVar of value_ident
    | EAbs of value_ident * expr
    | EApp of expr * expr
    | EUnit
    | ELet of binding * expr

  (* Modules *)
  and module
    = MPath of module_ident path
    | MStruct of binding list

  (* Bindings *)
  and binding
    = BVal of value_ident * expr
    | BType of type_ident * tycon
    | BModule of module_ident * module

  (* Signatures *)
  and signature
    = SPath of signature_ident path
    | SStruct of decl list

  (* Declarations *)
  and decl
    = DVal of value_ident * tycon
    | DType of type_ident * kind
    | DModule of module_ident * signature

  (* Paths *)
  and 'a path
    = PIdent of 'a
    | PProj of module * 'a

  (* Program *)
  type program = module

  (* Utility functions *)
  fun map_path f =
    fn PIdent x => PIdent (f x)
     | PProj (m, x) => PProj (m, f x)

  (* Pretty printing functions *)
  fun show_kind k =
    case k of
      KBase => "Type"
    | KArrow (k1, k2) => "(" ^ show_kind k1 ^ " -> " ^ show_kind k2 ^ ")"

  fun show_tycon ty =
    case ty of
      TPath p => show_path TypeIdent.show p
    | TVar x => "'" ^ x
    | TApp (t1, t2) => "(" ^ show_tycon t1 ^ " " ^ show_tycon t2 ^ ")"
    | TArrow (t1, t2) => "(" ^ show_tycon t1 ^ " -> " ^ show_tycon t2 ^ ")"
    | TUnit => "unit"

  and show_path show_id p =
    case p of
      PIdent id => show_id id
    | PProj (m, id) => show_module m ^ "." ^ show_id id

  and show_module m =
    case m of
      MPath p => show_path ModuleIdent.show p
    | MStruct bindings => "struct " ^ String.concatWith " " (map show_binding bindings) ^ " end"

  and show_binding b =
    case b of
      BVal (id, e) => "val " ^ ValueIdent.show id ^ " = " ^ show_expr e
    | BType (id, ty) => "type " ^ TypeIdent.show id ^ " = " ^ show_tycon ty
    | BModule (id, m) => "module " ^ ModuleIdent.show id ^ " = " ^ show_module m

  and show_expr e =
    case e of
      EPath p => show_path ValueIdent.show p
    | EVar x => ValueIdent.show x
    | EAbs (x, e) => "(fn " ^ ValueIdent.show x ^ " => " ^ show_expr e ^ ")"
    | EApp (e1, e2) => "(" ^ show_expr e1 ^ " " ^ show_expr e2 ^ ")"
    | EUnit => "()"
    | ELet (b, e) => "let " ^ show_binding b ^ " in " ^ show_expr e ^ " end"

  (* Identifier modules *)
  structure ModuleIdent = struct
    type t = module_ident
    val compare = String.compare
    structure Map = RedBlackMapFn(type ord_key = t val compare = compare)
    val show = fn x => x
  end

  structure SignatureIdent = struct
    type t = signature_ident
    val compare = String.compare
    structure Map = RedBlackMapFn(type ord_key = t val compare = compare)
    val show = fn x => x
  end

  structure ValueIdent = struct
    type t = value_ident
    val compare = String.compare
    structure Map = RedBlackMapFn(type ord_key = t val compare = compare)
    val show = fn x => x
  end

  structure TypeIdent = struct
    type t = type_ident
    val compare = String.compare
    structure Map = RedBlackMapFn(type ord_key = t val compare = compare)
    val show = fn x => x
  end

  structure TvarMap = RedBlackMapFn(type ord_key = tvar val compare = String.compare)

  structure Type = struct
    type t = tycon
    val show = show_tycon
  end
end