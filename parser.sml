structure Parser = struct
  exception ParseError of string * Token.t list

  fun error msg tokens = raise ParseError (msg, tokens)

  fun expect token [] = error ("Expected " ^ Token.toString token ^ ", but got end of input") []
    | expect token (t::ts) = 
        if t = token then ts
        else error ("Expected " ^ Token.toString token ^ ", but got " ^ Token.toString t) (t::ts)

  fun parseIdent [] = error "Expected identifier, but got end of input" []
    | parseIdent (Token.IDENT id :: ts) = (id, ts)
    | parseIdent (t::ts) = error ("Expected identifier, but got " ^ Token.toString t) (t::ts)

  fun parseTyVar [] = error "Expected type variable, but got end of input" []
    | parseTyVar (Token.TYVAR tv :: ts) = (tv, ts)
    | parseTyVar (t::ts) = error ("Expected type variable, but got " ^ Token.toString t) (t::ts)

  fun parseType tokens =
    let
      fun parseTypeAtom [] = error "Expected type, but got end of input" []
        | parseTypeAtom (Token.IDENT id :: ts) = (Syntax.TPath (Syntax.PIdent id), ts)
        | parseTypeAtom (Token.TYVAR tv :: ts) = (Syntax.TVar tv, ts)
        | parseTypeAtom (Token.LPAREN :: ts) =
            let
              val (ty, ts') = parseType ts
              val ts'' = expect Token.RPAREN ts'
            in
              (ty, ts'')
            end
        | parseTypeAtom (Token.UNIT :: ts) = (Syntax.TUnit, ts)
        | parseTypeAtom (t::ts) = error ("Expected type, but got " ^ Token.toString t) (t::ts)

      fun parseTypeApp tokens =
        let
          val (ty, rest) = parseTypeAtom tokens
          fun loop (ty, []) = (ty, [])
            | loop (ty, tokens as (Token.IDENT _ :: _)) =
                let
                  val (ty2, rest') = parseTypeAtom tokens
                in
                  loop (Syntax.TApp (ty, ty2), rest')
                end
            | loop (ty, tokens) = (ty, tokens)
        in
          loop (ty, rest)
        end

      val (ty, rest) = parseTypeApp tokens
    in
      case rest of
        Token.ARROW :: ts =>
          let
            val (ty2, rest') = parseType ts
          in
            (Syntax.TArrow (ty, ty2), rest')
          end
      | _ => (ty, rest)
    end

  fun parseExpr tokens =
    let
      fun parseExprAtom [] = error "Expected expression, but got end of input" []
        | parseExprAtom (Token.IDENT id :: ts) = (Syntax.EPath (Syntax.PIdent id), ts)
        | parseExprAtom (Token.LPAREN :: ts) =
            let
              val (expr, ts') = parseExpr ts
              val ts'' = expect Token.RPAREN ts'
            in
              (expr, ts'')
            end
        | parseExprAtom (Token.FN :: ts) =
            let
              val (param, ts') = parseIdent ts
              val ts'' = expect Token.ARROW ts'
              val (body, ts''') = parseExpr ts''
            in
              (Syntax.EAbs (param, body), ts''')
            end
        | parseExprAtom (Token.LET :: ts) =
            let
              val (binding, ts') = parseBinding ts
              val ts'' = expect Token.IN ts'
              val (body, ts''') = parseExpr ts''
              val ts'''' = expect Token.END ts'''
            in
              (Syntax.ELet (binding, body), ts'''')
            end
        | parseExprAtom (Token.UNIT :: ts) = (Syntax.EUnit, ts)
        | parseExprAtom (t::ts) = error ("Expected expression, but got " ^ Token.toString t) (t::ts)

      fun parseExprApp tokens =
        let
          val (expr, rest) = parseExprAtom tokens
          fun loop (expr, []) = (expr, [])
            | loop (expr, tokens as (Token.IDENT _ :: _ | Token.LPAREN :: _)) =
                let
                  val (expr2, rest') = parseExprAtom tokens
                in
                  loop (Syntax.EApp (expr, expr2), rest')
                end
            | loop (expr, tokens) = (expr, tokens)
        in
          loop (expr, rest)
        end
    in
      parseExprApp tokens
    end

  fun parseBinding tokens =
    case tokens of
      Token.VAL :: ts =>
        let
          val (id, ts') = parseIdent ts
          val ts'' = expect Token.EQUALS ts'
          val (expr, ts''') = parseExpr ts''
        in
          (Syntax.BVal (id, expr), ts''')
        end
    | Token.TYPE :: ts =>
        let
          val (id, ts') = parseIdent ts
          val ts'' = expect Token.EQUALS ts'
          val (ty, ts''') = parseType ts''
        in
          (Syntax.BType (id, ty), ts''')
        end
    | Token.MODULE :: ts =>
        let
          val (id, ts') = parseIdent ts
          val ts'' = expect Token.EQUALS ts'
          val (m, ts''') = parseModule ts''
        in
          (Syntax.BModule (id, m), ts''')
        end
    | _ => error "Expected binding (val, type, or module)" tokens

  and parseModule tokens =
    case tokens of
      Token.IDENT id :: ts => (Syntax.MPath (Syntax.PIdent id), ts)
    | Token.STRUCT :: ts =>
        let
          fun parseBindings acc [] = (List.rev acc, [])
            | parseBindings acc (Token.END :: ts) = (List.rev acc, ts)
            | parseBindings acc ts =
                let
                  val (binding, ts') = parseBinding ts
                in
                  parseBindings (binding :: acc) ts'
                end
          val (bindings, ts') = parseBindings [] ts
        in
          (Syntax.MStruct bindings, ts')
        end
    | _ => error "Expected module (identifier or struct)" tokens

  fun parseDecl tokens =
    case tokens of
      Token.VAL :: ts =>
        let
          val (id, ts') = parseIdent ts
          val ts'' = expect Token.COLON ts'
          val (ty, ts''') = parseType ts''
        in
          (Syntax.DVal (id, ty), ts''')
        end
    | Token.TYPE :: ts =>
        let
          val (id, ts') = parseIdent ts
          val ts'' = expect Token.COLON ts'
          val (kind, ts''') = parseKind ts''
        in
          (Syntax.DType (id, kind), ts''')
        end
    | Token.MODULE :: ts =>
        let
          val (id, ts') = parseIdent ts
          val ts'' = expect Token.COLON ts'
          val (sig', ts''') = parseSignature ts''
        in
          (Syntax.DModule (id, sig'), ts''')
        end
    | _ => error "Expected declaration (val, type, or module)" tokens

  and parseSignature tokens =
    case tokens of
      Token.IDENT id :: ts => (Syntax.SPath (Syntax.PIdent id), ts)
    | Token.SIG :: ts =>
        let
          fun parseDecls acc [] = (List.rev acc, [])
            | parseDecls acc (Token.END :: ts) = (List.rev acc, ts)
            | parseDecls acc ts =
                let
                  val (decl, ts') = parseDecl ts
                in
                  parseDecls (decl :: acc) ts'
                end
          val (decls, ts') = parseDecls [] ts
        in
          (Syntax.SStruct decls, ts')
        end
    | _ => error "Expected signature (identifier or sig)" tokens

  and parseKind tokens =
    let
      fun parseKindAtom (Token.IDENT "Type" :: ts) = (Syntax.KBase, ts)
        | parseKindAtom (Token.LPAREN :: ts) =
            let
              val (k, ts') = parseKind ts
              val ts'' = expect Token.RPAREN ts'
            in
              (k, ts'')
            end
        | parseKindAtom tokens = error "Expected kind (Type or (...))" tokens

      val (k, rest) = parseKindAtom tokens
    in
      case rest of
        Token.ARROW :: ts =>
          let
            val (k2, ts') = parseKind ts
          in
            (Syntax.KArrow (k, k2), ts')
          end
      | _ => (k, rest)
    end

  fun parse tokens =
    let
      val (m, rest) = parseModule tokens
    in
      case rest of
        [] => m
      | _ => error "Unexpected tokens after parsing module" rest
    end

  fun parseString s =
    parse (Lexer.lexString s)
    handle ParseError (msg, tokens) =>
      (print ("Parse error: " ^ msg ^ "\n");
       print ("Remaining tokens: " ^ String.concatWith " " (List.map Token.toString tokens) ^ "\n");
       raise Fail "Parse error")

  fun parseFile filename =
    parse (Lexer.lexFile filename)
    handle ParseError (msg, tokens) =>
      (print ("Parse error in file " ^ filename ^ ": " ^ msg ^ "\n");
       print ("Remaining tokens: " ^ String.concatWith " " (List.map Token.toString tokens) ^ "\n");
       raise Fail "Parse error")
end