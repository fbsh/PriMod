structure Lexer = struct
  exception LexError of string * int * int  (* message, line, column *)

  fun lexer (input : string) : Token.t list =
    let
      val lineNum = ref 1
      val lineStart = ref 0
      val pos = ref 0
      val maxPos = String.size input

      fun currPos () = !pos - !lineStart

      fun nextChar () =
        if !pos < maxPos then
          let val c = String.sub (input, !pos)
          in pos := !pos + 1; SOME c end
        else NONE

      fun peekChar () =
        if !pos < maxPos then SOME (String.sub (input, !pos))
        else NONE

      fun error msg = raise LexError (msg, !lineNum, currPos())

      fun isAlpha c = Char.isAlpha c orelse c = #"_"
      fun isAlphaNum c = Char.isAlphaNum c orelse c = #"_"
      fun isDigit c = Char.isDigit c
      fun isWhitespace c = c = #" " orelse c = #"\t" orelse c = #"\n" orelse c = #"\r"

      fun lexIdentOrKeyword () =
        let
          fun collect acc =
            case peekChar() of
              SOME c => if isAlphaNum c
                        then (nextChar(); collect (c :: acc))
                        else acc
            | NONE => acc
          val chars = rev (collect [])
          val str = String.implode chars
        in
          case Token.isKeyword str of
            SOME token => token
          | NONE => Token.IDENT str
        end

      fun lexNumber () =
        let
          fun collect acc =
            case peekChar() of
              SOME c => if isDigit c
                        then (nextChar(); collect (c :: acc))
                        else acc
            | NONE => acc
          val chars = rev (collect [])
          val str = String.implode chars
        in
          case Int.fromString str of
            SOME n => Token.INT n
          | NONE => error ("Invalid number: " ^ str)
        end

      fun lexString () =
        let
          fun collect acc =
            case nextChar() of
              SOME #"\"" => String.implode (rev acc)
            | SOME #"\\" => 
                (case nextChar() of
                   SOME c => collect (c :: #"\\" :: acc)
                 | NONE => error "Unterminated string literal")
            | SOME c => collect (c :: acc)
            | NONE => error "Unterminated string literal"
        in
          Token.STRING (collect [])
        end

      fun lexToken () =
        case nextChar() of
          NONE => Token.EOF
        | SOME c =>
            if isWhitespace c then
              (if c = #"\n" then (lineNum := !lineNum + 1; lineStart := !pos) else ();
               lexToken())
            else if isAlpha c then
              (pos := !pos - 1; lexIdentOrKeyword())
            else if isDigit c then
              (pos := !pos - 1; lexNumber())
            else
              case c of
                #"'" => Token.TYVAR (String.str (valOf (nextChar())))
              | #"(" => Token.LPAREN
              | #")" => Token.RPAREN
              | #"{" => Token.LBRACE
              | #"}" => Token.RBRACE
              | #":" => Token.COLON
              | #";" => Token.SEMICOLON
              | #"." => Token.DOT
              | #"," => Token.COMMA
              | #"=" => 
                  if peekChar() = SOME #">"
                  then (nextChar(); Token.DARROW)
                  else Token.EQUALS
              | #"+" => Token.PLUS
              | #"-" => 
                  if peekChar() = SOME #">" 
                  then (nextChar(); Token.ARROW) 
                  else Token.MINUS
              | #"*" => Token.TIMES
              | #"/" => Token.DIVIDE
              | #"\"" => lexString()
              | _ => error ("Unexpected character: " ^ String.str c)

      fun lexAll acc =
        case lexToken() of
          Token.EOF => rev (Token.EOF :: acc)
        | token => lexAll (token :: acc)
    in
      lexAll []
    end

  fun lexString s =
    lexer s
    handle LexError (msg, line, col) =>
      (print ("Lexical error at line " ^ Int.toString line ^ 
              ", column " ^ Int.toString col ^ ": " ^ msg ^ "\n");
       [])

  fun lexFile filename =
    let
      val inputStream = TextIO.openIn filename
      val content = TextIO.inputAll inputStream
      val _ = TextIO.closeIn inputStream
    in
      lexString content
    end
end