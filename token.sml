structure Token = struct
  datatype t
    = IDENT of string
    | TYVAR of string
    | INT of int
    | STRING of string

    (* Punctuation *)
    | LPAREN | RPAREN
    | LBRACE | RBRACE
    | COLON | SEMICOLON
    | DOT | COMMA
    | ARROW | DARROW (* -> and => *)
    | EQUALS

    (* Operators *)
    | PLUS | MINUS | TIMES | DIVIDE

    (* Keywords *)
    | VAL | TYPE | MODULE
    | STRUCT | SIGNATURE | SIG
    | FN | LET | IN | END
    | IF | THEN | ELSE
    | OPEN | INCLUDE

    (* Special *)
    | EOF

  fun toString token =
    case token of
      IDENT s => "IDENT(" ^ s ^ ")"
    | TYVAR s => "TYVAR(" ^ s ^ ")"
    | INT i => "INT(" ^ Int.toString i ^ ")"
    | STRING s => "STRING(" ^ s ^ ")"
    | LPAREN => "LPAREN"
    | RPAREN => "RPAREN"
    | LBRACE => "LBRACE"
    | RBRACE => "RBRACE"
    | COLON => "COLON"
    | SEMICOLON => "SEMICOLON"
    | DOT => "DOT"
    | COMMA => "COMMA"
    | ARROW => "ARROW"
    | DARROW => "DARROW"
    | EQUALS => "EQUALS"
    | PLUS => "PLUS"
    | MINUS => "MINUS"
    | TIMES => "TIMES"
    | DIVIDE => "DIVIDE"
    | VAL => "VAL"
    | TYPE => "TYPE"
    | MODULE => "MODULE"
    | STRUCT => "STRUCT"
    | SIGNATURE => "SIGNATURE"
    | SIG => "SIG"
    | FN => "FN"
    | LET => "LET"
    | IN => "IN"
    | END => "END"
    | IF => "IF"
    | THEN => "THEN"
    | ELSE => "ELSE"
    | OPEN => "OPEN"
    | INCLUDE => "INCLUDE"
    | EOF => "EOF"

  fun isKeyword s =
    case s of
      "val" => SOME VAL
    | "type" => SOME TYPE
    | "module" => SOME MODULE
    | "struct" => SOME STRUCT
    | "signature" => SOME SIGNATURE
    | "sig" => SOME SIG
    | "fn" => SOME FN
    | "let" => SOME LET
    | "in" => SOME IN
    | "end" => SOME END
    | "if" => SOME IF
    | "then" => SOME THEN
    | "else" => SOME ELSE
    | "open" => SOME OPEN
    | "include" => SOME INCLUDE
    | _ => NONE

  fun compare (t1, t2) =
    case (t1, t2) of
      (IDENT s1, IDENT s2) => String.compare (s1, s2)
    | (TYVAR s1, TYVAR s2) => String.compare (s1, s2)
    | (INT i1, INT i2) => Int.compare (i1, i2)
    | (STRING s1, STRING s2) => String.compare (s1, s2)
    | _ => 
        let
          fun tokenOrder t =
            case t of
              IDENT _ => 0
            | TYVAR _ => 1
            | INT _ => 2
            | STRING _ => 3
            | LPAREN => 4
            | RPAREN => 5
            | LBRACE => 6
            | RBRACE => 7
            | COLON => 8
            | SEMICOLON => 9
            | DOT => 10
            | COMMA => 11
            | ARROW => 12
            | DARROW => 13
            | EQUALS => 14
            | PLUS => 15
            | MINUS => 16
            | TIMES => 17
            | DIVIDE => 18
            | VAL => 19
            | TYPE => 20
            | MODULE => 21
            | STRUCT => 22
            | SIGNATURE => 23
            | SIG => 24
            | FN => 25
            | LET => 26
            | IN => 27
            | END => 28
            | IF => 29
            | THEN => 30
            | ELSE => 31
            | OPEN => 32
            | INCLUDE => 33
            | EOF => 34
        in
          Int.compare (tokenOrder t1, tokenOrder t2)
        end
end