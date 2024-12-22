{
  open Tiny_parser

  exception LexError of string

  let failwith msg = raise (LexError msg)
  let illegal c = failwith (Printf.sprintf "[lexer] illegal character %c" c)
}

let whitespace = [' ''\t']+
let newline = "\r\n" | '\n' | '\r'
let num = (['1'-'9']['0'-'9']*)|['0'-'9']
let letter = ['a'-'z''A'-'Z']
let string_char = [^'\"']
let ident = letter(letter|['0'-'9''_'])*
let inline_comment = "//" [^'\n']*

rule next_token =
  parse
  | eof { EOF }
  | whitespace { next_token lexbuf }
  | inline_comment { next_token lexbuf }
  | "/*" { multiline_comment lexbuf }
  | newline { Lexing.new_line lexbuf; next_token lexbuf }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { LBRC }
  | "}" { RBRC }
  | "String::from" { next_token lexbuf }
  | "i32" { I32 }
  | "->" { ARROW }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "else" { ELSE }
  | "loop" { LOOP }
  | "break" { BREAK }
  | "fn" { FN }
  | "let" { LET }
  | "mut" { MUT }
  | "&" { AMPERSAND }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | "%" { PERC }
  | "==" { EQ }
  | "=" { ASSIGN }
  | "," { COMMA }
  | "." { DOT }
  | ";" { SEP }
  | ":" { COLON }
  | "!" { BANG }
  | "\""string_char*"\""  { STRING (Lexing.lexeme lexbuf )}
  | num { CONST (Lexing.lexeme lexbuf) }
  | ident { ID (Lexing.lexeme lexbuf) }
  | _ as c { illegal c }

and multiline_comment =
  parse
  | eof { failwith "[lexer] unterminated comment at EOF" }
  | '\n' { Lexing.new_line lexbuf; multiline_comment lexbuf }
  | "*/" { next_token lexbuf }
  | _ { multiline_comment lexbuf }