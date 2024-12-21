module P = Nice_parser.Make (struct
  type result = Ast.crate
  type token = Tiny_parser.token
  exception ParseError = Tiny_parser.Error
  let parse = Tiny_parser.crate
  include Tiny_lexer
end)

include P

let _ = P.pp_exceptions ()
