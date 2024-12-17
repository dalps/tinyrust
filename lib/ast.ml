type ide = string
[@@deriving show]

type expr =
| CONST of int
| VAR of ide
| STRING of string
| PLUS of expr * expr
| ASSIGN of ide * expr
| CALL of ide * expr list
| WITH_BANG of expr
[@@deriving show]

type statement =
| FUNDECL of ide * ide list * statement list
| LET of ide * expr
| BLOCK of statement list * expr option
| CALL of ide * expr list
| EXPR of expr
| EMPTY
[@@deriving show]

type crate = statement list
[@@deriving show]
