type ide = string [@@deriving show]

type expr =
  | UNIT
  | VAR of ide
  | CONST of int
  | STRING of string
  | PLUS of expr * expr
  | ASSIGN of ide * expr
  | BLOCK of statement * expr option
  | BLOCK_EXEC of statement * expr option
  | BLOCK_RET of expr
  | CALL of ide * expr list
  | WITH_BANG of expr
  | IFE of expr * expr * expr
[@@deriving show]

and statement =
  | FUNDECL of ide * ide list * expr
  | LET of ide * bool * expr
  | SEQ of statement * statement
  | EXPR of expr
  | EMPTY
[@@deriving show]

let is_value = function CONST _ | STRING _ | UNIT -> true | _ -> false

type crate = statement list [@@deriving show]
