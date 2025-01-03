type ide = string [@@deriving show]

type binop = ADD | MUL | SUB | MOD | DIV | EQ | LEQ [@@deriving show]

type expr =
  | UNIT
  | TRUE
  | FALSE
  | VAR of ide
  | CONST of int
  | STRING of string
  | ARITH2 of binop * expr * expr
  | ASSIGN of ide * expr
  | BLOCK of statement * expr option
  | BLOCK_EXEC of statement * expr option
  | BLOCK_RET of expr
  | CALL of ide * expr list
  | IFE of expr * expr * expr
  | LOOP of expr * expr
  | REF of bool * expr (* (mutable?, expr) *)
  | BREAK
  | BORROW of expr
[@@deriving show]

and statement =
  | FUNDECL of ide * ide list * expr
  | LET of ide * bool * expr
  | SEQ of statement * statement
  | EXPR of expr
  | EMPTY
[@@deriving show]

let assign x e = ASSIGN (x, e)
let block_exec s e = BLOCK_EXEC (s, e)
let block_ret e = BLOCK_RET e
let ife e0 e1 e2 = IFE (e0, e1, e2)
let loop original e = LOOP (original, e)

let seq s1 s2 = SEQ (s1, s2)
let expr e = EXPR e
let let_stmt x mut e = LET (x, mut, e)

let apply_binop op n1 n2 : expr =
  match (op : binop) with
  | ADD -> CONST (n1 + n2)
  | MUL -> CONST (n1 * n2)
  | SUB -> CONST (n1 - n2)
  | MOD -> CONST (n1 mod n2)
  | DIV -> CONST (n1 / n2)
  | EQ -> if Int.equal n1 n2 then TRUE else FALSE
  | LEQ -> if Int.compare n1 n2 <= 0 then TRUE else FALSE

let arith2 op e1 e2 =
  match (e1, e2) with
  | CONST n1, CONST n2 -> apply_binop op n1 n2
  | _ -> ARITH2 (op, e1, e2)

let is_value = function
  | TRUE | FALSE | CONST _ | STRING _ | UNIT -> true
  | _ -> false

type crate = statement list [@@deriving show]
