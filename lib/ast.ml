type ide = string [@@deriving show]

module BorrowSet = Set.Make (String)

type variable = {
  id : ide;
  mut : bool;
  loc : int;
  borrows : [ `imm of BorrowSet.t | `mut of ide ]; [@opaque]
}
[@@deriving show]
type 'v owned = { value : 'v; owner : variable } [@@deriving show]
type borrow = { by : ide; mut : bool; owner : variable } [@@deriving show]

type binop = ADD | MUL | SUB | MOD | DIV | EQ | LEQ [@@deriving show]

type expr =
  | UNIT
  | TRUE
  | FALSE
  | VAR of ide
  | CONST of int
  | STR of string
  | STRING of string owned
  | ARITH2 of binop * expr * expr
  | ASSIGN of ide * expr
  | BLOCK of statement * expr option
  | BLOCK_EXEC of statement * expr option
  | BLOCK_RET of expr
  | CALL of ide * expr list
  | IFE of expr * expr * expr
  | LOOP of expr
  | LOOP_EXEC of { curr : statement; orig : statement }
  | REF of { mut : bool; e : expr }
  | BORROW of borrow
  | BREAK
  | CONTINUE
[@@deriving show]

and statement =
  | FUNDECL of {
      name : ide;
      pars : ide list;
      body : statement;
      ret : expr option;
    }
  | LET of { name : ide; mut : bool; body : expr }
  | SEQ of statement * statement
  | EXPR of expr
  | EMPTY
[@@deriving show]

let assign x e = ASSIGN (x, e)
let block_exec s e = BLOCK_EXEC (s, e)
let block_ret e = BLOCK_RET e
let ife e0 e1 e2 = IFE (e0, e1, e2)
let loop_exec orig curr = LOOP_EXEC { orig; curr }

let seq s1 s2 = SEQ (s1, s2)
let expr e = EXPR e
let let_stmt name mut body = LET { name; mut; body }

let apply_binop op n1 n2 : expr =
  match op with
  | ADD -> CONST (n1 + n2)
  | MUL -> CONST (n1 * n2)
  | SUB -> CONST (n1 - n2)
  | MOD -> CONST (n1 mod n2)
  | DIV -> CONST (n1 / n2)
  | EQ -> if Int.equal n1 n2 then TRUE else FALSE
  | LEQ -> if Int.compare n1 n2 <= 0 then TRUE else FALSE

let apply_stringop op n1 n2 : expr =
  match op with
  | EQ -> if String.equal n1 n2 then TRUE else FALSE
  | _ -> failwith "not supported"

let arith2 op e1 e2 =
  match (e1, e2) with
  | CONST n1, CONST n2 -> apply_binop op n1 n2
  | STRING n1, STRING n2 -> apply_stringop op n1.value n2.value
  | STRING n1, STR n2 -> apply_stringop op n1.value n2
  | STR n1, STRING n2 -> apply_stringop op n1 n2.value
  | STR n1, STR n2 -> apply_stringop op n1 n2
  | _ -> ARITH2 (op, e1, e2)

let is_value = function
  | TRUE | FALSE | CONST _ | STR _ | STRING _ | UNIT | BREAK | CONTINUE
  | BORROW _ ->
      true
  | _ -> false

let remove_block = function
  | BLOCK (s, e) -> (s, e)
  | _ -> failwith "Not a block expression"

type crate = statement list [@@deriving show]
