open Utils
open Ast

type trace_error =
  | TypeError of string
  | CannotMutate of ide
  | MutBorrowOfNonMut of ide
  | DataRace of ide * bool * bool
  | UnboundVar of ide
  | OutOfGas of int
  | NotInLoop
  | MovedValue of ide
  | TODO
[@@deriving show]

type 'a trace_result = ('a, trace_error) result
