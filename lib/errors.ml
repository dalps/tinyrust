open Utils
open Ast

type trace_error =
  | TypeError of string
  | CannotMutate of ide
  | MutBorrowOfNonMut of ide
  | DataRace of { borrowed : ide; is : [ `imm | `mut ]; want : [ `imm | `mut ] }
  | UnboundVar of ide
  | OutOfGas of int
  | NotInLoop
  | BorrowOfMovedValue of ide
  | MovedValue of ide
  | SegFault of int
  | MismatchedArgs of ide
  | CannotAssignBorrowed of ide
  | NoRuleApplies
  | TODO
[@@deriving show]

type 'a trace_result = ('a, trace_error) result
