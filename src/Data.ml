type binary_arithm_op  = Add | Sub | Mul | Div | Mod 
type binary_compare_op = Le  | Leq | Ge  | Geq
type binary_logical_op = And | Or

type expr =
  | Const of int
  | Var   of string
  | BinaryArithmExpr  of binary_arithm_op  * expr * expr
  | BinaryCompareExpr of binary_compare_op * expr * expr
  | BinaryLogicalExpr of binary_logical_op * expr * expr

type statement =
  | Skip
  | Read   of string
  | Write  of expr
  | Assign of string * expr
  | Seq    of statement * statement


type instr =
  | S_READ
  | S_WRITE
  | S_PUSH               of int
  | S_LD                 of string
  | S_ST                 of string
  | S_BINARY_ARITHM_OP   of binary_arithm_op
  | S_BINARY_COMPARE_OP  of binary_compare_op
  | S_BINARY_LOGICAL_OP  of binary_logical_op
