type binary_arithm_op    = Add | Sub | Mul | Div | Mod 
type binary_compare_op   = Le  | Leq | Ge  | Geq | Eq  | Neq
type binary_logical_op   = And | Or
type conditional_jump_op = Jz  | Jnz

type expr =
  | Const of int
  | Var   of string
  | BinaryArithmExpr  of binary_arithm_op  * expr * expr
  | BinaryCompareExpr of binary_compare_op * expr * expr
  | BinaryLogicalExpr of binary_logical_op * expr * expr
  | FunctionCallExpr  of string * (expr list)

type statement =
  | Skip
  | Read   of string
  | Write  of expr
  | Assign of string * expr
  | Seq    of statement * statement
  | If     of expr * statement * statement
  | While  of expr * statement 
  | Return of expr
  | FunctionCallStatement of string * (expr list)
  | FunctionDef of string * (string list) * statement


type instr =
  | S_READ
  | S_WRITE
  | S_PUSH               of int
  | S_LD                 of string
  | S_ST                 of string
  | S_LABEL              of string
  | S_JMP                of string
  | S_CONDITIONAL_JMP    of conditional_jump_op * string
  | S_BINARY_ARITHM_OP   of binary_arithm_op
  | S_BINARY_COMPARE_OP  of binary_compare_op
  | S_BINARY_LOGICAL_OP  of binary_logical_op
  | S_CALL               of string
  | S_RET
  | S_DROP
  | S_FUNC_BEGIN of string list
  | S_FUNC_END
  | S_END

let bool_to_int b = if b then 1 else 0

let int_to_bool x = if x == 0 then false else true

let binary_op_to_fun op = match op with
| Add -> (fun l r -> l + r)
| Sub -> (fun l r -> l - r)
| Mul -> (fun l r -> l * r)
| Div -> (fun l r -> l / r)
| Mod -> (fun l r -> l mod r)
 
let compare_op_to_fun op = match op with
| Le  -> (fun l r -> bool_to_int (l <  r))
| Leq -> (fun l r -> bool_to_int (l <= r))
| Ge  -> (fun l r -> bool_to_int (l >  r))
| Geq -> (fun l r -> bool_to_int (l >= r))
| Eq  -> (fun l r -> bool_to_int (l == r))
| Neq -> (fun l r -> bool_to_int (l != r))

let logical_op_to_fun op = match op with
| And -> (fun l r -> bool_to_int (int_to_bool l && int_to_bool r))
| Or  -> (fun l r -> bool_to_int (int_to_bool l || int_to_bool r))

let unsafe_pop_one x = match x with
| (y::ys) -> (y, ys)
| _       -> assert false

let unsafe_pop_two x = match x with
| (y1::y2::ys) -> (y1, y2, ys)
| _            -> assert false

exception Compilation_Error of string
