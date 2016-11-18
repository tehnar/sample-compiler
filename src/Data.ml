type binary_arithm_op    = Add | Sub | Mul | Div | Mod 
type binary_compare_op   = Le  | Leq | Ge  | Geq | Eq  | Neq
type binary_logical_op   = And | Or
type conditional_jump_op = Jz  | Jnz

module Value = 
  struct 
    type t = Int of int | String of bytes
    
    let of_int x    = Int    x
    let of_string x = String (Bytes.of_string x)
    let to_int      = function
      | Int x -> x
      | _     -> failwith "Value.to_int: value is not an int"
    let to_bytes = function
      | String x -> x
      | _        -> failwith "Value.to_string: value is not a string"
    let to_string = function
      | String x -> Bytes.to_string x
      | _         -> failwith "Value.to_string: value is not a string"
    let convert_to_string = function
      | String x -> Bytes.to_string x
      | Int x    -> Printf.sprintf "%d" x
  end

type expr =
  | Const of Value.t 
  | Var   of string
  | BinaryArithmExpr  of binary_arithm_op  * expr * expr
  | BinaryCompareExpr of binary_compare_op * expr * expr
  | BinaryLogicalExpr of binary_logical_op * expr * expr
  | FunctionCallExpr  of string * (expr list)

type statement =
  | Skip
  | Assign of string * expr
  | Seq    of statement * statement
  | If     of expr * statement * statement
  | While  of expr * statement 
  | Return of expr
  | FunctionCallStatement of string * (expr list)
  | FunctionDef of string * (string list) * statement


type instr =
  | S_PUSH               of Value.t
  | S_LD                 of string
  | S_ST                 of string
  | S_LABEL              of string
  | S_JMP                of string
  | S_CONDITIONAL_JMP    of conditional_jump_op * string
  | S_BINARY_ARITHM_OP   of binary_arithm_op
  | S_BINARY_COMPARE_OP  of binary_compare_op
  | S_BINARY_LOGICAL_OP  of binary_logical_op
  | S_CALL               of string * int
  | S_BUILTIN            of string * int
  | S_RET
  | S_DROP
  | S_FUNC_BEGIN of string list
  | S_FUNC_END
  | S_END
