type binary_arithm_op    = Add | Sub | Mul | Div | Mod 
type binary_compare_op   = Le  | Leq | Ge  | Geq | Eq  | Neq
type binary_logical_op   = And | Or
type conditional_jump_op = Jz  | Jnz

module Value = 
  struct 
    type t = Int of int | String of bytes | Array of bool * t array | FuncRef of string * (t list -> t) | Thread of Thread.t
    
    let of_int x    = Int    x

    let of_string x = String (Bytes.of_string x)

    let of_array boxed a = Array (boxed, a)

    let of_func_ref name func = FuncRef (name, func)

    let of_thread t = Thread t

    let to_int      = function
      | Int x -> x
      | _     -> failwith "Value.to_int: value is not an int"

    let to_bytes = function
      | String x -> x
      | _        -> failwith "Value.to_string: value is not a string"

    let to_string = function
      | String x -> Bytes.to_string x
      | _         -> failwith "Value.to_string: value is not a string"

    let to_array = function
      | Array (_, x)  -> x
      | _             -> failwith "Value.to_array: value is not an array"

    let is_boxed = function
      | Array (x, _)  -> x
      | _             -> failwith "Value.is_boxed: value is not an array"

    let to_func_name = function
      | FuncRef (name, _) -> name
      | _                 -> failwith "Value.to_func_name: value is not a func ref"

    let to_func = function
      | FuncRef (_, func) -> func
      | _                 -> failwith "Value.to_func: value is not a func ref"

    let to_thread = function
      | Thread t -> t
      | _        -> failwith "Value.to_thread: value is not a thread"

    let rec convert_to_string = function
      | String x -> Bytes.to_string x
      | Int x    -> Printf.sprintf "%d" x
      | Array (true, elems)  -> "{" ^ (String.concat ", " @@ List.map convert_to_string @@ Array.to_list elems) ^ "}"
      | Array (false, elems) -> "[" ^ (String.concat ", " @@ List.map convert_to_string @@ Array.to_list elems) ^ "]"
      | FuncRef _ -> failwith "Value.convert_to_string: not supported for func refs"
      | Thread _  -> failwith "Value.convert_to_string: not supported for threads"

  end

type expr =
  | Const of Value.t 
  | Var   of string
  | FuncRefName of string
  | Elem  of expr * expr 
  | Array of bool * expr list
  | BinaryArithmExpr  of binary_arithm_op  * expr * expr
  | BinaryCompareExpr of binary_compare_op * expr * expr
  | BinaryLogicalExpr of binary_logical_op * expr * expr
  | FunctionCallExpr  of string * (expr list)
  | FunctionRefCallExpr of expr * (expr list)

type statement =
  | Skip
  | Assign    of string * expr
  | ArrAssign of expr * expr * expr
  | Seq       of statement * statement
  | If        of expr * statement * statement
  | While     of expr * statement 
  | Return    of expr
  | FunctionCallStatement of string * (expr list)
  | FunctionRefCallStatement of expr * (expr list)
  | FunctionDef of string * (string list) * statement


type instr =
  | S_PUSH               of Value.t
  | S_LD                 of string
  | S_FUNC_REF_NAME      of string
  | S_ST                 of string
  | S_LABEL              of string
  | S_JMP                of string
  | S_CONDITIONAL_JMP    of conditional_jump_op * string
  | S_BINARY_ARITHM_OP   of binary_arithm_op
  | S_BINARY_COMPARE_OP  of binary_compare_op
  | S_BINARY_LOGICAL_OP  of binary_logical_op
  | S_CALL               of string * int
  | S_REF_CALL           of int
  | S_BUILTIN            of string * int
  | S_ARRAY              of bool   * int
  | S_STA               
  | S_ELEM
  | S_RET
  | S_DROP
  | S_FUNC_BEGIN of string list
  | S_FUNC_END
  | S_END
