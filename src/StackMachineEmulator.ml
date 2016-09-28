open Data

let bool_to_int b = match b with
| false -> 0
| true  -> 1

let int_to_bool x = match x with
| 0     -> false 
| _     -> true 

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

let logical_op_to_fun op = match op with
| And -> (fun l r -> bool_to_int (int_to_bool l && int_to_bool r))
| Or  -> (fun l r -> bool_to_int (int_to_bool l || int_to_bool r))

let rec eval state expr = match expr with
| Const  n     -> n
| Var    x     -> state x
| BinaryArithmExpr  (op, l, r) -> binary_op_to_fun  op (eval state l) (eval state r)
| BinaryCompareExpr (op, l, r) -> compare_op_to_fun op (eval state l) (eval state r)
| BinaryLogicalExpr (op, l, r) -> logical_op_to_fun op (eval state l) (eval state r)

let run input stmt =
  let rec run' ((state, input, output) as c) stmt =
    let state' x = List.assoc x state in
    match stmt with
    | Skip          -> c
    | Seq    (l, r) -> run' (run' c l) r
    | Assign (x, e) -> ((x, eval state' e) :: state, input, output)
    | Write   e     -> (state, input, output @ [eval state' e])
    | Read    x     -> (
       match input with
       | []        -> assert false
       | y::input' -> ((x, y)::state, input', output)
    )
  in
  let (_, _, result) = run' ([], input, []) stmt in
  result

let srun : int list -> (int -> int -> int) -> int list = fun stack op -> 
  match stack with
  | [] | _::[]   -> assert false
  | y::x::stack' -> (op x y)::stack'

let srun input code =
  let rec srun' (state, stack, input, output) code =
    match code with
    | []       -> output
    | i::code' ->
       srun'
         (match i with
          | S_READ -> (
            match stack with 
            | []        -> assert false
            | y::input' -> (state, y::stack, input', output)
          )
          | S_WRITE -> (
            match stack with
            | []        -> assert false
            | y::stack' -> (state, stack', input, output @ [y])
          )
          | S_PUSH n ->
             (state, n::stack, input, output)
          | S_LD x ->
             (state, (List.assoc x state)::stack, input, output)
          | S_ST x -> (
             match stack with
             | []        -> assert false
             | y::stack' -> ((x, y)::state, stack', input, output)
          )
          | S_BINARY_ARITHM_OP  op -> 
             (state, srun stack (fun x y -> binary_op_to_fun  op x y), input, output)
          | S_BINARY_COMPARE_OP op -> 
             (state, srun stack (fun x y -> compare_op_to_fun op x y), input, output)
          | S_BINARY_LOGICAL_OP op -> 
             (state, srun stack (fun x y -> logical_op_to_fun op x y), input, output)
         )
         code'
  in
  srun' ([], [], input, []) code
