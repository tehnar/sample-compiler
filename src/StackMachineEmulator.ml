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
| Eq  -> (fun l r -> bool_to_int (l == r))
| Neq -> (fun l r -> bool_to_int (l != r))

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
    | If     (cond, if_block, else_block) -> 
        if eval state' cond != 0 then run' c if_block
        else run' c else_block
    | While  (cond, block) -> 
      let rec run_while ((state, input, output) as c) cond block = 
        let state' x = List.assoc x state in
        if eval state' cond == 0 then c 
        else let c' = run' c block in run_while c' cond block
      in run_while c cond block

  in
  let (_, _, result) = run' ([], input, []) stmt in
  result

let srun : int list -> (int -> int -> int) -> int list = fun stack op -> 
  match stack with
  | [] | _::[]   -> assert false
  | y::x::stack' -> (op x y)::stack'

let find_label code label = 
  let indexed_code = List.mapi (fun x y -> (x, y)) code in
  let positions = List.find_all (fun (x, cmd) ->
  match cmd with
  | S_LABEL label -> true
  | _             -> false) indexed_code
  in match positions with
  | [(x, y)] -> x
  | []  -> raise (Compilation_Error (Printf.sprintf "No label %s found" label))
  | _   -> raise (Compilation_Error (Printf.sprintf "Multiple labels %s found" label))

let check_jmp_condition jmp x = 
  match jmp with 
  | Jz  -> x == 0
  | Jnz -> x != 0

let srun input code =
  let code_list = code in
  let rec srun' (state, stack, input, output, instruction_pointer) code =
    if instruction_pointer + 1 == Array.length code then output
    else let instr = code.(instruction_pointer) in
       srun'
         (match instr with
          | S_READ -> (
            match stack with 
            | []        -> assert false
            | y::input' -> (state, y::stack, input', output, instruction_pointer + 1)
          )
          | S_WRITE -> (
            match stack with
            | []        -> assert false
            | y::stack' -> (state, stack', input, output @ [y], instruction_pointer + 1)
          )
          | S_PUSH n ->
             (state, n::stack, input, output, instruction_pointer + 1)
          | S_LD x ->
             (state, (List.assoc x state)::stack, input, output, instruction_pointer + 1)
          | S_ST x -> (
             match stack with
             | []        -> assert false
             | y::stack' -> ((x, y)::state, stack', input, output, instruction_pointer + 1)
          )
          | S_BINARY_ARITHM_OP  op -> 
             (state, srun stack (fun x y -> binary_op_to_fun  op x y), input, output, instruction_pointer + 1)
          | S_BINARY_COMPARE_OP op -> 
             (state, srun stack (fun x y -> compare_op_to_fun op x y), input, output, instruction_pointer + 1)
          | S_BINARY_LOGICAL_OP op -> 
             (state, srun stack (fun x y -> logical_op_to_fun op x y), input, output, instruction_pointer + 1)
          | S_CONDITIONAL_JMP (op, label) -> ( 
            match stack with 
            | []        -> assert false
            | y::stack' -> if check_jmp_condition op y then (state, stack', input, output, find_label code_list label)
                                                       else (state, stack', input, output, instruction_pointer + 1)
          )
          | S_JMP label -> (state, stack, input, output, find_label code_list label)
          | S_LABEL _   -> (state, stack, input, output, instruction_pointer + 1)
         ) code
  in
  srun' ([], [], input, [], 0) (Array.of_list code)
