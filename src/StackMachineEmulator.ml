open Data


let run : int list -> (int -> int -> int) -> int list = fun stack op -> 
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

let run input code =
  let code_list = code in
  let rec run' (state, stack, input, output, instruction_pointer) code =
    if instruction_pointer == Array.length code then output
    else let instr = code.(instruction_pointer) in
       run'
         (match instr with
          | S_READ -> (
            match input with 
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
             (state, run stack (fun x y -> binary_op_to_fun  op x y), input, output, instruction_pointer + 1)
          | S_BINARY_COMPARE_OP op -> 
             (state, run stack (fun x y -> compare_op_to_fun op x y), input, output, instruction_pointer + 1)
          | S_BINARY_LOGICAL_OP op -> 
             (state, run stack (fun x y -> logical_op_to_fun op x y), input, output, instruction_pointer + 1)
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
  run' ([], [], input, [], 0) (Array.of_list code)
