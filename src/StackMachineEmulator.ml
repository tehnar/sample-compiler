module Map = Map.Make (String)

open Data

let run : int list -> (int -> int -> int) -> int list = fun stack op -> 
  match stack with
  | [] | _::[]   -> assert false
  | y::x::stack' -> (op x y)::stack'

let find_label code label = 
  let indexed_code = List.mapi (fun x y -> (x, y)) code in
  let positions = List.find_all (fun (x, cmd) ->
  match cmd with
  | S_LABEL label' -> label = label'
  | _              -> false) indexed_code
  in match positions with
  | [(x, y)] -> x
  | []  -> raise (Compilation_Error (Printf.sprintf "No label %s found" label))
  | _   -> raise (Compilation_Error (Printf.sprintf "%d labels '%s' found instead of one" (List.length positions) label))

let check_jmp_condition jmp x = 
  match jmp with 
  | Jz  -> x = 0
  | Jnz -> x <> 0

let run input code =
  let code_list = code in
  let rec run' (state, stack, stack_frames, input, output, instruction_pointer) code =
    if instruction_pointer = Array.length code then output
    else let instr = code.(instruction_pointer) in
       run'
         (match instr with
          | S_READ -> (
             match input with 
             | []        -> assert false
             | y::input' -> (state, y::stack, stack_frames, input', output, instruction_pointer + 1)
          )
          | S_WRITE -> (
             match stack with
             | []        -> assert false
             | y::stack' -> (state, stack', stack_frames, input, output @ [y], instruction_pointer + 1)
          )
          | S_PUSH n ->
             (state, n::stack, stack_frames, input, output, instruction_pointer + 1)
          | S_LD x -> (state, (Map.find x state)::stack, stack_frames, input, output, instruction_pointer + 1)
          | S_ST x -> (
             match stack with
             | []        -> assert false
             | y::stack' -> (Map.add x y state, stack', stack_frames, input, output, instruction_pointer + 1)
          )
          | S_BINARY_ARITHM_OP  op -> 
             (state, run stack (fun x y -> binary_op_to_fun  op x y), stack_frames, input, output, instruction_pointer + 1)
          | S_BINARY_COMPARE_OP op -> 
             (state, run stack (fun x y -> compare_op_to_fun op x y), stack_frames, input, output, instruction_pointer + 1)
          | S_BINARY_LOGICAL_OP op -> 
             (state, run stack (fun x y -> logical_op_to_fun op x y), stack_frames, input, output, instruction_pointer + 1)
          | S_CONDITIONAL_JMP (op, label) -> ( 
             match stack with 
             | []        -> assert false
             | y::stack' -> if check_jmp_condition op y then (state, stack', stack_frames, input, output, find_label code_list label)
                                                        else (state, stack', stack_frames, input, output, instruction_pointer + 1)
          )
          | S_JMP label       -> (state, stack, stack_frames, input, output, find_label code_list label)
          | S_LABEL _         -> (state, stack, stack_frames, input, output, instruction_pointer + 1)
          | S_FUNC_BEGIN args -> let ret_addr::stack' = stack in  
                                 let (state', stack'') = List.fold_left (fun (state, (x::stack)) arg -> (Map.add arg x state, stack)) (Map.empty, stack') args in
                                 (state', ret_addr::stack'', stack_frames, input, output, instruction_pointer + 1) 
          | S_CALL label      -> (Map.empty, (instruction_pointer+1)::stack,  state::stack_frames, input, output, find_label code_list label)
          | S_RET             -> let (x::y::stack') = stack in
                                 let (state'::stack_frames') = stack_frames in
                                 (state', x::stack', stack_frames', input, output, y)
          | S_DROP            -> let (x::stack') = stack in 
                                 (state, stack', stack_frames, input, output, instruction_pointer + 1)
          | S_FUNC_END        -> (state, stack,  stack_frames, input, output, instruction_pointer + 1)
          | S_END             -> (state, stack,  stack_frames, input, output, Array.length code)
         ) code
  in
  let get_entry_point code = 
    let rev_codei = List.rev (List.mapi   (fun i x -> (x, i)) code) in
    let ends = List.filter (fun (x, i) -> match x with S_FUNC_END -> true | _ -> false) rev_codei in
    match ends with 
    | []           -> 0
    | (_, pos)::xs -> pos + 1
  in
  run' (Map.empty, [], [], input, [], get_entry_point code) (Array.of_list code)
