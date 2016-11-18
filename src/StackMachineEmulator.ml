module Map = Map.Make (String)

open Data

let apply_binop: Value.t list -> (int -> int -> int) -> Value.t list = fun stack op -> 
  let (y, x, stack') = Util.unsafe_pop_two stack in
  match (x, y) with
  | (Value.Int x', Value.Int y') -> (Value.Int (op x' y'))::stack'
  | _                            -> failwith "Binops are supported only for ints"

let find_label code label = 
  let indexed_code = List.mapi (fun x y -> (x, y)) code in
  let positions = List.find_all (fun (x, cmd) ->
  match cmd with
  | S_LABEL label' -> label = label'
  | _              -> false) indexed_code
  in match positions with
  | [(x, y)] -> x
  | []  -> failwith (Printf.sprintf "No label %s found" label)
  | _   -> failwith (Printf.sprintf "%d labels '%s' found instead of one" (List.length positions) label)

let check_jmp_condition jmp x = 
  match jmp with 
  | Jz  -> x =  Value.Int 0
  | Jnz -> x <> Value.Int 0

let run code =
  let code_list = code in
  let rec run' (state, stack, stack_frames, instruction_pointer) code =
    if instruction_pointer = Array.length code then () 
    else let instr = code.(instruction_pointer) in
       run'
         (match instr with
          | S_PUSH n -> (state, n::stack, stack_frames, instruction_pointer + 1)

          | S_LD x   -> (state, (Map.find x state)::stack, stack_frames, instruction_pointer + 1)

          | S_ST x   -> let (y, stack') = Util.unsafe_pop_one stack in
            (Map.add x y state, stack', stack_frames, instruction_pointer + 1)

          | S_BINARY_ARITHM_OP  op -> 
            (state, apply_binop stack (fun x y -> Ops.binary_op_to_fun  op x y), stack_frames, instruction_pointer + 1)

          | S_BINARY_COMPARE_OP op -> 
            (state, apply_binop stack (fun x y -> Ops.compare_op_to_fun op x y), stack_frames, instruction_pointer + 1)

          | S_BINARY_LOGICAL_OP op -> 
            (state, apply_binop stack (fun x y -> Ops.logical_op_to_fun op x y), stack_frames, instruction_pointer + 1)

          | S_CONDITIONAL_JMP (op, label) -> let (y, stack') = Util.unsafe_pop_one stack in 
            if check_jmp_condition op y then (state, stack', stack_frames, find_label code_list label)
                                        else (state, stack', stack_frames, instruction_pointer + 1)

          | S_JMP label       -> (state, stack, stack_frames, find_label code_list label)

          | S_LABEL _         -> (state, stack, stack_frames, instruction_pointer + 1)

          | S_FUNC_BEGIN args -> 
            let (ret_addr, stack') = Util.unsafe_pop_one stack in  
            let fold_arg = fun state (arg_name, arg_val) -> Map.add arg_name arg_val state
            in
            let (arg_vals, stack'') = Util.unsafe_pop_many (List.length args) stack' in
            let state' = List.fold_left fold_arg Map.empty (List.combine args arg_vals) in
            (state', ret_addr::stack'', stack_frames, instruction_pointer + 1) 

          | S_CALL (label, _) -> (Map.empty, (Value.Int (instruction_pointer+1))::stack,  state::stack_frames, find_label code_list label)

          | S_BUILTIN (func_name, arg_cnt) -> 
              let (args, stack') = Util.unsafe_pop_many arg_cnt stack in 
              (state, (Builtins.get_builtin func_name args)::stack', stack_frames, instruction_pointer + 1) 

          | S_RET             -> let (x, y, stack') = Util.unsafe_pop_two stack in
                                 let (state', stack_frames') = Util.unsafe_pop_one stack_frames in (
                                 match y with 
                                 | Value.Int y' -> (state', x::stack', stack_frames', y')
                                 | _            -> failwith "Expected int at the top of the stack as return address"
                                 )
                                 

          | S_DROP            -> let (x, stack') = Util.unsafe_pop_one stack in 
                                 (state, stack', stack_frames, instruction_pointer + 1)

          | S_FUNC_END        -> (state, stack,  stack_frames, instruction_pointer + 1)

          | S_END             -> (state, stack,  stack_frames, Array.length code)
         ) code
  in
  let get_entry_point code = 
    let rev_codei = List.rev (List.mapi   (fun i x -> (x, i)) code) in
    let ends = List.filter (fun (x, i) -> match x with S_FUNC_END -> true | _ -> false) rev_codei in
    match ends with 
    | []           -> 0
    | (_, pos)::xs -> pos + 1
  in
  run' (Map.empty, [], [], get_entry_point code) (Array.of_list code)
