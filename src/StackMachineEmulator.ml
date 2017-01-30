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

let make_step threads thread_num code code_list =
  let (state, stack, stack_frames, instruction_pointer, thread_id) as thread = List.nth threads thread_num in
  if instruction_pointer = Array.length code then List.filter (fun e -> thread != e) threads
  else 
    let instr = code.(instruction_pointer) in
    let (new_thread_state, new_threads) = (match instr with
        | S_PUSH n -> ((state, n::stack, stack_frames, instruction_pointer + 1, thread_id), [])

        | S_LD x   -> ((state, (Map.find x state)::stack, stack_frames, instruction_pointer + 1, thread_id), [])

        | S_ST x   -> let (y, stack') = Util.unsafe_pop_one stack in
          ((Map.add x y state, stack', stack_frames, instruction_pointer + 1, thread_id), [])

        | S_FUNC_REF_NAME name -> 
            let stack' = (Value.of_func_ref name)::stack in
            ((state, stack', stack_frames, instruction_pointer + 1, thread_id), [])

        | S_ELEM   -> 
          let (i, a, stack') = Util.unsafe_pop_two stack in
          let arr = Value.to_array a in
          ((state, arr.(Value.to_int i)::stack', stack_frames, instruction_pointer + 1, thread_id), [])

        | S_STA    ->
          let (e, i, a, stack') = Util.unsafe_pop_three stack in
          (Value.to_array a).(Value.to_int i) <- e;
          ((state, stack', stack_frames, instruction_pointer + 1, thread_id), [])

        | S_ARRAY (boxed, n) -> 
          let (elems, stack') = Util.unsafe_pop_many n stack in
          ((state, (Value.of_array boxed (Array.of_list elems))::stack', stack_frames, instruction_pointer + 1, thread_id), [])

        | S_BINARY_ARITHM_OP  op -> 
          ((state, apply_binop stack (fun x y -> Ops.binary_op_to_fun  op x y), stack_frames, instruction_pointer + 1, thread_id), [])

        | S_BINARY_COMPARE_OP op -> 
          ((state, apply_binop stack (fun x y -> Ops.compare_op_to_fun op x y), stack_frames, instruction_pointer + 1, thread_id), [])

        | S_BINARY_LOGICAL_OP op -> 
          ((state, apply_binop stack (fun x y -> Ops.logical_op_to_fun op x y), stack_frames, instruction_pointer + 1, thread_id), [])

        | S_CONDITIONAL_JMP (op, label) -> let (y, stack') = Util.unsafe_pop_one stack in 
          if check_jmp_condition op y then ((state, stack', stack_frames, find_label code_list label, thread_id), [])
                                      else ((state, stack', stack_frames, instruction_pointer + 1, thread_id), [])

        | S_JMP label       -> ((state, stack, stack_frames, find_label code_list label, thread_id), [])

        | S_LABEL _         -> ((state, stack, stack_frames, instruction_pointer + 1, thread_id), [])

        | S_FUNC_BEGIN args -> 
          let (ret_addr, stack') = Util.unsafe_pop_one stack in  
          let fold_arg = fun state (arg_name, arg_val) -> Map.add arg_name arg_val state
          in
          let (arg_vals, stack'') = Util.unsafe_pop_many (List.length args) stack' in
          let state' = List.fold_left fold_arg Map.empty (List.combine args arg_vals) in
          ((state', ret_addr::stack'', stack_frames, instruction_pointer + 1, thread_id), [])

        | S_CALL (label, _) -> ((Map.empty, (Value.Int (instruction_pointer+1))::stack,  state::stack_frames, find_label code_list label, thread_id), [])

        | S_REF_CALL _      -> 
          let (func, stack') = Util.unsafe_pop_one stack in
          let label = Value.to_func_name func in
          ((Map.empty, (Value.Int (instruction_pointer + 1))::stack', state::stack_frames, find_label code_list label, thread_id), [])

        | S_BUILTIN (func_name, arg_cnt) -> 
            let (args, stack') = Util.unsafe_pop_many arg_cnt stack in 
              if func_name = "thread_create" then
                let (func_ref, params) = Util.unsafe_pop_one args in
                let func_start = find_label code_list @@ Value.to_func_name func_ref in
                let new_thread = (Value.of_thread @@ Thread.create (fun x -> x) ()) in
                ((state, new_thread::stack', stack_frames, instruction_pointer + 1, thread_id), 
                [(Map.empty, (Value.Int (-1))::params, [], func_start, new_thread)])
              else if func_name = "thread_join" then
                let thread = Util.match_one_arg args in
                if List.exists (fun (_, _, _, _, thread_id) -> thread_id == thread) threads then
                  ((state, stack, stack_frames, instruction_pointer, thread_id), [])
                else
                  ((state, (Value.Int 0)::stack', stack_frames, instruction_pointer + 1, thread_id), [])
              else
                ((state, (Builtins.get_builtin func_name args)::stack', stack_frames, instruction_pointer + 1, thread_id), [])
                
        | S_RET             -> 
            if stack_frames = [] then 
              ((state, stack, stack_frames, Array.length code, thread_id), [])
            else
              let (x, y, stack') = Util.unsafe_pop_two stack in
              let (state', stack_frames') = Util.unsafe_pop_one stack_frames in (
                match y with 
                | Value.Int y' -> ((state', x::stack', stack_frames', y', thread_id), [])
                | _            -> failwith "Expected int at the top of the stack as return address"
              )
                               

        | S_DROP            -> let (x, stack') = Util.unsafe_pop_one stack in 
                               ((state, stack', stack_frames, instruction_pointer + 1, thread_id), [])

        | S_FUNC_END        -> ((state, stack,  stack_frames, instruction_pointer + 1, thread_id), [])

        | S_END             -> ((state, stack,  stack_frames, Array.length code, thread_id), [])
       ) in
      let (l, r) = Util.unsafe_pop_many thread_num threads in
      (l @ [new_thread_state] @ (List.tl r) @ new_threads)


let run code =
  let code_list = code in
  let code = Array.of_list code_list in
  let rec run' threads = 
    if List.length threads = 0 then ()
    else let thread_num = Random.int (List.length threads) in
    (*Printf.eprintf "thread_num is %d\n" thread_num;*)
    let threads' = make_step threads thread_num code code_list in
    run' threads' 
  in
  let get_entry_point code = 
    let rev_codei = List.rev (List.mapi   (fun i x -> (x, i)) code) in
    let ends = List.filter (fun (x, i) -> match x with S_FUNC_END -> true | _ -> false) rev_codei in
    match ends with 
    | []           -> 0
    | (_, pos)::xs -> pos + 1
  in
  ignore @@ run' [(Map.empty, [], [], get_entry_point code_list, Value.of_thread @@ Thread.self ())] 
