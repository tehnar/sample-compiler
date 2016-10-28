open Data 

let func_name_to_label func_name = Printf.sprintf "func%s" func_name

let rec call_function (func_name, ops) = 
  let compiled_ops = List.map (fun e -> compile_expr e) ops in
  List.flatten (List.rev compiled_ops) @ [S_CALL func_name_to_label func_name]

and compile_expr expr =
  match expr with
  | Var    x     -> [S_LD   x]
  | Const  n     -> [S_PUSH n]
  | BinaryArithmExpr  (op, l, r) -> compile_expr l @ compile_expr r @ [S_BINARY_ARITHM_OP  op]
  | BinaryCompareExpr (op, l, r) -> compile_expr l @ compile_expr r @ [S_BINARY_COMPARE_OP op]
  | BinaryLogicalExpr (op, l, r) -> compile_expr l @ compile_expr r @ [S_BINARY_LOGICAL_OP op]
  | FunctionCallExpr (x, y) -> call_function (x, y)

and compile_statement stmt label_num =
  match stmt with
  | Skip          -> ([], label_num)
  | Assign (x, e) -> (compile_expr e @ [S_ST x], label_num)
  | Read    x     -> ([S_READ; S_ST x], label_num)
  | Write   e     -> (compile_expr e @ [S_WRITE], label_num)
  | Seq    (l, r) -> 
      let (l_compiled, label_num')  = compile_statement l label_num  in
      let (r_compiled, label_num'') = compile_statement r label_num' in
      (l_compiled @ r_compiled, label_num'')

  | If (cond, if_block, else_block) -> 
      let else_label     = Printf.sprintf "else%d"     label_num in
      let after_if_label = Printf.sprintf "after_if%d" label_num in
      let (if_compiled,   label_num')  = compile_statement if_block (label_num + 1) in
      let (else_compiled, label_num'') = compile_statement else_block label_num' in
      (
        compile_expr cond @ [S_CONDITIONAL_JMP (Jz, else_label)] @ 
        if_compiled @ [S_JMP after_if_label; S_LABEL else_label] @
        else_compiled @ [S_LABEL after_if_label], 
        label_num''
      )
  | While (cond, block) -> 
      let cond_label = Printf.sprintf "while_cond%d" label_num in
      let while_label = Printf.sprintf "while_label%d" label_num in
      let (block_compiled, label_num') = compile_statement block (label_num + 1) in
      (
        [S_JMP cond_label; S_LABEL while_label] @ block_compiled @ 
        [S_LABEL cond_label] @ compile_expr cond @ [S_CONDITIONAL_JMP (Jnz, while_label)],
        label_num'
      )
  | FunctionDef (func_name, args, body)  -> 
      let lbl = S_LABEL (func_name_to_label func_name) in
      let beg = S_FUNC_BEGIN args in  
      let body', label_num' = compile_statement body label_num in
      (lbl::beg::body' @ [S_FUNC_END], label_num')

  | FunctionCallStatement (x, y) -> (call_function (x, y) @ [S_DROP], label_num)
  | Return e -> (compile_expr e @ [S_RET], label_num)
 
let compile_code code = 
  let (code, _) = (compile_statement code 0) in code
