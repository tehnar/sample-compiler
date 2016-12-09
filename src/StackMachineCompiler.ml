open Data 

let func_name_to_label func_name = Printf.sprintf "func%s" func_name

let rec call_function (func_name, ops) = 
  let compiled_ops = List.map (fun e -> compile_expr e) ops in
  let call_stmt = if Builtins.is_builtin func_name 
  then [S_BUILTIN (func_name, List.length ops)]
  else [S_CALL    (func_name_to_label func_name, List.length ops)]
  in
  List.flatten (List.rev compiled_ops) @ call_stmt 

and compile_expr expr =
  match expr with
  | Var    x     -> [S_LD   x]
  | Const  n     -> [S_PUSH n]
  | Elem (e, i)  -> compile_expr e @ compile_expr i @ [S_ELEM]
  | Array (boxed, elems) -> List.concat (List.rev_map compile_expr elems) @ [S_ARRAY (boxed, List.length elems)]
  | BinaryArithmExpr  (op, l, r) -> compile_expr l @ compile_expr r @ [S_BINARY_ARITHM_OP  op]
  | BinaryCompareExpr (op, l, r) -> compile_expr l @ compile_expr r @ [S_BINARY_COMPARE_OP op]
  | BinaryLogicalExpr (op, l, r) -> compile_expr l @ compile_expr r @ [S_BINARY_LOGICAL_OP op]
  | FunctionCallExpr (x, y) -> call_function (x, y)

and compile_statement stmt label_num cur_func =
  match stmt with
  | Skip          -> ([], label_num)

  | Assign (x, e) -> (compile_expr e @ [S_ST x], label_num)

  | ArrAssign (a, i, e) -> (compile_expr a @ compile_expr i @ compile_expr e @ [S_STA], label_num)

  | Seq    (l, r) -> 
      let (l_compiled, label_num')  = compile_statement l label_num  cur_func in
      let (r_compiled, label_num'') = compile_statement r label_num' cur_func in
      (l_compiled @ r_compiled, label_num'')

  | If (cond, if_block, else_block) -> 
      let else_label     = Printf.sprintf "else%d%s"     label_num cur_func in
      let after_if_label = Printf.sprintf "after_if%d%s" label_num cur_func in
      let (if_compiled,   label_num')  = compile_statement if_block (label_num + 1) cur_func in
      let (else_compiled, label_num'') = compile_statement else_block label_num'    cur_func in
      (
        compile_expr cond @ 
        [S_CONDITIONAL_JMP (Jz, else_label)] @ 
        if_compiled @ 
        [S_JMP after_if_label; S_LABEL else_label] @
        else_compiled @ 
        [S_LABEL after_if_label], 
        label_num''
      )

  | While (cond, block) -> 
      let cond_label  = Printf.sprintf "while_cond%d%s"  label_num cur_func in
      let while_label = Printf.sprintf "while_label%d%s" label_num cur_func in
      let (block_compiled, label_num') = compile_statement block (label_num + 1) cur_func in
      (
        [S_JMP cond_label; S_LABEL while_label] @ 
        block_compiled @ 
        [S_LABEL cond_label] @ 
        compile_expr cond @ 
        [S_CONDITIONAL_JMP (Jnz, while_label)],
        label_num'
      )
  | FunctionDef (func_name, args, body)  -> 
      let lbl = S_LABEL (func_name_to_label func_name) in
      let beg = S_FUNC_BEGIN args in  
      let body', label_num' = compile_statement body label_num func_name in
      (lbl::beg::body' @ [S_FUNC_END], label_num')

  | FunctionCallStatement (x, y) -> (call_function (x, y) @ [S_DROP], label_num)

  | Return e -> (compile_expr e @ [S_RET], label_num)
 
let compile_code code = 
  let (code, _) = (compile_statement code 0 "") in code
