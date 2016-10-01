open Data

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
