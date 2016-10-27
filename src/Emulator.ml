module Map = Map.Make (String)

open Data
type state = (int Map.t) * ((string list * statement)  Map.t) * (int list) * (int list)

let rec evalArgs: state -> (expr list) -> ((int list) * state) = 
  fun c ops   -> match ops with
  | []        -> ([], c)
  | (e::ops') -> 
      let (c', y) = eval c e in 
      let (res, c'') = evalArgs c' ops' in
      (y::res, c'')

and callFunc: string -> (expr list) -> state -> (state * int) = 
  fun funcName ops ((vars, funcs, input, output) as c) -> 
    let (ops', (_, _, input', output')) = evalArgs c ops in 
    let (args, body) = Map.find funcName funcs in
    let argsBinding = (List.map2 (fun x y -> (x, y)) args ops') in 
    let vars' = List.fold_left (fun m (x, y) -> Map.add x y m) Map.empty argsBinding in
    let ((vars'', funcs'', input'', output''), y, _) = evalStmt (vars', funcs, input', output') body in
    ((vars, funcs, input'', output''), y)
    
    
and eval: state -> expr -> (state * int) =
  fun ((vars, funcs, input, output) as c) e -> 
    let calc op l r = let (c',  lhs) = eval c  l in
                      let (c'', rhs) = eval c' r in
                      (c'', (op lhs rhs)) in 
    match e with
    | Const  n     -> (c, n)
    | Var    x     -> (c, Map.find x vars)
    | FunctionCallExpr  (funcName, ops) -> callFunc funcName ops c
    | BinaryArithmExpr  (op, l, r) -> calc (fun l r -> binary_op_to_fun op l r)  l r
    | BinaryCompareExpr (op, l, r) -> calc (fun l r -> compare_op_to_fun op l r) l r
    | BinaryLogicalExpr (op, l, r) -> calc (fun l r -> logical_op_to_fun op l r) l r

and evalStmt: state -> statement -> (state * int * bool) =
  fun ((vars, funcs, input, output) as c) stmt ->
    match stmt with
    | Skip          -> (c, 0, false)
    | FunctionDef (x, y, z) ->  ((vars, Map.add x (y, z) funcs, input, output), 0, false)
    | Seq    (l, r) -> let (c', y, isReturn) = evalStmt c l in 
      if isReturn then (c', y, isReturn) else evalStmt c' r
    | Assign (x, e) -> let ((_, _, input', output'), y) = eval c e in 
                       ((Map.add x y vars, funcs, input', output'), 0, false)
    | Write   e     -> let ((_, _, input', output'), y) = eval c e in
                       ((vars, funcs, input', output' @ [y]), 0, false)
    | Read    x     -> (
       match input with
       | []        -> assert false
       | y::input' -> ((Map.add x y vars, funcs, input', output), 0, false)
    )
    | If     (cond, if_block, else_block) -> 
        let (c', y) = eval c cond in
        if y != 0 then evalStmt c' if_block
        else evalStmt c' else_block
    | While  (cond, block) -> 
      let rec run_while ((vars, funcs, input, output) as c) cond block = 
        let (c', y) = eval c cond in
        if y == 0 then (c', 0, false) 
        else let (c'', y, isReturn) = evalStmt c' block in 
        if isReturn then (c'', y, isReturn) else run_while c'' cond block
      in run_while c cond block
    | FunctionCallStatement (funcName, ops) -> let (c', y) = callFunc funcName ops c in (c', y, false)
    | Return e -> let (c', y) = eval c e in (c', y, true)

let run input stmt =
  let ((_, _, _, result), _, _) = evalStmt (Map.empty, Map.empty, input, []) stmt in
  result
