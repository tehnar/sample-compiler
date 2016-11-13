module Map = Map.Make (String)

open Data
type state = (int Map.t) * ((string list * statement)  Map.t)

let rec eval_args: state -> (expr list) -> ((int list) * state) = 
  fun c ops   -> match ops with
  | []        -> ([], c)
  | (e::ops') -> 
      let (c', y) = eval c e in 
      let (res, c'') = eval_args c' ops' in
      (y::res, c'')

and call_func: string -> (expr list) -> state -> (state * int) = 
  fun func_name ops ((vars, funcs) as c) -> 
    let (ops', _) = eval_args c ops in 
    let (args, body) = Map.find func_name funcs in
    let args_binding = (List.map2 (fun x y -> (x, y)) args ops') in 
    let vars' = List.fold_left (fun m (x, y) -> Map.add x y m) Map.empty args_binding in
    let ((vars'', funcs''), y, _) = evalStmt (vars', funcs) body in
    ((vars, funcs), y)
    
    
and eval: state -> expr -> (state * int) =
  fun ((vars, funcs) as c) e -> 
    let calc op l r = let (c',  lhs) = eval c  l in
                      let (c'', rhs) = eval c' r in
                      (c'', (op lhs rhs)) in 
    match e with
    | Const  n     -> (c, n)
    | Var    x     -> (c, Map.find x vars)
    | FunctionCallExpr  (func_name, ops) -> call_func func_name ops c
    | BinaryArithmExpr  (op, l, r) -> calc (fun l r -> binary_op_to_fun op l r)  l r
    | BinaryCompareExpr (op, l, r) -> calc (fun l r -> compare_op_to_fun op l r) l r
    | BinaryLogicalExpr (op, l, r) -> calc (fun l r -> logical_op_to_fun op l r) l r

and evalStmt: state -> statement -> (state * int * bool) =
  fun ((vars, funcs) as c) stmt ->
    match stmt with
    | Skip          -> (c, 0, false)

    | FunctionDef (x, y, z) ->  ((vars, Map.add x (y, z) funcs), 0, false)

    | Seq    (l, r) -> let (c', y, isReturn) = evalStmt c l in 
      if isReturn then (c', y, isReturn) else evalStmt c' r

    | Assign (x, e) -> let (_, y) = eval c e in 
                       ((Map.add x y vars, funcs), 0, false)

    | Write   e     -> let (_, y) = eval c e in
                       Printf.printf "%d\n" y;
                       ((vars, funcs), 0, false)

    | Read    x     -> 
      Printf.printf "> ";
      let y = read_int () in
      ((Map.add x y vars, funcs), 0, false)

    | If     (cond, if_block, else_block) -> 
        let (c', y) = eval c cond in
        if y != 0 then evalStmt c' if_block
        else evalStmt c' else_block

    | While  (cond, block) -> 
      let rec run_while ((vars, funcs) as c) cond block = 
        let (c', y) = eval c cond in
        if y == 0 then (c', 0, false) 
        else let (c'', y, isReturn) = evalStmt c' block in 
        if isReturn then (c'', y, isReturn) else run_while c'' cond block
      in run_while c cond block

    | FunctionCallStatement (func_name, ops) -> let (c', y) = call_func func_name ops c in (c', y, false)

    | Return e -> let (c', y) = eval c e in (c', y, true)

let run stmt = ignore @@ evalStmt (Map.empty, Map.empty) stmt
