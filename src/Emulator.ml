module Map = Map.Make (String)

(* TODO int -> Value.t, refactor state *)

open Data
type state = (Value.t Map.t) * ((string list * statement)  Map.t)

let rec eval_args: state -> (expr list) -> (Value.t list) = 
  fun c ops   -> match ops with
  | []        -> []
  | (e::ops') -> let y = eval c e in y::(eval_args c ops')


and call_builtin_func func_name args = Builtins.get_builtin func_name args

and call_user_func    func_name funcs args = 
  let (arg_names, body) = Map.find func_name funcs in
  let args_binding = (List.map2 (fun x y -> (x, y)) arg_names args) in 
  let vars = List.fold_left (fun m (x, y) -> Map.add x y m) Map.empty args_binding in
  let (_, y, _) = evalStmt (vars, funcs) body in
  y

and call_func: string -> (expr list) -> state -> Value.t = 
  fun func_name ops (vars, funcs) -> 
    let arg_values = eval_args (vars, funcs) ops in 
    if Map.mem func_name funcs then
      call_user_func    func_name funcs arg_values
    else
      call_builtin_func func_name arg_values
    
    
and eval: state -> expr -> Value.t =
  fun ((vars, funcs) as c) e -> 
    let calc op l r = let lhs = eval c l in
                      let rhs = eval c r in
                      match (lhs, rhs) with
                      | (Value.Int lhs', Value.Int rhs') -> Value.Int (op lhs' rhs')
                      | _                                -> failwith "Binops are supported only for ints"
    in

    match e with
    | Const  n     -> n
    | Var    x     -> Map.find x vars
    | FunctionCallExpr  (func_name, ops) -> call_func func_name ops c
    | BinaryArithmExpr  (op, l, r) -> calc (fun l r -> Ops.binary_op_to_fun op l r)  l r
    | BinaryCompareExpr (op, l, r) -> calc (fun l r -> Ops.compare_op_to_fun op l r) l r
    | BinaryLogicalExpr (op, l, r) -> calc (fun l r -> Ops.logical_op_to_fun op l r) l r

and evalStmt: state -> statement -> (state * Value.t * bool) =
  fun ((vars, funcs) as c) stmt ->
    match stmt with
    | Skip          -> (c, Value.Int 0, false)

    | FunctionDef (x, y, z) ->  ((vars, Map.add x (y, z) funcs), Value.Int 0, false)

    | Seq    (l, r) -> let (c', y, isReturn) = evalStmt c l in 
      if isReturn then (c', y, isReturn) else evalStmt c' r

    | Assign (x, e) -> let y = eval c e in 
                       ((Map.add x y vars, funcs), Value.Int 0, false)

    | If     (cond, if_block, else_block) -> 
        let y = eval c cond in
        if y <> Value.Int 0 then evalStmt c if_block
                            else evalStmt c else_block

    | While  (cond, block) -> 
      let rec run_while ((vars, funcs) as c) cond block = 
        let y = eval c cond in
        if y = Value.Int 0 then (c, y, false)
        else let (c', y', isReturn) = evalStmt c block in 
        if isReturn then (c', y', isReturn) else run_while c' cond block
      in run_while c cond block

    | FunctionCallStatement (func_name, ops) -> let _ = call_func func_name ops c in (c, Value.Int 0, false)

    | Return e -> let y = eval c e in (c, y, true)

let run stmt = ignore @@ evalStmt (Map.empty, Map.empty) stmt
