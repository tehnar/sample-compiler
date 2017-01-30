module Map = Map.Make (String)

open Data

let rec reduce_args vars funcs threads= function 
| [] -> []
| (Const x)::xs -> let xs' = reduce_args vars funcs threads xs in (Const x)::xs'
| x::xs ->  (reduceExpr vars funcs threads x)::xs
and 
is_args_reduced args = not @@ List.exists (fun e -> match e with Const _ -> false | _ -> true) args
and
do_call (arg_names, body) args =
  let args_binding = (List.map2 (fun x y -> (x, y)) arg_names args) in 
  let vars = List.fold_left (fun m (x, y) -> Map.add x y m) Map.empty args_binding in
  Scope (vars, body)
and 
call_builtin : string -> Value.t list -> ((string list * statement) Map.t) -> ((Thread.t * statement) list) ref -> expr = 
  fun func_name args funcs threads ->  
  if func_name = "thread_create" then (
    let (func_name', param) = Util.match_two_args args in
    let func = Map.find (Value.to_func_name func_name') funcs in 
    let thread = Thread.create (fun x -> x) () in
    threads := (thread, do_call func [param])::(!threads); 
    Const (Value.of_thread thread)
  )
  else if func_name = "thread_join" then (
    let thrd = Value.to_thread (Util.match_one_arg args) in
    if List.exists (fun (t, _) -> thrd == t) !threads then FunctionCallExpr (func_name, List.map (fun e -> Const e) args)
    else Const (Value.Int 0)
  )
  else Const (Builtins.get_builtin func_name args)
and

reduceStmt vars funcs threads stmt= match stmt with
| Scope (_, Skip) -> (vars, Skip)
| Scope (_, Return _) -> (vars, Skip)
| Scope (vars', stmt') -> let (vars'', stmt'') = reduceStmt vars' funcs threads stmt' in (vars, Scope (vars'', stmt''))

| Seq  (Skip, stmt') -> reduceStmt vars funcs threads stmt' 
| Seq  (Return (Const x), _) -> (vars, Return (Const x))
| Seq  (stmt', x) -> let (vars', stmt'') = reduceStmt vars funcs threads stmt' in (vars', Seq (stmt'', x))

| Assign (x, Const y) -> (Map.add x y vars, Skip)
| Assign (x, y) -> (vars, Assign (x, reduceExpr vars funcs threads y))

| ArrAssign (Const a, Const i, Const e) ->
  let arr = Value.to_array a in
  let i' = Value.to_int i in 
  arr.(i') <- e;
  (vars, Skip)
| ArrAssign (Const a, Const i, e) -> (vars, ArrAssign (Const a, Const i, reduceExpr vars funcs threads e))
| ArrAssign (Const a, i, e) -> (vars, ArrAssign (Const a, reduceExpr vars funcs threads i, e))
| ArrAssign (a, i, e) -> (vars, ArrAssign (reduceExpr vars funcs threads a, i, e))

| If (Const cond, if_block, else_block) ->
  if cond <> Value.Int 0 then (vars, if_block)
  else (vars, else_block)
| If (cond, if_block, else_block) -> (vars, If (reduceExpr vars funcs threads cond, if_block, else_block))

| While (cond, block) -> reduceStmt vars funcs threads @@ WhileRed (cond, cond, block)

| WhileRed (Const cond, fullCond, block) -> 
  if cond <> Value.Int 0 then (vars, Seq (block, WhileRed (fullCond, fullCond, block)))
  else (vars, Skip)
| WhileRed (cond, fullCond, block) -> (vars, WhileRed (reduceExpr vars funcs threads cond, fullCond, block))

| Return (Const x) -> (vars, Return (Const x))
| Return e -> (vars, Return (reduceExpr vars funcs threads e))

| FunctionDef _ -> (vars, Skip)

| FunctionCallStatement (func_name, args) ->
  if is_args_reduced args then (
    let args' = List.map (function | Const x -> x | _ -> assert false) args in
    if not @@ Map.mem func_name funcs then 
      let res = call_builtin func_name args' funcs threads in
      match res with | (FunctionCallExpr _) -> (vars, FunctionCallStatement (func_name, args)) | _ -> (vars, Skip) 
  else (vars, do_call (Map.find func_name funcs) args')
  )
  else (vars, FunctionCallStatement (func_name, reduce_args vars funcs threads args))

| FunctionRefCallStatement (Const func, ops) ->
  let func_name = Value.to_func_name func in
  (vars, FunctionCallStatement (func_name, ops))
| FunctionRefCallStatement (func, ops) -> 
  let func' = reduceExpr vars funcs threads func in (vars, FunctionRefCallStatement (func', ops))

| Skip -> (vars, Skip) 

and reduceExpr vars funcs threads expr = match expr with
| Const _ -> failwith "Cannot reduce const"

| ScopeExpr (_, Skip) -> failwith "Cannot reduce Skip to expression"
| ScopeExpr (_, Return (Const x)) -> Const x
| ScopeExpr (vars', stmt) -> let (vars'', stmt') = reduceStmt vars' funcs threads stmt in ScopeExpr (vars'', stmt') 

| Var x -> Const (Map.find x vars)

| Elem (Const a, Const i) ->
  let arr = Value.to_array a in
  let i' = Value.to_int i in Const (arr.(i'))
| Elem (Const a, i) -> Elem (Const a, reduceExpr vars funcs threads i)
| Elem (a, i) -> Elem (reduceExpr vars funcs threads a, i)

| Array (boxed, elems) -> 
  if not @@ is_args_reduced elems then Array (boxed, reduce_args vars funcs threads elems)
  else Const (Value.of_array boxed @@ Array.of_list @@ List.map (function | Const x -> x | _ -> failwith "non-reduced elem found") elems)

| BinaryArithmExpr (op, Const l, Const r) -> Const (Value.of_int @@ Ops.binary_op_to_fun op (Value.to_int l) (Value.to_int r))
| BinaryArithmExpr (op, Const l, r)       -> BinaryArithmExpr (op, Const l, reduceExpr vars funcs threads r)
| BinaryArithmExpr (op, l, r)             -> BinaryArithmExpr (op, reduceExpr vars funcs threads l, r)

| BinaryCompareExpr (op, Const l, Const r) -> Const (Value.of_int @@ Ops.compare_op_to_fun op (Value.to_int l) (Value.to_int r))
| BinaryCompareExpr (op, Const l, r)       -> BinaryCompareExpr (op, Const l, reduceExpr vars funcs threads r)
| BinaryCompareExpr (op, l, r)             -> BinaryCompareExpr (op, reduceExpr vars funcs threads l, r)

| BinaryLogicalExpr (op, Const l, Const r) -> Const (Value.of_int @@ Ops.logical_op_to_fun op (Value.to_int l) (Value.to_int r))
| BinaryLogicalExpr (op, Const l, r)       -> BinaryLogicalExpr (op, Const l, reduceExpr vars funcs threads r)
| BinaryLogicalExpr (op, l, r)             -> BinaryLogicalExpr (op, reduceExpr vars funcs threads l, r)

| FunctionCallExpr (func_name, args) -> 
  if is_args_reduced args then (
    let args' = List.map (function | Const x -> x | _ -> assert false) args in
    if not @@ Map.mem func_name funcs then call_builtin func_name args' funcs threads
    else (
      let (arg_names, body) = Map.find func_name funcs in
      let args_binding = (List.map2 (fun x y -> (x, y)) arg_names args') in 
      let vars = List.fold_left (fun m (x, y) -> Map.add x y m) Map.empty args_binding in
      ScopeExpr (vars, body)
    )
  )
  else FunctionCallExpr (func_name, reduce_args vars funcs threads args)


| FunctionRefCallExpr (Const func, ops) ->
  let func_name = Value.to_func_name func in
  FunctionCallExpr (func_name, ops)
| FunctionRefCallExpr (func, ops) -> FunctionRefCallExpr (reduceExpr vars funcs threads func, ops)

| FuncRefName (name) -> Const (Value.of_func_ref name)



let run stmt = 
  Random.init 30;
  let rec find_func_defs funcs = function 
  | FunctionDef (x, y, z) -> Map.add x (y, z) funcs
  | Seq (s1, s2)          -> find_func_defs (find_func_defs funcs s1) s2
  | _                     -> funcs
  in
  let funcs = find_func_defs Map.empty stmt in
  let threads = ref [Thread.create (fun x -> x) (), (Scope (Map.empty, stmt))] in
  let rec run' () = 
    threads := List.filter (function | (_, Skip) -> false | _ -> true) !threads;
    if List.length !threads = 0 then ()
    else ( 
      let thread_num = Random.int @@ List.length !threads in
      let (thrd, stmt) = List.nth !threads thread_num in
      let (_, stmt') = reduceStmt Map.empty funcs threads stmt in
      threads := (thrd, stmt')::(List.filter (fun (t, _) -> t != thrd) !threads);
      run' ()
    )
  in
  run' ()
