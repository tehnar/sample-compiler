open Data

let binary_op_to_fun op = match op with
| Add -> (fun l r -> l + r)
| Sub -> (fun l r -> l - r)
| Mul -> (fun l r -> l * r)
| Div -> (fun l r -> l / r)
| Mod -> (fun l r -> l mod r)
 
let compare_op_to_fun op = match op with
| Le  -> (fun l r -> Util.bool_to_int (l <  r))
| Leq -> (fun l r -> Util.bool_to_int (l <= r))
| Ge  -> (fun l r -> Util.bool_to_int (l >  r))
| Geq -> (fun l r -> Util.bool_to_int (l >= r))
| Eq  -> (fun l r -> Util.bool_to_int (l == r))
| Neq -> (fun l r -> Util.bool_to_int (l != r))

let logical_op_to_fun op = match op with
| And -> (fun l r -> Util.bool_to_int (Util.int_to_bool l && Util.int_to_bool r))
| Or  -> (fun l r -> Util.bool_to_int (Util.int_to_bool l || Util.int_to_bool r))

