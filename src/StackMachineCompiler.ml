open Data 

let rec compile_expr expr =
  match expr with
  | Var    x     -> [S_LD   x]
  | Const  n     -> [S_PUSH n]
  | BinaryArithmExpr  (op, l, r) -> compile_expr l @ compile_expr r @ [S_BINARY_ARITHM_OP  op]
  | BinaryCompareExpr (op, l, r) -> compile_expr l @ compile_expr r @ [S_BINARY_COMPARE_OP op]
  | BinaryLogicalExpr (op, l, r) -> compile_expr l @ compile_expr r @ [S_BINARY_LOGICAL_OP op]

let rec compile_statement stmt =
  match stmt with
  | Skip          -> []
  | Assign (x, e) -> compile_expr e @ [S_ST x]
  | Read    x     -> [S_READ; S_ST x]
  | Write   e     -> compile_expr e @ [S_WRITE]
  | Seq    (l, r) -> compile_statement l @ compile_statement r

