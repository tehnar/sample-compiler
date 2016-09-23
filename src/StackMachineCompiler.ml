open Data 

let rec compile_expr expr =
  match expr with
  | Var    x     -> [S_LD   x]
  | Const  n     -> [S_PUSH n]
  | Add   (l, r) -> compile_expr l @ compile_expr r @ [S_ADD]
  | Sub   (l, r) -> compile_expr l @ compile_expr r @ [S_SUB]
  | Mul   (l, r) -> compile_expr l @ compile_expr r @ [S_MUL]
  | Div   (l, r) -> compile_expr l @ compile_expr r @ [S_DIV]
  | Mod   (l, r) -> compile_expr l @ compile_expr r @ [S_MOD]
  | Le    (l, r) -> compile_expr l @ compile_expr r @ [S_LE]
  | Leq   (l, r) -> compile_expr l @ compile_expr r @ [S_LEQ]
  | Ge    (l, r) -> compile_expr l @ compile_expr r @ [S_GE]
  | Geq   (l, r) -> compile_expr l @ compile_expr r @ [S_GEQ]
  | And   (l, r) -> compile_expr l @ compile_expr r @ [S_AND]
  | Or    (l, r) -> compile_expr l @ compile_expr r @ [S_OR]

let rec compile_statement stmt =
  match stmt with
  | Skip          -> []
  | Assign (x, e) -> compile_expr e @ [S_ST x]
  | Read    x     -> [S_READ; S_ST x]
  | Write   e     -> compile_expr e @ [S_WRITE]
  | Seq    (l, r) -> compile_statement l @ compile_statement r

