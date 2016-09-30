module Expr =
  struct

    open Language.Expr

    let rec eval state = function
    | Const  n -> n
    | Var    x -> state x
    | Binop  _ -> failwith "not supported"
 
  end
  
module Stmt =
  struct

    open Language.Stmt

    let eval input stmt =
      let rec eval' ((state, input, output) as c) stmt =
	let state' x = List.assoc x state in
	match stmt with
	| Skip          -> c
	| Seq    (l, r) -> eval' (eval' c l) r
	| Assign (x, e) -> ((x, Expr.eval state' e) :: state, input, output)
	| Write   e     -> (state, input, output @ [Expr.eval state' e])
	| Read    x     ->
	    let y::input' = input in
	    ((x, y) :: state, input', output)
      in
      let (_, _, result) = eval' ([], input, []) stmt in
      result

  end
