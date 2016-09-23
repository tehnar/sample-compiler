open Data

let bool_to_int b = match b with
  | false -> 0
  | true  -> 1

let int_to_bool x = match x with
  | 0     -> false 
  | _     -> true 

let rec eval state expr =
  match expr with
  | Const  n     -> n
  | Var    x     -> state x
  | Add   (l, r) -> eval state l  +   eval state r
  | Sub   (l, r) -> eval state l  -   eval state r
  | Mul   (l, r) -> eval state l  *   eval state r
  | Div   (l, r) -> eval state l  /   eval state r
  | Mod   (l, r) -> eval state l mod  eval state r
  | Le    (l, r) -> bool_to_int (eval state l <  eval state r)
  | Leq   (l, r) -> bool_to_int (eval state l <= eval state r)
  | Ge    (l, r) -> bool_to_int (eval state l >  eval state r)
  | Geq   (l, r) -> bool_to_int (eval state l >= eval state r)
  | And   (l, r) -> bool_to_int (int_to_bool (eval state l) && int_to_bool (eval state r))
  | Or    (l, r) -> bool_to_int (int_to_bool (eval state l) || int_to_bool (eval state r))

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
  in
  let (_, _, result) = run' ([], input, []) stmt in
  result

let srun : int list -> (int -> int -> int) -> int list = fun stack op -> 
  match stack with
  | [] | _::[]   -> assert false
  | y::x::stack' -> (op x y)::stack'

let srun input code =
  let rec srun' (state, stack, input, output) code =
    match code with
    | []       -> output
    | i::code' ->
       srun'
         (match i with
          | S_READ -> (
            match stack with 
            | []        -> assert false
            | y::input' -> (state, y::stack, input', output)
          )
          | S_WRITE -> (
            match stack with
            | []        -> assert false
            | y::stack' -> (state, stack', input, output @ [y])
          )
          | S_PUSH n ->
             (state, n::stack, input, output)
          | S_LD x ->
             (state, (List.assoc x state)::stack, input, output)
          | S_ST x -> (
             match stack with
             | []        -> assert false
             | y::stack' -> ((x, y)::state, stack', input, output)
          )
          | S_ADD ->
             (state, srun stack (fun x y -> x + y), input, output)
          | S_SUB ->
             (state, srun stack (fun x y -> x - y), input, output)
          | S_MUL ->
             (state, srun stack (fun x y -> x * y), input, output)
          | S_DIV -> 
             (state, srun stack (fun x y -> x / y), input, output)
          | S_MOD -> 
             (state, srun stack (fun x y -> x mod y), input, output)
          | S_LE  -> 
             (state, srun stack (fun x y -> bool_to_int (x < y)), input, output)
          | S_LEQ -> 
             (state, srun stack (fun x y -> bool_to_int (x <= y)), input, output)
          | S_GE  -> 
             (state, srun stack (fun x y -> bool_to_int (x < y)), input, output)
          | S_GEQ -> 
             (state, srun stack (fun x y -> bool_to_int (x <= y)), input, output)
          | S_AND -> 
             (state, srun stack (fun x y -> bool_to_int (int_to_bool x && int_to_bool y)), input, output)
          | S_OR  -> 
             (state, srun stack (fun x y -> bool_to_int (int_to_bool x || int_to_bool y)), input, output)
         )
         code'
  in
  srun' ([], [], input, []) code
