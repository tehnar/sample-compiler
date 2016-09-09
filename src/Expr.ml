type expr =
  | Const of int
  | Var   of string
  | Add   of expr * expr
  | Mul   of expr * expr

let rec eval state expr =
  match expr with
  | Const  n     -> n
  | Var    x     -> state x
  | Add   (l, r) -> eval state l + eval state r
  | Mul   (l, r) -> eval state l * eval state r

type stmt =
  | Skip
  | Read   of string
  | Write  of expr
  | Assign of string * expr
  | Seq    of stmt * stmt
                       
let run input stmt =
  let rec run' ((state, input, output) as c) stmt =
    let state' x = List.assoc x state in
    match stmt with
    | Skip          -> c
    | Seq    (l, r) -> run' (run' c l) r
    | Assign (x, e) -> ((x, eval state' e) :: state, input, output)
    | Write   e     -> (state, input, output @ [eval state' e])
    | Read    x     ->
       let y::input' = input in
       ((x, y) :: state, input', output)
  in
  let (_, _, result) = run' ([], input, []) stmt in
  result
    
type instr =
  | S_READ
  | S_WRITE
  | S_PUSH  of int
  | S_LD    of string
  | S_ST    of string
  | S_ADD
  | S_MUL

let srun input code =
  let rec srun' (state, stack, input, output) code =
    match code with
    | []       -> output
    | i::code' ->
       srun'
         (match i with
          | S_READ ->
             let y::input' = input in
             (state, y::stack, input', output)
          | S_WRITE ->
             let y::stack' = stack in
             (state, stack', input, output @ [y])
          | S_PUSH n ->
             (state, n::stack, input, output)
          | S_LD x ->
             (state, (List.assoc x state)::stack, input, output)
          | S_ST x ->
             let y::stack' = stack in
             ((x, y)::state, stack', input, output)
          | S_ADD ->
             let y::x::stack' = stack in
             (state, (x+y)::stack', input, output)
          | S_MUL ->
             let y::x::stack' = stack in
             (state, (x*y)::stack', input, output)
         )
         code'
  in
  srun' ([], [], input, []) code

let rec compile_expr expr =
  match expr with
  | Var    x     -> [S_LD   x]
  | Const  n     -> [S_PUSH n]
  | Add   (l, r) -> compile_expr l @ compile_expr r @ [S_ADD]
  | Mul   (l, r) -> compile_expr l @ compile_expr r @ [S_MUL]

let rec compile_stmt stmt =
  match stmt with
  | Skip          -> []
  | Assign (x, e) -> compile_expr e @ [S_ST x]
  | Read    x     -> [S_READ; S_ST x]
  | Write   e     -> compile_expr e @ [S_WRITE]
  | Seq    (l, r) -> compile_stmt l @ compile_stmt r

let x86regs = [|"%eax"; "%ebx"; "%ecx"; "%edx"; "%esi"; "%edi"|]
let num_of_regs = Array.length x86regs
let word_size = 4

type opnd = R of int | S of int | M of string | L of int

let allocate stack =
  match stack with
  | []                              -> R 0
  | (S n)::_                        -> S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> S 0

type x86instr =
  | X86Add  of opnd * opnd
  | X86Mul  of opnd * opnd
  | X86Mov  of opnd * opnd
  | X86Push of opnd
  | X86Pop  of opnd
  | X86Ret
  | X86Call of string

let x86compile : instr list -> x86instr list = fun code ->
  let rec x86compile' stack code =
    match code with
    | []       -> []
    | i::code' ->
       let (stack', x86code) =
         match i with
         | S_READ   -> ([R 0], [X86Call "read"])
         | S_PUSH n ->
            let s = allocate stack in
            (s::stack, [X86Mov (L n, s)])
       in
       x86code @ x86compile' stack' code'
  in
  x86compile' [] code
                                         
