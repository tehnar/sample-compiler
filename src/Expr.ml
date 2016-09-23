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

type opnd = R of int | S of int | M of string | L of int

let x86regs = [|"%ebx"; "%ecx"; "%esi"; "%edi"; "%eax"; "%edx"|]
let num_of_regs = Array.length x86regs
let word_size = 4

let eax = R 4

let allocate env stack =
  match stack with
  | []                              -> R 0
  | (S n)::_                        -> env#allocate (n+1); S (n+1)
      (* It doesn't allocate %eax and %edx. *)
  | (R n)::_ when n < num_of_regs-3 -> R (n+1)
  | _                               -> S 0

type x86instr =
  | X86Add  of opnd * opnd
  | X86Mul  of opnd * opnd
  | X86Mov  of opnd * opnd
  | X86Push of opnd
  | X86Pop  of opnd
  | X86Ret
  | X86Call of string

module S = Set.Make (String)

class x86env =
  object(self)
    val    local_vars = ref S.empty
    method local x    = local_vars := S.add x !local_vars
    method local_vars = S.elements !local_vars

    val    allocated  = ref 0
    method allocate n = allocated := max n !allocated
    method allocated  = !allocated
  end

let slot : opnd -> string = function
  | (R i) -> x86regs.(i)
  | (S i) -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
  | (M x) -> x
  | (L i) -> Printf.sprintf "$%d" i
let x86print : x86instr -> string = function
  | X86Add (s1, s2) -> Printf.sprintf "\taddl\t%s,\t%s"  (slot s1) (slot s2)
  | X86Mul (s1, s2) -> Printf.sprintf "\timull\t%s,\t%s" (slot s1) (slot s2)
  | X86Mov (s1, s2) -> Printf.sprintf "\tmovl\t%s,\t%s"  (slot s1) (slot s2)
  | X86Push s       -> Printf.sprintf "\tpushl\t%s"     (slot s )
  | X86Pop  s       -> Printf.sprintf "\tpopl\t%s"      (slot s )
  | X86Ret          -> "\tret"
  | X86Call p       -> Printf.sprintf "\tcall\t%s" p

let x86compile : x86env -> instr list -> x86instr list = fun env code ->
  let rec x86compile' stack code =
    match code with
    | []       -> []
    | i::code' ->
       let (stack', x86code) =
         match i with
         | S_READ   -> ([eax], [X86Call "read"])
         | S_WRITE  -> ([], [X86Push (R 0); X86Call "write"; X86Pop (R 0)])
         | S_PUSH n ->
           let s = allocate env stack in
           (s::stack, [X86Mov (L n, s)])
         | S_LD x   ->
           env#local x;
           let s = allocate env stack in
           (s::stack, [X86Mov (M x, s)])
         | S_ST x   ->
           env#local x;
           let s::stack' = stack in
           (stack', [X86Mov (s, M x)])
         | S_ADD   ->
           let x::y::stack' = stack in
           (match x, y with
            | S _, S _ ->
              (y::stack', [X86Mov (x, eax);
                           X86Add (eax, y)])
            | _ ->
              (y::stack', [X86Add (x, y)]))
         | S_MUL   ->
           let x::y::stack' = stack in
            (match x, y with
            | S _, S _ ->
              (y::stack', [X86Mov (y, eax);
                           X86Mul (x, eax);
                           X86Mov (eax, y)])
            | _ ->
              (y::stack', [X86Mul (x, y)]))
       in
       x86code @ x86compile' stack' code'
  in
  x86compile' [] code

let genasm stmt =
  let env = new x86env in
  let code = x86compile env @@ compile_stmt stmt in
  let asm = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in

  !"\t.text";
  List.iter (fun x ->
      !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
    env#local_vars;
  !"\t.globl\tmain";
  let prologue, epilogue =
    if env#allocated = 0
    then (fun () -> ()), (fun () -> ())
    else
      (fun () ->
         !"\tpushl\t%ebp";
         !"\tmovl\t%esp,\t%ebp";
         !(Printf.sprintf "\tsubl\t$%d,\t%%esp" (env#allocated * word_size))
      ),
      (fun () ->
         !"\tmovl\t%ebp,\t%esp";
         !"\tpopl\t%ebp"
      )
  in
  !"main:";
  prologue();
  List.iter (fun i -> !(x86print i)) code;
  epilogue();
  !"\txorl\t%eax,\t%eax";
  !"\tret";

  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (genasm stmt);
  close_out outf;
  Sys.command (Printf.sprintf "gcc -m32 -o %s ../runtime/runtime.o %s.s" name name)
