open Data

let x86regs = [|"%eax"; "%edx"; "%ebx"; "%ecx"; "%esi"; "%edi"|]
let x86lower_regs = [|"%al"; "%dl"; "%bl"; "%cl"|]

let num_of_regs = Array.length x86regs
let num_of_lower_regs = Array.length x86lower_regs

let word_size = 4

type operand = RegisterIndex of int | StackIndex of int | VariableName of string | Literal of int | RegisterLowerIndex of int

let x86eax = RegisterIndex 0
let x86edx = RegisterIndex 1 
let x86al  = RegisterLowerIndex 0
let x86dl  = RegisterLowerIndex 1

let allocate env stack =
  match stack with
  | []                                          -> RegisterIndex 2
  | (StackIndex n)::_                           -> env#allocate (n+1); StackIndex (n+1)
  | (RegisterIndex n)::_ when n < num_of_regs-1 -> RegisterIndex (n+1)
  | _                                           -> env#allocate 0; StackIndex 0

type x86instr =
  | X86Add       of operand * operand
  | X86Sub       of operand * operand
  | X86And       of operand * operand
  | X86Or        of operand * operand
  | X86Mul       of operand * operand
  | X86Mov       of operand * operand
  | X86Xor       of operand * operand
  | X86Cmp       of operand * operand
  | X86Push      of operand
  | X86Pop       of operand
  | X86Div       of operand
  | X86Mod       of operand
  | X86Setl      of operand
  | X86Setle     of operand
  | X86Setg      of operand
  | X86Setge     of operand
  | X86Setne     of operand
  | X86Ret
  | X86Cld    
  | X86Call          of string

module S = Set.Make (String)

class x86environment =
  object(self)
    val    local_vars = ref S.empty
    method local x    = local_vars := S.add x !local_vars
    method local_vars = S.elements !local_vars

    val    allocated  = ref 0
    method allocate n = allocated := max n !allocated
    method allocated  = !allocated
  end

let slot : operand -> string = function
  | (RegisterIndex i) -> x86regs.(i)
  | (StackIndex i) -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
  | (VariableName x) -> x
  | (Literal i) -> Printf.sprintf "$%d" i
  | (RegisterLowerIndex i) -> x86lower_regs.(i)

let x86print : x86instr -> string = function
  | X86Add  (s1, s2)    -> Printf.sprintf "\taddl\t%s,\t%s"  (slot s1) (slot s2)
  | X86Sub  (s1, s2)    -> Printf.sprintf "\tsubl\t%s,\t%s"  (slot s1) (slot s2)
  | X86Mul  (s1, s2)    -> Printf.sprintf "\timull\t%s,\t%s" (slot s1) (slot s2)
  | X86Mov  (s1, s2)    -> Printf.sprintf "\tmovl\t%s,\t%s"  (slot s1) (slot s2)
  | X86Xor  (s1, s2)    -> Printf.sprintf "\txorl\t%s,\t%s"  (slot s1) (slot s2)
  | X86And  (s1, s2)    -> Printf.sprintf "\tandl\t%s,\t%s"  (slot s1) (slot s2)
  | X86Or   (s1, s2)    -> Printf.sprintf "\torl\t%s,\t%s"   (slot s1) (slot s2)
  | X86Push  s          -> Printf.sprintf "\tpushl\t%s"      (slot s )
  | X86Pop   s          -> Printf.sprintf "\tpopl\t%s"       (slot s )
  | X86Div   s          -> Printf.sprintf "\tidivl\t%s"      (slot s )
  | X86Mod   s          -> Printf.sprintf "\tidivl\t%s"      (slot s )
  | X86Cmp  (s1, s2)    -> Printf.sprintf "\tcmpl\t%s,\t%s"  (slot s2) (slot s1) (*Not an error, that's GAS syntax: cmp arg2, arg1 *)
  | X86Setl  s          -> Printf.sprintf "\tsetl\t%s"       (slot s )
  | X86Setle s          -> Printf.sprintf "\tsetle\t%s"      (slot s )
  | X86Setg  s          -> Printf.sprintf "\tsetg\t%s"       (slot s )
  | X86Setge s          -> Printf.sprintf "\tsetge\t%s"      (slot s )
  | X86Setne s          -> Printf.sprintf "\tsetne\t%s"      (slot s )
  | X86Ret              -> "\tret"
  | X86Call  p          -> Printf.sprintf "\tcall\t%s" p
  | X86Cld       -> Printf.sprintf "\tcdq"

let x86_compile_binary_op : x86environment -> operand list -> (operand * operand -> x86instr) -> (operand list) * (x86instr list) = 
  fun env stack op ->
    match stack with
    | [] | _::[]   -> assert false
    | x::y::stack' -> 
      match (x, y) with
      | (_, RegisterIndex _) -> (y::stack', [op (x, y)])
      | (_, _)               -> (y::stack', [X86Mov (x, x86eax); X86Mov (y, x86edx); op (x86eax, x86edx); X86Mov (x86edx, y)])

let x86_compile_logical_op : x86environment -> operand list -> (operand * operand -> x86instr) -> (operand list) * (x86instr list) = 
  fun env stack op ->
    match stack with
    | [] | _::[]   -> assert false
    | x::y::stack' -> 
      let process x x' = [X86Or (Literal 0, x); X86Mov (Literal 0, x); X86Setne x'] in
      let process'     = (process x86eax x86al) @ (process x86edx x86dl) @ [op (x86eax, x86edx)] in
          (y::stack', [X86Mov (x, x86eax); X86Mov (y, x86edx)] @ process' @ [X86Mov (x86edx, y)]) 

let x86_compile_compare_op : x86environment -> operand list -> (operand -> x86instr) -> (operand list) * (x86instr list) = 
  fun env stack op ->
    match stack with
    | [] | _::[]   -> assert false
    | x::y::stack' -> (y::stack', [X86Mov (y, x86eax); X86Cmp (x86eax, x); X86Mov (Literal 0, x86eax); op x86al; X86Mov(x86eax, y)])

let x86compile : x86environment -> instr list -> x86instr list = fun env code ->
  let rec x86compile' stack code =
    match code with
    | []       -> []
    | i::code' ->
       let (stack', x86code) =
         match i with
         | S_READ   -> ([RegisterIndex 0], [X86Call "read"])
         | S_WRITE  -> ( 
             match stack with 
             | []        -> assert false 
             | x::stack' ->  
               match x with
               | RegisterIndex _ -> ([], [X86Push x; X86Call "write"; X86Pop x])
               | _               -> ([], [X86Call "write"])
         ) 
         | S_PUSH n ->
           let s = allocate env stack in
           (s::stack, [X86Mov (Literal n, s)])
         | S_LD x   ->
           env#local x;
           let s = allocate env stack in (
             match s with
             | RegisterIndex _ -> (s::stack, [X86Mov (VariableName x, s)])
             | _               -> (s::stack, [X86Mov (VariableName x, x86eax); X86Mov (x86eax, s)])
         )
         | S_ST x   -> ( 
           env#local x;
           match stack with
           | []        -> assert false
           | s::stack' -> 
             match s with
             | RegisterIndex _ -> (stack', [X86Mov (s, VariableName x)])
             | _               -> (stack', [X86Mov (s, x86eax); X86Mov (x86eax, VariableName x)])
           )
         | S_ADD    -> x86_compile_binary_op env stack (fun (x, y) -> X86Add (x, y))
         | S_SUB    -> x86_compile_binary_op env stack (fun (x, y) -> X86Sub (x, y))
         | S_MUL    -> x86_compile_binary_op env stack (fun (x, y) -> X86Mul (x, y))
         | S_DIV    -> ( 
             match stack with
             | [] | _::[]   -> assert false
             | y::x::stack' -> (y::stack', [X86Mov (x, x86eax); X86Cld; X86Div y; X86Mov (x86eax, y)])
         )
         | S_MOD    -> (
             match stack with
             | [] | _::[]   -> assert false
             | y::x::stack' -> (y::stack', [X86Mov (x, x86eax); X86Cld; X86Mod y; X86Mov (x86edx, y)])
         )
         | S_LE     -> x86_compile_compare_op env stack (fun x -> X86Setl  x)
         | S_LEQ    -> x86_compile_compare_op env stack (fun x -> X86Setle x)
         | S_GE     -> x86_compile_compare_op env stack (fun x -> X86Setg  x)
         | S_GEQ    -> x86_compile_compare_op env stack (fun x -> X86Setge x)
         | S_AND    -> x86_compile_logical_op env stack (fun (x, y) -> X86And (x, y))
         | S_OR     -> x86_compile_logical_op env stack (fun (x, y) -> X86Or  (x, y))
       in
       x86code @ x86compile' stack' code'
  in
  x86compile' [] code

let genasm statement =
  let env = new x86environment in
  let code = x86compile env @@ StackMachineCompiler.compile_statement statement in
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

let build statement name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (genasm statement);
  close_out outf;
  Sys.command (Printf.sprintf "gcc -m32 -o %s ../runtime/runtime.o %s.s" name name)
