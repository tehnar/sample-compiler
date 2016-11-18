open Data

let x86regs = [|"%esp"; "%eax"; "%edx"; "%ebx"; "%ecx"; "%esi"; "%edi";|]
let x86lower_regs = [|""; "%al"; "%dl"; "%bl"; "%cl"|]

let num_of_regs = Array.length x86regs
let num_of_lower_regs = Array.length x86lower_regs

let word_size = 4

type operand = RegisterIndex of int | StackIndex of int | Literal of Value.t | RegisterLowerIndex of int

let x86esp = RegisterIndex 0
let x86eax = RegisterIndex 1
let x86edx = RegisterIndex 2 
let x86ecx = RegisterIndex 4 
let x86al  = RegisterLowerIndex 1
let x86dl  = RegisterLowerIndex 2

let volatile_regs = [x86eax; x86edx; x86ecx]
let non_volatile_regs = [RegisterIndex 3; RegisterIndex 5; RegisterIndex 6]

let allocate env stack =
  match stack with
  | []                                          -> RegisterIndex 3
  | (StackIndex n)::_                           -> let i = (max n env#local_vars_count) + 1 in
                                                   env#allocate i; StackIndex i
  | (RegisterIndex n)::_ when n < num_of_regs-1 -> RegisterIndex (n+1)
  | _                                           -> let i = env#local_vars_count + 1 in
                                                   env#allocate i; StackIndex i
module Map = Map.Make (String)

class x86environment =
  object(self)
    val    args_count       = ref 0
    val    local_vars_count = ref 0
    val    local_vars       = ref Map.empty
    method local x          = if not (Map.mem x !local_vars) then (
                                local_vars_count := !local_vars_count + 1;
                                local_vars := Map.add x !local_vars_count !local_vars;
                                self#allocate !local_vars_count
    )
    method local_arg x      = args_count := !args_count + 1;
                              local_vars := Map.add x (-(!args_count) - 1) !local_vars
    method local_addr x     = StackIndex (Map.find x !local_vars)
    method local_vars_count = !local_vars_count
    (*method local_vars       = Map.elements !local_vars*)

    val    allocated  = ref 0
    method allocate n = allocated := max n !allocated
    method allocated  = !allocated
  end


type setsuffix = SetLe | SetLeq | SetGe | SetGeq | SetEq | SetNeq
type x86instr =
  | X86Add       of operand * operand
  | X86Sub       of operand * operand
  | X86And       of operand * operand
  | X86Or        of operand * operand
  | X86Mul       of operand * operand
  | X86Mov       of operand * operand
  | X86Xor       of operand * operand
  | X86Cmp       of operand * operand
  | X86Test      of operand * operand
  | X86Push      of operand
  | X86Pop       of operand
  | X86Div       of operand
  | X86Mod       of operand
  | X86Set       of setsuffix * operand
  | X86Jz        of string
  | X86Jnz       of string
  | X86Jmp       of string
  | X86Ret
  | X86Cld    
  | X86Call      of string
  | X86Label     of string
  | X86Prologue  of x86environment

let slot : operand -> string = function
  | (RegisterIndex i) -> x86regs.(i)
  | (StackIndex i) -> Printf.sprintf "%d(%%ebp)" (-i * word_size)
  | (Literal i) -> Printf.sprintf "$%s" (Value.convert_to_string i)
  | (RegisterLowerIndex i) -> x86lower_regs.(i)

let suf_to_str suf = 
  match suf with
  | SetLe  -> "l"
  | SetLeq -> "le"
  | SetGe  -> "g"
  | SetGeq -> "ge"
  | SetEq  -> "e"
  | SetNeq -> "ne"


let push_regs stack regs = match stack with 
| [] -> []
| (s::_) -> 
  let push = fun reg -> X86Push reg in
  match s with 
  | StackIndex _    -> List.map push regs
  | RegisterIndex i -> List.map push @@ List.filter (fun reg -> List.mem reg stack) regs
  | _               -> assert false

let pop_regs stack regs = match stack with 
| [] -> []
| (s::_) -> 
  let pop = fun reg -> X86Pop reg in
  match s with 
  | StackIndex _    -> List.rev_map pop regs
  | RegisterIndex i -> List.rev_map pop @@ List.filter (fun reg -> List.mem reg stack) regs
  | _               -> assert false

let rec x86print : x86instr -> string = function instr ->
  let get_prologue env = 
    String.concat "\n" ["\tpushl\t%ebp";
                   "\tmovl\t%esp,\t%ebp";
                   Printf.sprintf "\tsubl\t$%d,\t%%esp" (env#allocated * word_size);
                   String.concat "\n" @@ List.map x86print (push_regs [StackIndex 0] non_volatile_regs)] 
  in match instr with
  | X86Add  (s1, s2)    -> Printf.sprintf "\taddl\t%s,\t%s"  (slot s1) (slot s2)
  | X86Sub  (s1, s2)    -> Printf.sprintf "\tsubl\t%s,\t%s"  (slot s1) (slot s2)
  | X86Mul  (s1, s2)    -> Printf.sprintf "\timull\t%s,\t%s" (slot s1) (slot s2)
  | X86Mov  (s1, s2)    -> Printf.sprintf "\tmovl\t%s,\t%s"  (slot s1) (slot s2)
  | X86Xor  (s1, s2)    -> Printf.sprintf "\txorl\t%s,\t%s"  (slot s1) (slot s2)
  | X86And  (s1, s2)    -> Printf.sprintf "\tandl\t%s,\t%s"  (slot s1) (slot s2)
  | X86Or   (s1, s2)    -> Printf.sprintf "\torl\t%s,\t%s"   (slot s1) (slot s2)
  | X86Test (s1, s2)    -> Printf.sprintf "\ttest\t%s,\t%s"  (slot s1) (slot s2)
  | X86Push  s          -> Printf.sprintf "\tpushl\t%s"      (slot s )
  | X86Pop   s          -> Printf.sprintf "\tpopl\t%s"       (slot s )
  | X86Div   s          -> Printf.sprintf "\tidivl\t%s"      (slot s )
  | X86Mod   s          -> Printf.sprintf "\tidivl\t%s"      (slot s )
  | X86Cmp  (s1, s2)    -> Printf.sprintf "\tcmpl\t%s,\t%s"  (slot s2) (slot s1) (*Not an error, that's GAS syntax: cmp arg2, arg1 *)
  | X86Set  (suf, s)    -> Printf.sprintf "\tset%s\t%s"      (suf_to_str suf) (slot s)
  | X86Jz    s          -> Printf.sprintf "\tjz\t%s"          s
  | X86Jnz   s          -> Printf.sprintf "\tjnz\t%s"         s
  | X86Jmp   s          -> Printf.sprintf "\tjmp\t%s"         s
  | X86Ret              -> Printf.sprintf "\tmovl\t%%ebp,\t%%esp\n\tpopl\t%%ebp\n\tret"
  | X86Call  s          -> Printf.sprintf "\tcall\t%s"        s
  | X86Label s          -> Printf.sprintf "%s:"               s
  | X86Cld              -> Printf.sprintf "\tcdq"
  | X86Prologue env     -> get_prologue env



let func_epilogue_label func_name = Printf.sprintf "__func_%s_end" func_name

let binary_arithm_op_to_asm = fun op -> 
  match op with
  | Add -> (fun (l, r) -> X86Add (l, r))
  | Sub -> (fun (l, r) -> X86Sub (l, r))
  | Mul -> (fun (l, r) -> X86Mul (l, r))
  | _   -> assert false

let binary_compare_op_to_asm = fun op -> 
  match op with
  | Le  -> (fun l -> X86Set (SetLe,  l))
  | Leq -> (fun l -> X86Set (SetLeq, l))
  | Ge  -> (fun l -> X86Set (SetGe,  l))
  | Geq -> (fun l -> X86Set (SetGeq, l))
  | Eq  -> (fun l -> X86Set (SetEq,  l))
  | Neq -> (fun l -> X86Set (SetNeq, l))

let binary_logical_op_to_asm = fun op -> 
  match op with
  | Or  -> (fun (l, r) -> X86Or  (l, r))
  | And -> (fun (l, r) -> X86And (l, r))

let conditional_jmp_to_asm = fun op label -> 
  match op with
  | Jz  -> X86Jz  label
  | Jnz -> X86Jnz label

let x86_compile_binary_arithm_op : x86environment -> operand list -> (operand * operand -> x86instr) -> (operand list) * (x86instr list) = 
  fun env stack op -> 
    let (x, y, stack') = Util.unsafe_pop_two stack in  
    match (x, y) with
    | (_, RegisterIndex _) -> (y::stack', [op (x, y)])
    | (_, _)               -> (y::stack', [X86Mov (x, x86eax); 
                                           X86Mov (y, x86edx); 
                                           op (x86eax, x86edx); 
                                           X86Mov (x86edx, y)])

let x86_compile_binary_logical_op : x86environment -> operand list -> (operand * operand -> x86instr) -> (operand list) * (x86instr list) = 
  fun env stack op ->
    let (x, y, stack') = Util.unsafe_pop_two stack in 
    let process x x' = [X86Or (Literal (Value.Int 0), x); 
                        X86Mov (Literal (Value.Int 0), x); 
                        X86Set (SetNeq, x')] in
    let process'     = (process x86eax x86al) @ 
                       (process x86edx x86dl) @ 
                       [op (x86eax, x86edx)] in
        (y::stack', [X86Mov (x, x86eax); 
                     X86Mov (y, x86edx)] @ 
                     process' @ 
                     [X86Mov (x86edx, y)]) 

let x86_compile_binary_compare_op : x86environment -> operand list -> (operand -> x86instr) -> (operand list) * (x86instr list) = 
  fun env stack op ->
    let (x, y, stack') = Util.unsafe_pop_two stack in 
    (y::stack', [X86Mov (y, x86eax); 
                 X86Cmp (x86eax, x); 
                 X86Mov (Literal (Value.Int 0), x86eax); 
                 op x86al; 
                 X86Mov(x86eax, y)])

let x86_compile_conditional_jmp: x86environment -> operand list -> x86instr -> (operand list) * (x86instr list) = 
  fun env stack op ->
    match stack with
    | []        -> assert false
    | (RegisterIndex x)::stack' -> (stack', [X86Test (RegisterIndex x, RegisterIndex x); op])
    | x::stack'                 -> (stack', [X86Mov (x, x86eax); X86Test(x86eax, x86eax); op])

let x86_compile_call: x86environment -> operand list -> string -> int -> (operand list) * (x86instr list) =
  fun env stack label arg_cnt -> 
    let (args, stack') = Util.unsafe_pop_many arg_cnt stack in
    let pushes         = List.map (fun op -> X86Push op) args in
    let s              = allocate env stack' in
    (s::stack', push_regs stack' volatile_regs @ 
               pushes @
               [X86Call label] @
               [X86Add (Literal (Value.Int (word_size * arg_cnt)), x86esp);
                X86Mov (x86eax, s)] @
               pop_regs stack' volatile_regs)

let x86compile : string -> x86environment -> instr list -> x86instr list = fun func_name env code ->
  let rec x86compile' stack code =
    match code with
    | []       -> []
    | i::code' ->
       let (stack', x86code) =
         match i with
         | S_FUNC_BEGIN args -> List.iter (fun arg -> env#local_arg arg) (List.rev args); ([], [X86Prologue env])

         | S_PUSH n ->
           let s = allocate env stack in
           (s::stack, [X86Mov (Literal n, s)])

         | S_LD x   ->
           env#local x;
           let s = allocate env stack in ( 
           match s with 
           | RegisterIndex _ -> (s::stack, [X86Mov (env#local_addr x, s)])
           | _               -> (s::stack, [X86Mov (env#local_addr x, x86eax); X86Mov (x86eax, s)])           
           )

         | S_ST x   ->  
           env#local x;
           let (s, stack') = Util.unsafe_pop_one stack in (
           match s with
           | RegisterIndex _ -> (stack', [X86Mov (s, env#local_addr x)])
           | _               -> (stack', [X86Mov (s, x86eax); X86Mov (x86eax, env#local_addr x)])
           )


         | S_BINARY_ARITHM_OP Div -> 
           let (y, x, stack') = Util.unsafe_pop_two stack in 
           (y::stack', [X86Mov (x, x86eax); X86Cld; X86Div y; X86Mov (x86eax, y)])

         | S_BINARY_ARITHM_OP Mod ->
           let (y, x, stack') = Util.unsafe_pop_two stack in
           (y::stack', [X86Mov (x, x86eax); X86Cld; X86Mod y; X86Mov (x86edx, y)])

         | S_BINARY_ARITHM_OP  op -> x86_compile_binary_arithm_op  env stack (binary_arithm_op_to_asm op)

         | S_BINARY_LOGICAL_OP op -> x86_compile_binary_logical_op env stack (binary_logical_op_to_asm op)

         | S_BINARY_COMPARE_OP op -> x86_compile_binary_compare_op env stack (binary_compare_op_to_asm op)

         | S_CONDITIONAL_JMP (op, label) -> x86_compile_conditional_jmp env stack (conditional_jmp_to_asm op label)

         | S_CALL (label, arg_cnt) -> x86_compile_call env stack label arg_cnt

         | S_BUILTIN (builtin_name, arg_cnt) -> x86_compile_call env stack builtin_name arg_cnt

         | S_JMP   label           -> (stack, [X86Jmp label])

         | S_LABEL label           -> (stack, [X86Label label])

         | S_RET                   -> let (x, stack') = Util.unsafe_pop_one stack in
                                      (stack', [X86Mov (x, x86eax); X86Jmp (func_epilogue_label func_name)])
         | S_DROP                  -> let (x, stack') = Util.unsafe_pop_one stack in
                                      (stack', [])
         | S_END                   -> (stack, []) (* TODO *)

         | S_FUNC_END              -> (stack, []) 
       in
       x86code @ x86compile' stack' code'
  in
  x86compile' [] code

let rec split_to_funcs_and_main stmt = match stmt with
| FunctionDef (name, _, _) -> ([(name, stmt)], [])
| Seq (l, r)    -> 
    let (funcs',  main')  = split_to_funcs_and_main l in
    let (funcs'', main'') = split_to_funcs_and_main r in
    (funcs' @ funcs'', main' @ main'')
| _             -> ([], [stmt])

let genasm func func_name =
  let env = new x86environment in
  let code = x86compile func_name env @@ StackMachineCompiler.compile_code func in
  let asm = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in

  let epilogue =
      (fun () ->
         !(x86print (X86Label (func_epilogue_label func_name)));
         List.iter (fun i -> !(x86print i)) @@ pop_regs [StackIndex 0] non_volatile_regs;
         !"\tmovl\t%ebp,\t%esp";
         !"\tpopl\t%ebp";
         if func_name = "main" then !"\txorl\t%eax,\t%eax";
         !"\tret"
      )
  in
  if func_name = "main" then (
    !"main:"; 
    !(x86print @@ X86Prologue env);
  );
  List.iter (fun i -> !(x86print i)) code;

  epilogue();

  Buffer.contents asm

let build code name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  let funcs, main = split_to_funcs_and_main code in
  let seq_main = List.fold_right (fun l r -> Seq (l, r)) main Skip in
  let compiled_funcs = List.map (fun (func_name, func) -> genasm func func_name) funcs in
  let compiled_main = genasm seq_main "main" in
  Printf.fprintf outf "\t.globl\tmain\n";
  List.iter (fun asm -> Printf.fprintf outf "%s\n" asm) compiled_funcs;
  Printf.fprintf outf "main:\n%s" compiled_main;
  close_out outf;
  let runtime_dir = try
    Sys.getenv "RC_RUNTIME"
    with Not_found -> "../runtime"
  in  
  ignore (Sys.command (Printf.sprintf "gcc -m32 -o %s %s/runtime.o %s.s" name runtime_dir name))
