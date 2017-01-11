open Ostap
open Matcher
open Data 

ostap (
  expr: op5; 

  op5:
    l:op4 suf:("!!" r:op4)* {List.fold_left (fun l (op, r) -> BinaryLogicalExpr (Or, l, r)) l suf};

  op4:
    l:op3 suf:("&&" r:op3)* {List.fold_left (fun l (op, r) -> BinaryLogicalExpr (And, l, r)) l suf};

  op3:
    l:op2 suf:(("=="|"!="|"<="|"<"|">="|">") r:op2)* {List.fold_left (fun l (op, r) -> 
      match op with 
      | ("==", _) -> BinaryCompareExpr (Eq,  l, r)
      | ("!=", _) -> BinaryCompareExpr (Neq, l, r)
      | ("<", _)  -> BinaryCompareExpr (Le,  l, r)
      | ("<=", _) -> BinaryCompareExpr (Leq, l, r)
      | (">", _)  -> BinaryCompareExpr (Ge,  l, r)
      | (">=", _) -> BinaryCompareExpr (Geq, l, r)
      | _        -> assert false
    ) l suf};

  op2:
    l:op1 suf:(("+"|"-") r:op1)* {List.fold_left (fun l (op, r) -> 
      match op with 
      | ("+", _) -> BinaryArithmExpr (Add, l, r)
      | ("-", _) -> BinaryArithmExpr (Sub, l, r)
      | _        -> assert false
    ) l suf};

  op1:
    l:primary suf:(("*"|"/"|"%") r:primary)* {List.fold_left (fun l (op, r) ->
     match op with
     | ("*", _) -> BinaryArithmExpr (Mul, l, r)
     | ("/", _) -> BinaryArithmExpr (Div, l, r)
     | ("%", _) -> BinaryArithmExpr (Mod, l, r)
     | _        -> assert false
   ) l suf}
  | primary;

  primary:
    "[" elems:args_expr_list "]" { Array (false, elems) }
  | "{" elems:args_expr_list "}" { Array (true,  elems) }
  | e:array_expr i:(-"[" expr -"]")+  { List.fold_left (fun a i -> Elem (a, i)) e i }
  | c:DECIMAL                    { Const (Value.Int c) }
  | s:STRING                     { Const (Value.of_string (String.sub s 1 (String.length s - 2))) }
  | c:CHAR                       { Const (Value.Int (Char.code c)) }
  | "true"                       { Const (Value.Int 1) }
  | "false"                      { Const (Value.Int 0) }
  | x:function_call              { x }
  | x:function_ref_call          { x }
  | x:IDENT                      { Var x }
  | "&" x:IDENT                  { FuncRefName x}
  | -"(" expr -")";

  stmt:
    s1:function_def s2:stmt         { Seq (s1, s2) }
  | s1:simple_statement ";" s2:stmt { Seq (s1, s2) }
  | s1:simple_statement             { s1 };

  simple_statement: 
    %"if" e:expr %"then" s1:stmt elifs:(%"elif" expr %"then" stmt)* els:(%"else" stmt)? %"fi" {
      If (e, s1, 
        List.fold_right (fun (e, s) r -> If (e, s, r)) 
        elifs 
        (match els with None -> Skip | Some x -> x)
      )
    }
  | %"repeat" s:stmt %"until" e:expr {Seq (s, While (BinaryCompareExpr (Eq, e, Const (Value.Int 0)), s))}
  | %"while" e:expr %"do" s:stmt %"od" {While (e, s)}
  | %"for" s1:stmt "," e:expr "," s2:stmt %"do" s:stmt %"od" {Seq(s1, While (e, Seq(s, s2)))}
  | %"skip"                               { Skip            }
  | %"return" e:expr                      { Return e        }
  | a:array_expr i:(-"[" expr -"]")+ ":=" e:expr { 
    let rev_i  = List.rev i in
    let last_i = List.hd rev_i in
    let i'     = List.rev @@ List.tl rev_i in
    let a'     = List.fold_left (fun a i -> Elem (a, i)) a i' in
    ArrAssign (a', last_i, e) 
  } 
  | x:IDENT ":=" e:expr                   { Assign (x , e)  }
  | s:function_def        { s }
  | s:function_call { match s with FunctionCallExpr (name, args) -> FunctionCallStatement (name, args) | _ -> assert false }
  | s:function_ref_call { match s with FunctionRefCallExpr (f, args) -> FunctionRefCallStatement (f, args) | _ -> assert false }
  | "" {Skip};

  array_expr: 
    function_call
  | x:IDENT        {Var x};
  
  function_def:
    "fun"? fun_name:IDENT "(" args:args_name_list ")" %"begin" body:stmt %"end" {FunctionDef (fun_name, args, body)};

  function_call:
    fun_name:IDENT "(" args:args_expr_list ")" {FunctionCallExpr (fun_name, args)};

  function_ref_call:
    "(" "*" f:expr ")" args:args_expr_list { FunctionRefCallExpr (f, args) };

  args_expr_list:
    first_arg:expr args:(-"," expr)* { first_arg::args }
  | ""                               { [] };
 
  args_name_list:
    first_arg:IDENT args:(-"," IDENT)* { first_arg::args }
  | ""                                 { [] }
)

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident ["skip"] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.string s
       inherit Util.Lexers.char s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--"
       ] s
     end
    )
    (ostap (stmt -EOF))
