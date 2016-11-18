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
    c:DECIMAL  { Const (Value.Int c) }
  | s:STRING   { Const (Value.of_string (String.sub s 1 (String.length s - 2))) }
  | c:CHAR     { Const (Value.Int (Char.code c)) }
  | x:function_call { x }
  | x:IDENT         { Var   x }
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
  | %"skip"                     { Skip            }
  | %"return" e:expr            { Return e        }
  | x:IDENT ":=" e:expr         { Assign (x , e)  }
  | s:function_def        { s }
  | s:function_call { match s with FunctionCallExpr (name, args) -> FunctionCallStatement (name, args) | _ -> assert false }
  | "" {Skip};

  function_def:
    fun_name:IDENT "(" first_arg:IDENT args:(-"," IDENT)* ")" %"begin" body:stmt %"end" {FunctionDef (fun_name, first_arg::args, body)}
  | fun_name:IDENT "(" ")" %"begin" body:stmt %"end"                               {FunctionDef (fun_name, [], body)};

  function_call:
    fun_name:IDENT "(" first_arg:expr args:(-"," expr)* ")" {FunctionCallExpr (fun_name, first_arg::args)}
  | fun_name:IDENT "(" ")"                                 {FunctionCallExpr (fun_name, [])}
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
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip. nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (stmt -EOF))
