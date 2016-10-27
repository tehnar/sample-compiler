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
    c:DECIMAL      { Const c }
  | x:functionCall { x }
  | x:IDENT        { Var   x }
  | -"(" expr -")";

  stmt:
    s1:functionDef s2:stmt         { Seq (s1, s2) }
  | s1:simpleStatement ";" s2:stmt { Seq (s1, s2) }
  | s1:simpleStatement             { s1 };

  simpleStatement: 
    %"if" e:expr %"then" s1:stmt elifs:(%"elif" expr %"then" stmt)* els:(%"else" stmt)? %"fi" {
      If (e, s1, 
        List.fold_right (fun (e, s) r -> If (e, s, r)) 
        elifs 
        (match els with None -> Skip | Some x -> x)
      )
    }
  | %"repeat" s:stmt %"until" e:expr {Seq (s, While (BinaryCompareExpr (Eq, e, Const 0), s))}
  | %"while" e:expr %"do" s:stmt %"od" {While (e, s)}
  | %"for" s1:stmt "," e:expr "," s2:stmt %"do" s:stmt %"od" {Seq(s1, While (e, Seq(s, s2)))}
  | %"read"  "(" name:IDENT ")" { Read name       }
  | %"write" "(" e:expr     ")" { Write e         }
  | %"skip"                     { Skip            }
  | %"return" e:expr            { Return e        }
  | x:IDENT ":=" e:expr         { Assign (x , e)  }
  | s:functionDef        { s }
  | s:functionCall { match s with FunctionCallExpr (name, args) -> FunctionCallStatement (name, args) | _ -> assert false }
  | "" {Skip};

  functionDef:
    funName:IDENT "(" firstArg:IDENT args:(-"," IDENT)* ")" %"begin" body:stmt %"end" {FunctionDef (funName, firstArg::args, body)}
  | funName:IDENT "(" ")" %"begin" body:stmt %"end"                               {FunctionDef (funName, [], body)};

  functionCall:
    funName:IDENT "(" firstArg:expr args:(-"," expr)* ")" {FunctionCallExpr (funName, firstArg::args)}
  | funName:IDENT "(" ")"                                 {FunctionCallExpr (funName, [])}
)

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident ["read"; "write"; "skip"] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip. nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (stmt -EOF))
