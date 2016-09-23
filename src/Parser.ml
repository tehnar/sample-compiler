open Ostap
open Matcher
open Data 

ostap (
  expr: op5; 

  op5:
    l:op4 suf:("||" r:op4)* {List.fold_left (fun l (op, r) -> Or (l, r)) l suf};

  op4:
    l:op3 suf:("&&" r:op3)* {List.fold_left (fun l (op, r) -> And (l, r)) l suf};

  op3:
    l:op2 suf:(("<="|"<"|">="|">") r:op2)* {List.fold_left (fun l (op, r) -> 
      match op with 
      | ("<", _)  -> Le  (l, r)
      | ("<=", _) -> Leq (l, r)
      | (">", _)  -> Ge  (l, r)
      | (">=", _) -> Geq (l, r)
      | _        -> assert false
    ) l suf};

  op2:
    l:op1 suf:(("+"|"-") r:op1)* {List.fold_left (fun l (op, r) -> 
      match op with 
      | ("+", _) -> Add (l, r)
      | ("-", _) -> Sub (l, r)
      | _        -> assert false
    ) l suf};

  op1:
    l:primary suf:(("*"|"/"|"%") r:primary)* {List.fold_left (fun l (op, r) ->
     match op with
     | ("*", _) -> Mul (l, r)
     | ("/", _) -> Div (l, r)
     | ("%", _) -> Mod (l, r)
     | _        -> assert false
   ) l suf}
  | primary;

  primary:
    c:DECIMAL { Const c }
  | x:IDENT   { Var   x }
  | -"(" expr -")";

  stmt:
    s1:simple ";" s2:stmt       { Seq    (s1, s2) }
  | simple;

  simple:
    %"read"  "(" name:IDENT ")" { Read name       }
  | %"write" "(" e:expr     ")" { Write e         }
  | %"skip"                     { Skip            }
  | x:IDENT ":=" e:expr         { Assign (x , e)  }
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
