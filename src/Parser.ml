open Ostap
open Matcher
open Expr

ostap (
    expr: addi;

    addi:
      l:multi  suf:("+" r:multi)*  { List.fold_left (fun l (op, r) -> Add (l, r)) l suf };

    multi:
      l:primary "*" r:multi { Mul (l, r) }
    | primary;

    primary:
      c:DECIMAL { Const c }
    | x:IDENT   { Var   x }
    | -"(" expr -")"
)

ostap (
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
