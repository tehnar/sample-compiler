open Data
open X86Compiler 
open StackMachineEmulator

(*
let ( !! )       = (!)
let ( !  ) x     = Var x
let c n     = Const n
let ( +  ) e1 e2 = Add (e1, e2)
let ( -  ) e1 e2 = Add (e1, e2)
let ( *  ) e1 e2 = Mul (e1, e2)
let ( /  ) e1 e2 = Div (e1, e2)
let ( %  ) e1 e2 = Mod (e1, e2)
let ( <  ) e1 e2 = Le  (e1, e2)
let ( <= ) e1 e2 = Leq (e1, e2)
let ( >  ) e1 e2 = Ge  (e1, e2)
let ( >= ) e1 e2 = Geq (e1, e2)
let ( && ) e1 e2 = And (e1, e2)
let ( || ) e1 e2 = Or  (e1, e2)

let skip     = Skip
let (:=) x e = Assign (x, e)
let read x   = Read x
let write x  = Write x
let (|>) l r = Seq (l, r)

let p = 
  read "x" |> 
  read "y" |>
  ("z" := c 3 + ((c (-123) / c 7) - (!"x" * !"x"))) |>
  write (!"z" + !"y") |>
  ("var" := (((((!"x" + !"y") + !"z") + c 123) + c 234) + c 456) % c 239) |>
  write !"var" |>
  write (!"var" < c 97) |>
  write (!"var" < c 98) |>
  write (!"var" < c 99) |>
  write (!"var" <= c 97) |>
  write (((((((!"var" <= c 98) * c 100000) + c 123) + c 456) + c 789) + c 1023) > !"x")  |>
  write (!"var" <= c 99) |>
  ("test1" := c 1) |>
  write ((!"test1" >= c 2) || (c 2 * c 2) > c 3) |>
  write (c 2 && c 1) |>
  write (c 2 && c 0)
(*
let _ =
  let vals = run [3; 4] p in
  List.iter (fun x -> Printf.printf "%d\n" x) vals



let _ = build p "p"
*)
*)
let main =
  try
    let filename = Sys.argv.(1) in
    match Parser.parse filename with
    | `Ok stmt -> ignore @@ build stmt (Filename.chop_suffix filename ".expr")
    | `Fail er -> Printf.eprintf "%s" er
  with Invalid_argument _ ->
    Printf.printf "Usage: rc.byte <name.expr>\n"
