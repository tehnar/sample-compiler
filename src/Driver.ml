open Ostap

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
    (ostap (!(Language.Stmt.parse) -EOF))

let main = ()
  try
    let mode, filename =
      match Sys.argv.(1) with
      | "-s" -> `SM , Sys.argv.(2)
      | "-o" -> `X86, Sys.argv.(2)
      | _    -> `Int, Sys.argv.(1)
    in
    match parse filename with
    | `Ok stmt -> 
	let rec read acc =
	  try
	    let r = read_int () in
	    Printf.printf "> ";
	    read (acc @ [r]) 
          with End_of_file -> acc
	in
	let input = read [] in
	(match mode with
	 | `X86 -> failwith "native not supported"
	 | `SM  ->
	     let output = 
	       StackMachine.Interpreter.run input (StackMachine.Compile.stmt stmt) 
	     in
	     List.iter (fun i -> Printf.printf "%d\n" i) output
	 | `Int -> 
	     let output = Interpreter.Stmt.eval input stmt in
	     List.iter (fun i -> Printf.printf "%d\n" i) output
	)

    | `Fail er -> Printf.eprintf "%s" er
  with 
  | Invalid_argument _ -> Printf.printf "Usage: rc.byte <name.expr>"
