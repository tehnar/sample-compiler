let main = ()
  try
    let mode, filename =
      match Sys.argv.(1) with
      | "-s" -> `SM , Sys.argv.(2)
      | "-o" -> `X86, Sys.argv.(2)
      | "-i" -> `Int, Sys.argv.(2)
      | _    -> `Int, Sys.argv.(1)
    in
    match Parser.parse filename with
    | `Ok stmt -> 
      (match mode with
       | `X86 ->
         let basename = Filename.chop_suffix filename ".expr" in 
         X86Compiler.build stmt basename
       | _ ->
         match mode with
         | `SM -> StackMachineEmulator.run (StackMachineCompiler.compile_code stmt)
         | _   -> Emulator.run stmt
      )
    | `Fail er -> Printf.eprintf "%s" er
  with
  | Invalid_argument _ -> Printf.printf "Usage: rc.byte <name.expr>" 
