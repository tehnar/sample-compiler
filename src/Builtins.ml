open Data

let read x = Printf.printf "> "; Value.Int (read_int @@ Util.match_empty_list x)

let write x = Printf.printf "%s\n" @@ Value.convert_to_string @@ Util.match_one_arg x; Value.Int 0

let strmake x = 
  let (n, c) = Util.match_two_args x in
  Value.String (Bytes.make (Value.to_int n) (Char.chr @@ Value.to_int c))

let strset x = 
  let (s, i, c) = Util.match_three_args x in
  Bytes.set (Value.to_bytes s) (Value.to_int i) (Char.chr @@ Value.to_int c); s

let strget x = 
  let (s, i) = Util.match_two_args x in
  Value.Int (Char.code @@ Bytes.get (Value.to_bytes s) (Value.to_int i))

let strdup x = Value.String (Bytes.copy @@ Value.to_bytes @@ Util.match_one_arg x)

let strcat x = 
  let (s, t) = Util.match_two_args x in
  Value.String (Bytes.cat (Value.to_bytes s) (Value.to_bytes t))

let strcmp x = 
  let (s, t) = Util.match_two_args x in
  Value.Int (Bytes.compare (Value.to_bytes s) (Value.to_bytes t))

let strlen x = Value.Int (Bytes.length @@ Value.to_bytes @@ Util.match_one_arg x)

let strsub x = 
  let (s, start, len) = Util.match_three_args x in
  Value.String (Bytes.sub (Value.to_bytes s) (Value.to_int start) (Value.to_int len))

let arrlen x = Value.Int (Array.length @@ Value.to_array @@ Util.match_one_arg x)

let arrmake_unboxed x = 
  let (n, v)  = Util.match_two_args x in
  Value.of_array false @@ Array.make (Value.to_int n) v
  
let arrmake_boxed x = 
  let (n, v)  = Util.match_two_args x in
  Value.of_array true @@ Array.make (Value.to_int n) v

let thread_join x = 
  let t = Util.match_one_arg x in 
  Thread.join (Value.to_thread t);
  Value.Int 0
  
let thread_sleep x = 
  let delay = Util.match_one_arg x in
  Thread.delay @@ (float_of_int @@ Value.to_int delay) /. 1000.0;
  Value.Int 0

let is_builtin name = List.mem name ["read"; "write"; "strmake"; "strset"; "strget"; "strdup"; "strcat"; "strcmp"; "strlen"; "strsub"; "arrlen"; "arrmake"; "Arrmake";
                                     "thread_create"; "thread_join"; "thread_sleep"]
let get_builtin : string -> (Value.t list -> Value.t) = function 
  | "read"    -> read
  | "write"   -> write
  | "strmake" -> strmake
  | "strset"  -> strset
  | "strget"  -> strget
  | "strdup"  -> strdup
  | "strcat"  -> strcat
  | "strcmp"  -> strcmp
  | "strlen"  -> strlen
  | "strsub"  -> strsub
  | "arrlen"  -> arrlen
  | "arrmake" -> arrmake_unboxed
  | "Arrmake" -> arrmake_boxed
  | "thread_create" -> failwith "thread_create is a special builtin that should be treated by interpreter"
  | "thread_join"   -> thread_join
  | "thread_sleep"  -> thread_sleep
  | _         -> failwith "No builtin found"
