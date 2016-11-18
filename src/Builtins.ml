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

let is_builtin name = List.mem name ["read"; "write"; "strmake"; "strset"; "strget"; "strdup"; "strcat"; "strcmp"; "strlen"; "strsub"]
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
  | _         -> failwith "No builtin found"
