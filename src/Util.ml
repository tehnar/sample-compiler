let bool_to_int b = if b then 1 else 0

let int_to_bool x = if x == 0 then false else true

let unsafe_pop_one x = match x with
| (y::ys) -> (y, ys)
| _       -> failwith "unsafe_pop_one: empty list"

let unsafe_pop_two x = match x with
| (y1::y2::ys) -> (y1, y2, ys)
| _            -> failwith "unsafe_pop_two: length of list is less then 2"

let rec unsafe_pop_many n stack = if n = 0 then ([], stack) else match stack with
| [] -> failwith "unsafe_pop_many: size of stack is less then arg cnt"
| x::xs -> let (y, ys) = unsafe_pop_many (n - 1) xs in (x::y, ys)

let match_empty_list = function
  | [] -> ()
  | _  -> failwith "match_empty_list: empty list expected"

let match_one_arg    = function
  | [x] -> x
  | _   -> failwith "match_one_arg: one-element list expected"

let match_two_args   = function
  | [x; y] -> (x, y)
  | _      -> failwith "match_two_args: two-element list expected"

let match_three_args = function
  | [x; y; z] -> (x, y, z)
  | _         -> failwith "match_three_args: three-element list expected"

