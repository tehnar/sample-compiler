type expr =
  | Const of int
  | Var   of string
  | Add   of expr * expr
  | Mul   of expr * expr
  | Sub   of expr * expr
  | Mod   of expr * expr
  | Div   of expr * expr
  | Le    of expr * expr
  | Leq   of expr * expr
  | Ge    of expr * expr
  | Geq   of expr * expr
  | And   of expr * expr
  | Or    of expr * expr

type statement =
  | Skip
  | Read   of string
  | Write  of expr
  | Assign of string * expr
  | Seq    of statement * statement


type instr =
  | S_READ
  | S_WRITE
  | S_PUSH  of int
  | S_LD    of string
  | S_ST    of string
  | S_ADD
  | S_MUL
  | S_SUB
  | S_DIV
  | S_MOD
  | S_LE
  | S_LEQ
  | S_GE
  | S_GEQ
  | S_AND
  | S_OR
