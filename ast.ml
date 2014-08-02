
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr = 
   Lint of int
 | Ldouble of float
 | Lbool of bool
 | Lcoord of coord_t
 | Lstring of string
 | Id of string
 | Binop of expr * op * expr
 | Assign of string * expr
 | Call of string * expr list
 | Access of string * expr list
 | Noexpr

type stmt = 
   Block of stmt list
 | Expr of expr
 | Return of expr
 | If of expr * stmt * stmt
 | Loop of expr * stmt

type coord_t = {
   xc : int;
   yc : int;
 }

type piece_t = { 
   owner : string;
   name : string;
   num : int;
   ptval : int;
   cloc : coord_t;
 }

type board_t = {
   rows : int;
   cols : int;
 }

type rules_t = {
   rname : string;
   rlocals : (string * string) list;
   rbody : string list;
 }

type play_t = {
   plocals : (string * string) list;
   pbody : string list;

type program = {
   svars: (string * string) list;
   board : board_t;
   players : string list;
   pieces : piece_t list;
   rsec : rules_t list;
   psec : (string * string) list;
 }
