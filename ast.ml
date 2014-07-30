
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr = 
   Literal of int
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
   rlocals : (string,string) list;
   rbody : string list;
 }

type game_t = {
   players : string list;
   pieces : piece_t list;
   board : board_t;
   rules : rules_t list;
 }

type program = (string,string) list * game_t list

