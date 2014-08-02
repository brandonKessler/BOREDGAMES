
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Or | And

type bg_t = Int | Float | Bool | Coord | String | Pieces | Mat


type expr = 
   Lint of int
 | Lfloat of float
 | Lbool of bool
 | Lcoord of coord_t
 | Lstring of string
 | Lpieces of pieces_t
 | Lmat of mat_t
 | Id of string
 | Binop of expr * op * expr
 | Through of expr * expr
 | Assign of string * expr
 | Call of string * expr list
 | Access of expr * expr
 | Daccess of expr * Dot * expr
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

type mat_t = {
   rows : int;
   cols : int;
 }

type rules_t = {
   rname : string;
   rlocals : (bg_t * string) list;
   rbody : stmt list;
 }

type play_t = {
   plocals : (bg_t * string) list;
   pbody : stmt list;

type program = {
   svars: (bg_t * string) list;
   board : mat_t;
   players : string list;
   pieces : piece_t list;
   rsec : rules_t list;
   psec : play_t list;
 }
