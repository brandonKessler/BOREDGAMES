
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Or | And

type bg_t = Int | Float | Bool | Coord | String | Piece | Matrix

type inc = Plus | Minus
type gmpc = Brd | Plr | Pcs
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
type player_t = {
   name : string;
   inventory : piece_t list;
   points : int;
   onBoard : piece_t list;
}

type mat_t = {
   rows : int;
   cols : int;
   values : string list;
 }
type tile_t = {
   pieces : piece_t list;
   pointValue : int;
}
type board_t = {
   rows : int;
   cols : int;
   values : tile_t list;
}



type expr = 
   Lint of int
 | Lfloat of float
 | Lbool of bool
 | Lcoord of coord_t
 | Lstring of string
 | Lpieces of piece_t
 | Lmat of mat_t
 | Id of string
 | Binop of expr * op * expr
 | Through of expr * expr
 | Incr of expr * inc
 | Assign of expr * expr
 | Call of string * expr list
 | Access of expr * expr
 | Daccess of expr * string list
 | Noexpr

type stmt = 
   Block of stmt list
 | Expr of expr
 | Return of expr
 | If of expr * stmt * stmt 
 | Loop of expr * stmt 
 | Decl of bg_t * expr
 | Set of gmpc * bg_t list



type rules_t = {
   rname : string;
   rlocals : stmt list;
   rbody : stmt list;
 }

type play_t = {
   plocals : stmt list;
   pbody : stmt list;
}


type program = {
   svars: stmt list;
   board : mat_t;
   players : string list;
   pieces : piece_t list;
   rsec : rules_t list;
   psec : play_t list;
 }
