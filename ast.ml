

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Or | And

type bg_t = Int | Float | Bool | Coord | String | Piece | Matrix

type inc = Plus | Minus
 
type coord_t = {
   xc : int;
   yc : int;
}

type player_t = {
   plrname : string;
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


type expr = 
   Lint of int
 | Lfloat of float
 | Lbool of bool
 | Lstring of string
 | Lpieces of piece_t
 | Lmat of mat_t
 | Id of string
 | Binop of expr * op * expr
 | Through of expr * expr
 | Incr of expr * inc
 | Assign of string * expr
 | Call of string * expr list
 | Access of expr * expr
 | Baccess of expr * coord_t
 | Daccess of expr * expr
 | Noexpr

type stmt = 
   Block of stmt list
 | Expr of expr
 | Return of expr
 | If of expr * stmt * stmt 
 | Loop of expr * stmt 
 | Decl of bg_t * expr

type setup_dec = 
   Setbd of mat_t
 | Setpc of piece_t
 | Setplr of player_t
 | Stmt of stmt

type rules_t = {
   rname : string;
   rbody : stmt list;
 }

type program = setup_dec list * rules_t list * stmt list


