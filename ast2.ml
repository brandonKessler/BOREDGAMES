
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Or | And

type bg_t = Int | Float | Bool | Coord | String | Piece | Matrix

type inc = Plus | Minus
type gmpc = Brd | Plr | Pcs

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

type setup_dec = 
 | Set of gmpc * bg_t
 | Stmt of stmt



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
   rlocals : vardec_t list;
   rbody : stmt list;
 }


type program = setup_dec list * rules_t list * stmt list




