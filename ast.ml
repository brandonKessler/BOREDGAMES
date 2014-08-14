

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Or | And

type bg_t = Int | Float | Bool | Coord | String | Piece | Matrix | Player | Tile
| Rule

type inc = Plus | Minus
 


type player_t = {
   plrname : string;
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
 | Id of string
 | Binop of expr * op * expr
 | Cat of expr * expr
 | Through of expr * expr
 | Incr of expr * inc
 | Assign of string * expr
 | Call of expr * expr list
 | Access of expr * expr
 | Baccess of expr * coord_t
 | Daccess of expr * expr
 | Noexpr
and coord_t = {
   xc : expr;
   yc : expr;
}
type piece_t = { 
   owner : string;
   name : string;
   num : int;
   ptval : int;
   cloc : coord_t;
}
type stmt = 
   Block of stmt list
 | Expr of expr
 | Return of expr
 | If of expr * stmt * stmt 
 | Loop of expr * stmt 
 | Decl of bg_t * expr
 | NextPlayer
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


let rec string_of_expr = function
   Lint(l) -> string_of_int l
 | Lfloat(f) -> string_of_float f
 | Lbool(b) -> string_of_bool b
 | Lstring(st) -> st
 | Id(s) -> s
 | Binop(e1, o, e2) ->
	string_of_expr e1 ^ " " ^
	(match o with
	  Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
	| Equal -> "==" | Neq -> "!="
        | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
	| Or -> "||" | And -> "&&") ^ " " ^
      string_of_expr e2
 | Through(e1,e2) ->
	string_of_expr e1 ^ ":" ^ string_of_expr e2
 | Incr(e,i) -> 
	string_of_expr e ^ 
	(match i with
	  Plus -> "++" | Minus -> "--") 
 | Assign(v,e) -> v ^ " = " ^ string_of_expr e
 | Call(f,el) ->
	string_of_expr f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
 | Access(e1,e2) ->
	string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
 | Baccess(e,c) -> 
	string_of_expr e ^ "[(" ^ string_of_expr c.xc ^ "," 
	^ string_of_expr c.yc ^ ")]"
 | Daccess(e1,e2) ->
	string_of_expr e1 ^ "." ^ string_of_expr e2
 | Noexpr -> ""


let rec string_of_stmt = function
   Block(stmts) ->
	"{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
 | Expr(expr) -> string_of_expr expr ^ ";\n";
 | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
 | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
 | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
 | Loop(e, s) -> "loop (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
 | Decl(bg,e) ->
	(match bg with
	  Int -> "int" | Float -> "float" | Bool -> "bool" 
	| Coord -> "coord" | String -> "string"
	| Piece -> "piece" | Matrix -> "matrix" | Player -> "player" | Tile ->
                        "tile" | Rule -> "rule") ^ " " ^
	string_of_expr e
 | NextPlayer -> "NextPlayer"
let string_of_setup = function
   Setbd(m) -> "new Board(" ^ string_of_int m.rows ^ "," ^ string_of_int m.cols ^ ")\n"
  | Setpc(pc) -> "new Pieces(" ^ pc.owner ^ ", " ^ pc.name ^ ", " ^ 
	string_of_int pc.num ^ ", (" ^ string_of_expr pc.cloc.xc ^ "," ^
	string_of_expr pc.cloc.yc ^ "))\n" 
 | Setplr(plr) -> "new Player(" ^ plr.plrname ^ ")\n"
 | Stmt(s) -> string_of_stmt s ^ "\n"

let string_of_rules r = "rule " ^ r.rname ^ ": " ^ String.concat "" 
	(List.map string_of_stmt r.rbody) ^ "\n"

let string_of_program (su,r,st) = 
   "Setup {\n" ^ String.concat "" (List.map string_of_setup su) ^ "}\n" ^ 
   "Rules {\n" ^ String.concat "" (List.map string_of_rules r) ^ "}\n" ^
   "Play {\n" ^ String.concat "\n" (List.map string_of_stmt st) ^ "}\n"

