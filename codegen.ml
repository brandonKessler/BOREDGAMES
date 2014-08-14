open Sast
open Ast
open Printf

let err str = raise(Failure(“Compile: “ ^ str));;

and let rec Jexpr = function
   SLint(l,d) -> string_of_int l
 | SLfloat(f,d) -> string_of_float f
 | SLbool(b,d) -> string_of_bool b
 | SLstring(st,d) -> st
 | SId(s,scope, d) -> s
 | SBinop(e1, o, e2, d) ->
	(match d with
	Piece -> Jexpr e1 ^ ".equals(" ^ Jexpr e2 ^ ")"
	| Coord -> Jexpr e1 ^ ".equals(" ^ Jexpr e2 ^ ")"
	| String -> Jexpr e1 ^ ".equals(" ^ Jexpr e2 ^ ")" 
	| _ -> Jexpr e1 ^ " " ^
		(match o with
		  Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
		| Equal -> "==" | Neq -> "!="
        	| Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
		| Or -> "||" | And -> "&&") ^ " " ^
      		Jexpr e2)
 | SThrough(e1,e2,d) -> 
	(match e1 with
	Lint(l) -> “for (int i=string_of_int e1; i<string_of_int e2; i++) {\n”

 | SIncr(e,i,d) -> 
	Jexpr e ^ 
	(match i with
	  Plus -> "++" | Minus -> "--" | _ -> err “Invalid Increment/Decrement”) 

 | SAssign(v,e,scope,d) ->  v ^ " = " ^ Jexpr e )

 | SCall(func,args,scope,d) ->
	(match Id(func) with
	“sizeOf” -> SizeAccess args
	| “move” -> (match args with
		[pc; c] -> “PC = “ ^ Jexpr pc^ “;\nPC.loc.x = “ ^ string_of_int c.xc ^”;\nPC.loc.y = “ ^ string_of_int c.yc ^ “;\n”
		| _ -> err “Invalid Move Arguments” )
	| “add” -> (match args with 
		[pc; c] -> “PC = “ ^ Jexpr pc^ “;\nPC.loc.x = “ ^ string_of_int c.xc ^”;\nPC.loc.y = “ ^ string_of_int c.yc ^ “;\n”)
	)

 | SDaccess(e1,e2,d) ->
	(match e1 with
	SBaccess(expr,coord,d) -> BoardAccess string_of_int coord.xc string_of_int coord.yc e2
	| SId(keyword,d) -> (match keyword with
		“Player” -> PlayerDot e2
		| _ -> err “Invalid Left Dot Access” )
	| -> err Invalid Left Dot Access” )
		
 | SNoexpr -> ""


and let SizeAccess args = 
	(match args with
	SId(keyword,d) -> (match keyword with
		“Board” -> “BD_SIZE”
		| _ -> err “Invalid sizeOf Argument”)
	| SCall(func, args,scope,d) -> SizeAccessCall func
	| SDaccess(e1,e2,d) -> (match Id(e1) with 
		“Player” -> (match e2 with
			SCall(func,args,scope,d) -> SizeAccessCall func
			| _ -> err “Invalid sizeOf Player Function Argument” )
		| _ -> err “Invalid sizeOf Function Argument”
	| _ -> err “Invalid sizeOf Argument” )

and let SizeAccessCall func =
	(match SId(func,_) with 
	“inventory” -> “Crd_SearchCt(PCS,0,0,Players.get(curPlayer))”
	| “onBoard” -> “Crd_SearchCt_Gt(PCS,0,0,Players.get(curPlayer))”
	| _ -> err “Invalid Size Function Argument”)

and let PlayerDot e2 = 
	(match e2 with 
	SCall(func,args,scope,d) -> (match Id(func) with
		“name” -> “Player.get(curPlayer);”
		| “inventory” -> PlayerDotInvFunc args 
		| “onBoard” -> PlayerDotBdFunc args
		| _ -> err “Invalid Player Function Access” )
	| SAccess(keyword, pos,d) -> (match Id(keyword) with
		“inventory” -> “PCS.get( Crd_Plr_Pos(PCS,0,0,Players.get(curPlayer),” ^ string_of_int pos ^ “) )”
		| “onBoard” -> “PCS.get( Crd_Plr_Pos_Gt(PCS,0,0,Players.get(curPlayer),” ^ string_of_int pos ^ “) )”
		| _ -> err “Invalid Player Array Access” )
	| _ -> err “Invalid Player Access” )



and let PlayerDotInvFunc args = 
	(match args with
	[pc_n] -> “PCS.get( Crd_Plr_Pcn(PCS,0,0,Players.get(curPlayer),” ^ Id(pc_n) ^”) )”
	| [pl_n; pc_n] -> “PCS.get( Crd_Plr_Pcn(PCS,0,0,Players.get(curPlayer),” ^ Id(pc_n) ^”) )”
	| [pl_n; pc_n; c] -> “PCS.get( Crd_Plr_Pcn(PCS,0,0,Players.get(curPlayer),” ^ Id(pc_n) ^”) )”
	| _ -> err “Invalid Player Access Function” 


and let PlayerDotBdFunc args = 
	(match args with
	[pc_n] -> “PCS.get( Pcn(PCS,” ^ Id(pc_n) ^”) )”
	| [pl_n; pc_n] -> “PCS.get( Plr_Pcn(PCS,” ^ Id(pl_n) ^ “,” ^ Id(pc_n) ^”) )”
	| [pl_n; pc_n; c] -> “PCS.get( Crd_Plr_Pcn(PCS,” ^ string_of_int c.xc ^ “,” ^ string_of_int c.yc ^ “,” ^ Id(pl_n) ^ “,” ^ Id(pc_n) ^”) )”
	| _ -> err “Invalid Player Access Function” 
	

and let BoardAccess x y right = 
	(match right with
	SAccess(keyword,pos,d) -> BoardAccessLoc x y keyword string_of_int pos
	| SCall(func, args,scope,d) -> BoardFunction x y func args
	| SDaccess(lftexpr, rtexpr,d) -> 
		(match lftexpr with
		SAccess(keyword,pos,d) -> BoardAccessDot x y keyword string_of_int pos rtexpr
		| SCall(func, args,scope,d) -> BoardFunctionDot x y func args rtexpr 
		| _ -> err “Invalid Board Access”)
	| _ -> err “Invalid Board Argument

and let BoardFunctionDot x y func args rtexpr = 
	(match SId(func,_) with
	“Pieces” -> BoardFunctionPieces x y args ^ 
		(match rtexpr with
		SCall(func, args,scope,d) -> (match SId(func,_) with
			“owner” -> “.owner”
			| “name” -> “.name”
			| “point” -> “.val”
			| “location” -> “.loc”
			| _ -> err “Invalid Pieces field”)
		| _ -> err “Invalid Pieces Access”)
	| _ -> err “Invalid Board Access Function”)
	


and let BoardFunctionPieces x y args = 
	(match args with
	[pc_n] -> “PCS.get( Crd_Pcn(PCS,” ^ x ^ “,” ^ y ^ “,” ^ Id(pc_n) ^”) )”
	| [pl_n; pc_n] -> “PCS.get( Crd_Plr_Pcn(PCS,” ^ x ^ “,” ^ y ^ “,” ^ Id(pl_n) ^ “,” ^ Id(pc_n) ^”) )”
	| [pl_n; pc_n; c] -> “PCS.get( Crd_Plr_Pcn(PCS,” ^ x ^ “,” ^ y ^ “,” ^ Id(pl_n) ^ “,” ^ Id(pc_n) ^”) )”
	| _ -> err “Invalid Board function” 


and let BoardFunction x y func args = 
	(match string_of_expr func with
	“unoccupied” -> “Crd(PCS,”^x^”,”^y^”)>-1”
	| “Pieces” -> BoardFunctionPieces x y args


and let BoardFunctionPieces x y args = 
	(match args with
	[pc_n] -> “PCS.get( Crd_Pcn(PCS,” ^ x ^ “,” ^ y ^ “,” ^ Id(pc_n) ^”) )”
	| [pl_n; pc_n] -> “PCS.get( Crd_Plr_Pcn(PCS,” ^ x ^ “,” ^ y ^ “,” ^ Id(pl_n) ^ “,” ^ Id(pc_n) ^”) )”
	| [pl_n; pc_n; c] -> “PCS.get( Crd_Plr_Pcn(PCS,” ^ x ^ “,” ^ y ^ “,” ^ Id(pl_n) ^ “,” ^ Id(pc_n) ^”) )”
	| _ -> err “Invalid Board function” 


and let BoardAccessLoc x y keyword pos = 
	(match string_of_expr keyword with
	“Pieces” -> “PCS.get( Crd_Pos(PCS,” ^ x ^ “,” ^ y ^ “,” ^ pos ^ “) )”
	| _ -> err = “Cannot access part of Board”


and let rec Jstmt globals = function
   SBlock(stmts) ->
	"{\n" ^ String.concat "" (List.map Jstmt stmts) ^ "}\n"

 | SExpr(expr) -> Jexpr expr ^ ";\n";

 | SReturn(expr) -> "return " ^ Jexpr expr ^ ";\n";

 | SIf(e, s, Block([])) -> "if (" ^ Jexpr e ^ ")\n" ^ Jstmt s

 | SIf(e, s1, s2) ->  "if (" ^ Jexpr e ^ ")\n" ^
      Jstmt s1 ^ "else\n" ^ Jstmt s2

 | SLoop(e, s) -> 
	(match e with
	   SThrough(e1,e2,d) -> “for(int i=“ ^ SLint(e1) ^ “; i<“ ^
			SLint(e2) ^ “; i++)” ^ Jstmt s 
	| e -> “while (" ^ Jexpr e ^ ") " ^ Jstmt s)

 | SDecl(bgtype,expr,scope) ->
	(match scope with
	Global -> Declare bgtype expr :: globals; “”
	
	| _ -> Declare bgtype expr )


and let Declare bgtype expr = 
	(match bgtype with
	  Int -> "int" | Float -> "float" | Bool -> "boolean” 
	| Coord -> “Point” | String -> “String"
	| Piece -> “Pieces”) ^ " " ^
	(match expr with 
	SAssign(v,ex,sc,d) -> “v = new “ 
		(match bg with
		Piece -> “Pieces(\” “ ^ Jexpr ex ^ “\”);” 
		| String -> “String(\” “ ^ Jexpr ex ^ “\”);” 
		| Coord -> “Point(” ^ Jexpr ex ^ “);”
		| _ -> “”)
	| _ -> Jexpr expr ^ “;”)



and let Jsetup  = function
   Setbd(m) -> "new Board(" ^ string_of_int m.rows ^ "," ^ string_of_int m.cols ^ ")\n"
  | Setpc(pc) -> “for(int i=0; i<pc.num; i++)\nPieces P = new Pieces(" ^ 
	pc.owner ^ "," ^ pc.name ^ "," ^ string_of_int pc.ptval ^ "," ^ 
	string_of_int pc.cloc.xc ^ "," ^ string_of_int pc.cloc.yc ^ “);
	\nPCS.add(P);\n}”
 | Setplr(plr) -> “PLR.add(“ ^ plr ^ “);”
 | Stmt(s) -> Jstmt s ^ "\n"


and let Jrules r = “static void “ ^ r.rname ^ “() {\n” ^ 
	String.concat “” (List.map Jstmt r.rbody) ^ “\n}”

and let JprogSetup setup_list = 
	String.concat “” (List.map Jsetup setup_list)

and let JprogRules rule_list = String.concat “” (List.map Jrules rule_list) ^ “}”
	

and let Jprogram program = function
	let (ssetup srules sstmt) = program in
	let globals = [] in
	let setup_func = JprogSetup ssetup
	and body = Jstmt globals sstmt
	and rule_func = JprogRules srules
	in 
	sprintf “public Class BG {
		%s
		public static void main(String[] args) {
			%s
			%s
		}
		%s
	} globals setup_func body rule_func

