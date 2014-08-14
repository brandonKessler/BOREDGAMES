open Sast
open Ast
open Printf
open Semantics 




let rec jexpr = function
   SLint(l,d) -> string_of_int l
 | SLfloat(f,d) -> string_of_float f
 | SLbool(b,d) -> string_of_bool b
 | SLstring(st,d) -> st
 | SId(s,scope, d) -> s
 | SBinop(e1, o, e2, d) ->
	(match d with
	Datatype(Piece) -> jexpr e1 ^ ".equals(" ^ jexpr e2 ^ ")"
	| Datatype(String) -> jexpr e1 ^ ".equals(" ^ jexpr e2 ^ ")" 
	| _ -> jexpr e1 ^ " " ^
		(match o with
		  Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
		| Equal -> "==" | Neq -> "!="
        	| Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
		| Or -> "||" | And -> "&&") ^ " " ^
      		jexpr e2)
 | SThrough(e1,e2,d) -> "for (int i=" ^ jexpr e1^ "; i<" ^
	jexpr e2^"; i++) {\n"

 | SIncr(e,i,d) -> 
	jexpr e ^ 
	(match i with
	  Plus -> "++" | Minus -> "--" | _ -> "Invalid Increment/Decrement") 

 | SAssign(v,e,scope,d) ->  v ^ " = " ^ jexpr e 

 | SCall(func,args,scope,d) -> 
	(match Ast.string_of_expr func with
	"sizeOf" -> (match args with
		[a] -> sizeAccess a
		| _ -> "Invalid sizeOf Arguments")
	| "move" -> (match args with
		[pc; x; y] -> "PC = " ^ jexpr pc^ ";\nPC.loc.x = " ^ jexpr x ^
			";\nPC.loc.y = " ^ jexpr y ^ ";\n"
		| _ -> "Invalid Move Arguments" )
	| "add" -> (match args with 
		[pc; x; y] -> "PC = " ^ jexpr pc^ ";\nPC.loc.x = " ^ jexpr x ^
			";\nPC.loc.y = " ^ jexpr y ^ ";\n"
		| _ -> "Invalid Add Arguments" )
	| _ -> "blah")

 | SBaccess(e1,c,d) -> "PCS.get( Crd(PCS," ^ string_of_int c.sxc ^ "," ^
	string_of_int c.syc ^ ")"
 | SAccess(key,pos,d) -> (match jexpr key with
	"Player" -> "Players.get(" ^ jexpr pos ^ ")"
	| _ -> "Invalid Access" )

 | SDaccess(e1,e2,d) ->
	(match e1 with
	SBaccess(expr,coord,d) -> 
		boardAccess (string_of_int coord.sxc) (string_of_int coord.syc) e2
	| SId(keyword,scope,d) -> (match keyword with
		"Player" -> playerDot e2
		| _ -> "Invalid Left Dot Access" )
	| _ -> "Invalid Left Dot Access" )
		
 | SNoexpr -> ""


and sizeAccess args = 
	(match args with
	SId(keyword,scope,d) -> (match keyword with
		"Board" -> "BD_SIZE"
		| _ -> "Invalid sizeOf Argument")
	| SCall(func, args,scope,d) -> sizeAccessCall func
	| SDaccess(e1,e2,d) -> (match jexpr e1 with 
		"Player" -> (match e2 with
			SCall(func,args,scope,d) -> sizeAccessCall func
			| _ -> "Invalid sizeOf Player Function Argument" )
		| _ -> "Invalid sizeOf Function Argument" )
	| _ -> "Invalid sizeOf Argument" )

and sizeAccessCall func =
	(match Ast.string_of_expr func with 
	"inventory" -> "Crd_SearchCt(PCS,0,0,Players.get(curPlayer))"
	| "onBoard" -> "Crd_SearchCt_Gt(PCS,0,0,Players.get(curPlayer))"
	| _ -> "Invalid Size Function Argument")

and playerDot e2 = 
	(match e2 with 
	SCall(func,args,scope,d) -> (match Ast.string_of_expr func with
		"name" -> "Player.get(curPlayer);"
		| "inventory" -> playerDotInvFunc args 
		| "onBoard" -> playerDotBdFunc args
		| _ -> "Invalid Player Function Access" )
	| SAccess(keyword, pos,d) -> (match jexpr keyword with
		"inventory" -> "PCS.get( Crd_Plr_Pos(PCS,0,0,Players.get(curPlayer)," ^ 
			jexpr pos ^ ") )"
		| "onBoard" -> "PCS.get( Crd_Plr_Pos_Gt(PCS,0,0,Players.get(curPlayer)," ^ 
			jexpr pos ^ ") )"
		| _ -> "Invalid Player Array Access" )
	| _ -> "Invalid Player Access" )



and playerDotInvFunc args = 
	(match args with
	[pc_n] -> "PCS.get( Crd_Plr_Pcn(PCS,0,0,Players.get(curPlayer)," ^ 
		jexpr pc_n ^") )"
	| [pl_n; pc_n] -> "PCS.get( Crd_Plr_Pcn(PCS,0,0,Players.get(curPlayer)," ^ 
		jexpr pc_n ^") )"
	| [pl_n; pc_n; c] -> "PCS.get( Crd_Plr_Pcn(PCS,0,0,Players.get(curPlayer)," ^ 
		jexpr pc_n ^") )"
	| _ -> "Invalid Player Access Function") 


and playerDotBdFunc args = 
	(match args with
	[pc_n] -> "PCS.get( Pcn(PCS," ^ jexpr pc_n ^") )"
	| [pl_n; pc_n] -> "PCS.get( Plr_Pcn(PCS," ^ jexpr pl_n ^ "," ^ jexpr pc_n ^") )"
	| [pl_n; pc_n; x;y] -> "PCS.get( Crd_Plr_Pcn(PCS," ^ jexpr x ^ "," ^ 
		jexpr y ^ "," ^ jexpr pl_n ^ "," ^ jexpr pc_n ^") )"
	| _ ->  "Invalid Player Access Function") 
	
and boardAccess x y right = 
	(match right with
	SAccess(keyword,pos,d) -> boardAccessLoc x y keyword pos
	| SCall(func, args,scope,d) -> boardFunction x y func args
	| SDaccess(lftexpr, rtexpr,d) -> 
		(match lftexpr with
		SAccess(keyword,pos,d) -> boardAccessDot x y keyword pos rtexpr
		| SCall(func, args,scope,d) -> boardFunctionDot x y func args rtexpr 
		| _ -> "Invalid Board Access")
	| _ -> "Invalid Board Argument" )


and boardFunctionDot x y func args rtexpr = 
	(match Ast.string_of_expr func with
	"Pieces" -> boardFunctionPieces x y args ^ 
		(match rtexpr with
		SCall(func, args,scope,d) -> (match Ast.string_of_expr func with
			"owner" -> ".owner"
			| "name" -> ".name"
			| "point" -> ".val"
			| "locationx" -> ".loc.x"
			| "locationy" -> ".loc.y"
			| _ -> "Invalid Pieces field")
		| _ -> "Invalid Pieces Access")
	| _ -> "Invalid Board Access Function")
	
and boardAccessDot x y key pos rtexpr = 
	(match jexpr key with
	"Pieces" -> boardAccessLoc x y key pos ^ 
		(match rtexpr with
		SCall(func, args,scope,d) -> (match Ast.string_of_expr func with
			"owner" -> ".owner"
			| "name" -> ".name"
			| "point" -> ".val"
			| "locationx" -> ".loc.x"
			| "locationy" -> ".loc.y"
			| _ -> "Invalid Pieces field")
		| _ -> "Invalid Pieces Access")
	| _ -> "Invalid Board Access Function")
	

and boardFunctionPieces x y args = 
	(match args with
	[pc_n] -> "PCS.get( Crd_Pcn(PCS," ^ x ^ "," ^ y ^ "," ^ jexpr pc_n ^") )"
	| [pl_n; pc_n] -> "PCS.get( Crd_Plr_Pcn(PCS," ^ x ^ "," ^ y ^ "," ^ jexpr pl_n ^ 
		"," ^ jexpr pc_n ^") )"
	| [pl_n; pc_n; c] -> "PCS.get( Crd_Plr_Pcn(PCS," ^ x ^ "," ^ y ^ "," ^ 
		jexpr pl_n ^ "," ^ jexpr pc_n ^") )"
	| _ ->  "Invalid Board Function" ) 


and boardFunction x y func args = 
	(match Ast.string_of_expr func with
	"unoccupied" -> "Crd(PCS,"^x^","^y^")>-1"
	| "Pieces" -> boardFunctionPieces x y args
	| _ -> "Invalid Board Function" )

and boardAccessLoc x y keyword pos = 
	(match jexpr keyword with
	"Pieces" -> "PCS.get( Crd_Pos(PCS," ^ x ^ "," ^ y ^ "," ^  jexpr pos ^ ") )"
	| _ -> "Cannot Access Part of Board" )


let rec jstmt globals = function
   SBlock(stmts) ->
	"{\n" ^ String.concat "" (List.map (jstmt globals) stmts) ^ "}\n"

 | SExpr(expr) -> jexpr expr ^ ";\n";

 | SReturn(expr) -> "return " ^ jexpr expr ^ ";\n";

 | SIf(e, s, SBlock([])) -> "if (" ^ jexpr e ^ ")\n" ^ jstmt globals s

 | SIf(e, s1, s2) ->  "if (" ^ jexpr e ^ ")\n" ^
      jstmt globals s1 ^ "else\n" ^ jstmt globals s2

 | SLoop(e, s) -> 
	(match e with
	   SThrough(e1,e2,d) -> "for(int i=" ^ jexpr e1 ^ "; i<" ^
			jexpr e2 ^ "; i++)" ^ jstmt globals s 
	| e -> "while (" ^ jexpr e ^ ") " ^ jstmt globals s)

 | SDecl(bgtype,expr,scope) ->
	(match scope with
	Global -> let s = declare bgtype expr in s::globals; ""
	
	| _ -> declare bgtype expr )

 | SNextPlayer -> "NP();" 

and declare bgtype expr = 
	(match bgtype with
	  Datatype(Int) -> "int" | Datatype(Float) -> "double" | Datatype(Bool) -> "boolean" 
	| Datatype(String) -> "String"
	| Datatype(Piece) -> "Pieces") ^ " " ^
	(match expr with 
	SAssign(v,ex,sc,d) -> "v = new " 
		(match bgtype with
		Piece -> "Pieces(\" " ^ jexpr ex ^ "\");" 
		| String -> "String(\" " ^ jexpr ex ^ "\");" 
		| Coord -> "Point(" ^ jexpr ex ^ ");"
		| _ -> "")
	| _ -> jexpr expr ^ ";")



and jsetup globals  = function
   SSetbd(m) -> "new Board(" ^ string_of_int m.srows ^ "," ^ string_of_int m.scols ^ ")\n"
  | SSetpc(pc) -> "for(int i=0; i<" ^pc.snum^ "; i++)\nPieces P = new Pieces(" ^ 
	pc.sowner ^ "," ^ pc.sname ^ "," ^ string_of_int pc.sptval ^ "," ^ 
	string_of_int pc.scloc.sxc ^ "," ^ string_of_int pc.scloc.syc ^ ");
	\nPCS.add(P);\n}"
 | SSetplr(plr) -> "PLR.add(" ^ plr.sname ^ ");"
 | SStmt(s) -> jstmt globals s ^ "\n"


and jrules global r = "static void " ^ r.srname ^ "() {\n" ^ 
	String.concat "" (List.map jstmt global r.srbody) ^ "\n}"

and jprogSetup globals setup_list = 
	String.concat "" (List.map jsetup globals setup_list)

and jprogRules globals rule_list = String.concat "" (List.map jrules global rule_list) ^ "}"
	

and jprogram program =
	let (ssetup, srules, sstmt) = program in
	let globals = [] in
	let setup_func = jprogSetup globals ssetup
	and body = jstmt globals sstmt
	and rule_func = jprogRules globals srules
	in 
	sprintf "public class BG {
		%s
		public static void main(String[] args) {
			%s
			%s
		}
		%s
	}" globals setup_func body rule_func

