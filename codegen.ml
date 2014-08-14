open Sast
open Ast
open Printf
open Semantics 


let globals = [];;

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
 | SCat(e1,e2,d) -> jexpr e1 ^ "+" ^ jexpr e2

 | SThrough(e1,e2,d) -> "for (int i=" ^ jexpr e1^ "; i<" ^
	jexpr e2^"; i++) {\n"

 | SIncr(e,i,d) -> 
	jexpr e ^ 
	(match i with
	  Plus -> "++" | Minus -> "--" ) 

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

 | SBaccess(e1,c,d) -> "PCS.get( Crd(PCS," ^ jexpr c.sxc ^ "," ^
	jexpr c.syc ^ ")"
 | SAccess(key,pos,d) -> (match jexpr key with
	"Player" -> "Players.get(" ^ jexpr pos ^ ")"
	| _ -> "Invalid Access" )

 | SDaccess(e1,e2,d) ->
	(match e1 with
	SBaccess(expr,coord,d) -> 
		boardAccess (jexpr coord.sxc) (jexpr coord.syc) e2
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


let rec jstmt = function
   SBlock(stmts) ->
	"{\n" ^ String.concat "" (List.map jstmt stmts) ^ "}\n"

 | SExpr(expr) -> jexpr expr ^ ";\n";

 | SReturn(expr) -> "return " ^ jexpr expr ^ ";\n";

 | SIf(e, s, SBlock([])) -> "if (" ^ jexpr e ^ ")\n" ^ jstmt s

 | SIf(e, s1, s2) ->  "if (" ^ jexpr e ^ ")\n" ^
      jstmt s1 ^ "else\n" ^ jstmt s2

 | SLoop(e, s) -> 
	(match e with
	   SThrough(e1,e2,d) -> "for(int i=" ^ jexpr e1 ^ "; i<" ^
			jexpr e2 ^ "; i++)" ^ jstmt s 
	| e -> "while (" ^ jexpr e ^ ") " ^ jstmt s)

 | SDecl(bgtype,expr,scope) ->
	(match scope with
	Global -> ignore (let gl = declare bgtype expr in gl::globals ); ""
	
	| _ -> declare bgtype expr )

 | SNextPlayer -> "NP();" 

and declare bgtype expr = 
	(match bgtype with
	  Datatype(Int) -> "int" | Datatype(Float) -> "double" | Datatype(Bool) -> "boolean" 
	| Datatype(String) -> "String"
	| Datatype(Piece) -> "Pieces"
	| Datatype(Player) -> ""
	| Datatype(Tile) -> ""
	| Datatype(Rule) -> "" ) ^ " " ^
	(match expr with 
	SAssign(v,ex,sc,d) -> v ^ "= new " ^ 
		(match bgtype with
		Datatype(Piece) -> "Pieces(" ^ jexpr ex ^ ");" 
		| Datatype(String) -> "String(" ^ jexpr ex ^ ");" 
		| _ -> "")
	| _ -> jexpr expr ^ ";")



and jsetup = function
   SSetbd(m) -> "new Board(" ^ string_of_int m.srows ^ "," ^ string_of_int m.scols ^ ")\n"
  | SSetpc(pc) -> "for(int i=0; i<" ^ string_of_int pc.snum^ "; i++)\n" ^ 
	"Pieces P = new Pieces(" ^ pc.sowner ^ "," ^ pc.sname ^ "," ^ 
	string_of_int pc.sptval ^ "," ^ jexpr pc.scloc.sxc ^ "," ^ 
	jexpr pc.scloc.syc ^ ");\nPCS.add(P);\n}"
 | SSetplr(plr) -> "PLR.add(" ^ plr.splrname ^ ");"
 | SStmt(s) -> jstmt s ^ "\n"


and jrules r = (match r with
	SRules_Decl(rule,d) ->  
		"static void " ^ rule.srname ^ "() {\n" ^ 
		String.concat "" (List.map jstmt rule.srbody) ^ "\n}" )

and jprogSetup setup_list = 
	String.concat "" (List.map jsetup setup_list)

and jprogRules rule_list =  
	String.concat "" (List.map jrules rule_list) ^ "}"
	

and jprogram program =
	let (ssetup, srules, sstmt) = program in
	let setup_func = jprogSetup ssetup
	and body = String.concat "" (List.map jstmt sstmt)
	and rule_func = jprogRules srules
	in 
	sprintf "public class BG {
		%s
		public static void main(String[] args) {
			%s
			%s
		}
		%s
	}" (String.concat " " globals) setup_func body rule_func

