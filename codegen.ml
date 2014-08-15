open Sast
open Ast
open Printf
open Semantics 


let rec jexpr = function
   SLint(l,d) -> string_of_int l
 | SLfloat(f,d) -> string_of_float f
 | SLbool(b,d) -> string_of_bool b
 | SLstring(st,d) -> st
 | SId(s,scope, d) -> (match (String.get s 0) with
	'R' -> s^"()"
	| _ -> s) 
 | SBinop(e1, o, e2, d) -> jexpr e1 ^
	(match o with
	  Add -> "+ "^jexpr e2 | Sub -> "- "^jexpr e2 | Mult -> "* "^jexpr e2
	 | Div -> "/ "^jexpr e2| Equal -> "== "^jexpr e2 | Neq -> "!= "^jexpr e2
       	| Less -> "< "^jexpr e2 | Leq -> "<= "^jexpr e2 | Greater -> "> "^jexpr e2 
	| Geq -> ">= "^jexpr e2| Or -> "|| "^jexpr e2 | And -> "&& "^ jexpr e2
	| Dequal -> ".equals(" ^ jexpr e2 ^")")
 | SCat(e1,e2,d) -> jexpr e1 ^ "+" ^ jexpr e2

 | SThrough(e1,e2,d) -> "for (int IND=" ^ jexpr e1^ "; IND<" ^
	jexpr e2^"; IND++) {\n"

 | SIncr(e,i,d) -> 
	jexpr e ^ 
	(match i with
	  Plus -> "++" | Minus -> "--" ) 

 | SAssign(v,e,scope,d) ->  v ^ " = " ^ jexpr e 

 | SCall(func,args,scope,d) -> 
	(match Ast.string_of_expr func with
	"move" -> (match args with
		[pc; x; y] -> "PC = " ^ jexpr pc^ ";\nPC.loc.x = " ^ jexpr x ^
			";\nPC.loc.y = " ^ jexpr y ^ ";\n"
		| _ -> "Invalid Move Arguments" )
	| "add" -> (match args with 
		[pc; x; y] -> "PC = " ^ jexpr pc^ ";\nPC.loc.x = " ^ jexpr x ^
			";\nPC.loc.y = " ^ jexpr y 
		| _ -> "Invalid Add Arguments" )
	| "Input" -> (match args with
		[a] -> jexpr a ^ " = " ^ (match a with
			SId(v,sc,dt) -> (match dt with 
				Datatype(Int) -> "Integer.parseInt(input.nextLine());"
				| Datatype(String) -> "input.nextLine();"
				| Datatype(Float) -> "Double.parseDouble(input.nextLine());" 
				| _ -> "Cannot Accept Input")
			| _ -> "Bad Input" )
		| _ -> "Invalid Input Arguments" )
	| "Output" -> (match args with
		[a] -> "System.out.println(" ^ jexpr a ^ ")"
		| _ -> "Invalid Output Arguments")
	| "EndGame" -> (match args with 
		[a] -> "System.out.println(" ^ jexpr a ^ "); System.exit(0)" 
		| _ -> "Invalid EndGame Arguments")
	| _ -> "blah")

 | SBaccess(e1,c,d) -> "PCS.get( Crd(PCS," ^ jexpr c.sxc ^ "," ^
	jexpr c.syc ^ ") )"
 | SAccess(key,pos,d) -> print_string ("SACCESS" ^jexpr key); (match jexpr key with
	"Player" -> "Players.get(" ^ jexpr pos ^ ")"
	| "Pieces" -> print_string "Pieces!!!!"; "Piece test"
	| "Board" -> print_string "Board!!!!!"; "Board test"
	| _ -> "Invalid Access" )

 | SDaccess(e1,e2,d) -> print_string "SDACCESS";
	(match e1 with
	SBaccess(expr,coord,d) -> (match jexpr expr with
		"Player" -> playerDot e2
		| "Board" -> boardAccess (jexpr coord.sxc) (jexpr coord.syc) e2
		| _ -> "test err!!!!" )
	| SId(keyword,scope,d) -> (match keyword with
		"Player" -> playerDot e2
		| _ -> "Invalid Left Dot Access" )
	| SAccess(keyword,pos,d) -> (match jexpr keyword with
		"Player" -> playerAccessDot pos e2
		| _ -> "")
	| SDaccess(ex1,ex2,d) -> print_string "sdaccess in sdaccess";
		(match ex1 with
		SBaccess(expr,coord,d) -> 
			(match ex2 with
			SAccess(keyword,pos,d) -> 
			   boardAccessDot (jexpr coord.sxc) (jexpr coord.syc) keyword pos e2
			| SCall(func, args,scope,d) -> 
			   boardFunctionDot (jexpr coord.sxc) (jexpr coord.syc) func args e2 
			| _ -> "Invalid Board Access")
		| SId(plr,scope,d) -> playerDotInvFunc [] ^
			(match e2 with
			SCall(func, args,scope,d) -> (match Ast.string_of_expr func with
				"owner" -> ".owner"
				| "name" -> ".name"
				| "point" -> ".val"
				| "locationx" -> ".loc.x"
				| "locationy" -> ".loc.y"
				| _ -> "Invalid Pieces field")
			| _ -> "Invalid Pieces Access")
			 
		| _ -> "Invalid Board Dot Argument" )
		
	| _ -> "Invalid Left Dot Access2" )
		
 | SNoexpr -> ""

and playerAccessDot plr_pos e2 = 
	(match e2 with 
	SCall(func,args,scope,d) -> (match Ast.string_of_expr func with
		"name" -> "Player.get(" ^ jexpr plr_pos ^ ")"
		| "inventory" -> "PCS.get( Crd_Plr_Pos(PCS,0,0,Players.get(" ^ 
			jexpr plr_pos ^ "-1), 1 ) )"
		| "onBoard" -> "PCS.get( Crd_Plr_Pos_Gt(PCS,0,0,Players.get(" ^
			jexpr plr_pos ^ "-1), 1) )"
		| _ -> "Invalid Player Function Access" )
	| SAccess(keyword, pos,d) -> (match jexpr keyword with
		"inventory" -> "PCS.get( Crd_Plr_Pos(PCS,0,0,Players.get(" ^
			jexpr plr_pos ^ "-1)," ^ jexpr pos ^ ") )"
		| "onBoard" -> "PCS.get( Crd_Plr_Pos_Gt(PCS,0,0,Players.get(" ^
			jexpr plr_pos ^ "-1)," ^ jexpr pos ^ ") )"
		| _ -> "Invalid Player Array Access" )
	| _ -> "Invalid Player Access" )

and playerDot e2 = 
	(match e2 with 
	SCall(func,args,scope,d) -> (match Ast.string_of_expr func with
		"name" -> "Players.get(curPlayer)"
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



and playerDotInvFunc args  = 
	(match args with
	[] -> "PCS.get( Crd_Plr_Pos(PCS,0,0,Players.get(curPlayer),1) )"
	| [pc_n] -> "PCS.get( Crd_Plr_Pcn(PCS,0,0,Players.get(curPlayer)," ^ 
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
	"Pieces" -> (boardFunctionPieces x y args) ^ 
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
	"unoccupied" -> "(Crd(PCS,"^x^","^y^")==-1)"
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
	   SThrough(e1,e2,d) -> "for(int IND=" ^ jexpr e1 ^ "; IND<=" ^
			jexpr e2 ^ "; IND++)" ^ jstmt s 
	| e -> "while (" ^ jexpr e ^ ") " ^ jstmt s)

 | SDecl(bgtype,expr,scope) ->
	(match scope with
	Global -> ""
	| _ -> declare bgtype expr )

 | SNextPlayer -> "NP();" 

and jrulestmt stmt = (match stmt with 
	SDecl(bgtype,expr,scope) -> declare bgtype expr
	| _ -> jstmt stmt ) 


and declare bgtype expr = 
	(match bgtype with
	  Datatype(Int) -> "int" | Datatype(Float) -> "double" | Datatype(Bool) -> "boolean" 
	| Datatype(String) -> "String"
	| Datatype(Piece) -> "Pieces"
	| Datatype(Player) -> ""
	| Datatype(Tile) -> ""
	| Datatype(Rule) -> "" ) ^ " " ^
	(match expr with 
	SAssign(v,ex,sc,d) -> v ^ (match bgtype with
		Datatype(Piece) -> "= new Pieces(" ^ jexpr ex ^ ");" 
		| Datatype(String) -> "= new String(" ^ jexpr ex ^ ");" 
		| _ -> "= " ^ jexpr ex ^ ";")
	| _ -> jexpr expr ^ ";")



and jsetup = function
   SSetbd(m) -> "rows = " ^ string_of_int m.srows ^ "; cols = " 
	^ string_of_int m.scols ^ ";"
  | SSetpc(pc) -> "for(int IND=0; IND<" ^ string_of_int pc.snum^ "; IND++) {\n" ^ 
	"Pieces P = new Pieces(" ^ pc.sowner ^ "," ^ pc.sname ^ "," ^ 
	string_of_int pc.sptval ^ "," ^ jexpr pc.scloc.sxc ^ "," ^ 
	jexpr pc.scloc.syc ^ ");\nPCS.add(P);}"
 | SSetplr(plr) -> "Players.add(" ^ plr.splrname ^ ");"
 | SStmt(s) -> jstmt s


and jrules r = (match r with
	SRules_Decl(rule,d) ->  
		"static boolean " ^ rule.srname ^ "() {\n" ^ 
		String.concat "\n" (List.rev(List.map jrulestmt rule.srbody)) ^ "}" )

and jprogSetup setup_list = 
	String.concat "\n" (List.rev(List.map jsetup setup_list))

and jprogRules rule_list =  
	String.concat "\n" (List.rev(List.map jrules rule_list)) 
	
and findGlobals play = (match play with
	SDecl(bgtype,expr,scope) -> 
		(match scope with
		Global -> "public static " ^ declare bgtype expr
		| _ -> "" )
	| _ -> "")  

and jprogram program =
	let (ssetup, srules, sstmt) = program in
	let setup_func = jprogSetup ssetup
	and globals = String.concat "\n" (List.rev(List.map findGlobals sstmt))
	and body = String.concat "" (List.rev(List.map jstmt sstmt))
	and rule_func = jprogRules srules
	in 
	sprintf 
"
import java.awt.Point;
import java.util.LinkedList;
import java.util.Scanner;

public class BG {
	
static Scanner input = new Scanner (System.in);
public static LinkedList<String> Players = new LinkedList<String>();
public static LinkedList<Pieces> PCS = new LinkedList<Pieces>();
public static int curPlayer = 0;
public static int rows;
public static int cols;
public static Pieces PC;

static void setup() {
%s
}

%s

public static void main(String[] args) {
setup();	
while(true) {
%s
}

}

%s

/*BoredGame helper functions*/
static void NP () {
	curPlayer++;
	if (curPlayer > Players.size()-1) {
		curPlayer=0;
	}
}
static int Crd_Plr_SearchCt (LinkedList<Pieces> plist, int x, int y, String plr) {
	int count = 0;
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test)) {
			count++;
		}
	}
	return count;
}
static int Crd_Plr_SearchCt_Gt (LinkedList<Pieces> plist, int x, int y, String plr) {
	int count = 0;
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.x > test.x && t.loc.y>test.y) {
			count++;
		}
	}
	return count;
}
static int Pc_Pos (LinkedList<Pieces> plist, Pieces p, int pos) {
	int count = pos;
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.equals(p)) {
			if (count==1) {
				return i;
			}
			count--;
		}
		
	}
	return -1;
}
static int Crd_Plr_Pos (LinkedList<Pieces> plist, int x, int y, String plr, int pos) {
	int count = pos;
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test) && t.owner.equals(plr)) {
			if (count==1) {
				return i;
			}
			count--;
		}
	}
	return -1;
}
static int Crd_Plr_Pos_Gt (LinkedList<Pieces> plist, int x, int y, String plr, int pos) {
	int count = pos;
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.x > test.x && t.loc.y > test.y && t.owner.equals(plr)) {
			if (count==1) {
				return i;
			}
			count--;
		}
	}
	return -1;
}
static int Crd_Pos (LinkedList<Pieces> plist, int x, int y, int pos) {
	int count = pos;
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test)) {
			if (count==1) {
				return i;
			}
			count--;
		}
	}
	return -1;
}
static int Crd_Plr (LinkedList<Pieces> plist, int x, int y, String plr) {
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test) && t.owner.equals(plr)) {
			return i;
		}
	}
	return -1;
}
static int Crd_Plr_Pcn (LinkedList<Pieces> plist, int x, int y, String plr, String pcn) {
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test) && t.owner.equals(plr) && t.name.equals(pcn)) {
			return i;
		}
	}
	return -1;
}
static int Pcn (LinkedList<Pieces> plist, String pcn) {
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.name.equals(pcn)) {
			return i;
		}
	}
	return -1;
}
static int Plr_Pcn (LinkedList<Pieces> plist, String plr, String pcn) {
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.name.equals(pcn) && t.owner.equals(plr)) {
			return i;
		}
	}
	return -1;
}
static int Crd_Pcn (LinkedList<Pieces> plist, int x, int y, String pcn) {
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test) && t.name.equals(pcn)) {
			return i;
		}
	}
	return -1;
}
static int Crd (LinkedList<Pieces> plist, int x, int y) {
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test)) {
			return i;
		}
	}
	return -1;
}
static int Crd_Plr_Gt (LinkedList<Pieces> plist, int x, int y,String plr) {
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.x>test.x && t.loc.y>test.y && t.owner.equals(plr)) {
			return i;
		}

	}
	return -1;
}

}
" setup_func globals body rule_func


