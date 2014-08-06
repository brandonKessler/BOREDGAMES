

%{ open Ast %}

%token SEMI LBRACKET RBRACKET LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE COLON
%token ASSIGN EQ NEQ LT LEQ GT GEQ RETURN IF ELSE LOOP EOF
%token <int> literal
%token <string> ID SETUP RULES PLAY
%token <bool> BOOL
%token <float> DOUBLE
%token <int> INT
%token <coord_t> COORD
%token <string> STRING

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left AND OR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%left MINUSMINUS PLUSPLUS 
%left DOT
%nonassoc LPAREN RPAREN RBRACKET LBRACKET

%start program
%type <Ast.program> program

%%

program:
   setup rules play	{ ($1,$2,$3) }

setup:
   SETUP LBRACE setup_list RBRACE	{ List.rev $3 }

rules:
   RULES LBRACE rule_list RBRACE	{ List.rev $3 } 

play:
   PLAY LBRACE stmt_list RBRACE		{ List.rev $3 }



(* ~~~~~VARIABLE SECTION~~~~~ *)
var_dec:
   vtdec var SEMI		{ ($1,$2) }

vtec:
   INT var		{ Decl($1,$2) }
 | FLOAT var		{ Decl($1,$2) }
 | STRING var		{ Decl($1,$2) }
 | BOOL	var		{ Decl($1,$2) }
 | COORD var		{ Decl($1,$2) }
 | PIECE var            { Decl($1,$2) }
 | MATRIX var           { Decl($1,$2) }

   INT var		{ (Int,$2) }
 | FLOAT var		{ (Float,$2) }
 | STRING var		{ (String,$2) }
 | BOOL	var		{ (Bool,$2) }
 | COORD var		{ (Coord,$2) }
 | PIECE var            { (Piece,$2) }
 | MATRIX var           { (Mat,$2) }

   INT			{ Int }
 | FLOAT		{ Float }
 | STRING		{ String }
 | BOOL			{ Bool }
 | COORD		{ Coord }
 | PIECE		{ Piece }
 | MATRIX		{ Mat }


var:
   ID			{ Id($1) }
 | ID ASSIGN expr	{ Assign($1,$3) }



(* ~~~~~SETUP SECTION~~~~~ *)
setup_list:
   setup_dec			{ [$1] }
 | setup_list setup_dec		{ $2::$1 }

setup_dec:
   bdecl		{ Set(Brd,$1) }
 | pldecl		{ Set(Plr,$1) }
 | pcdecl		{ Set(Pcs,$1) }
 | stmt			{ Stmt($1) }

pldecl:
   PLAYER LPAREN expr RPAREN SEMI{ $3 }

pcdecl:
   PIECES LPAREN pcargs RPAREN SEMI	{ $3 } 

pcargs:
   expr COMMA expr COMMA expr		
		{ {owner = $1; name = $3; num = $5; ptval = 0; cloc = {xc=0; yc=0}} }
 | expr COMMA expr COMMA expr COMMA expr	
		{ {owner = $1; name = $3; num = $5; ptval = $7; cloc = {xc=0; yc=0}} }

bdecl:
   BOARD LPAREN INT COMMA INT RPAREN SEMI 	{ {xc = $3; yc = $5} }



(* ~~~~~RULES SECTION~~~~~ *)
rule_list:
   /* nothing */		{ [] }
 | rule_list rule_dec		{ $2 :: $1 }

rule_dec:
   RULE ID COLON stmt_list SEMI
		{ {rname: $2; rbody = List.rev $4} }



(* ~~~~~STATEMENT SECTION~~~~~ *)
stmt_list:
   /* nothing */	{ [] }
 | stmt_list stmt	{ $2 :: $1 }

stmt:
   expr SEMI					{ Expr($1) }
 | RETURN expr SEMI				{ Return($2) }
 | LBRACE stmt_list RBRACE			{ Block(List.rev $2) }
 | IF LPAREN expr RPAREN stmt %prec NOELSE	{ If($3, $5, Block([])) } (*
 NOELSE ? *)
 | IF LPAREN expr RPAREN stmt ELSE stmt		{ If($3, $5, $7) }
 | LOOP LPAREN expr RPAREN stmt			{ Loop($3, $5) }
 | var_dec					{ Decl(fst $1, snd $1) }



(* ~~~~~EXPRESSION SECTION~~~~~ *)
expr:
   INTLITERAL		{ Lint($1) }
 | DOUBLELITERAL	{ Ldouble($1) }
 | STRINGLITERAL	{ Lstring($1) }
 | COORDLITERAL		{ Lcoord($1) }
 | BOOLLITERAL		{ Lbool($1) }
 | ID			{ Id($1) }
 | expr PLUS expr	{ Binop($1, Add, $3) }
 | expr MINUS expr	{ Binop($1, Sub, $3) }
 | expr TIMES expr	{ Binop($1, Mult, $3) }
 | expr DIVIDE expr	{ Binop($1, Div, $3) }
 | expr EQ expr		{ Binop($1, Equal, $3) }
 | expr NEQ expr	{ Binop($1, Neq, $3) }
 | expr LT expr		{ Binop($1, Less, $3) }
 | expr LEQ expr	{ Binop($1, Leq, $3) }
 | expr GT expr		{ Binop($1, Great'er, $3) }
 | expr GEQ expr	{ Binop($1, Geq, $3) }
 | expr OR expr		{ Binop($1, Or, $3) }
 | expr AND expr	{ Binop($1, And, $3) }
 | expr COLON expr	{ Through($1, $3) }
 | ID ASSIGN expr	{ Assign($1, $3) }
 | expr DOT expr	{ Daccess($1, $3) }
 | expr PLUSPLUS	{ Incr($1,Plus) }
 | expr MINUSMINUS	{ Incr($1,Minus) }

(*????*)
 | ID LBRACKET expr RBRACKET				{ Access($1, $3) }
 | ID LBRACKET LPAREN INT COMMA INT RPAREN RBRACKET	{ Access($1,{xc=$4,yc=$6}) }
 | ID LPAREN actuals RPAREN				{ Call($1, $3) }
 | LPAREN expr RPAREN					{ $2 }
(*????*)
 | LPAREN vtdec RPAREN expr				{ Cast($2,$4) }



actuals:
   /* nothing */		{ [] }
 | actuals_list			{ List.rev $1 }

actuals_list:
   expr				{ [$1] }
 | actuals_list COMMA expr	{ $3::$1 }
