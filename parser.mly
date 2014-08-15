
%{ open Ast %}

%token SEMI LBRACKET RBRACKET LPAREN RPAREN LBRACE RBRACE COMMA 
%token PLUS MINUS TIMES DIVIDE COLON DOT PLUSPLUS MINUSMINUS
%token ASSIGN EQ NEQ LT LEQ GT GEQ RETURN IF ELSE LOOP EOF AND OR CAT
%token INT BOOL FLOAT STRING BOARD RULE PIECES PLAYER PIECE NEXTPLAYER DICE
%token SETUP RULES PLAY
%token <int> INTLITERAL
%token <string> ID
%token <bool> BOOLLITERAL
%token <float> FLOATLITERAL
%token <string> STRINGLITERAL


%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left AND OR
%left EQ NEQ
%left LT GT LEQ GEQ
%left CAT
%left PLUS MINUS
%left TIMES DIVIDE
%left MINUSMINUS PLUSPLUS 
%nonassoc COLON /* is this right */
%nonassoc LPAREN RPAREN RBRACKET LBRACKET
%left DOT

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

var_dec:
   vdecl SEMI		{ $1 }

vdecl:
   INT var		{ (Int,$2) }
 | FLOAT var		{ (Float,$2) }
 | STRING var		{ (String,$2) }
 | BOOL	var		{ (Bool,$2) }
 | PIECE var            { (Piece,$2) }

var:
   ID			{ Id($1) }
 | ID ASSIGN expr	{ Assign($1,$3) }

setup_list:
   setup_dec			{ [$1] }
 | setup_list setup_dec		{ $2::$1 }

setup_dec:
   bdecl		{ Setbd($1) }
 | pldecl		{ Setplr($1) }
 | pcdecl		{ Setpc($1) }
 | stmt			{ Stmt($1) }


pldecl:
   PLAYER LPAREN STRINGLITERAL RPAREN SEMI{ {plrname = $3} }

pcdecl:
   PIECES LPAREN pcargs RPAREN SEMI	{ $3 } 


pcargs:
   STRINGLITERAL COMMA STRINGLITERAL COMMA INTLITERAL		
		{ {owner = $1; name = $3; num = $5; ptval = 0; cloc =
                        {xc=Lint(0); yc=Lint(0)}} }
 | STRINGLITERAL COMMA STRINGLITERAL COMMA INTLITERAL COMMA INTLITERAL	
		{ {owner = $1; name = $3; num = $5; ptval = $7; cloc =
                        {xc=Lint(0); yc=Lint(0)}} }


bdecl:
   BOARD LPAREN INTLITERAL COMMA INTLITERAL RPAREN SEMI 	{ {rows = $3; cols = $5} }


rule_list:
   /* nothing */		{ [] }
 | rule_list rule_dec		{ $2 :: $1 }

rule_dec:
   RULE ID COLON stmt_list SEMI
		{ {rname= $2; rbody = List.rev $4} }


stmt_list:
   /* nothing */	{ [] }
 | stmt_list stmt	{ $2 :: $1 }

stmt:
   expr SEMI			{ Expr($1) }
 | RETURN expr SEMI		{ Return($2) }
 | LBRACE stmt_list RBRACE	{ Block(List.rev $2) }
 | IF LPAREN expr RPAREN stmt %prec NOELSE	{ If($3, $5, Block([])) } 
 | IF LPAREN expr RPAREN stmt ELSE stmt		{ If($3, $5, $7) }
 | LOOP LPAREN expr RPAREN stmt		{ Loop($3, $5) }
 | var_dec					{ Decl(fst $1, snd $1) }
 | NEXTPLAYER SEMI        {NextPlayer}

expr:
   INTLITERAL		{ Lint($1) }
 | FLOATLITERAL 	{ Lfloat($1) }
 | STRINGLITERAL	{ Lstring($1) }
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
 | expr GT expr		{ Binop($1, Greater, $3) }
 | expr GEQ expr	{ Binop($1, Geq, $3) }
 | expr OR expr		{ Binop($1, Or, $3) }
 | expr AND expr	{ Binop($1, And, $3) }
 | expr COLON expr	{ Through($1, $3) }
 | expr CAT expr        { Cat ($1, $3) }
 | ID ASSIGN expr	{ Assign($1, $3) }
 | expr DOT expr	{ Daccess($1, $3) }
 | expr PLUSPLUS	{ Incr($1,Plus) }
 | expr MINUSMINUS	{ Incr($1,Minus) }
 | expr LPAREN actuals RPAREN		{ Call($1, $3) }
 | LPAREN expr RPAREN	{ $2 }
 | expr LBRACKET expr RBRACKET	{ Access($1, $3) }
 | expr LBRACKET LPAREN expr COMMA expr RPAREN RBRACKET	{ Baccess($1,{xc=$4;yc=$6}) }


actuals:
   /* nothing */		{ [] }
 | actuals_list			{ List.rev $1 }

actuals_list:
   expr				{ [$1] }
 | actuals_list COMMA expr	{ $3::$1 }



