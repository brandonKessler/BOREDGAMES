

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

%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
   /* nothing */	{ {svars=[]; board=_; players=[]; pieces=[]; rules=[]; pvars=[]} }
 | program setup rules play
     { {svars=$2.svars::$1.svars; board=$2.board; players=$2.players::$1.players; 
        pieces=$2.pieces::$1.pieces; rsec=$3::$1.rsec; psec=$4::$1.psec} }

setup:
   SETUP LBRACE vdecl bdecl pldecl_list pcdecl_list RBRACE	
      { {svars = List.rev $3; board = $4; players = List.rev $5; pieces = List.rev $6} }

rules:
   RULES LBRACE rule_list RBRACE	{ List.rev $5 }

play:
   PLAY LBRACE vdecl_list stmt_list RBRACE	{ {plocals = List.rev $3; pbody = List.rev $4} }



vdecl_list:
   /* nothing */	{ [] }
 | vdecl_list vdecl	{ $2 :: $1 }

vdecl:
   type ID SEMI		{ ($1,$2) }

type:
   INT		{ LINT($1) }
 | DOUBLE	{ LDOUBLE($1) }
 | STRING	{ LSTRING($1) }
 | BOOL		{ LBOOL($1) }
 | COORD	{ LCOORD($1) }

pldecl_list:
    pldecl		{ [$1] }
 |  pldecl_list pldecl	{ $2 :: $1 }

pldecl:
   PLAYER LPAREN ID RPAREN	{ $3 }

pcdecl_list:
   pcdecl		{ [$1] }
 | pcdecl_list pcdecl	{ $2 :: $1 }

pcdecl:
   PIECES LPAREN pcargs RPAREN	{ $3 }

pcargs:
   ID COMMA ID COMMA INT		
		{ {owner = $1; name = $3; num = $5; ptval = 0; cloc = {xc=0; yc=0}} }
 | ID COMMA ID COMMA INT COMMA INT	
		{ {owner = $1; name = $3; num = $5; ptval = $7; cloc = {xc=0; yc=0}} }

bdecl:
   BOARD LPAREN INT COMMA INT	{ {xc = $3; yc = $5} }



rule_list:
   rule			{ [$1] }
 | rule_list rule	{ $2 :: $1 }

rule:
   RULE ID COLON vdecl_list stmt_list SEMI
		{ {rname: $2; rlocals = List.rev $4; rbody = List.rev $5} }

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
 | expr GT expr		{ Binop($1, Greater, $3) }
 | expr GEQ expr	{ Binop($1, Geq, $3) }
 | expr OR expr		{ Binop($1, Or, $3) }
 | expr AND expr	{ Binop($1, And, $3) }
 | expr COLON expr	{ Through($1, $3) }
 | expr ASSIGN expr	{ Assign($1, $3) }
 | expr DOT access	{ Daccess($1, $3) }

????
 | ID LPAREN actuals RPAREN		{ Call($1, $3) }
 | LPAREN expr RPAREN	{ $2 }
 | expr LBRACKET expr RBRACKET	{ Access($1, $3) }
????

access:
   ID LPAREN expr_opt RPAREN	{ Call($1, $3) }
 | expr DOT access		{ }
 | access_list			{ List.rev $1 }

access_list:
   expr				{ $1 }
 | access_list DOT expr		{ $2::$1 }

actuals:
   /* nothing */		{ [] }
 | actuals_list			{ List.rev $1 }

actuals_list:
   expr				{ [$1] }
 | actuals_list COMMA expr	{ $3::$1 }
