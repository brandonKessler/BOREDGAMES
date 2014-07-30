

%{ open Ast %}

%token SEMI LBRACKET RBRACKET LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE
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
   /* nothing */	{[], []}
 | program vdecl	{ ($2 :: fst $1), snd $1 }
 | program setup	{ fst $1, ($2 :: snd $1) }
 | program rules	{ fst $1, (snd $1).rules=$2 }
 | program play		{ ($2 :: fst $1), snd $1 } 

setup:
   SETUP LBRACE bdecl pldecl_list pcdecl_list RBRACE	{ 
		{players = List.rev $6; pieces = List.rev $7; board = $5} }

rules:
   RULES LBRACE rule_list RBRACE	{ List.rev $5 }

play:
   PLAY LBRACE vdecl_list stmt_list RBRACE	{ List.rev $5 }



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
   LINT			{ Lint($1) }
 | LDOUBLE		{ Ldouble($1) }
 | LSTRING		{ Lstring($1) }
 | LCOORD		{ Lcoord($1) }
 | LBOOL		{ Lbool($1) }
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
 | ID ASSIGN expr	{ Assign($1, $3) }
 | ID LPAREN actuals_opt RPAREN		{ Call($1, $3) }
 | LPAREN expr RPAREN	{ $2 }
 | ID LBRACKET LITERAL RBRACKET		{ Access($1, $3) }

expr_opt:
   /* nothing */	{ Noexpr }
 | expr			{ $1 }


