

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ NEQ LT LEQ GT GEQ RETURN IF ELSE LOOP EOF
%token <int> literal
%token <string> ID SETUP RULES PLAY
%token <string> TYPE

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
 | program vdecl	{ ($2 :: fst $1), snd $1) }
 | program setup	{ (fst $1, ($2 :: snd $1) }

vdecl_list:
   /* nothing */	{ [] }
 | vdecl_list vdecl	{ $2 :: $1 }

vdecl:
   TYPE ID SEMI		{ ($1,$2) }

setup:
   SETUP LPAREN RPAREN LBRACE bdecl pldecl_list pcdecl_list RBRACE	{ 
		{players = List.rev $6; pieces = List.rev $7; board = $5} }

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


