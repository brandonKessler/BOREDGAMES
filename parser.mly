

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ NEQ LT LEQ GT GEQ RETURN IF ELSE FOR WHILE EOF
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
   SETUP LPAREN RPAREN LBRACE vdecl_list idecl_list stmt_list RBRACE	{ 
		{sname = $1; locals = List.rev $5; items = List.rev $6; body = List.rev $7} }

idecl_list:
   /* nothing */
 |  BOARD LPAREN 
