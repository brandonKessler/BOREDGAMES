{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ','      { COMMA }
| ':'      { COLON }
| '.'      { DOT }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'      { CAT }
| "++"     { PLUSPLUS }
| "--"     { MINUSMINUS }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
(* add not?*)
| ".=" { DEQ } 
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "loop"   { LOOP }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "string" { STRING }
| "double" { FLOAT }
| "piece"  { PIECE }
| "new Board"  { BOARD }
| "new Player" { PLAYER }
| "new Pieces" { PIECES }
| "Setup"  { SETUP }
| "Rules"  { RULES }
| "rule"   { RULE }
| "Play"   { PLAY }
| "NextPlayer" { NEXTPLAYER }
| "Dice" { DICE }
| ('-')? ['0'-'9']+ as lxm { INTLITERAL(int_of_string lxm) }
| ("true" | "false") as lxm { BOOLLITERAL(bool_of_string lxm) }
| ('-')? (['0'-'9'])+ '.' (['0' - '9'])* as lxm { FLOATLITERAL(float_of_string lxm) }
| '"' [^'"']* '"'  as lxm  { STRINGLITERAL(lxm) } (* check for correctness *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }


