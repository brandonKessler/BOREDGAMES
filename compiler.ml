
let _ = 
let lexbuf = Lexing.from_channel stdin in
let ast = Parser.program Scanner.token lexbuf in
(*let listing = Ast.string_of_program ast
in print_string listing in *)
let sast = Semantics.check_program ast in
print_string "hello"