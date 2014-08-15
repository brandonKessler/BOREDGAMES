
let _ = 
let lexbuf = Lexing.from_channel stdin in
let ast = Parser.program Scanner.token lexbuf in
(*let listing = Ast.string_of_program ast
in print_string listing in *)
let sast = Semantics.check_program ast in
let pgm = Codegen.jprogram sast in
let output = open_out "BG.java" in
output_string output pgm
(*print_string "done \n"*)

