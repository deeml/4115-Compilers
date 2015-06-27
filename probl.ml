(* to run (with bash), do: 
 * $make && cat <source-file> | ./probl *)
let _ =
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.program Scanner.token lexbuf in
    let code = Generator.string_of_program program in 
    print_string code
