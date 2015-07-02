open Printf

(* to run, do ./probl <source-filename> *)

let in_f = Sys.argv.(1)
let out_f = "probl-out.java"

let _ =
    let in_ch = open_in in_f in
    let lexbuf = Lexing.from_channel in_ch in
    let program = Parser.program Scanner.token lexbuf in
    let code = Generator.string_of_program program in 
    close_in in_ch;
    try 
        Semantics.check_program program;
        let o_ch = open_out out_f in
        fprintf o_ch "%s" code ;
        close_out o_ch;
        print_string code
    with Semantics.Error(m) -> 
        print_string ( code ^ "\n/* ERR : " ^ m ^ " */" ^ "\n" );

