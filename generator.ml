open Printf
open Ast

(* primitive types *)
let string_of_type = function 
      Int -> "int"
    | Float -> "double"
    | String -> "char *"
    | Boolean -> "int"
    | Enum -> "typedef enum"

(* type declaration *)
let string_of_vdecl vd = string_of_type vd.vtype ^ " " ^ vd.vname ^";\n"

(* expressions *)
let rec string_of_expr = function
   (* String_literal(l) -> "\"" ^ l ^ "\"" *)
    String_literal(l) -> "\"" ^ l ^ "\""
  | Int_literal(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	    Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" 
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

(* statement -- TODO *)
 let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
 (* | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s *)
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let includes = "#include <stdio.h>\n#include <stdbool.h>\n"

(* program *)
let string_of_program (input, output, funcs, stmts) = 
    includes ^ "int main() \n{\n//input\n" ^ String.concat "" (List.map string_of_vdecl input) ^ "\n"
  ^  "//output\n" ^ String.concat "" (List.map string_of_vdecl output) ^ "\n" 
  ^ String.concat "" (List.map string_of_stmt stmts) ^ "\nreturn 0;\n}\n"

