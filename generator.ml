open Printf
open Ast

(* primitive types *)
let string_of_type = function 
      Int -> "int"
    | Float -> "float"
    | String -> "string"
    | Boolean -> "bool"

(* type declaration *)
let string_of_vdecl vd = string_of_type vd.vtype ^ " " ^ vd.vname ^";\n"

(* expressions *)
let rec string_of_expr = function
   (* String_literal(l) -> "\"" ^ l ^ "\"" *)
    String_literal(l) -> "\"" ^ l ^ "\""
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
let string_of_stmt stmt = string_of_expr stmt ^ ";"

(* program *)
let string_of_program (input, output, funcs, stmts) = 
    String.concat "void main() \n{\n//input\n" (List.map string_of_vdecl input) ^ "\n"
  ^ String.concat "//output\n" (List.map string_of_vdecl output) ^ "\n"
  (* add function declarations here *)
  ^ String.concat "" (List.map string_of_stmt stmts) ^ "\n}"

