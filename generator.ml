open Printf
open Ast

(* primitive types *)
let string_of_type = function 
      Int -> "int"
    | Float -> "double"
    | String -> "String"
    | Boolean -> "boolean"
    | Enum -> "enum"

(* loval variable declaration *)
let string_of_vdecl vd = string_of_type vd.vtype ^ " " ^ vd.vname ^";\n"

(* global variable declaration*)
let string_of_global g = "public " ^ string_of_type g.vtype ^ " " ^ g.vname ^ ";\n"

(* expressions *)
let rec string_of_expr = function
   (* String_literal(l) -> "\"" ^ l ^ "\"" *)
    String_literal(l) -> "\"" ^ l ^ "\""
  | Float_literal(l) -> string_of_float l
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
  | Call(f, el) -> (match f with
    | "print" -> "System.out.println(" ^ String.concat ", " (List.map string_of_expr el)^")"
    | "srand" -> "__rand = new Random(" ^ String.concat ", " (List.map string_of_expr el)^ ")"
    | "rand"  -> "__rand.nextFloat()"
    | _  ->  f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    )
      
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

let imports = "import java.util.*;\n\n"

(* program *)
let string_of_program (input, output, funcs, stmts) = 
  imports
  ^ "class program \n{\n"
    ^ "\t//input\n" ^ String.concat "" (List.map string_of_global input) ^ "\n"
    ^ "\t//output\n" ^ String.concat "" (List.map string_of_global output) ^ "\n" 
    ^ "\tprivate static Random __rand = new Random(System.currentTimeMillis());\n"
    ^ "\tpublic static void main(String[] args){\n"
      ^ "\t\t" ^ String.concat "" (List.map string_of_stmt stmts) ^ "\n\t\treturn ;\n"
    ^ "\t}\n"
  ^ "}\n"

