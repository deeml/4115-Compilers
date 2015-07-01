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

let string_of_formal f = string_of_type f.vtype ^ " " ^ f.vname 

(* global variable declaration*)
let string_of_global g = "public static " ^ string_of_type g.vtype ^ " " ^ g.vname ^ ";\n"

(* expressions *)
let rec string_of_expr = function
   (* String_literal(l) -> "\"" ^ l ^ "\"" *)
    String_literal(l) -> "\"" ^ l ^ "\""
  | Float_literal(l) -> string_of_float l
  | Int_literal(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    (match o with
        Exp -> "Math.pow(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
      | _ ->
            string_of_expr e1 ^ " " ^
            (match o with
      	      Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
            | Equal -> "==" | Neq -> "!="
            | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
            | And -> "&&" | Or -> "||"
            | _ -> "") ^ " " ^
            string_of_expr e2
    )
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) -> (match f with
    | "print" -> "System.out.println(" ^ String.concat ", " (List.map string_of_expr el)^")"
    | "srand" -> "__rand = new Random(" ^ String.concat ", " (List.map string_of_expr el)^ ")"
    | "rand"  -> "__rand.nextFloat()"
    | "log"   -> "Math.log(" ^ String.concat ", " (List.map string_of_expr el)^ ")"
    | "sqrt"  -> "Math.sqrt(" ^ String.concat ", " (List.map string_of_expr el)^ ")"
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
  | Vdecl(v) -> 
     string_of_type v.vtype ^ " " ^ v.vname 
     ^ match v.vtype with
        | Int -> " = 0;\n"
        | Float -> " = 0.0;\n"
        | Boolean -> " = False;\n"
        | String -> " = null;\n"

let imports = "import java.util.*;\nimport java.lang.*;\n\n"

let string_of_fdecl f = "private static " ^ string_of_type f.ret_type ^ " "
                      ^ f.fname ^ "("  
                      ^ String.concat "," (List.map string_of_formal f.formals) 
                      ^ ")\n"
                      ^ "{\n" ^ String.concat "" (List.map string_of_stmt f.body) 
                      ^ "}\n"

(* program *)
let string_of_program (input, output, funcs, stmts) = 
  imports
  ^ "class program \n{\n"
    ^ "//input\n" ^ String.concat "" (List.map string_of_global input) ^ "\n"
    ^ "//output\n" ^ String.concat "" (List.map string_of_global output) ^ "\n" 
    ^ "private static Random __rand = new Random(System.currentTimeMillis());\n"
    ^  String.concat "" (List.map string_of_fdecl funcs) ^ "\n"
    ^ "public static void main(String[] args){\n"
     ^ String.concat "" (List.map string_of_stmt stmts) ^ "\nreturn ;\n"
    ^ "}\n"
  ^ "}\n"

