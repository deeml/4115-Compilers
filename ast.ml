type op = Add | Sub | Mult | Div | Exp | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type dtype = 
    Int
  | Float
  | Boolean
  | String
  | Enum
  (* | Array of dtype * int *)
  (* TODO: array, add 2d arrays, enums *)

type expr =
    String_literal of string
  | Float_literal of float
  | Int_literal of int
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type var_decl = {
    vname : string;
    vtype : dtype;
  }

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt 
  | For of expr * expr * expr * stmt 
  | While of expr * stmt
  | Vdecl of var_decl 
  
type func_decl = {
    fname : string;
    formals : var_decl list; 
    body : stmt list;
    ret_type : dtype;
  }

type program = var_decl list * var_decl list * func_decl list * stmt list

(* code generation moved to generator.ml *)

(* 
let rec string_of_expr = function
    String_literal(l) -> l
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

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_dtype = function
    Int -> "int"
  | Float -> "float"
  | String -> "char*"
  | Boolean -> "bool"
  | Enum -> "int" (* TODO: fix this *)


let string_of_vdecl vdecl = 
  string_of_dtype vdecl.vtype ^ " " ^ vdecl.vname

(*let string_of_formal_vdecl var_d = string_of_vdecl var_d ^ ", " *)
let string_of_vdecl_semi var_d = string_of_vdecl var_d  ^ ";\n"

let string_of_fdecl fdecl =
  string_of_dtype fdecl.ret_type ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl_semi fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
*)

