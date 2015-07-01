open Printf
open Ast

(* primitive types *)
let rec string_of_type = function 
      Int -> "int"
    | Float -> "double"
    | String -> "String"
    | Boolean -> "boolean"
    | Array(t, l) -> string_of_type t ^ "[]"


(* global variable declaration *)
let string_of_vdecl vd = "public static " ^ string_of_type vd.vtype ^ " " ^ vd.vname  
                         ^ match vd.vtype with
                            | Int -> " = 0;\n"
                            | Float -> " = 0.0;\n"
                            | Boolean -> " = false;\n"
                            | String -> " = null;\n"
                            | Array(t, l) -> " = new " ^ string_of_type t 
                                             ^ "[" ^ string_of_int l ^ "];\n"

let string_of_formal f = string_of_type f.vtype ^ " " ^ f.vname 

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
  | Lnot(e) -> "!(" ^ string_of_expr e ^")"
  | Call(f, el) -> (match f with
    | "print" -> "System.out.println(" ^ String.concat ", " (List.map string_of_expr el)^")"
    | "srand" -> "__rand = new Random(" ^ String.concat ", " (List.map string_of_expr el)^ ")"
    | "rand"  -> "__rand.nextFloat()"
    | "log"   -> "Math.log(" ^ String.concat ", " (List.map string_of_expr el)^ ")"
    | "sqrt"  -> "Math.sqrt(" ^ String.concat ", " (List.map string_of_expr el)^ ")"
    | _  ->  f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    )
      
  | Noexpr -> ""

let string_of_pcassign i pn = "double " ^ pn ^ " = res[" ^ string_of_int i ^ "];\n"

(* statement *)
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
  | Pcall(p, f, a) -> "double[] res = " ^ f ^ ".run(" ^ (* TODO find replacement for List.mapi *) 
                       String.concat ", " (List.map string_of_expr a) ^ ";\n"
                      (* ^ String.concat "" (List.mapi string_of_pcassign p) *)
  | Vdecl(v) -> 
     string_of_type v.vtype ^ " " ^ v.vname 
     ^ match v.vtype with
        | Int -> " = 0;\n"
        | Float -> " = 0.0;\n"
        | Boolean -> " = false;\n"
        | String -> " = null;\n"
        | Array(t, l) -> " = new " ^ string_of_type t 
                         ^ "[" ^ string_of_int l ^ "];\n"


(* normal fdecl *)

let string_of_rfdecl f = "private static " ^ string_of_type f.ret_type ^ " "
                      ^ f.fname ^ "("  
                      ^ String.concat ", " (List.map string_of_formal f.formals) 
                      ^ ")\n"
                      ^ "{\n" ^ String.concat "" (List.map string_of_stmt f.body) 
                      ^ "}\n"

(* sampling / parameter estimation *)
let sid pn = string_of_expr pn

let string_of_param pn = string_of_vdecl { vname = (sid pn) ; vtype = Float }   

let string_of_pinit pn = "double[] __" ^ (sid pn) ^ " = new double[5000];\n"
                       ^ (sid pn) ^ " = __rand.nextFloat();\n"

let string_of_pincr pn = "__" ^ (sid pn) ^ "[i] = " ^ (sid pn) ^ ";\n" 
                       
let string_of_ploop pfd = "for (int i=0; i<5000; i++){\n" 
                          ^ "for (int j=0;j<100;j++){\n "
                          ^ String.concat "" (List.map string_of_stmt pfd.body)
                          ^ "}\n" ^ String.concat "" (List.map string_of_pincr pfd.params)
                          ^ "}\n"

let string_of_mean pn = (sid pn) ^ " = mean(__" ^ (sid pn) ^ ");\n"  


let string_of_pfdecl p = "private class " ^ p.fname
                         ^ "\n{\n" ^ String.concat ""
                                     (List.map string_of_param p.params)
                         ^ "\n"

                         ^ "public double mean(double[] d) { double sum = 0.0;\n"
                         ^ "for(double i : d) { sum += i; } return "
                         ^ "sum/((double) d.length);\n}"

                         ^ "\npublic double[] run(" ^ String.concat ", "
                           (List.map string_of_formal p.formals) ^ ")\n{\n"

                         ^ "double[] res = new double[" ^ string_of_int
                           (List.length p.params) ^ "];\n"

                         ^ String.concat "" (List.map string_of_pinit p.params)

                         ^ string_of_ploop p
                         
                         ^ String.concat "" (List.map string_of_mean p.params) 
                         ^ "res = {" ^ String.concat ", " (List.map sid p.params) ^ "};\n"
                         ^ "return res;\n}\n}"

let string_of_fdecl f = if f.params = [] then 
                                 string_of_rfdecl f 
                        else string_of_pfdecl f


let imports = "import java.util.*;\nimport java.lang.*;\n\n"

(* program *)
let string_of_program (input, output, funcs, stmts) = 
  imports
  ^ "class program \n{\n"
    ^ "//input\n" ^ String.concat "" (List.map string_of_vdecl input) ^ "\n"
    ^ "//output\n" ^ String.concat "" (List.map string_of_vdecl output) ^ "\n" 
    ^ "private static Random __rand = new Random(System.currentTimeMillis());\n"
    ^  String.concat "" (List.map string_of_fdecl funcs) ^ "\n"
    ^ "public static void main(String[] args){\n"
     ^ String.concat "" (List.map string_of_stmt stmts) ^ "\nreturn ;\n"
    ^ "}\n"
  ^ "}\n"

