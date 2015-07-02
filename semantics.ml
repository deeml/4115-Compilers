open Printf
open Ast

exception Error of string

(* types *)

type symbol_table = {
    parent : symbol_table option;
    mutable variables : Ast.var_decl list;
}

type environment = { 
    scope : symbol_table;
    mutable funcs : Ast.func_decl list; (* always the same- functions are global *)
    return_type : Ast.dtype;    (* only used inside function definition *)
}

(*built-in functions*)
let core = [ "print"; "rand"; "srand"; "sqrt"; "log" ] 

let rec string_of_type = function
    | Int -> "int"
    | Float -> "float"
    | String -> "string" 
    | Boolean -> "bool"
    | Array(t, l) -> (string_of_type t) ^ "[]"

let rec find_var (scope : symbol_table) name = 
    try
        List.find (fun v -> v.vname = name) scope.variables
    with Not_found -> 
        match scope.parent with 
          Some(parent) -> find_var parent name
        | _ -> raise Not_found

let find_func env name = 
    if List.mem name core then { fname = name; formals = []; 
                                params = []; body = []; ret_type = Float}
    else 
        List.find (fun f -> f.fname = name) env.funcs
    
let var_exists (scope : symbol_table) name = 
    try let _ = find_var scope name in true
    with Not_found -> false

let func_exists env name = 
    try let _ = find_func env name in true
    with Not_found -> false

(* building environments *)

let init_env : environment = 
    let s = { parent = None; 
            variables = [{ vname = "true"; vtype = Boolean}; 
                         {vname = "false"; vtype = Boolean}] } in
    { scope = s; funcs = []; return_type = Float; }


let create_f_env env f = 
    let s = {  parent = Some env.scope; variables = env.scope.variables } 
    in { scope = s; funcs = env.funcs; return_type = f.ret_type }


(* function declarations *)

let check_fname env f = 
    if not ( func_exists env f.fname )
    then  env.funcs <- ( f :: env.funcs )
    else raise(Error("function " ^ f.fname ^ " is previously defined."))

(* variable declarations for input and output blocks *)

let vnames l = List.map (fun v -> v.vname) l

let check_vdecl env vd =
    if not ( List.mem vd.vname (vnames env.scope.variables) )
    then env.scope.variables <- (vd :: env.scope.variables)
    else raise(Error("variable " ^ vd.vname ^ " is previously defined."))

(* expressions *)

let rec check_expr env = function 
    Int_literal(v) -> Int
  | Float_literal(v) -> Float
  | String_literal(v) -> String
  | Id(v) -> 
          let vdecl = try
              find_var env.scope v
          with Not_found -> 
              raise (Error("undeclared identifiier : " ^ v))
          in
          vdecl.vtype
  | Binop(e1, o, e2) -> 
          let bool_ops = [Equal; Neq; Less; Leq; Greater; Geq; And; Or] in 
          if (check_expr env e1) != (check_expr env e2) then
              raise(Error("expressions in binary operators should be of equivalent type"))
          else 
              if List.mem o bool_ops then Boolean else check_expr env e1
  | Lnot(e) -> 
          if (check_expr env e) != Boolean then 
                    raise(Error("\'not\' operator was used on a non-boolean
                    expression"))
          else Boolean
  | Assign(v, e) -> 
         if not (var_exists env.scope v) then raise(Error("variable " ^ v ^ 
         "was referenced before declaration"))
         else 
             let vd = find_var env.scope v in
             if vd.vtype = check_expr env e then vd.vtype
             else raise(Error("type mismatch in assignment : " ^
             (string_of_type vd.vtype) ^ " " ^ vd.vname ^ " = " 
             ^ string_of_type (check_expr env e)))       
          
          
(*       try
              let vd = find_var env.scope v in
              if vd.vtype = check_expr env e then
                  vd.vtype
              else raise(Error("types must match for assignment."))
          with Not_found ->
              raise(Error("variable " ^ v ^ " assigned before declaration."))
*)
  | Call(f, l) -> if not (func_exists env f) 
                    then raise(Error("function " ^ f ^ " is undefined"))
                    else let fd = (find_func env f) in fd.ret_type 
  | Noexpr -> Int 
(* statements *)

let rec check_stmt env = function
    Expr(s) -> check_expr env s ;
  | If(e, s1, s2) -> 
          let et = check_expr env e in
          if et != Ast.Boolean then 
              raise (Error("predicate of if statement must be of type bool"))
          else 
              begin
                  check_stmt env s1;
                  check_stmt env s2;
              end
                            
  | Vdecl(v) -> 
           
          if var_exists env.scope v.vname then
               raise (Error("previously declared variable : " ^ v.vname))
          else
              env.scope.variables <- (v :: env.scope.variables); v.vtype

  | Return(e) -> 
          let et = check_expr env e in
          if et != env.return_type then 
              raise (Error("return types do not match"))
          else et
          
  | For(e1, e2, e3, s) ->
          if (check_expr env e1 = Ast.Int) &&
             (check_expr env e2 = Ast.Boolean) &&
             (check_expr env e3 = Ast.Int) 
          then 
              check_stmt env s
          else raise(Error("for loop expressions must be of types int, bool,
          int ."))

  | While(e, s) -> 
          if check_expr env e != Ast.Boolean then 
              raise(Error("while loop expression must be of type bool."))
          else
              check_stmt env s

  | Block(l) -> 
          let helper sl = check_stmt env sl in
          let _ = (List.map helper l) in Boolean
  
  | Pcall(p, f, a) -> Array(Float, (List.length p)) 


(* check a function declaration for errors *)

let read_formals env f = 
    let fn v = check_vdecl env v in
    List.map fn f.formals

let param_name = function
    Id(s) -> s
  | _ -> ""
 
let read_params env f = 
    if f.params != [] then
        let fn p = 
            env.scope.variables <- ({ vname = param_name p; vtype = Float } ::
                env.scope.variables)
        in List.map fn f.params
    else []

let check_fdecl env f =     
    let e = create_f_env env f (* function body environment *)
    in let helper s = check_stmt e s
    in 
    begin
        check_fname env f;
        read_formals e f;
        read_params env f;
        List.map helper f.body;
    end 
     

(* vdecl list and fdecl list *)

 
let vlist_check env l = 
    let helper k = check_vdecl env k in
    List.map helper l
    

 
let flist_check env l = 
    let helper k = check_fdecl env k in
    List.map helper l
    
let block_check env l = 
    let helper k = check_stmt env k in
    List.map helper l

(* program *)

let check_program p =     
    let (input, output, funcs, model) = p 
    and env = init_env in
    begin
        vlist_check env input;
        vlist_check env output;
        flist_check env funcs;
        block_check env model;
    end

