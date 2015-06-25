%{ open Ast %}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LSQRPAREN RSQRPAREN COMMA
%token PLUS MINUS TIMES DIVIDE MODULO EXPONENT ASSIGN PROBASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT
%token LOGICALNOT LOGICALAND LOGICALOR
%token FUNC 
%token INT FLOAT BOOLEAN STRING ENUM
%token INPUT OUTPUT MODEL
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <float> FLOAT_LITERAL
%token <string> ID
%token EOF
%token RETURN 
%token TRUE FALSE

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start program
%type <Ast.program> program

%%

program:
    /* nothing */ { [], [], [], [] }
 | INPUT RBRACE vdecl_list LBRACE OUTPUT LBRACE vdecl_list RBRACE fdecl_list
   MODEL LBRACE stmt_list RBRACE { $3, $7, $9, $12 }

fdecl_list:
    /* nothing */ { [] }
  | fdecl_list fdecl { $2 :: $1 }

fdecl:
   FUNC ID LPAREN formals_opt RPAREN COLON dtype LBRACE block RBRACE
     { { fname = $2;
	 formals = $4;
     body = $9;
     ret_type = $7 } }
     

dtype: 
    INT     { Int }
  | FLOAT   { Float }
  | BOOLEAN { Boolean }
  | STRING  { String }
  | ENUM    { Enum }
/*  | dtype LSQPAREN INT_LITERAL RSQPAREN { Array($1, $3) } */

block:
    /* nothing */  { [], [] }
  | vdecl block { ($1::fst $2) , snd $2 }
  | stmt block { fst $2, ($1::snd $2) }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   INT ID SEMI { { vname = $2, vtype = Int } }
 | FLOAT ID SEMI { { vname = $2, vtype = Float } }
 | BOOLEAN ID SEMI { { vname = $2, vtype = Boolean } }
 | STRING ID SEMI { { vname = $2, vtype = String } }
 | ENUM ID SEMI { { vname = $2, vtype = Enum } }
 /* TODO: arrays, enums */



stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | IF LPAREN expr RPAREN LBRACE block RBRACE %prec NOELSE { If($3, $6, Block([])) }
  | IF LPAREN expr RPAREN LBRACE block RBRACE ELSE LBRACE block RBRACE  
     { If($3, $6, $10) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN LBRACE block RBRACE
     { For($3, $5, $7, $10) }
  | WHILE LPAREN expr RPAREN LBRACE block RBRACE { While($3, $6) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | MINUS expr %prec UMINUS { - $2 }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
