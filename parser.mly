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
 | INPUT LBRACE vdecl_list RBRACE OUTPUT LBRACE vdecl_list RBRACE fdecl_list
   MODEL LBRACE stmt_list RBRACE { $3, $7, $9, $12 } 

fdecl_list:
    /* nothing */ { [] }
  | fdecl_list fdecl { $2 :: $1 }

fdecl:
   FUNC ID LPAREN formals_opt RPAREN COLON dtype LBRACE stmt_list RBRACE
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

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
  | type_decl                  { [$1] }
  | formal_list COMMA type_decl { $3 :: $1 }

type_decl:
    dtype ID { { vname = $2; vtype = $1 } }
  /*| dtype ID SEMI { { vname = $2; vtype = $1 } } */
 /* TODO: add enums and arrays */

/*
vdecl_opt:
     nothing  { [] }
  | vdecl_list   { List.rev $1 }
*/
vdecl_list:
  /*  nothing */  { [] }
  | type_decl                   { [$1] }
  | vdecl_list SEMI type_decl       { $3 :: $1 }
  | vdecl_list COMMA type_decl SEMI { $3 :: $1 }
  | vdecl_list SEMI      { $1 }

stmt_list:
    stmt_list_opt { List.rev $1 }

stmt_list_opt:
    /* nothing */  { [] }
  | stmt_list_opt stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) } 
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) } 
  | IF LPAREN expr RPAREN stmt ELSE stmt 
     { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN  stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | type_decl SEMI { Vdecl($1) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LITERAL      { Int_literal($1) }
  | FLOAT_LITERAL    { Float_literal($1) }
  | STRING_LITERAL   { String_literal($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | MINUS expr %prec UMINUS { Binop(Int_literal(0), Sub, $2) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr LOGICALAND expr { Binop($1, And,   $3) }
  | expr LOGICALOR expr { Binop($1, Or,   $3) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
