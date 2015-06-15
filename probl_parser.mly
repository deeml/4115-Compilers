%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT
%token <int> INT_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left EXPONENT
%nonassoc UMINUS

%start program
%type <Ast.program> program

%%

program:
    /* nothing */ { [], [] }
  | program decl {} /* TODO */
  | program stmt {}

decl:
    vdecl {$1}
  | fdecl {$1}
  | mdecl {$1}

vdecl:
    TYPE ID SEMI {$2}
  | TYPE ID      {$2}

fdecl:
    FUN ID LBRACE FORMALS RBRACE COLON TYPE LBRACE BLOCK RBRACE
    {{}} /* TODO */

mdecl:
    INPUT RBRACE vdecl_list LBRACE OUTPUT RBRACE vdecl_list LBRACE {}

block:
    /* nothing */ { [] }
  | block decl    { $2::$1 } /* TODO: CHECK */
  | block stmt    { $2::$1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($1) }
  | IF LPAREN expr RPAREN block %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN RBRACE block LBRACE ELSE RBRACE block LBRACE 
      { If($3, $6, $10) }
  | WHILE LPAREN expr RPAREN RBRACE block LBRACE { While($3, $6) }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN RBRACE block LBRACE 
      { For($3, $5, $7, $10) }





