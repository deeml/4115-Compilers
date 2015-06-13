{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "'"      { strlit lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LSQRPAREN }
| ']'      { RSQRPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ':'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MODULO }
| '='      { ASSIGN }
| '~'      { PROBASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "not"	   { LOGICALNOT }
| "and"	   { LOGICALAND }
| "or"	   { LOGICALOR }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "fun"    { FUNC }
| "return" { RETURN }
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOLEAN }
| "string" { STRING }
| "enum"   { ENUM }
| ['0'-'9']*['.']['0'-'9']+ as lxm { FLOAT_LITERAL (float_of_string lxm) }
| ['0'-'9']+ as lxm { INT_LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| "true"   { TRUE }
| "false"  { FALSE }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and strlit = parse
  "'" {token lexbuf}
| _   { _::(strlit lexbuf) } 
