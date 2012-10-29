%{ 
  (* parse expressions defined in ast.ml *)
  open Ast;;
%}

/* tokens to parse */
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> TYPE
%token UNIT
%token <string> VAR
%token ADD MIN MUL DIV MOD POW
%token LBRAC RBRAC
%token LET IN EQ
%token FUNC POINT
%token IF THEN ELSE
%token NOT AND ORR
%token LT GT LE GE
%token TRUE FALSE
%token TYPE_DEC OR
%token END EOF TLEND
%token WILDCARD MATCH WITH COMMA
%token SQLBRAC SQRBRAC SEMICOLON CRLBRAC CRRBRAC LAPPEND LCONS
%token NONE SOME

%left NOT
%left LT GT LE GE
%left EQ
%left LET
%left IN
%left IF THEN ELSE
%left FUNC POINT
%left INT FLOAT STRING VAR TRUE FALSE
%left ADD MIN       
%left MUL DIV MOD
%left MATCH WITH OR
%left AND ORR
%left SQLBRAC SQRBRAC
%left LAPPEND 
%left LCONS
%right COMMA SEMICOLON
%right RBRAC
%left LBRAC 
%nonassoc END EOF TLEND

%start parse /* start with parse */
%type <Ast.P.expression> parse
%%
  parse:
  | stmt TLEND                               { $1 }
  | stmt END EOF                             { $1 }
  | stmt EOF                                 { $1 }
  ;
 
  /* statement either let statement or expression */
  stmt:
  | let_stmt                                 { $1 }
  | expr                                     { $1 }
  ;

  let_stmt:
  | LET var let_expr                         { P.Let($2,$3) }
  ;

  let_expr:
  | EQ stmt                                  { $2 }
  | var let_expr                             { P.Func($1,$2) }
  ;

  match_stmt:
  | MATCH expr WITH match_crit               { P.Match($2,$4) }
  ;

  match_crit:
  | OR expr POINT expr                       { [($2,$4)] }
  | OR expr POINT expr match_crit            { ($2,$4)::$5 }
  ;

  /* expressions */
  expr:
  | const                                    { $1 }
  | var                                      { $1 }
  | arith                                    { $1 }
  | match_stmt                               { $1 }
  | compare                                  { $1 }
  | func                                     { $1 }
  | IF expr THEN stmt ELSE stmt              { P.IfBlock($2,$4,$6) }  
  | let_in                                   { $1 }
  | call                                     { $1 }
  | datatype                                 { $1 }
  | tuple                                    { P.DataType(P.Tuple,$1) }
  | list                                     { P.DataType(P.List,$1) }
  | boolean                                  { $1 }
  | LBRAC expr RBRAC                         { $2 }
  ;

  datatype:
  | TYPE                                { P.DataType(P.Other($1),[]) }
  | TYPE WILDCARD                       { P.DataType(P.Other($1),[P.WildCard]) }
  | TYPE VAR                            { P.DataType(P.Other($1),[P.Var $2]) }
  | TYPE LBRAC expr RBRAC               { P.DataType(P.Other($1),[$3]) }
  | TYPE LBRAC tuple_expr expr RBRAC    { P.DataType(P.Other($1),$3@[$4]) }
  ;

  list:
  | expr LCONS var                           { $1::[$3] }
  | expr LCONS list                          { $1::$3 }
  | list LAPPEND list                        { $1@$3 }    
  | SQLBRAC SQRBRAC                          { [] }
  | SQLBRAC expr SQRBRAC                     { [$2] }
  | SQLBRAC list_expr expr SQRBRAC           { $2@[$3] }

  ;

  list_expr:
    expr SEMICOLON                           { [$1] }
  | list_expr expr SEMICOLON                 { $1@[$2] }
  ;

  tuple:
    LBRAC tuple_expr expr RBRAC              { $2@[$3] }
  ;

  tuple_expr:
    expr COMMA                               { [$1] }
  | tuple_expr expr COMMA                    { $1@[$2] }
  ;

  let_in:
    let_stmt IN expr                         { P.LetIn($1,$3) } 
  ;

  call:
  | call expr                                { P.FCall($1,$2) }
  | var expr                                 { P.FCall($1,$2) } 
  | LBRAC var expr RBRAC                     { P.FCall($2,$3) }
  | LBRAC func RBRAC expr                    { P.FCall($2,$4) } 
  ;
  
  func:
  | FUNC var func_expr                       { P.Func($2,$3) }
  ;

  func_expr:
  | POINT expr                               { $2 }
  | var func_expr                            { P.Func($1,$2) } 
  ;

  var:
    VAR                                      { P.Var $1 }
  ;

  const:
    INT                       { P.DataType(P.Int,[P.Const(P.CInt $1)]) }
  | FLOAT                     { P.DataType(P.Float,[P.Const(P.CFloat $1)]) }
  | STRING                    { P.DataType(P.String,[P.Const(P.CString $1)]) }
  | TRUE                      { P.DataType(P.Bool,[P.Const(P.CBool 1)]) }
  | FALSE                     { P.DataType(P.Bool,[P.Const(P.CBool 0)]) }
  | WILDCARD                  { P.WildCard }
  ;

  arith:
    expr ADD expr             { P.Arith(P.Add,$1,$3) }
  | expr MIN expr             { P.Arith(P.Min,$1,$3) }
  | expr MUL expr             { P.Arith(P.Mul,$1,$3) }
  | expr DIV expr             { P.Arith(P.Div,$1,$3) }
  | expr MOD expr             { P.Arith(P.Mod,$1,$3) }
  | expr POW expr             { P.Arith(P.Pow,$1,$3) }
  ;

  compare:
    expr LT expr              { P.Comp(P.LT,$1,$3) }
  | expr GT expr              { P.Comp(P.GT,$1,$3) }
  | expr LE expr              { P.Comp(P.LE,$1,$3) }
  | expr GE expr              { P.Comp(P.GE,$1,$3) }
  | expr EQ expr              { P.Comp(P.EQ,$1,$3) }
  ;

  boolean:
  | LBRAC expr RBRAC AND LBRAC expr RBRAC    { P.BLogic(P.And($2,$6)) }
  | LBRAC expr RBRAC ORR LBRAC expr RBRAC    { P.BLogic(P.Or($2,$6)) }
  | expr AND expr                            { P.BLogic(P.And($1,$3)) }
  | expr ORR expr                            { P.BLogic(P.Or($1,$3)) }
  | NOT expr                                 { P.BLogic(P.Not($2)) }
  ;
