{
  (* tokens defined in parser.mly *)
  open Parser
}
rule token = parse
  | [' ' '\n' '\t']                              { token lexbuf }
  | ['0'-'9']*'.'['0'-'9']* as f                 { FLOAT (float_of_string f) }
  | ['0'-'9']+ as s                              { INT (int_of_string s) }
  | '"'[^ '"']+'"' as s                          { STRING s }
  | '''[^ ''']''' as s                           { STRING s }
  | "let"[' ' '\n' '\t']                         { LET }
  | "let rec"[' ' '\n' '\t']                     { LET }
  | [' ' '\n' '\t']*"in"                         { IN }
  | ";;"[' ' '\t']*"\n"                          { TLEND }
  | ";;"                                         { END }
  | "fun"[' ' '\n' '\t']                         { FUNC }
  | "->"                                         { POINT }
  | "if"[' ' '\n' '\t']                          { IF }
  | "then"[' ' '\n' '\t']                        { THEN }
  | "else"[' ' '\n' '\t']                        { ELSE }
  | "not"[' ' '\n' '\t']*                        { NOT }
  | "type"[' ' '\n' '\t']                        { TYPE_DEC }
  | "match"[' ' '\n' '\t']                       { MATCH }
  | "with"[' ' '\n' '\t']                        { WITH }
  | "none"[' ' '\n' '\t']                        { NONE }
  | "some"[' ' '\n' '\t']                        { SOME }
  | "&&"                                         { AND }
  | "||"                                         { ORR }
  | "<="                                         { LE }
  | ">="                                         { GE }
  | "true"                                       { TRUE }
  | "**"                                         { POW }
  | "false"                                      { FALSE }
  | "()"                                         { UNIT } 
  | "mod"[' ' '\n' '\t']                         { MOD }
  | "+."                                         { ADD
  (* HACK: float ops and int ops parsed as same because JS doesn't care *)}
  | "-."                                         { MIN }
  | "*."                                         { MUL }
  | "/."                                         { DIV }
  | "::"                                         { LCONS }
  | ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*  as s   { TYPE s }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']*  as s   { VAR s }
  | '['                                          { SQLBRAC }
  | ']'                                          { SQRBRAC }
  | ';'                                          { SEMICOLON }
  | '{'                                          { CRLBRAC }
  | '}'                                          { CRRBRAC }
  | '@'                                          { LAPPEND }
  | '+'                                          { ADD }
  | '^'                                          { ADD 
  (* HACK: str cat as add because that's how JS does it, makes it easier *) } 
  | '-'                                          { MIN }
  | '*'                                          { MUL }
  | '/'                                          { DIV }
  | '('                                          { LBRAC }
  | ')'                                          { RBRAC }
  | '<'                                          { LT }
  | '>'                                          { GT }
  | '='                                          { EQ }
  | '|'                                          { OR }
  | '_'                                          { WILDCARD }
  | ','                                          { COMMA }
  | eof                                          { EOF }
