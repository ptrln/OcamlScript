(* Final Project - OcamlScript
 * CS51 Spring 2011
 * Author: Peter Lin (CSCI E-250)
 *)

module ParsedOcamlExpr =
  struct

    (* arithmetic types, + and +. are both parsed to the same Add as JS doesn't
     * need different ops for different types *)
    type arith = Add | Min | Mul | Div | Mod | Pow

    (* comparison types *)
    type compare = GE | LE | GT | LT | EQ

    (* const types, purely for parsing purposes. no expr should be just a const
     * as all data is wrapped in a data type *)
    type const =  
      CInt of int | CFloat of float | CString of string | CBool of int

    (* some built in data types plus Other for user defined types. Note type
     * declarations are not required in ocamlscript, attempting to translate
     * those will result in parse error *)
    type dtypes = Tuple | List | Int | Float | String | Bool | Other of string
    
    (* all the expressions that can be parsed by ocamlscript, detailed below:
     * expr        what it is          example
     * Func:       function            fun e1 -> e2 => Func(e1,e2)
     * FCall       function call       e1 e2 => FCall(e1,e2)
     * Arith       arithmetic op       e1 + e2 => Arith(Add,e1,e2)
     * Comp        comparison op       e1 = e2 => Comp(Eq,e1,e2)
     * Var         variable            hello => Var("hello")
     * WildCard    wildcard char       _ => WildCard
     * Const       constants           1 => Int(1)
     * Match       match statements    match e with | pat -> expr =>
     *                                   Match(e,[(pat,expr)])
     * BLogic      boolean logic       e1 && e2 => BLogic(And,e1,e2)
     * IfBlock     if/then/else block  if e1 then e2 else e3 =>
     *                                   IfBlock(e1,e2,e3)
     * Let         let statement       let v = e => Let(v,e)
     * LetIn       let in expr         let y = e in y => LetIn(Let(y,e),e)
     * DataType    data types          (e1,e2) => DataType(Tuple,[e1;e2])
     * note that in order to have universal pattern matching for all datatypes,
     * all datatypes are represented by some type dtypes, followed by a list of
     * values. 
     *)
    type expression = 
      | Func of expression * expression
      | FCall of expression * expression
      | Arith of arith * expression * expression
      | Comp of compare * expression * expression 
      | Var of string
      | WildCard
      | Const of const
      | Match of expression * ((expression * expression) list)
      | BLogic of boolean
      | IfBlock of expression * expression * expression
      | Let of expression * expression
      | LetIn of expression * expression
      | DataType of dtypes * (expression list)
    and boolean = 
      And of expression * expression 
      | Or of expression * expression 
      | Not of expression

    (* converts an ParsedOcamlExpr into a more readable string for display.
     * mainly used for debugging. not really useful otherwise *)
    let rec e2string e =
      match e with
        | Let (v,e) -> 
	    "Let(" ^ e2string v ^ ", " ^ e2string e ^")"
	| LetIn (v,e) -> 
	    "LetIn(" ^ e2string v ^ ", " ^ e2string e ^")"
        | Func (v,e) -> 
	    "Func(" ^ e2string v ^ ", " ^ e2string e ^ ")"
        | FCall (e1,e2) -> 
	    "Call(" ^ e2string e1 ^ ", " ^ e2string e2 ^ ")"
	| Var vs -> 
	    "Var(" ^ vs ^ ")"
        | Arith (a,e1,e2) -> 
            (match a with
               | Add -> "Add("
               | Min -> "Min("
               | Mul -> "Mul(" 
               | Div -> "Div(" 
               | Mod -> "Mod(" 
	       | Pow -> "Pow(" ) ^ e2string e1 ^ ", " ^ e2string e2 ^ ")"
        | Const c -> 
            (match c with
               | CInt i -> "Int " ^ string_of_int i
               | CFloat f -> "Float " ^ string_of_float f
               | CString s -> "String " ^ s 
	       | CBool b -> "Bool" ^ (if b!=0 then "true" else "false"))
	| WildCard -> "WildCard(ANY)"
	| DataType(n,arg) ->
	    let kind = 
              (match n with
                 | Tuple -> "tuple"
                 | List -> "list"
                 | Int -> "int"
                 | Float -> "float"
                 | String -> "string"
                 | Bool -> "bool"
                 | Other s -> s ) 
            in
            let vall = 
              List.fold_left (fun acc e -> acc ^ "(" ^ e2string e ^ ")") "" arg
            in
	    "DataType(" ^ kind ^ ", " ^ vall ^ ")"
        | IfBlock (ife,thene,elsee) ->
            "IfBlock(" ^ e2string ife ^ ", " ^ 
	             e2string thene ^ ", " ^ 
	             e2string elsee ^ ")"
        | Comp(c,e1,e2) ->
            let cs = 
              (match c with
                 | GT -> ">"
                 | LT -> "<"
                 | GE -> ">="
                 | LE -> "<="
                 | EQ -> "==") 
            in
            "Compare(" ^ cs ^ ", " ^ e2string e1 ^ ", " ^ e2string e2 ^ ")" 
        | BLogic(b) -> 
            (match b with
              | Not(e) -> 
                  "BLogic(Not, " ^ e2string e ^ "))"
              | And(e1,e2) ->  
                  "BLogic(And, " ^ "," ^  e2string e1 ^ "," ^ e2string e2 ^ ")"
              | Or(e1,e2) ->
                  "BLogic(Or, " ^ "," ^  e2string e1 ^ "," ^ e2string e2 ^ ")")
        | Match(m,l) ->
            (match l with
               | [] -> assert false
               | _::_ ->
                   let m_str = 
                      List.fold_left (fun acc (c,e) -> 
                        acc ^ "(" ^ e2string c ^ ", " ^ e2string e ^ ")") "" l
                   in
                   "Match(" ^ e2string m ^ ", " ^ m_str ^ ")")

    let string_of_expr = e2string
  end (* module ParsedOcamlExpr *)


module JavaScriptExpr = 
 struct
  
  (* JS arithmetic types. *)
  type arith = Add | Min | Mul | Div | Mod | Pow

  (* JS comparison types *)
  type compare = GE | LE | GT | LT | EQ


  (* all the expressions that can be translated to by ocamljs, a psuedo 
   * javascript output by print_js is used in the example section below (the
   * examples do not truly represent the nature of datatypes being returned)
   *
   *
   * expr        what it is          example
   * Func:       function            Func(e1,e2) -> function(e1){return e2}
   * FCall       function call       Call([e1;e2;e3]) -> (e1)(e2)(e3) 
   * Arith       arithmetic op       Arith(add,e1,e2) -> ((e1) + (e2))
   * Compare     comparison op       Comp(Eq,e1,e2) -> (e1 == e2)
   * Var         variable            Var("hello") -> hello
   * Const       string              1.2 -> 1.2
   * IfElse      if/else block       IfElse(e1,e2,e3) -> e1 ? e2 : e3
   * Assign      var assignment      Assign(v,e) -> v = e
   * Logic       boolean logic       Logic(And(e1,e2)) -> e1 && e2
   * Array       data types          ["tuple2";1;2] -> new Array ("tuple2",1,2)
   * Match       pattern match       Match(x,[(pat1,e1);(pat2,e2);(ANY,e3)]) ->
   *                                 function() {
   *                                     if (x==pat1)
   *                                       return e1
   *                                     else if (x==pat2)
   *                                       return e2
   *                                     else
   *                                       return e3
   *                                 } ()
   *
   * Note: Match is the only expression that is not a valid expression in
   * javsscript. I had to put a Match block in here to do pattern matching
   * more easily.
   *
   * NOTE: that in order to pattern match across data types, all values are 
   * maintained in a data type with the javascript array as:
   *     [kind,val1,val2,val3...] 
   * all data types are maintained in this Array. the first field records the
   * type to pattern match, and the rest records the data types values. because
   * all the code must maintain the datatype, some operations (airth / compare)
   * may seem strage, but remember that after evaluation in javascript, the 
   * data type array structure must always be maintained. *)

  type js =
    | Func of js * js
    | Call of (js list)
    | Arith of arith * js * js
    | Var of string
    | Const of string
    | Any
    | IfElse of js * js * js
    | Assign of js * js
    | Logic of blogic
    | Match of string * ((js*js) list)
    | Array of (js list)
    | Compare of compare * js * js
  and blogic =
      And of js * js
    | Or of js * js
    | Not of js

  (* js_rec prints JavaScriptExpr expressions into valid javascript code that
   * can be evaluated. for most expression, js_rec does very little work other 
   * than print out code in the correct JS syntax. In the case of match 
   * expressions, js_rec does a lot of work in order to output pattern matching
   * JS code. it relies on the two functions detailed below: 
   *
   * pattern_match var_name pattern n
   * pattern_match takes the pattern to match on and matches it against 
   * var_name. pattern_match only matches data types and wildcards, if attempt
   * to match something else, ocamlscript will exit from assert false. if 
   * pattern match is data type, then pattern_match attempts to match the type
   * and every field in the data type. For example, for data type [tuple2,1,2]
   * pattern match will match (v[0] == "tuple2" && v[1] == 1 && v[2] ==2),
   * making sure every field matches. if it encounters Any or Var it just 
   * returns true. if it encounters another data type (eg nested data types), 
   * it recursively calls pattern_match only the nested data type, ensuring 
   * every field in the pattern is matched properly.
   *
   * assign_var vn pattern
   * assign_var looks for variables in the pattern and assigns it the value
   * from the match_var. for example match x with | Type(var) -> do_this, the 
   * variable var is assigned x[1] iff the data type matches, that is iff 
   * x[0] == "Type" and all the other field match. if it encounters nested data
   * types it will recurse and continue var assignment in a similar fashion to
   * pattern_match.
   * 
   *)
  let rec js_rec (j:js) : string =
    let rec pattern_match v pattern n =
      let strn = string_of_int n in
      match pattern with
        | Array a ->
            (match a with
	      | [] -> "true"
	      | h::t-> 
		  (match h with
		     | Any -> ""
		     | Var _ -> ""
		     | Array _ -> 
                         (* nested data type, recurse on it *)
			 pattern_match (v ^ "[" ^ strn ^ "]") h 0 ^ " && " 
		     | _ ->
                         (* some field to match on *)
		         "(" ^ v ^ "[" ^ strn ^ "] == " ^ js_rec h ^ ") && " 
                  ) ^ pattern_match v (Array t) (n+1) (* recurse on rest *)
	    )
        | Any -> "true"
	| Var _ -> "true"
        | _ -> assert false (* pattern match only matches datatypes/any/var *)
    in
    let rec assign_var vn pattern =
      match pattern with
        | Array a ->
            let rec rec_avar mv cl n =
              let strn = string_of_int n in
              (match cl with 
                 | [] -> ""
                 | h::t -> 
                     (match h with
                        | Var v -> 
                            (* found var, assign it *)
                            "var " ^ v ^ "=" ^ mv ^ "[" ^ strn ^ "];"
                        | Array a1 ->
                            (* recurse on nested data types *)
                            assign_var ( mv ^ "[" ^ strn ^ "]") h
                        | _ -> "" ) ^ rec_avar mv t (n+1)) (* recurse on rest*)
            in
            rec_avar vn a 0
	| Var v ->
            "var " ^ v ^ "=" ^ vn ^ ";"
        | _ -> "" (* don't assign any other expr *)
    in
    match j with
      | Assign (v,e) ->
          js_rec v ^ "=" ^ js_rec e
      | Func (e1,e2) -> 
          "(function(" ^ js_rec e1 ^ "){" ^ "return " ^ js_rec e2 ^ "})"
      | Call (c) -> 
          List.fold_right (fun e r -> "(" ^ js_rec e ^ ")" ^ r) c ""
      | Var s -> s    
      | Const c -> c
      | Arith (a,e1,e2) -> 
          let kind = "(" ^ js_rec e1 ^ ")[0]" in (* preserve data type *)
          let op = 
            let v1 = "(" ^ js_rec e1 ^ ")[1]" in
            let v2 = "(" ^ js_rec e2 ^ ")[1]" in
            (match a with
               | Add ->  v1 ^ " + " ^ v2
               | Min ->  v1 ^ " - " ^ v2
               | Mul ->  v1 ^ " * " ^ v2
               | Div ->  v1 ^ " / " ^ v2
               | Mod ->  v1 ^ " % " ^ v2
               | Pow -> "Math.pow(" ^ v1 ^ "," ^ v2 ^ ")" )
          in
          "new Array(" ^ kind ^ "," ^ op ^ ")"
      | IfElse (ife,thene,elsee) ->
          "(((" ^ js_rec ife ^ ")[1]) ? " ^ 
          "(" ^ js_rec thene ^ ") : (" ^ js_rec elsee ^ "))"
      | Array a ->
          let s = 
            if a = [] then assert false (* empty data type undefined *)
            else
            let srec =
              List.fold_left (fun acc e ->  acc ^ js_rec e ^ ", ") "" a
            in
            (* remove the last ", " which shouldn't be there *)
            String.sub srec 0 ((String.length srec) - 2)
          in
          "new Array(" ^ s ^ ")"
        (* Any only used in pattern matches, thus should never be printed *)
      | Any -> assert false 
      | Logic b ->
          let b_expr = 
            (match b with
             | And(e1,e2) ->
                 let v1 = "(" ^ js_rec e1 ^ ")[1]" in
                 let v2 = "(" ^ js_rec e2 ^ ")[1]" in
                 "(" ^ v1 ^ ") && (" ^ v2 ^ ")"
             | Or(e1,e2) ->
                 let v1 = "(" ^ js_rec e1 ^ ")[1]" in
                 let v2 = "(" ^ js_rec e2 ^ ")[1]" in
                 "(" ^ v1 ^ ") || (" ^ v2 ^ ")"
             | Not(e) ->
                 let v = "(" ^ js_rec e ^ ")[1]" in
                 "(!(" ^ v ^ "))")
          in
          "new Array(\"bool\"," ^ b_expr ^ ")" (* boolean logic returns bool *)
      | Compare(c,e1,e2)->
          let c_expr = 
          let v1 = "(" ^ js_rec e1 ^ ")[1]" in
          let v2 = "(" ^ js_rec e2 ^ ")[1]" in
          (match c with
             | GT -> "((" ^ v1 ^ ") " ^ ">" ^ " (" ^ v2 ^ "))"
             | LT -> "((" ^ v1 ^ ") " ^ "<" ^ " (" ^ v2 ^ "))"
             | GE -> "((" ^ v1 ^ ") " ^ ">=" ^ " (" ^ v2 ^ "))"
             | LE -> "((" ^ v1 ^ ") " ^ "<=" ^ " (" ^ v2 ^ "))"
             | EQ -> 
                 (* EQ is special case - check both type and val *)
                 let t1 = "(" ^ js_rec e1 ^ ")[0]" in
                 let t2 = "(" ^ js_rec e2 ^ ")[0]" in
                 "((" ^ t1 ^ ") " ^ "==" ^ " (" ^ t2 ^ "))&&" ^
                 "((" ^ v1 ^ ") " ^ "==" ^ " (" ^ v2 ^ "))") 
          in
          "new Array(\"bool\", " ^ c_expr ^ ")" (* comparison returns bool *)
      | Match (v,l) ->
          let rec match_rec lst =
            (match lst with
              | [] -> ""
              | [(c,d)] -> 
                  (* last pattern, no need for else/recurse *)
                  "if(" ^ pattern_match v c 0 ^ "){" ^
                  assign_var v c ^
                  "return " ^ js_rec d ^ "}"
              | (c,d)::t ->  
                  (* not last, do else and recurse *)
                  "if(" ^ pattern_match v c 0 ^ "){" ^
                  assign_var v c ^
                  "return " ^ js_rec d ^ "" ^
                  "}else{" ^ match_rec t ^ "}" 
            )
          in
          "(function(){" ^ match_rec l ^ "}) ()"

  (* converts an JavaScriptExpr into a more readable string for display. mainly
   * used for debugging *)
  let rec js2string (j:js) : string =
    match j with
      | Assign (v,e) -> 
          "Assign(" ^ js2string v ^ ", " ^js2string e ^ ")"
      | Func (e1,e2) -> 
          "Func(" ^ js2string e1 ^ ", " ^ js2string e2 ^ ")"
      | Call (c) -> 
          "Call(" ^ 
            List.fold_left (fun acc e -> acc ^ "(" ^ js2string e ^ ")") "" c
          ^ ")"
      | Var s -> "Var(" ^ s ^ ")"
      | Const c -> "Const(" ^ c ^ ")"
      | Array a ->
          let s = if a = [] then "" 
          else
            let srec =
              List.fold_left (fun acc e ->  acc ^ js2string e ^ ", ") "" a 
            in
            String.sub srec 0 ((String.length srec) - 2)
          in
          "Array(" ^ s ^ ")"
      | Any -> "ANY"
      | Match (v,l) ->
          let rec cond_s lst = 
            (match lst with
               | [] -> ""
               | (c,d)::t -> 
                   "(" ^ js2string c ^ ", " ^ js2string d ^ ")" ^ (cond_s t))
          in
          "Match(" ^ v ^ ", " ^ (cond_s l) ^ ")"
      | Arith (a,e1,e2) -> 
          (match a with
             | Add -> "Add"
             | Min -> "Min"
             | Mul -> "Mul"
             | Div -> "Div"
             | Mod -> "Mod"
             | Pow -> "Pow") ^ "(" ^ js2string e1 ^ ", " ^ js2string e2 ^ ")"
      | IfElse (ife,thene,elsee) ->
          "IfElse(" ^ js2string ife ^ ", " ^
                      js2string thene ^ ", " ^
                      js2string elsee ^ ")"
      | Logic b ->
          (match b with
             | And(e1,e2) ->
                 "LogicalAnd(" ^ js2string e1 ^ ", " ^ js2string e2 ^ ")"
             | Or(e1,e2) ->
                 "LogicalOr(" ^ js2string e1 ^ ", " ^ js2string e2 ^ ")"
             | Not(e) ->
                 "LogicalNot(" ^ js2string e ^ ")")
      | Compare(c,e1,e2)->
          let cs = 
            (match c with
               | GT -> "GT"
               | LT -> "LT"
               | GE -> "GE"
               | LE -> "LE"
               | EQ -> "EQ") 
          in
          "Compare(" ^ cs ^ ", " ^ js2string e1 ^ ", " ^ js2string e2 ^ ")"

  type expression = js
  let print_js e = js_rec e
  let string_of_expr e = js2string e

 end (* module JavaScriptExpr *)

(* provide shorthand to these modules *)
module P = ParsedOcamlExpr;;
module J = JavaScriptExpr;;

