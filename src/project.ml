(* Final Project - OcamlScript
 * CS51 Spring 2011
 * Author: Peter Lin (CSCI E-250)
 *)

open Ast;;

(* ocamljs function takes an expression of type ParsedOcamlExpr and translates
 * it into an expression of type JavaScriptExpr. ocamljs simply recursively 
 * translates the expression outside-in, occaionally doing something special in
 * order to have things working properly in javascript, those are commented
 * below. There are no comments for expressions that are straight conversions, 
 * they are quite self-explanatory.
 *
 * 1. "let x = y in y" is wrapped in a function calls and translated as if the
 *    ocaml code was something like (fun y -> y) (x)
 * 2. built in types all have a lower case type name, so built in types will
 *    never conflict or be incorrectly matched with user defined types. tuple
 *    has special name tuplen where n is the number of vals in the tuple. For
 *    example (1,2,3) is a tuple3.
 * 3. Lists are handled differently here because we don't always want elist to
 *    be at the end of all lists. For example h::t, we don't want it to end up
 *    as h::t::[], so we need to check if the last elem is Var, if so don't
 *    append the last elist. If I had represented the Lists in a way other 
 *    than Ocaml lists this wouldn't have been a problem. Would be an easy fix.
 * 4. In ocamljs all match statements match var against set of patterns. If the
 *    match target given is not a var (is some expr other than var), match
 *    first assigns the expr to some var, then matches that var. Based on 
 *    advice from TF.
 * 
 *)

let rec ocamljs (e:P.expression) : J.expression =
  match e with               
    | P.Let (v,e) ->
        J.Assign(ocamljs v,ocamljs e)
    | P.LetIn(e1,e2) ->
        (match e1 with
           | P.Let(v,e) ->
               (* wraps let_in exprs in a function call, see 1*)
               J.Call(J.Func(ocamljs v,ocamljs e2) :: [ocamljs e])
           (* if let_in parsed correct, this should never be matched *)
           | _ -> assert false )
    | P.Func (v,e) ->
        J.Func(ocamljs v,ocamljs e)
    | P.FCall (e1,e2) ->
        J.Call(ocamljs e1 :: ocamljs e2 :: [])
    | P.Arith (a,e1,e2) -> 
        let arith = 
          (match a with
             | P.Add -> J.Add
             | P.Min -> J.Min
             | P.Mul -> J.Mul
             | P.Div -> J.Div
             | P.Mod -> J.Mod
             | P.Pow -> J.Pow) 
        in
        J.Arith(arith, ocamljs e1, ocamljs e2)
    | P.Const c -> 
        (match c with
           (* JS weakly typed, only have one const type (string) *)
           | P.CInt i -> J.Const(string_of_int i)
           | P.CFloat f -> J.Const(string_of_float f)
           | P.CString s -> J.Const(s)
           | P.CBool b ->  J.Const(string_of_int b))
    | P.BLogic b ->
        (match b with
           | P.And(e1,e2) -> J.Logic(J.And(ocamljs e1, ocamljs e2))
           | P.Or(e1,e2) -> J.Logic(J.Or(ocamljs e1, ocamljs e2))
           | P.Not(e) -> J.Logic(J.Not(ocamljs e)))
    | P.Comp(c,e1,e2) ->
        let cs = 
          (match c with
             | P.GT -> J.GT
             | P.LT -> J.LT
             | P.GE -> J.GE
             | P.LE -> J.LE
             | P.EQ -> J.EQ ) 
        in
	J.Compare(cs, ocamljs e1, ocamljs e2)
    | P.DataType(t,l) ->
        let kind = 
          (* see 2 on data types *)
          (match t with
             | P.Tuple -> "\"tuple" ^ string_of_int (List.length l) ^ "\""
             | P.Int -> "\"int\""
             | P.Float -> "\"float\""
             | P.String -> "\"string\""
             | P.Bool -> "\"bool\""
             | P.List -> "\"list\""
             | P.Other (k) -> "\"" ^ k ^ "\"") 
        in 
	(match t with
           (* Lists are handled slightly differently, see 3 *)
	   | P.List ->
	       let rec listify lst = 
		 (match lst with
		   | [] -> 
                       J.Array(J.Const("\"elist\"") :: [])
		   | [P.Var _] -> 
                       ocamljs (List.hd lst)
		   | h::t -> 
                       J.Array(J.Const(kind) :: (ocamljs h) :: [listify t]))
	       in
	       listify l
	   | _ ->
               J.Array(J.Const(kind):: (List.map ocamljs l)))
    | P.Match (e,l) ->
        (* match always matches against var, see 4 *)
	(match e with
           | P.Var(v) ->
	       J.Match(v,List.map (fun (c,d) -> (ocamljs c, ocamljs d)) l)
	   | _ -> 
               (* non-var, assign expr to Var m, then match var *)
               let assign = P.Let(P.Var("m"),e) in
               ocamljs (P.LetIn(assign,P.Match(P.Var("m"),l))))
    | P.WildCard -> J.Any 
    | P.Var v -> J.Var v
    | P.IfBlock(ife, thene, elsee) -> 
        J.IfElse(ocamljs ife, ocamljs thene, ocamljs elsee)
;;

(* NOTE: all the assert tests have been deleted. In the end I decided they were
 * no more useful than simply running the translated ocaml code as JS. In fact 
 * using assert tests were less useful because it was difficult to read, hard
 * to be sure it's correct, and doesn't show that the javascript code output 
 * was even valid. I have tested the JS output quite extensively, parts of 
 * which are in test.html which I will go through in the screen cast. *)

(* main program *)
let _ =
      (* infinite loop *)
      while true do
        (* get user input *)
        let lexbuf = Lexing.from_channel stdin in
        try
        (* get parsed expr *)
        let parsedExpr = Parser.parse Lexer.token lexbuf in
        (* translates using ocamljs *)
        let jsExpr = ocamljs parsedExpr in 
        print_string("\n/* JavaScript from ocamlscript */\n"); 
        (* prints javascript *)
        print_string(J.print_js jsExpr);
        print_string("\n"); 
        flush stdout
	with e ->
	    print_string(Printexc.to_string e ^ "\n")
      done
       

