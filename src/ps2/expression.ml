(* CS51 PS2 Spring 2011 *)
(* SOLUTIONS (slightly modified) *)
open Ast ;;
open ExpressionLibrary ;;

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "4^x") = true
 *                contains_var (parse "4+3") = false *)

let rec contains_var e =
  match e with
    | Var -> true
    | Unop (_,e1) -> contains_var e1
    | Binop (_,e1,e2) -> (contains_var e1) || contains_var e2
    | _ -> false
;;

(*>* Problem 2.2 *>*)

(* --------------------- Evaluate ---------------------- *)

(* Evaluate expression e at x.
   0/0 is evaluated rather arbitrarily as 0.
   So are 0*nan and 0*infinity. *)

(* evaluate : evaluates an expression for a particular value of x.
 *  example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)

let unop_sin = sin;;
let unop_cos = cos;;
let unop_ln = log;;
let unop_neg x = -. x

let rec evaluate e x =
  let unop_fn un= 
  match un with
    | Sin -> unop_sin
    | Cos -> unop_cos
    | Ln -> unop_ln
    | Neg -> unop_neg
  in
  let binop_fn bin =
  match bin with
    | Add -> fun x y -> x +. y
    | Sub -> fun x y -> x -. y
    | Mul -> fun x y -> x *. y
    | Div -> fun x y -> x /. y
    | Pow -> fun x y -> x ** y
  in
  match e with
    | Num f -> f
    | Var -> x 
    | Binop(binop,e_b1,e_b2) -> 
	  (binop_fn binop)((evaluate e_b1) x)((evaluate e_b2) x)
    | Unop(unop,e_u) -> 
	  (unop_fn unop)((evaluate e_u) x)
;;



(*>* Problem 2.3 *>*)

let rec derivative e =
  match e with
    | Num _ -> Num (0.)
    | Var -> Num (1.)
    | Unop (u,e1) ->
        (match u with
           | Sin -> Binop(Mul,Unop(Cos,e1),derivative e1)
           | Cos -> Binop(Mul,Unop(Neg,Unop(Sin,e1)),derivative e1)
           | Ln -> Binop(Div,derivative e1,e1)
           | Neg -> Unop(Neg,derivative e1)
	)
    | Binop (b,e1,e2) ->
        match b with
          | Add -> Binop(Add,derivative e1,derivative e2)
          | Sub -> Binop(Sub,derivative e1,derivative e2)
          | Mul -> Binop(Add,Binop(Mul,e1,derivative e2),
                         Binop(Mul,derivative e1,e2))
          | Div -> Binop(Div,
			 Binop(Sub,Binop(Mul,derivative e1,e2),
			       Binop(Mul,e1,derivative e2)),
			 Binop(Pow,e2,Num(2.)))
          | Pow ->
	      if contains_var e2 then
                Binop(Mul,e,Binop(Add,Binop(Mul,derivative e2,Unop(Ln,e1)),
                                  Binop(Div,Binop(Mul,derivative e1,e2),e1)))
              else Binop(Mul,e2,
			 Binop(Mul,derivative e1,Binop(Pow,e1,
                                                       Binop(Sub,e2,Num (1.)))))
;;


(*>* Problem 2.4 *>*)
(* didn't do this because I haven't implemented options *)
let rec find_zero (e:expression) (guess:float) (epsilon:float) (lim:int)
    : float option =
  let e' = derivative e in
  let rec find_zero' (guess:float) (count:int) : float option =
    if count >= lim then None else
    let e_of_guess = evaluate e guess in
    if abs_float e_of_guess >= epsilon then
      find_zero' (guess -. (e_of_guess /. evaluate e' guess)) (count + 1)
    else Some guess
  in find_zero' guess 0
;;


(*>* Problem 2.5 *>*)

let rec simplify e =
  match e with
    | Num n -> if (n < 0.) then Unop (Neg, Num (0. -. n)) else e
    | Var -> e
    | Unop (u,e1) ->
        let e1dash = simplify e1 in
          (match (u,e1dash) with
             | (Neg, Unop (Neg,e2)) -> simplify e2
             | (Neg,Num (0.)) -> Num (0.)
             | (Ln,Num (1.)) -> Num (0.)
             | (Sin,Num (0.)) -> Num (0.)
             | (Cos,Num (0.)) -> Num (1.)
             | _ -> Unop(u,e1dash)
	  )
    | Binop(b,e1,e2) ->
        let e1dash = simplify e1 in
        let e2dash = simplify e2 in
          match b with
            | Add ->
		if (e1dash = e2dash) then simplify (Binop(Mul,Num (2.),e1dash)) else
                  (match (e1dash,e2dash) with
                     | (Num (0.),_) -> e2dash
                     | (_,Num (0.)) -> e1dash
                     | (Unop(Neg,e1dashdash),_) -> simplify (Binop(Sub,e2dash,e1dashdash))
                     | (_,Unop(Neg,e2dashdash)) -> simplify (Binop(Sub,e1dash,e2dashdash))
                     | (Num (n1),Num (n2)) -> simplify (Num (n1 +. n2))
                     | _ -> Binop(Add,e1dash,e2dash)
		  )
            | Sub ->
		if (e1dash = e2dash) then Num (0.) else
                  (match (e1dash,e2dash) with
                     | (Num (0.),_) -> simplify (Unop(Neg,e2dash))
                     | (_,Num (0.)) -> e1dash
                     | (_,Unop(Neg,e2dashdash)) -> simplify (Binop(Add,e1dash,e2dashdash))
                     | (Unop(Neg,e1dashdash),_) ->
			 simplify (Unop(Neg,Binop(Add,e1dashdash,e2dash)))
                     | (Num (n1),Num (n2)) -> simplify (Num(n1 -. n2))
                     | _ -> Binop(Sub,e1dash,e2dash)
                  )
            | Mul ->
		if (e1dash = e2dash) then simplify (Binop(Pow,e1dash,Num (2.))) else
                  (match (e1dash,e2dash) with
                     | (Num (0.),_) -> Num (0.)
                     | (_,Num (0.)) -> Num (0.)
                     | (Num (1.),_) -> e2dash
                     | (_,Num (1.)) -> e1dash
                     | (Unop(Neg,e1dashdash),_) ->
                         simplify (Unop(Neg,Binop(Mul,e1dashdash,e2dash)))
                     | (_,Unop(Neg,e2dashdash)) ->
                         simplify (Unop(Neg,Binop(Mul,e1dash,e2dashdash)))
                     | (Binop(Div,e1dashone,e1dashtwo),_) ->
                         simplify (Binop(Div,Binop(Mul,e1dashone,e2dash),e1dashtwo))
                     | (_,Binop(Div,e2dashone,e2dashtwo)) ->
                         simplify (Binop(Div,Binop(Mul,e1dash,e2dashone),e2dashtwo))
                     | (Num (n1),Num (n2)) -> simplify (Num(n1 *. n2))
                     | _ -> Binop(Mul,e1dash,e2dash)
                  )
            | Div ->
		if (e1dash = e2dash) then Num (1.) else
                  (match (e1dash,e2dash) with
                     | (Num (0.),_) -> Num (0.)
                     | (_,Num (1.)) -> e1dash
                     | (Unop(Neg,e1dashdash),_) ->
                         simplify (Unop(Neg,Binop(Div,e1dashdash,e2dash)))
                     | (_,Unop(Neg,e2dashdash)) ->
                         simplify (Unop(Neg,Binop(Div,e1dash,e2dashdash)))
                     | (Binop(Div,e1dashone,e1dashtwo),_) ->
                         simplify (Binop(Div,e1dashone,Binop(Mul,e1dashtwo,e2dash)))
                     | (_,Binop(Div,e2dashone,e2dashtwo)) ->
                         simplify (Binop(Div,Binop(Mul,e1dash,e2dashtwo),e2dashone))
                     | (Num (n1),Num (n2)) -> simplify (Num(n1 /. n2))
                     | _ -> Binop(Div,e1dash,e2dash)
                  )
          | Pow ->
	      (match (e1dash,e2dash) with
                 | (_,Num (0.)) -> Num (1.)
                 | (Num (0.),_) -> Num (0.)
                 | (_,Num (1.)) -> e1dash
                 | (_,Unop(Neg,e2dashdash)) ->
                     simplify (Binop(Div,Num (1.),Binop(Pow,e1dash,e2dashdash)))
                 | (Binop(Pow,e1dashone,e1dashtwo),_) ->
                     simplify (Binop(Pow,e1dashone,Binop(Mul,e1dashtwo,e2dash)))
                 | _ -> Binop(Pow,e1dash,e2dash)
              )
;;


let e1 = Num(1.);;
let e4 = Binop(Sub,Binop(Pow,Var,Var),Var);;
let e5 = Binop(Mul,Binop(Pow,Num(2.),Var),Unop(Ln,Var));;
let e6 = Binop(Div,Binop(Add,Var,Num(1.)),Binop(Pow,Var,Num(2.)));;

(* e4 *)
print_string(to_string(simplify(derivative(e4))));;
print_string("\n");;
print_float(evaluate(e4)(2.5));;
print_string("\n");;
print_float(evaluate(derivative e4)(3.14));;
print_string("\n");;

(* e5 *)
print_string(to_string(simplify(derivative(e5))));;
print_string("\n");;
print_float(evaluate(e5)(3.14));;
print_string("\n");;
print_float(evaluate(derivative e5)(2.5));;
print_string("\n");;

(* e6 *)
print_string(to_string(simplify(derivative(e6))));;
print_string("\n");;
print_float(evaluate(e6)(2.5));;
print_string("\n");;
print_float(evaluate(derivative e6)(3.14));;
print_string("\n");;

