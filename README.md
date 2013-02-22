OCamlScript
===========

CS51 Final Project

Summary
------
Small program that translates/converts OCaml code to JavaScript code. Primarily written in OCaml to pattern match and build ASTs.

For example:

    let add x y z =   
        x + y + z  
    ;;

in OCaml code is translated to:

    add = function(x){
        return function(y) {
            return function(z) { 
                return x + y + z;
            } 
        }
    }


Implementation
------

*   Lex/Parser
    *   Ocamllex/Ocamlyacc using grammar rules to tokenise and parse Ocaml code into ASTs
*   Abstract Syntax Trees
    *   Parsed Ocaml expression AST - ParsedOcamlExpr Translated JavaScript expression AST - JavaScriptExpr
*   Translator
    *   let ocamljs (e:P.expression) : J.expression = ...
*   Printer
    *   print_js function in JavaScriptExpr that outputs JavaScript code


More details in the slides.pdf file in the repo. Presentation can be viewed [here](http://www.youtube.com/watch?v=hmoXpwP7ybM)
