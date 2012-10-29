all: expression

FILES=ast.ml expressionLibrary.ml expression.ml

expression: $(FILES)
	ocamlc -g -o expression unix.cma str.cma $(FILES)

clean: 
	rm -f expression *.cmi *.cmo
