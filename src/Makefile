FILES=project.ml ast.ml lexer.mll parser.mly

all: project

project: project.ml ast lex_parse
	ocamlc -c project.ml
	ocamlc -o ocamlscript lexer.cmo parser.cmo ast.cmo project.cmo

ast: ast.ml
	ocamlc -c ast.ml

lex_parse: lexer.mll parser.mly ast
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml

check: $(FILES)
	chmod u+x ./check_width
	./check_width $(FILES)

clean:
	rm -f ocamlscript lexer.ml parser.ml parser.mli *.cmi *.cmo
