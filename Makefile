
all:markdown

markdown: markdown.ml 
	ocamlopt -o markdown markdown.ml


clean:
	-rm -r file _build docs *.o *.cmo *.cmx *.mli *.cmi *.dot
